
(in-package :hack-asm)


(defparameter *symbol-table* (make-instance 'symbol-table))

;; Command Types:
;; A = address 
;; C = compute 
;; L = label 
(deftype command () '(member :A :C :L))

(defparameter *current-ram-address* 15)
(defun next-ram-address ()
  "Returns the next available RAM address."
  (setf *current-ram-address* (+ *current-ram-address* 1)))

(defun validate-file (file type)
  "If FILE is not a path and the type doesn't match the
specified TYPE, then throw an error."
  (declare (pathname file))
  (unless (equalp (pathname-type file) type)
    (error "File ~A must be of type ~A" file type)))

(defun read-asm-file (file)
  "Opens the FILE and collects lines into a list."
  (validate-file file "ASM")
  (uiop:read-file-lines file))

(defun write-hack-file (contents file)
  "Writes the list of binary CONTENTS to a .hack FILE."
  (validate-file file "HACK")
  (with-open-file (f file :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (line contents)
      (when line
        (write-line line f)))))


(defun comment-p (line)
  "Determines if the LINE is a comment."
  (when (>= (length line) 2)
    (let ((first-2-chars (subseq line 0 2)))
      (equalp first-2-chars "//"))))

(defun a-command-p (line)
  "LINE is A command if first letter is '@'."
  (eq (char line 0) #\@))

(defun l-command-p (line)
  "LINE is L command if first and last char are parentheses."
  (let ((final-index (- (length line) 1)))
    (and (eq (char line 0) #\()
         (eq (char line final-index) #\)))))

(defun command-type (line)
  "Returns the command type of the current LINE.
If line is a comment or whitespace, return nil."
  (let ((l (string-trim " " line)))
    (if (or (uiop:emptyp l) (comment-p l))
        NIL
        (cond ((a-command-p l) :A)
              ((l-command-p l) :L)
              (t :C)))))

(defun pad-binary (number &optional (max-len 15))
  "Pad a binary NUMBER with zeros until it is of length MAX-LEN."
  (let* ((number-len (length number))
         (num-zeros (- max-len number-len))
         (zeros (make-string num-zeros :initial-element #\0)))
    (concatenate 'string zeros number)))

(defun get-memory-address-for-symbol (symbol)
  "Returns the stored memory address for the symbol, if it
exists. Else, add a new symbol table entry with the next
available RAM address."
  (let ((try-address (get-address *symbol-table* symbol)))
    (if try-address
        try-address
        (let ((new-address (next-ram-address)))
          (progn
            (add-entry *symbol-table* symbol new-address)
            new-address)))))

(defun process-a-command (command)
  "Return binary value for an A commmand."
  (let* ((value (subseq command 1)) ; remove @ char
         (try-int (parse-integer value :junk-allowed t)))
    (when (and try-int (< try-int 0))
      (error "Address cannot be negative: ~A" command))
    (let* ((address-int (if try-int
                            try-int
                            (get-memory-address-for-symbol value)))
           (address (write-to-string address-int :base 2)))
      (if (> (length address) 15)
          (error "Provided address is longer than 15 bits: ~A" command)
          (concatenate 'string "0" (pad-binary address))))))

(defun split-c-command (command)
  "Splits a C COMMAND into a list of three parts: 'dest', 'comp', and 'jump'.
If a command doesn't have one of these fields, it will be null in the result."
  (let ((contains-dest (find #\= command))
        (contains-jump (find #\; command))
        (split (uiop:split-string command :separator '(#\= #\;))))
    (cond ((and contains-dest contains-jump) split)
          (contains-dest (append split '(nil)))
          (contains-jump (cons nil split))
          ;; just a comp string
          (t `(nil ,(first split) nil)))))

(defun process-c-command (command)
  "Return binary value a C command."
  (let ((split (split-c-command command)))
    (let ((dest (translate-dest (first split)))
          (comp (translate-comp (second split)))
          (jump (translate-jump (third split))))
      (concatenate 'string "111" comp dest jump))))

(defun process-l-command (label pc)
  "Create symbol table entry for the label."
  ;; remove brackets from label
  (let ((strip-label (subseq label 1 (- (length label) 1))))
    (add-entry *symbol-table* strip-label pc)))
