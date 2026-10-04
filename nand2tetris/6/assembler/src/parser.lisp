
(in-package :hack-asm)


(defparameter *symbol-table* (make-instance 'symbol-table))

;; Command Types:
;; A = address 
;; C = compute 
;; L = label 
(deftype command () '(member :A :C :L))

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

(defun c-command-p (line)
  "LINE is C command if line contains '=' or ';'."
  (or (find #\; line) (find #\= line)))

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
              ((c-command-p l) :C)
              ((l-command-p l) :L)
              (t NIL)))))

(defun pad-binary (number &optional (max-len 15))
  "Pad a binary NUMBER with zeros until it is of length MAX-LEN."
  (let* ((number-len (length number))
         (num-zeros (- max-len number-len))
         (zeros (make-string num-zeros :initial-element #\0)))
    (concatenate 'string zeros number)))

(defun process-a-command (command)
  "Return binary value for an A commmand."
  (let* ((value (subseq command 1)) ; remove @ char
         (try-int (parse-integer value :junk-allowed t)))
    ;; TODO: check for negative numbers
    (if (null try-int)
        'symbol
        (let ((binary (write-to-string try-int :base 2)))
          (if (> (length binary) 15)
              (error "Provided address is longer than 15 bits: ~A" command)
              (concatenate 'string "0" (pad-binary binary)))))))

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
