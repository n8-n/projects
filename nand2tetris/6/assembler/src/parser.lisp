
(in-package :asm)


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
      (write-line line f))))


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



;; decimal to binary: (write-to-string 25 :base 2)

;;
;; for splitting
;; (uiop:split-string "AM=-D;JMP" :separator '(#\= #\;))
