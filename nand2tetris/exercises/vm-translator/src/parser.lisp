
(in-package #:translator)

(deftype command () '(member
                      :C_ARITHMETIC
                      :C_PUSH
                      :C_POP
                      :C_LABEL
                      :C_GOTO
                      :C_IF
                      :C_FUNCTION
                      :C_RETURN
                      :C_CALL))

(defparameter +arithmetic-commands+
  '("add" "sub" "neg" "eq" "gt" "lt" "and" "or" "not"))

(define-condition vm-syntax-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream "SYNTAX ERROR: ~A~&" (message condition))))
  (:documentation "Signals error in provided VM code."))

;;; commands to check command types
;; Expects command to be tokenised, i.e. list of words rather than raw string

(defun valid-length-p (command length)
  "Checks if the specified command has the expected number of non-comment tokens. 
Also verifies that any extra tokens at the end are part of a comment."
  (let ((tokens-len (length command)))
    (if (< tokens-len length)
        NIL
        (let* ((token-tests
                 (loop for i from 0 to (- length 1)
                       for x in command
                       collect (comment-p x)))
               (tokens-are-comments
                 (reduce (lambda (x y) (or x y)) token-tests)))
          (if tokens-are-comments
              NIL
              (cond ((comment-p (nth length command)) T)
                    ((= tokens-len length) T)
                    (t NIL)))))))


(defun arithmetic-p (command)
  (when (valid-length-p command 1)
    (let ((operator (first command)))
      (find operator +arithmetic-commands+ :test #'equalp))))

(defun simple-verify-p (command keyword length)
  "Verify that COMMAND begins with KEYWORD and is the expected LENGTH."
  (when (valid-length-p command length)
    (equalp (first command) keyword)))

(defun push-p (command)
  (simple-verify-p command "push" 3))

(defun pop-p (command)
  (simple-verify-p command "pop" 3))

(defun label-p (command)
  (simple-verify-p command "label" 2))

(defun goto-p (command)
  'todo)

(defun if-p (command)
  'todo)

(defun function-p (command)
  'todo)

(defun return-p (command)
  'todo)

(defun call-p (command)
  'todo)

(defun comment-p (s)
  "Determines if the string S is a comment."
  (when (>= (length s) 2)
    (let ((first-2-chars (subseq s 0 2)))
      (equalp first-2-chars "//"))))

(defun process-and-split-line (line)
  "Returns nil if line is whitespace or a comment. Otherwise,
returns the line as a list of string tokens."
  (let ((l (string-trim " " line)))
    (if (or (uiop:emptyp l) (comment-p l))
        nil
        (uiop:split-string l))))

(defun get-command-type (command)
  "Returns a command type keyword for the specified COMMAND.
COMMAND can be a list of string tokens or a string. If it's a string,
split it with process-and-split-line function."
  (let ((cmd (if (typep command 'string)
                 (process-and-split-line command)
                 command)))
    (cond ((arithmetic-p cmd) :C_ARITHMETIC)
          ((push-p cmd) :C_PUSH)
          ((pop-p cmd) :C_POP)
          ((label-p cmd) :C_LABEL)
          ((goto-p cmd) :C_GOTO)
          ((if-p cmd) :C_IF)
          ((function-p cmd) :C_FUNCTION)
          ((return-p cmd) :C_RETURN)
          ((call-p cmd) :C_CALL)
          (t (error 'vm-syntax-error
                    :message (format nil "Unknown or malformed command: ~A~%" cmd))))))

(defun arg1 (command type)
  "Return the first argument for most command types. For arithmetic types, return
the command itself."
  (cond ((eq type :C_RETURN)
         (error "Cannot return arg1 for `return` command type"))
        ((eq type :C_ARITHMETIC) (first command))
        (t (second command))))

(defun arg2 (command type)
  "Return the second arg for certain command types."
  (let ((supported-types '(:C_POP :C_PUSH :C_FUNCTION :C_CALL)))
    (if (member type supported-types)
        (third command)
        (error "Cannot return arg2 for unsupported type ~A" type))))
