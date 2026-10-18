
(in-package #:translator)

(defparameter +inc-stack+ '("@SP" "M=M+1")
  "Commands to increment stack pointer.")

(defparameter +dec-stack+ '("@SP" "M=M-1")
  "Commands to decrement stack pointer.")

(defparameter +sp-address+ '("@SP" "A=M")
  "Get the address of the top of the stack.")

(defparameter +pop-to-d+
  (append +sp-address+ '("D=M") +dec-stack+)
  "Commands to pop from top of stack to D register.")

(defparameter +subtract+
  (append +pop-to-d+ '("A=M" "M=M-D"))
  "Commands to subtract y from x. 
Defining it here because it is used in predicate checks as well. ")

(defparameter +label-number+ 0)
(defun new-label ()
  "Create a new number to append to generated label tags."
  (prog1
      +label-number+
    (setf +label-number+ (+ +label-number+ 1))))

;; create function for equality checks
;; need to generate labels and use jump commands

(defparameter +arithmetic-mappings+
  `(("add" ,(append +pop-to-d+ '("A=M" "M=M+D")))
    ("sub" +subtract+)
    ("neg" ,(append +sp-address+ '("!M")))
    ("eq" ,(append +subtract+ ))))

(defun translate-arithmetic (command)
  'todo)

(defun translate-push (command)
  'todo)

(defun translate-pop (command)
  'todo)

(defun translate-label (command)
  'todo)

(defun translate-goto (command)
  'todo)

(defun translate-if (command)
  'todo)

(defun translate-function (command)
  'todo)

(defun translate-return (command)
  'todo)

(defun translate-call (command)
  'todo)


(defun translate-command (line)
  "Translates the VM command into the equivalent assembly commands.
LINE is expected to be a single string. Returns a list of ASM string commands."
  (let ((command (process-and-split-line line)))
    (when command
      (let ((type (get-command-type command)))
        (case type
          (:C_ARITHMETIC (translate-arithmetic command))
          (:C_PUSH (translate-push command))
          (:C_POP (translate-pop command))
          (:C_LABEL (translate-label command))
          (:C_GOTO (translate-goto command))
          (:C_IF (translate-if command))
          (:C_FUNCTION (translate-function command))
          (:C_RETURN (translate-return command))
          (:C_CALL (translate-call command))
          (t (error "Cannot translate unknown command type: ~A~%" line)))))))

