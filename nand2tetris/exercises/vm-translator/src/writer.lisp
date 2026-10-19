
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

(defparameter +arithmetic-mappings+
  `(("add" ,(append +pop-to-d+ '("A=M" "M=M+D")))
    ("sub" ,(append +pop-to-d+ '("A=M" "M=M-D")))
    ("neg" ,(append +sp-address+ '("-M")))
    ("and" ,(append +pop-to-d+ '("M=D&M")))
    ("or" ,(append +pop-to-d+ '("M=D|M")))
    ("not" ,(append +sp-address+ '("M=!M"))))
  "Mappings between function keyword and ASM commands.
This is only for 'fixed' commands: for predicate commands, we'll need to
dynamically generate at runtime.")

(defparameter +predicates+ '("eq" "gt" "lt"))

(defparameter +label-number+ 0)
(defun new-label (base &optional number-override)
  "Create a new label from the provide BASE string and the label
counter +label-number+. User NUMBER-OVERRIDE to provide fixed label number."
  (let ((number (if number-override
                    number-override
                    (prog1
                        +label-number+
                      (setf +label-number+ (+ +label-number+ 1))))))
    (format nil "~A~A" base number)))

(defun generate-predicate (vm-cmd)
  (flet ((cc (&rest sequence)
           "Just a shorter concatenate function."
           (apply #'concatenate 'string sequence)))
    (let* ((asm-cmd (cond ((equalp vm-cmd "eq") "JEQ")
                          ((equalp vm-cmd "gt") "JGT")
                          ((equalp vm-cmd "lt") "JLT")
                          (t (error "Unsupported predicate ~A" vm-cmd))))
           (jump-cmd (cc "D;" asm-cmd))
           (true-base (new-label (cc asm-cmd "_TRUE_")))
           (end-base (new-label (cc asm-cmd "_END_")))
           (at-true (cc "@" true-base))
           (at-end (cc "@" end-base))
           (true-label (cc "(" true-base ")"))
           (end-label (cc "(" end-base ")")))
      (append +pop-to-d+
              (list "A=M"
                    "M=M-D"
                    "D=M"
                    at-true
                    jump-cmd)
              +sp-address+
              (list "M=0"
                    at-end
                    "0;JMP"
                    true-label)
              +sp-address+
              (list "M=-1"
                    end-label)))))

(defun translate-arithmetic (command)
  (let* ((cmd (first command))
         (assoc-result (assoc cmd +arithmetic-mappings+ :test #'equalp)))
    (cond (assoc-result (second assoc-result))
          ((member cmd +predicates+ :test #'equalp) (generate-predicate cmd))
          (t (error "Unknown arithmetic command ~A" command)))))

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

