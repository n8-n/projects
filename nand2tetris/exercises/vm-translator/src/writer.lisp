
(in-package #:translator)


(defun cmd-list (&rest cmds)
  "Create a list of commands. Values in CMDS can be a string or a
list of strings. Function will flatten sublists."
  (flet ((wrap (x)
           (if (listp x)
               x
               (list x))))
    (flet ((wrap-append (x1 x2)
             (append (wrap x1)
                     (wrap x2))))
      (reduce #'wrap-append cmds))))

(defun binary-function (alu-cmd)
  "Generate ASM commands to perform the specified ALU command on
the top two stack values."
  (cmd-list +pop-to-d+
            +dec-stack+
            "A=M"
            alu-cmd
            +inc-stack+))

(defun unary-function (alu-cmd)
  "Generate ASM commands to perform the specified ALU command on
the top stack value."
  (cmd-list +dec-stack+
            "A=M"
            alu-cmd
            +inc-stack+))

(defparameter +inc-stack+ '("@SP" "M=M+1")
  "Commands to increment stack pointer.")

(defparameter +dec-stack+ '("@SP" "M=M-1")
  "Commands to decrement stack pointer.")

(defparameter +sp-address+ '("@SP" "A=M")
  "Get the address of the top of the stack.")

(defparameter +pop-to-d+
  (cmd-list +dec-stack+ "A=M" "D=M")
  "Commands to pop from top of stack to D register.")

(defparameter +push-d+
  (cmd-list +sp-address+
            "M=D"
            +inc-stack+)
  "Commands to push from register D to the stack.")

(defparameter +arithmetic-alist+
  `(("add" ,(binary-function "M=D+M"))
    ("sub" ,(binary-function "M=D-M"))
    ("neg" ,(unary-function "M=-M"))
    ("and" ,(binary-function "M=D&M"))
    ("or" ,(binary-function "M=D|M"))
    ("not" ,(unary-function "M=!M")))
  "Mappings between function keyword and ASM commands.
This is only for 'fixed' commands: for predicate commands, we'll need to
dynamically generate at runtime.")

(defparameter +predicate-alist+
  '(("eq" "JEQ")
    ("gt" "JGT")
    ("lt" "JLT"))
  "Mapping between VM predicate function and CPU jump instruction.")

(defparameter +segment-alist+
  '(("local" "LCL")
    ("argument" "ARG")
    ("this" "THIS")
    ("that" "THAT"))
  "Mapping between VM segment keyword and Hack ASM address keyword.")

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

(defun cc (&rest sequence)
  "Just a shorter concatenate function."
  (apply #'concatenate 'string sequence))

(defun a-find (value alist)
  "Find a string VALUE in the ALIST."
  (assoc value alist :test #'equalp))

(defun generate-predicate (vm-cmd)
  (let* ((asm-cmd (car (a-find vm-cmd +predicate-alist+)))
         (jump-cmd (cc "D;" asm-cmd))
         (true-base (new-label (cc asm-cmd "_TRUE_")))
         (end-base (new-label (cc asm-cmd "_END_")))
         (at-true (cc "@" true-base))
         (at-end (cc "@" end-base))
         (true-label (cc "(" true-base ")"))
         (end-label (cc "(" end-base ")")))
    (cmd-list +pop-to-d+
              "A=M"
              "M=M-D"
              "D=M"
              at-true
              jump-cmd
              +sp-address+
              "M=0"
              at-end
              "0;JMP"
              true-label
              +sp-address+
              "M=-1"
              end-label)))

(defun translate-arithmetic (command)
  (let* ((cmd (first command))
         (arithmetic-result (a-find cmd +arithmetic-alist+)))
    (cond (arithmetic-result (second arithmetic-result))
          ((a-find cmd +predicate-alist+) (generate-predicate cmd))
          (t (error "Unknown arithmetic command ~A" command)))))

(defun segment-address (pointer i)
  "Return ASM instructions to get the address at the POINTER
address plus I."
  (cmd-list (cc "@" i)
            "D=A"
            (cc "@" pointer)
            "A=M"
            "A=D+A"))

(defun translate-push (command)
  (let* ((a1 (arg1 command :C_PUSH))
         (a2 (arg2 command :C_PUSH))
         (segment (second (a-find a1 +segment-alist+))))
    (cond ((equalp a1 "constant") (cmd-list (cc "@" a2)
                                            "D=A"
                                            +push-d+))
          (segment (cmd-list (segment-address segment a2)
                             "D=M"
                             +push-d+))
          (t 'todo))))

(defun translate-pop (command)
  (let* ((a1 (arg1 command :C_PUSH))
         (a2 (arg2 command :C_PUSH))
         (segment (second (a-find a1 +segment-alist+))))
    (cond (segment (cmd-list (segment-address segment a2)
                             "D=A"
                             "@R13"
                             "M=D"
                             +pop-to-d+
                             "@R13"
                             "A=M"
                             "M=D"))
          (t 'todo))))

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

(defun translate-vm-command (line)
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


;; testing functions
(defun print-cmd-list (l)
  (dolist (i l) (format t "~A~%" i)))

(defun translate-list (vm-cmd-list)
  (reduce #'append
          (mapcar #'translate-vm-command vm-cmd-list)))

(defun translate-print (&rest vm-cmd-list)
  (print-cmd-list (translate-list vm-cmd-list)))
