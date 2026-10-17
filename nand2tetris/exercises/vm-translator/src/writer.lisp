
(in-package #:translator)

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

