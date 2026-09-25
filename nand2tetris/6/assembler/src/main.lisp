
(in-package #:asm)


(defparameter *symbol-table* (make-instance 'symbol-table))

(defun assemble (file)
  "Reads the provided ASM FILE and assembles contents into
Hack binary code."
  (let ((filepath (truename file)))
    'todo-parser
    ))
