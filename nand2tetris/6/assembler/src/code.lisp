
(in-package :asm)

(defparameter allowed-dest-strings
  '("ADM" "AD" "AM" "DM" "A" "D" "M")
  "Sorted valid values for destination string.")

(defun translate-dest (dest)
  (let ((sorted-dest (sort dest #'char-lessp)))
    (unless (member sorted-dest allowed-dest-strings :test #'equalp)
      (error (format nil "Syntax error: ~A is not a valid destination~%" dest))) 
    (let* (
           (results (mapcar
                     (lambda (c) (if (find c sorted-dest) 1 0))
                     '(#\A #\D #\M))))
      (format nil "~{~A~}" results))))


