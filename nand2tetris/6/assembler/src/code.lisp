
(in-package :asm)

(defun translate-dest (dest)
  (let ((results (mapcar
                  (lambda (c) (if (find c dest) 1 0))
                  '(#\A #\D #\M))))
    (format nil "~{~A~}" results)))
