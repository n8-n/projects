
(in-package :asm)

(define-condition asm-syntax-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream "SYNTAX ERROR: ~A~&" (message condition))))
  (:documentation "Signals error in provided ASM code."))

(defparameter +allowed-dest-strings+
  '("ADM" "AD" "AM" "DM" "A" "D" "M")
  "Sorted valid values for destination string.")

(defun translate-dest (dest)
  (when (null dest)
    (return-from translate-dest "000"))
  (let ((sorted-dest (sort dest #'char-lessp)))
    (unless (member sorted-dest +allowed-dest-strings+ :test #'equalp)
      (error 'asm-syntax-error
             :message (format nil "~A is not a valid destination~%" dest)))
    (let* ((results
             (mapcar (lambda (c) (if (find c sorted-dest) 1 0))
                     '(#\A #\D #\M))))
      (format nil "~{~A~}" results))))


(defparameter +comp-mappings+ (make-hash-table :test #'equalp)
  "Mapping between comp string and binary representation.")

(defun init-comp-mappings ()
  (let ((list-mappings
          '(("0"   "101010")
            ("1"   "111111")
            ("-1"  "111010")
            ("D"   "001100")
            ("A"   "110000")
            ("M"   "110000")
            ("!D"  "001101")
            ("!A"  "110001")
            ("!M"  "110001")
            ("-D"  "001111")
            ("-A"  "110011")
            ("-M"  "110011")
            ("D+1" "011111")
            ("A+1" "110111")
            ("M+1" "110111")
            ("D-1" "001110")
            ("A-1" "110010")
            ("M-1" "110010")
            ("D+A" "000010")
            ("D+M" "000010")
            ("D-A" "010011")
            ("D-M" "010011")
            ("A-D" "000111")
            ("M-D" "000111")
            ("D&A" "000000")
            ("D&M" "000000")
            ("D|A" "010101")
            ("D|M" "010101"))))
    (dolist (pair list-mappings)
      (setf (gethash (first pair) +comp-mappings+) (second pair)))))

(init-comp-mappings)

(defun translate-comp (comp)
  "Translate comp string into 7 digit binary sequence (a flag plus 6 Cs)"
  (let* ((is-m (find #\M comp))
         (a-flag (if is-m "1" "0"))
         (six-c (gethash comp +comp-mappings+)))
    (if (null six-c)
        (error 'asm-syntax-error
               :message (format nil "~A is not a valid compute string~%" comp))
        (concatenate 'string a-flag six-c))))
