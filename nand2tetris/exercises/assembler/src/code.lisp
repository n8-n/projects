
(in-package :hack-asm)

(defparameter +allowed-dest-strings+
  '("ADM" "AD" "AM" "DM" "A" "D" "M")
  "Sorted valid values for destination string.")

(defparameter +comp-mappings+ (make-hash-table :test #'equalp)
  "Mapping between comp string and binary representation.")

(defparameter +jmp-mappings+ (make-hash-table :test #'equalp)
  "Mapping between jmp string and binary representation.")

(define-condition asm-syntax-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream "SYNTAX ERROR: ~A~&" (message condition))))
  (:documentation "Signals error in provided ASM code."))

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

(defun init-hash-table (list-mappings hash-table)
  "Puts the values in LIST-MAPPINGS into HASH-TABLE. Each
entry mapping is expected to be a lsit with two values."
  (dolist (pair list-mappings)
    (setf (gethash (first pair) hash-table) (second pair))))

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
    (init-hash-table list-mappings +comp-mappings+)))

(defun translate-comp (comp)
  "Translate comp string into 7-digit binary sequence (a flag plus 6 Cs)"
  (let* ((is-m (find #\M comp))
         (a-flag (if is-m "1" "0"))
         (six-c (gethash comp +comp-mappings+)))
    (if (null six-c)
        (error 'asm-syntax-error
               :message (format nil "~A is not a valid compute string~%" comp))
        (concatenate 'string a-flag six-c))))

(defun init-jmp-mappings ()
  (let ((list-mappings
          '(("JGT" "001")
            ("JEQ" "010")
            ("JGE" "011")
            ("JLT" "100")
            ("JNE" "101")
            ("JLE" "110")
            ("JMP" "111"))))
    (init-hash-table list-mappings +jmp-mappings+)))

(defun translate-jump (jmp)
  "Translate jmp string into 3-digit binary sequence."
  (if (null jmp)
      "000"
      (let ((jmp-result (gethash jmp +jmp-mappings+)))
        (if (null jmp-result)
            (error 'asm-syntax-error
                   :message (format nil "~A is not a valid jump string~%" jmp))
            jmp-result))))

