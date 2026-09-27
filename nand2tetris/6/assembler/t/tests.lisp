
(defpackage #:asm-test
  (:use #:cl #:asm #:fiveam)
  (:export run-tests))

(in-package :asm-test)

(defun run-tests ()
  (run! 'code-tests))

(def-suite code-tests)
(in-suite code-tests)

(test translates-dest-str
  (is (equal "111" (translate-dest "AMD")))
  (is (equal "110" (translate-dest "AD")))
  (is (equal "101" (translate-dest "MA")))
  (is (equal "011" (translate-dest "MD")))
  (is (equal "001" (translate-dest "M")))
  (is (equal "010" (translate-dest "D")))
  (is (equal "100" (translate-dest "A"))))

(test translates-null-str
  (is (equal "000" (translate-dest nil))))

(test dest-string-double-chars
  (signals asm-syntax-error
    (translate-dest "AAAMD")))

(test translates-comp-str
  (is (equal "0101010" (translate-comp "0")))
  (is (equal "1110000" (translate-comp "M")))
  (is (equal "0011111" (translate-comp "D+1")))
  (is (equal "0110010" (translate-comp "A-1")))
  (is (equal "1000000" (translate-comp "D&M"))))

(test unknown-comp-str
  (signals asm-syntax-error
    (translate-comp "foo")))
