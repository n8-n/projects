
(defpackage #:asm-test
  (:use #:cl
        #:5am
        #:asm)
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

(test dest-string-double-chars
  (signals simple-error
    (translate-dest "AAAMD")))
