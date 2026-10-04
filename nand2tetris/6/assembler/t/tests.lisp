
(defpackage #:asm-test
  (:use #:cl #:hack-asm #:fiveam)
  (:export run-tests))

(in-package :asm-test)

(defun run-tests ()
  (run! 'code-tests)
  (run! 'parser-tests))

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

(test translates-jmp-str
  (is (equal "001" (translate-jump "JGT")))
  (is (equal "111" (translate-jump "JMP"))))

(test unknown-jmp-str
  (signals asm-syntax-error
    (translate-jump "foo")))

(test translates-null-str
  (is (equal "000" (translate-jump nil))))


(def-suite parser-tests)
(in-suite parser-tests)

(test command-type-is-a
  (is (equal :A (command-type "@111"))))

(test command-type-is-c
  (is (equal :C (command-type "A=-A")))
  (is (equal :C (command-type "0;jmp")))
  (is (equal :C (command-type "D-1;JLT"))))

(test command-type-is-l
  (is (equal :L (command-type "(label1)"))))

(test invalid-command-type
  (is (null (command-type "// this is a comment")))
  (is (null (command-type "   "))))

(test process-a-command
  (is (equal "0111111111111111" (process-a-command "@32767")))
  (is (equal "0000000000000000" (process-a-command "@0"))))

(test process-a-command-invalid-address
  (signals simple-error
    (process-a-command "@100000")))

(test process-c-command
  (is (equal "1111110000010000" (process-c-command "D=M")))
  (is (equal "1110001100000001" (process-c-command "D;JGT")))
  (is (equal "1110110000000000" (process-c-command "A")))
  (is (equal "1110000010101111" (process-c-command "AM=D+A;JMP"))))
