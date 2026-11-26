
(defpackage #:vm-test
  (:use #:cl #:translator #:fiveam)
  (:export run-tests))

(in-package :vm-test)

(defun run-tests ()
  (run! 'parser-tests))

(def-suite parser-tests)
(in-suite parser-tests)

(test process-and-split-line
  (is-false (process-and-split-line "    "))
  (is-false (process-and-split-line "// this is a comment line"))
  (is-true (process-and-split-line "push constant 0")))

(test valid-length
  (is-true (valid-length-p '("push" "constant" "0") 3))
  (is-true (valid-length-p '("add" "//comment") 1))
  (is-false (valid-length-p '("if-goto" "end" "junk") 2))
  (is-false (valid-length-p '("if-goto" "//comment") 2))
  (is-false (valid-length-p '("add") 2)))

(test command-type
  (is (equal :C_ARITHMETIC (get-command-type "add")))
  (is (equal :C_ARITHMETIC (get-command-type '("not"))))
  (is (equal :C_PUSH (get-command-type "push constant 1")))
  (is (equal :C_POP (get-command-type "pop pointer 0")))
  (is (equal :C_LABEL (get-command-type "label hello")))
  (is (equal :C_GOTO (get-command-type "goto hello")))
  (is (equal :C_IF (get-command-type "if-goto hello")))
  (is (equal :C_FUNCTION (get-command-type "function foo 2")))
  (is (equal :C_CALL (get-command-type "call foo 3")))
  (is (equal :C_RETURN (get-command-type "return")))
  )

(test arg1
  (is (equal "add" (arg1 '("add" "//comment") :C_ARITHMETIC)))
  (is (equal "constant" (arg1 '("push" "constant" "12") :C_PUSH))))

(test arg1-unsupported
  (signals simple-error
    (arg1 '("return") :C_RETURN)))
