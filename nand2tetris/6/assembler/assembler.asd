
(asdf:defsystem #:assembler
  :description "Hack assembler for Nand2Tetris"
  :version "0.0.1"
  :author "Nathan Flynn"
  :depends-on (:uiop)
  :components ((:module "src"
                :pathname #P"src/"
                :serial t
                :components ((:file "package")
                             (:file "table")
                             (:file "code")
                             (:file "parser")
                             (:file "main"))))
  :in-order-to ((asdf:test-op (asdf:test-op "assembler/test"))))

(asdf:defsystem #:assembler/test
  :description "Unit tests for Hack assembler for Nand2Tetris"
  :author "Nathan Flynn"
  :depends-on (:assembler
               :fiveam)
  :components ((:module "t"
                :components ((:file "tests"))))
  :perform (asdf:test-op (o c) (uiop:symbol-call :asm-test '#:run-tests)))
