
(asdf:defsystem #:vm-translator
  :description "Translate VM code into Hack assembly"
  :version "0.0.1"
  :author "Nathan Flynn"
  :depends-on (:uiop)
  :components ((:module "src"
                :pathname #P"src/"
                :serial t
                :components ((:file "package")
                             (:file "main")
                             (:file "parser")
                             (:file "writer"))))
  :in-order-to ((asdf:test-op (asdf:test-op "vm-translator/test"))))

(asdf:defsystem #:vm-translator/test
  :description "Unit tests for VM translator"
  :author "Nathan Flynn"
  :depends-on (:vm-translator
               :fiveam)
  :components ((:module "t"
                :components ((:file "tests"))))
  :perform (asdf:test-op (o c) (uiop:symbol-call :vm-test '#:run-tests)))
