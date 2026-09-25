
(asdf:defsystem #:assembler
  :description "Hack assembler for Nand2Tetris"
  :version "0.0.1"
  :author "Nathan Flynn"
  :depends-on (:uiop)
  :components ((:module "src"
                :pathname #P"src/"
                :components ((:file "package")
                             (:file "table")
                             (:file "code")
                             (:file "parser")
                             (:file "main"))))
  ;; no build config, just load into sly
  )
