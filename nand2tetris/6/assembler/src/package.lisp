
(defpackage #:hack-asm
  (:use #:cl)
  (:export translate-dest
           translate-comp
           translate-jump
           asm-syntax-error
           command-type
           process-a-command
           process-c-command))

(in-package :hack-asm)
