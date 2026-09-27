
(defpackage #:asm
  (:use #:cl)
  (:export translate-dest
           translate-comp
           translate-jmp
           asm-syntax-error))

(in-package :asm)
