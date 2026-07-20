#lang sicp

(#%require "01-registers.rkt")


;; Exercise 5.21
(define count-leaves
  (make-machine
   (list (list 'null? null?) (list 'pair? pair?) (list '+ +) (list 'not not)
         (list 'car car) (list 'cdr cdr))
   '(controller
     (assign continue (label count-done))
     count-loop
     (test (op null?) (reg tree)) ; null check
     (branch (label null-case))
     (assign temp (op pair?) (reg tree)) ; leaf check
     (test (op not) (reg temp))
     (branch (label leaf-case))
     ;; else case
     (save continue)
     (save tree)
     (assign continue (label after-count-car))
     (assign tree (op car) (reg tree))
     (goto (label count-loop))
     after-count-car
     (restore tree)
     (assign tree (op cdr) (reg tree))
     (assign continue (label after-count-cdr))
     (save val)
     (goto (label count-loop))
     after-count-cdr
     (assign temp (reg val))
     (restore val)
     (assign val (op +) (reg temp) (reg val))
     (restore continue)
     (goto (reg continue))
     null-case
     (assign val (const 0))
     (goto (reg continue))
     leaf-case
     (assign val (const 1))
     (goto (reg continue))
     count-done)))


(define count-leaves-2
  (make-machine
   (list (list 'null? null?) (list 'pair? pair?) (list '+ +) (list 'not not)
         (list 'car car) (list 'cdr cdr))
   '(controller
     (assign continue (label count-done))
     (assign val (const 0)) ;; n accumulator
     count-loop
     (test (op null?) (reg tree))
     (branch (label null-case))
     (assign temp (op pair?) (reg tree))
     (test (op not) (reg temp))
     (branch (label leaf-case))
     ;; else case
     ;; set up for car tree
     (save continue)
     (save tree)
     (assign continue (label after-car))
     (assign tree (op car) (reg tree))
     (goto (label count-loop))
     after-car
     (restore tree)
     (restore continue)
     (assign tree (op cdr) (reg tree))
     (goto (label count-loop))
     null-case
     (goto (reg continue))
     leaf-case
     (assign val (op +) (reg val) (const 1))
     (goto (reg continue))
     count-done)))


;;(define t '((3) . ((2 . 3) . ((5) . (4 . 1))))) ; 6 leaves
;;(define t2 '((1 (2 3)) ((4)(5 (6 (7 8)))))) ; 8


;; Exercise 5.22
(define append-1
  (make-machine
   (list (list 'null? null?) (list 'car car) (list 'cdr cdr) (list 'cons cons))
   '(controller
     (assign continue (label append-end))
     (assign val (const '()))
     append-loop
     (test (op null?) (reg x))
     (branch (label null-case))
     ;; cons
     (save x)
     (save continue)
     (assign x (op cdr) (reg x))
     (assign continue (label after-recurse))
     (goto (label append-loop))     
     after-recurse
     (restore continue)
     (restore x)
     (assign temp (op car) (reg x))
     (assign val (op cons) (reg temp) (reg val))
     (goto (reg continue))
     null-case
     (assign val (reg y))
     (goto (reg continue))
     append-end)))

(define append-2
  (make-machine
   (list (list 'null? null?) (list 'cdr cdr) (list 'set-cdr! set-cdr!))
   '(controller
     (assign val (reg x))
     last-pair-loop
     (assign temp (op cdr) (reg x))
     (test (op null?) (reg temp))
     (branch (label after-last-pair))
     (assign x (reg temp))
     (goto (label last-pair-loop))
     after-last-pair
     (perform (op set-cdr!) (reg x) (reg y))
     append-end)))


(define x '(1 2 3))
(define y '(4 5 6))
