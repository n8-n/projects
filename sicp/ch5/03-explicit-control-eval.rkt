#lang sicp

(#%require "01-registers.rkt")

;; TODO: add require for syntax parsing from chapter 4


(define evaluator
  (make-machine
   (list (list 'self-evaluating? self-evaluating?) (list 'quoted? quoted?)
          (list 'assignment? assignment?) (list 'definition? definition?)
          (list 'if? if?) (list 'lambda? lambda?) (list 'begin? begin?)
          (list 'application? application?) (list 'variable? variable?)
          (list 'lookup-variable-value lookup-variable-value) (list 'empty-arglist empty-arglist)
          (list 'text-of-quotation text-of-quotation) (list 'lambda-parameters lambda-parameters)
          (list 'lambda-body lambda-body) (list 'make-procedure make-procedure)
          (list 'operands operands) (list 'operator operator) (list 'no-operands? no-operands?)
          (list 'first-operand first-operand) (list 'last-operand? last-operand?)
          (list 'adjoin-arg adjoin-arg) (list 'rest-operands rest-operands)
          
          )
   '(eval-dispatch
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))

     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))
     ev-variable
     (assign val (op lookup-variable-value) (reg exp) (reg env))
     (goto (reg continue))
     ev-quoted
     (assign val (op text-of-quotation) (reg exp))
     (goto (reg continue))
     ev-lambda
     (assign unev (op lambda-parameters) (reg exp))
     (assign exp (op lamda-body) (reg exp))
     (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
     (goto (reg continue))

     ev-application
     (save continue)
     (save env)
     (assign unev (op operands) (reg exp))
     (save unev)
     (assign exp (op operator) (reg exp))
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))

     ev-appl-did-operator
     (restore unev) ; operands
     (restore env)
     (assign argl (op empty-arglist))
     (assign proc (reg val)) ; operator
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
     (save proc)

     ev-appl-operand-loop
     (save argl)
     (assign exp (op first-operand) (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg))
     (save env)
     (save unev)
     (assign continue (label ev-appl-accumulate-arg))
     (goto (label eval-dispath))

     ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (assign unev (op rest-operands) (reg unev))
     (goto (label ev-appl-operand-loop))

     ev-appl-last-arg
     (assign continue (label ev-appl-accum-last-arg))
     (goto (lavel eval-dispatch))
     ev-appl-accum-last-arg
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (restore proc)
     (goto (label apply-dispatch))

     ;; apply-dispatch
      )))
