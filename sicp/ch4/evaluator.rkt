#lang sicp

(#%require "../util/table.rkt")

(define expressions-table (make-table))


;; Exercise 4.3
(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((exp-in-table exp) (apply-table-proc exp env))
        ((application? exp)
         (my-apply (my-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (exp-in-table exp)
  (lookup (car exp) expressions-table))

(define (apply-table-proc exp env)
  ((exp-in-table exp) exp env))

(define (install-eval-expressions)
  (define (put key value)
    (insert! key value expressions-table))
  
  (put 'quote
       (lambda (exp _) (text-of-quotation exp)))
  (put 'set! eval-assignment)
  (put 'define eval-definition)
  (put 'if eval-if)
  (put 'lambda
       (lambda (exp env) (make-procedure (lambda-parameters exp)
                                         (lambda-body exp)
                                         env)))
  (put 'begin
       (lambda (exp env) (eval-sequence (begin-actions exp) env)))
  (put 'cond
       (lambda (exp env) (my-eval (cond->if exp) env)))
  (put 'and eval-and)
  (put 'or eval-or)
  (put 'let eval-let)
  (put 'let* eval-let*)
  (put 'letrec eval-letrec)
  'done)


(define (old-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (old-eval (cond->if exp) env))
        ((application? exp)
         (my-apply (old-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type: EVAL" exp))))


(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error "Unknown procedure type: APPLY" procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))


(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (my-eval (first-exp exps) env))
        (else
          (my-eval (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))


(define (eval-assignment exp env)
  (set-variable-value!
    (assignment-variable exp)
    (my-eval (assignment-value exp) env)
    env)
  'ok-assign)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (my-eval (definition-value exp) env)
    env)
  'ok-define)


;; Expressions
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda
        (cdadr exp) ; formal parameters
        (cddr exp)))) ; body

;; if you want functions to be e.g. (define func (x) (* x 10))
  ;; (if (= (length exp) 3)
  ;;     (caddr exp)
  ;;     (make-lambda
  ;;       (caddr exp)
  ;;       (cdddr exp))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


;; Derived expressions
(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))


;; Exercise 4.5
(define (cond-recipient-clause? exp) (eq? (cadr exp) '=>))

(define (cond-recipient->exp exp)
  (define (cond-recipient-proc exp) (caddr exp))
  (list (cond-recipient-proc exp) (cond-predicate exp)))


(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last -- COND->IF" clauses)))
              ((cond-recipient-clause? first)
               (make-if (cond-predicate first)
                        (cond-recipient->exp first)
                        (expand-clauses rest)))
              (else (make-if (cond-predicate first)
                             (sequence->exp (cond-actions first))
                             (expand-clauses rest)))))))


;; exercise 4.2 b
(define (louis-application? exp)
  (tagged-list? 'call exp))
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

;; Exercise 4.16
(define (split-defines seq)
  (if (null? seq)
      '()
      (let ((first (car seq)))
        (if (definition? first)
            (cons (list (definition-variable first) (definition-value first))
                  (split-defines (cdr seq)))
            (split-defines (cdr seq))))))
      
(define (scan-out-defines exp)
  (define (make-unassigned var) (list var ''*unassigned*))
  (define (make-set! var body) (list 'set! var body))
  (define (make-lets-n-sets lets sets pairs)
    (if (null? pairs)
        (list lets sets)
        (let ((first (car pairs)))
          (let ((var (car first))
                (value (cadr first)))
            (make-lets-n-sets (append lets (list (make-unassigned var)))
                              (append sets (list (make-set! var value)))
                              (cdr pairs))))))
  (define (get-body exp)
    (if (= (length exp) 1)
        (car exp)
        (get-body (cdr exp))))
  (if (= (length exp) 1)
      exp
      (let ((lets-sets (make-lets-n-sets '() '() (split-defines exp))))
        (let ((lets (car lets-sets))
              (sets (cadr lets-sets)))
          (list (list 'let lets (cons
                                 'begin
                                 (append sets (list (get-body exp))))))))))


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define logging false)
(define (toggle-logging)
  (set! logging (not logging)))
    
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '* *)
        (list '+ +)
        (list '- -)
        (list '= =)
        (list '/ /)
        (list '< <)
        (list '> >)
        (list '>= >=)
        (list '<= <=)
        (list 'log toggle-logging)))
      

(define (apply-primitive-procedure proc args)
  ;; uses underlying scheme apply
  (apply (primitive-implementation proc) args))

(define (compound-procedure? proc)
  (tagged-list? proc 'procedure))

(define (procedure-body proc) (caddr proc))
(define (procedure-parameters proc) (cadr proc))
(define (procedure-environment proc) (cadddr proc))

(define (primitive-implementation proc) (cadr proc))
         
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))


(define (unassigned? value)
  (eq? value '*unassigned*))

;; exercise 4.12
(define (lookup-variable-value var env)
  (define (env-loop env)    
    (if (eq? env the-empty-environment)
        (error "Unbound variable!" var)
        (let ((frame (first-frame env))
              (next-env (lambda () (env-loop (enclosing-environment env))))
              (get-value (lambda (pairs)
                           (let ((value (var-value (first-var pairs))))
                             (if (unassigned? value)
                                 (error "Variable value is *unassigned*")
                                 value)))))
          (frame-scan frame var next-env get-value))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env))
              (next-env (lambda () (env-loop (enclosing-environment env))))
              (update (lambda (pairs) (update-frame-pair pairs var val))))
          (frame-scan frame var next-env update))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((add (lambda () (add-binding-to-frame! var val frame)))
          (update (lambda (pairs) (update-frame-pair pairs var val))))
      (frame-scan frame var add update))))

(define (update-frame-pair pairs var val)
  (begin
     (set-car! pairs (list var val))
     (set-cdr! pairs (cdr pairs))))

(define (frame-scan init-pairs var null-proc eq-proc)
  (define (loop pairs)
    (cond ((null? pairs) (null-proc))
          ((eq? var (var-name (first-var pairs))) (eq-proc pairs))
          (else (loop (cdr pairs)))))
  (loop init-pairs))

;; Exercise 4.13
;; We'll make it so that it only unbounds for the current frame.
;; A user might expect that it would completely unbind the variable for all frames, but I don't
;; think a frame should have the ability to modify its enclosing environment. Too messy.
;; NOTE: not working properly: can't get it to remove final element.
(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (define not-found
      (lambda ()
        (error "Variable not found in current frame." var frame)))
    (define unbind
      (lambda (pairs)
        (begin (set-car! pairs (cadr pairs))
               (set-cdr! pairs (cddr pairs)))))
    (frame-scan frame var not-found unbind)))


(define (first-var frame) (car frame))
(define (var-name var) (car var))
(define (var-value var) (cadr var))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (if (= (length variables) (length values))
      (map list variables values)
      (error "Lists are not the same length -- make-frame" variables values)))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cadr frame))
(define (add-binding-to-frame! var val frame)
  ;; setting car of null doesn't work. can frames be empty? 
  (if (null? frame)
      (set-car! frame (cons (list var val) '()))
      (set-cdr! frame (cons (list var val) (cdr frame)))))


;; old environment structure
; (define env '(((a b) 1 2) (older frame)))

;; new environment structure, Exercise 4.11
;; list of bindings 
(define test-env '(((a 10) (b 40) (c 50)) ((x 2))))
(define frame-test (first-frame test-env))

;; Exercise 4.4
(define (eval-and exp env)
  (define (eval-predicates seq env)
    (cond ((null? seq) true)
          ((not (my-eval (car seq) env)) false)
          (else (eval-predicates (cdr seq) env))))
  (let ((predicates (cdr exp)))
    (eval-predicates predicates env)))

(define (eval-or exp env)
  (define (eval-predicates seq env)
    (cond ((null? seq) false)
          ((my-eval (car seq) env) true)
          (else (eval-predicates (cdr seq) env))))
  (let ((predicates (cdr exp)))
    (eval-predicates predicates env)))

;;Exercise 4.6
;; returns two lists: ((variables) (expressions))
(define (split-let-bindings bindings)
  (define (split-bindings-iter bindings vars exps)
    (if (null? bindings)
        (list vars exps)
        (let ((first (car bindings))
              (rest (cdr bindings)))
          (if (pair? first)
              (let ((new-vars (append vars (list (car first))))
                    (new-exps (append exps (list (cadr first)))))
                (split-bindings-iter rest new-vars new-exps))
              (error "Invalid syntax in LET bindings: should be pair -- " first)))))
  (split-bindings-iter bindings '() '()))

(define (let-bindings exp) (cadr exp))
(define (let-body exp) (caddr exp))
(define (let-split-get-vars split) (car split))
(define (let-split-get-exps split) (cadr split))

(define (let? exp) (tagged-list? exp 'let))
(define (let*? exp) (tagged-list? exp 'let*))

(define (make-let-lambda vars body expressions)
  (append (list (list 'lambda vars body)) expressions))

(define (let->combination exp)
  (if (named-let? exp)
      (named-let->define exp)
      (let ((split (split-let-bindings (let-bindings exp))))
        (let ((variables (let-split-get-vars split))
              (expressions (let-split-get-exps split))
              (body (let-body exp)))
          (make-let-lambda variables body expressions)))))

(define (eval-let exp env)
  (my-eval (let->combination exp) env))

;; exercise 4.7
(define (let*->nested-lets exp)
  (define (let*-expand-iter variables expressions body)
    (if (null? variables)
        body
        (let ((inner-body (let*-expand-iter (cdr variables) (cdr expressions) body))
              (current-var (list (car variables)))
              (current-expression (list (car expressions))))
          (make-let-lambda current-var inner-body current-expression))))
  (let ((split (split-let-bindings (let-bindings exp))))
    (let ((variables (let-split-get-vars split))
          (expressions (let-split-get-exps split))
          (body (let-body exp)))
      (let*-expand-iter variables expressions body))))

(define (eval-let* exp env)
  (my-eval (let*->nested-lets exp) env))


;;(define test-let '(let* ((x 1) (y (* x 20))) (+ x y)))
;; (define test-let
;;   '(let* ((x 5)
;;           (y (* x 6)) ; 30
;;           (z (+ x y))) ; 35
;;      (+ x y z))) ; 70

;; (define test-named-let
;;   '(let fib-iter ((a 1) (b 0) (count n))
;;     (if (= count 0)
;;         b
;;         (fib-iter (+ a b) a (- count 1)))))

;; Exercise 4.8
(define (named-let? exp) (variable? (cadr exp)))
(define (named-let-name exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cadddr exp))
(define (make-define name vars body)
  (list 'define (cons name vars) body))

(define (named-let->define exp)
  (let ((name (named-let-name exp))
        (split (split-let-bindings (named-let-bindings exp))))
    (let ((vars (let-split-get-vars split))
          (exps (let-split-get-exps split))
          (body (named-let-body exp)))
      (sequence->exp (list (make-define name vars body) (cons name exps))))))


;; Exercise 4.9
;; (do (n 0) (body))
(define template
  '(let iter ((n 0))
     (if (not (= n limit))
         (begin
           (display n)
           (iter (+ n 1)))
         'done)))

(define (do-body exp) (caddr exp))
(define (do-count-var exp) (caadr exp))
(define (do-count-limit exp) (cadadr exp))

(define (make-named-let name bindings body)
  (list 'let name bindings body))

(define (do->let exp)
  (define (make-loop-body limit func-name count body)
    (let ((predicate (list 'not (list '= count limit)))
          (increment (list func-name (list '+ count 1))))
      (make-if
       predicate
       (sequence->exp (list body increment))
       '(display 'done))))

  (let ((body (do-body exp))
        (limit (do-count-limit exp))
        (iter-var-name (do-count-var exp)))
    (let ((bindings (list (list iter-var-name 0)))
          (iter-func (string->symbol (string-append "iter-" (symbol->string iter-var-name)))))
      (make-named-let
       iter-func
       bindings
       (make-loop-body limit iter-func iter-var-name body)))))

;; (define test-do
;;   '(do (n 10) (display n)))


;; Exercise 4.20
(define (eval-letrec exp env)
  (my-eval (letrec->let exp) env))

(define (letrec->define exp)
  (let ((name (car exp))
        (body (cadr exp)))
    (list 'define name body)))

(define (letrec-lets exp) (cadr exp))
(define (letrec-body exp) (caddr exp))

(define (letrec->let exp)
  (define (to-define-iter lets defines)
    (if (null? lets)
        defines
        (to-define-iter (cdr lets)
                        (cons (letrec->define (car lets)) defines))))                              
  (let* ((lets (letrec-lets exp))
         (as-defines (to-define-iter (reverse lets) '())))
    (car (scan-out-defines (append as-defines (list (letrec-body exp)))))))

(define letrec-test '(letrec ((fact
                               (lambda (n)
                                 (if (= n 1)
                                     1
                                     (* n (fact (- n 1)))))))
                       (fact 10)))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read))
        (starttime (runtime)))
    (let ((output (my-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)
      (if logging
          (begin
            (newline) (display "Time taken: ") (display (- (runtime) starttime))))))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;; (define t-define '(define (sq x)
;;                     (define (final y) (y x x))
;;                     (define (other z) (final z))
;;                     (other *)))


(define the-global-environment (setup-environment))
;;(install-eval-expressions)
;;(driver-loop)


(#%provide
 let?
 let*?
 let->combination
 let*->nested-lets)
