#lang sicp

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define expressions-table (make-table))


;; Exercise 4.3
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((exp-in-table exp) (apply-table-proc exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
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
       (lambda (exp env) (eval (cond->if exp) env)))
  (put 'and eval-and)
  (put 'or eval-or)
  (put 'let eval-let)
  (put 'let* eval-let*)
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
         (apply (old-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type: EVAL" exp))))


(define (apply procedure arguments)
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
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))


(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
          (eval (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))


(define (eval-assignment exp env)
  (set-variable-value!
    (assignment-variable exp)
    (eval (assignment-value exp) env)
    env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)


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
      (caadr exp)
      (make-lambda
        (cdadr exp) ; formal parameters
        (cddr exp)))) ; body

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
  (if (not (null? cdddr exp))
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

;;
(define (lookup-variable-value exp end) 'todo)

(define (make-procedure parameters body) 'todo)

(define (primitive-procedure? proc) 'todo)

(define (apply-primitive-procedure proc args) 'todo)

(define (compound-procedure? proc) 'todo)

(define (procedure-body proc) 'todo)

(define (procedure-parameters proc) 'todo)

(define (procedure-environment proc) 'todo)

(define (extend-environment parameters arguments env) 'todo)

(define (true? predicate) 'todo)

(define (set-variable-value! variable value env) 'todo)

(define (define-variable! variable value env) 'todo)



;; Exercise 4.4
(define (eval-and exp env)
  (define (eval-predicates seq env)
    (cond ((null? seq) true)
          ((not (eval (car seq) env)) false)
          (else (eval-predicates (cdr seq) env))))
  (let ((predicates (cdr exp)))
    (eval-predicates predicates env)))

(define (eval-or exp env)
  (define (eval-predicates seq env)
    (cond ((null? seq) false)
          ((eval (car seq) env) true)
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
  (eval (let->combination exp) env))

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
  (eval (let*->nested-lets exp) env))


;;(define test-let '(let* ((x 1) (y (* x 20))) (+ x y)))
(define test-let
  '(let* ((x 5)
          (y (* x 6)) ; 30
          (z (+ x y))) ; 35
     (+ x y z))) ; 70

(define test-named-let
  '(let fib-iter ((a 1) (b 0) (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

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


(define (fib n)
  (begin
    (define (fib-iter a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))
    (fib-iter 1 0 n)))

;; Exercise 4.9
;; (do n (body))
(define template
  '(let iter ((n 0))
     (if (not (= n limie))
         (begin
           (display n)
           (iter (+ n 1)))
         'done)))

(define (do-number exp) (cadr exp))
(define (do-body exp) (caddr exp))

(define (make-named-let name bindings body)
  (list 'let name bindings body))

(define (do->let exp)
  (define (make-loop-body limit name count body)
    (let ((predicate (list 'not (list '= count limit)))
          (increment (list name (list '+ count 1))))
      (make-if
        predicate
        (sequence->exp (list body increment))
        'done)))
  
  (let ((body (do-body exp))
        (limit (do-number exp))
        (iter-name 'iter) ; should do something better
        (iter-count 'iter-count))
    (let ((bindings (list (list iter-count 0))))
      (make-named-let
        iter-name
        bindings
        (make-loop-body limit iter-name iter-count body)))))

(define test-do
  '(do 10 (display 'a)))

(install-eval-expressions)
