#lang sicp

(define (analyse exp)
  (cond ((self-evaluating? exp) (analyse-self-evaluating exp))
        ((quoted? exp) (analyse-quoted exp))
        ((variable? exp) (analyse-variable exp))
        ((assignment? exp) (analyse-assignment exp))
        ((definition? exp) (analyse-definition exp))
        ((if? exp) (analyse-if exp))
        ((let? exp) (analyse (let->lambda exp)))
        ((lambda? exp) (analyse-lambda exp))
        ((begin? exp) (analyse-sequence (begin-actions exp)))
        ((cond? exp) (analyse (cond->if exp)))
        ((amb? exp) (analyse-amb exp))
        ((application? exp) (analyse-application exp))
        (else (error "Unknown expression type -- ANALYSE" exp))))

(define (ambeval exp env succeed fail)
  ((analyse exp) env succeed fail))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

;; Exercise 4.50
(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

(define (analyse-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyse-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyse-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyse-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyse (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var old-value env)
                            (fail2)))))
             fail))))

(define (analyse-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyse (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyse-if exp)
  (let ((pproc (analyse (if-predicate exp)))
        (cproc (analyse (if-consequent exp)))
        (aproc (analyse (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

             
(define (analyse-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyse-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

(define (analyse-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyse exps)))
    (if (null? procs)
        (error "Empty sequence -- Analyse"))
    (loop (car procs) (cdr procs))))


(define (analyse-application exp)
  (let ((fproc (analyse (operator exp)))
        (aprocs (map analyse (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    ;; success continuation for this aproc
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                 env
                                 ;; success continuation for recursive
                                 ;; call to get-args
                                 (lambda (args fail3)
                                   (succeed (cons arg args) fail3))
                                 fail2))
                      fail)))

  
(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args) fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else (error "Unknown procedure type -- EXECUTE_APPLICATION" proc))))

(define (analyse-amb exp)
  (let ((cprocs (map analyse (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

;; return pair: (random-choice rest-of-list)
(define (take-random l)
  (define (loop i head tail)
    (cond ((= i 0) (cons (car tail) (list (append head (cdr tail)))))
          (else (loop (- i 1)
                      (append head (list (car tail)))
                      (cdr tail)))))
  (let ((len (length l)))
    (if (= len 0)
        '()
        (loop (random len) '() l))))

(define (analyse-ramb exp)
  (let ((cprocs (map analyse (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))


(define (let? exp)
  (tagged-list? exp 'let))

(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let->lambda exp)
  (let ((bindings (let-bindings exp)))
    (let ((vars (map car bindings))
          (exps (map cadr bindings)))
      (append (list (make-lambda vars (let-body exp))) exps))))



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
       (cdadr exp)   ; formal parameters
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

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

 
(define (distinct? l)
  (cond ((null? l) true)
        ((null? (cdr l)) true)
        ((member (car l) (cdr l)) false)
        (else (distinct? (cdr l)))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define logging false)
(define (toggle-logging)
  (set! logging (not logging)))
    
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list 'eq? eq?)
        (list 'append append)
        ;;(list 'and and)
        ;;(list 'or or)
        (list 'abs abs)
        (list 'sqrt sqrt)
        (list 'integer? integer?)
        (list '* *)
        (list '+ +)
        (list '- -)
        (list '= =)
        (list '/ /)
        (list '< <)
        (list '> >)
        (list '>= >=)
        (list '<= <=)
        (list 'list list)
        (list 'log toggle-logging)
        (list 'distinct? distinct?)))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define int-starting
  '(define (an-integer-starting-from n)
     (amb n (an-integer-starting-from (+ n 1)))))

;; exercise 4.35
(define int-between
  '(define (an-integer-between low high)
     (require (<= low high))
     (amb low (an-integer-between (+ low 1) high))))

(define pyth-triple-old
  '(define (a-pythagorean-triple-between_old low high)
     (let ((i (an-integer-between low high)))
       (let ((j (an-integer-between i high)))
         (let ((k (an-integer-between j high)))
           (require (= (+ (* i i) (* j j)) (* k k)))
           (list i j k))))))

;; Exercise 4.36
;; If we simply replace, then we will get stuck in an endless loop incrementing value of k
(define pyth-triples
  '(define (pythagorean-triples)
     (let ((k (an-integer-starting-from 1)))
       (let ((i (an-integer-between 1 k)))
         (let ((j (an-integer-between i k)))
           (require (= (+ (* i i) (* j j)) (* k k)))
           (list i j k))))))

;; Exercise 4.37
;; Yes, it's more efficient. Only two amb calculations rather than three.
(define pyth-triples-between
  '(define (a-pythagorean-triple-between low high)
     (let ((i (an-integer-between low high))
           (hsq (* high high)))
       (let ((j (an-integer-between i high)))
         (let ((ksq (+ (* i i) (* j j))))
           (require (>= hsq ksq))
           (let ((k (sqrt ksq)))
             (require (integer? k))
             (list i j k)))))))

(define multiple-dwelling
  '(define (multiple-dwelling)
     (let ((baker (amb 1 2 3 4 5))
           (cooper (amb 1 2 3 4 5))
           (fletcher (amb 1 2 3 4 5))
           (miller (amb 1 2 3 4 5))
           (smith (amb 1 2 3 4 5)))
       (require
        (distinct? (list baker cooper fletcher miller smith)))
       (require (not (= baker 5)))
       (require (not (= cooper 1)))
       (require (not (= fletcher 5)))
       (require (not (= fletcher 1)))
       (require (> miller cooper))
       ;;(require (not (= (abs (- smith fletcher)) 1)))
       (require (not (= (abs (- fletcher cooper)) 1)))
       (list (list 'baker baker)
             (list 'cooper cooper)
             (list 'fletcher fletcher)
             (list 'miller miller)
             (list 'smith smith)))))

       
;;Exercise 4.39
;; Yes, it matters because evaluation will backtrack once it hits a requirement that is false.
;; We should put the most restrictive requirements first to limit the possibilities. We should
;; also move computational expensive requirements later in the query.


;; Exercise 4.40
(define multiple-dwelling-2
  '(define (multiple-dwelling-2)
     (let ((cooper (amb 2 3 4 5))
           (fletcher (amb 2 3 4)))
       (require (not (= (abs (- fletcher cooper)) 1)))
       (let ((smith (amb 1 2 3 4 5)))
         (require (not (= (abs (- smith fletcher)) 1)))
         (let ((miller (amb 1 2 3 4 5)))
           (require (> miller cooper))
           (let ((baker (amb 1 2 3 4)))
             (require
              (distinct? (list baker cooper fletcher miller smith)))
             (list (list 'baker baker)
                   (list 'cooper cooper)
                   (list 'fletcher fletcher)
                   (list 'miller miller)
                   (list 'smith smith))))))))


;;Exercise 4.41
;; TODO
(define (multiple-dwellings-scheme)
  'todo)

;; Exercise 4.42
(define liars
  '(define (liars)
     (let ((betty (amb 1 2 3 4 5))
           (ethel (amb 1 2 3 4 5))
           (joan (amb 1 2 3 4 5))
           (kitty (amb 1 2 3 4 5))
           (mary (amb 1 2 3 4 5)))
       (require (xor (= kitty 2) (= betty 3)))
       (require (xor (= ethel 1) (= joan 2)))
       (require (xor (= joan 3) (= ethel 5)))
       (require (xor (= kitty 2) (= mary 4)))
       (require (xor (= mary 4) (= betty 1)))
       (require (distinct? (list betty ethel joan kitty mary)))
       (list
        (list 'betty betty)
        (list 'ethel ethel)
        (list 'joan joan)
        (list 'kitty kitty)
        (list 'mary mary)))))


;; Exercise 4.43
(define yachts
  '(define (yachts)
     (define (yacht pair) (car pair))
     (define (daughter pair) (cdr pair))
     (define (lister surname x)
       (list surname (yacht x) (daughter x)))
     
     (let ((downing (cons 'melissa (amb 'rosalind 'gabrielle 'lorna)))
           (hall (cons 'rosalind (amb 'gabrielle 'lorna)))
           (hood (cons 'gabrielle 'melissa))
           (moore (cons 'lorna 'mary-ann)))
       (require (not (eq? (daughter hall) (daughter downing))))
       (let ((parker (cons 'mary-ann (amb 'rosalind 'lorna))))
         (require (not (eq? (daughter hall) (daughter parker))))
         (require (not (eq? (daughter parker) (daughter downing))))
         (if (eq? (daughter hall) 'gabrielle)
             (require (eq? (yacht hall) (daughter parker)))
             (require (eq? (yacht downing) (daughter parker))))
         (list
          (lister 'downing downing)
          (lister 'hall hall)
          (lister 'hood hood)
          (lister 'moore moore)
          (lister 'parker parker))))))

;; Exercise 4.44
(define queens
  '(define (queens)
     (define (x queen) (car queen))
     (define (y queen) (cdr queen))
     (define (diagonal q1 q2)
       (define (x queen) (car queen))
       (define (y queen) (cdr queen))
       (let ((x-diff (abs (- (x q1) (x q2))))
             (y-diff (abs (- (y q1) (y q2)))))
         (= x-diff y-diff)))
     (define (in-check? q1 q2)
       (let ((x1 (x q1))
             (y1 (y q1))
             (x2 (x q2))
             (y2 (y q2)))
         (cond ((= x1 x2) true)
               ((= y1 y2) true)
               ((diagonal q1 q2) true)
               (else false))))
     (define (valid-queen? queen prev-queens)
       (cond ((null? prev-queens) true)
             ((in-check? queen (car prev-queens)) false)
             (else (valid-queen? queen (cdr prev-queens)))))
     (define (amb-xy)
       (cons (amb 1 2 3 4 5 6 7 8)
             (amb 1 2 3 4 5 6 7 8)))
     (define (loop queens-acc i)
       (if (= i 8)
           queens-acc
           (begin
             (let ((new-queen (amb-xy)))
               (require (valid-queen? new-queen queens-acc))
               (loop (append queens-acc (list new-queen)) (+ i 1))))))
     (loop '() 0)))



;; Language Parsing
;; TODO





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (or (eq? input 'try-again)
              (eq? input 't))
          (try-again)
          (begin
            (newline)
            (display ";; Starting a new problem")
            (ambeval input
                     the-global-environment
                     ;;ameval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output ";; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";; There is no current problem")
     (driver-loop))))

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

(define the-global-environment (setup-environment))

(define (eval-in-env code)
  (let ((empty-success (lambda (val next) 'success))
        (empty-fail (lambda () 'fail)))
    (ambeval code the-global-environment empty-success empty-fail)))

(define (eval-multiple statements)
  (if (null? statements)
      'done
      (begin
        (eval-in-env (car statements))
        (eval-multiple (cdr statements)))))

(eval-in-env
 '(define (require p)
    (if (not p) (amb))))

(eval-in-env
 '(define (xor a b)
    (if a (not b) b)))

(eval-multiple (list int-starting
                     multiple-dwelling
                     int-between
                     pyth-triples
                     pyth-triples-between
                     multiple-dwelling-2
                     liars
                     yachts
                     queens))

;;(driver-loop)
      
