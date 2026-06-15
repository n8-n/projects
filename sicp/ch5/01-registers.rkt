#lang sicp

;; Machine model
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operation) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialise)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialise) (initialise))
            (else (error "Unknown requests -- STACK" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))


(define (make-new-machine)
  (let ((pc (make-register 'pc)) ; program counter
        (flag (make-register 'flag)) ; for branching
        (stack (make-stack))
        (the-instruction-sequence '())
        (sorted-instructions '())
        (goto-registers '())
        (stack-registers '())
        (register-sources '()))
    (let ((the-ops
           (list (list 'initialise-stack
                       (lambda () (stack 'initialise)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (information type)
        (cond ((eq? type 'instructions) 'todo)
              ((eq? type 'registers) 'todo)
              ((eq? type 'stack-operations) 'todo)
              ((eq? type 'register-sources) 'todo)
              (else (error "Unknown information type -- MACHINE" type))))
      (define (create-information-lists text) 'todo)
        
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operation)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'create-information-lists) create-information-lists)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (start machine)
  (machine 'start))

;; start machine with initialised registers, and print final return
(define (run machine reg-bindings return)
  (let ((reg-setup (lambda (reg-binding)
                     (set-register-contents! machine
                                             (car reg-binding)
                                             (cadr reg-binding)))))
    (for-each reg-setup reg-bindings)
    (machine 'start)
    (get-register-contents machine return)))

(define (start-print machine register)
  (machine 'start)
  (get-register-contents machine register))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))


;; Assembler
(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              ;; Exercise 5.8
                              (if (assoc next-inst labels)
                                  (error "Label is already defined!" next-inst)
                                  (receive
                                   insts
                                   (cons (make-label-entry next-inst insts) labels)))
                              (receive
                               (cons (make-instruction next-inst) insts)
                               labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure (instruction-text inst) labels machine
                                  pc flag stack ops)))
     insts)))

(define (make-instruction text) (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cdr inst))
(define (set-instruction-execution-proc! inst proc) (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))


(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE" inst))))

(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda () ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instructions) (cadr assign-instructions))
(define (assign-value-exp assign-instructions) (cddr assign-instructions))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction) (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label labels (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (get-register machine (register-exp-reg dest))))
             (lambda () (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE" inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts (lookup-label labels (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (make-operation-exp exp machine labels operations)
  (if (assoc 'label exp)
      (error "Cannot operate on a label!" exp)
      (let ((op (lookup-prim (operation-exp-op exp) operations))
            (aprocs
             (map (lambda (e) (make-primitive-exp e machine labels))
                  (operation-exp-operands exp))))
        (lambda ()
          (apply op (map (lambda (p) (p)) aprocs))))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))


;; shortcuts
(define src! set-register-contents!)
(define grc get-register-contents)


;; Exercise 5.2
(define fac-iter-machine1
  '(controller
    factorial
    (assign product 1)
    (assign count 1)
    iter
    (test (op >) (reg count) (reg n))
    (branch (label fac-done))
    (assign product (op *) (reg product) (reg count))
    (assign count (op +1) (reg count))
    (goto (label iter))
    fac-done))


;; Exercise 5.3
(define sqrt-1
  '(controller
    sqrt
    (assign guess (const 1.0))
    iter
    (test (op good-enough?) (reg guess))
    (branch (label sqrt-done))
    (assign guess (op improve) (reg guess))
    sqrt-done))

(define (square x) (* x x))

(define (avg a b)
  (/ (+ a b) 2))

(define sqrt-2
  (make-machine
   '(guess t x)
   (list (list 'square square) (list '/ /) (list 'avg avg)
         (list 'abs abs) (list '- -) (list '< <))
   '(sqrt
     (assign guess (const 1.0))
     iter
     ;;good-enough?
     (assign t (op square) (reg guess))
     (assign t (op -) (reg t) (reg x))
     (assign t (op abs) (reg t))
     (test (op <) (reg t) (const 0.001))
     (branch (label sqrt-done))
     improve
     (assign t (op /) (reg x) (reg guess))
     (assign guess (op avg) (reg t) (reg guess))
     (goto (label iter))
     sqrt-done)))


;; Exercise 5.4
(define expt-1
  (make-machine
   '(continue n b val)
   (list (list '= =) (list '- -) (list '* *))
   '(controller
     (assign continue (label expt-done))
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     ;; setup recursive call
     (save continue)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-expt))
     (goto (label expt-loop))
     after-expt
     (restore continue)
     (assign val (op *) (reg b) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     expt-done)))

(define expt-2
  '(controller
    (assign (reg count) (reg n))
    (assign (reg n) (const 1))
    expt-iter
    (test (op =) (reg count) (const 0))
    (branch (label expt-done))
    (assign count (op -) (reg count) (const 1))
    (assign n (op *) (reg b) (reg n))
    (goto (label expt-iter))
    expt-done))


(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))




;; Exercise 5.10
;; Could modify operation initialisation to add the colon automatically
;; e.g. in (list (list 'print print1) ...)
;; Don't know if there's a better way than parsing symbols to strings

;; (define (operation-exp? exp)
;;   (define (first-letter-colon? x)
;;     (equal? #\: (string-ref (symbol->string x) 0)))
;;   (and (pair? exp) (first-letter-colon? (car exp))))
;; (define (operation-exp-op operation-exp)
;;   (car operation-exp))
;; (define (operation-exp-operands operation-exp)
;;   (cdr operation-exp))

;; (define (print1 label) (display "> ") (display label) (newline))

;; (define op-test
;;   (make-machine
;;    '(a)
;;    (list (list ':print print1) (list ':= =) (list ':inc (lambda (x) (+ x 1))))
;;    '(start
;;      (test := (reg a) (const 5))
;;      (branch (label done))
;;      (perform :print (reg a))
;;      (assign a :inc (reg a))
;;      (goto (label start))
;;      done)))


(define fib1
  (make-machine
   '(n continue val)
   (list (list '< <) (list '- -) (list '+ +))
   '(controller
     (assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n - 1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)
     (assign n (op -) (reg n) (const 1))
     (goto (label fib-loop))
     afterfib-n-1
     (restore n)
     ;; set up to compute Fib(n - 2)
     (assign n (op -) (reg n) (const 2))
     (assign continue (label afterfib-n-2))
     (save val)
     (goto (label fib-loop))
     afterfib-n-2
     (assign n (reg val))
     (restore val)
     (restore continue)
     (assign val (op +) (reg val) (reg n))
     (goto (reg continue))
     immediate-answer
     (assign val (reg n))
     (goto (reg continue))
     fib-done)))


;; Exercise 5.11
;; question a:
;; in afterfib-n-2
;; (assign n (reg val))
;; (restore val)
;; replaced with => (restore n)




;;; TODO for exercise 5.12
;; where to create these lists? during assemble, or do it separately?
;; separately might be easier, but will have to go through controller text multiple times.
;; If doing it in assemble, look at make-execution-procedure?
