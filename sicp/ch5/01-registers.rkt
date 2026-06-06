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

(define sqrt-2
  '(controller
    sqrt
      (assign guess (const 1.0))
    iter
    good-enough?
      (assign t (op square) (reg guess))
      (assign t (op -) (reg t) (reg x))
      (assign t (op abs) (reg t))
      (test (op <) (reg t) (const 0.001))
      (branch (label sqrt-done))
    improve
      (assign t (op /) (reg x) (reg guess))
      (assign guess (op avg) (reg t) (reg guess))
      (goto (label iter))
    sqrt-done))
      
;; Exercise 5.4

(define expt-1
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
    expt-done))

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
     

;; Machine model
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
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

(define (set-contents register value)
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
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialise-stack
                       (lambda () (stack 'initialise))))
           (register-table
            (list (list 'pc pc) (list 'flag flag)))))
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
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-registers) allocate-registers)
              ((eq? message 'get-registers) lookup-register)
              ((eq? message 'install-operation)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))
