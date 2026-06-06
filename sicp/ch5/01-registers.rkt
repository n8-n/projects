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
                              (receive insts (cons (make-label-entry next-inst insts)
                                                   labels))
                              (receive (cons (make-instruction next-inst) insts)
                                  labels)))))))

(define (update-insts! insts labels machine)
  (let ((px (get-register machine 'pc))
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

(define (make-instuction text) (cons text '()))
(define (instruction-text inst) (car insts))
(define (instruction-execution-proc inst) (cdr inst))
(define (set-instruction-execution-proc! inst proc) (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cond label-name insts))

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

(define (make-assign inst machin labels operations pc)
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

;; Continue at make-branch
