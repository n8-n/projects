#lang sicp

(define (flatmap f l)
  (if (null? l)
      '()
      (append (f (car l))
              (flatmap f (cdr l)))))

(define (remove-item x l eq-func)
  (define (remove-iter old-l new-l)
    (cond ((null? old-l) new-l)
          ((eq-func x (car old-l)) (append new-l (cdr old-l)))
          (else (remove-iter (cdr old-l)
                             (append new-l (list (car old-l)))))))
  (remove-iter l '()))

(define (remove-simple x l)
  (remove-item x l (lambda (a b) (eq? a b))))

(define (contains? l x)
  (cond ((null? l) false)
        ((eq? (car l) x) true)
        (else (cdr l) x)))

(define (final l)
  (if (null? l) '() (car (reverse l))))

;; Machine model
(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operation) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*)
        (tracing false))
    ;; Exercise 5.18
    (define (set-tracing! value)
      (set! tracing value)
      (if value
          'tracing-on
          'tracing-off))
    (define (trace-print value)
      (newline)
      (display "Changing ") (display name)
      (display " from ") (display contents)
      (display " to ") (display value))
    (define (internal-set! value)
      (if tracing
          (trace-print value))
      (set! contents value))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set) internal-set!)
            ((eq? message 'tracing-on) (set-tracing! true))
            ((eq? message 'tracing-off) (set-tracing! false))
            (else (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialise)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialise) (initialise))
            ((eq? message 'print-statistics) (print-statistics))
            (else (error "Unknown requests -- STACK" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

(define (make-breakpoint label n)
  (let ((current-position n))
    (lambda (message)
      (cond ((eq? message 'decrement)
             (set! current-position (- current-position 1))
             current-position)
            ((eq? message 'reset)
             (set! current-position n))
            ((eq? message 'label) label)
            ((eq? message 'n) n)
            ((eq? message 'current) current-position)
            (else (error "Unknown BREAKPOINT message -- " message))))))

(define (add-sorted l x)
  ;; fairly naive
  (define (sort-iter x left-l right-l)
    (cond ((null? left-l) (append right-l (list x)))
          ((string<? (symbol->string x) (symbol->string (car left-l)))
           (append right-l (list x) left-l))
    (else (sort-iter x (cdr left-l) (append right-l (list (car left-l)))))))
  (sort-iter x l '()))

(define (make-new-machine)
  (let ((pc (make-register 'pc)) ; program counter
        (flag (make-register 'flag)) ; for branching
        (stack (make-stack))
        (the-instruction-sequence '())
        (sorted-instructions '())
        (goto-registers '())
        (stack-registers '())
        (register-sources '())
        (instruction-count 0) ; exercise 5.15
        (tracing false) ; exercise 5.16
        (breakpoints '())
        (current-bp-label 'none)) ; breakpoints under current label
    (let ((the-ops
           (list (list 'initialise-stack
                       (lambda () (stack 'initialise)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))

      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (let ((reg (make-register name)))
              (begin
                (set! register-table (cons (list name reg) register-table))
                reg)))) ; return newly-created register
      ;; Exercise 2.13: allocate unknown registers on lookup
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (allocate-register name))))
      
      (define (trace-func inst)
        (for-each (lambda (label)
                    (newline) (display label))
                  (instruction-labels inst))
        (newline) (display (instruction-text inst)))
      
      (define (check-breaks inst)
        "Return true if should break on instruction"
        (let* ((inst-labels (instruction-labels inst))
               (has-label (not (null? inst-labels)))
               (latest-label (final inst-labels))
               (current-bp (assoc current-bp-label breakpoints))
               (should-set-new (and has-label (not (eq? latest-label current-bp-label)))))
          (cond ((null? breakpoints) false)
                (should-set-new
                 (let ((new-bp (assoc latest-label breakpoints)))
                   (if (not new-bp)
                       false
                       (begin
                         (let ((bp-proc (cdr new-bp)))
                           (set! current-bp-label latest-label)
                           (bp-proc 'reset)
                           (bp-proc 'decrement)
                           (= (bp-proc 'current) -1))))))
                (current-bp ; exists
                 (let ((bp-proc (cdr current-bp)))
                   (bp-proc 'decrement)
                   (= (bp-proc 'current) -1))) ;decide if we break
                (else false)))) ; possible?

      (define (break)
        (newline)
        (display "Breakpoint hit, pausing execution.")(newline)
        (display "Breakpoint label = ") (display current-bp-label))
      
      (define (continue inst)
        ((instruction-execution-proc inst))
        (set! instruction-count (+ 1 instruction-count))
        (execute))

      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (let ((current-inst (car insts)))
                (begin
                  (if tracing (trace-func current-inst))
                  (if (check-breaks current-inst)
                      (break)
                      (continue current-inst)))))))
      
      (define (info-add-instruction inst-tag)
        (if (not (memq inst-tag sorted-instructions))
            (set! sorted-instructions (add-sorted sorted-instructions inst-tag))))
      (define (info-add-entry-reg inst)
        (let ((reg (goto-dest (register-exp-reg inst))))
          (if (not (memq reg goto-registers))
              (set! goto-registers (cons reg goto-registers)))))
      (define (info-add-stack-reg inst)
        (let ((reg (stack-inst-reg-name inst)))
          (if (not (memq reg stack-registers))
              (set! stack-registers (cons reg stack-registers)))))
      (define (info-add-reg-source inst)
        (let* ((reg (assign-reg-name inst))
               (expr (assign-value-exp inst))
               (entry (assoc reg register-sources)))
          (if entry
              (let ((entry-sources (cadr entry)))
                (set-cdr! entry (list (append (list expr) entry-sources))))
              (set! register-sources (cons (list reg (list expr)) register-sources)))))
      (define (info-get-reg-source reg)
        (assoc reg register-sources))
      
      (define (get-information)
        (list (list 'sorted-instructions sorted-instructions)
              (list 'goto-registers goto-registers)
              (list 'stack-registers stack-registers)
              (list 'register-sources register-sources)))
      
      (define (instruction-count-reset)
        (set! instruction-count 0)
        'reset)
      
      (define (set-tracing! value)
        (set! tracing value)
        (if value
            'tracing-on
            'tracing-off))
      
      (define (all-labels) (flatmap cddr the-instruction-sequence))
      
      (define (add-breakpoint label n)
        (define (valid-label? labels)
          (cond ((null? labels) false)
                ((eq? label (car labels)) true)
                (else (valid-label? (cdr labels)))))
        (if (valid-label? (all-labels))
            (let* ((max-n (max n 0))
                   (bp-temp (cons label (make-breakpoint label max-n))))
              (set! breakpoints (cons bp-temp breakpoints)))
            (begin (display "Label not found in instructions: ")
                   (display label) (newline))))
      
      ;; TODO: fix this
      (define (remove-breakpoint label n)
        (define (bp-eq x bp)
          (and (eq? ((cdr bp) 'label) label)
               (= ((cdr bp) 'n) n)))
        (let ((bp (assoc label breakpoints)))
          (if (not bp)
              (error "Breakpoint not found -- " label)
              (remove-item 'x breakpoints bp-eq))))
      
      (define (proceed-execution)
        ;; reset breakpoints
        (for-each
         (lambda (bp) ((cdr bp) 'reset))
         breakpoints)
        (set! current-bp-label 'none)
        (execute))
      
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'get-instructions) the-instruction-sequence)
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operation)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'registers) register-table)
              ;; Information operations
              ((eq? message 'info-add-instruction) info-add-instruction)
              ((eq? message 'info-add-entry-reg) info-add-entry-reg)
              ((eq? message 'info-add-stack-reg) info-add-stack-reg)
              ((eq? message 'info-add-reg-source) info-add-reg-source)
              ((eq? message 'get-information) (get-information))
              ((eq? message 'get-reg-source) info-get-reg-source)
              ((eq? message 'instruction-count) instruction-count)
              ((eq? message 'instruction-count-reset) (instruction-count-reset))
              ((eq? message 'tracing-on) (set-tracing! true))
              ((eq? message 'tracing-off) (set-tracing! false))
              ((eq? message 'add-breakpoint) add-breakpoint)
              ((eq? message 'list-breakpoints) breakpoints)
              ((eq? message 'cancel-breakpoint) remove-breakpoint)
              ((eq? message 'cancel-all-breakpoints)
               (set! current-bp-label 'none)
               (set! breakpoints '()))
              ((eq? message 'proceed) (proceed-execution))
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
                                  (begin
                                    (if (not (null? insts))
                                        (add-instruction-label! (car insts) next-inst))
                                    (receive
                                      insts
                                      (cons (make-label-entry next-inst insts) labels))))
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

(define (make-instruction text) (cons text (cons '() '())))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cadr inst))
(define (instruction-labels inst) (cddr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-car! (cdr inst) proc))
(define (set-instruction-labels! inst labels)
  (set-cdr! (cdr inst) labels))
(define (add-instruction-label! inst label)
  (set-instruction-labels! inst (cons label (instruction-labels inst))))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))


;; Exercise 5.12
;; Might be a better way to do this, but adding the info logging
;; functionality here.
(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (let ((instruction (car inst)))
    ((machine 'info-add-instruction) instruction)
    (cond ((eq? instruction 'assign)
           ((machine 'info-add-reg-source) inst)
           (make-assign inst machine labels ops pc))
          ((eq? instruction 'test)
           (make-test inst machine labels ops flag pc))
          ((eq? instruction 'branch)
           (make-branch inst machine labels flag pc))
          ((eq? instruction 'goto)
           ((machine 'info-add-entry-reg) inst)
           (make-goto inst machine labels pc))
          ((eq? instruction 'save)
           ((machine 'info-add-stack-reg) inst)
           (make-save inst machine stack pc))
          ((eq? instruction 'restore)
           ((machine 'info-add-stack-reg) inst)
           (make-restore inst machine stack pc))
          ((eq? instruction 'perform)
           (make-perform inst machine labels ops pc))
          (else (error "Unknown instruction type -- ASSEMBLE" inst)))))

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

;; Exercise 5.19
(define (set-breakpoint machine label n)
  ((machine 'add-breakpoint) label n))

(define (proceed-machine machine)
  ((machine 'proceed)))

(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))

(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))


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
   (list (list 'square square) (list '/ /) (list 'avg avg)
         (list 'abs abs) (list '- -) (list '< <))
   '(sqrt
     (assign guess (const 1.0))
     iter
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


;; Exercise 5.14
(define fact-1
  (make-machine
   (list (list '= =) (list '- -) (list '* *))
   '(controller
     (assign continue (label fact-done))     ; set up final return address
     (perform (op initialise-stack))
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     ;; Set up for the recursive call by saving n and continue.
     ;; Set up continue so that the computation will continue
     ;; at after-fact when the subroutine returns.
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
     (goto (reg continue))                   ; return to caller
     base-case
     (assign val (const 1))                  ; base case: 1! = 1
     (goto (reg continue))                   ; return to caller
     fact-done
     (perform (op print-stack-statistics)))))


(define (fact-stats-run n)
  (if (= n 0)
      'done
      (begin
        (newline)(newline)
        (display "Running factorial func for n = ") (display n)
        (run fact-1 (list (list 'n n)) 'val)
        (fact-stats-run (- n 1)))))

;; total-pushes = 2n - 2

(define (debug)
  (fact-1 'tracing-on)
  (set-breakpoint fact-1 'fact-loop 0))
