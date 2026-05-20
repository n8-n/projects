#lang sicp

(define (square x)
  (* x x))


(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                      (list (square (car things)))))))
  (iter items nil))

(define (reverse1 l)
  (define (iter curr answer)
    (if (null? curr)
        answer
        (iter (cdr curr)
              (cons (car curr) answer))))
  (iter l nil))


(define (reverse2 l)
  (if (null? l)
      nil
      (append (reverse2 (cdr l))
              (list (car l)))))


(define (reverse-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) tree)
        (else (append (reverse-tree (cdr tree))
                      (list (reverse-tree (car tree)))))))


(define (fringe tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))



;;(reverse2 '(1 2 3 4))
;;(define test-l '((1 2) (3 4)))
;;(display test-l)
;;(reverse-tree test-l)


(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (cond ((not (number? length)) (error "length must be number"))
        (else(list length structure))))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))


(define (branch-structure branch)
  (cadr branch))

(define (mobile-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (mobile-weight (branch-structure (right-branch mobile)))
                 (mobile-weight (branch-structure (left-branch mobile)))))))



(define mb (make-mobile
            (make-branch 5 5)
            (make-branch 4 6)))

(define mb2 (make-mobile
             (make-branch 5 mb)
             (make-branch 4 6)))


(define mb3 (make-mobile
             (make-branch 5 mb)
             (make-branch 4 mb2)))


(define (square-tree t)
  (define (mapper sub-tree)
    (if (pair? sub-tree)
        (square-tree sub-tree)
        (* sub-tree sub-tree)))
  (map mapper t))



(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;;(subsets '(1 2 3))


(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))


(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))


(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))


(define (map2 p seq)
  (accumulate (lambda (x y) (cons (p x) y)) nil seq))

(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length2 seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))



(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


(define fold-right accumulate)


(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(define (reverse-fr seq)
  (fold-right (lambda (x y) (append y (list x))) nil seq))


(define (reverse-fl seq)
  (fold-left (lambda (x y) (cons y x)) nil seq))


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list k j i))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (triples-sum n s)
  (filter (lambda (t) (= s (apply + t)))
          (unique-triples n)))



;; Exercises chapter 3
(define (make-accumulator total)
  (lambda (x)
    (set! total (+ x total))
    total))


(define (make-monitored func)
  (let ((calls 0))
    (lambda (x)
      (if (eq? x 'how-many-calls?)
          calls
          (begin (set! calls (+ calls 1))
                 (func x))))))



(define (make-account balance password)
  (let ((limit 0))
    (define (call-the-cops)
      (error "CALLING THE COPS!"))
    (define (reset-limit) (set! limit 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch p m)
      (cond ((>= limit 3)
             (call-the-cops))
            ((not (eq? password p))
             (set! limit (+ limit 1))
             (error "Unknown password!"))
            ((eq? m 'withdraw) (begin (reset-limit) withdraw))
            ((eq? m 'deposit) (begin (reset-limit) deposit))
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))



;; Monte Carlo
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (rand)
  (random 1000))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(define (estimate-integral pred x1 x2 y1 y2 trials)
  (define (pred-test)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (pred x y)))
  (let ((area (* (- x2 x1) (- y2 y1))))
    (* (monte-carlo trials pred-test) area)))

(define (p1 x y)
  (<= (+ (expt (- x 5) 2)
         (expt (- y 7) 2))
      (expt 3 2)))


(define (p? x y)
  (<= (+ (* x x) (* y y)) 1))


;; Exercise 3.6

(define random-init 4)
(define (rand-update x)
  (modulo (+ 33 (* x 173)) 101))

(define rand2
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define rand3
  (let ((x random-init))
    (lambda (message)
      (cond ((eq? message 'generate)
             (set! x (rand-update x))
             x)
            ((eq? message 'reset)
             (lambda (x-new) (set! x x-new)))
            (else (error "Unknown message!"))))))



(define-syntax λ
  (syntax-rules ()
    ((_ args body)
     (lambda args body))))





(define (cons: x y)
  (λ (M)
    (M x
       y
       (λ (n) (set! x n))
       (λ (n) (set! y n)))))

(define (car: pair)
  (pair (λ (x y sx sy) x)))

(define (cdr: pair)
  (pair (λ (x y sx sy) y)))

(define (set-car:! pair n)
  (pair (λ (x y sx sy) (sx n))))

(define (set-cdr:! pair n)
  (pair (λ (x y sx sy) (sy n))))


(define (display-pair pair)
  (pair (λ (x y sx sy)
          (begin (display "(")
                 (display x)
                 (display " ")
                 (display y)
                 (display ")")
                 (newline)))))
