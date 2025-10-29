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

(define (triple-sums n s)
  (define (triple-sums-iter n s)
    (define (summer i j k)
      (let ((result (list k j i)))
        ;(display result)
        (if (= s (+ i j k))
            result
            '())))
    (flatmap (lambda (i)
               (flatmap (lambda (j)
                          (map (lambda (k) (summer i j k))
                               (enumerate-interval 1 j)))
                        (enumerate-interval 1 i)))
             (enumerate-interval 1 n)))
  (filter (lambda (l) (not (null? l)))
          (triple-sums-iter n s)))











;; 2.3.1
(define (equal2? l1 l2)
  (cond ((and (symbol? l1)
              (symbol? l2))
         (eq? l1 l2))
        ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        ((and (list? l1)
              (list? l2)
              (equal2? (car l1) (car l2))
              (equal2? (cdr l1) (cdr l2)))
         #t)
        (else #f)))
      



;; 2.3.2
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-exponentiation (make-product (exponent exp) (base exp))
                               (- (exponent exp) 1))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))



(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base ex) (cadr ex))
(define (exponent ex) (caddr ex))

(define (make-exponentiation b ex)
  (cond ((or (=number? b 0) (=number? ex 0)) 0)
        ((=number? b 1) 1)
        ((=number? ex 1) b)
        (else (list '** b ex))))




;; 2.3.3  SETS

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set2) set1))
         (union-set (append set1 (list (car set2)))
                    (cdr set2)))
        (else (union-set set1 (cdr set2)))))


;; allowing duplicates in list
; element-of-set? and intersection-set should be the same

(define (adjoin-set2 x set2)
  (cons x set2))

(define (union-set2 set1 set2)
  (append set1 set2))


(define (adjoin-set-ord x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set-ord x (cdr set))))))

(define (union-set-ord s1 s2)
  (define (iter result set1 set2)
    (cond ((null? set1) (append result set2))
          ((null? set2) (append result set1))
          (else
           (let ((a (car set1)) (b (car set2)))
             (cond ((= a b)
                    (iter (append result (list a))
                          (cdr set1)
                          (cdr set2)))
                   ((< a b)
                    (iter (append result (list a))
                          (cdr set1)
                          set2))
                 (else (iter (append result (list b))
                             set1
                             (cdr set2))))))))
  (iter '() s1 s2))


;; Tree
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define t1
  (let ((mt make-tree))
    (mt 7 (mt 3 (mt 1 '() '())
              (mt 5 '() '()))
        (mt 9 '() (mt 11 '() '())))))
