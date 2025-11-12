#lang sicp


(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map-old proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map-old proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line l)
  (newline)
  (display l))



(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


;; Exercise 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


;; Exercise 3.51
(define (show x)
  (display-line x)
  x)



(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))


(define (divisible? num to-test)
  (= 0 (remainder num to-test)))


(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x) (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))


(define (take num stream)
  (if (= num 0)
      the-empty-stream
      (cons-stream (stream-car stream)
                   (take (- num 1) (stream-cdr stream)))))


(define (display-stream-n n stream)
  (display-stream (take n stream)))


(define (add-streams s1 s2)
  (stream-map + s1 s2))


(define fibs
  (cons-stream 0 (cons-stream 1
                              (add-streams (stream-cdr fibs) fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))


;; Exercise 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams integers factorials)))


;; Exercise 3.55
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s) (partial-sums s))))


(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

;; (define (interleave s1 s2)
;;   (cond ((stream-null? s1) s2)
;;         ((stream-null? s2) s1)
;;         (else
;;          (cons-stream (stream-car s1)
;;                       (cons-stream (stream-car s2)
;;                                    (interleave (stream-cdr s1)
;;                                                (stream-cdr s2)))))))

(define (interleave s t)
  (if (stream-null? s)
      t
      (cons-stream (stream-car s)
                   (interleave t (stream-cdr s)))))

(define (repeat-stream x)
  (cons-stream x (repeat-stream x)))


;; Exercise 5.56
(define S (cons-stream
           1
           (merge (scale-stream S 2)
                         (merge (scale-stream S 3)
                                       (scale-stream S 5)))))


(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))


;;Exercise 3.59
(define (divide-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series s)
  (divide-streams s integers))
  

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))


(define plus-minus
  (interleave (repeat-stream 1) (repeat-stream -1)))


(define (every-second s)
  (cons-stream (stream-car s)
               (every-second (stream-cdr (stream-cdr s)))))


;; mine are wrong.
;; remember that you need 0s for the series values that "don't exist"

;; (define cos-series
;;   (cons-stream 1 (mul-streams
;;                   plus-minus
;;                   (every-second exp-series))))

               
;; (define sine-series
;;   (mul-streams plus-minus
;;                (every-second (stream-cdr exp-series))))

;; correct
(define cos-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0 (integrate-series cos-series)))





(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (average . x)
  (/ (apply + x) (length x)))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))


(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)



;; Exercise 3.64
(define (stream-limit s tolerance)
  (let ((first (abs (stream-car s)))
        (second (abs (stream-car (stream-cdr s)))))
    (if (< (abs (- second first)) tolerance)
        second
        (stream-limit (stream-cdr s) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))


;; Exercise 3.65
(define (log-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (log-summands (+ n 1)))))

(define log-2-stream
  (partial-sums (log-summands 1)))

(define log-2-euler
  (euler-transform log-2-stream))

(define log-2-accelerated
  (accelerated-sequence euler-transform log-2-stream))



;;Exercise 3.66
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


;; Exercise 3.69
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (pairs (stream-cdr t) (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))


(define (is-pyth-triple? x)
  (if (null? x)
      #f     
      (let ((i (expt (car x) 2))
            (j (expt (cadr x) 2))
            (k (expt (caddr x) 2)))
        (= (+ i j) k))))
          

(define pythagorean-triples
  (stream-filter is-pyth-triple?
                 (triples integers integers integers)))


;; Exercise 3.70
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2))
               (s1cdr (stream-cdr s1))
               (s2cdr (stream-cdr s2))
               (s1-weight (weight (stream-car s1)))
               (s2-weight (weight (stream-car s2))))
           (cond ((<= s1-weight s2-weight)
                  (cons-stream s1car
                               (merge-weighted s1cdr s2 weight)))
                 (else
                  (cons-stream s2car
                               (merge-weighted s1 s2cdr weight))))))))
                      

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))


;; Why is this not working?
(define w-pairs-a
  (weighted-pairs integers
                  integers
                  (lambda (x) (+ (car x) (cadr x)))))

(define (filter-fn x)
  (not (or (= (modulo x 2) 0)
           (= (modulo x 3) 0)
           (= (modulo x 5) 0))))

(define (sum-fn p)
  (let ((i (car p))
        (j (cadr p)))
    (+ (* 2 i)
       (* 3 j)
       (* 5 i j))))

(define w-pairs-b
  (let ((filtered-ints (stream-filter filter-fn integers)))
    (weighted-pairs filtered-ints filtered-ints sum-fn)))
                  
