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
      (assign guess 1.0)
    iter
      (test (op good-enough?) (reg guess))
      (branch (label sqrt-done))
      (assign guess (op improve) (reg guess))
    sqrt-done))

(define sqrt-2
  '(controller
    sqrt
      (assign guess 1.0)
    iter
    good-enough?
      (assign t (op square) (reg guess))
      (assign t (op -) (reg t) (reg x))
      (assign t (op abs) (reg t))
      (test (op <) (reg t) 0.001)
      (branch (label sqrt-done))
    improve
      (assign t (op /) (reg x) (reg guess))
      (assign guess (op avg) (reg t) (reg guess))
      (goto (label iter))
    sqrt-done))
