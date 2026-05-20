#lang sicp

(#%require "../util/queue.rkt")
(#%require "../util/table.rkt")

;; Exercise 3.22
(define (make-queue2)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (empty?)
      (null? front-ptr))

    (define (add-to-queue x)
      (let ((new-pair (cons x '())))
        (cond ((empty?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr)
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr))))
    (define (delete-from-queue)
      (cond ((empty?)
             (error "Cannot delete from empty queue!"))
            (else
             (set! front-ptr (cdr front-ptr))
             front-ptr)))
    (define (print-queue) front-ptr)
    (define (front-queue) (car front-ptr))
    
    (define (dispatch m)
      (cond ((eq? m 'add) add-to-queue)
            ((eq? m 'delete) delete-from-queue)
            ((eq? m 'print) print-queue)
            ((eq? m 'front) front-queue)
            (else
             (error "Unknown option --" m))))
    dispatch))


;; Exercise 3.23
(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (make-node value prev next)
      (list value prev next))

    (define (get-value node) (car node))
    (define (get-prev node) (cadr node))
    (define (set-prev! node x) (set-car! (cdr node) x))
    (define (get-next node) (caddr node))
    (define (set-next! node x) (set-car! (cddr node) x))

    (define (empty?)
      (null? front-ptr))

    (define (add-to-front x)
      (cond ((empty?)
             (let ((new-node (make-node x '() '())))
               (set! front-ptr new-node)
               (set! rear-ptr new-node)
               (print-queue)))
              (else
               (let ((front-node (make-node x '() front-ptr)))
                 (set-prev! front-ptr front-node)
                 (set! front-ptr front-node)
                 (print-queue)))))
    (define (add-to-rear x)
      (cond ((empty?)
             (let ((new-node (make-node x '() '())))
               (set! front-ptr new-node)
               (set! rear-ptr new-node)
               (print-queue)))
            (else
             (let ((rear-node (make-node x rear-ptr '())))
               (set-next! rear-ptr rear-node)
               (set! rear-ptr rear-node)
               (print-queue)))))
    
    (define (delete-from-front)
      (cond ((empty?)
             (error "Cannot delete from empty queue!"))
            (else
             (set! front-ptr (get-next front-ptr))
             (set-prev! front-ptr '())
             (print-queue))))
    (define (delete-from-rear)
      (cond ((empty?)
             (error "Cannot delete from empty queue!"))
            (else
             (set! rear-ptr (get-prev rear-ptr))
             (set-next! rear-ptr '())
             (print-queue))))
    
    (define (print-queue)
      (define (loop l curr)
        (if (null? curr)
            l
            (loop (append l (list (get-value curr)))
                  (get-next curr))))
      (loop '() front-ptr))
      
    (define (front-queue) (car front-ptr))

    (define (raw-print) front-ptr)
    
    (define (dispatch m)
      (cond ((eq? m 'add-front) add-to-front)
            ((eq? m 'add-rear) add-to-rear)
            ((eq? m 'delete-front) delete-from-front)
            ((eq? m 'delete-rear) delete-from-rear)
            ((eq? m 'print) print-queue)
            ((eq? m 'front) front-queue)
            ((eq? m 'raw-print) raw-print)
            (else
             (error "Unknown option --" m))))
    dispatch))


(define (init-deque . args)
  (let ((deque (make-deque)))
    (define (loop l)
      (if (null? l)
          deque
          (begin ((deque 'add-rear) (car l))
                 (loop (cdr l)))))
    (loop args)))

;; (define q2 (make-deque))
;; ((q2 'add-front) 'a)
;; ((q2 'add-front) 'b)
;; ((q2 'add-rear) 'd)
;; ((q2 'add-rear) 'c)



(define (lookup2 key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert2! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr table))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)


;; exercise 3.24 & 3.25
(define (make-table-eq equality)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((equality key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (lookup-general key table)
      (let ((record (assoc key (cdr table))))
        (if record
            (cdr record)
            false)))
    
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print) local-table)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))



        ;; exercise 3.26
(define (make-table-tree)
  (list '*table-tree*))

(define (tree-node key value)
  (list key value '() '()))



;; Exercise 3.27
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previous-result (lookup x table)))
        (or previous-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))


(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

