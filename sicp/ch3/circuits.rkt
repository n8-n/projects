#lang sicp

(#%require "../util/queue.rkt")

(define inverter-delay 2)

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define and-gate-delay 3)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond ((and (= s1 1)
              (= s2 1)) 1)
        ((or (= s1 0)
             (= s2 0)) 0)
        (else (error "Invalid signal" (s1 s2)))))

(define (create-2-input-gate logic gate-delay)
  (lambda (a1 a2 output)
    (let ((new-value
           (logic (get-signal a1) (get-signal a2))))
      (after-delay gate-delay
                   (lambda ()
                     (set-signal! output new-value))))))


;; exercise 3.28
(define or-gate-delay 5)

;; (define (or-gate a1 a2 output)
;;   (let ((or-action-procedure
;;          ((create-2-input-gate logical-or or-gate-delay)
;;           a1
;;           a2
;;           output)))
;;     (add-action! a1 or-action-procedure)
;;     (add-action! a2 or-action-procedure)
;;     'ok))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)


(define (logical-or s1 s2)
  (cond ((and (= s1 0)
              (= s2 0)) 0)
        ((or (= s1 1)
             (= s2 1)) 1)
        (else (error "Invalid signal" (s1 s2)))))


(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))


;; exercise 3.29
(define (nand a b z)
  (let ((x (make-wire)))
    (and-gate a b x)
    (inverter x z)
    'ok))

;; delay = and-gate-delay + (inverter-delay * 3)
(define (or2 a b z)
  (let ((x (make-wire)) (y (make-wire)))
    (inverter a x)
    (inverter b y)
    (nand x y z)))


;; Exercise 3.30
(define (make-wire-list n)
  (let ((wire (make-wire)))
    (if (= n 0)
        '()
        (cons wire (make-wire-list (- n 1))))))


(define (map f ls)
  (if (null? ls)
      '()
      (cons (f (car ls))
            (map f (cdr ls)))))

(define (equal-lengths . ls)
  (apply = (map length ls)))

;; doesn't work
(define (ripple-carry-adder an bn sn c-in-init c-out-final)
  (define (adder-loop a b s c)
    (let ((new-carry (make-wire)))
      (if (= (length s) 1)
          (full-adder (car a) (car b) c (car s) c-out-final)
          (begin
            (adder-loop (cdr a) (cdr b) (cdr s) new-carry)
            (full-adder (car a) (car b) c (car s) new-carry)))))
  
  (if (not (equal-lengths an bn sn))
      (error "a b and s are not equal lengths!")
      (adder-loop an bn sn c-in-init))
  'ok)

	

(define (ripple-carry-adder2 As Bs Ss C)
  (let ((carry (make-wire)))
    (if (null? As)
        (set-signal! C 0)
        (begin
          (ripple-carry-adder2 (cdr As) (cdr Bs) (cdr Ss) carry)
          (full-adder (car As) (car Bs) carry (car Ss) C)))))



(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


(define (get-wire-list-signals wires)
  (if (null? wires)
      '()
      (cons (get-signal (car wires))
            (get-wire-list-signals (cdr wires)))))

(define (set-wire-list-signals! wires vals)
  (define (loop w v)
    (if (null? w)
        'done
        (begin
          (set-signal! (car w) (car v))
          (loop (cdr w) (cdr v)))))
  (if (equal-lengths wires vals)
      (loop wires vals)
      (error "Mismatched list lengths!")))


;; AGENDA -----------
(define (make-agenda) (list 0))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))


(define the-agenda (make-agenda))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))

  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments (cons
                                  (make-new-time-segment time action)
                                  (cdr segments)))
              (add-to-segments! rest)))))
  
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))


(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))


(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))


(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (begin
      (delete-queue! q)
      (if (empty-queue? q)
          (set-segments! agenda (rest-segments agenda))))))
    
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty!")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))


(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))



(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))


;; ---------------------------
;; Decimal and binary conversions
;; ---------------------------
(define (decimal-to-binary number bits)
  (define (loop result dividend)
    (let ((quo (quotient dividend 2))
          (rem (remainder dividend 2)))
      (if (= (length result) bits)
          result
          (loop (cons rem result) quo))))
    (loop '() number))


;; Convert decimal number to list of 8 bits
(define (to-8-bit-binary n)
  (decimal-to-binary n 8))

(define (binary-to-decimal n)
  (define (loop result position numbers)
    (if (null? numbers)
        result
        (let ((position-mult (* (car numbers)
                                (expt 2 position))))
          (loop (+ result position-mult)
                (inc position)
                (cdr numbers)))))
  (loop 0 0 (reverse n)))



(define i1 (make-wire))
(define i2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))


(define a1 (make-wire-list 8))
(define a2 (make-wire-list 8))

(define s1 (make-wire-list 8))
(define c1 (make-wire))
(define c2 (make-wire))

(ripple-carry-adder2 a1 a2 s1 c1)

(set-wire-list-signals! a1 (to-8-bit-binary 17))
(set-wire-list-signals! a2 (to-8-bit-binary 31))
