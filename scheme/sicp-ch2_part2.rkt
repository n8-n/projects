#lang sicp


;; Set Trees
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

(define t2
  (let ((mt make-tree))
    (mt 3 (mt 1 '() '())
        (mt 7 (mt 5 '() '())
            (mt 9 '() (mt 11 '() '()))))))

(define t3
  (let ((mt make-tree))
    (mt 5 (mt 3 (mt 1 '() '()) '())
        (mt 9 (mt 7 '() '())
            (mt 11 '() '())))))


(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


;; Exercise  2.66
(define itree-1 (list->tree '((1 "hello") (2 "world") (3 "my") (4 "name") (5 "is"))))

(define (key entry) (car entry))

(define (lookup-tree given-key tree)
  (cond ((null? tree) false)
        ((= given-key (key (entry tree)))
         (entry tree))
        ((> given-key (key (entry tree)))
         (lookup-tree given-key (right-branch tree)))
        (else (lookup-tree given-key (left-branch tree)))))


;; Huffman encoding
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))


(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch-huff tree) (car tree))

(define (right-branch-huff tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch-huff branch))
        ((= bit 1) (right-branch-huff branch))
        (else (error "bad bit --" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; A: 0
; B: 10
; C: 111
; D: 110


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol s tree)
  (define (contains-symbol? s syms)
    (cond ((null? syms) #f)           
          ((eq? s (car syms)) #t)
          (else (contains-symbol? s (cdr syms)))))
  (define (iter s tree result)
    (cond ((null? tree) (error "shouldn't be here"))
          ((leaf? tree)
           (if (eq? (symbol-leaf tree) s)
               result
               '()))
          ((contains-symbol? s (symbols (left-branch-huff tree)))
           (iter s (left-branch-huff tree) (append result (list 0))))
          (else (iter s (right-branch-huff tree)
                      (append result (list 1))))))
  (if (not (contains-symbol? s (symbols tree)))
      (error "symbol -- not in tree!" s)
      (iter s tree '())))
          
           
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; My solution -- only works with reversed list
;; (define (successive-merge leaf-set)
;;   (cond ((null? leaf-set) '())
;;         ((= 1 (length leaf-set)) leaf-set)
;;         ((= 2 (length leaf-set))
;;          (make-code-tree (car leaf-set) (cadr leaf-set)))
;;         (else (make-code-tree (car leaf-set)
;;                               (successive-merge (cdr leaf-set))))))

(define (successive-merge leaves)
  (cond ((null? (cdr leaves)) (car leaves))
        (else
         (successive-merge (adjoin-set
                            (make-code-tree (car leaves) (cadr leaves))
                            (cddr leaves))))))

(define ht1 (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))))

(define mess '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))



;;
;; quick detour back to chapter 1

(define coins '(50 20 10 5 2 1))

(define (count-change amount)
  (cc amount coins))

(define (cc amount coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= (length coins) 0)) 0)
        (else (+ (cc amount (cdr coins))
                 (cc (- amount (car coins))
                     coins)))))



(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
