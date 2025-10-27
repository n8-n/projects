#lang racket


(define (df f)
  (lambda () f))


(define (run-df df)
  (df))
