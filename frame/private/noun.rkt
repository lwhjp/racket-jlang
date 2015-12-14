#lang racket/base

(require math/array
         racket/sequence)

(provide (all-defined-out))

(define rank? exact-nonnegative-integer?)

(define (->array n)
  (cond
    [(array? n) n]
    [(and (sequence? n)
          (not (number? n)))
     (build-array
      (vector (sequence-length n))
      (λ (indexes)
        (sequence-ref n (vector-ref indexes 0))))]
    [else (array n)]))

(define (rank n)
  (cond
    [(array? n) (array-dims n)]
    [(and (sequence? n)
          (not (number? n)))
     1]
    [else 0]))

(define (shape n)
  (cond
    [(array? n) (array-shape n)]
    [(and (sequence? n)
          (not (number? n)))
     (vector (sequence-length n))]
    [else '#()]))