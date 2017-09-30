#lang racket/base

(provide
 atom?
 normalized-value?
 normalize-value
 ->array
 sequence->array
 rank
 shape)

(require math/array
         racket/sequence
         racket/unsafe/ops)

(define (atom? v)
  (cond
    [(array? v) (zero? (array-dims v))]
    [(sequence? v) (number? v)]
    [else #t]))

(define (normalized-value? v)
  (cond
    [(array? v) (or (positive? (array-dims v))
                    (not (normalized-value? (unsafe-array-ref v #[]))))]
    [(sequence? v) (number? v)]
    [else #t]))

(define (normalize-value v)
  (cond
    [(array? v) (if (zero? (array-dims v))
                    (let ([x (unsafe-array-ref v #[])])
                      (if (normalized-value? x) x v))
                    v)]
    [(list? v) (list->array v)]
    [(vector? v) (vector->array v)]
    [(and (sequence? v) (not (number? v))) (sequence->array v)]
    [else v]))

(define (->array v)
  (cond
    [(array? v) v]
    [(list? v) (list->array v)]
    [(vector? v) (vector->array v)]
    [(and (sequence? v) (not (number? v))) (sequence->array v)]
    [else (array v)]))

(define (sequence->array s)
  (unless (sequence? s)
    (raise-argument-error 'sequence->array "sequence?" s))
  (build-array
   (vector (sequence-length s))
   (λ (js) (sequence-ref s (unsafe-vector-ref js 0)))))

(define (rank v)
  (cond
    [(array? v) (array-dims v)]
    [(and (sequence? v) (not (number? v))) 1]
    [else 0]))

(define (shape v)
  (cond
    [(array? v) (array-shape v)]
    [(and (sequence? v) (not (number? v))) (vector (sequence-length v))]
    [else #[]]))
