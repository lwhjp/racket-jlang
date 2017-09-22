#lang racket/base

(provide
 atom?
 normalized-noun?
 normalize-noun
 ->array
 sequence->array
 noun-rank
 noun-shape
 noun-tally
 in-items)

(require math/array
         racket/sequence
         racket/unsafe/ops)

(define (atom? v)
  (cond
    [(array? v) (zero? (array-dims v))]
    [(sequence? v) (number? v)]
    [else #t]))

(define (normalized-noun? v)
  (cond
    [(array? v) (or (positive? (array-dims v))
                    (not (normalized-noun? (unsafe-array-ref v #[]))))]
    [(sequence? v) (number? v)]
    [else #t]))

(define (normalize-noun v)
  (cond
    [(array? v) (if (zero? (array-dims v))
                    (let ([x (unsafe-array-ref v #[])])
                      (if (normalized-noun? x) x v))
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

(define (noun-rank v)
  (cond
    [(array? v) (array-dims v)]
    [(and (sequence? v) (not (number? v))) 1]
    [else 0]))

(define (noun-shape v)
  (cond
    [(array? v) (array-shape v)]
    [(and (sequence? v) (not (number? v))) (vector (sequence-length v))]
    [else #[]]))

(define (noun-tally v)
  (cond
    [(array? v) (let ([s (array-shape v)]) (if (zero? (vector-length s)) 1 (vector-ref s 0)))]
    [(and (sequence? v) (not (number? v))) (sequence-length v)]
    [else 1]))

(define (in-items v)
  (cond
    [(array? v) (case (array-dims v)
                  [(0) (in-value (unsafe-array-ref v #[]))]
                  [(1) (in-array v)]
                  [else (in-array-axis v 0)])]
    [(and (sequence? v) (not (number? v))) v]
    [else (in-value v)]))
