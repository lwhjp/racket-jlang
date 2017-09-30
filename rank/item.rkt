#lang racket/base

(provide (all-defined-out))

(require math/array
         racket/sequence
         racket/vector
         "base.rkt")

(define (tally v)
  (cond
    [(array? v) (let ([s (array-shape v)]) (if (zero? (vector-length s)) 1 (vector-ref s 0)))]
    [(and (sequence? v) (not (number? v))) (sequence-length v)]
    [else 1]))

(define (item-ref v pos)
  (define len (tally v))
  (define (do-ref ref)
    (unless (<= (- len) pos (sub1 len))
      (raise-range-error 'item-ref "value" "" pos v (- len) (sub1 len)))
    (ref (if (negative? pos) (+ pos len) pos)))
  (cond
    [(array? v) (case (array-dims v)
                  [(0) (do-ref (λ (pos) (unsafe-array-ref v #[])))]
                  [else (do-ref (λ (pos) (array-axis-ref v 0 pos)))])]
    [(and (sequence? v) (not (number? v))) (do-ref (λ (pos) (sequence-ref v pos)))]
    [else (do-ref (λ (pos) v))]))

(define (item-shape v)
  (define v-shape (shape v))
  (if (zero? (vector-length v-shape))
      #[]
      (vector-drop v-shape 1)))

(define (in-items v)
  (cond
    [(array? v) (case (array-dims v)
                  [(0) (in-value (unsafe-array-ref v #[]))]
                  [(1) (in-array v)]
                  [else (in-array-axis v 0)])]
    [(and (sequence? v) (not (number? v))) v]
    [else (in-value v)]))
