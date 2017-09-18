#lang racket/base

(require math/array
         racket/sequence
         racket/unsafe/ops)

(define (sequence->array s)
  (build-array
   (vector (sequence-length s))
   (λ (js) (sequence-ref s (unsafe-vector-ref js 0)))))

(define (->array v)
  (cond
    [(array? v) v]
    [(list? v) (list->array v)]
    [(vector? v) (vector->array v)]
    [(and (sequence? v) (not (number? v))) (sequence->array v)]
    [else (array v)]))

(define (normalize-noun v)
  (cond
    [(array? v) (unwrap-atom v)]
    [(and (sequence? v) (not (number? v))) (sequence->array v)]
    [else v]))

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

(define (atom? v)
  (and (array? v) (zero? (array-dims v))))

(define (unsafe-atom-ref v)
  (unsafe-array-ref v #[]))

(define (unwrap-atom v)
  (if (atom? v) (unsafe-atom-ref v) v))

; TODO: reshape

(define (in-items v)
  (cond
    [(array? v) (if (zero? (array-dims v)) (in-value (unwrap-atom v)) (in-array-axis v))]
    [(and (sequence? v) (not (number? v))) v]
    [else (in-value v)]))

(provide (all-defined-out))