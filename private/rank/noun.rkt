#lang racket/base

(provide
 atom?
 normalized-value?
 normalize-value
 ->array
 sequence->array
 value-rank
 value-shape
 value-tally
 item-ref
 item-shape
 in-items)

(require math/array
         racket/sequence
         racket/unsafe/ops
         racket/vector)

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

(define (value-rank v)
  (cond
    [(array? v) (array-dims v)]
    [(and (sequence? v) (not (number? v))) 1]
    [else 0]))

(define (value-shape v)
  (cond
    [(array? v) (array-shape v)]
    [(and (sequence? v) (not (number? v))) (vector (sequence-length v))]
    [else #[]]))

(define (value-tally v)
  (cond
    [(array? v) (let ([s (array-shape v)]) (if (zero? (vector-length s)) 1 (vector-ref s 0)))]
    [(and (sequence? v) (not (number? v))) (sequence-length v)]
    [else 1]))

(define (item-ref v pos)
  (define len (value-tally v))
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
  (define v-shape (value-shape v))
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
