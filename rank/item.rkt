#lang racket/base

(require racket/contract/base)

(provide
 item-count
 item-shape
 head-item
 last-item
 tail-items
 in-items
 (contract-out
  [item-ref (-> any/c (ranked-value exact-integer?) any/c)]
  [take-items (-> any/c (ranked-value (or/c exact-integer? infinite?)) any/c)]
  [drop-items (-> any/c (ranked-value (or/c exact-integer? infinite?)) any/c)]
  [reshape-items (-> any/c (ranked-value exact-nonnegative-integer?) any/c)]))

(require math/array
         racket/math
         racket/sequence
         racket/vector
         "base.rkt")

(define/rank (item-count v)
  (cond
    [(array? v) (let ([s (array-shape v)]) (if (zero? (vector-length s)) 1 (vector-ref s 0)))]
    [(and (sequence? v) (not (number? v))) (sequence-length v)]
    [else 1]))

(define/rank (item-ref v pos)
  (define len (item-count v))
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

(define/rank (item-shape v)
  (define v-shape (shape v))
  (if (zero? (vector-length v-shape))
      #[]
      (vector-drop v-shape 1)))

(define/rank (head-item v) (item-ref (take-items v 1) 0))

(define/rank (last-item v) (item-ref (take-items v -1) 0))

(define/rank (take-items v [pos 1])
  (define x-vec (array->vector (->array pos)))
  (define y-arr
    (if (array? v)
        v
        (reshape-items v (make-vector (vector-length x-vec) 1))))
  (define y-shape (array-shape y-arr))
  (unless (<= (vector-length x-vec) (vector-length y-shape))
    (error 'take-items "index error: ~a" pos))
  ;; Can't use array-slice-ref due to overtaking
  (define out-shape
    (for/vector ([k (in-sequences (in-vector x-vec) (in-cycle '(+inf.0)))]
                 [d (in-vector y-shape)])
      (if (infinite? k) d (abs k))))
  (define index-offsets
    (for/vector ([k (in-sequences (in-vector x-vec) (in-cycle '(+inf.0)))]
                 [d (in-vector y-shape)])
      (if (negative? k) (+ k d) 0)))
  (define fill (current-fill))
  (build-array
   out-shape
   (λ (js)
     (define offset-js (vector-map + js index-offsets))
     (if (for/and ([j (in-vector offset-js)]
                   [d (in-vector y-shape)])
           (<= 0 j (sub1 d)))
         (array-ref y-arr offset-js)
         fill))))

(define/rank (tail-items v) (drop-items v 1))

(define/rank (drop-items v [pos 1])
  (define x-vec (array->vector (->array pos)))
  (define y-arr (if (atom? v) (array #[v]) v))
  (define y-shape (array-shape y-arr))
  (unless (<= (vector-length x-vec) (vector-length y-shape))
    (error 'drop-items "index error: ~a" pos))
  (array-slice-ref
   y-arr
   (for/list ([k (in-sequences (in-vector x-vec) (in-cycle '(0)))]
              [d (in-vector y-shape)])
     (if (negative? k)
         (:: (max (+ d k) 0))
         (:: (min d k) d)))))

(define/rank (reshape-items v [new-shape 1])
  (define x-vec (array->vector (->array new-shape)))
  (cond
    [(zero? (vector-length x-vec)) (if (atom? v) v (array-axis-ref (->array v) 0 0))]
    [(atom? v) (make-array x-vec v)]
    [else
     (define x-len (vector-length x-vec))
     (define y-arr (->array v))
     (define y-shape (array-shape y-arr))
     (define item-count (vector-ref y-shape 0))
     (define item-shape (vector-drop y-shape 1))
     (array-transform
      y-arr
      (vector-append x-vec item-shape)
      (λ (js)
        (define item-idx
          (for/fold ([i 0])
                    ([j (in-vector js)]
                     [d (in-vector x-vec)])
            (+ (* i d) j)))
        (define y-part (vector-drop js (sub1 x-len)))
        (vector-set! y-part 0 (remainder item-idx item-count))
        y-part))]))

(define (in-items v)
  (cond
    [(array? v) (case (array-dims v)
                  [(0) (in-value (unsafe-array-ref v #[]))]
                  [(1) (in-array v)]
                  [else (in-array-axis v 0)])]
    [(and (sequence? v) (not (number? v))) v]
    [else (in-value v)]))
