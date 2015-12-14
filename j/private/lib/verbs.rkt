#lang racket/base

(require (prefix-in r: racket/math)
         math/array
         racket/sequence
         racket/vector
         (except-in "../../../frame/main.rkt" shape)
         "../noun.rkt"
         "../verb.rkt")

(provide (all-defined-out))

;; TODO: make polymorphic verb types instead of using Any

(define (append x y)
  (unless (eq? (noun-type x) (noun-type y))
    (error "type mismatch"))
  (noun
   (noun-type x)
   (array-list->array
    (map noun-value
         (append (noun->items x)
                 (noun->items y))))))

(define conjugate
  (wrap/monad r:conjugate))

(define (copy x y)
  (define y-items (in-noun-items y))
  (array->noun
   (apply/rank
    (ranked-procedure
     (λ (x)
       (unless (or (zero? (vector-length (array-dims x)))
                   (= (array-size (noun-value x)) (sequence-length y-items)))
         (error "length error"))
       ;; FIXME: do this more efficiently
       (array-append*
        (for/list ([item y-items]
                   [count (in-cycle (in-array (noun-value x)))]
                   #:unless (zero? count))
          (make-array (vector count) item))))
     (dyadic-rank 1 'any))
    (list (noun-value x) (noun-value y)))))

(define divide
  (wrap/dyad
   (λ (x y)
     (if (zero? y)
         (cond
           [(zero? x) 0]
           [(positive? x) +inf.0]
           [(negative? x) -inf.0])
         (/ x y)))))

(define (head y)
  (sequence-ref (in-noun-items y) 0))

(define (link x y)
  (noun
   'box
   (array-append*
    (list
     (array #[(box x)])
     (if (eq? 'box (noun-type y))
         (noun-value y)
         (array #[(box y)]))))))

(define minus
  (wrap/dyad -))

(define negate
  (wrap/monad -))

(define plus
  (wrap/dyad +))

(define (ravel y)
  (noun
   (noun-type y)
   (array-flatten (noun-value y))))

(define (raze y)
  (cond
    [(noun-empty? y) (noun (noun-type y) (array #[]))]
    [(eq? 'box (noun-type y))
     (array->noun
      (array-append*
       (for/list ([cell (in-array (noun-value y))])
         (define v
           (if (box? cell) (unbox cell) cell))
         (if (zero? (noun-rank v))
             (array-axis-insert (noun-value v) 0)
             (noun-value v)))))]
    [else
     (noun (noun-type y)
           (array-flatten (noun-value y)))]))

(define reciprocal
  (wrap/monad
   (λ (y)
     (if (zero? y) +inf.0 (/ y)))))

(define (shape x y)
  (unless (< (noun-rank x) 2)
    (error "length error"))
  (define items (in-noun-items y))
  (define shape
    (vector-append
     (array->vector (noun-value x))
     (if (zero? (noun-rank y))
         '#[]
         (vector-drop (array-shape (noun-value y)) 1))))
  (array->noun
   (for/array #:shape shape
              ([item (in-cycle items)])
              item)))

(define (shape-of y)
  (array->noun
   (vector->array (noun-shape y))))

(define signum
  (wrap/monad
   (λ (n)
     (if (real? n)
         (r:sgn n)
         (make-polar 1 (angle n))))))

(define (tally y)
  (noun
   'number
   (array-size (noun-value y))))

(define times
  (wrap/dyad *))