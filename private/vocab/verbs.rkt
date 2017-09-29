#lang racket/base

(provide (filtered-out
          (λ (name)
            (and (regexp-match? #rx"^jv:." name)
                 name))
          (all-defined-out)))

(require (for-syntax racket/base)
         math/array
         math/number-theory
         (only-in racket/list make-list)
         racket/math
         racket/provide
         racket/vector
         "../../customize.rkt"
         "../../rank.rkt"
         "../../private/word.rkt"
         "../parameters.rkt")

(define (monad proc) (procedure-reduce-arity proc 1))
(define (dyad proc) (procedure-reduce-arity proc 2))

(define (self-inverse proc) (make-verb proc #:obverse proc))

(define-syntax-rule (lambda/atomic (arg ...) body ...)
  (atomic-procedure->ranked-procedure
   (lambda (arg ...) body ...)))

(define-syntax-rule (define/atomic id proc)
  (define id (atomic-procedure->ranked-procedure proc)))

(define-syntax-rule (define-monad-alias id proc)
  (define/atomic id (monad proc)))

(define-syntax-rule (define-dyad-alias id proc)
  (define/atomic id (dyad proc)))

(define (->jbool v) (if v 1 0))

(define (*zero? v) (and (number? v) (zero? v)))
(define (*positive? v) (and (real? v) (positive? v)))

(define (compare/numeric numeric-cmp else-cmp x y)
  (cond
    [(and (number? x) (number? y)) (numeric-cmp x y)]
    [(and (box? x) (box? y)) (compare/numeric numeric-cmp else-cmp (unbox x) (unbox y))]
    [else (else-cmp x y)]))

(define (=/t t x y)
  (<= (magnitude (- x y))
      (* t (max (magnitude x) (magnitude y)))))

; self-classify

(define-customizable/cond (jv:equal t)
  current-default-tolerance
  [(*zero? t) (lambda/atomic (x y) (->jbool (compare/numeric = equal? x y)))]
  [(*positive? t) (lambda/atomic (x y) (->jbool (compare/numeric (λ (x y) (=/t t x y)) equal? x y)))])

(define/rank (jv:box y) (box y))

(define-customizable/cond (jv:less-than t)
  current-default-tolerance
  [(*zero? t) (lambda/atomic (x y) (->jbool (< x y)))]
  [(*positive? t) (lambda/atomic (x y) (->jbool (and (< x y) (not (=/t t x y)))))])

;floor

(define-customizable/cond (jv:lesser-of t)
  current-default-tolerance
  [(*zero? t) (lambda/atomic (x y) (if (< x y) x y))]
  [(*positive? t) (lambda/atomic (x y) (if (and (< x y) (not (=/t t x y))) x y))])

(define-monad-alias jv:decrement sub1)

(define-customizable/cond (jv:less-than-or-equal t)
  current-default-tolerance
  [(*zero? t) (lambda/atomic (x y) (->jbool (<= x y)))]
  [(*positive? t) (lambda/atomic (x y) (->jbool (or (<= x y) (=/t t x y))))])

(define/atomic jv:open (λ (y) (if (box? y) (unbox y) y)))

(define-customizable/cond (jv:larger-than t)
  current-default-tolerance
  [(*zero? t) (lambda/atomic (x y) (->jbool (> x y)))]
  [(*positive? t) (lambda/atomic (x y) (->jbool (and (> x y) (not (=/t t x y)))))])

; ceiling

(define-customizable/cond (jv:larger-of t)
  current-default-tolerance
  [(*zero? t) (lambda/atomic (x y) (if (> x y) x y))]
  [(*positive? t) (lambda/atomic (x y) (if (and (> x y) (not (=/t t x y))) x y))])

(define-monad-alias jv:increment add1)

(define-customizable/cond (jv:larger-than-or-equal t)
  current-default-tolerance
  [(*zero? t) (lambda/atomic (x y) (->jbool (>= x y)))]
  [(*positive? t) (lambda/atomic (x y) (->jbool (or (>= x y) (=/t t x y))))])

(define jv:conjugate (self-inverse conjugate))

(define jv:plus
  (make-verb
   (dyad +)
   #:obverse (λ (x y) (- y x))))

(define/atomic jv:real+imaginary (λ (y) (array #[(real-part y) (imag-part y)])))

; GCD [how to apply to complex?]

(define/atomic jv:double (λ (y) (* y 2)))

(define/atomic jv:not-or (λ (x y) (= 0 x y)))

; signum (complex)

(define-dyad-alias jv:times *)

(define/atomic jv:length+angle (λ (y) (array #[(magnitude y) (angle y)])))

; lcm

(define/atomic jv:square (λ (y) (expt y 2)))

(define/atomic jv:not-and (λ (x y) (if (= 1 x y) 0 1)))

(define-monad-alias jv:negate -)

(define-dyad-alias jv:minus -)

(define/atomic jv:not (λ (y) (- 1 y)))

; less

(define/atomic jv:halve (λ (y) (/ y 2)))

; match

(define/atomic jv:reciprocal (λ (y) (if (zero? y) +inf.0 (/ y))))

(define-dyad-alias jv:divided-by /) ; TODO 0/0, complex/0

; matrix inverse
; matrix divide

(define-monad-alias jv:square-root sqrt)

(define/atomic jv:root (λ (x y) (if (zero? x) +inf.0 (expt y (/ x)))))

(define-monad-alias jv:exponential exp)

(define-dyad-alias jv:power expt) ; TODO: fit

(define-monad-alias jv:natural-log log)

(define/atomic jv:logarithm (λ (x y) (/ (log y) (log x))))

(define/rank (jv:shape-of y) (value-shape y))

(define/rank (jv:shape [x 1] y)
  ; FIXME: this is wrong
  (array-reshape (->array y) (array->vector (->array x))))

; Sparse: not implemented

; self-reference

; nub

; nub-sieve

(define-customizable/cond (jv:not-equal t)
  current-default-tolerance
  [(*zero? t) (lambda/atomic (x y) (->jbool (not (compare/numeric = equal? x y))))]
  [(*positive? t) (lambda/atomic (x y) (->jbool (not (compare/numeric (λ (x y) (=/t t x y)) equal? x y))))])

(define-monad-alias jv:magnitude magnitude)

; residue (non-integer)

(define/rank (jv:reverse y) ; TODO: fit (right-shift)
  (if (zero? (value-rank y))
      y
      (array-slice-ref (->array y) (list (:: #f #f -1) ::...))))

; rotate

(define/rank (jv:transpose y)
  (let* ([y (->array y)]
         [r (array-dims y)])
    (array-axis-permute y (build-list r (λ (i) (- (sub1 r) i))))))

; transpose (dyad)

(define/rank (jv:ravel y) (array-flatten (->array y)))

; append

; ravel items

; stitch

(define/rank (jv:itemize y) (array-axis-insert (->array y) 0))

; laminate

; raze
; link

; words
; sequential machine

(define/rank (jv:tally y) (value-tally y))

; copy

; base two
;base
;antibase two
;antibase
;factorial
;out-of
;grade-up
;sort
;grade-down
;sort

(define/rank (jv:same y) y)

(define/rank (jv:left x y) x)

(define/rank (jv:right x y) y)

;catalogue
;from
;head
;take
;tail
;map
;fetch
;behead
;drop
;curtail
;do
;numbers
;default-format
;format
;roll
;deal
;roll/fixed
;deal/fixed
;anagram-index
;anagram
;cycle-direct
;permute
;raze-in
;member (in)
;member-of-interval

(define/rank (jv:integers [y 1]) ; TODO: fit
  (define shape (array->vector (->array y)))
  (array-slice-ref (index-array (vector-map abs shape))
                   (map (λ (d) (:: #f #f (sgn d))) (vector->list shape))))

;index-of
;indices
;interval-index

(define/atomic jv:imaginary (λ (y) (* 0+1i y)))

(define/atomic jv:complex (λ (x y) (+ x (* 0+1i y))))

;level-of
;pi-times
;circle-function
;roots
;polynomial
;poly-derive
;poly-integral

(define-monad-alias jv:primes nth-prime)

; primes (dyad)

(define/rank (jv:prime-factors [y 0])
  (list->array (apply append (map (λ (p) (make-list (cadr p) (car p))) (factorize y)))))

;prime-exponents

(define/atomic jv:angle (λ (y) (make-polar 1 y)))

(define-dyad-alias jv:polar make-polar)

;symbol
;unicode
;extended precision
