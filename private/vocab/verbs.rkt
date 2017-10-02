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
         "../type.rkt"
         "../parameters.rkt"
         "../word.rkt")

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
(define (*nonnegative? v) (and (real? v) (not (negative? v))))

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

(define/rank (jv:floor [z 0])
  ; TODO: tolerance
  (cond
    [(real? z) (floor z)]
    [else
     (define r (real-part z))
     (define i (imag-part z))
     (define rip (floor r))
     (define iip (floor i))
     (define x (- r rip))
     (define y (- i iip))
     (cond
       [(> 1 (+ x y)) (make-rectangular rip iip)]
       [(>= x y) (make-rectangular (add1 rip) iip)]
       [else (make-rectangular rip (add1 iip))])]))

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

(define/rank (jv:ceiling [z 0])
  ; TODO: tolerance
  (cond
    [(real? z) (ceiling z)]
    [else (- (jv:floor (- z)))]))

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

(define/rank (jv:signum [y 0])
  (cond
    [(real? y) (sgn y)]
    [else (/ y (magnitude y))]))

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

(define-customizable/cond (jv:match t)
  current-default-tolerance
  [(*nonnegative? t) (let ([cmp (customize jv:equal t)])
                       (lambda/rank (x y)
                         (let ([x (->array x)]
                               [y (->array y)])
                           (and (equal? (array-shape x) (shape y))
                                (array-andmap cmp x y)))))])

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

(define/rank (jv:shape-of y) (shape y))

(define/rank (jv:shape [x 1] y)
  ; TODO: fit
  (reshape-items y x))

; Sparse: not implemented

; self-reference

; nub

; nub-sieve

(define-customizable/cond (jv:not-equal t)
  current-default-tolerance
  [(*zero? t) (lambda/atomic (x y) (->jbool (not (compare/numeric = equal? x y))))]
  [(*positive? t) (lambda/atomic (x y) (->jbool (not (compare/numeric (λ (x y) (=/t t x y)) equal? x y))))])

(define-monad-alias jv:magnitude magnitude)

(define/rank (jv:residue [x 0] [y 0])
  ; TODO: tolerance
  (cond
    [(zero? x) y]
    [(- y (* x (jv:floor (/ y x))))]))

(define/rank (jv:reverse y) ; TODO: fit (right-shift)
  (if (zero? (rank y))
      y
      (array-slice-ref (->array y) (list (:: #f #f -1) ::...))))

; rotate

(define/rank (jv:transpose y)
  (let* ([y (->array y)]
         [r (array-dims y)])
    (array-axis-permute y (build-list r (λ (i) (- (sub1 r) i))))))

; transpose (dyad)

(define/rank (jv:ravel y) (array-flatten (->array y)))

(define/rank (jv:append x y)
  ; TODO: fit
  (let*-values
      ([(x y) (cond
                [(atom? x) (values (jv:shape (item-shape y) x) y)]
                [(atom? y) (values x (jv:shape (item-shape x) y))]
                [else (values x y)])]
       [(x y) (values (for/fold ([x (->array x)])
                                ([i (in-range (rank x) (rank y))])
                        (array-axis-insert x 0))
                      (for/fold ([y (->array y)])
                                ([i (in-range (rank y) (rank x))])
                        (array-axis-insert y 0)))])
    (cond
      [(zero? (array-dims x)) (array #[(array-ref x #[]) (array-ref y #[])])]
      [else
       (define fill (or (guess-fill x) (guess-fill y) 0)) ; TODO: types
       (define x-shape (array-shape x))
       (define y-shape (array-shape y))
       (define x-tally (vector-ref x-shape 0))
       (define y-tally (vector-ref y-shape 0))
       (define out-shape (vector-map max x-shape y-shape))
       (vector-set! out-shape 0 (+ x-tally y-tally))
       (build-array
        out-shape
        (λ (js)
          (define item-idx (vector-ref js 0))
          (define-values (src-arr src-shape src-item-idx)
            (if (< item-idx x-tally)
                (values x x-shape item-idx)
                (values y y-shape (- item-idx x-tally))))
          (define src-js (vector-copy js))
          (vector-set! src-js 0 src-item-idx)
          (if (for/and ([j (in-vector src-js)]
                        [d (in-vector src-shape)])
                (< j d))
              (unsafe-array-ref src-arr src-js)
              fill)))])))

; ravel items

; stitch

(define/rank (jv:itemize y) (array-axis-insert (->array y) 0))

; laminate

; raze
; link

; words
; sequential machine

(define/rank (jv:tally y) (item-count y))

(define/rank (jv:copy [x 1] y)
  (cond
    [(eqv? (item-count x) (item-count y))
     (define y-arr
       (if (array? y) y (array #[y])))
     (define indexes
       (list->vector
        (reverse
         (for/fold ([is '()])
                   ([k (in-array (->array x))]
                    [i (in-naturals)])
           (append (make-list (imag-part k) #f)
                   (make-list (real-part k) i)
                   is)))))
     (define fill (or (guess-fill y) 0)) ; TODO: types
     (build-array
      (vector-append (vector (vector-length indexes)) (item-shape y-arr))
      (λ (js)
        (define idx (vector-ref indexes (vector-ref js 0)))
        (cond
          [(not idx) fill]
          [else
           (define y-js (vector-copy js))
           (vector-set! y-js 0 idx)
           (array-ref y-arr y-js)])))]
    [(atom? x) (jv:copy (jv:shape (shape y) x) y)]
    [(atom? y) (jv:copy x (jv:shape (shape x) y))]
    [else (error 'copy "length error: ~a" x)]))

(define/rank (jv:base [x 1] [y 1])
  ; TODO: inverse
  (define x-arr (if (array? x) x (->array (jv:shape (shape y) x))))
  (define y-arr (if (array? y) y (->array (jv:shape (shape x) y))))
  (unless (equal? (array-shape x-arr) (array-shape y-arr))
    (error 'base "length error: ~a" x))
  (for/fold ([a 0])
            ([x (in-array x-arr)]
             [y (in-array y-arr)])
    (+ (* a x) y)))

(define/rank (jv:antibase-two y)
  (define y-arr (->array y))
  (define len
    (array-all-max (array-map integer-length y-arr)))
  (define x-arg (make-list (max len 1) 2))
  (define bits
    (array-map (λ (n) (jv:antibase x-arg n)) y-arr))
  (build-array
   (vector-append (array-shape y-arr) (vector (max len 1)))
   (λ (js)
     (array-ref (array-ref bits (vector-drop-right js 1))
                (vector-take-right js 1)))))

(define/rank (jv:antibase [x 1] [y 0])
  (list->array
   (reverse
    (let loop ([rs (if (array? x) (reverse (array->list x)) (list x))]
               [n y])
      (cond
        [(null? rs) '()]
        [else
         (define r (car rs))
         (define-values (a d) (quotient/remainder n r))
         (cons d (loop (cdr rs) a))])))))


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

(define/rank (jv:from [x 0] y)
  ; TODO: non-integer x
  (item-ref y x))

(define/rank (jv:head y) (head-item y))

(define/rank (jv:take [x 1] y)
  ; TODO: fit
  (parameterize ([current-fill (or (guess-fill y) 0)]) ; TODO: types
    (take-items y x)))

(define/rank (jv:tail y) (last-item y))

;map
;fetch

(define/rank (jv:behead y) (tail-items y))

(define/rank (jv:drop [x 1] y) (drop-items y x))

(define/rank (jv:curtail y) (jv:drop -1 y))

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

(define/rank (jv:index-of x y)
  ; TODO: fit
  (define rix (max 0 (sub1 (rank x))))
  (define tx (item-count x))
  ; FIXME: this is a little clumsy...
  ((make-ranked-procedure
    (λ (c)
      (or (for/first ([i (in-naturals)]
                      [xc (in-items x)]
                      #:unless (zero? (jv:match c xc)))
            i)
          tx))
    `((,rix)))
   y))

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
