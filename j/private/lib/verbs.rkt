#lang racket/base

(require (for-syntax racket/base)
         math/array
         racket/list
         racket/math
         racket/provide
         racket/sequence
         racket/vector
         "../../../frame/main.rkt"
         "../noun.rkt"
         "../verb.rkt")

(provide (filtered-out
          (λ (name)
            (and (regexp-match? #rx"^j:" name)
                 (substring name 2)))
          (all-defined-out)))

;; TODO: make polymorphic verb types instead of using Any

(module+ test
  (require rackunit))

(define (j:append x y)
  (define t (noun-type x))
  (unless (eq? t (noun-type y))
    (error "type mismatch"))
  (let*-values
      ([(x y) ; Reshape atomic arguments to the shape of the items of the other
        (values
         (if (zero? (noun-rank x))
             (j:shape (noun 'number (vector->array (noun-item-shape y))) x)
             x)
         (if (zero? (noun-rank y))
             (j:shape (noun 'number (vector->array (noun-item-shape x))) y)
             y))]
       [(x y) ; Bring to common rank
        (let ([common-rank (max (noun-rank x)
                                (noun-rank y)
                                1)])
          (values
           (noun t (array-reshape (noun-value x)
                                  (vector-append
                                   (make-vector (- common-rank (noun-rank x)) 1)
                                   (noun-shape x))))
           (noun t (array-reshape (noun-value y)
                                  (vector-append
                                   (make-vector (- common-rank (noun-rank y)) 1)
                                   (noun-shape y))))))])
    ;; TODO: fill
    (noun t (array-append* (map noun-value (list x y))))))

(module+ test
  (check-equal? (j:append (->noun "abc") (->noun "de"))
                (->noun "abcde"))
  ;; TODO: ('abc',"0/'de')
  (check-equal? (j:append (->noun '(5 6 7)) (->noun (index-array '#[2 3])))
                (noun 'number (array #[#[5 6 7]
                                       #[0 1 2]
                                       #[3 4 5]])))
  (check-equal? (j:append (->noun 7) (->noun (index-array '#[2 3])))
                (noun 'number (array #[#[7 7 7]
                                       #[0 1 2]
                                       #[3 4 5]]))))

(define (j:box y)
  (noun 'box (array (box y))))

(define j:conjugate
  (wrap/monad conjugate))

(define/rank/noun (j:copy [x 1] y)
  (define-values (x-items y-items)
    (cond
      [(zero? (noun-rank x)) (values (in-cycle (in-value x)) (in-noun-items y))]
      [(zero? (noun-rank y)) (values (in-noun-items x) (in-cycle (in-value y)))]
      [else
       (unless (eqv? (vector-ref (noun-shape x) 0)
                     (vector-ref (noun-shape y) 0))
         (error "length error"))
       (values (in-noun-items x) (in-noun-items y))]))
  (noun
   (noun-type y)
   (array-list->array
    (append*
     (for/list ([n x-items]
                [i y-items])
       ;; TODO: fill (complex x)
       (make-list (noun->racket-value n) (noun-value i)))))))

(module+ test
  (check-equal? (j:copy (->noun '(3 0 1)) (->noun (index-array '#[3 4])))
                (noun 'number (array #[#[0 1 2 3]
                                       #[0 1 2 3]
                                       #[0 1 2 3]
                                       #[8 9 10 11]])))
  (check-equal? (j:copy (->noun 2) (->noun "abc"))
                (noun 'char (array #[#\a #\a #\b #\b #\c #\c])))
  (check-equal? (j:copy (->noun '(1 0 1)) (->noun 2))
                (noun 'number (array #[2 2]))))

(define j:divide
  (wrap/dyad
   (λ (x y)
     (if (zero? y)
         (cond
           [(zero? x) 0]
           [(positive? x) +inf.0]
           [(negative? x) -inf.0])
         (/ x y)))))

(define (j:head y)
  (sequence-ref (in-noun-items y) 0))

(define j:larger-than
  (wrap/dyad >))

(define j:less-than
  (wrap/dyad <))

(define (j:link x y)
  (j:append
   (j:box x)
   (if (eq? 'box (noun-type y))
       y
       (j:box y))))

(module+ test
  (check-equal? (j:link (->noun 1) (->noun 2))
                (->noun (list (box (->noun 1)) (box (->noun 2)))))
  (check-equal? (j:link (->noun (box (->noun 1))) (->noun 2))
                (->noun (list (box (->noun (box (->noun 1)))) (box (->noun 2)))))
  (check-equal? (j:link (->noun 1) (->noun (box (->noun 2))))
                (->noun (list (box (->noun 1)) (box (->noun 2))))))

(define j:minus
  (wrap/dyad -))

(define j:negate
  (wrap/monad -))

(define/rank/noun (j:open [y 0])
  (if (eq? 'box (noun-type y))
      (unbox (array-ref (noun-value y) '#[]))
      y))

(module+ test
  (check-equal? (j:open (noun 'box (array #[(box (->noun '(1 2 3)))
                                            (box (->noun '(4 5 6)))
                                            (box (->noun '(7 8 9)))])))
                (noun 'number (array #[#[1 2 3]
                                       #[4 5 6]
                                       #[7 8 9]]))))

(define j:plus
  (wrap/dyad +))

(define (j:ravel y)
  (noun
   (noun-type y)
   (array-flatten (noun-value y))))

(module+ test
  (check-equal? (j:ravel (noun 'number (index-array '#[2 3 3])))
                (noun 'number (index-array '#[18]))))

(define (j:raze y)
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

(module+ test
  (check-equal? (j:raze (noun 'box (array #[(box (->noun '(1 2 3)))
                                            (box (->noun '(4 5 6)))
                                            (box (->noun '(7 8 9)))])))
                (noun 'number (array #[1 2 3 4 5 6 7 8 9]))))

(define j:reciprocal
  (wrap/monad
   (λ (y)
     (if (zero? y) +inf.0 (/ y)))))

(define/rank/noun (j:shape [x 1] y)
  (define y-shape
    (noun-shape y))
  (define y-item-shape
    (if (equal? '#[] y-shape)
        '#[]
        (vector-drop y-shape 1)))
  (define prefix
    (array->vector (noun-value x)))
  (define prefix-length
    (vector-length prefix))
  (define item-arr
    (for/array #:shape prefix
               ([item (in-cycle (in-noun-items y))])
      (noun-value item)))
  (noun
   (noun-type y)
   (build-array
    (vector-append prefix y-item-shape)
    (λ (indexes)
      (define-values (pre suf)
        (vector-split-at indexes prefix-length))
      (array-ref
       (array-ref item-arr pre)
       suf)))))

(module+ test
  (let ([y (j:shape (->noun '(3 4))
                      (->noun "abcdefghijkl"))])
    (check-equal? y (noun 'char (array #[#[#\a #\b #\c #\d]
                                         #[#\e #\f #\g #\h]
                                         #[#\i #\j #\k #\l]])))
    (check-equal? (j:shape (->noun '(2 2)) y)
                  (noun 'char (array #[#[#[#\a #\b #\c #\d]
                                         #[#\e #\f #\g #\h]]
                                       #[#[#\i #\j #\k #\l]
                                         #[#\a #\b #\c #\d]]]))))
  (check-equal? (j:shape (noun 'number (array #[#[1 2] #[1 2]]))
                         (noun 'number (array #[5 6 7 8])))
                (noun 'number (array #[#[#[5 6]] #[#[5 6]]]))))

(define (j:shape-of y)
  (noun
   'number
   (vector->array
    (noun-shape y))))

(module+ test
  (check-equal? (j:shape-of (->noun (index-array '#[2 3])))
                (noun 'number (array #[2 3])))
  (check-equal? (j:shape-of (->noun 3))
                (noun 'number (array #[])))
  (check-equal? (j:shape-of (->noun ""))
                (noun 'number (array #[0]))))

(define j:signum
  (wrap/monad
   (λ (n)
     (if (real? n)
         (sgn n)
         (make-polar 1 (angle n))))))

(module+ test
  (check-= (noun->racket-value (j:signum (->noun 3+4i)))
           0.6+0.8i
           0.001))

(define (j:tally y)
  (define y-shape
    (noun-shape y))
  (noun
   'number
   (array
    (if (equal? '#[] y-shape)
        1
        (vector-ref y-shape 0)))))

(module+ test
  (check-eqv? (noun->racket-value (j:tally (->noun ""))) 0)
  (check-eqv? (noun->racket-value (j:tally (->noun "a"))) 1)
  (check-eqv? (noun->racket-value (j:tally (->noun "octothorpe"))) 10)
  (check-eqv? (noun->racket-value (j:tally (->noun 3))) 1)
  (check-eqv? (noun->racket-value (j:tally (->noun (index-array '#[4 5 6])))) 4))

(define j:times
  (wrap/dyad *))
