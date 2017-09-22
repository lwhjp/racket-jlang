#lang racket/base

(require (for-syntax racket/base)
         racket/list
         racket/match
         racket/provide
         racket/sequence
         "../customize.rkt"
         "../rank.rkt")

(define (j:power u n)
  ; TODO: inverse, boxed, gerund, infinite
  (case-lambda/rank
    [(y) (for/fold ([y y]) ([i (in-range n)]) (u y))]
    [(x y) (for/fold ([y y]) ([i (in-range n)]) (u x y))]))

;determinant
;dot-product
;even
;odd
;explicit

(define (j:monad+dyad u v)
  (define monad-rank (procedure-rank u 1))
  (define dyad-rank (procedure-rank v 2))
  (make-ranked-procedure
   (case-lambda [(y) (u y)] [(x y) (v x y)])
   (λ (arity) (case arity [(1) monad-rank] [(2) dyad-rank]))))

;obverse

(define (j:adverse u v #:pred [pred exn:fail?])
  (make-ranked-procedure
   (λ args (with-handlers ([pred (apply v args)]) (apply u args)))
   (λ (arity) (make-list arity #f))))

;cut

(define j:fit customize)

;foreign

(define (j:rank m n)
  (define (->rank v)
    (cond
      [(integer? v) (inexact->exact v)]
      [(eqv? +inf.0) #f]
      [(eqv? -inf.0) 0]
      [else (error "invalid rank:" v)]))
  (unless (and (<= (noun-rank n) 1)
               (<= 1 (noun-tally n) 3))
    (error "invalid rank specifier:" n))
  (define-values (monad-rank dyad-rank)
    (match (map ->rank (sequence->list (in-items n)))
      [(list r) (values (list r) (list r r))]
      [(list l r) (values (list r) (list l r))]
      [(list m l r) (values (list m) (list l r))]))
  (make-ranked-procedure
   (if (procedure? m)
       (case-lambda [(y) (m y)] [(x y) (m x y)])
       (case-lambda [(y) m] [(x y) m]))
   (λ (arity) (case arity [(1) monad-rank] [(2) dyad-rank]))))

;tie
;evoke-gerund

(define (j:atop u v)
  (make-ranked-procedure
   (compose1 u v)
   (λ (arity) (procedure-rank v arity))))

;agenda

(define (j:at u v)
  (make-ranked-procedure
   (compose1 u v)
   (λ (arity) (make-list arity #f))))

(define (j:bond x y)
  (define proc
    (match* (x y)
      [(m (? procedure? v)) (λ (y) (v m y))]
      [((? procedure? u) n) (λ (y) (u y n))]))
  (case-lambda/rank
   [(y) (proc y)]
   [([x 0] y) (j:power proc x) y]))

(define (j:compose u v)
  (define mv (procedure-rank v 1))
  (make-ranked-procedure
   (case-lambda
     [(y) (u (v y))]
     [(x y) (u (v x) (v y))])
   (λ (arity) (make-list arity mv))))

;under

(define (j:appose u v)
  (make-ranked-procedure
   (case-lambda
     [(y) (u (v y))]
     [(x y) (u (v x) (v y))])
   (λ (arity) (make-list arity #f))))

;derivative
;secant-slope
;hypergeometric
;level-at
;spread
;taylor-approximation

(provide (filtered-out
          (λ (name)
            (and (regexp-match? #rx"^j:." name)
                 (substring name 2)))
          (all-defined-out)))
