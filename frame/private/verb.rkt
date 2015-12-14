#lang racket/base

(require math/array
         racket/list
         racket/sequence)

(provide (all-defined-out))

(define (procedure-rank? v)
  (or (exact-integer? v)
      (eq? 'any v)))

(struct ranked-procedure
  (proc rank)
  #:property prop:procedure (struct-field-index proc))

(define (monadic-rank r)
  (λ (arity)
    (and (eqv? 1 arity)
         (list r))))

(define (dyadic-rank r1 r2)
  (λ (arity)
    (and (eqv? 2 arity)
         (list r1 r2))))

(define (rank-combine . ranks)
  (λ (arity)
    (for/or ([p (in-list ranks)])
      (p arity))))

(define (procedure-rank proc arity)
  (if (ranked-procedure? proc)
      ((ranked-procedure-rank proc) arity)
      (make-list arity 0)))

(define (->ranked-procedure proc)
  (if (ranked-procedure? proc)
      proc
      (ranked-procedure
       (λ args
         (array
          (apply
           proc
           (map (λ (arr)
                  (array-ref arr '#[]))
                args))))
       (λ (arity)
         (make-list arity 0)))))