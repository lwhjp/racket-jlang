#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract/base
         "../../frame/main.rkt"
         "noun.rkt")

(define monad/c
  (-> noun? noun?))

(define dyad/c
  (-> noun? noun? noun?))

(struct verb (monad dyad)
  #:property prop:procedure
  (case-lambda
    [(v y) ((verb-monad v) y)]
    [(v x y) ((verb-dyad v) x y)]))

(define (wrap/monad proc)
  (λ (y)
    (array->noun
     (apply/rank proc (list (noun-value y))))))

(define (wrap/dyad proc)
  (λ (x y)
    (array->noun
     (apply/rank proc (list (noun-value x) (noun-value y))))))

(define-syntax-rule (define/rank/noun (name . args) body ...)
  (define name
    (rank-lambda/noun args body ...)))

(begin-for-syntax
  (define-syntax-class ranked-arg
    #:attributes (id rank)
    (pattern id:identifier #:attr rank #''any)
    (pattern [id:identifier rank])))

(define-syntax rank-lambda/noun
  (syntax-parser
    [(_ (arg:ranked-arg ...)
        body ...)
     #'(make-ranked-procedure/noun
        (lambda (arg.id ...)
          body ...)
        arg.rank ...)]))

(define (make-ranked-procedure/noun proc . ranks)
  (procedure-reduce-arity
   (λ args
     (define arg-types
       (map noun-type args))
     (array->noun
      (apply/rank
       (ranked-procedure
        (λ arrs
          (noun-value
           (apply proc (map noun arg-types arrs))))
        (λ any ranks))
       (map noun-value args))))
   (length ranks)))

(provide
 monad/c
 dyad/c
 (contract-out
  (struct verb
    ([monad (or/c monad/c #f)]
     [dyad (or/c dyad/c #f)]))
  [wrap/monad (-> (-> any/c any/c) monad/c)]
  [wrap/dyad (-> (-> any/c any/c any/c) dyad/c)])
 define/rank/noun
 rank-lambda/noun)