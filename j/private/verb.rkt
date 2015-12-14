#lang racket/base

(require racket/contract/base
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

(provide
 monad/c
 dyad/c
 (contract-out
  (struct verb
    ([monad (or/c monad/c #f)]
     [dyad (or/c dyad/c #f)]))
  [wrap/monad (-> (-> any/c any/c) monad/c)]
  [wrap/dyad (-> (-> any/c any/c any/c) dyad/c)]))