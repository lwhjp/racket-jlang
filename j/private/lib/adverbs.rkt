#lang racket/base

(require math/array
         racket/sequence
         racket/vector
         "../../../frame/main.rkt"
         "../adverb.rkt"
         "../noun.rkt"
         "../verb.rkt")

(provide (all-defined-out))

(define insert-table
  (adverb
   (λ (u)
     (define d (verb-dyad u))
     (unless d
       (error "verb is not dyadic"))
     (define identity (->noun 0)) ;; FIXME
     (verb
      (λ (y)
        (define items (noun->items y))
        (if (null? items)
            identity
            (for/fold ([a (car items)])
                      ([b (in-list (cdr items))])
              (d a b))))
      (λ (x y)
        ;; TODO left arity 1
        (define x-items (in-noun-items x))
        (define y-items (in-noun-items y))
        (array->noun
         (unframe/fill
          (build-array
           (vector-append (noun-shape x) (noun-shape y))
           (λ (indexes)
             (noun-value
              (d
               (sequence-ref x-items (vector-ref indexes 0))
               (sequence-ref y-items (vector-ref indexes 1))))))
          identity)))))))