#lang racket/base

(provide
 adverb? make-primitive-adverb make-compound-adverb
 conjunction? make-primitive-conjunction make-compound-conjunction
 (struct-out name)
 noun?
 make-hook make-fork make-train
 (all-from-out "verb.rkt"))

(require math/array
         "verb.rkt")

(struct adverb (proc)
  #:property prop:procedure
  (λ (v u)
    (make-compound-verb ((adverb-proc v) u))))

(struct adverb:primitive adverb (name))

(define make-primitive-adverb
  (case-lambda
    [(name proc) (adverb:primitive proc name)]
    [(name monad dyad) (adverb:primitive (λ (u) (combine-monad+dyad/verb (monad u) (dyad u))) name)]))

(struct adverb:compound adverb ())

(define make-compound-adverb adverb:compound)

(struct conjunction (proc)
  #:property prop:procedure
  (λ (v x y)
    (make-compound-verb ((conjunction-proc v) x y))))

(struct conjunction:primitive conjunction (name))

(struct conjunction:compound conjunction ())

(define (make-primitive-conjunction name proc)
  (conjunction:primitive proc name))

(define make-compound-conjunction conjunction:compound)

(struct name (locale id))

(define (noun? v)
  (or (array? v)
      (box? v)
      (char? v)
      (number? v)
      (symbol? v)))

(define (make-hook g h)
  (make-compound-verb
   (case-lambda
     [(y) (g y (h y))]
     [(x y) (g x (h y))])))

(define (make-fork f g h)
  (make-compound-verb
   (cond
     [(verb:cap? f) (compose1 g h)]
     [(verb? f) (case-lambda
                  [(y) (g (f y) (h y))]
                  [(x y) (g (f x y) (h x y))])]
     [(noun? f) (case-lambda
                  [(y) (g f (h y))]
                  [(x y) (g f (h x y))])])))

(define (make-train g h)
  (cond
    [(and (adverb? g) (adverb? h)) (adverb:compound (compose1 (adverb-proc g) (adverb-proc h)))]
    [(and (conjunction? g) (noun? h)) (adverb:compound (λ (y) (g y h)))]
    [(and (conjunction? g) (verb? h)) (adverb:compound (λ (y) (g y h)))]
    [(and (noun? g) (conjunction? h)) (adverb:compound (λ (y) (h g y)))]
    [(and (verb? g) (conjunction? h)) (adverb:compound (λ (y) (h g y)))]
    [(and (verb? g) (verb? h)) (make-hook g h)]
    [else (error "invalid train")]))
