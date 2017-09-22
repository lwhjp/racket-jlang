#lang racket/base

(provide
 (struct-out word)
 adverb? make-primitive-adverb
 conjunction? make-primitive-conjunction
 (struct-out name)
 verb? make-primitive-verb make-cap make-hook make-fork
 noun?
 make-train)

(require math/array
         "../customize.rkt"
         "../rank.rkt")

(define (combine m d)
  (case-lambda [(y) (m y)] [(x y) (d x y)]))

(struct word ())

(struct adverb word (proc)
  #:property prop:procedure (struct-field-index proc))

(struct adverb:primitive adverb (name))

(define make-primitive-adverb
  (case-lambda
    [(name proc) (adverb:primitive (λ (u) (verb:compound (proc u))) name)]
    [(name monad dyad) (make-primitive-adverb name (combine monad dyad))]))

(struct adverb:compound adverb ())

(struct conjunction word (proc)
  #:property prop:procedure (struct-field-index proc))

(struct conjunction:primitive conjunction (name))

(define (make-primitive-conjunction name proc)
  (conjunction:primitive
   (λ (x y) (verb:compound (proc x y)))
   name))

(struct name word (locale id))

(struct verb word (proc)
  #:property prop:procedure (struct-field-index proc))

(struct verb:primitive verb (name))

(struct verb:customizable verb:primitive ()
  #:property prop:customize
  (λ (v param) (customize (verb-proc v) param)))

(define make-primitive-verb
  (case-lambda
    [(name proc)
     ((if (customizable? proc)
          verb:customizable
          verb:primitive)
      proc
      name)]
    [(name monad dyad)
     (make-primitive-verb
      name
      (if (or (customizable? monad) (customizable? dyad))
          (customizable-procedure
           (λ (param)
             (combine (if (customizable? monad) (customize monad param) monad)
                      (if (customizable? dyad) (customize dyad param) dyad)))
           (combine monad dyad))
          (combine monad dyad)))]))

(struct verb:cap verb:primitive ())

(define (make-cap name)
  (verb:cap (case-lambda) name))

(struct verb:compound verb ())

(define (make-hook g h)
  (verb:compound
   (case-lambda
     [(y) (g y (h y))]
     [(x y) (g x (h y))])))

(define (make-fork f g h)
  (verb:compound
   (cond
     [(verb:cap? f) (compose1 g h)]
     [(verb? f) (case-lambda
                  [(y) (g (f y) (h y))]
                  [(x y) (g (f x y) (h x y))])]
     [(noun? f) (case-lambda
                  [(y) (g f (h y))]
                  [(x y) (g f (h x y))])])))

(define (noun? v)
  (or (array? v)
      (box? v)
      (char? v)
      (number? v)
      (symbol? v)))

(define (make-train g h)
  (cond
    [(and (adverb? g) (adverb? h)) (adverb:compound (compose1 g h))]
    [(and (conjunction? g) (noun? h)) (adverb:compound (λ (y) (g y h)))]
    [(and (conjunction? g) (verb? h)) (adverb:compound (λ (y) (g y h)))]
    [(and (noun? g) (conjunction? h)) (adverb:compound (λ (y) (h g y)))]
    [(and (verb? g) (conjunction? h)) (adverb:compound (λ (y) (h g y)))]
    [(and (verb? g) (verb? h)) (make-hook g h)]
    [else (error "invalid train")]))
