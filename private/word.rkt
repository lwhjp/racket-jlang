#lang racket/base

(require math/array
         "../customize.rkt"
         "../rank.rkt")

(struct word (spelling)
  #:property prop:custom-write
  (λ (v port mode)
    (cond
      [(word-spelling v) => (λ (spelling)
                              (fprintf port "#<~a:~a>" (object-name v) spelling))]
      [else (fprintf port "#<~a>" (object-name v))])))

(struct adverb word (proc)
  #:property prop:procedure (struct-field-index proc))

(struct conjunction word (proc)
  #:property prop:procedure (struct-field-index proc))

(struct name word (locale id))

(struct verb word (monad dyad)
  #:property prop:procedure
  (case-lambda
    ; TODO: check presence of monad / dyad
    [(v y) ((verb-monad v) y)]
    [(v x y) ((verb-dyad v) x y)]))

(struct customizable-verb verb ()
  #:property prop:customize
  (λ (v param)
    (verb
     #f
     (let ([monad (verb-monad v)])
       (if (customizable? monad) (customize monad param) monad))
     (let ([dyad (verb-dyad v)])
       (if (customizable? dyad) (customize dyad param) dyad)))))

(define (make-verb spelling monad dyad)
  (if (or (customizable? monad) (customizable? dyad))
      (customizable-verb spelling monad dyad)
      (verb spelling monad dyad)))

(define (constant-verb v)
  (verb #f
        (lambda/rank (y) v)
        (lambda/rank (y) v)))

(define (noun? v)
  (or (array? v)
      (box? v)
      (char? v)
      (number? v)
      (symbol? v)))

(provide (all-defined-out))