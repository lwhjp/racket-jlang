#lang racket/base

(require math/array
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