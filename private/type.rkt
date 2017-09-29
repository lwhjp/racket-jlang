#lang racket/base

(provide fill-for-frame)

(require math/array
         "vocab/nouns.rkt")

(define (fill-for-frame arr)
  (or (array-ormap guess-fill arr) (void)))

(define (guess-fill v)
  (cond
    [(array? v) (array-ormap guess-fill v)]
    [(number? v) 0]
    [(char? v) #\space]
    [(box? v) jn:ace]
    [else #f]))
