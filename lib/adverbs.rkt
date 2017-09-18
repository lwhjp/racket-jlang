#lang racket/base

(require "../rank.rkt")

; TODO: check arity

(define (reflex u)
  (lambda/rank (y)
    (apply/rank u y y)))

(define (passive u)
  (define u-rank (procedure-rank u))
  (lambda/rank ([x (cadr u-rank)] [y (car u-rank)])
    (apply/rank u y x)))

(define (insert u)
  ; TODO: gerund
  (lambda/rank (y)
    (for/fold ([a 0 #| XXX: identity |#])
              ([t (in-items y)])
      (u a t))))

;table
;oblique
;key
;prefix
;infix
;suffix
;outfix
;item amend
;amend
;boolean
;fix
;memo
;taylor-coeff
;weighted-taylor

(provide (all-defined-out))