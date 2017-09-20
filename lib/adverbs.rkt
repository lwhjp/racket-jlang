#lang racket/base

(require "../rank.rkt")

; TODO: check arity

(define (reflex u)
  (lambda/rank (y) (u y y)))

(define (passive u)
  (lambda/rank (x y) (u y x)))

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