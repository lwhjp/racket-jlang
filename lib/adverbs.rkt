#lang racket/base

(provide (all-defined-out))

(require "../rank.rkt"
         "../private/word.rkt")

; TODO: check arity
; TODO: maintain obverse if appropriate

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
