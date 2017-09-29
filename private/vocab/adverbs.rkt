#lang racket/base

(provide (all-defined-out))

(require "../../rank.rkt")

; TODO: check arity
; TODO: maintain obverse if appropriate

(define (ja:reflex+passive u)
  ; TODO: evoke
  (case-lambda/rank
   [(y) (u y y)]
   [(x y) (u y x)]))

(define (ja:insert+table u)
  ; TODO: gerund
  (case-lambda/rank
   [(y) (for/fold ([a 0 #| XXX: identity |#])
                  ([t (in-items y)])
          (u a t))]
   ; TODO: table
   ))

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
