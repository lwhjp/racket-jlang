#lang racket/base

(provide (all-defined-out))

(require racket/sequence
         "../../rank.rkt"
         "verbs.rkt")

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

(define (ja:prefix+infix u)
  ; TODO: gerund
  (case-lambda/rank
   [(y) (apply/rank
         (λ (k) (u (jv:take k y)))
         (list (build-list (value-tally y) add1)))]
   [([x 0] y) (define yt (value-tally y))
              (apply/rank
               (λ (k) (u (jv:from (build-list (min (abs x) (- yt k)) (λ (i) (+ i k))) y)))
               (list (sequence->list (if (negative? x)
                                         (in-range 0 yt (- x))
                                         (in-range 0 (add1 (- yt x)))))))]))

;suffix
;outfix
;item amend
;amend
;boolean
;fix
;memo
;taylor-coeff
;weighted-taylor
