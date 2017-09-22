#lang racket/base

(require (for-syntax racket/base)
         racket/provide
         "../customize.rkt")

;power
;determinant
;dot-product
;even
;odd
;explicit
;monad-dyad
;obverse
;adverse
;cut

(define j:fit customize)

;foreign
;rank
;tie
;evoke-gerund
;atop
;agenda
;at
;bond/compose
;under
;appose
;derivative
;secant-slope
;hypergeometric
;level-at
;spread
;taylor-approximation

(provide (filtered-out
          (λ (name)
            (and (regexp-match? #rx"^j:." name)
                 (substring name 2)))
          (all-defined-out)))
