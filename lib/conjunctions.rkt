#lang racket/base

(provide (filtered-out
          (λ (name)
            (and (regexp-match? #rx"^jc:." name)
                 (substring name 3)))
          (all-from-out "../private/vocab/conjunctions.rkt")))

(require (for-syntax racket/base)
         racket/provide
         "../private/vocab/conjunctions.rkt")
