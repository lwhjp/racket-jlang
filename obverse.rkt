#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [prop:obverse (struct-type-property/c (-> has-obverse? procedure?))]
  [has-obverse? predicate/c]
  [obverse (-> has-obverse? procedure?)]))

(define-values (prop:obverse has-obverse? get-obverse)
  (make-struct-type-property 'obverse))

(define (obverse v)
  ((get-obverse v) v))
