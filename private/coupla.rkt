#lang racket/base

(require "locale.rkt"
         "word.rkt")

(struct coupla word (name proc)
  #:property prop:procedure (struct-field-index proc))

(define-values (coupla/global coupla/local)
  (let ([make-coupla
         (λ (global? name)
           (coupla
            name
            (λ (x y)
              ; TODO: indirect names
              (unless (name? x) (error "indirect coupla not implemented"))
              (name-set! x y global?)
              y)))])
    (values (make-coupla #t '=:) (make-coupla #f '=.))))

(provide (all-defined-out))