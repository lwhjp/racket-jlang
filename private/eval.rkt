#lang racket/base

(provide
 j-namespace
 execute/j)

(require "executor.rkt"
         "locale.rkt"
         "read.rkt")

(define-namespace-anchor here)

(define j-namespace
  (parameterize
      ([current-namespace (namespace-anchor->empty-namespace here)])
    (namespace-require 'j/private/bindings)
    (current-namespace)))

(define (execute/j)
  (parameterize ([current-j-executor execute/j]
                 [current-namespace j-namespace])
    (with-new-j-private-vars
      (λ ()
        (for/last ([stx (in-producer read-j-syntax)]
                   #:break (eof-object? stx))
          (eval stx))))))
