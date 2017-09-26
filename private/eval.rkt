#lang racket/base

(provide
 j-namespace
 execute/j)

(require "read.rkt")

(define-namespace-anchor here)

(define j-namespace
  (parameterize
      ([current-namespace (namespace-anchor->empty-namespace here)])
    (namespace-require 'j/private/bindings)
    (current-namespace)))

(define (execute/j)
  (parameterize ([current-namespace j-namespace])
    (for/last ([stx (in-producer read-j-syntax)]
               #:break (eof-object? stx))
      (eval stx))))
