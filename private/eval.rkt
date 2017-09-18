#lang racket/base

(require racket/port
         "read.rkt")

(define-namespace-anchor here)

; This doesn't seem like the right way to do this...
(define j-namespace
  (parameterize
      ([current-namespace (namespace-anchor->empty-namespace here)])
    (namespace-require 'j/private/bindings)
    (current-namespace)))

(define (eval/j str)
  (eval
   (with-input-from-string str read-j)
   j-namespace))

(provide eval/j)