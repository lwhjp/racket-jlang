#lang racket/base

(require racket/port
         "locale.rkt"
         "read.rkt")

(define-namespace-anchor here)

; This doesn't seem like the right way to do this...
(define j-namespace
  (parameterize
      ([current-namespace (namespace-anchor->empty-namespace here)])
    (namespace-require 'j/private/bindings)
    (current-namespace)))

(define (eval/j str
                [env (make-j-environment)]
                #:locale [locale 'base])
  (parameterize ([current-j-environment env]
                 [current-j-locale locale]
                 [current-j-private-vars (make-hasheq)])
    (eval
     (with-input-from-string str read-j)
     j-namespace)))

(provide eval/j)