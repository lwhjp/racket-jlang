#lang racket/base

(require racket/port
         "read.rkt")

(provide eval-j)

(define-namespace-anchor here)

(define (make-j-namespace)
  ;; This seems a bit weird
  (parameterize
      ([current-namespace (namespace-anchor->empty-namespace here)])
    (namespace-require 'j/lib)
    (current-namespace)))

(define (eval-j sentence [ns (make-j-namespace)])
  (eval
   (with-input-from-string sentence read-j)
   ns))