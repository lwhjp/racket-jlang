#lang racket/base

(provide j)

(require racket/port
         "private/eval.rkt"
         "private/locale.rkt")

(define (j str)
  (with-new-j-environment
    (λ ()
      (with-input-from-string str execute/j))))
