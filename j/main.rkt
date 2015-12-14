#lang racket/base

(require "private/eval.rkt"
         "private/noun.rkt"
         "private/verb.rkt")

(provide j)

(define (j sentence)
  (define answer
    (eval-j sentence))
  (cond
    [(noun? answer)
     (noun->racket-value answer)]
    [(verb? answer)
     (λ args
       (noun->racket-value
        (apply answer (map ->noun args))))]
    [else (void)]))