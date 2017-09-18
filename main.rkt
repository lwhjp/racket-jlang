#lang racket/base

(require "private/eval.rkt"
         "private/locale.rkt"
         "rank.rkt")

(define (j str)
  (with-new-environment
   (λ ()
     (normalize-noun (eval/j str)))))

(provide j)