#lang racket/base

(require "private/eval.rkt"
         "private/locale.rkt"
         "rank.rkt")

(define (j str)
  (normalize-noun (eval/j str)))

(provide j)