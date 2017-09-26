#lang racket/base

(provide j-console)

(require "private/eval.rkt"
         "private/locale.rkt"
         "private/read.rkt")

(define (j-console)
  (parameterize ([current-namespace j-namespace]
                 [current-read-interaction read-j-syntax])
    (with-new-j-environment read-eval-print-loop)))
