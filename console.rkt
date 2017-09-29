#lang racket/base

(provide j-console)

(require "private/executor.rkt"
         "private/eval.rkt"
         "private/locale.rkt"
         "private/read.rkt")

(define (j-console)
  (parameterize ([current-j-executor execute/j]
                 [current-namespace j-namespace]
                 [current-read-interaction read-j-syntax])
    (with-new-j-environment read-eval-print-loop)))
