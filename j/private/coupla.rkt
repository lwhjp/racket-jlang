#lang racket/base

(require "name.rkt")

(provide (all-defined-out))

(define (is/global x y)
  ;; TODO: implement fully
  (unless (locative? x)
    (error "can't assign to non-name:" x))
  (set!/name x y)
  y)