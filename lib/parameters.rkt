#lang racket/base

(provide (all-defined-out))

(define current-default-tolerance (make-parameter (expt 2 -44)))
