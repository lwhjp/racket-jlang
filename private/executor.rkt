#lang racket/base

(provide current-j-executor)

(define current-j-executor
  (make-parameter
   (λ () (error "not in a J execution context"))))
