#lang s-exp syntax/module-reader
j/private/bindings
#:read read-j-module
#:read-syntax read-j-module-syntax
#:whole-body-readers? #t

(require racket/port
         "../private/read.rkt")

(define (read-j-module in)
  (port->string in))

(define (read-j-module-syntax src in)
  (datum->syntax #f (read-j-module in)))
