#lang racket/base

(require "lex.rkt"
         "parse.rkt")

(provide (all-defined-out))

(define (read-j [in (current-input-port)])
  (define s
    (parse (λ () (lex in))))
  (if (null? s)
      (if (eof-object? (peek-byte in))
          eof
          (read-j in))
      s))

(define (syntax:read-j [src (object-name (current-input-port))]
                       [in (current-input-port)])
  (define s (read-j in))
  (if (eof-object? s)
      eof
      (datum->syntax #f s)))