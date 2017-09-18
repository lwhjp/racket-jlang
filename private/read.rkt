#lang racket/base

(require "lex.rkt"
         "parse.rkt")

(define (read-j [in (current-input-port)])
  (let ([v (read-j-syntax (object-name in) in)])
    (if (syntax? v)
        (syntax->datum v)
        v)))

(define (read-j-syntax [source-name (object-name (current-input-port))]
                       [in (current-input-port)])
  (define (next-token)
    (let ([tok (lex in)])
      (if (memq tok '(whitespace comment))
          (next-token)
          tok)))
  (if (eof-object? (peek-char in))
      eof
      (let ([v (parse next-token)])
        (if (null? v)
            (read-j-syntax source-name in)
            v))))

(provide (all-defined-out))