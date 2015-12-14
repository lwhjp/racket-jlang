#lang racket/base

(require parser-tools/yacc
         "lex.rkt")

(provide parse)

(define parse
  (parser
   (tokens WORDS PUNCTUATION)
   (start sentence)
   (end EOL EOF)
   (error
    (λ (tok-ok? tok-name tok-value)
      (error "parse error" tok-name)))
   (grammar
    [sentence
     [(words) (if (null? $1) '() (cons '#%sentence $1))]]
    [words
     [(maybe-ws word words) (cons $2 $3)]
     [(maybe-ws LP sentence RP words) (cons $3 $5)]
     [(maybe-ws) '()]]
    [maybe-ws
     [(ws maybe-ws) 'ws]
     [() 'ws]]
    [ws
     [(WS) 'ws]
     [(COMMENT) 'comment]]
    [word
     [(NUMBER) $1]
     [(PRIMARY) $1]
     [(NAME) `(#%name ,$1)]
     [(STRING) $1]])))