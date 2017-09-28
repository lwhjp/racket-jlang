#lang racket/base

(require parser-tools/lex
         parser-tools/yacc
         racket/string
         "../number.rkt"
         "lex.rkt")

(define (wrap v start end)
  (let ([line (position-line start)]
        [col (position-col start)]
        [offset (position-offset start)]
        [span (- (position-offset end)
                 (position-offset start))])
    (datum->syntax
     #f
     v
     (list #f line col offset span))))

(define (parse-string str)
  (let ([v (string-replace
            (substring str 1 (sub1 (string-length str)))
            "''"
            "'")])
    (if (eqv? 1 (string-length v))
        (string-ref v 0)
        (string->list v))))

(define parse
  (parser
   (tokens WORDS PUNCTUATION)
   (src-pos)
   (error
    (λ (tok-ok? tok-name tok-value start-pos end-pos)
      (error "parse error")))
   (start maybe-sentence)
   (end EOL EOF)
   (grammar
    (maybe-sentence
     [(sentence) $1]
     [() '()])
    (sentence
     [(sentence-elements) (wrap (cons '#%sentence $1) $1-start-pos $1-end-pos)])
    (sentence-elements
     [(sentence-element sentence-elements) (cons $1 $2)]
     [(sentence-element) (list $1)])
    (sentence-element
     [(LPAREN sentence RPAREN) $2]
     [(word) $1])
    (word
     [(NAME) (cons '#%name (wrap (string->symbol $1) $1-start-pos $1-end-pos))]
     [(PRIMARY) (wrap (string->symbol $1) $1-start-pos $1-end-pos)]
     [(noun) (cons '#%noun (wrap $1 $1-start-pos $1-end-pos))])
    (noun
     [(number-list) $1]
     [(STRING) (wrap (parse-string $1) $1-start-pos $1-end-pos)])
    (number-list
     [(number number-list) (cons $1 $2)]
     [(number) (list $1)])
    (number
     [(NUMBER) (wrap (string->number/j $1) $1-start-pos $1-end-pos)]))))

(provide parse)