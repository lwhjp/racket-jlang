#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         racket/string)

(provide lex WORDS PUNCTUATION)

(define-lex-abbrev end-of-line (:or "\n" "\r\n"))

(define-lex-abbrev comment
  (:: "NB." (complement (:: any-string end-of-line any-string))))

(define-lex-abbrevs
  ;; TODO
  [number (:: (:+ numeric) (:? #\. (:+ numeric)))])

(define-lex-abbrevs
  [primitive (:- graphic alphabetic numeric)]
  [name (:: alphabetic (:* (:or alphabetic #\_ numeric)))]
  [inflection (:or #\. #\:)]
  [primary (:or (:: primitive (:* inflection))
                (:: name (:+ inflection)))])

(define-lex-abbrev string
  (:: #\' (:* (:or (complement (:or #\' end-of-line)) "''")) #\'))

(define-tokens WORDS (NUMBER PRIMARY NAME STRING))
(define-empty-tokens PUNCTUATION (WS EOL EOF COMMENT LP RP))

(define lex
  (lexer
   [(:+ (:or #\space #\tab)) (token-WS)]
   [end-of-line (token-EOL)]
   [comment (token-COMMENT)]
   [#\( (token-LP)]
   [#\) (token-RP)]
   [number (token-NUMBER (parse-number lexeme))]
   [primary (token-PRIMARY (string->symbol lexeme))]
   [name (token-NAME (string->symbol lexeme))]
   [string (token-STRING (parse-string lexeme))]
   [(eof) (token-EOF)]))

(define (parse-number str)
  ;; TODO
  (string->number str))

(define (parse-string str)
  (string-replace (substring str 1 (sub1 (string-length str))) "''" "'"))