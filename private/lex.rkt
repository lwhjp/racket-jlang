#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-lex-abbrev comment
  (:: "NB." (complement (:: any-string #\newline any-string))))

(define-lex-abbrev number
  (:: (:or numeric #\_) (:* (:or numeric alphabetic #\. #\_))))

(define-lex-abbrev string
  (:: #\' (:* (:or (char-complement (:or #\' #\newline)) "''")) #\'))

(define-lex-abbrev name
  (:: alphabetic (:* (:or alphabetic #\_ numeric))))

(define-lex-abbrev primary
  (:or (char-complement (:or numeric alphabetic #\_ #\'))
       (:: (:or graphic name) (:+ (:or #\. #\:)))))

(define-tokens WORDS
  (NUMBER STRING NAME PRIMARY))

(define-empty-tokens PUNCTUATION
  (EOL EOF LPAREN RPAREN))

(define lex
  (lexer-src-pos
   [#\newline (token-EOL)]
   [(:+ (:- whitespace #\newline)) (return-without-pos 'whitespace)]
   [comment (return-without-pos 'comment)]
   [#\( (token-LPAREN)]
   [#\) (token-RPAREN)]
   [primary (token-PRIMARY lexeme)]
   [name (token-NAME lexeme)]
   [number (token-NUMBER lexeme)]
   [string (token-STRING lexeme)]
   [(eof) (token-EOF)]))

(provide lex WORDS PUNCTUATION)