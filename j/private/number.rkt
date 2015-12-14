#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         racket/math
         racket/string)

(provide string->number/j)

(define-tokens NUMERIC
  (IN-BASE DIGITS))

(define-tokens NUMERIC-EMPTY
  (_ E AD AR J P X R NAN INVALID END))

(define lex-number
  (lexer
   [(eof) 'END]
   [(:: (:+ numeric) (:? (:: #\. (:* numeric))))
    (token-DIGITS lexeme)]
   ["_." 'NAN]
   [(:: #\b (:: (:* (:or numeric lower-case))
                (:? (:: #\. (:* (:or numeric lower-case))))))
    (token-IN-BASE (substring lexeme 1))]
   [(:or #\_ #\e "ad" "ar" #\j #\p #\x #\r)
    (string->symbol (string-upcase lexeme))]
   [any-char 'INVALID]))

;; We'll cheat and use Racket's string->number for part
;; of this to avoid tricky precision errors.
(define parse-number
  (parser
   (tokens NUMERIC NUMERIC-EMPTY)
   (start number)
   (end END)
   (error
    (lambda (tok-ok? tok-name tok-value)
      (error "ill-formed number")))
   (grammar
    [digit-str
     [(DIGITS) $1]]
    [signed-str
     [(_ digit-str) (string-append "-" $2)]
     [(digit-str) $1]]
    [exp-str
     [(signed-str E signed-str) (string-append $1 "e" $3)]
     [(signed-str) $1]]
    [basic
     [(exp-str) (let ([n (string->number $1)])
                  (if (integer? n)
                      (inexact->exact n)
                      n))]
     [(_) +inf.0]
     [(_ _) -inf.0]]
    [rational
     [(basic R basic) (/ $1 $3)]
     [(basic) $1]]
    [complex
     [(rational AD rational) (make-polar $1 (degrees->radians $3))]
     [(rational AR rational) (make-polar $1 $3)]
     [(rational J rational) (make-rectangular $1 $3)]
     [(rational) $1]]
    [pi-e
     [(complex P complex) (* $1 (expt pi $3))]
     [(complex X complex) (* $1 (exp $3))]
     [(complex) $1]]
    [in-base
     [(pi-e IN-BASE) (*string->number $2 $1)]
     [(pi-e) $1]]
    [extended
     ;; TODO
     [(in-base) $1]]
    [number
     [(NAN) +nan.0]
     [(extended) $1]])))

(define (*string->number str radix)
  ;; radix can be pretty much anything, so we can't use the Racket version
  (define (char->digit c)
    (- (char->integer c)
       (if (char-numeric? c)
           (char->integer #\0)
           (- (char->integer #\a) 10))))
  (define parts (string-split str "."))
  (define mag (sub1 (string-length (car parts))))
  (for/sum ([c (in-string (string-append* parts))]
            [i (in-naturals)])
    (* (char->digit c)
       (expt radix (- mag i)))))

(define (string->number/j str)
  (parse-number
   (let ([in (open-input-string str)])
     (λ () (lex-number in)))))

(module+ test
  (require rackunit)
  (let ([ε 0.001])
    (check-eqv? (string->number/j "2.3e2") 230)
    (check-= (string->number/j "2.3e_2") 0.023 ε)
    (check-eqv? (string->number/j "2j3") 2+3i)
    (check-= (string->number/j "2p1") 6.28319 ε)
    (check-= (string->number/j "1p_1") 0.31831 ε)
    (check-= (string->number/j "1x2") 7.38906 ε)
    (check-= (string->number/j "2x1") 5.43656 ε)
    (check-= (string->number/j "1x_1") 0.367879 ε)
    (check-eqv? (string->number/j "2e2j_2e2") 200-200i)
    (check-= (string->number/j "2e2j2p1") 628.319+6.28319i ε)
    (check-= (string->number/j "2ad45") 1.41421+1.41421i ε)
    (check-= (string->number/j "2ar0.785398") 1.41421+1.41421i ε)
    (check-eqv? (string->number/j "16b1f") 31)
    (check-eqv? (string->number/j "10b23") 23)
    (check-eqv? (string->number/j "_10b23") -17)
    (check-eqv? (string->number/j "1e2b23") 203)
    (check-= (string->number/j "2b111.111") 7.875 ε)
    (check-eqv? (string->number/j "1r2") 1/2)
    (check-eqv? (string->number/j "_1r2") -1/2)
    (check-eqv? (string->number/j "2r4") 1/2)
    (check-eqv? (string->number/j "2r_4") -1/2)
    (check-eqv? (string->number/j "_2r_4") 1/2)
    (check-eqv? (string->number/j "0r9") 0)
    (check-eqv? (string->number/j "5") 5)
    (check-eqv? (string->number/j "_5") -5)
    (check-pred nan? (string->number/j "_."))
    (check-pred (λ (x) (and (infinite? x) (positive? x)))
                (string->number/j "_"))
    (check-pred (λ (x) (and (infinite? x) (negative? x)))
                (string->number/j "__"))))