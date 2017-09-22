#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     "../number.rkt")
         racket/provide
         (prefix-in jlib: "../lib.rkt")
         "coupla.rkt"
         "word.rkt")

(define-syntax (define-word stx)
  (syntax-case stx ()
    [(_ j-id-str expr)
     (with-syntax ([j-id (format-id #'j-id-str "j:~a" (syntax-e #'j-id-str))])
       #'(define j-id expr))]))

(define-syntax-rule (define-verbs [id-str proc ...] ...)
  (begin (define-word id-str (make-primitive-verb (string->symbol id-str) proc ...)) ...))

(define TODO (λ args (error "not implemented")))

(define-verbs
  ["=" TODO jlib:equal]
  ["<" jlib:box jlib:less-than]
  ["<." TODO jlib:lesser-of]
  ["<:" jlib:decrement jlib:less-than-or-equal]
  [">" jlib:open jlib:larger-than]
  [">." TODO jlib:larger-of]
  [">:" jlib:increment jlib:larger-than-or-equal]
  ["+" jlib:conjugate jlib:plus]
  ["+." jlib:real+imaginary TODO]
  ["+:" jlib:double jlib:not-or]
  ["*" TODO jlib:times]
  ["*." jlib:length+angle TODO]
  ["*:" jlib:square jlib:not-and]
  ["-" jlib:negate jlib:minus]
  ["-." jlib:not TODO]
  ["-:" jlib:halve TODO]
  ["%" jlib:reciprocal jlib:divided-by]
  ["%." TODO TODO]
  ["%:" jlib:square-root jlib:root]
  ["^" jlib:exponential jlib:power]
  ["^." jlib:natural-log jlib:logarithm]
  ["$" jlib:shape-of TODO]
  ["$." TODO TODO]
  ["$:" TODO TODO] ; Not a monad/dyad pair
  ["~." TODO #f]
  ["~:" TODO jlib:not-equal]
  ["|" jlib:magnitude TODO]
  ["|." jlib:reverse TODO]
  ["|:" jlib:transpose TODO] ; Not a monad/dyad pair
  ["," jlib:ravel TODO]
  [",." TODO TODO]
  [",:" TODO TODO]
  [";" TODO TODO]
  [";:" TODO TODO]
  ["#" jlib:tally TODO]
  ["#." TODO TODO]
  ["#:" TODO TODO]
  ["!" TODO TODO]
  ["/:" TODO TODO]
  ["\\:" TODO TODO]
  ["[" jlib:same jlib:left]
  ["]" jlib:same jlib:right]
  ["{" TODO TODO]
  ["{." TODO TODO]
  ["{:" TODO #f]
  ["{::" TODO TODO]
  ["}." TODO TODO]
  ["}:" TODO #f]
  ["\"." TODO TODO]
  ["\":" TODO TODO]
  ["?" TODO TODO]
  ["?." TODO TODO]
  ["A." TODO TODO]
  ["C." TODO TODO]
  ["e." TODO TODO]
  ["E." #f TODO]
  ["i." jlib:integers TODO]
  ["i:" TODO TODO]
  ["I." TODO TODO]
  ["j." jlib:imaginary jlib:complex]
  ["L." TODO #f]
  ["o." TODO TODO]
  ["p." TODO TODO]
  ["p.." TODO TODO]
  ["p:" jlib:primes TODO]
  ["q:" jlib:prime-factors TODO]
  ["r." jlib:angle jlib:polar]
  ["s:" TODO TODO]
  ["u:" TODO TODO]
  ["x:" TODO TODO])

(define-word "[:" (make-cap '|[:|))

(define-syntax-rule (define-constant-verb id-str v)
  (define-word id-str (make-primitive-verb (string->symbol id-str) (lambda args v))))

(define-syntax (define-constant-verbs stx)
  (syntax-case stx ()
    [(_ j-val ...)
     (let ([j-val-strs (syntax->datum #'(j-val ...))])
       (with-syntax ([(id-str ...) (map (λ (j-val) (string-append j-val ":")) j-val-strs)]
                     [(v ...) (map string->number/j j-val-strs)])
         #'(begin (define-constant-verb id-str v) ...)))]))

(define-constant-verbs
  "_" "_9" "_8" "_7" "_6" "_5" "_4" "_3" "_2" "_1"
  "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")

(define-syntax-rule (define-adverbs [id-str proc ...] ...)
  (begin (define-word id-str (make-primitive-adverb (string->symbol id-str) proc ...)) ...))

(define-adverbs
  ["~" jlib:reflex jlib:passive]
  ["/" jlib:insert (λ args TODO)]
  ["/." TODO TODO]
  ["\\" TODO TODO]
  ["\\." TODO TODO]
  ["}" TODO TODO]
  ["b." TODO TODO]
  ["f." TODO]
  ["M." TODO]
  ["t." TODO]
  ["t:" TODO])

(define-syntax-rule (define-conjunctions [id-str proc] ...)
  (begin (define-word id-str (make-primitive-conjunction (string->symbol id-str) proc)) ...))

(define-conjunctions
  ["^:" TODO]
  ["." TODO]
  [".." TODO]
  [".:" TODO]
  [":" TODO]
  [":." TODO]
  ["::" TODO]
  [";." TODO]
  ["!." jlib:fit]
  ["!:" TODO]
  ["\"" TODO]
  ["`" TODO]
  ["`:" TODO]
  ["@" TODO]
  ["@." TODO]
  ["@:" TODO]
  ["&" TODO]
  ["&." TODO]
  ["&.:" TODO]
  ["&:" TODO]
  ["d." TODO]
  ["D." TODO]
  ["D:" TODO]
  ["H." TODO]
  ["L:" TODO]
  ["S:" TODO]
  ["T." TODO])

(define-word "=." coupla/local)
(define-word "=:" coupla/global)
(define-word "a." jlib:alphabet)
(define-word "a:" jlib:ace)

(provide (filtered-out
          (λ (name)
            (and (regexp-match? #rx"^j:." name)
                 (substring name 2)))
          (all-defined-out)))