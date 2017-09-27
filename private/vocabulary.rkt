#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     "../number.rkt")
         racket/provide
         (prefix-in ja: "../lib/adverbs.rkt")
         (prefix-in jc: "../lib/conjunctions.rkt")
         (prefix-in jn: "../lib/nouns.rkt")
         (prefix-in jv: "../lib/verbs.rkt")
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
  ["=" TODO jv:equal]
  ["<" jv:box jv:less-than]
  ["<." TODO jv:lesser-of]
  ["<:" jv:decrement jv:less-than-or-equal]
  [">" jv:open jv:larger-than]
  [">." TODO jv:larger-of]
  [">:" jv:increment jv:larger-than-or-equal]
  ["+" jv:conjugate jv:plus]
  ["+." jv:real+imaginary TODO]
  ["+:" jv:double jv:not-or]
  ["*" TODO jv:times]
  ["*." jv:length+angle TODO]
  ["*:" jv:square jv:not-and]
  ["-" jv:negate jv:minus]
  ["-." jv:not TODO]
  ["-:" jv:halve TODO]
  ["%" jv:reciprocal jv:divided-by]
  ["%." TODO TODO]
  ["%:" jv:square-root jv:root]
  ["^" jv:exponential jv:power]
  ["^." jv:natural-log jv:logarithm]
  ["$" jv:shape-of TODO]
  ["$." TODO TODO]
  ["$:" TODO TODO] ; Not a monad/dyad pair
  ["~." TODO]
  ["~:" TODO jv:not-equal]
  ["|" jv:magnitude TODO]
  ["|." jv:reverse TODO]
  ["|:" jv:transpose TODO] ; Not a monad/dyad pair
  ["," jv:ravel TODO]
  [",." TODO TODO]
  [",:" TODO TODO]
  [";" TODO TODO]
  [";:" TODO TODO]
  ["#" jv:tally TODO]
  ["#." TODO TODO]
  ["#:" TODO TODO]
  ["!" TODO TODO]
  ["/:" TODO TODO]
  ["\\:" TODO TODO]
  ["[" jv:same jv:left]
  ["]" jv:same jv:right]
  ["{" TODO TODO]
  ["{." TODO TODO]
  ["{:" TODO]
  ["{::" TODO TODO]
  ["}." TODO TODO]
  ["}:" TODO]
  ["\"." TODO TODO]
  ["\":" TODO TODO]
  ["?" TODO TODO]
  ["?." TODO TODO]
  ["A." TODO TODO]
  ["C." TODO TODO]
  ["e." TODO TODO]
  ["E." TODO]
  ["i." jv:integers TODO]
  ["i:" TODO TODO]
  ["I." TODO TODO]
  ["j." jv:imaginary jv:complex]
  ["L." TODO]
  ["o." TODO TODO]
  ["p." TODO TODO]
  ["p.." TODO TODO]
  ["p:" jv:primes TODO]
  ["q:" jv:prime-factors TODO]
  ["r." jv:angle jv:polar]
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
  ["~" ja:reflex ja:passive]
  ["/" ja:insert (λ args TODO)]
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
  ["^:" jc:power]
  ["." TODO]
  [".." TODO]
  [".:" TODO]
  [":" (λ (x y) (if (and (procedure? x) (procedure? y)) (jc:monad+dyad x y) (jc:explicit x y)))]
  [":." jc:obverse]
  ["::" (λ (u v) (jc:adverse u v #:pred exn:fail?))] ; TODO: only catch J errors
  [";." TODO]
  ["!." jc:fit]
  ["!:" TODO]
  ["\"" jc:rank]
  ["`" TODO]
  ["`:" TODO]
  ["@" jc:atop]
  ["@." TODO]
  ["@:" jc:at]
  ["&" (λ (x y) (if (and (procedure? x) (procedure? y)) (jc:compose x y) (jc:bond x y)))]
  ["&." TODO]
  ["&.:" TODO]
  ["&:" jc:appose]
  ["d." TODO]
  ["D." TODO]
  ["D:" TODO]
  ["H." TODO]
  ["L:" TODO]
  ["S:" TODO]
  ["T." TODO])

(define-word "=." coupla/local)
(define-word "=:" coupla/global)
(define-word "a." jn:alphabet)
(define-word "a:" jn:ace)

(provide (filtered-out
          (λ (name)
            (and (regexp-match? #rx"^j:." name)
                 (substring name 2)))
          (all-defined-out)))
