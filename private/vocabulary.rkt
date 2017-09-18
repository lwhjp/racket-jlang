#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/provide
         (prefix-in jlib: "../lib.rkt")
         "coupla.rkt"
         "word.rkt")

(define-syntax (define-word stx)
  (syntax-case stx ()
    [(_ j-id-str expr)
     (with-syntax ([j-id (format-id #'j-id-str "j:~a" (syntax-e #'j-id-str))])
       #'(define j-id expr))]))

(define-syntax-rule (define-verbs/monad+dyad [id-str monad dyad] ...)
  (begin (define-word id-str (verb (string->symbol id-str) monad dyad)) ...))

(define TODO (λ args (error "not implemented")))

(define-verbs/monad+dyad
  ["=" TODO TODO]
  ["<" box TODO]
  ["<." TODO TODO]
  ["<:" jlib:decrement TODO]
  [">" jlib:open TODO]
  [">." TODO TODO]
  [">:" jlib:increment TODO]
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
  ["~:" TODO TODO]
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
  ["[:" TODO TODO] ; Special
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

(define-word "_:" (constant-verb +inf.0))
(define-word "_9:" (constant-verb -9))
(define-word "_8:" (constant-verb -8))
(define-word "_7:" (constant-verb -7))
(define-word "_6:" (constant-verb -6))
(define-word "_5:" (constant-verb -5))
(define-word "_4:" (constant-verb -4))
(define-word "_3:" (constant-verb -3))
(define-word "_2:" (constant-verb -2))
(define-word "_1:" (constant-verb -1))
(define-word "0:" (constant-verb 0))
(define-word "1:" (constant-verb 1))
(define-word "2:" (constant-verb 2))
(define-word "3:" (constant-verb 3))
(define-word "4:" (constant-verb 4))
(define-word "5:" (constant-verb 5))
(define-word "6:" (constant-verb 6))
(define-word "7:" (constant-verb 7))
(define-word "8:" (constant-verb 8))
(define-word "9:" (constant-verb 9))

(define-syntax-rule (define-adverbs/monad+dyad [id-str monad dyad] ...)
  (begin (define-word id-str (adverb (string->symbol id-str) (λ (u) (verb #f (monad u) (dyad u))))) ...))

(define-adverbs/monad+dyad
  ["~" jlib:reflex jlib:passive]
  ["/" jlib:insert (λ args TODO)]
  ["/." TODO TODO]
  ["\\" TODO TODO]
  ["\\." TODO TODO]
  ["}" TODO TODO]
  ["b." TODO TODO]
  ["f." TODO TODO]
  ["M." TODO TODO]
  ["t." TODO TODO]
  ["t:" TODO TODO])

(define-syntax-rule (define-conjunctions [id-str proc] ...)
  (begin (define-word id-str (conjunction (string->symbol id-str) proc)) ...))

(define-conjunctions
  ["^:" TODO]
  ["." TODO]
  [".." TODO]
  [".:" TODO]
  [":" TODO]
  [":." TODO]
  ["::" TODO]
  [";." TODO]
  ["!." TODO]
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