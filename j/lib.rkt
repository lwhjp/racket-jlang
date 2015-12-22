#lang racket/base

(require (for-syntax racket/base
                     racket/provide-transform)
         (prefix-in j: (combine-in "private/lib/adverbs.rkt"
                                   "private/lib/verbs.rkt"))
         "private/coupla.rkt"
         "private/name.rkt"
         "private/sentence.rkt"
         "private/verb.rkt")

(define-syntax verb-out
  (make-provide-pre-transformer
   (λ (stx modes)
     (syntax-case stx ()
       [(_ [out-id monad dyad] ...)
        (with-syntax ([(v-id ...) (generate-temporaries #'(out-id ...))])
          (syntax-local-lift-module-end-declaration
           #'(define-values (v-id ...)
               (values (verb monad dyad) ...)))
          #'(rename-out [v-id out-id] ...))]))))

(define-syntax j-module-begin
  (make-rename-transformer #'#%module-begin))

(provide (verb-out
          [+ j:conjugate j:plus]
          [* j:signum j:times]
          [- j:negate j:minus]
          [% j:reciprocal j:divide]
          [$ j:shape-of j:shape]
          [|,| j:ravel j:append]
          [|;| j:raze j:link]
          [|#| j:tally j:copy]
          [|{| j:head #f])
         (rename-out
          [j:insert-table /]
          [is/global =:])
         (rename-out
          [j-module-begin #%module-begin]
          [name #%name]
          [sentence #%sentence])
         #%app
         #%datum)