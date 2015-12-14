#lang racket/base

(require (for-syntax racket/base
                     racket/provide-transform)
         "private/lib/adverbs.rkt"
         "private/lib/verbs.rkt"
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
          [+ conjugate plus]
          [* signum times]
          [- negate minus]
          [% reciprocal divide]
          [$ shape-of shape]
          [|,| ravel append]
          [|;| raze link]
          [|#| tally copy]
          [|{| head #f])
         (rename-out
          [insert-table /]
          [is/global =:])
         (rename-out
          [j-module-begin #%module-begin]
          [name #%name]
          [sentence #%sentence])
         #%app
         #%datum)