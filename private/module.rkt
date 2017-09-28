#lang racket/base

(provide
 #%datum
 #%module-begin
 #%top-interaction
 #%name
 #%noun
 #%sentence)

(require (for-syntax racket/base)
         (rename-in racket/base [#%module-begin racket-module-begin])
         math/array
         racket/port
         "eval.rkt"
         "locale.rkt"
         "sentence.rkt"
         "word.rkt")

(define-syntax (#%module-begin stx)
  (syntax-case stx ()
    [(_ . str)
     #'(racket-module-begin
        (module configure-runtime racket/base
          (require j/private/locale
                   j/private/read)
          (current-j-locale 'base)
          (current-read-interaction read-j-syntax))
        (with-input-from-string str execute/j))]))

(define-syntax (#%name stx)
  (syntax-case stx ()
    [(_ . id)
     ; TODO: locative
     #'(name #f 'id)]))

(define-syntax (#%noun stx)
  (syntax-case stx ()
    [(_ v) #'v]
    [(_ v ...) #'(array #[v ...])]
    [(_ . v) #'v]))

(define-syntax (#%sentence stx)
  (syntax-case stx()
    [(_ word ...) #'(sentence word ...)]))
