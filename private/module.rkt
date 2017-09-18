#lang racket/base

(require (for-syntax racket/base)
         (rename-in racket/base [#%module-begin racket-module-begin])
         math/array
         "sentence.rkt"
         "word.rkt")

(define-syntax (#%name stx)
  (syntax-case stx ()
    [(_ . id)
     ; TODO: locative
     #'(name 'id #f 'id)]))

(define-syntax (#%noun stx)
  (syntax-case stx ()
    [(_ v) #'v]
    [(_ v0 v ...) #'(array #[v0 v ...])]))

(define-syntax (#%sentence stx)
  (syntax-case stx()
    [(_ word ...) #'(sentence word ...)]))

(provide
 #%datum
 #%module-begin
 #%name
 #%noun
 #%sentence)