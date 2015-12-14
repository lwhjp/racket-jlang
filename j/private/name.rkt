#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide current-locale
         locative?
         name
         locale-ref
         set!/name)

;; FIXME: parameterize this
(define locales (make-hasheq))

(define (get-locale l)
  (hash-ref! locales l make-hasheq))

(define current-locale (make-parameter 'base))

(struct locative (name locale) #:transparent)

(define-syntax name
  (syntax-parser
    [(_ n:id) #'(locative 'n #f)]
    [(_ n:id l:id) #'(locative 'n 'l)]
    [(_ n:id l:expr) #'(locative 'n l)]))

(define (get-locale-for-locative l)
  (define ln (locative-locale l))
  (cond
    [(not ln) (get-locale (current-locale))]
    [(symbol? ln) (get-locale ln)]
    [(locative? ln) (locale-ref ln)]))

(define (locale-ref l [fail (λ () (error 'undefined))])
  (hash-ref
   (get-locale-for-locative l)
   (locative-name l)
   fail))

(define (set!/name l v)
  (hash-set!
   (get-locale-for-locative l)
   (locative-name l)
   v))