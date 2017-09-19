#lang racket/base

(require data/gvector
         "word.rkt")

(struct Environment
  (named-locales
   numbered-locales))

(struct Locale
  ([path #:mutable]
   vars))

(define (make-j-environment)
  (Environment
   (make-hasheq)
   (gvector)))

(define global-j-environment (make-j-environment))

(define current-j-environment (make-parameter global-j-environment))

(define current-j-locale (make-parameter 'base))

(define current-j-private-vars (make-parameter (make-hasheq)))

(define (with-new-private-vars thunk)
  (parameterize ([current-j-private-vars (make-hasheq)])
    (thunk)))

(define (make-locale)
  (Locale '(z) (make-hasheq)))

(define (locale id)
  (define env (current-j-environment))
  (cond
    [(number? id) (error "not implemented")]
    [(symbol? id) (hash-ref! (Environment-named-locales env) id make-locale)]
    [else (error 'locale "invalid id: ~a" id)]))

(define (locale-ref l-id v-id)
  (cond
    [(locale l-id)
     => (λ (l)
          (for/or ([l (in-list (cons l (map locale (Locale-path l))))]
                   #:when l)
            (hash-ref (Locale-vars l) v-id #f)))]
    [else #f]))

(define (locale-set! l-id v-id v)
  (hash-set! (Locale-vars (locale l-id)) v-id v))

(define (locale-path l-id)
  (Locale-path (locale l-id)))

(define (set-locale-path! l-id path)
  (set-Locale-path! (locale l-id) path))

(define (name-ref n)
  (define v-id (name-id n))
  (cond
    [(name-locale n) => (λ (l-id) (locale-ref l-id v-id))]
    [else (or (hash-ref (current-j-private-vars) v-id #f)
              (locale-ref (current-j-locale) v-id))]))

(define (name-set! n v global?)
  (define v-id (name-id n))
  (cond
    [(name-locale n) => (λ (l-id) (locale-set! l-id v-id v))]
    [global? (when (hash-has-key? (current-j-private-vars) v-id)
               (error 'name-set! "already exists as private variable: ~a" v-id))
             (locale-set! (current-j-locale) v-id v)]
    [else (hash-set! (current-j-private-vars) v-id v)]))

(provide global-j-environment
         current-j-environment
         current-j-locale
         current-j-private-vars
         make-j-environment
         with-new-private-vars
         name-ref
         name-set!)