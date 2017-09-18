#lang racket/base

(require data/gvector
         "word.rkt")

(struct Environment (numbered named))
(struct Locale (name [path #:mutable] vars))

(define current-environment (make-parameter #f))
(define current-locale (make-parameter #f))
(define current-private-vars (make-parameter #f))

(define (with-new-environment thunk)
  (parameterize* ([current-environment (make-environment)]
                  [current-locale (locale 'base)]
                  [current-private-vars (make-hasheq)])
    (thunk)))

(define (with-new-private-vars thunk)
  (parameterize ([current-private-vars (make-hasheq)])
    (thunk)))

(define (make-environment)
  (Environment (gvector) (make-hasheq)))

(define (make-locale id)
  (Locale id '(z) (make-hasheq)))

(define (locale id)
  (define env (current-environment))
  (cond
    [(number? id) (error "not implemented")]
    [(symbol? id) (hash-ref! (Environment-named env) id (λ () (make-locale id)))]
    [else (error 'locale "invalid id: ~a" id)]))

(define (locale-ref l-id v-id)
  (cond
    [(locale l-id) => (λ (l) (*locale-ref l v-id))]
    [else #f]))

(define (*locale-ref l v-id)
  (for/or ([l (in-list (cons l (map locale (Locale-path l))))]
           #:when l)
    (hash-ref (Locale-vars l) v-id #f)))

(define (locale-set! l-id v-id v)
  (*locale-set! (locale l-id) v-id v))

(define (*locale-set! l v-id v)
  (hash-set! (Locale-vars l) v-id v))

(define (locale-path l-id)
  (Locale-path (locale l-id)))

(define (set-locale-path! l-id path)
  (set-Locale-path! (locale l-id) path))

(define (name-ref n)
  (define v-id (name-id n))
  (cond
    [(name-locale n) => (λ (l-id) (locale-ref l-id v-id))]
    [else (or (hash-ref (current-private-vars) v-id #f)
              (*locale-ref (current-locale) v-id))]))

(define (name-set! n v global?)
  (define v-id (name-id n))
  (cond
    [(name-locale n) => (λ (l-id) (locale-set! l-id v-id v))]
    [global? (when (hash-has-key? (current-private-vars) v-id)
               (error 'name-set! "already exists as private variable: ~a" v-id))
             (*locale-set! (current-locale) v-id v)]
    [else (hash-set! (current-private-vars) v-id v)]))

;; XXX: global environment is not good!

(current-environment (make-environment))
(current-locale (locale 'base))
(current-private-vars (make-hasheq))

(provide current-locale
         with-new-environment
         with-new-private-vars
         name-ref
         name-set!)