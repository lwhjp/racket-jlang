#lang racket/base

(require racket/contract/base)

(define (list-length-exactly/c n)
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'list-length-exactly/c "exact-nonnegative-integer?" n))
  (flat-named-contract
   `(list-length-exactly/c ,n)
   (λ (v) (and (list? v) (eqv? n (length v))))))

(define procedure-rank/c (or/c exact-integer? #f))

(define rankable-procedure/c
  (and/c procedure?
         (flat-named-contract
          'procedure-without-required-keywords?
          (λ (v)
            (let-values ([(kws opt-kws) (procedure-keywords v)])
              (null? kws))))
         (unconstrained-domain-> any/c)))

(provide
 list-length-exactly/c
 procedure-rank/c
 rankable-procedure/c
 lambda/rank
 case-lambda/rank
 define/rank
 (contract-out
  [ranked-procedure? predicate/c]
  [apply/rank (->* (procedure?)
                   (#:fill any/c)
                   #:rest (*list/c any/c list?)
                   normalized-noun?)]
  [make-ranked-procedure
   (-> rankable-procedure/c
       (->i ([arity exact-positive-integer?])
            [_ (arity) (and/c (listof procedure-rank/c)
                              (list-length-exactly/c arity))])
       ranked-procedure?)]
  [atomic-procedure->ranked-procedure (-> rankable-procedure/c
                                          ranked-procedure?)]
  [procedure-rank (-> procedure? exact-positive-integer?
                      (listof procedure-rank/c))]))

(require (for-syntax racket/base
                     syntax/parse)
         racket/list
         "frame.rkt"
         "noun.rkt")

(struct ranked-procedure
  (proc
   get-rank
   wrapper)
  #:property prop:procedure (struct-field-index wrapper))

(define current-fill (make-parameter (void)))

(define (apply/rank #:fill [fill (current-fill)] proc . args)
  (define real-proc
    (if (ranked-procedure? proc)
        (ranked-procedure-proc proc)
        proc))
  (define vs (map normalize-noun (apply list* args)))
  (define arity (length vs))
  (unless (procedure-arity-includes? real-proc arity)
    (apply raise-arity-error 'apply/rank (procedure-arity real-proc) vs))
  (parameterize ([current-fill fill])
    (if (null? vs)
        (normalize-noun (real-proc))
        (apply map/frame/fill real-proc (procedure-rank proc arity) fill vs))))

(define (make-ranked-procedure proc get-rank)
  (define p
    (ranked-procedure
     proc
     get-rank
     (procedure-reduce-arity
      (let ([wrapper (λ args (apply/rank p args))])
        (cond
          [(object-name proc) => (λ (name) (procedure-rename wrapper name))]
          [else wrapper]))
      (procedure-arity proc))))
  p)

(define (atomic-procedure->ranked-procedure proc)
  (make-ranked-procedure proc (λ (arity) (make-list arity 0))))

(define (procedure-rank proc arity)
  (unless (procedure-arity-includes? proc arity)
    (raise-argument-error
     'procedure-rank
     "arity accepted by procedure"
     arity))
  (if (ranked-procedure? proc)
      ((ranked-procedure-get-rank proc) arity)
      (make-list arity 0)))

(begin-for-syntax
  (define-syntax-class rank
    (pattern (~or _:exact-integer #f)))
  (define-syntax-class arg-decl
    (pattern id:identifier #:attr rank #'#f)
    (pattern [id:identifier rank:rank])))

(define-syntax (lambda/rank stx)
  (syntax-parse stx
    [(_ (arg:arg-decl ...) body:expr ...+)
     (with-syntax ([proc (syntax/loc stx (lambda (arg.id ...) body ...))]
                   [get-rank #'(λ (arity) (list arg.rank ...))])
       #'(make-ranked-procedure proc get-rank))]))

(define-syntax (case-lambda/rank stx)
  (syntax-parse stx
    [(_ [(arg:arg-decl ...) body:expr ...+] ...)
     (with-syntax ([proc (syntax/loc stx (case-lambda [(arg.id ...) body ...] ...))]
                   [(case-arity ...) (map length (syntax->datum #'((arg ...) ...)))])
       #'(make-ranked-procedure
          proc
          (let ([rank-map (make-immutable-hasheqv '((case-arity arg.rank ...) ...))])
            (λ (arity) (hash-ref rank-map arity)))))]))

(define-syntax (define/rank stx)
  (syntax-parse stx
    [(_ (id:identifier arg:arg-decl ...) body:expr ...+)
     (with-syntax ([proc (syntax/loc stx (lambda/rank (arg ...) body ...))])
       (syntax/loc stx (define id proc)))]))
