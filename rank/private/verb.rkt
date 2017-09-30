#lang racket/base

(require racket/contract/base)

(define argument-rank/c (or/c exact-integer? #f))

(define procedure-rank/c (listof argument-rank/c))

(define procedure-rank-spec/c
  (or/c (listof procedure-rank/c)
        (-> exact-positive-integer? procedure-rank/c)))

(define rankable-procedure/c
  (and/c procedure?
         (flat-named-contract
          'procedure-without-required-keywords?
          (λ (v)
            (let-values ([(kws opt-kws) (procedure-keywords v)])
              (null? kws))))
         (unconstrained-domain-> any/c)))

(provide
 argument-rank/c
 procedure-rank/c
 rankable-procedure/c
 ranked-procedure?
 current-fill
 lambda/rank
 case-lambda/rank
 define/rank
 (contract-out
  [prop:rank (struct-type-property/c
              (-> any/c exact-positive-integer? procedure-rank/c))]
  [apply/rank (->* (procedure?)
                   (#:fill any/c)
                   #:rest (*list/c any/c list?)
                   normalized-value?)]
  [make-ranked-procedure (-> rankable-procedure/c
                             procedure-rank-spec/c
                             ranked-procedure?)]
  [atomic-procedure->ranked-procedure (-> rankable-procedure/c
                                          ranked-procedure?)]
  [procedure-rank (-> procedure?
                      exact-positive-integer?
                      procedure-rank/c)]))

(require (for-syntax racket/base
                     syntax/parse)
         racket/list
         "frame.rkt"
         "noun.rkt")

(define-values (prop:rank has-rank? get-rank)
  (make-struct-type-property 'rank))

(define (ranked-procedure? v)
  (and (procedure? v) (has-rank? v)))

(struct ranked-procedure-wrapper
  (proc
   get-rank
   invoke)
  #:property prop:procedure (struct-field-index invoke)
  #:property prop:rank (λ (p arity) ((ranked-procedure-wrapper-get-rank p) arity)))

(define current-fill (make-parameter #f))

(define (apply/rank #:fill [fill (current-fill)] proc . args)
  (define real-proc
    (if (ranked-procedure-wrapper? proc)
        (ranked-procedure-wrapper-proc proc)
        proc))
  (define vs (map normalize-value (apply list* args)))
  (define arity (length vs))
  (unless (procedure-arity-includes? real-proc arity)
    (apply raise-arity-error 'apply/rank (procedure-arity real-proc) vs))
  (parameterize ([current-fill fill])
    (if (null? vs)
        (normalize-value (real-proc))
        (apply map/frame/fill real-proc (procedure-rank proc arity) fill vs))))

(define (make-ranked-procedure proc rank-spec)
  (define p
    (ranked-procedure-wrapper
     proc
     (cond
       [(procedure? rank-spec) rank-spec]
       [(eqv? 1 (length rank-spec)) (λ (arity) (car rank-spec))]
       [else (λ (arity) (or (findf (λ (r) (eqv? arity (length r))) rank-spec)
                            (error "no rank found for arity:" arity)))])
     (procedure-reduce-arity
      (λ args (apply/rank p args))
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
  (if (has-rank? proc)
      (let ([proc-rank ((get-rank proc) proc arity)])
        (unless (eqv? arity (length proc-rank))
          (error (format "expected rank of length ~a;\n  given: ~a\n  in: ~a\n"
                         arity (length proc-rank) proc)))
        proc-rank)
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
     (with-syntax ([proc (syntax/loc stx (lambda (arg.id ...) body ...))])
       #'(make-ranked-procedure proc '((arg.rank ...))))]))

(define-syntax (case-lambda/rank stx)
  (syntax-parse stx
    [(_ [(arg:arg-decl ...) body:expr ...+] ...)
     (with-syntax ([proc (syntax/loc stx (case-lambda [(arg.id ...) body ...] ...))])
       #'(make-ranked-procedure proc '((arg.rank ...) ...)))]))

(define-syntax (define/rank stx)
  (syntax-parse stx
    [(_ (id:identifier arg:arg-decl ...) body:expr ...+)
     (with-syntax ([proc (syntax/loc stx (lambda/rank (arg ...) body ...))])
       (syntax/loc stx (define id proc)))]))
