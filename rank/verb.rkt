#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         math/array
         racket/list
         "frame.rkt"
         "noun.rkt")

; TODO: multiple arities
(struct RankedProcedure
  (proc
   rank)
  #:property prop:procedure
  (λ (p . args)
    (normalize-noun (apply/rank p args))))

(define-syntax ranked-procedure? (make-rename-transformer #'RankedProcedure?))

(define-syntax procedure-rank (make-rename-transformer #'RankedProcedure-rank))

(define (make-ranked-procedure proc rank)
  (unless (procedure? proc)
    (error 'make-ranked-procedure "not a procedure"))
  (unless (procedure-arity-includes? proc (length rank))
    (error 'make-ranked-procedure "procedure does not have arity: ~a" (length rank)))
  (RankedProcedure proc rank))

(define (atomic-procedure->ranked-procedure proc [arity-hint #f])
  (define arity (or arity-hint (procedure-arity proc)))
  (unless (exact-nonnegative-integer? arity)
    (error 'atomic-procedure->ranked-procedure "procedure must have fixed non-zero arity"))
  (make-ranked-procedure proc (make-list arity 0)))

(define current-fill (make-parameter (void)))

(define (apply/rank #:fill [fill (current-fill)] proc . args)
  (when (null? args)
    (error 'apply/rank "expected argument list"))
  (unless (list? (last args))
    (error 'apply/rank "last argument to apply/rank must be a list"))
  (*apply/rank fill proc (apply list* args)))

(define (*apply/rank fill proc args)
  (parameterize ([current-fill fill])
    (apply map/frame/fill
           fill
           (RankedProcedure-proc proc)
           (RankedProcedure-rank proc)
           args)))

(begin-for-syntax
  (define-syntax-class arg-decl
    (pattern id:identifier #:attr rank #'#f)
    (pattern [id:identifier rank:expr])))

(define-syntax lambda/rank
  (syntax-parser
    [(_ (arg:arg-decl ...) body:expr ...+)
     #'(make-ranked-procedure
        (λ (arg.id ...) body ...)
        (list arg.rank ...))]))

(define-syntax define/rank
  (syntax-parser
    [(_ (id:identifier arg:arg-decl ...) body:expr ...+)
     #'(define id (lambda/rank (arg ...) body ...))]))

(provide
 ranked-procedure?
 procedure-rank
 make-ranked-procedure
 atomic-procedure->ranked-procedure
 current-fill
 apply/rank
 lambda/rank
 define/rank)