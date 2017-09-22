#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [customizable? predicate/c]
  [customize (-> customizable? any/c any/c)]
  [make-customizable-procedure (-> procedure? any/c customizable?)])
 define-customizable/cond
 prop:customize)

(require (for-syntax racket/base
                     syntax/parse))

(define-values (prop:customize customizable? get-customize)
  (make-struct-type-property
   'customize
   (λ (v si)
     (cond
       [(exact-nonnegative-integer? v) (λ (c param) (((list-ref si 3) c v) param))]
       [(procedure? v) v]
       [else (error 'prop:customize "expected procedure or struct field index")]))))

(struct customizable-procedure
  (customize
   default)
  #:property prop:customize (struct-field-index customize)
  #:property prop:procedure (struct-field-index default))

(define (customize c v)
  ((get-customize c) c v))

(define (make-customizable-procedure proc default)
  (customizable-procedure
   proc
   (if (procedure? default)
       (λ args (apply (proc (default)) args))
       (proc default))))

(define-syntax (define-customizable/cond stx)
  (syntax-parse stx #:literals (else)
    [(_ (id:id param:id)
        default:expr
        [test-expr:expr then-expr:expr] ...
        (~optional [else else-expr:expr] #:defaults ([else-expr #f])))
     #`(define id
         (make-customizable-procedure
          (lambda (param)
            (cond
              [test-expr then-expr] ...
              [else #,(or (attribute else-expr)
                          #'(error 'id "invalid customization\n  given: ~a" param))]))
          default))]))
