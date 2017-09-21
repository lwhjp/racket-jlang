#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [customizable-procedure? predicate/c]
  [customize (-> customizable-procedure? any/c procedure?)]
  [make-customizable-procedure (-> procedure? any/c customizable-procedure?)])
 define-customizable/cond)

(require (for-syntax racket/base
                     syntax/parse))

(struct customizable-procedure
  (proc
   default)
  #:property prop:procedure (struct-field-index default))

(define (customize proc v)
  ((customizable-procedure-proc proc) v))

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
