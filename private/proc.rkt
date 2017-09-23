#lang racket/base

(provide make-j-procedure
         combine-monad+dyad)

(require "../customize.rkt"
         "../rank.rkt")

(struct j-procedure
  (proc)
  #:property prop:procedure (struct-field-index proc)
  #:property prop:rank (λ (v arity) (procedure-rank (j-procedure-proc v) arity))
  #:property prop:customize
  (λ (v param)
    (define proc (j-procedure-proc v))
    (if (customizable? proc)
        (struct-copy j-procedure proc
                     [proc (customize proc param)])
        (error "not customizable:" proc))))

(define (make-j-procedure proc)
  (j-procedure proc))

(define (combine-monad+dyad m d)
  (define proc
    (case-lambda [(y) (m y)] [(x y) (d x y)]))
  (make-j-procedure
   (if (or (customizable? m) (customizable? d))
       (customizable-procedure
        (λ (param)
          (combine-monad+dyad
           (if (customizable? m) (customize m param) m)
           (if (customizable? d) (customize d param) d)))
        proc)
       proc)))
