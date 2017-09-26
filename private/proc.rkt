#lang racket/base

(provide make-j-procedure
         combine-monad+dyad)

(require "../customize.rkt"
         "../obverse.rkt"
         "../rank.rkt")

(struct j-procedure
  (proc obverse)
  #:property prop:procedure (struct-field-index proc)
  #:property prop:rank (λ (v arity) (procedure-rank (j-procedure-proc v) arity))
  #:property prop:obverse
  (λ (v)
    (j-procedure
     (cond
       [(j-procedure-obverse v)]
       [else (error "not invertible:" v)])
     (j-procedure-proc v)))
  #:property prop:customize
  (λ (v param)
    (define proc (j-procedure-proc v))
    (if (customizable? proc)
        (struct-copy j-procedure proc
                     [proc (customize proc param)])
        (error "not customizable:" proc))))

(define (make-j-procedure proc #:obverse [obverse #f])
  (j-procedure proc obverse))

(define (combine-monad+dyad m d)
  (define (proc+obverse v)
    (if (j-procedure? v)
        (values (j-procedure-proc v) (j-procedure-obverse v))
        (values v #f)))
  (define-values (m-proc m-obv) (proc+obverse m))
  (define-values (d-proc d-obv) (proc+obverse d))
  (define proc
    (case-lambda [(y) (m-proc y)] [(x y) (d-proc x y)]))
  (make-j-procedure
   (if (or (customizable? m-proc) (customizable? d-proc))
       (customizable-procedure
        (λ (param)
          (combine-monad+dyad
           (if (customizable? m-proc) (customize m-proc param) m-proc)
           (if (customizable? d-proc) (customize d-proc param) d-proc)))
        proc)
       proc)
   #:obverse
   (cond
     [(not m-obv) d-obv]
     [(not d-obv) m-obv]
     [else (case-lambda [(y) (m-obv y)] [(x y) (d-obv x y)])])))
