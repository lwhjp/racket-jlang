#lang racket/base

(provide
 verb? verb:cap?
 ->verb
 make-verb
 make-primitive-verb
 make-cap
 make-compound-verb
 combine-monad+dyad combine-monad+dyad/verb)

(require "../customize.rkt"
         "../obverse.rkt"
         "../rank.rkt"
         "type.rkt")

(struct verb (proc obverse)
  #:property prop:procedure
  (λ (v . args)
    (apply/rank (verb-proc v) args #:fill fill-for-frame))
  #:property prop:rank (λ (v arity) (procedure-rank (verb-proc v) arity))
  #:property prop:obverse
  (λ (v)
    (verb
     (cond
       [(verb-obverse v)]
       [else (error "not invertible:" v)])
     (verb-proc v)))
  #:property prop:customize
  (λ (v param)
    (define proc (verb-proc v))
    (if (customizable? proc)
        (struct-copy verb v
                     [proc (customize proc param)])
        (error "not customizable:" v))))

(struct verb:primitive verb (name))

(struct verb:cap verb:primitive ())

(struct verb:compound verb ())

(define (->verb v)
  (if (verb? v)
      v
      (verb v #f)))

(define (make-verb proc #:obverse [obverse #f])
  (verb proc obverse))

(define make-primitive-verb
  (case-lambda
    [(name proc)
     (let ([v (->verb proc)])
       (verb:primitive (verb-proc v) (verb-obverse v) name))]
    [(name monad dyad)
     (let ([v (combine-monad+dyad/verb monad dyad)])
       (verb:primitive (verb-proc v) (verb-obverse v) name))]))

(define (make-cap name)
  (verb:cap (case-lambda) #f name))

(define (make-compound-verb v)
  (let ([v (->verb v)])
    (verb:compound (verb-proc v) (verb-obverse v))))

(define (combine-monad+dyad m d)
  (make-ranked-procedure
   (case-lambda [(y) (m y)] [(x y) (d x y)])
   (list (procedure-rank m 1) (procedure-rank d 2))))

(define (combine-monad+dyad/verb m d)
  (define (proc+obverse v)
    (if (verb? v)
        (values (verb-proc v) (verb-obverse v))
        (values v #f)))
  (define-values (m-proc m-obv) (proc+obverse m))
  (define-values (d-proc d-obv) (proc+obverse d))
  (define proc (combine-monad+dyad m-proc d-proc))
  (verb
   (if (or (customizable? m-proc) (customizable? d-proc))
       (customizable-procedure
        (λ (param)
          (combine-monad+dyad
           (if (customizable? m-proc) (customize m-proc param) m-proc)
           (if (customizable? d-proc) (customize d-proc param) d-proc)))
        proc)
       proc)
   (cond
     [(not m-obv) d-obv]
     [(not d-obv) m-obv]
     [else (combine-monad+dyad m-obv d-obv)])))
