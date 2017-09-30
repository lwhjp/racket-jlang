#lang racket/base

(provide (all-defined-out))

(require math/array
         racket/list
         racket/match
         racket/port
         racket/sequence
         racket/string
         "../../customize.rkt"
         "../../obverse.rkt"
         "../../rank.rkt"
         "../executor.rkt"
         "../locale.rkt"
         "../word.rkt")

; TODO: maintain obverse if appropriate

(define (jc:power u n)
  ; TODO: boxed, gerund, infinite, verb
  (cond
    [(negative? n) (jc:power (obverse u) (- n))]
    [else (case-lambda/rank
           [(y) (for/fold ([y y]) ([i (in-range n)]) (u y))]
           [(x y) (for/fold ([y y]) ([i (in-range n)]) (u x y))])]))

;determinant
;dot-product
;even
;odd

(define (jc:explicit m n)
  (define-values (noun-def str)
    (let ([n (normalize-value n)])
      (cond
        [(zero? n)
         (define str
           (string-join
            (sequence->list (in-producer read-line (λ (str) (regexp-match? #rx"^\\)" str))))
            "\n"))
         (values str str)]
        [(char? n)
         (values n (list->string (list n)))]
        [(and (eqv? 1 (array-dims n)) (array-andmap char? n))
         (values n (list->string (array->list n)))]
        [(and (eqv? 1 (rank n)) (array-andmap box? n))
         (define lines
           (map (λ (e)
                  (define str (unbox e))
                  (unless (and (array? str) (eqv? 1 (array-dims str)) (array-andmap char? str))
                    (error "invalid argument:" n))
                  (list->string (array->list str)))
                (array->list n)))
         (values n (string-join lines "\n"))]
        [(and (eqv? 2 (rank n)) (array-andmap char? n))
         (values n (map list->string (array->list-array n)))]
        [else (error "invalid argument:" n)])))
  ; TODO: control statements
  (define (eval str)
    (with-new-j-private-vars
        (λ ()
          (with-input-from-string str (current-j-executor)))))
  (let ([m (normalize-value m)])
    (cond
      [(= 0 m) noun-def]
      [(= 1 m) (make-compound-adverb (eval str))]
      [(= 2 m) (make-compound-conjunction (eval str))]
      [(= 3 m) (let-values ([(monad-def dyad-def)
                             (apply
                              (λ (m [d #f] . rest)
                                (unless (null? rest) (error "invalid definition"))
                                (values m d))
                              (string-split str #px"(?m:^\\s*:\\s*$)"))])
                 (make-compound-verb
                  (if dyad-def
                      (combine-monad+dyad/verb (eval monad-def) (eval dyad-def))
                      (let ([m (eval monad-def)])
                        (lambda/rank (y) (m y))))))]
      [(= 4 m) (let ([d (eval str)])
                 (make-compound-verb
                  (lambda/rank (x y) (d x y))))]
      [(= 13 m) (error 'TODO)]
      [else (error "invalid argument:" m)])))

(define (jc:monad-dyad u v)
  (combine-monad+dyad/verb u v))

(define (jc:obverse u v)
  (make-compound-verb
   (make-verb u #:obverse v)))

(define (jc:adverse u v #:pred [pred exn:fail?])
  (make-ranked-procedure
   (λ args (with-handlers ([pred (apply v args)]) (apply u args)))
   (λ (arity) (make-list arity #f))))

;cut

(define jc:fit customize)

;foreign

(define (jc:rank m n)
  (define (->rank v)
    (cond
      [(integer? v) (inexact->exact v)]
      [(eqv? +inf.0) #f]
      [(eqv? -inf.0) 0]
      [else (error "invalid rank:" v)]))
  (unless (and (<= (rank n) 1)
               (<= 1 (item-count n) 3))
    (error "invalid rank specifier:" n))
  (define rank
    (match (map ->rank (sequence->list (in-items n)))
      [(list r) `((,r) (,r ,r))]
      [(list l r) `((,r) (,l ,r))]
      [(list m l r) `((,m) (,l ,r))]))
  (make-ranked-procedure
   (if (procedure? m)
       (case-lambda [(y) (m y)] [(x y) (m x y)])
       (case-lambda [(y) m] [(x y) m]))
   rank))

;tie
;evoke-gerund

(define (jc:atop u v)
  (make-ranked-procedure
   (compose1 u v)
   (λ (arity) (procedure-rank v arity))))

;agenda

(define (jc:at u v)
  (make-ranked-procedure
   (compose1 u v)
   (λ (arity) (make-list arity #f))))

(define (jc:bond x y)
  (define proc
    (match* (x y)
      [(m (? procedure? v)) (λ (y) (v m y))]
      [((? procedure? u) n) (λ (y) (u y n))]))
  (make-verb
   (case-lambda/rank
    [(y) (proc y)]
    [([x 0] y) (jc:power proc x) y])))

(define (jc:compose u v)
  (define mv (procedure-rank v 1))
  (make-ranked-procedure
   (procedure-reduce-arity
    (lambda args
      (apply u (map v args)))
    (procedure-arity u))
   (λ (arity) (make-list arity mv))))

;under

(define (jc:appose u v)
  (make-ranked-procedure
   (procedure-reduce-arity
    (lambda args
      (apply u (map v args)))
    (procedure-arity u))
   (λ (arity) (make-list arity #f))))

;derivative
;secant-slope
;hypergeometric
;level-at
;spread
;taylor-approximation
