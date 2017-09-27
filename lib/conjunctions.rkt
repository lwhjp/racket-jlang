#lang racket/base

(require (for-syntax racket/base)
         math/array
         racket/list
         racket/match
         racket/port
         racket/provide
         racket/sequence
         racket/string
         "../customize.rkt"
         "../obverse.rkt"
         "../rank.rkt"
         "../private/executor.rkt"
         "../private/proc.rkt")

(define (j:power u n)
  ; TODO: boxed, gerund, infinite
  (cond
    [(negative? n) (j:power (obverse u) (- n))]
    [else (make-j-procedure
           (case-lambda/rank
            [(y) (for/fold ([y y]) ([i (in-range n)]) (u y))]
            [(x y) (for/fold ([y y]) ([i (in-range n)]) (u x y))]))]))

;determinant
;dot-product
;even
;odd

(define (j:explicit m n)
  (define-values (noun-def str)
    (let ([n (normalize-noun n)])
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
        [(and (eqv? 1 (noun-rank n)) (array-andmap box? n))
         (define lines
           (map (λ (e)
                  (define str (unbox e))
                  (unless (and (array? str) (eqv? 1 (array-dims str)) (array-andmap char? str))
                    (error "invalid argument:" n))
                  (list->string (array->list str)))
                (array->list n)))
         (values n (string-join lines "\n"))]
      [(and (eqv? 2 (noun-rank n)) (array-andmap char? n))
       (values n (map list->string (array->list-array n)))]
      [else (error "invalid argument:" n)])))
  ; TODO: need to annotate adverb etc.
  ; TODO: control statements
  (define (eval str)
    (with-input-from-string str (current-j-executor)))
  (let ([m (normalize-noun m)])
    (cond
      [(= 0 m) noun-def]
      [(= 1 m) (eval str)]
      [(= 2 m) (eval str)]
      [(= 3 m) (let-values ([(monad-def dyad-def)
                             (apply
                              (λ (m [d #f] . rest)
                                (unless (null? rest) (error "invalid definition"))
                                (values m d))
                              (string-split str #px"(?m:^\\s*:\\s*$)"))])
                 (if dyad-def
                     (j:monad+dyad (eval monad-def) (eval dyad-def))
                     (let ([m (eval monad-def)])
                       (lambda/rank (y) (m y)))))]
      [(= 4 m) (let ([d (eval str)])
                 (lambda/rank (x y) (d x y)))]
      [(= 13 m) (error 'TODO)]
      [else (error "invalid argument:" m)])))

(define (j:monad+dyad u v)
  (make-j-procedure
   (make-ranked-procedure
    (case-lambda [(y) (u y)] [(x y) (v x y)])
    (list (procedure-rank u 1) (procedure-rank v 2)))))

;obverse

(define (j:adverse u v #:pred [pred exn:fail?])
  (make-j-procedure
   (make-ranked-procedure
    (λ args (with-handlers ([pred (apply v args)]) (apply u args)))
    (λ (arity) (make-list arity #f)))))

;cut

(define j:fit customize)

;foreign

(define (j:rank m n)
  (define (->rank v)
    (cond
      [(integer? v) (inexact->exact v)]
      [(eqv? +inf.0) #f]
      [(eqv? -inf.0) 0]
      [else (error "invalid rank:" v)]))
  (unless (and (<= (noun-rank n) 1)
               (<= 1 (noun-tally n) 3))
    (error "invalid rank specifier:" n))
  (define rank
    (match (map ->rank (sequence->list (in-items n)))
      [(list r) `((,r) (,r ,r))]
      [(list l r) `((,r) (,l ,r))]
      [(list m l r) `((,m) (,l ,r))]))
  (make-j-procedure
   (make-ranked-procedure
    (if (procedure? m)
        (case-lambda [(y) (m y)] [(x y) (m x y)])
        (case-lambda [(y) m] [(x y) m]))
    rank)))

;tie
;evoke-gerund

(define (j:atop u v)
  (make-j-procedure
   (make-ranked-procedure
    (compose1 u v)
    (λ (arity) (procedure-rank v arity)))))

;agenda

(define (j:at u v)
  (make-j-procedure
   (make-ranked-procedure
    (compose1 u v)
    (λ (arity) (make-list arity #f)))))

(define (j:bond x y)
  (define proc
    (match* (x y)
      [(m (? procedure? v)) (λ (y) (v m y))]
      [((? procedure? u) n) (λ (y) (u y n))]))
  (make-j-procedure
   (case-lambda/rank
    [(y) (proc y)]
    [([x 0] y) (j:power proc x) y])))

(define (j:compose u v)
  (define mv (procedure-rank v 1))
  (make-j-procedure
   (make-ranked-procedure
    (procedure-reduce-arity
     (lambda args
       (apply u (map v args)))
     (procedure-arity u))
    (λ (arity) (make-list arity mv)))))

;under

(define (j:appose u v)
  (make-j-procedure
   (make-ranked-procedure
    (procedure-reduce-arity
     (lambda args
       (apply u (map v args)))
     (procedure-arity u))
    (λ (arity) (make-list arity #f)))))

;derivative
;secant-slope
;hypergeometric
;level-at
;spread
;taylor-approximation

(provide (filtered-out
          (λ (name)
            (and (regexp-match? #rx"^j:." name)
                 (substring name 2)))
          (all-defined-out)))
