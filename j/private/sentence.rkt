#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx)
         math/array
         racket/match
         "../../frame/main.rkt"
         "adverb.rkt"
         "coupla.rkt"
         "name.rkt"
         "noun.rkt"
         "verb.rkt")

(provide sentence)

(define-syntax (sentence stx)
  (syntax-case stx ()
    [(_ word ...)
     (build-sentence
      ;; Should list collapsing be done in the parser?
      (let loop ([stx (stx-cdr stx)])
        (syntax-parse stx
          [(n:number ns:number ...+ rest ...)
           (cons #'#[n ns ...] (loop #'(rest ...)))]
          [(v rest ...)
           (cons #'v (loop #'(rest ...)))]
          [() '()])))]))

(define-for-syntax (build-sentence words)
  (with-syntax ([(word ...) (reverse words)])
    #'(let* ([stack '()]
             [stack (run-stack (push word stack))]
             ...)
        (stack-result (run-stack (cons 'mark stack))))))

(define (push v stack)
  (cons
   (cond
     [(locative? v)
      (if (and (not (null? stack))
               (equal? is/global (car stack)))
          v
          (locale-ref v))]
     [(procedure? v) v]
     [else (->noun v)])
   stack))

(define (make-fork f g h)
  (verb
   (λ (y)
     (g (f y) (h y)))
   (λ (x y)
     (g (f x y) (h x y)))))

(define (run-stack stack)
  (define (asgn? v) (equal? is/global v))
  (define (avn? v) (or (adverb? v) (verb? v) (noun? v)))
  (define (name+noun? v) (or (locative? v) (noun? v)))
  (define (verb+noun? v) (or (verb? v) (noun? v)))
  (define (edge+avn? v) (or (edge? v) (avn? v)))
  (define (cavn? v) (or #|C|# (adverb? v) (verb? v) (noun? v)))
  (define (edge? v) (or (eq? 'mark v) (asgn? v)))
  ;(printf "run-stack: ~a~n" stack)
  (let/ec return
    (run-stack
     (match stack
       [(list-rest (? edge? a) (? verb? b) (? noun? c) rest)
        ;(displayln "0 monad")
        (unless (verb-monad b)
          (error "verb is not monadic"))
        (list* a (b c) rest)]
       [(list-rest (? edge+avn? a) (? verb? b) (? verb? c) (? noun? d) rest)
        ;(displayln "1 monad")
        (unless (verb-monad c)
          (error "verb is not monadic"))
        (list* a b (c d) rest)]
       [(list-rest (? edge+avn? a) (? noun? b) (? verb? c) (? noun? d) rest)
        ;(displayln "2 dyad")
        (unless (verb-dyad c)
          (error "verb is not dyadic"))
        (list* a (c b d) rest)]
       [(list-rest (? edge+avn? a) (? verb+noun? b) (? adverb? c) rest)
        ;(displayln "3 adverb")
        (list* a (c b) rest)]
       ;; conj
       [(list-rest (? edge+avn? a) (? verb+noun? b) (? verb? c) (? verb? d) rest)
        ;(displayln "5 fork")
        (list* a (make-fork b c d) rest)]
       ;; bident
       [(list-rest (? name+noun? a) (? asgn? b) (? cavn? c) rest)
        ;(displayln "7 is")
        (list* (b a c) rest)]
       [_ (return stack)]))))

(define (stack-result stack)
  (match stack
    [(list 'mark v) v]
    [_ (error "unprocessed stack:" stack)]))