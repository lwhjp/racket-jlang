#lang racket/base

(provide
 sentence)

(require racket/match
         "coupla.rkt"
         "locale.rkt"
         "word.rkt")

(define mark (string->uninterned-symbol "§"))

(define (mark? v) (eq? mark v))

(define (adv? v) (adverb? v))
(define (asgn? v) (coupla? v))
(define (avn? v) (or (adv? v) (verb? v) (noun? v)))
(define (cavn? v) (or (conj? v) (avn? v)))
(define (conj? v) (conjunction? v))
(define (edge? v) (or (mark? v) (asgn? v)))
(define (edge+avn? v) (or (edge? v) (avn? v)))
(define (name+noun? v) (or (name? v) (noun? v)))
(define (verb+noun? v) (or (verb? v) (noun? v)))

(define (sentence . words)
  (when (null? words)
    (error 'sentence "expected one or more words"))
  (let loop ([queue (reverse (cons mark words))]
             [stack '()])
    (match stack
      [(list-rest (? edge? a)      (? verb? b)      (? noun? c)                  rest) (loop queue (list* a (b c) rest))] ; 0 Monad
      [(list-rest (? edge+avn? a)  (? verb? b)      (? verb? c) (? noun? d)      rest) (loop queue (list* a b (c d) rest))] ; 1 Monad
      [(list-rest (? edge+avn? a)  (? noun? b)      (? verb? c) (? noun? d)      rest) (loop queue (list* a (c b d) rest))] ; 2 Dyad
      [(list-rest (? edge+avn? a)  (? verb+noun? b) (? adv? c)                   rest) (loop queue (list* a (c b) rest))] ; 3 Adverb
      [(list-rest (? edge+avn? a)  (? verb+noun? b) (? conj? c) (? verb+noun? d) rest) (loop queue (list* a (c b d) rest))] ; 4 Conj
      [(list-rest (? edge+avn? a)  (? verb+noun? b) (? verb? c) (? verb? d)      rest) (loop queue (list* a (make-fork b c d) rest))] ; 5 Fork
      [(list-rest (? edge? a)      (? cavn? b)      (? cavn? c)                  rest) (loop queue (list* a (make-train b c) rest))] ; 6 Bident
      [(list-rest (? name+noun? a) (? asgn? b)      (? cavn? c)                  rest) (loop queue (list* (b a c) rest))] ; 7 Is
      ; Parentheses are handled by Racket
      [(list (? mark?) v) v]
      [_ (cond
           [(null? queue) (error "evaluation failed")]
           [else (let ([v (car queue)])
                   (loop (cdr queue)
                         (cons (if (and (name? v) (or (null? stack) (not (asgn? (car stack)))))
                                   (name-ref v)
                                   v)
                               stack)))])])))
