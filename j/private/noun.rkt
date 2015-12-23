#lang racket/base

(require math/array
         racket/contract/base
         racket/sequence
         racket/vector
         "../../frame/main.rkt")

(define (type? v)
  (if (memv v '(number char symbol box)) #t #f))

(define (atom? v)
  (or (number? v)
      (char? v)
      (symbol? v)
      (box? v)))

(struct noun (type value) #:transparent)

(provide
 (contract-out
  [type? predicate/c]
  [atom? predicate/c]
  (struct noun ([type type?] [value array?]))
  [->noun (-> any/c noun?)]
  [noun-empty? (-> noun? boolean?)]
  [noun-item-shape (-> noun? (vectorof exact-nonnegative-integer?))]
  [noun-shape (-> noun? (vectorof exact-nonnegative-integer?))]
  [noun-rank (-> noun? exact-nonnegative-integer?)]
  [noun->racket-value (-> noun? any/c)]
  [in-noun-items (-> noun? sequence?)]
  [noun->items (-> noun? (listof noun?))]
  [array-empty? (-> array? boolean?)]
  [array-first (-> (and/c array? (not/c array-empty?)) any)]
  [array-type (-> (and/c array? (not/c array-empty?)) type?)]
  [array->noun (->* (array?) ((or/c type? #f)) noun?)]
  [atom-type (-> atom? type?)]))

(define (->noun v)
  (cond
    [(noun? v) v]
    [(string? v) (noun 'char (->array v))]
    [else (array->noun (->array v))]))

(define (noun-empty? n)
  (array-empty? (noun-value n)))

(define (noun-item-shape n)
  (if (zero? (noun-rank n))
      '#[]
      (vector-drop (noun-shape n) 1)))

(define (noun-shape n)
  (array-shape (noun-value n)))

(define (noun-rank n)
  (array-dims (noun-value n)))

(define (noun->racket-value n)
  (define t (noun-type n))
  (define v (noun-value n))
  (define r (rank v))
  (cond
    [(zero? r)
     (if (eq? 'box t)
         (noun->racket-value (unbox (array-ref v '#[])))
         (array-ref v '#[]))]
    [(and (= 1 r) (eq? 'char t))
     (list->string
      (vector->list
       (array->vector v)))]
    [else
     (if (eq? 'box t)
         (array-map
          (λ (b)
            (noun->racket-value (unbox b)))
          v)
         v)]))

(define (in-noun-items n)
  (if (zero? (noun-rank n))
      (in-value n)
      (sequence-map
       (λ (item)
         (noun (noun-type n) item))
       (in-array-axis (noun-value n) 0))))

(define (noun->items n)
  (sequence->list (in-noun-items n)))

(define (array-empty? arr)
  (for/or ([d (in-vector (array-shape arr))])
    (zero? d)))

(define (array-first arr)
  (when (array-empty? arr)
    (error "empty array has no first item"))
  (array-ref arr (make-vector (array-dims arr) 0)))

(define (array-type arr)
  (atom-type (array-first arr)))

(define (array->noun arr [type #f])
  (when (and type (not (array-empty? arr)))
    (define arr-type (array-type arr))
    (unless (eq? arr-type type)
      (error "incorrect type given for array")))
  (noun (or type (array-type arr)) arr))

(define (atom-type a)
  (cond
    [(number? a) 'number]
    [(char? a) 'char]
    [(symbol? a) 'symbol]
    [(box? a) 'box]
    [else (error "not an atom:" a)]))