#lang racket/base

(require math/array
         racket/vector
         "noun.rkt")

(define (make-frame v cell-rank)
  (define arr (->array v))
  (*make-frame arr (frame-length (array-dims arr) cell-rank)))

(define (frame-length v-rank cell-rank)
  (cond
    [(eqv? #f cell-rank) 0]
    [(negative? cell-rank) (min (- cell-rank) v-rank)]
    [else (max (- v-rank cell-rank) 0)]))

(define (*make-frame arr frame-length)
  (define frame-shape (vector-take (array-shape arr) frame-length))
  (cond
    [(zero? frame-length) arr]
    [(= (array-dims arr) frame-length) arr]
    [else (build-array (vector-take (array-shape arr) frame-length)
                       (λ (js) (array-slice-ref arr (append (vector->list js) (list ::...)))))]))

(define (collapse-frame/fill arr fill [cell-shape-hint #f])
  (define max-shape
    (or cell-shape-hint
        (array-all-fold (array-map noun-shape arr)
                        cell-shape-broadcast
                        #[])))
  (define max-rank (vector-length max-shape))
  (define (fill-cell c)
    (define cell-shape (noun-shape c))
    (define cell-rank (vector-length cell-shape))
    (cond
      [(equal? max-shape cell-shape) (unwrap-atom c)]
      [(zero? max-rank) (array-ref (->array c) (make-vector cell-rank 0))]
      [else
       (define projected-shape
         (vector-append (make-vector (max (- max-rank cell-rank) 0) 1)
                        (vector-take-right cell-shape (min cell-rank max-rank))))
       (build-array max-shape
                    (let ([c (->array c)])
                      (λ (js)
                        (if (for/and ([j (in-vector js)]
                                      [d (in-vector projected-shape)])
                              (< j d))
                            (unsafe-array-ref c (vector-take-right js cell-rank))
                            fill))))]))
  (unsafe-collapse-frame (array-map fill-cell arr) max-shape))

(define (collapse-frame arr cell-shape)
  (for ([c (in-array arr)])
    (unless (equal? cell-shape (noun-shape c))
      (error 'collapse-frame "cell shape mismatch")))
  (unsafe-collapse-frame arr cell-shape))

(define (unsafe-collapse-frame arr cell-shape)
  (cond
    [(zero? (vector-length cell-shape)) (unwrap-atom arr)]
    [else
     (define frame-length (array-dims arr))
     (build-array
      (vector-append (array-shape arr) cell-shape)
      (λ (js)
        (define-values (frame-js cell-js)
          (vector-split-at js frame-length))
        (unsafe-array-ref (unsafe-array-ref arr frame-js) cell-js)))]))

(define (cell-shape-broadcast s1 s2)
  (define d1 (vector-length s1))
  (define d2 (vector-length s2))
  (cond
    [(equal? s1 s2) s1]
    [(= d1 d2) (vector-map max s1 s2)]
    [(< d1 d2) (vector-map max (vector-append (make-vector (- d2 d1) 1) s1) s2)]
    [else (vector-map max (vector-append (make-vector (- d1 d2) 1) s2) s1)]))

(define (map/frame/fill fill proc ranks . vs)
  (define arity (length vs))
  (unless (procedure-arity-includes? proc arity)
    (error 'map/frame/fill "procedure does not have arity: ~a" arity))
  (unless (eqv? arity (length ranks))
    (error 'map/frame/fill "expected the same number of ranks as arguments: ~a" arity))
  (define args (map ->array vs))
  (define arg-lengths (map array-dims args))
  (define frame-lengths (map frame-length arg-lengths ranks))
  (cond
    [(zero? arity) (proc)]
    [(andmap zero? frame-lengths) (apply proc (map unwrap-atom args))]
    [else (apply map/frame/fill/broadcast fill proc frame-lengths args)]))

(define (map/frame/fill/broadcast fill proc frame-lengths . args)
  (define frame-shapes (map (λ (l a) (vector-take (array-shape a) l)) frame-lengths args))
  (define max-shape
    (let check-prefix ([prefix #[]]
                       [frames (sort frame-shapes < #:key vector-length)])
      (cond
        [(null? frames) prefix]
        [(equal? prefix (vector-take (car frames) (vector-length prefix)))
         (check-prefix (car frames) (cdr frames))]
        [else (error "frame shape mismatch")])))
  (define frames
    (map (λ (f)
           (for/fold ([f f])
                     ([d (in-vector max-shape (array-dims f))])
             (array-axis-insert f (array-dims f) d)))
         (map *make-frame args frame-lengths)))
  (collapse-frame/fill (apply array-map proc frames) fill))

(provide
 make-frame
 collapse-frame/fill
 collapse-frame
 map/frame/fill)