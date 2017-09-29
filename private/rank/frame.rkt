#lang racket/base

(provide
 map/frame/fill)

(require math/array
         racket/vector
         "noun.rkt")

(define (map/frame/fill proc proc-rank fill . vs)
  ; It is assumed that all the vs are normalized
  (define arg-ranks (map value-rank vs))
  (define frame-lengths (map frame-length arg-ranks proc-rank))
  (if (andmap zero? frame-lengths)
      (apply proc vs)
      (apply map/frame/fill/broadcast proc frame-lengths fill vs)))

(define (map/frame/fill/broadcast proc frame-lengths fill . vs)
  (define frame-shapes (map (λ (v l) (vector-take (value-shape v) l)) vs frame-lengths))
  (define max-shape
    (for/fold ([prefix #[]])
              ([shape (in-list (sort frame-shapes < #:key vector-length))])
      (unless (equal? prefix (vector-take shape (vector-length prefix)))
        (error "length error"))
      shape))
  (define frames
    (map (λ (f)
           (for/fold ([f f])
                     ([d (in-vector max-shape (array-dims f))])
             (array-axis-insert f (array-dims f) d)))
         (map make-frame vs frame-lengths)))
  (collapse-frame/fill (apply array-map proc frames) fill))

(define (collapse-frame/fill arr fill [cell-shape-hint #f])
  (define shape
    (or cell-shape-hint
        (array-all-fold (array-map value-shape arr)
                        cell-shape-broadcast
                        #[])))
  (collapse-frame (array-map (fill-cell shape fill) arr) shape))

(define (collapse-frame arr cell-shape)
  (cond
    [(zero? (vector-length cell-shape)) (normalize-value arr)]
    [else
     (define frame-length (array-dims arr))
     (build-array
      (vector-append (array-shape arr) cell-shape)
      (λ (js)
        (define-values (frame-js cell-js)
          (vector-split-at js frame-length))
        (unsafe-array-ref (unsafe-array-ref arr frame-js) cell-js)))]))

(define (frame-length v-rank cell-rank)
  (cond
    [(eqv? #f cell-rank) 0]
    [(negative? cell-rank) (min (- cell-rank) v-rank)]
    [else (max (- v-rank cell-rank) 0)]))

(define (make-frame v len)
  (cond
    [(zero? len) (if (atom? v) (->array v) (array v))]
    [(= len (array-dims v)) v]
    [else (build-array (vector-take (array-shape v) len)
                       (λ (js)
                         ; Without types, array-slice-ref assumes integers are sequences :(
                         (for/fold ([cell v])
                                   ([j (in-vector js)])
                           (array-axis-ref cell 0 j))))]))

(define (cell-shape-broadcast s1 s2)
  (define d1 (vector-length s1))
  (define d2 (vector-length s2))
  (cond
    [(equal? s1 s2) s1]
    [(= d1 d2) (vector-map max s1 s2)]
    [(< d1 d2) (vector-map max (vector-append (make-vector (- d2 d1) 1) s1) s2)]
    [else (vector-map max (vector-append (make-vector (- d1 d2) 1) s2) s1)]))

(define ((fill-cell shape fill) c)
  (define out-rank (vector-length shape))
  (define cell-shape (value-shape c))
  (define cell-rank (vector-length cell-shape))
  (cond
    [(equal? shape cell-shape) (normalize-value c)]
    [(zero? out-rank) (array-ref (->array c) (make-vector cell-rank 0))]
    [else
     (define projected-shape
       (vector-append (make-vector (max (- out-rank cell-rank) 0) 1)
                      (vector-take-right cell-shape (min cell-rank out-rank))))
     (build-array shape
                  (let ([c (->array c)])
                    (λ (js)
                      (if (for/and ([j (in-vector js)]
                                    [d (in-vector projected-shape)])
                            (< j d))
                          (unsafe-array-ref c (vector-take-right js cell-rank))
                          fill))))]))

(module+ test
  (require rackunit)
  (define i23 (index-array #[2 3]))
  (check-equal? (make-frame i23 0) (array i23))
  (check-equal? (make-frame i23 1) (array #[(array #[0 1 2]) (array #[3 4 5])]))
  (check-equal? (make-frame i23 2) i23)
  (check-equal? (make-frame (array (array 5)) 0) (array (array 5)))
  (check-equal? (make-frame 5 0) (array 5))
  (check-equal? (collapse-frame/fill (array #[1 'two (array 3)]) 0)
                (array #[1 'two 3]))
  (check-equal? (collapse-frame/fill (array (array 5)) 0) 5)
  (check-equal? (collapse-frame/fill (array (array (array 5))) 0) (array (array 5)))
  (check-equal? (collapse-frame/fill (array #[(array 5)]) 0) (array #[5]))
  (check-equal? (collapse-frame/fill (array #[(array (array 5))]) 0) (array #[(array (array 5))]))
  (check-equal? (collapse-frame/fill (array #[1 '(1 2) (array #[1 2 3])]) 0 #[2])
                (array #[#[1 0] #[1 2] #[1 2]]))
  (check-equal? (collapse-frame/fill (array #[1 '(1 2) (array #[1 2 3])]) 0)
                (array #[#[1 0 0] #[1 2 0] #[1 2 3]]))
  (check-equal? (collapse-frame/fill (array #[(array #[1 2]) (array #[#[1 2] #[3 4]])]) 0)
                (array #[#[#[1 2] #[0 0]] #[#[1 2] #[3 4]]])))
