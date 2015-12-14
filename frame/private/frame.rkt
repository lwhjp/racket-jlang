#lang racket/base

(require math/array
         racket/class
         racket/sequence
         racket/vector)

(provide (all-defined-out))

(struct frame
  (data
   length
   fill)
  #:transparent)

(define (frame-shape f)
  (vector-take
   (array-shape (frame-data f))
   (frame-length f)))

(define (frame-cell-shape f)
  (vector-take-right
   (array-shape (frame-data f))
   (- (array-dims (frame-data f))
      (frame-length f))))

(define (make-frame arr cell-rank fill)
  (frame
   arr
   (cond
     [(eq? 'any cell-rank) 0]
     [(negative? cell-rank)
      (min (- cell-rank)
           (array-dims arr))]
     [else
      (max (- (array-dims arr) cell-rank)
           0)])
   fill))

(module+ test
  (require rackunit)
  (let ([A (index-array '#(4 5 6))])
    (check-equal? (frame-shape (make-frame A 0 0))
                  '#(4 5 6))
    (check-equal? (frame-shape (make-frame A 1 0))
                  '#(4 5))
    (check-equal? (frame-shape (make-frame A 2 0))
                  '#(4))
    (check-equal? (frame-shape (make-frame A 3 0))
                  '#())
    (check-equal? (frame-shape (make-frame A 4 0))
                  '#())
    (check-equal? (frame-shape (make-frame A 'any 0))
                  '#())
    (check-equal? (frame-shape (make-frame A -1 0))
                  '#(4))
    (check-equal? (frame-shape (make-frame A -4 0))
                  '#(4 5 6))))

(define (frame-extend f ds)
  (define cell-shape (frame-cell-shape f))
  (define cell-rank (vector-length cell-shape))
  (if (equal? ds (frame-shape f))
      f
      (struct-copy
       frame
       f
       [data (array-transform
              (frame-data f)
              (vector-append ds cell-shape)
              (λ (indexes)
                (vector-append
                 (vector-take indexes (frame-length f))
                 (vector-take-right indexes cell-rank))))]
       [length (vector-length ds)])))

(module+ test
  (require rackunit)
  (let ([f (make-frame (array #[100 200]) 0 0)])
    (check-equal? (frame-extend f '#(2 3))
                  (frame (array #[#[100 100 100] #[200 200 200]])
                         2
                         0))))

(define (frame-fill-cell f)
  (define cs (frame-cell-shape f))
  (make-array (frame-cell-shape f)
              (frame-fill f)))

(define (frame-map proc f . fs)
  (define ds
    (frame-shape f))
  (unless (for/and ([f2 (in-list fs)])
            (equal? ds (frame-shape f2)))
    (error 'frame-map "shape mismatch"))
  (define arrs
    (map
     (λ (f)
       (define arr (frame-data f))
       (define cs (frame-cell-shape f))
       (if (zero? (frame-length f))
           (array arr)
           (build-array
            ds
            (λ (indexes)
              ;; array-slice-ref is broken in untyped code
              (array-transform
               arr
               cs
               (λ (cell-indexes)
                 (vector-append indexes cell-indexes)))))))
     (cons f fs)))
  (apply array-map proc arrs))

(define (cell-shape-broadcast dss)
  (define max-rank
    (foldl max 0 (map vector-length dss)))
  (apply
   vector-map
   max
   (map (λ (v)
          (define r (vector-length v))
          (if (= max-rank r)
              v
              (vector-append (make-vector (- max-rank r ) 0) v)))
        dss)))

(define (unframe/fill arr fill [maybe-cell-shape #f])
  (define cs
    (or maybe-cell-shape
        (array-all-fold
         (array-map array-shape arr)
         (λ shapes (cell-shape-broadcast shapes))
         '#[])))
  (define (shape-extend ds len)
    (vector-append (make-vector (- len (vector-length ds)) 1)
                   ds))
  (define (fill-cell cell)
    ;; It feels like there should be a better way of doing this
    (define orig-shape (array-shape cell))
    (if (equal? cs orig-shape)
        cell
        (let ([orig-rank (vector-length orig-shape)]
              [extended-shape (shape-extend orig-shape (vector-length cs))])
          (build-array
           cs
           (λ (indexes)
             (if (for/and ([i (in-vector indexes)]
                           [j (in-vector extended-shape)])
                   (< i j))
                 (array-ref cell (vector-take-right indexes orig-rank))
               fill))))))
  (define filled (array-map fill-cell arr))
  (define arr-rank (array-dims arr))
  (build-array
   (vector-append (array-shape arr) cs)
   (λ (indexes)
     (define-values (frame-index cell-index)
       (vector-split-at indexes arr-rank))
     (array-ref
      (array-ref filled frame-index)
      cell-index))))

(module+ test
  (check-equal? (unframe/fill (array #[(array #[1]) (array #[2 3])])
                              0
                              '#[2])
                (array #[#[1 0] #[2 3]])))