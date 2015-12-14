#lang racket/base

(require math/array
         racket/list
         racket/sequence
         racket/vector
         "main.rkt")

(module+ test
  (require rackunit))

(define (in-items v)
  (cond
    [(array? v)
     (if (< (array-dims v) 2)
         (in-array v)
         (in-array-axis v 0))]
    [(and (sequence? v)
          (not (number? v)))
     v]
    [else (in-value v)]))

(define (rerank proc new-rank)
  (ranked-procedure
   (λ vs
     (apply/rank proc vs))
   (if (procedure? new-rank)
       new-rank
       (λ (arity)
         (make-list arity new-rank)))))

(module+ test
  (check-equal?
   (apply/rank + (list '(100 200) (index-array '#[2 3])))
   (array #[#[100 101 102] #[203 204 205]]))
  (check-equal?
   (apply/rank (rerank + 1)
               (list '(100 200 300)
                     (index-array '#[2 3])))
   (array #[#[100 201 302] #[103 204 305]])))

(define integers
  (ranked-procedure
   (λ (y)
     (index-array (array->vector y)))
   (monadic-rank 1)))

(module+ test
  (check-equal?
   (apply/rank integers '((2 3 4)))
   (array #[#[#[0 1 2 3] #[4 5 6 7] #[8 9 10 11]]
            #[#[12 13 14 15] #[16 17 18 19] #[20 21 22 23]]]))
  (check-equal?
   (apply/rank (rerank integers 0) '((0 1 2 3)))
   (array #[#[0 0 0] #[0 0 0] #[0 1 0] #[0 1 2]])))

(define (insert u [identity 0])
  (ranked-procedure
   (case-lambda
     [(y)
      (define items (in-items y))
      (case (sequence-length items)
        [(0) identity]
        [(1) (sequence-ref items 0)]
        [else (for/fold ([a (sequence-ref items 0)])
                        ([b (sequence-tail items 1)])
                (apply/rank u (list a b)))])]
     [(x y)
      (define x-items (in-items x))
      (define y-items (in-items y))
      (unframe/fill
       (build-array
        (vector-append (shape x) (shape y))
        (λ (indexes)
          (apply/rank
           u
           (list (sequence-ref x-items (vector-ref indexes 0))
                 (sequence-ref y-items (vector-ref indexes 1))))))
       identity)])
   (rank-combine
    (monadic-rank 'any)
    (dyadic-rank 'any 'any))))

(module+ test
  (let ([m (index-array '#[3 2])])
    (check-equal? (apply/rank (insert +) (list m))
                  (array #[6 9]))
    (check-equal? (apply/rank (rerank (insert +) 1) (list m))
                  (array #[1 5 9])))
  (check-equal? (apply/rank (insert +) '((2 3 4))) (array 9))
  (check-equal? (apply/rank (insert *) '((1 2 3) (4 5 6 7)))
                (array #[#[4 5 6 7] #[8 10 12 14] #[12 15 18 21]])))