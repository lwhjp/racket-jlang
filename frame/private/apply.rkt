#lang racket

(require math/array
         "frame.rkt"
         "noun.rkt"
         "verb.rkt")

(provide (all-defined-out))

(module+ test
  (require rackunit))

(define (apply/rank proc args)
  (define ranked-proc
    (->ranked-procedure proc))
  (define raw-frames
    (map
     (λ (v r)
       (make-frame (->array v) r 0))
     args
     (procedure-rank ranked-proc (length args))))
  (define common-shape
    (let ([frame-shapes (map frame-shape raw-frames)])
      (argmax vector-length frame-shapes)))
  (define frames
    (map (λ (f)
           (frame-extend f common-shape))
         raw-frames))
  (define result
    (apply frame-map ranked-proc frames))
  (unframe/fill
   result
   0 ;; FIXME: appropriate fill
   #f)) ;; TODO: use fill cell to deduce result shape when frame is empty

(module+ test
  (check-equal? (apply/rank + (list '(100 200) (index-array '#[2 3])))
                (array #[#[100 101 102] #[203 204 205]])))