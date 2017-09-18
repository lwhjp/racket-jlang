#lang racket/base

(require racket/math
         racket/string)

(define (*string->number str radix start end)
  ;; radix can be pretty much anything, so we can't use the Racket version
  (define (char->digit c)
    (- (char->integer c)
       (if (char-numeric? c)
           (char->integer #\0)
           (- (char->integer #\a) 10))))
  (define parts (string-split (substring str start end) "."))
  (define mag (sub1 (string-length (car parts))))
  (for/sum ([c (in-string (string-append* parts))]
            [i (in-naturals)])
    (* (char->digit c)
       (expt radix (- mag i)))))

(define (string->number/j str)
  (let/ec return
    (define (invalid) (return #f))
    (define (parse-base start end)
      (cond
        [(regexp-match-positions #rx"b" str start end)
         => (λ (result)
              (define pos (caar result))
              (*string->number str (parse-px start pos) (add1 pos) end))]
        [else (parse-px start end)]))
    (define (parse-px start end)
      (cond
        [(regexp-match-positions #rx"[px]" str start end)
         => (λ (result)
              (define pos (caar result))
              (* (parse-complex start pos)
                 (let ([e (parse-complex (add1 pos) end)])
                   (if (eqv? #\p (string-ref str pos))
                       (expt pi e)
                       (exp e)))))]
        [else (parse-complex start end)]))
    (define (parse-complex start end)
      (cond
        [(regexp-match-positions #rx"j" str start end)
         => (λ (result)
              (define pos (caar result))
              (make-rectangular (parse-rational start pos)
                                (parse-rational (add1 pos) end)))]
        [(regexp-match-positions #rx"a[dr]" str start end)
         => (λ (result)
              (define pos (caar result))
              (define degrees?
                (eqv? #\d (string-ref str (add1 pos))))
              (let ([a (parse-rational (+ pos 2) end)])
                (make-polar (parse-rational start pos)
                            (if degrees?
                                (degrees->radians a)
                                a))))]
        [else (parse-rational start end)]))
    (define (parse-rational start end)
      (cond
        [(regexp-match-positions #rx"r" str start end)
         => (λ (result)
              (define pos (caar result))
              (/ (parse-inf start pos)
                 (parse-inf (add1 pos) end)))]
        [else (parse-inf start end)]))
    (define (parse-inf start end)
      (cond
        [(regexp-match? #rx"^__$" str start end) -inf.0]
        [(regexp-match? #rx"^_$" str start end) +inf.0]
        [else (parse-exp start end)]))
    (define (parse-exp start end)
      (let ([n (or (string->number
                    (string-replace
                     (substring str start end)
                     "_" "-"))
                   (invalid))])
        (if (integer? n)
            (inexact->exact n)
            n)))
    (if (string=? "_." str)
        +nan.0
        (parse-base 0 (string-length str)))))

(provide string->number/j)

(module+ test
  (require rackunit)
  (let ([ε 0.001])
    (check-eqv? (string->number/j "2.3e2") 230)
    (check-= (string->number/j "2.3e_2") 0.023 ε)
    (check-eqv? (string->number/j "2j3") 2+3i)
    (check-= (string->number/j "2p1") 6.28319 ε)
    (check-= (string->number/j "1p_1") 0.31831 ε)
    (check-= (string->number/j "1x2") 7.38906 ε)
    (check-= (string->number/j "2x1") 5.43656 ε)
    (check-= (string->number/j "1x_1") 0.367879 ε)
    (check-eqv? (string->number/j "2e2j_2e2") 200-200i)
    (check-= (string->number/j "2e2j2p1") 628.319+6.28319i ε)
    (check-= (string->number/j "2ad45") 1.41421+1.41421i ε)
    (check-= (string->number/j "2ar0.785398") 1.41421+1.41421i ε)
    (check-eqv? (string->number/j "16b1f") 31)
    (check-eqv? (string->number/j "10b23") 23)
    (check-eqv? (string->number/j "_10b23") -17)
    (check-eqv? (string->number/j "1e2b23") 203)
    (check-= (string->number/j "2b111.111") 7.875 ε)
    (check-eqv? (string->number/j "1r2") 1/2)
    (check-eqv? (string->number/j "_1r2") -1/2)
    (check-eqv? (string->number/j "2r4") 1/2)
    (check-eqv? (string->number/j "2r_4") -1/2)
    (check-eqv? (string->number/j "_2r_4") 1/2)
    (check-eqv? (string->number/j "0r9") 0)
    (check-eqv? (string->number/j "5") 5)
    (check-eqv? (string->number/j "_5") -5)
    (check-pred nan? (string->number/j "_."))
    (check-pred (λ (x) (and (infinite? x) (positive? x)))
                (string->number/j "_"))
    (check-pred (λ (x) (and (infinite? x) (negative? x)))
                (string->number/j "__"))))