#lang racket/base

(require math/array
         "../rank/noun.rkt")

(define ace (array #[]))

(define alphabet (list->array (build-list 256 integer->char)))

(provide (all-defined-out))