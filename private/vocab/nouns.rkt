#lang racket/base

(provide (all-defined-out))

(require math/array)

(define jn:alphabet (list->array (build-list 256 integer->char)))

(define jn:ace (array #[]))
