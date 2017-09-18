#lang racket/base

(require "lib/adverbs.rkt"
         "lib/conjunctions.rkt"
         "lib/nouns.rkt"
         "lib/verbs.rkt")

(provide (all-from-out "lib/adverbs.rkt"
                       "lib/conjunctions.rkt"
                       "lib/nouns.rkt"
                       "lib/verbs.rkt"))