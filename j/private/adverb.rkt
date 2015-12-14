#lang racket/base

(require racket/contract/base
         "verb.rkt")

(struct adverb (proc)
  #:property prop:procedure (struct-field-index proc))

(provide
 (contract-out
  (struct adverb ([proc (-> verb? verb?)]))))