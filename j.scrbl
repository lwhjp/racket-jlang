#lang scribble/manual

@(require (for-label math/array
                     racket
                     "main.rkt"
                     "number.rkt"
                     "rank.rkt")
          racket/sandbox
          scribble/eval)

@title{J in Racket}

@centered{@bold{Note}}

This is a work in progress. J support is incomplete at this time.
The interfaces here are liable to change substantially.

@section{Main}

@defmodule[j]{
}

@(define j-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'racket/base #:requires '(j))))

@defproc[(j [str string?]) any/c]{
  Evaluates @racket[str] as a J program. The result is an appropriate Racket value.
  In particular, verbs may be applied to arguments and should behave as expected.
  @examples[#:eval j-evaluator
            (j "4 * 1 + 4")
            (define mean (j "+/ % #"))
            (mean '(2 3 4 5 6))]
}

@section{J library}

@defmodule[j/lib]{
  This module exposes J primitives with Racket-like names.
}

@section{Numbers}

@defmodule[j/number]{
  Support for parsing J-style numeric constants.
}

@defproc[(string->number/j [str string?]) (or/c number? #f)]{
  Parses @racket[str] according to the J grammar. Returns the number or @racket[#f]
  if the @racket[str] does not contain a recognizable numeric constant.
}

@section{Ranked apply}

@(define rank-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'racket/base #:requires '(math/array j/rank))))

@defmodule[j/rank]{
  Support for J-style ranked apply.
}

@subsection{Nouns}

@defproc[(->array [v any/c]) array?]{
  Coerces @racket[v] to an array.
}

@defproc[(noun-rank [v any/c]) exact-nonnegative-integer?]{
  Returns the rank of @racket[v].
}

@defproc[(noun-shape [v any/c]) (vectorof exact-nonnegative-integer?)]{
  Returns the shape of @racket[v].
}

@defproc[(noun-tally [v any/c]) exact-nonnegative-integer?]{
  Returns the number of items in @racket[v].
}

@defproc[(in-items [v any/c]) sequence?]{
  Returns a sequence consisting of the items of @racket[v].
}

@subsection{Verbs}

@defproc[(ranked-procedure? [v any/c]) boolean?]{
  Tests whether @racket[v] is a ranked procedure.
}

@defproc[(atomic-procedure->ranked-procedure [proc procedure?]
                                             [arity-hint (option/c exact-nonnegative-integer?) #f])
         ranked-procedure?]{
  Converts @racket[proc] to a ranked procedure with rank @racket[0].
  If @racket[proc] has multiple arities, supply @racket[arity-hint] to pick one.
}

@defform[(lambda/rank (arg ...) body ...+)
         #:grammar
         [(arg id [id rank])]]{
  Creates a ranked procedure. If @racket[rank] is not specified, it defaults
  to @racket[#f] (infinite).
}

@defform[(define/rank (id arg ...) body ...+)]{
  Shorthand for defining ranked procedures.
}

@defproc[(apply/rank [proc (or/c procedure? ranked-procedure?)]
                     [v any/c] ...
                     [lst list?]
                     [#:fill fill any/c (void)])
         any/c]{
  Similar to regular @racket[apply], except that @racket[proc] may be mapped
  over the arguments depending on its rank. Non-ranked procedures are assumed
  to have rank @racket[0].

  Ranked procedures will operate in this way implicitly.

  @examples[#:eval rank-evaluator
            (require math/array)
            (apply/rank + (list '(100 200) (index-array '#[2 3])))
            (define/rank (box-rows [y 1]) (box y))
            (apply/rank box-rows (list (index-array '#[2 3])))
            (box-rows (index-array '#[2 3]))]
}