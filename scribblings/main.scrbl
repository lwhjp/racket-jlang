#lang scribble/manual

@(require scribble/example
          (for-label racket/base
                     j/main))

@title{Main}

@(define j-eval (make-base-eval))
@examples[#:hidden #:eval j-eval (require j/main)]

@defmodule[j]{
}

@defproc[(j [str string?]) any/c]{
  Evaluates @racket[str] as a J program. The result is an appropriate Racket value.
  In particular, verbs may be applied to arguments and should behave as expected.
  @examples[#:eval j-eval
            (j "4 * 1 + 4")
            (define mean (j "+/ % #"))
            (mean '(2 3 4 5 6))]
}
