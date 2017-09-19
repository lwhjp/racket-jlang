#lang scribble/manual

@(require (for-label racket/base
                     j/number))

@title{Numbers}

@defmodule[j/number]{
  Support for parsing J-style numeric constants.
}

@defproc[(string->number/j [str string?]) (or/c number? #f)]{
  Parses @racket[str] according to the J grammar. Returns the number or @racket[#f]
  if the @racket[str] does not contain a recognizable numeric constant.
}
