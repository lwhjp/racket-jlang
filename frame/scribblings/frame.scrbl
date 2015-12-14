#lang scribble/manual

@(require (for-label math/array
                     racket
                     "../main.rkt"))

@title{Frame Library}

@defmodule[frame]{
}

@section{Nouns}

@defproc[(->array [v any/c]) array?]{
  Converts the value @racket[v] to an array. Arrays are unchanged;
  sequences (except numbers) become one-dimensional arrays; and
  other values become zero-dimensional arrays.
}

@defproc[(rank [v any/c]) exact-nonnegative-integer?]{
  Gives the rank of @racket[v]. Equivalent to
  @racket[(vector-length (shape v))].
}

@defproc[(shape [v any/c]) (vectorof exact-nonnegative-integer?)]{
  Gives the shape of @racket[v]. Equivalent to
  @racket[(array-shape (->array v))].
}

@section{Frames}

@defstruct[frame ([data array?]
                  [length exact-nonnegative-integer?]
                  [fill any/c])]{
  Structure representing a frame.
}

@defproc[(frame-shape [f frame?]) (vectorof exact-nonnegative-integer?)]{
  Gives the shape of the frame.
}

@defproc[(frame-cell-shape [f frame?]) (vectorof exact-nonnegative-integer?)]{
  Gives the shape of the cells of @racket[f].
}

@defproc[(make-frame [arr array?] [cell-rank exact-integer?] [fill any/c]) frame?]{
  Imposes a frame on @racket[arr] with cell rank
  @racket[cell-rank], which may be negative.
  @racket[fill] is used when creating fill cells.
}

@defproc[(frame-extend [f frame?] [ds (vectorof exact-nonnegative-integer?)]) frame?]{
  Changes the shape of @racket[f] to @racket[ds].
}

@defproc[(frame-fill-cell [f frame?]) array?]{
  Creates a fill cell for @racket[f].
}

@defproc[(frame-map [proc procedure?] [f frame?] ...+) array?]{
  Creates an array of the results of applying @racket[proc]
  to each of the cells in each frame @racket[f] (which must be
  the same size).
}

@defproc[(cell-shape-broadcast [dss (listof (vectorof exact-nonnegative-integer?))])
         (vectorof exact-nonnegative-integer?)]{
}

@defproc[(unframe/fill [arr array?]
                       [fill any/c]
                       [maybe-cell-shape (or/c (vectorof exact-nonnegative-integer?) #f) #f])
         array?]{
}

@section{Verbs}
