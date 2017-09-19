J in Racket
===========

Installation
------------

Probably the easiest method is to create a local link using
`raco pkg install` in the top level of this repository.
This will create links so that `require` and friends work properly.

Motivation
----------

I've been playing with J a little, and the rank-based implicit looping
and point-free style are very appealing. As a fan of Racket, I'd like
to see all the cool ideas from other languages available. This in
theory would mean that any cool language could be implemented easily
on top of Racket using appropriate libraries and simple parser and
translation modules (tricky semantics aside).

So, for fun, my goal with this project is to implement most or all
of the J language in Racket, and factor out all the useful bits into
separate collections which can be packaged separately.

Status
------

Language support is far from complete, but some simple expressions
("sentences") can be evaluated. I've made a start on a Racket-style
"ranked apply" which is one of my favourite concepts from J.

There is a `#lang j` which isn't very useful, but will evaluate
J sentences in order.

You can `(require j)` to get access to a `j` procedure for evaluating
J sentences.

Example:

    > (require j)
    > (j "+/ 1 2 3")
    6
    > (define sum/j (j "+/"))
    > (sum/j '(4 5 6))
    15
    > (sum-j '(1 2 3) '(4 5 6))
    (array #[#[5 6 7] #[6 7 8] #[7 8 9]])

Using frames:

    > (require math/array j/rank)
    > (apply/rank + (list '(100 200) (index-array '#[2 3])))
    (array #[#[100 101 102] #[203 204 205]])

Note that this is the usual Racket `+` function. You can define
more advanced operations by using `lambda/rank`.
