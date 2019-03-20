#lang info

(define collection "json-sourcery")
(define deps '("base" "syntax-classes"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/json-sourcery.scrbl" ())))
(define pkg-desc "A library built on top of the json package for improving JSON serialization and
                  adding clearer syntax macros.")
(define version "0.5.0")
(define pkg-authors '(adjkant))
