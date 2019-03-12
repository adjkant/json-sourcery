#lang info

(define collection "json-sourcery")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/json-sourcery.scrbl" ())))
(define pkg-desc "A library built on top of the json package for improving JSON serialization and
                  adding clear syntax macros.")
(define version "1.0.0")
(define pkg-authors '(adjkant))
