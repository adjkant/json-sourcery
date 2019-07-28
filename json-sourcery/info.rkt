#lang info

(define collection 'multi)

(define version "0.5.1")
(define pkg-authors '(adjkant))
(define pkg-desc "A library built on top of the json package for improving JSON serialization and
                  adding clearer syntax macros.")

(define deps
  '("base"
    "json-sourcery-lib"
    "json-sourcery-doc"))

(define build-deps '())

(define implies
  '("json-sourcery-lib"
    "json-sourcery-doc"))