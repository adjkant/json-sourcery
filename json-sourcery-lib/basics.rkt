#lang racket

(provide (all-defined-out))

(require (for-syntax syntax/parse
                     racket/syntax))

;; Any -> Any
;; displayln and return any value
(define (tee x)
  (displayln x) x)

;; [List-of X] [List-of Y] -> [List-of (list X Y)]
;; zip two equal sized lists together, erroring if the lists are not equal
(define (zip l1 l2)
  (map list l1 l2))

;; Boolean Any -> Any U #false
;; return a value if a condition is true, otherwise false
(define-syntax when/f
  (syntax-parser
    [(_ condition if-true) #'(if condition if-true #false)]))

;; [Syntax-of Id] -> String
;; Convert an identifier to a string
(define (id->string id)
  (symbol->string (syntax-e id)))