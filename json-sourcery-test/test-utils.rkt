#lang racket

(provide (all-from-out rackunit)
         (all-defined-out))

(require rackunit)

(require (for-syntax syntax/parse
                     rackunit))

(define-syntax check-error
  (syntax-parser
    [(_ exn-expr failure-message:string) #'(check-exn exn:fail? (λ () exn-expr) failure-message)]
    [(_ exn-expr) #'(check-exn exn:fail? (λ () exn-expr))]))

(define-syntax check-not-error
  (syntax-parser
    [(_ expr failure-message:string) #'(check-not-exn (λ () expr) failure-message)]
    [(_ expr) #'(check-not-exn (λ () expr))]))

(define-syntax check-compile
  (syntax-parser
    [(_ cmp-exn-expr ... failure-message:string)
     #`(check-not-error (eval-syntax #`(begin (require "main.rkt") #,#'cmp-exn-expr ...))
                        failure-message)]
    [(_ cmp-exn-expr ...)
     #`(check-not-error (eval-syntax #`(begin (require "main.rkt") #,#'cmp-exn-expr ...)))]))

(define-syntax check-compile-error
  (syntax-parser
    [(_ cmp-exn-expr ... failure-message:string)
     #`(check-error (eval-syntax #`(begin (require "main.rkt") #,#'cmp-exn-expr ...))
                    failure-message)]
    [(_ cmp-exn-expr ...)
     #`(check-error (eval-syntax #`(begin (require "main.rkt") #,#'cmp-exn-expr ...)))]))