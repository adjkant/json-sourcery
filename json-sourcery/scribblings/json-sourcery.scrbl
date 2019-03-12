#lang scribble/manual

@;{------------------------------------------------------------------------------------------------}
@;{Requirements} 

@(require scribble/example
          racket/sandbox
          (for-label "../main.rkt"
                     racket))

@(define sourcery-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (sandbox-path-permissions (list (list 'write "../")))
     (define me (make-evaluator 'racket))
     #;(me '(require "../main.rkt"))
     me))

@examples[#:eval sourcery-eval #:hidden]


@;{------------------------------------------------------------------------------------------------}
@;{Documentation Start} 

@title{JSONSourcery}

@author{Adrian Kant}

JSONSourcery is a library built on top of the json base package in order to add automatic and manual
JSON serialization in order to enable Racket values to be used directly for computation with less
boilerplate needed to convert to JSON as well as adding clarifying syntax macros.


@(hyperlink "https://github.com/adjkant/json-sourcery"
            "Github Repo")

@defmodule[json-sourcery]


@;{------------------------------------------------------------------------------------------------}
@section{JSON Serializers}

@defform[(json-serializer-val predicate transformer)
         #:contracts([predicate (-> any? boolean)]
                     [transformer (-> any? any?)])]{
 Create a JSON serializer for anything that passes the given predicate
 using the given transformer
}

@defform[(json-serializer-obj predicate (name accessor) ...)
         #:contracts([predicate (-> any? boolean)]
                     [name id]
                     [accessor (-> any? any?)])]{
 Create a JSON serializer for anything that passes the given predicate
 using the given names and accessors to create a JSON object jsexpr
}

@defform[(json-serializer-struct [struct-name struct?])]{
 Create a JSON serializer for the given struct automaticially using all fields of the structure to
 create a json-serializer-obj
}

@defproc[(json-serializer? [x any?])
         boolean?]{
 Predicate for JSON serializers
}


@;{------------------------------------------------------------------------------------------------}
@section{Serialization}

@defproc[(json-serializable? [expr any?] [serializers (listof json-serializer?)])
         boolean?]{
 Determines if the given expression is serializable with the given serializers. Errors with
 malformed serializers with the apropriate predicate use of accessor error
}

@defproc[(serialize-json [expr any?] [serializers (listof json-serializer?)])
         jsexpr?]{
 Attempts to serialize the expression with the given serializers. Guaranteed to succeed on any
 expression that passes json-serializable?
 If a unserializable value is encountered, the entire function will return the symbol 'failure.
 Errors with  malformed serializers with the apropriate predicate use of accessor error.
}

@defproc[(serialize-failure? [expr any?])
         boolean?]{
 Semantic predicate for checking for failures (not errors) in serialize-json
}


@;{------------------------------------------------------------------------------------------------}
@section{JSON Object Shortcuts}

@defproc[(json-obj [kv-pairs (listof json-kv)])
         hash?]{
 A shortcut for creating JSON serializable objects with the given list of json-kv's
}

@defproc[(json-kv [key string?] [value json-serializable?])
         list?]{
 A semantic shortcut for creating JSON serializable pair in a list for a JSON object.
 Equivalent to list.
}

