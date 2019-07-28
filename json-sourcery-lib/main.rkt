#lang racket

(provide json-serializer-val
         json-serializer-obj
         json-serializer-struct
         json-serializer?
         json-serializable?
         serialize-json
         serialize-failure?
         json-obj
         json-kv)

(require json
         "struct.rkt"
         "basics.rkt"
         racket/exn)

(require (for-syntax syntax/parse
                     racket/syntax
                     syntax/parse/class/struct-id
                     "struct.rkt"
                     "basics.rkt"))

;; ---------------------------------------------------------------------------------------------------
;; Data Definitions

(struct json-sourcery-obj-serializer [predicate fields] #:transparent)
(struct json-sourcery-val-serializer [predicate converter] #:transparent)

;; A JSONSerializer is one of:
;; - (json-sourcery-obj-serializer [Any -> Boolean] (list Symbol [X -> Any]))
;; - (json-sourcery-val-serializer [Any -> Boolean] [X -> Any])


;; ---------------------------------------------------------------------------------------------------
;; Serializers

;; [Syntax-of [Any -> Boolean] [X -> Any]] -> JSONSerializer
;; Create a custom JSONSerializer using the given predictes and pairs of ids and accessors
(define-syntax json-serializer-val
  (syntax-parser
    [(_ predicate converter) #`(json-sourcery-val-serializer predicate converter)]))

;; [Syntax-of [Any -> Boolean] (Id [X -> Any]) ...] -> JSONSerializer
;; Create a custom JSONSerializer using the given predictes and pairs of ids and accessors
(define-syntax json-serializer-obj
  (syntax-parser
    [(_ predicate (field:id getter) ...)
     (define field-strings (cons list (map id->string (syntax->list #'(field ...)))))
     #`(json-sourcery-obj-serializer predicate
                                     (zip (map string->symbol #,field-strings)
                                          (list getter ...)))]))

;; Id -> JSONSerializer
;; Create a JSONSerializer for the given struct name
(define-syntax json-serializer-struct
  (syntax-parser
    [(_ s:struct-id)
     (define struct-predicate (format-id #'s "~a?" (syntax-e #'s)))
     #`(let [(named-accessors-list (struct-named-accessors s))]
         (if named-accessors-list
             (json-sourcery-obj-serializer #,struct-predicate named-accessors-list)
             (error 'json-serializer-struct
                    "cannot serialize ~a: subtyped structures not allowed" s)))]))



;; Any -> Boolean
;; determine if the given item is a JSON serializer
(define (json-serializer? x)
  (or (json-sourcery-obj-serializer? x)
      (json-sourcery-val-serializer? x)))

;; ---------------------------------------------------------------------------------------------------
;; Serialization

;; Any -> Boolean
;; semantic shortcut for checking for 'failure
(define (serialize-failure? e)
  (and (symbol? e) (symbol=? e 'failure)))

;; Any [List-of JSONSerializer] -> JSExpr U 'failure
;; Serialize the expression with the given serializers to a valid JSExpr
;; If the expression cannot be serialized, return #f
(define (serialize-json e serializers)
  (with-handlers ([exn:fail:user? (λ (e) 'failure)])
    (serialize-json/error e serializers)))

;; Any [List-of JSONSerializer] -> JSExpr
;; Serialize the expression with the given serializers to a valid JSExpr
;; If the expression cannot be serialized, raise an error
(define (serialize-json/error e serializers)
  (define matched-serializer (find-matching-serializer e serializers))
  (cond
    [matched-serializer (serialize-and-recur e matched-serializer serializers)]
    [(jsexpr? e) e]
    [else (error 'serialize-json
                 "given value and serializers results in an unserializable value: ~a" e)]))

;; Any [List-of JSONSerializer] -> JSExpr
;; serialize the expression with the given serializers and recur on the serialized result
(define (serialize-and-recur e s serializers)
  (cond
    [(json-sourcery-obj-serializer? s)
     (apply hash
            (foldr append '()
                   (map
                    (λ (f)
                      (json-kv (first f) (serialize-json ((second f) e) serializers)))
                    (json-sourcery-obj-serializer-fields s))))]
    [(json-sourcery-val-serializer? s)
     (serialize-json ((json-sourcery-val-serializer-converter s) e) serializers)]))

;; Any [List-of JSONSerializer] -> Boolean
;; determine if the given expression can be serialized with the given serializers
(define (json-serializable? e serializers)
  (define matched-serializer (find-matching-serializer e serializers))
    (or (and matched-serializer
             (andmap (λ (subexp) (json-serializable? subexp serializers))
                     (serialized-values e matched-serializer)))
        (jsexpr? e)))


;; Any JSONSerializer -> [List-of Any]
;; get a list of the serialized values from the result of serializing the given expression
(define (serialized-values e s)
  (cond
    [(json-sourcery-obj-serializer? s)
     (map (λ (a) (a e))
          (map second (json-sourcery-obj-serializer-fields s)))]
    [(json-sourcery-val-serializer? s)
     (list ((json-sourcery-val-serializer-converter s) e))]))

;; Any [List-of JSONSerializer] -> [Maybe JSONSerializer]
;; return the first matchign serializer for the given data, if any exists
;; if no serailizers match, return #false
(define (find-matching-serializer e serializers)
  (cond [(empty? serializers) #false]
        [(cons? serializers)
         (if (matching-serializer? e (first serializers))
             (first serializers)
             (find-matching-serializer e (rest serializers)))]))

(define (matching-serializer? e s)
  (cond
    [(json-sourcery-obj-serializer? s) ((json-sourcery-obj-serializer-predicate s) e)]
    [(json-sourcery-val-serializer? s) ((json-sourcery-val-serializer-predicate s) e)]))

;; ---------------------------------------------------------------------------------------------------
;; JSON Macros

;; [List-of json-kv] -> Hash
;; shorthand for creating a hash in the context of JSON with a list of json-kv pairs
(define-syntax json-obj
  (syntax-parser
    [(_ json-kvs)
     #'(if (and (list? json-kvs) (list? (first json-kvs)))
           (apply hash (foldr append '() json-kvs))
           (apply hash json-kvs))]
    [(_ json-kv ...)
     #'(apply hash (foldr append '() (list json-kv ...)))]))

;; String Any -> [list String Any)
;; way to more clearly pair JSON values for use in json-obj
(define (json-kv k v)
  (list k v))
