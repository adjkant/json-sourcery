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

(module+ test
  (require "testing.rkt")
  (struct s1 [a b c])
  (struct s2 [d e f])
  (struct s3 [y z])
  (define s1-serializer (json-serializer-struct s1))
  (define symbol-serializer (json-serializer-val symbol? symbol->string))
  (define s2-obj-serializer (json-serializer-obj s2?
                                                 (d s2-d)
                                                 (e (compose string-length s2-e))))
  (define bad-serializer (json-serializer-val string? 1))

  (define valid-serializers (list s1-serializer symbol-serializer s2-obj-serializer))
  (define all-serializers (cons bad-serializer valid-serializers)))

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

(module+ test
  (check-compile (json-serializer-val symbol? symbol->string))
  (check-compile (json-serializer-val "can be garbage" "at compile time"))
  (check-compile-error (json-serializer-val))
  (check-compile-error (json-serializer-val 1))
  (check-compile-error (json-serializer-val 1 2 3)))


;; [Syntax-of [Any -> Boolean] (Id [X -> Any]) ...] -> JSONSerializer
;; Create a custom JSONSerializer using the given predictes and pairs of ids and accessors
(define-syntax json-serializer-obj
  (syntax-parser
    [(_ predicate (field:id getter) ...)
     (define field-strings (cons list (map id->string (syntax->list #'(field ...)))))
     #`(json-sourcery-obj-serializer predicate
                                     (zip (map string->symbol #,field-strings)
                                          (list getter ...)))]))

(module+ test
  (check-compile
   (json-serializer-obj symbol?
                        (str symbol->string)
                        (x (compose string-length symbol->string))))
  (check-compile
   (json-serializer-obj "can"
                        (str "also be garbage")
                        (x "in the string spots here at compile time")))
  (check-compile-error (json-serializer-obj))
  (check-compile-error (json-serializer-obj symbol? ("a" symbol->string)))
  (check-compile-error (json-serializer-obj symbol? (a 1 2)))
  (check-compile-error (json-serializer-obj symbol? (a))))

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

;; TODO figure out bug in check-compile
#;(module+ test
    (check-compile (struct basic-s [a b c]) (json-serializer-struct basic-s)))

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

(module+ test
  (check-true (serialize-failure? 'failure))
  (check-false (serialize-failure? 'failures))
  (check-false (serialize-failure? (list 1 2 3))))

;; Any [List-of JSONSerializer] -> JSExpr U 'failure
;; Serialize the expression with the given serializers to a valid JSExpr
;; If the expression cannot be serialized, return #f
(define (serialize-json e serializers)
  (with-handlers ([exn:fail:user? (λ (e) 'failure)])
    (serialize-json/error e serializers)))

(module+ test
  (check-equal? (serialize-json 1 '()) 1)
  (check-equal? (serialize-json 1 valid-serializers) 1)
  (check-equal? (serialize-json 'a valid-serializers) "a")
  (check-equal? (serialize-json (s1 1 2 3) valid-serializers)
                (json-obj (json-kv 'a 1)
                          (json-kv 'b 2)
                          (json-kv 'c 3)))
  (check-equal? (serialize-json (s2 1 "to" 3) valid-serializers)
                (json-obj (json-kv 'd 1)
                          (json-kv 'e 2)))
  (check-error (serialize-json (s2 1 2 3) valid-serializers)))
  

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

(module+ test
  (check-true (json-serializable? 1 '()))
  (check-true (json-serializable? 1 valid-serializers))
  (check-true (json-serializable? 'a valid-serializers))
  (check-true (json-serializable? (s1 1 2 3) valid-serializers))
  (check-true (json-serializable? (s2 1 "to" 3) valid-serializers))
  (check-error (json-serializable? (s2 1 2 3)))
  (check-false (json-serializable? 'a '()))
  (check-false (json-serializable? (s1 1 2 3) '()))
  (check-false (json-serializable? (s3 1 2) valid-serializers)))


;; Any JSONSerializer -> [List-of Any]
;; get a list of the serialized values from the result of serializing the given expression
(define (serialized-values e s)
  (cond
    [(json-sourcery-obj-serializer? s)
     (map (λ (a) (a e))
          (map second (json-sourcery-obj-serializer-fields s)))]
    [(json-sourcery-val-serializer? s)
     (list ((json-sourcery-val-serializer-converter s) e))]))

(module+ test
  (check-equal? (serialized-values 'a symbol-serializer) (list "a"))
  (check-equal? (serialized-values (s1 1 2 3) s1-serializer) (list 1 2 3))
  (check-equal? (serialized-values (s2 1 "to" 3) s2-obj-serializer) (list 1 2)))

;; Any [List-of JSONSerializer] -> [Maybe JSONSerializer]
;; return the first matchign serializer for the given data, if any exists
;; if no serailizers match, return #false
(define (find-matching-serializer e serializers)
  (cond [(empty? serializers) #false]
        [(cons? serializers)
         (if (matching-serializer? e (first serializers))
             (first serializers)
             (find-matching-serializer e (rest serializers)))]))

(module+ test
  (check-false (find-matching-serializer 1 valid-serializers))
  (check-equal? (find-matching-serializer (s1 1 2 3) valid-serializers) s1-serializer)
  (check-equal? (find-matching-serializer (s2 1 2 3) valid-serializers) s2-obj-serializer)
  (check-equal? (find-matching-serializer 'a valid-serializers) symbol-serializer)
  (check-equal? (find-matching-serializer "a" all-serializers) bad-serializer))

(define (matching-serializer? e s)
  (cond
    [(json-sourcery-obj-serializer? s) ((json-sourcery-obj-serializer-predicate s) e)]
    [(json-sourcery-val-serializer? s) ((json-sourcery-val-serializer-predicate s) e)]))

(module+ test
  (check-false (matching-serializer? 1          s1-serializer))
  (check-true  (matching-serializer? (s1 1 2 3) s1-serializer)))


;; ---------------------------------------------------------------------------------------------------
;; JSON Macros

;; [List-of json-kv] -> Hash
;; shorthand for creating a hash in the context of JSON with a list of json-kv pairs
(define-syntax json-obj
  (syntax-parser
    [(_ json-kvs)
     #'(if (list? json-kvs)
           (apply hash (foldr append '() json-kvs))
           (apply hash (foldr append '() (list json-kvs))))]
    [(_ json-kv ...)
     #'(apply hash (foldr append '() (list json-kv ...)))]))

;; String Any -> [list String Any)
;; way to more clearly pair JSON values for use in json-obj
(define (json-kv k v)
  (list k v))
