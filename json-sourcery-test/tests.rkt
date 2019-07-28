#lang racket

(require "test-utils.rkt")
(require json-sourcery-lib)

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
(define all-serializers (cons bad-serializer valid-serializers))

(check-compile (json-serializer-val symbol? symbol->string))
(check-compile (json-serializer-val "can be garbage" "at compile time"))
(check-compile-error (json-serializer-val))
(check-compile-error (json-serializer-val 1))
(check-compile-error (json-serializer-val 1 2 3))

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
(check-compile-error (json-serializer-obj symbol? (a)))


;; TODO figure out bug in check-compile
#;(check-compile (struct basic-s [a b c]) (json-serializer-struct basic-s))

(check-true (serialize-failure? 'failure))
(check-false (serialize-failure? 'failures))
(check-false (serialize-failure? (list 1 2 3)))


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
(check-error (serialize-json (s2 1 2 3) valid-serializers))


(check-true (json-serializable? 1 '()))
(check-true (json-serializable? 1 valid-serializers))
(check-true (json-serializable? 'a valid-serializers))
(check-true (json-serializable? (s1 1 2 3) valid-serializers))
(check-true (json-serializable? (s2 1 "to" 3) valid-serializers))
(check-error (json-serializable? (s2 1 2 3)))
(check-false (json-serializable? 'a '()))
(check-false (json-serializable? (s1 1 2 3) '()))
(check-false (json-serializable? (s3 1 2) valid-serializers))


(check-equal? (serialized-values 'a symbol-serializer) (list "a"))
(check-equal? (serialized-values (s1 1 2 3) s1-serializer) (list 1 2 3))
(check-equal? (serialized-values (s2 1 "to" 3) s2-obj-serializer) (list 1 2))

(check-false (find-matching-serializer 1 valid-serializers))
(check-equal? (find-matching-serializer (s1 1 2 3) valid-serializers) s1-serializer)
(check-equal? (find-matching-serializer (s2 1 2 3) valid-serializers) s2-obj-serializer)
(check-equal? (find-matching-serializer 'a valid-serializers) symbol-serializer)
(check-equal? (find-matching-serializer "a" all-serializers) bad-serializer)


(check-false (matching-serializer? 1          s1-serializer))
(check-true  (matching-serializer? (s1 1 2 3) s1-serializer))

