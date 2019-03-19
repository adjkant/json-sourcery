#lang racket/base

(require racket/require)

(require (multi-in racket [bool list struct-info])
         (rename-in syntax/parse [attribute @])
         syntax/parse/class/local-value
         syntax/parse/experimental/specialize)

(provide struct-id)

(define-syntax-class/specialize local-value/struct-info
  (local-value struct-info? #:name "structure type"))

(define-syntax-class struct-id
  #:description #f
  #:commit
  #:attributes [descriptor-id constructor-id predicate-id all-fields-visible? supertype-id
                     [accessor-id 1] num-fields num-supertype-fields num-own-fields]
  [pattern id:local-value/struct-info
    #:attr info (extract-struct-info (@ id.local-value))
    #:attr descriptor-id #`#,(first (@ info))
    #:attr constructor-id #`#,(second (@ info))
    #:attr predicate-id #`#,(third (@ info))
    #:attr all-fields-visible? #`#,(or (empty? (fourth (@ info)))
                                   (not (false? (last (fourth (@ info))))))
    #:attr [accessor-id 1] (let ([accessor-ids (reverse (fourth (@ info)))])
                             (if (@ all-fields-visible?)
                                 accessor-ids
                                 (rest accessor-ids)))
    #:attr supertype-id #`#,(sixth (@ info))
    #:attr num-fields #`#,(length (@ accessor-id))
    #:attr num-supertype-fields
           #`#,(if (identifier? (@ supertype-id))
               (let ([supertype-info (extract-struct-info (syntax-local-value #'supertype-id))])
                 (count identifier? (fourth supertype-info)))
               0)
    #:attr num-own-fields #`#,(- (syntax-e (@ num-fields)) (syntax-e (@ num-supertype-fields)))])
