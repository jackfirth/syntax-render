#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [strings-insert-between
   (->* ((sequence/c string?))
        (#:header string?
         #:leading-separator string?
         #:first-separator (or/c string? #false)
         #:separator string?
         #:last-separator (or/c string? #false)
         #:trailing-separator string?
         #:trailer string?)
        string?)]))


(require racket/sequence
         racket/string
         rebellion/collection/vector
         rebellion/collection/vector/builder
         rebellion/streaming/reducer
         rebellion/streaming/transducer)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define (insert-between seq
                        #:header [header #()]
                        #:leading-separator [leading-separator #()]
                        #:first-separator [first-separator #false]
                        #:separator [separator #()]
                        #:last-separator [last-separator #false]
                        #:trailing-separator [trailing-separator #()]
                        #:trailer [trailer #()])
  (let* ([vec (sequence->vector seq)]
         [header (sequence->vector header)]
         [leading-separator (sequence->vector leading-separator)]
         [separator (sequence->vector separator)]
         [trailing-separator (sequence->vector trailing-separator)]
         [trailer (sequence->vector trailer)]
         [first-separator (if first-separator (sequence->vector first-separator) separator)]
         [last-separator (if last-separator (sequence->vector last-separator) separator)])
    (define num-elements (vector-length vec))
    (cond
      [(zero? num-elements)
       (define size (+ (vector-length header) (vector-length trailer)))
       (define builder (make-vector-builder #:expected-size size))
       (vector-builder-add-all builder header)
       (vector-builder-add-all builder trailer)
       (build-vector builder)]
      [(equal? num-elements 1)
       (define size
         (+ (vector-length header)
            (vector-length leading-separator)
            num-elements
            (vector-length trailing-separator)
            (vector-length trailer)))
       (define builder (make-vector-builder #:expected-size size))
       (vector-builder-add-all builder header)
       (vector-builder-add-all builder leading-separator)
       (vector-builder-add-all builder vec)
       (vector-builder-add-all builder trailing-separator)
       (vector-builder-add-all builder trailer)
       (build-vector builder)]
      [(equal? num-elements 2)
       (define size
         (+ (vector-length header)
            (vector-length leading-separator)
            num-elements
            (vector-length first-separator)
            (vector-length trailing-separator)
            (vector-length trailer)))
       (define builder (make-vector-builder #:expected-size size))
       (vector-builder-add-all builder header)
       (vector-builder-add-all builder leading-separator)
       (vector-builder-add builder (vector-ref vec 0))
       (vector-builder-add-all builder first-separator)
       (vector-builder-add builder (vector-ref vec 1))
       (vector-builder-add-all builder trailing-separator)
       (vector-builder-add-all builder trailer)
       (build-vector builder)]
      [else
       (define size
         (+ (vector-length header)
            (vector-length leading-separator)
            (vector-length first-separator)
            num-elements
            (* (vector-length separator) (- num-elements 2))
            (vector-length last-separator)
            (vector-length trailing-separator)
            (vector-length trailer)))
       (define builder (make-vector-builder #:expected-size size))
       (vector-builder-add-all builder header)
       (vector-builder-add-all builder leading-separator)
       (vector-builder-add builder (vector-ref vec 0))
       (vector-builder-add-all builder first-separator)
       (vector-builder-add builder (vector-ref vec 1))
       (for ([v (in-vector vec 2 (sub1 num-elements))])
         (vector-builder-add-all builder separator)
         (vector-builder-add builder v))
       (vector-builder-add-all builder last-separator)
       (vector-builder-add builder (vector-ref vec (sub1 num-elements)))
       (vector-builder-add-all builder trailing-separator)
       (vector-builder-add-all builder trailer)
       (build-vector builder)])))


(define (string-insert-between seq
                               #:header [header #()]
                               #:leading-separator [leading-separator #()]
                               #:first-separator [first-separator #false]
                               #:separator [separator #()]
                               #:last-separator [last-separator #false]
                               #:trailing-separator [trailing-separator #()]
                               #:trailer [trailer #()])
  (define chars
    (insert-between
     seq
     #:header header
     #:leading-separator leading-separator
     #:first-separator first-separator
     #:separator separator
     #:last-separator last-separator
     #:trailing-separator trailing-separator
     #:trailer trailer))
  (transduce chars #:into into-string))


(define (strings-insert-between strings
                                #:header [header ""]
                                #:leading-separator [leading-separator ""]
                                #:first-separator [first-separator #false]
                                #:separator [separator ""]
                                #:last-separator [last-separator #false]
                                #:trailing-separator [trailing-separator ""]
                                #:trailer [trailer ""])
  (define modified-strings
    (insert-between
     strings
     #:header (vector header)
     #:leading-separator (vector leading-separator)
     #:first-separator (and first-separator (vector first-separator))
     #:separator (vector separator)
     #:last-separator (and last-separator (vector last-separator))
     #:trailing-separator (vector trailing-separator)
     #:trailer (vector trailer)))
  (string-join (vector->list modified-strings) ""))


(module+ test

  (test-case "basic separators"
    (check-equal? (string-insert-between "hello" #:separator ", ") "h, e, l, l, o")
    (check-equal? (string-insert-between "h" #:separator ", ") "h")
    (check-equal? (string-insert-between "" #:separator ", ") ""))

  (test-case "headers and trailers"
    (check-equal? (string-insert-between "hello" #:header "<" #:trailer ">") "<hello>")
    (check-equal? (string-insert-between "h" #:header "<" #:trailer ">") "<h>")
    (check-equal? (string-insert-between "" #:header "<" #:trailer ">") "<>"))

  (test-case "leading separator"
    (check-equal? (string-insert-between "hello" #:header "<" #:leading-separator "=") "<=hello")
    (check-equal? (string-insert-between "h" #:header "<" #:leading-separator "=") "<=h")
    (check-equal? (string-insert-between "" #:header "<" #:leading-separator "=") "<"))

  (test-case "trailing separator"
    (check-equal? (string-insert-between "hello" #:trailer ">" #:trailing-separator "=") "hello=>")
    (check-equal? (string-insert-between "h" #:trailer ">" #:trailing-separator "=") "h=>")
    (check-equal? (string-insert-between "" #:trailer ">" #:trailing-separator "=") ">"))

  (test-case "first and last separators"
    (check-equal?
     (string-insert-between "hello" #:first-separator ":" #:separator "," #:last-separator ";")
     "h:e,l,l;o")
    (check-equal?
     (string-insert-between "hel" #:first-separator ":" #:separator "," #:last-separator ";") "h:e;l")
    (check-equal?
     (string-insert-between "he" #:first-separator ":" #:separator "," #:last-separator ";") "h:e")
    (check-equal?
     (string-insert-between "h" #:first-separator ":" #:separator "," #:last-separator ";") "h")
    (check-equal?
     (string-insert-between "" #:first-separator ":" #:separator "," #:last-separator ";") ""))

  (test-case "everything"
    (check-equal?
     (string-insert-between "hello"
                            #:header "<"
                            #:leading-separator "["
                            #:first-separator ":"
                            #:separator ","
                            #:last-separator ";"
                            #:trailing-separator "]"
                            #:trailer ">")
     "<[h:e,l,l;o]>")
    (check-equal?
     (string-insert-between "hel"
                            #:header "<"
                            #:leading-separator "["
                            #:first-separator ":"
                            #:separator ","
                            #:last-separator ";"
                            #:trailing-separator "]"
                            #:trailer ">")
     "<[h:e;l]>")
    (check-equal?
     (string-insert-between "he"
                            #:header "<"
                            #:leading-separator "["
                            #:first-separator ":"
                            #:separator ","
                            #:last-separator ";"
                            #:trailing-separator "]"
                            #:trailer ">")
     "<[h:e]>")
    (check-equal?
     (string-insert-between "h"
                            #:header "<"
                            #:leading-separator "["
                            #:first-separator ":"
                            #:separator ","
                            #:last-separator ";"
                            #:trailing-separator "]"
                            #:trailer ">")
     "<[h]>")
    (check-equal?
     (string-insert-between ""
                            #:header "<"
                            #:leading-separator "["
                            #:first-separator ":"
                            #:separator ","
                            #:last-separator ";"
                            #:trailing-separator "]"
                            #:trailer ">")
     "<>")))
