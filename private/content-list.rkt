#lang racket/base


(require racket/contract/base)


(provide
 (struct-out content-line)
 (struct-out content-list)
 (record-out linebreak)
 (contract-out
  [content-list-builder? predicate/c]
  [build-content-list (-> content-list-builder? content-list?)]
  [make-content-list-builder (-> content-list-builder?)]
  [content-list-builder-add
   (-> content-list-builder? (or/c string? linebreak?) content-list-builder?)]
  [content-list-builder-add-subcontent
   (-> content-list-builder? content-list? content-list-builder?)]))


(require racket/match
         rebellion/collection/vector
         rebellion/collection/vector/builder
         rebellion/type/record)


(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))


;@----------------------------------------------------------------------------------------------------


(struct content-line (indentation text) #:transparent)


(struct content-list (lines)
  #:transparent
  #:guard (λ (lines _) (sequence->vector lines)))


(struct content-list-builder (lines-builder [next-line #:mutable] [current-indentation #:mutable]))


(define (make-content-list-builder)
  (content-list-builder (make-vector-builder) "" 0))


(define (content-list-builder-add-text builder text)
  (define next-line (string-append (content-list-builder-next-line builder) text))
  (set-content-list-builder-next-line! builder next-line)
  builder)


(define (content-list-builder-add-linebreak builder break)
  (match-define
    (linebreak #:before-break before-break
               #:after-break after-break
               #:indentation indentation
               #:line-quantity line-quantity)
    break)
  (define current-indentation (content-list-builder-current-indentation builder))
  (define next-line
    (content-line
     current-indentation (string-append (content-list-builder-next-line builder) before-break)))
  (vector-builder-add (content-list-builder-lines-builder builder) next-line)
  (for ([_ (in-range (sub1 line-quantity))])
    (vector-builder-add (content-list-builder-lines-builder builder)
                        (content-line current-indentation "")))
  (set-content-list-builder-next-line! builder after-break)
  (set-content-list-builder-current-indentation! builder indentation)
  builder)


(define (content-list-builder-add builder item)
  (if (string? item)
      (content-list-builder-add-text builder item)
      (content-list-builder-add-linebreak builder item)))


(define (content-list-builder-add-subcontent builder subcontent)
  (match-define (content-line _ first-line-text) (vector-ref (content-list-lines subcontent) 0))
  (content-list-builder-add-text builder first-line-text)
  (define outer-indentation (content-list-builder-current-indentation builder))
  (for ([line (in-vector (content-list-lines subcontent) 1)])
    (define lb
      (linebreak #:before-break ""
                 #:after-break (content-line-text line)
                 #:indentation (+ outer-indentation (content-line-indentation line))
                 #:line-quantity 1))
    (content-list-builder-add-linebreak builder lb))
  builder)


(define (build-content-list builder)
  (define current-indentation (content-list-builder-current-indentation builder))
  (define last-line
    (content-line
     current-indentation (content-list-builder-next-line builder)))
  (vector-builder-add (content-list-builder-lines-builder builder) last-line)
  (content-list (build-vector (content-list-builder-lines-builder builder))))


;; This is a string that's broken across (possibly multiple) lines.
(define-record-type linebreak (before-break after-break indentation line-quantity))


(module+ test
  (test-case (name-string build-content-list)

    (test-case "basics"
      (define builder (make-content-list-builder))
      (define lb (linebreak #:before-break "" #:after-break "" #:indentation 1 #:line-quantity 1))

      (content-list-builder-add-text builder "(")
      (content-list-builder-add-text builder "fruit-list")
      (content-list-builder-add-linebreak builder lb)
      (content-list-builder-add-text builder "apple")
      (content-list-builder-add-linebreak builder lb)
      (content-list-builder-add-text builder "banana")
      (content-list-builder-add-linebreak builder lb)
      (content-list-builder-add-text builder "orange")
      (content-list-builder-add-text builder ")")
      (define actual (build-content-list builder))

      (define expected
        (content-list
         (list
          (content-line 0 "(fruit-list")
          (content-line 1 "apple")
          (content-line 1 "banana")
          (content-line 1 "orange)"))))
      (check-equal? actual expected))

    (test-case "content-list-builder-add-subcontent"
      (define builder (make-content-list-builder))
      (define lb (linebreak #:before-break "" #:after-break "" #:indentation 1 #:line-quantity 1))
      (define subcontent
        (content-list
         (list
          (content-line 0 "(fruit-list")
          (content-line 1 "apple")
          (content-line 1 "banana")
          (content-line 1 "orange)"))))

      (content-list-builder-add-text builder "(")
      (content-list-builder-add-text builder "multi-fruit-list")
      (content-list-builder-add-linebreak builder lb)
      (content-list-builder-add-subcontent builder subcontent)
      (content-list-builder-add-linebreak builder lb)
      (content-list-builder-add-subcontent builder subcontent)
      (content-list-builder-add-linebreak builder lb)
      (content-list-builder-add-subcontent builder subcontent)
      (content-list-builder-add-text builder ")")
      (define actual (build-content-list builder))

      (define expected
        (content-list
         (list
          (content-line 0 "(multi-fruit-list")
          (content-line 1 "(fruit-list")
          (content-line 2 "apple")
          (content-line 2 "banana")
          (content-line 2 "orange)")
          (content-line 1 "(fruit-list")
          (content-line 2 "apple")
          (content-line 2 "banana")
          (content-line 2 "orange)")
          (content-line 1 "(fruit-list")
          (content-line 2 "apple")
          (content-line 2 "banana")
          (content-line 2 "orange))"))))
      (check-equal? actual expected))))
