#lang racket/base


(require racket/format
         racket/list
         racket/match
         racket/sequence
         racket/set
         racket/string
         racket/syntax
         rebellion/collection/vector
         rebellion/collection/vector/builder
         rebellion/private/guarded-block
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/enum
         rebellion/type/record
         syntax/parse)


(module+ test
  (require rackunit))


;; This is the tree structure that syntax objects should be turned into in order to render them.
(struct content-tree () #:transparent)

;; A node has children and a sequence of possible ways (punctuation specs) to combine them into a
;; single string. The list of specs is in priority order: the first one should be tried before others
;; are tried.
(struct content-node (children punctuation-options) #:transparent)

;; A leaf is just a (single line!) string.
(struct content-leaf (text) #:transparent)


;; This is a string that's broken across (possibly multiple) lines.
(define-record-type linebreak (before-break after-break indentation))

;; Each field in punctuation-spec can be either a string (without any line breaks in it) *or* a
;; (linebreak ...) object.
(define-record-type punctuation-spec
  (;; Content that is *always* inserted at the start of the node, even if it's empty
   header

   ;; Content that is inserted between the header and the first child. Ignored if the node is empty.
   header-separator

   ;; Overrides the separator between the first child and the second child. Does nothing if the node
   ;; has less than two children.
   first-separator

   ;; Content that is inserted between children. Does nothing if the node has less than two children.
   separator
   
   ;; Overrides the separator between the second-to-last child and the last child. Does nothing if the
   ;; node has less than two children.
   last-separator

   ;; Content that is inserted between the last child and the trailer. Ignored if the node is empty.
   trailer-separator

   ;; Content that is *always* inserted at the start of this node, even if it's empty
   trailer))


(define one-line-s-expression-punctuation-spec
  (punctuation-spec #:header "("
                     #:header-separator ""
                     #:first-separator " "
                     #:separator " "
                     #:last-separator " "
                     #:trailer-separator ""
                     #:trailer ")"))


(define racket-default-s-expression-punctuation-options
  (vector-immutable
   one-line-s-expression-punctuation-spec
   ;; Function name on first line, all arguments on next line
   (punctuation-spec #:header "("
                     #:header-separator ""
                     #:first-separator (linebreak #:before-break "" #:after-break "" #:indentation 1)
                     #:separator " "
                     #:last-separator " "
                     #:trailer-separator ""
                     #:trailer ")")
   ;; Function name and arguments all on separate lines
   (punctuation-spec #:header "("
                     #:header-separator ""
                     #:first-separator (linebreak #:before-break "" #:after-break "" #:indentation 1)
                     #:separator (linebreak #:before-break "" #:after-break "" #:indentation 1)
                     #:last-separator (linebreak #:before-break "" #:after-break "" #:indentation 1)
                     #:trailer-separator ""
                     #:trailer ")")))


(define racket-begin-like-keyword-punctuation-options
  (vector-immutable
   ;; All on separate lines (preferred)
   (punctuation-spec #:header "("
                     #:header-separator ""
                     #:first-separator (linebreak #:before-break "" #:after-break "" #:indentation 2)
                     #:separator (linebreak #:before-break "" #:after-break "" #:indentation 2)
                     #:last-separator (linebreak #:before-break "" #:after-break "" #:indentation 2)
                     #:trailer-separator ""
                     #:trailer ")")
   ;; All on same line (less common but still valid)
   one-line-s-expression-punctuation-spec))


(define racket-define-like-keyword-punctuation-options
  (vector-immutable
   ;; Everything on one line
   one-line-s-expression-punctuation-spec
   ;; Definition keyword and header on same line, remaining forms on separate lines
   (punctuation-spec #:header "("
                     #:header-separator ""
                     #:first-separator " "
                     #:separator (linebreak #:before-break "" #:after-break "" #:indentation 2)
                     #:last-separator (linebreak #:before-break "" #:after-break "" #:indentation 2)
                     #:trailer-separator ""
                     #:trailer ")")))


(define racket-lambda-like-keyword-punctuation-options
  (vector-immutable
   ;; Everything on one line
   one-line-s-expression-punctuation-spec
   ;; Lambda keyword and header on same line, remaining forms on separate lines
   (punctuation-spec #:header "("
                     #:header-separator ""
                     #:first-separator " "
                     #:separator (linebreak #:before-break "" #:after-break "" #:indentation 2)
                     #:last-separator (linebreak #:before-break "" #:after-break "" #:indentation 2)
                     #:trailer-separator ""
                     #:trailer ")")
   ;; All forms on separate lines, header indented further than body forms
   (punctuation-spec #:header "("
                     #:header-separator ""
                     #:first-separator (linebreak #:before-break "" #:after-break "" #:indentation 4)
                     #:separator (linebreak #:before-break "" #:after-break "" #:indentation 2)
                     #:last-separator (linebreak #:before-break "" #:after-break "" #:indentation 2)
                     #:trailer-separator ""
                     #:trailer ")")))


;; Can't express for/fold-like punctuation in this system directly.
;; Maybe if instead of a separator, first separator, and last separator, we had a separator, a list
;; of separator overrides starting from the beginning, and a list of separator overrides starting
;; from the end. That would let for/fold override the first 2 separators to have more indentation,
;; not just the first separator.


;; There's no way to specify relative indentation yet. Maybe if the linebreak struct specified whether
;; indentation was relative to the previous child or relative to the enclosing node's start. Also
;; there isn't a way to "softly bypass" the parent node for a node up the chain. For instance, in this
;; code:
;;
;; return foo(
;;   x,
;;   y)
;;
;; ...if the syntax tree looks like (return (foo x y)), there's no way for the (foo x y) node to state
;; that its arguments should be indented relative to the start of the (return ...) node, not the
;; (foo ...) node. So we'd only be able to indent it like this:
;;
;; return foo(
;;          x,
;;          y)
;;
;; Or like this:
;;
;; return
;;   foo(
;;     x,
;;     y)
;;
;; Maybe we could have a way for the (return ...) node to state that it should "absorb"
;; children-of-children for indentation purposes?


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


(define (syntax-subcomponents stx)
  (syntax-parse stx
    [(sub ...) (syntax->list #'(sub ...))]
    [(sub ...+ . tail) (syntax->list #'(sub ... tail))]
    [_ #false]))


(define/guard (syntax-render-oneline stx)
  (define subcomponents (syntax-subcomponents stx))

  (guard subcomponents else
    (define datum (syntax-e stx))
    (if (and (symbol? datum) (syntax-property stx 'unescaped?))
        (symbol->string datum)
        (~s datum)))

  (define subcomponent-count (length subcomponents))
  (guard (syntax-oneline-allowed? stx subcomponent-count) else
    #false)

  (define rendered-subs
    (let loop ([builder (make-vector-builder)] [subcomponents subcomponents])
      (guarded-block
       (guard-match (cons head tail) subcomponents else
         (build-vector builder))
       (guard (syntax-property head 'implicit?) then
         (loop builder tail))
       (define next (syntax-render-oneline head))
       (and next (loop (vector-builder-add builder next) tail)))))

  (guard rendered-subs else
    #false)

  (define header (or (syntax-property stx 'header) "("))
  (define leading-separator (or (syntax-property stx 'leading-separator) ""))
  (define separator (or (syntax-property stx 'separator) " "))
  (define first-separator (or (syntax-property stx 'first-separator) separator))
  (define last-separator (or (syntax-property stx 'last-separator) separator))
  (define trailing-separator (or (syntax-property stx 'trailing-separator) ""))
  (define trailer (or (syntax-property stx 'trailer) ")"))
  (strings-insert-between rendered-subs
                          #:header header
                          #:leading-separator leading-separator
                          #:first-separator first-separator
                          #:separator separator
                          #:last-separator last-separator
                          #:trailing-separator trailing-separator
                          #:trailer trailer))


(define (syntax-oneline-allowed? stx subcomponent-count)
  (define mode-list
    (append
     (list (syntax-property stx 'header-linebreak-mode)
           (syntax-property stx 'trailer-linebreak-mode))
     (if (>= subcomponent-count 1)
         (list (syntax-property stx 'leading-separator-linebreak-mode)
               (syntax-property stx 'trailing-separator-linebreak-mode))
         (list))
     (if (= subcomponent-count 2)
         (list (or (syntax-property stx 'first-separator-linebreak-mode)
                   (syntax-property stx 'last-separator-linebreak-mode)
                   (syntax-property stx 'separator-linebreak-mode)))
         (list))
     (if (= subcomponent-count 3)
         (list (or (syntax-property stx 'first-separator-linebreak-mode)
                   (syntax-property stx 'separator-linebreak-mode))
               (or (syntax-property stx 'last-separator-linebreak-mode)
                   (syntax-property stx 'separator-linebreak-mode)))
         (list))
     (if (>= subcomponent-count 3)
         (list (syntax-property stx 'separator-linebreak-mode)
               (or (syntax-property stx 'first-separator-linebreak-mode)
                   (syntax-property stx 'separator-linebreak-mode))
               (or (syntax-property stx 'last-separator-linebreak-mode)
                   (syntax-property stx 'separator-linebreak-mode)))
         (list))))
  (define modes (set-remove (list->set mode-list) #false))
  (subset? modes (set 'allowed 'discouraged 'disallowed)))


(define/guard (syntax-render-multiline stx)
  (define subcomponents (syntax-subcomponents stx))

  (guard subcomponents else
    (define datum (syntax-e stx))
    (if (and (symbol? datum) (syntax-property stx 'unescaped?))
        (symbol->string datum)
        (~s datum)))
     
  (define header
    (or (syntax-property stx 'multiline-header)
        (syntax-property stx 'header)
        "("))
  (define leading-separator
    (or (syntax-property stx 'multiline-leading-separator)
        (syntax-property stx 'leading-separator)
        ""))
  (define separator
    (or (syntax-property stx 'multiline-separator)
        (syntax-property stx 'separator)
        " "))
  (define first-separator
    (or (syntax-property stx 'multiline-first-separator)
        (syntax-property stx 'first-separator)
        separator))
  (define last-separator
    (or (syntax-property stx 'multiline-last-separator)
        (syntax-property stx 'last-separator)
        separator))
  (define trailing-separator
    (or (syntax-property stx 'multiline-trailing-separator)
        (syntax-property stx 'trailing-separator)
        ""))
  (define trailer
    (or (syntax-property stx 'multiline-trailer)
        (syntax-property stx 'trailer)
        ")"))
  (define rendered-subs
    (for*/vector ([sub-stx (in-list subcomponents)]
                  #:unless (syntax-property sub-stx 'implicit?))
      (syntax-render-preprocessed sub-stx)))
  (strings-insert-between rendered-subs
                          #:header header
                          #:leading-separator leading-separator
                          #:first-separator first-separator
                          #:separator separator
                          #:last-separator last-separator
                          #:trailing-separator trailing-separator
                          #:trailer trailer))


(define (syntax-render-preprocessed stx)
  (or (syntax-render-oneline stx) (syntax-render-multiline stx)))


(define (syntax-render stx)
  (displayln stx)
  (syntax-render-preprocessed (syntax-preprocess stx)))


(define/guard (syntax-preprocess-paren-shape stx)
  (define shape (syntax-property stx 'paren-shape))
  (guard shape else
    stx)
  (define header (match shape [#\[ "["] [#\{ "{"]))
  (define trailer (match shape [#\[ "]"] [#\{ "}"]))
  (syntax-property (syntax-property stx 'header header) 'trailer trailer))


(define (syntax-preprocess stx)
  (syntax-parse stx
    #:datum-literals (quote quasiquote unquote unquote-splicing)
    [((~and sub0 quote) sub)
     (let* ([stx #`(#,(syntax-property #'sub0 'implicit? #true) #,(syntax-preprocess #'sub))]
            [stx (syntax-property stx 'header "'")]
            [stx (syntax-property stx 'trailer "")])
       stx)]
    [((~and sub0 quasiquote) sub)
     (let* ([stx #`(#,(syntax-property #'sub0 'implicit? #true) #,(syntax-preprocess #'sub))]
            [stx (syntax-property stx 'header "`")]
            [stx (syntax-property stx 'trailer "")])
       stx)]
    [((~and sub0 unquote) sub)
     (let* ([stx #`(#,(syntax-property #'sub0 'implicit? #true) #,(syntax-preprocess #'sub))]
            [stx (syntax-property stx 'header ",")]
            [stx (syntax-property stx 'trailer "")])
       stx)]
    [((~and sub0 unquote-splicing) sub)
     (let* ([stx #`(#,(syntax-property #'sub0 'implicit? #true) #,(syntax-preprocess #'sub))]
            [stx (syntax-property stx 'header "@,")]
            [stx (syntax-property stx 'trailer "")])
       stx)]
    [(sub ...)
     (define preprocessed-subs
       (for/list ([sub-stx (in-syntax #'(sub ...))])
         (syntax-preprocess sub-stx)))
     (syntax-preprocess-paren-shape (datum->syntax stx preprocessed-subs stx stx))]
    [(sub ...+ . tail)
     (define preprocessed-subs
       (for/list ([sub-stx (in-syntax #'(sub ...))])
         (syntax-preprocess sub-stx)))
     (define preprocessed-subs+tail
       (append* preprocessed-subs (list (syntax-preprocess #'tail))))
     (let* ([stx (datum->syntax stx preprocessed-subs+tail stx stx)]
            [stx (syntax-preprocess-paren-shape stx)])
       (syntax-property stx 'last-separator " . "))]
    [other stx]))


(define (syntax-add-property stx . k+vs)
  (for/fold ([stx stx]) ([k+v (in-slice 2 k+vs)])
    (match-define (list k v) k+v)
    (syntax-property stx k v)))


(define (implicitly-tagged tag-name forms)
  (define/with-syntax tag (syntax-add-property #`#,tag-name 'implicit? #true))
  #`(tag #,@forms))


(define (j-expression.inline . forms)
  (syntax-add-property (implicitly-tagged 'expression forms)
                       'header ""
                       'separator ""
                       'trailer ""))


(define (j-expression.block . forms)
  (syntax-add-property (implicitly-tagged 'expression forms)
                       'header ""
                       'separator " "
                       'trailer ""))


(define (j-chain-link . forms)
  (syntax-add-property (implicitly-tagged 'chain-link forms)
                       'header ""
                       'separator ""
                       'trailer ""))


(define (j-chainer form)
  (syntax-add-property (implicitly-tagged 'chainer (list form))
                       'header ""
                       'trailer ""))


(define (j-indices . forms)
  (syntax-add-property (implicitly-tagged 'indices forms)
                       'header "["
                       'separator ", "
                       'trailer "]"))


(define (j-parameters . forms)
  (syntax-add-property (implicitly-tagged 'parameters forms)
                       'header "("
                       'separator ", "
                       'trailer ")"))


(define j-dot
  (syntax-add-property #'|.| 'unescaped? #true))


(define (j-block . forms)
  (syntax-add-property (implicitly-tagged 'block forms)
                       'header "{"
                       'leading-separator " "
                       'separator " "
                       'trailing-separator " "
                       'trailer "}"
                       'multiline-leading-separator "\n  "
                       'multiline-separator "\n  "
                       'multiline-trailing-separator "\n"
                       'leading-linebreak-mode 'preferred
                       'separator-linebreak-mode 'preferred
                       'trailing-linebreak-mode 'preferred))


(define (j-expression.inline-statement . forms)
  (syntax-add-property (implicitly-tagged 'expression forms)
                       'header ""
                       'separator ""
                       'trailer ";"))


(displayln
 (syntax-render
  (j-expression.inline
   #'baz
   (j-chain-link (j-chainer j-dot) #'foo (j-indices #'x #'y) (j-parameters #'a #'b))
   (j-chain-link (j-chainer j-dot) #'bar (j-parameters)))))


(displayln
 (syntax-render
  (j-block
   (j-expression.inline-statement #'baz)
   (j-expression.inline-statement #'foo (j-indices #'x #'y))
   (j-expression.inline-statement #'bar (j-parameters #'a #'b))
   (j-expression.block #'baz (j-block)))))



; baz|.foo[|x, |y](|a, |b)|.bar()
