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
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/enum
         rebellion/type/record
         syntax/parse
         syntax-render/private/content-list
         syntax-render/private/insert-between)


(module+ test
  (require rackunit))


;@----------------------------------------------------------------------------------------------------


;; A leaf is just a (single line!) string.
(struct content-leaf (text) #:transparent)


;; A node has children and a sequence of possible ways (punctuation specs) to combine them into a
;; single string. The list of specs is in priority order: the first one should be tried before others
;; are tried.
(struct content-node (children punctuation-options)
  #:transparent
  #:guard
  (λ (children punctuation-options _)
    (values (sequence->vector children) (sequence->vector punctuation-options))))


(struct fixed-content-node (children punctuation)
  #:transparent
  #:guard (λ (children punctuation _) (values (sequence->vector children) punctuation)))


(define (fixed-content #:punctuation spec . children)
  (fixed-content-node children spec))
        

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
   (punctuation-spec
    #:header "("
    #:header-separator ""
    #:first-separator (linebreak #:before-break "" #:after-break "" #:indentation 1 #:line-quantity 1)
    #:separator " "
    #:last-separator " "
    #:trailer-separator ""
    #:trailer ")")
   ;; Function name and arguments all on separate lines
   (punctuation-spec
    #:header "("
    #:header-separator ""
    #:first-separator (linebreak #:before-break "" #:after-break "" #:indentation 1 #:line-quantity 1)
    #:separator (linebreak #:before-break "" #:after-break "" #:indentation 1 #:line-quantity 1)
    #:last-separator (linebreak #:before-break "" #:after-break "" #:indentation 1 #:line-quantity 1)
    #:trailer-separator ""
    #:trailer ")")
   ;; Can't yet support formatting where function and first argument are on one line and remaining
   ;; arguments are each on separate lines indented to align with the first argument
   ))


(define racket-begin-like-keyword-punctuation-options
  (vector-immutable
   ;; All on separate lines (preferred)
   (punctuation-spec
    #:header "("
    #:header-separator ""
    #:first-separator (linebreak #:before-break "" #:after-break "" #:indentation 2 #:line-quantity 1)
    #:separator (linebreak #:before-break "" #:after-break "" #:indentation 2 #:line-quantity 1)
    #:last-separator (linebreak #:before-break "" #:after-break "" #:indentation 2 #:line-quantity 1)
    #:trailer-separator ""
    #:trailer ")")
   ;; All on same line (less common but still valid)
   one-line-s-expression-punctuation-spec))


(define racket-define-like-keyword-punctuation-options
  (vector-immutable
   ;; Everything on one line
   one-line-s-expression-punctuation-spec
   ;; Definition keyword and header on same line, remaining forms on separate lines
   (punctuation-spec
    #:header "("
    #:header-separator ""
    #:first-separator " "
    #:separator (linebreak #:before-break "" #:after-break "" #:indentation 2 #:line-quantity 1)
    #:last-separator (linebreak #:before-break "" #:after-break "" #:indentation 2 #:line-quantity 1)
    #:trailer-separator ""
    #:trailer ")")))


(define racket-lambda-like-keyword-punctuation-options
  (vector-immutable
   ;; Everything on one line
   one-line-s-expression-punctuation-spec
   ;; Lambda keyword and header on same line, remaining forms on separate lines
   (punctuation-spec
    #:header "("
    #:header-separator ""
    #:first-separator " "
    #:separator (linebreak #:before-break "" #:after-break "" #:indentation 2 #:line-quantity 1)
    #:last-separator (linebreak #:before-break "" #:after-break "" #:indentation 2 #:line-quantity 1)
    #:trailer-separator ""
    #:trailer ")")
   ;; All forms on separate lines, header indented further than body forms
   (punctuation-spec
    #:header "("
    #:header-separator ""
    #:first-separator (linebreak #:before-break "" #:after-break "" #:indentation 4 #:line-quantity 1)
    #:separator (linebreak #:before-break "" #:after-break "" #:indentation 2 #:line-quantity 1)
    #:last-separator (linebreak #:before-break "" #:after-break "" #:indentation 2 #:line-quantity 1)
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


(define (content-tree-choose-layout tree #:column-count [column-count 102])
  (match tree
    [(? content-leaf?) tree]
    [(content-node children punctuation-options)
     (define last-spec-index (sub1 (vector-length punctuation-options)))
     (define chosen-spec
       (for/first
           ([spec (in-vector punctuation-options 0 last-spec-index)]
            #:when
            (for/and
                ([child (in-vector children)]
                 [child-column-count
                  (in-punctuation-child-start-column-counts spec #:column-count column-count)])
              (content-tree-outermost-spec-satisfiable?
               child spec #:column-count child-column-count)))
         spec))
     (cond
       [chosen-spec
        (define modified-children
          (for/vector ([child (in-vector children)])
            (content-tree-force-primary-layout child)))
        (content-node modified-children (vector chosen-spec))]
       [else
        (define chosen-spec (vector-ref punctuation-options last-spec-index))
        (define modified-children
          (for/vector
              ([child (in-vector children)]
               [column-count
                (in-punctuation-child-start-column-counts chosen-spec #:column-count column-count)])
            (content-tree-choose-layout child #:column-count column-count)))
        (content-node modified-children (vector chosen-spec))])]))


(define (content-tree-outermost-spec-satisfiable? tree #:column-count column-count)
  (match tree
    [(content-leaf text) (<= (string-length text) column-count)]
    [(content-node children punctuation-options)
     #false]))


(define (content-tree-force-primary-layout tree)
  tree)


(define (in-punctuation-child-start-column-counts
         spec #:column-count column-count #:child-count child-count)
  '())





(define (content-tree-try-rendering-children children spec #:column-count column-count)
  #false)


(define (content-tree-render-children children spec #:column-count column-count)
  
  "")


(define (fixed-content-tree-render tree)
  (define builder (make-content-list-builder))
  (match tree
    [(content-leaf text)
     (content-list-builder-add builder text)
     (build-content-list builder)]
    [(fixed-content-node children punctuation)
     (match-define
       (punctuation-spec
        #:header header
        #:header-separator header-separator
        #:first-separator first-separator
        #:separator separator
        #:last-separator last-separator
        #:trailer-separator trailer-separator
        #:trailer trailer)
       punctuation)
     (content-list-builder-add builder header)

     (define (first? i)
       (equal? i 0))

     (define (second? i)
       (equal? i 1))

     (define (last? i)
       (equal? i (sub1 (vector-length children))))

     (for ([child (in-vector children)]
           [i (in-naturals)])
       (cond
         [(first? i)
          (content-list-builder-add builder header-separator)]
         [(second? i)
          (content-list-builder-add builder (or first-separator separator))]
         [(last? i)
          (content-list-builder-add builder (or last-separator separator))]
         [else
          (content-list-builder-add builder separator)])
       (content-list-builder-add-subcontent builder (fixed-content-tree-render child))
       (when (last? i)
         (content-list-builder-add builder trailer-separator)))

     (content-list-builder-add builder trailer)
     (build-content-list builder)]))


(module+ test
  (test-case (name-string fixed-content-tree-render)

    (test-case "single leaf"
      (define tree (content-leaf "foo"))
      (define expected (content-list (list (content-line 0 "foo"))))
      (check-equal? (fixed-content-tree-render tree) expected))

    (test-case "single expression, one-line"
      (define tree
        (fixed-content
         #:punctuation one-line-s-expression-punctuation-spec
         (content-leaf "foo")
         (content-leaf "x")
         (content-leaf "y")
         (content-leaf "z")))
      (define expected (content-list (list (content-line 0 "(foo x y z)"))))
      (check-equal? (fixed-content-tree-render tree) expected))

    (test-case "single expression, multi-line"
      (define spec
        (punctuation-spec
         #:header "("
         #:header-separator ""
         #:first-separator #false
         #:separator (linebreak #:before-break "" #:after-break "" #:indentation 1 #:line-quantity 1)
         #:last-separator #false
         #:trailer-separator ""
         #:trailer ")"))
      (define tree
        (fixed-content
         #:punctuation spec
         (content-leaf "foo")
         (content-leaf "x")
         (content-leaf "y")
         (content-leaf "z")))
      (define expected
        (content-list
         (list
          (content-line 0 "(foo")
          (content-line 1 "x")
          (content-line 1 "y")
          (content-line 1 "z)"))))
      (check-equal? (fixed-content-tree-render tree) expected))))


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
