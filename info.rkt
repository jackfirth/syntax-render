#lang info

(define collection "syntax-render")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list 'library)
              "syntax-render")))

(define deps
  (list "base"))

(define build-deps
  (list "racket-doc"
        "rackunit-lib"
        "scribble-lib"))
