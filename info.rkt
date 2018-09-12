#lang info
(define collection "roman-numeral")
(define deps '(("base" #:version "6.4")
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/roman-numeral.scrbl" ())))
(define pkg-desc "Utilities for parsing and writing Roman numerals")
(define version "0.0")
(define pkg-authors '(philip))
