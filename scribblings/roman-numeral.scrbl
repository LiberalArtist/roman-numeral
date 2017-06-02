#lang scribble/manual

@title{Roman Numerals}
@author[(author+email @elem{Philip M@superscript{c}Grath}
                      "philip@philipmcgrath.com"
                      #:obfuscate? #t)]
@defmodule[roman-numeral]

@(require scribble/example
          (for-label roman-numeral
                     racket
                     ))

This library provides utilities for parsing
and writing Roman numerals.

@(define make-roman-eval
   (make-eval-factory '(roman-numeral)))

@defproc[(number->roman [number (and/c natural-number/c (not/c 0))])
         (and/c string? #rx"(?i:[mdclxvi]+)")]{
 Returns the Roman numeral representation of @racket[number]
 as a string. The string will be in lower case if
 @racket[(current-roman-numeral-case)] is @racket['lower]
 (the default) or in upper case otherwise.
 @examples[
 #:eval (make-roman-eval)
 (number->roman 1990)
 (number->roman 1666)
 (parameterize ([current-roman-numeral-case 'upper])
   (number->roman 1666))
 ]
}

@defparam[current-roman-numeral-case case
          (or/c 'lower 'upper)]{
 Specifies whether the string returned by @racket[number->roman]
 is upper case or lower case.
}

@defproc[(roman->number [numeral (and/c string? #rx"(?i:[mdclxvi]+)")])
         (and/c natural-number/c (not/c 0))]{
 Returns the number represented by the Roman numeral string
 @racket[neumeral].

 Roman numeral strings that do not use
 the standard subtractive forms are supported, but @racket[numeral]
 must be a valid Roman numeral, not simply an arbitrary sequence
 of the characters listed in the contract.
 @examples[
 #:eval (make-roman-eval)
 (roman->number "mcmiv")
 (code:line (roman->number "mcMlIv") (code:comment "case is ignored"))
 (code:comment "non-standard Roman numerals")
 (roman->number "iiii")
 (number->roman 4)
 (roman->number "xiix")
 (roman->number "iixx")
 (number->roman 18)
 (roman->number "MDCDIII")
 (number->roman 1903)
 (roman->number "MDCCCCX")
 (number->roman 1910)
 (code:comment "not a valid Roman numeral")
 (eval:error (roman->number "cdm"))
 ]
}


