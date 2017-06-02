#lang racket/base

(require racket/contract
         racket/match
         )

(module+ test
  (require rackunit
           (submod "..")))

(provide number->roman
         roman->number
         current-roman-numeral-case
         )

(define/contract current-roman-numeral-case
  (parameter/c (or/c 'lower 'upper))
  (make-parameter 'lower))

(define/contract (number->roman n)
  (-> (and/c natural-number/c (not/c 0))
      (and/c string? #rx"(?i:[mdclxvi]+)"))
  ;; based on http://rosettacode.org/wiki/Roman_numerals/Encode#Racket
  ((case (current-roman-numeral-case)
     [(lower) values]
     [(upper) string-upcase])
   (let number->roman ([n n])
     (cond [(>= n 1000)
            (string-append "m" (number->roman (- n 1000)))]
           [(>= n 900)
            (string-append "cm" (number->roman (- n 900)))]
           [(>= n 500)
            (string-append "d" (number->roman (- n 500)))]
           [(>= n 400)
            (string-append "cd" (number->roman (- n 400)))]
           [(>= n 100)
            (string-append "c" (number->roman (- n 100)))]
           [(>= n 90)
            (string-append "xc" (number->roman (- n 90)))]
           [(>= n 50)
            (string-append "l" (number->roman (- n 50)))]
           [(>= n 40)
            (string-append "xl" (number->roman (- n 40)))]
           [(>= n 10)
            (string-append "x" (number->roman (- n 10)))]
           [(>= n 9)
            (string-append "ix" (number->roman (- n 9)))]
           [(>= n 5)
            (string-append "v" (number->roman (- n 5)))]
           [(>= n 4)
            (string-append "iv" (number->roman (- n 4)))]
           [(>= n 1)
            (string-append "i" (number->roman (- n 1)))]
           [else
            ""]))))

(module+ test
  (check-equal? (number->roman 2008)
                "mmviii")
  (check-equal? (number->roman 1990)
                "mcmxc")
  (check-equal? (number->roman 1666)
                "mdclxvi")
  (parameterize ([current-roman-numeral-case 'upper])
    (check-equal? (number->roman 2008)
                  "MMVIII")
    (check-equal? (number->roman 1990)
                  "MCMXC")
    (check-equal? (number->roman 1666)
                  "MDCLXVI")))
    

(struct letter (value count)
  #:prefab)

(struct subtractive (prefix base)
  #:prefab)

(define (char->value char)
  (case char
    [(#\m) 1000]
    [(#\d) 500]
    [(#\c) 100]
    [(#\l) 50]
    [(#\x) 10]
    [(#\v) 5]
    [(#\i) 1]))

(define/contract (roman->number str)
  (-> (and/c string? #rx"(?i:[mdclxvi]+)")
      (and/c natural-number/c (not/c 0)))
  (for/sum
      ([this
        (in-list
         (let group-subtraction
           ([prev-value +inf.0]
            [to-go
             (let ([to-go (string->list
                           (string-downcase str))])
               (let group-letters ([current-char (car to-go)]
                                   [count-so-far 1]
                                   [to-go (cdr to-go)])
                 (match to-go
                   ['()
                    (cons (letter (char->value current-char)
                                  count-so-far)
                          null)]
                   [(cons (? (λ (cf) (eqv? cf current-char)))
                          more)
                    (group-letters current-char
                                   (add1 count-so-far)
                                   more)]
                   [(cons new-char more)
                    (cons (letter (char->value current-char)
                                  count-so-far)
                          (group-letters new-char
                                         1
                                         more))])))])
           (match to-go
             ['() '()]
             [(cons (letter val _) _)
              #:when (val . > . prev-value)
              (raise-arguments-error
               'roman->number
               "malformed Roman numeral string"
               "given" str)]
             [(list-rest (and prefix (letter prefix-val _))
                         (and base (letter base-val _))
                         more)
              #:when (prefix-val . < . base-val)
              (cons (subtractive prefix base)
                    (group-subtraction base-val more))]
             [(cons (and this (letter val _))
                    more)
              (cons this
                    (group-subtraction val more))])))])
    (match this
      [(letter val count)
       (* val count)]
      [(subtractive  (letter prefix-val prefix-count)
                     (letter base-val base-count))
       (- (* base-val base-count)
          (* prefix-val prefix-count))])))

(module+ test
  ;examples from Wikipedia
  (check-equal? (roman->number "mcmiv")
                1904)
  (check-equal? (roman->number "mcMlIv")
                1954)
  (check-equal? (roman->number "MCMXC")
                1990)
  (check-equal? (roman->number "mmxiv")
                2014)
  (check-equal? (roman->number "xxii")
                22)
  ;"Alternative forms"
  (check-equal? (roman->number "iiii")
                4)
  (check-equal? (roman->number "viiii")
                9)
  (check-equal? (roman->number "xiix")
                18)
  (check-equal? (roman->number "iixx")
                18)
  (check-equal? (roman->number "xviii")
                18)
  (check-equal? (roman->number "MDCCCCX")
                1910)
  (check-equal? (roman->number "MCMX")
                1910)
  (check-equal? (roman->number "MDCDIII")
                1903)
  (check-equal? (roman->number "MCMIII")
                1903))
                                