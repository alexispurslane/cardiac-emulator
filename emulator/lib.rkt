#lang racket
(provide
 exactly
 get-digits
 concat-digits)

(define (exactly v)
  v)

(define (get-digits n)
  (or (map string->number (map string (string->list (number->string n)))) 0))

(define (concat-digits lst)
  (or (string->number (apply string-append (map number->string lst))) 0))
