#lang racket
(provide
 exactly
 get-digits
 concat-digits)

(define (exactly v)
  v)

(define (get-digits n)
  (map string->number (map string (string->list (number->string n)))))
(define (concat-digits lst)
  (string->number (apply string-append (map number->string lst))))
