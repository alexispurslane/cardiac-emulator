#lang racket
(provide
 exactly
 get-digits
 concat-digits)

(define (exactly v)
  v)

(define (get-digits n)
  (or (map string->number (map string (string->list (if (number? n)
							(number->string n)
							n)))) 0))

(define (concat-digits lst)
  (or (string->number (apply string-append (map (lambda (e)
						  (if (number? e)
						      (number->string e)
						      e)) lst))) 0))
