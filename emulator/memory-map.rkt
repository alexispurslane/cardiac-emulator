#lang racket
(provide
 memory-access
 create-memory-map
 (struct-out data)
 change-memory-map)

(struct data (data code?) #:transparent)

(define (create-memory-map [pc 1] [template (build-list 98 (lambda (e) (data -1 #f)))])
  (define start (cons (data pc #f) template))
  (append start `(,(data 800 #f))))

(define (change-memory-map memory-map changes [code? #f])
  (map (lambda (e i)
	 (cond
	  [(not (null? (filter (lambda (e)
				 (= (second e) i)) changes)))
	   (data (first (first (filter (lambda (e)
				   (= (second e) i)) changes))) code?)]
	  [else e])) memory-map (build-list (length memory-map) values)))

(define (memory-access memory-map idx [acc 0] [pc (memory-access 0 memory-map)])
  (match idx
    ["acc"       acc]
    ["pc"        pc]
    [(? number?) (data-data (list-ref memory-map idx))]))
