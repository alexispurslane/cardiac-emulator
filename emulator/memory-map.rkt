#lang racket
(provide
 memory-access
 create-memory-map
 change-memory-map)

(define (create-memory-map [template (build-list 98 (lambda (e) 0))])
  (define start (cons 001 template))
  (append start '(800)))

(define (change-memory-map memory-map changes)
  (map (lambda (e i)
	 (cond
	  [(not (null? (filter (lambda (e)
				 (= (second e) i)) changes)))
	   (first (first (filter (lambda (e)
				   (= (second e) i)) changes)))]
	  [else e])) memory-map (build-list (length memory-map) values)))

(define (memory-access memory-map idx)
  (list-ref memory-map idx))
