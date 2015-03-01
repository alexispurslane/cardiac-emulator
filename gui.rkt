#lang racket/gui
(require "emulator/cpu-assembly.rkt")
(require "emulator/lib.rkt")
(require "emulator/memory-map.rkt")
(define COLS 10)

(define (run-assembly start-mem code [input 0] [output 0] [acc 0])
  					; Make a frame by instantiating the frame% class
  (define frame (new frame%
		     [label  "Memory Preview"]
		     [width  900]
		     [height 900]))
					; Show the frame by calling its show method
  (define canvas (new canvas% [parent frame]))
  (define dc (send canvas get-dc))
  (send frame show #t)
  (define res (run-cardiac-assembly
	       start-mem
	       code
	       input
	       output
	       acc
	       #:listener ; ->
	       (lambda (mem)
		 (for-each (lambda (e i)
			     (define colors (map (lambda (e)
						   (* (or e 0) 10))
						 (get-digits (data-data e))))
			     (cond
			      [(= (length colors) 2)
			       (send dc set-brush (apply make-object
							 (cons color% (cons 0 colors))) 'solid)]
			      [(= (length colors) 1)
			       (send dc set-brush (apply make-object
							 (cons color% (cons 0 (cons 0 colors)))) 'solid)]
			      [(= (length colors) 3)
			       (send dc set-brush (apply make-object
							 (cons color% colors)) 'solid)])
			     (send dc draw-rectangle
				   (* (modulo i COLS) 50)
				   (* 50 (floor (/ i COLS)))
				   50 50)
			     (send dc set-text-mode 'solid)
			     (send dc set-text-background "white")
			     (send dc set-text-foreground "black")
			     (send dc draw-text (cond
						 [(and (number? (data-data e)) (<= 0 (data-data e)) (data-code? e))
						  (decode-single-inst (data-data e))]
						 [(string? (data-data e)) (decode-single-inst (data-data e))]
						 [else (number->string (data-data e))])
				   (+ (* (modulo i COLS) 50) 6)
				   (+ (* 50 (floor (/ i COLS))) 20))) mem (build-list (length mem) values)))))
  res)
