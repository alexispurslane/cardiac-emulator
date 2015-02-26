#lang racket/gui
(require "emulator/cpu-assembly.rkt")
(require "emulator/lib.rkt")
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
  (define res (run-cardiac-assembly start-mem code input output acc #:listener (lambda (mem)
										 (sleep/yield 0.6)
										 (for-each (lambda (e i)
											     (define colors (map (lambda (e)
														   (* (or e 0) 10))
														 (get-digits e)))
											     (if (not (equal? colors '(10)))
												 (send dc set-brush (apply make-object
															   (cons color% (cons 0 colors))) 'solid)
												 (send dc set-brush (apply make-object
															   (cons color% (cons 0 (cons 0 colors)))) 'solid))
											     (displayln (round (/ i 18)))
											     (send dc draw-rectangle
												   (* (modulo i COLS) 50)
												   (* 50 (floor (/ i COLS)))
												   50 50)) mem (build-list (length mem) values)))))
  res)
