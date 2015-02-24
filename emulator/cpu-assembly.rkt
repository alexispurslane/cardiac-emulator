#lang racket
(require "memory-map.rkt")
(provide
 run-cardiac-assembly
 cpu-encode
 cpu-execute
 example-code)

(define (exactly v)
  v)
(define example-code (string-join '(
				    "inp 7"
				    "hrs -1"
				    ) "\n"))

(define-syntax match-lambda
  (syntax-rules ()
    [(_ (el ...) (args ...) body ...) (lambda (e args ...)
					(apply (lambda (el ...)
						 body ...) e))]))
					; Main function

(define (run-cardiac-assembly start-mem code [input 0] [output 0] [acc 0] [memory-map-no-code (create-memory-map start-mem)])
  (define memory-map (change-memory-map memory-map-no-code (map (lambda (e i)
								  `(,e ,(+ start-mem i)))
								(flatten (cpu-encode code))
								(build-list (length (flatten (cpu-encode code))) values))))
  (cpu-execute (length (cpu-encode code)) memory-map input output acc))

(define (cpu-encode code)
  (define real-code (map (lambda (e)
			   (string-split e " "))
			 (string-split code "\n")))
  (map (lambda (instruction)
	 (cons (match (first instruction)
		 ["inp" 0]
		 ["cla" 1]
		 ["add" 2]
		 ["tac" 3]
		 ["sft" 4]
		 ["out" 5]
		 ["sto" 6]
		 ["sub" 7]
		 ["jmp" 8]
		 ["hrs" 9]
		 ["nop" -1]) `(,(string->number (second instruction))))) real-code))
(define (split-by lst n)
  (if (not (empty? lst))
      (cons (take lst n) (split-by (drop lst n) n))
      '()))
(define (cpu-execute end-pc memory-map-start input-slot output-slot-start acc-start)
  (define begin-pc (memory-access memory-map-start 0))
  (define memory-map-code-split (split-by (take (drop memory-map-start 1) 98) 2))
  (define code (filter exactly (map (lambda (e i)
				      (if (and (not (= (first e) -1)) (>= i (sub1 begin-pc)))
					  e
					  #f))
		    memory-map-code-split
		    (build-list (length memory-map-code-split) values))))
  (displayln code)
  (foldl (lambda (instruction i prev-state)
	   (define pc (+ begin-pc i))
	   (define memory-map (or (first prev-state) memory-map-start))
	   (define acc (or (second prev-state) acc-start))
	   (define output-slot (or (third prev-state) output-slot-start))
	   (match instruction
	     [`(0 ,mloc)
	      (list (change-memory-map memory-map `((,input-slot ,mloc))) acc output-slot)]
	     [`(1 ,mloc)
	      (list memory-map (memory-access memory-map mloc) output-slot)]
	     [`(2 ,mloc)
	      (list memory-map (+ acc (memory-access memory-map mloc)) output-slot)]
	     [`(3 ,num) (if (negative? acc)
			    (cpu-execute memory-map input-slot output-slot acc (+ pc num))
			    (list memory-map acc output-slot))]
	     [`(4 ,num)  (list memory-map (arithmetic-shift num acc) output-slot)]
	     [`(5 ,mloc) (list memory-map acc (memory-access memory-map mloc))]
	     [`(6 ,mloc) (list (change-memory-map memory-map `((,acc ,mloc))) acc output-slot)]
	     [`(7 ,mloc) (list memory-map (- acc (memory-access memory-map mloc)) output-slot)]
	     [`(8 ,num) (cpu-execute (remove (sub1 num) code) memory-map input-slot output-slot acc i)]
	     [else (list memory-map acc output-slot)])) '(#f #f #f) code (build-list (length code) values)))


