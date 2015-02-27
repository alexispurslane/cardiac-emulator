#lang racket
(require "memory-map.rkt")
(require "lib.rkt")
(provide
 run-cardiac-assembly
 cpu-encode
 cpu-decode
 decode-single-inst
 cpu-execute
 example-code
 data)


(define example-code (string-join '(
				    "inp 7"
				    "inp 8"
				    "inp 10"
				    "inp 97"
				    "add 97"
				    "sto 96"
				    ) "\n"))

					; Main function

(define (run-cardiac-assembly start-mem code [input 0] [output 0] [acc 0] [memory-map-no-code (create-memory-map start-mem)] #:listener [mem-change-callback (lambda (mem) (void))])
  (define memory-map (change-memory-map memory-map-no-code (map (lambda (e i)
								  `(,e ,(+ start-mem i)))
								(cpu-encode code)
								(build-list (length (cpu-encode code)) values)) #t))
  (cpu-execute (length (cpu-encode code)) memory-map input output acc mem-change-callback))

(define decode-single-inst (lambda (instr)
			     (define split-code (cons
						 (first (get-digits instr))
						 (list (concat-digits
							(rest (get-digits instr))))))
			     (match split-code
			       [`(1 ,n) (string-append "inp " (number->string n))]
			       [`(2 ,n) (string-append "cla " (number->string n))]
			       [`(3 ,n) (string-append "add " (number->string n))]
			       [`(4 ,n) (string-append "tac " (number->string n))]
			       [`(5 ,n) (string-append "sft " (number->string n))]
			       [`(6 ,n) (string-append "out " (number->string n))]
			       [`(7 ,n) (string-append "sto " (number->string n))]
			       [`(8 ,n) (string-append "sub " (number->string n))]
			       [`(9 ,n) (string-append "jmp " (number->string n))]
			       [`(10)   "hrs"])))

(define (cpu-decode code)
  (define diss-list (map decode-single-inst code))
  (string-join diss-list "\n"))

(define (cpu-encode code)
  (define real-code (map (lambda (e)
			   (string-split e " "))
			 (string-split code "\n")))
  (map (lambda (instruction)
	 (define command-code (match (first instruction)
				["inp" 1]
				["cla" 2]
				["add" 3]
				["tac" 4]
				["sft" 5]
				["out" 6]
				["sto" 7]
				["sub" 8]
				["jmp" 9]
				["hrs" 10]))
	 (cond
	  [(> (length instruction) 1)
	   (define command-arg `(,(string->number (second instruction))))
	   (concat-digits (cons command-code command-arg))]
	  [else
	   (concat-digits (list command-code))])) real-code))

(define (cpu-execute end-pc memory-map-start input-slot output-slot-start acc-start mem-change-callback)
  (define begin-pc (memory-access memory-map-start 0))
  (define memory-map-code-split (map (lambda (el)
				       (define e (data-data el))
				       (if (not (= e -1))
					   (list (first (get-digits e)) (concat-digits (rest (get-digits e))))
					   '(-1 -1))) (take (drop memory-map-start 1) 98)))
  
  (define code (filter exactly (map (lambda (e i)
				      (if (and (not (= (first e) -1)) (>= i (sub1 begin-pc)))
					  e
					  #f))
				    memory-map-code-split
				    (build-list (length memory-map-code-split) values))))
  (foldl (lambda (instruction i prev-state)
	   (define pc (+ begin-pc i))
	   (define memory-map (or (first prev-state) memory-map-start))
	   (define acc (or (second prev-state) acc-start))
	   (define output-slot (or (third prev-state) output-slot-start))
	   
	   (match instruction
	     [`(1 ,mloc)
	      (mem-change-callback (change-memory-map memory-map `((,input-slot ,mloc))))
	      (list (change-memory-map memory-map `((,input-slot ,mloc))) acc output-slot)]
	     [`(2 ,mloc)
	      (list memory-map (memory-access memory-map mloc) output-slot)]
	     [`(3 ,mloc)
	      (list memory-map (+ acc (memory-access memory-map mloc)) output-slot)]
	     [`(4 ,num) (if (negative? acc)
			    (cpu-execute memory-map input-slot output-slot acc (+ pc num))
			    (list memory-map acc output-slot))]
	     [`(5 ,num)  (list memory-map (arithmetic-shift num acc) output-slot)]
	     [`(6 ,mloc) (list memory-map acc (memory-access memory-map mloc))]
	     [`(7 ,mloc)
	      (mem-change-callback (change-memory-map memory-map `((,acc ,mloc))))
	      (list (change-memory-map memory-map `((,acc ,mloc))) acc output-slot)]
	     [`(8 ,mloc) (list memory-map (- acc (memory-access memory-map mloc)) output-slot)]
	     [`(9 ,num) (cpu-execute (remove (sub1 num) code) memory-map input-slot output-slot acc i)]
	     [else (list memory-map acc output-slot)])) '(#f #f #f) code (build-list (length code) values)))


