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
				    "idi"
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
			       [`(10)   "hrs"]
			       [`(11)   "nom"]
			       [`(12)   "idi"]
			       [`(13)   "lit"])))

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
				["hrs" 10]
				["nom" 11]
				["idi" 12]
				["lit" 13]))
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
					   (cond
					    [(= (length (get-digits e)) 3)
					     (list (first (get-digits e)) (concat-digits (rest (get-digits e))))]
					    [(= (length (get-digits e)) 2)
					     (list e)])
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
	   (define mode (fourth prev-state))
	   
	   (match instruction
	     [`(1 ,mloc)
	      (cond
	       [(equal? mode 'normal)
		(mem-change-callback (change-memory-map memory-map `((,input-slot ,mloc))))
		(list (change-memory-map memory-map `((,input-slot ,mloc))) acc output-slot mode)]
	       [(equal? mode 'indirect)
		(mem-change-callback (change-memory-map memory-map `((,input-slot ,(memory-access memory-map mloc)))))
		(list (change-memory-map memory-map `((,input-slot ,(memory-access memory-map mloc)))) acc output-slot mode)])]
	     [`(2 ,mloc)
	      (cond
	       [(equal? mode 'normal)
		(list memory-map (memory-access memory-map mloc) output-slot mode)]
	       [(equal? mode 'literal)
		(list memory-map mloc output-slot mode)]
	       [(equal? mode 'indirect)
		(list memory-map (memory-access memory-map (memory-access memory-map mloc)) output-slot mode)])]
	     [`(3 ,mloc)
	      (cond
	       [(equal? mode 'normal)
		(list memory-map (+ acc (memory-access memory-map mloc)) output-slot mode)]
	       [(equal? mode 'literal)
		(list memory-map (+ acc mloc) output-slot mode)]
	       [(equal? mode 'indirect)
		(list memory-map (+ acc (memory-access memory-map (memory-access memory-map mloc))) output-slot mode)])]
	     [`(4 ,num) (if (negative? acc)
			    (cpu-execute (change-memory-map memory-map `((0 ,num))) input-slot output-slot acc)
			    (list memory-map acc output-slot mode))]
	     [`(5 ,num)
	      (cond
	       [(equal? mode 'normal)
		(list memory-map (arithmetic-shift num acc) output-slot mode)]
	       [(equal? mode 'literal)
		(list memory-map (arithmetic-shift num acc) output-slot mode)]
	       [(equal? mode 'indirect)
		(list memory-map (arithmetic-shift (memory-access memory-map num) acc) output-slot mode)])]
	     [`(6 ,mloc)
	      (cond
	       [(equal? mode 'normal)
		(list memory-map (memory-access memory-map mloc) output-slot mode)]
	       [(equal? mode 'literal)
		(list memory-map mloc output-slot mode)]
	       [(equal? mode 'indirect)
		(list memory-map acc (memory-access memory-map (memory-access memory-map mloc)) mode)])]
	     [`(7 ,mloc)
	      (cond
	       [(equal? mode 'normal)
		(mem-change-callback (change-memory-map memory-map `((,acc ,mloc))))
		(list (change-memory-map memory-map `((,acc ,mloc))) acc output-slot mode)]
	       [(equal? mode 'literal)
		(mem-change-callback (change-memory-map memory-map `((,acc ,mloc))))
		(list (change-memory-map memory-map `((,acc ,mloc))) acc output-slot mode)]
	       [(equal? mode 'indirect)
		(mem-change-callback (change-memory-map memory-map `((,acc ,(memory-access memory-map mloc)))))
		(list (change-memory-map memory-map `((,acc ,(memory-access memory-map mloc)))) acc output-slot mode)])]
	     [`(8 ,mloc)
	      (cond
	       [(equal? mode 'normal)
		(list memory-map (- acc (memory-access memory-map mloc)) output-slot mode)]
	       [(equal? mode 'literal)
		(list memory-map (- acc mloc) output-slot mode)]
	       [(equal? mode 'indirect)
		(list memory-map (- acc (memory-access memory-map (memory-access memory-map mloc))) output-slot mode)])]
	     [`(9 ,num) (cpu-execute (change-memory-map memory-map `((0 ,num))) memory-map input-slot output-slot acc i)]
	     [`(11) (list memory-map acc output-slot 'normal)]
	     [`(12) (list memory-map acc output-slot 'indirect)]
	     [`(13) (list memory-map acc output-slot 'literal)]
	     [else (list memory-map acc output-slot mode)])) '(#f #f #f normal) code (build-list (length code) values)))


