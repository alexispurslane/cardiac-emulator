#lang scribble/manual

@title{The @larger{CARDIAC} Assembly Language Interface}

This document details how to use the assembly language interface functions that the @bold{CARDIAC Emulator} supplies.

@section{The Assembly Language}
Each command has three modes:
@itemlist[@item{Direct addressing Mode}
               @item{Indirect addressing Mode}
               @item{Literal Mode}]


@subsection{The Direct Addressing Mode}
This addressing mode is the default mode that all assembly language commands are executed in. In this mode, all memory locations refered to are the memory locations used in the command arguments. This mode acts exactly how the command is described. This is also the simplest mode.
This is how you use a direct addressed command:
@verbatim{
add 7
}
In this case, it adds what is from memory location seven to the accumulator. To switch back to normal mode after using other modes, such as indirect or literal modes, use the @verbatim{nom} code.

@subsection{The Indirect Addressing Mode}
This is the most useful, yet most complicated of the addressing modes. In this addressing mode, instead of directly using the address given to it, it insted uses the number from the address given as the argument, therefor allowing a sort of variably addressed command. This is useful for loops.
Here is an example:
@verbatim{
idi
add 7
nom
}
In this case, it gets the number in memory location 7, lets say 88, and uses @bold{that} as the argument. So in this case, it is equivalant to:

@verbatim{
add 88
}
But that's not all that indirect addressing mode can do. If you need to referance the program counter or accumulator, which are not accesable in memory, you can use these labels, instead of numbers, after the # symbol:

@tabular[#:sep @hspace[1]
	       (list (list @bold{Label} @bold{Meaning})
	       (list "ACC"                "The contents of the accumulator")
	       (list "PC"                 "The contents of the program counter"))]

@subsection{The Literal Mode}
In this mode, you can use literal numbers instead of memory addresses for certain opcodes. For example:
@verbatim{
lit
add 88
nom
}
There is no real equivilant in normal assembly for this. In this case, it adds the number 88 to the accumulator.

@subsection{The Commands}
The assembly language commands that you can use are as follows:
@tabular[#:sep @hspace[1]
	       (list (list @bold{Command} @bold{Action} @bold{Opcode})
	       (list "INP"                "INPut from input slot"       "1")
	       (list "CLA"                "CLear and input from memory to Accumulator"       "2")
	       (list "ADD"                "ADD from memory to accumulator"       "3")
	       (list "TAC"                "Test ACcumulator and jump to specified memory address if negative."       "4")
	       (list "SFT"                "ShiFT Accumulator by specified bits"       "5")
	       (list "OUT"                "OUTput memory location to output slot"       "6")
	       (list "STO"                "STOre accumulator in memory"       "7")
	       (list "SUB"                "SUBtract memory from accumulator"       "8")
	       (list "JMP"                "JuMP to memory location"       "9")
	       (list "HRS"                "Halt and reset"       "10"))]
@margin-note{
Notice that the HRS instruction has a two digit opcode, therefor leaving only one digit for the argument. Luckely, it takes no arguments.
}

@section{Without the Visualizer}
To run assembly language without the memory visualizer, using the Racket REPL, enter:

@verbatim{
(run-cardiac-assembly
 <start-mem>
 <code>
 [<input> 0]
 [<output> 0]
 [<acc> 0]
 [<starting-memory-map>
  (create-memory-map start-mem)]
 #:listener [<mem-change-callback> (lambda (mem) (void))])
}

To construct assembly language programs, you need to use this command:

@verbatim{
(string-join '(
               "<your line of code here 1>"
               "<your line of code here 2>"
               "<your line of code here 3>"
               "<your line of code here n>"
               ) "\n")
}

@section{With the Visualizer}
To run code with the memory Visualizer, everything is the same, exept that you use a different funciton:

@verbatim{
(run-assembly <start-mem> <code> [<input> 0] [<output> 0] [<acc> 0])
}

@subsection{What is the Visualizer?}
The Visualizer is a source code disassembler, crossed with a memory map light matrix. In layman's terms, it displays the contents of the memory of the @bold{CARDIAC} in a grid of color coded squares, with the disassembly of that memory address on top. It is very helpful for visualizing the memory of the @bold{CARDIAC} and the execution flow.
Here is a screenshot:
@image["visualizer.png"
       #:scale 0.3]
