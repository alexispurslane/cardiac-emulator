#lang scribble/manual

@title{How The @larger{CARDIAC} Emulator Works}
The CARIDAC Emulator is useful to illustrate how computation works in a real life computor, without going into the rigamarole of binary and hex, or the many layers of complicated memory and operating system, but instead just using this simple and abstract environment to teach people about computors.

@section{Who is this Document For?}

This chapter of this document is mainly targeted at would-be contributors or teachers looking to use this in their lessons. Furthur chapters will be documentation and description for how to program this machene, while this chapter will deatail how it works and how to use it in an educational environment. It is helpful if the teacher has some programming experiance, but that is obvous anyway since they are teaching a programming/computor science class. It will also help if the students are somewhat farmiliar with a dialect of lisp, javascript, lua, or ruby.
@margin-note{
Students may read this too. This document is just more focused on the questions the teacher may have while teaching using this equitment.
}

@section{What Goes on Inside...}
As you may have already guessed, the @bold{CARDIAC} Emulator is written in a dialect of scheme, known as Racket.
It is fully cross-platform, and espesially designed to be simple and fairly easy to use. It may require a little bit of knowledge of scheme to use the more advanced tools, but there is an editor that we are working on to curcumvent the need to use string variables for your assembly in racket and launch the visualizer from the REPL.
The memory map is represented by a list of 100 memory locations, each containing up to three digits of value. Each memory loaction number has some metadata attached to it, indicating weather it is code or data, so that the bootstrapper knows what to run and what to use as data.
Empty memory map locations are represented by -1, becouse CARIDAC does not support negative numbers.
There are 10 instructions for the @bold{CARDIAC}, but the non-single-digit instructions do not take arguments, therby mitigating the possible problem of having too little space to represent numbers.
@margin-note{
If you wish to participate in the development of this project, check out the source code, beocuse this is all I will say about implementation details here.
}
@section{Binary vs. Decimal?}
If you want to teach how computors work to people unfarmilliar with that topic previously, it will be long, hard, and tedious to make them fluent enough in binary for it to be helpful. Especially since assembly language uses hex or decimal for numbers, and there are many binary to hex or binary to decimal or hex to binary or hex to decimal converters out there already, without doing it by hand. This is why we chose to use decimal for the @bold{CARDIAC} instead.
