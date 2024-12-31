# Forth Re-compiler

Traditionally when Forth is saved as a program the entire dictionary is saved in the image. The includes the interpreter, the compiler and sometimes even the Forth Assembler. 

This is an experimental project to compile "threaded" code as a standalone
binary program that consists only of the Forth words required to make the
program. 

One interesting feature is the addition of the word IMPORT:
IMPORT: can pull machine code segments from the Forth kernel and transcibe
them into the TARGET image while keeping a named reference in the COMPILER wordspace of the "host" Forth system. 

## How it Works
The method used here is based on information found in the "Forth Progammers Handbook" , Conklin and Rather although it is not followed exactly. 

The fundamental idea is to use the Forth wordlists (Vocabulary) system
to control the Cross-compiler's search order so that difference words will
be accessed when needed.  See the file RECOMPILER.FTH and the words TARGET COMPILER and HOST to view the search order for each condition. 

## Dec 2024
Began work to allow creation of a dictionary in the target code.
The commands HEADERS ON   -or-  HEADERS OFF  control the presence of the dictionary headers. 

The header control is part of making a "meta-compiler" to allow Camel99 Forth to re-build itself. 


