\ TESTPROG1.FTH   minimal program for FXCC CROSS-COMPILER Ict 2024 Fox

COMPILER
NEW

HEX A000 ORG

\ teach cross-compiler how "steal" host code words
\ MUST GO AFTER ORG STATEMENT SO CODE WORD ENTRY IS CORRECT
INCLUDE DSK7.IMPORT

\ target code starts here
\ build:;  EXIT NEXT DOCOL DOVAR DOCON LIT
INCLUDE DSK7.EXECUTORS

INCLUDE DSK7.BRANCHING  \ teach cross-compiler to branch etc.

\ IMPORT:  needs the word DOVAR and so must be loaded after EXECUTORS
INCLUDE DSK7.ITCTYPES   \ teach compiler to make CONSTANT VARIABLE etc.

\ create program entry word, COLD, which builds the Forth stacks
\ Also creates the target variable called BOOT
INCLUDE DSK7.PREAMBLE

\ your program starts hers
\ need the word BYE to exit program.
\ BYE in the kernel does not end with NEXT, so we can't use IMPORT:
COMPILER HEX
TARGET
CODE BYE   ( -- )
    83C4 @@ CLR,
    0000 @@ BLWP,
ENDCODE

\ need these primitives
IMPORT: 1- 0= DUP

TARGET
: MAIN
  DEAD
  BEGIN
    1- DUP
  -UNTIL
  BYE ;

\ Postlude commands: SET start word, save, END does checks
COMPILER
PREAMBLE COLD        \ program entry word
BOOT AUTOSTART MAIN  \ set 1st Forth word to run in the BOOT variable
END.                 \ test if patches have been made

SAVE DSK7.TEST1
