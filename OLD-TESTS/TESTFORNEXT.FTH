\ TESTPROG1.FTH   minimal program for FXCC CROSS-COMPILER Ict 2024 Fox

COMPILER 
NEW 

HEX A000 ORG 

\ target code starts here
\ build:;  EXIT NEXT DOCOL DOVAR DOCON LIT ?BRANCH BRANCH 
INCLUDE DSK7.EXECUTORS  

\ Extend the compiler for indirect threaded code
INCLUDE DSK7.ITCTYPES   \ setup compiler for ITC and define TLITERAL 
INCLUDE DSK7.BRANCHING  \ teach cross-compiler to branch etc.
INCLUDE DSK7.IMPORT     \ teach cross-compiler how "steal" host code words

\ PREAMBLE creates program entry word, COLD, which builds the Forth stacks
\ Also creates the target variable called BOOT 
INCLUDE DSK7.PREAMBLE 

INCLUDE DSK7.FORNEXT 

\ your program starts hers 

\ need the word BYE to exit program.
\ BYE in the kernel does not end with NEXT, so we can't use IMPORT: 
COMPILER HEX 
TARGET 
CODE BYE   ( -- )
    83C4 @@ CLR,  
    0000 @@ BLWP, 
ENDCODE

: MAIN  
    FFFF FOR
    NEXT 
    BYE 
;

\ Postlude commands: SET start word, save, END does checks 
COMPILER 
PREAMBLE COLD        \ program entry word 
BOOT AUTOSTART MAIN  \ set 1st Forth word to run in the BOOT variable 
END.                 \ test if patches have been made 

SAVE DSK7.TESTFOR
