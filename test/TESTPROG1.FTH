\ TESTPROG1.FTH   minimal program for FXCC CROSS-COMPILER Ict 2024 Fox

COMPILER 
NEW 

HEX A000 ORG 

\ target code starts here
\ build:;  EXIT NEXT DOCOL DOVAR DOCON LIT ?BRANCH BRANCH (NEXT)
INCLUDE DSK7.EXECUTORS  

\ IMPORT:  needs the word DOVAR and so must be loaded after EXECUTORS 
INCLUDE DSK7.IMPORT     \ teach cross-compiler how "steal" host code words
INCLUDE DSK7.ITCTYPES   \ teach compiler to make CONSTANT VARIABLE etc. 
INCLUDE DSK7.BRANCHING  \ teach cross-compiler to branch etc.

\ create program entry word, COLD, which builds the Forth stacks
\ Also creates the target variable called BOOT 
INCLUDE DSK7.PREAMBLE 

\ Steal this code from the Host Forth and put in target 
IMPORT: ?TERMINAL

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
    BEGIN 
      ?TERMINAL 
    UNTIL 
    BYE 
;

\ Postlude commands: SET start word, save, END does checks 
COMPILER 
PREAMBLE COLD        \ program entry word 
BOOT AUTOSTART MAIN  \ set 1st Forth word to run in the BOOT variable 
END.                 \ test if patches have been made 

SAVE DSK7.TEST1
