\ TESTPROG1.FTH   minimal program for FXCC CROSS-COMPILER Ict 2024 Fox
COMPILER 
NEW 

HEX A000 ORG 

\ ===============[ THIS WILL BE PUT INTO A SINGLE FILE ]==================
\ target code starts here
\ build:;  EXIT NEXT DOCOL DOVAR DOCON LIT ?BRANCH BRANCH (NEXT)
INCLUDE DSK7.EXECUTORS  

\ IMPORT:  needs the word DOVAR and so must be loaded after EXECUTORS 
INCLUDE DSK7.IMPORT     \ teach cross-compiler how "steal" host code words
INCLUDE DSK7.ITCTYPES   \ teach compiler CONSTANT VARIABLE : ; etc. 
INCLUDE DSK7.BRANCHING  \ teach cross-compiler IF ELSE THEN BEGIN etc.
INCLUDE DSK7.DOLOOP 

IMPORT: 1+ DUP =

COMPILER HEX 

TARGET 
CODE BYE   ( -- )
    83C4 @@ CLR,  
    0000 @@ BLWP, 
ENDCODE

TARGET 
: MAIN 
    FFFF 0 
    DO 
    LOOP 
    BYE
;

\ Postlude commands: SET start word, save, END does checks 
COMPILER  INCLUDE DSK7.PREAMBLE 

PREAMBLE COLD        \ program entry word 
BOOT AUTOSTART MAIN  \ set 1st Forth word to run in the BOOT variable 
END.                 \ test if patches have been made 

SAVE DSK7.DOO

HOST 

