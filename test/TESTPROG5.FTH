\ TESTPROG3.FTH  test if else then 

\ You see T on 1st line and F on 3rd line.


COMPILER 
NEW 

HEX A000 ORG 

\ ===============[ THIS WILL BE PUT INTO A SINGLE FILE ]==================
\ target code starts here
\ build:;  EXIT NEXT DOCOL DOVAR DOCON LIT ?BRANCH BRANCH (NEXT)
COMPILER WARNINGS OFF 
INCLUDE DSK7.EXECUTORS  

\ IMPORT:  needs the word DOVAR and so must be loaded after EXECUTORS 
INCLUDE DSK7.IMPORT     \ teach cross-compiler how "steal" host code words
INCLUDE DSK7.ITCTYPES   \ teach compiler CONSTANT VARIABLE : ; etc. 
INCLUDE DSK7.BRANCHING  \ teach cross-compiler IF ELSE THEN BEGIN FOR NEXT 

\ create program entry word, COLD, which builds the Forth stacks
\ Also creates the target variable called BOOT 
INCLUDE DSK7.PREAMBLE 

\ need the VDP words 
INCLUDE DSK7.VDP-API
COMPILER WARNINGS ON 

\ ========================[ PROGRAM BEGINS ]=============================
COMPILER HEX 

TARGET
CODE BYE   ( -- )
    0 LIMI, 
    83C4 @@ CLR,  
    0000 @@ BLWP, 
ENDCODE

COMPILER DECIMAL 
TARGET 
     32 CONSTANT BL 
CHAR  A CONSTANT 'A'

: PAGE       0 768 BL VFILL ;

IMPORT:  ?TERMINAL PICK  1+ 

TARGET INCLUDE DSK7.FORNEXT 


TARGET 
: MAIN  
    BEGIN 
      'A' 
      25 FOR 
          PAGE 
          0 768 2 PICK VFILL
          1+  ( inc. character )
      NEXT 
      ?TERMINAL 
    UNTIL 
    BYE 
;

COMPILER 
PREAMBLE COLD 
BOOT AUTOSTART MAIN 
END. 

SAVE DSK7.ITCTEST5

HOST 
