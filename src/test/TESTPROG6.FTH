\ TESTPROG3.FTH  test if else then 

\ You see T on 1st line and F on 3rd line.


COMPILER 
NEW 

HEX A000 ORG 

\ ===============[ THIS WILL BE PUT INTO A SINGLE FILE ]==================
\ target code starts here
\ build:;  EXIT NEXT DOCOL DOVAR DOCON LIT ?BRANCH BRANCH (NEXT)
WARNINGS OFF 
INCLUDE DSK7.EXECUTORS  

\ IMPORT:  needs the word DOVAR and so must be loaded after EXECUTORS 
INCLUDE DSK7.IMPORT     \ teach cross-compiler how "steal" host code words
INCLUDE DSK7.ITCTYPES   \ teach compiler CONSTANT VARIABLE : ; etc. 
INCLUDE DSK7.BRANCHING  \ teach cross-compiler IF ELSE THEN BEGIN FOR NEXT 

INCLUDE DSK7.DOLOOP 
INCLUDE DSK7.COMPARISON

\ need the VDP words 
INCLUDE DSK7.VDP-API
INCLUDE DSK7.TICKTOCK

COMPILER WARNINGS ON 
\ ========================================================================
COMPILER HEX 

IMPORT:  ?TERMINAL 

TARGET
CODE BYE   ( -- )
    0 LIMI, 
    83C4 @@ CLR,  
    0000 @@ BLWP, 
ENDCODE


20 CONSTANT BL 
54 CONSTANT 'T'
46 CONSTANT 'F'

: PAGE   ( -- )   0 300 BL VFILL ;


TARGET 
: MAIN  
    BEGIN 
      2000 0 DO LOOP 
      0 300  'T' VFILL 
      2000 0 DO LOOP 
      PAGE 
\     0 2000 300 VREAD
      2000 0 DO LOOP 
      0 300  'F' VFILL
\     0 2000 300 VREAD
      
      ?TERMINAL 
    UNTIL 
    BYE 
;


\ we can change the default stack location and workspace here 
COMPILER  
HEX 
8300 TO WKSPACE 
83BE TO DSTACK 
836E TO RSTACK 

\ Using the values above we create the program entry word: COLD
\ Also creates a target variable called BOOT 
INCLUDE DSK7.PREAMBLE 

COMPILER 
PREAMBLE COLD 
BOOT AUTOSTART MAIN 
END. 

SAVE DSK7.ITCTEST6

HOST 
