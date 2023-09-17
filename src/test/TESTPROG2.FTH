\ TESTPROG2.FTH  Demo IMPORT: CODE  loops and AUTOSTART   Sep 2023 Fox 

HEX 2000 ORG   \ this must be set before compiling any code 

INCLUDE DSK7.ITC-FORTH \ preamble for indirect threaded Forth

IMPORT: DUP DROP  1- 0=

COMPILER HEX 

TARGET
CODE BYE ( --) 0 LIMI,   0 @@ BLWP,    NEXT,  ENDCODE 

: LOOP2 ( -- ) 5000 BEGIN  1- DUP   0= UNTIL   DROP ;
: LOOP1 ( -- ) 5000 BEGIN  1-  DUP WHILE REPEAT DROP ;

: MAIN  ( -- ) LOOP1 LOOP2 BYE ;

COMPILER 
AUTOSTART MAIN 
SAVE DSK7.TEST2
