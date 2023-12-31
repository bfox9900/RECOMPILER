\ TESTPROG2.FTH  Demo IMPORT: CODE  loops and AUTOSTART   Sep 2023 Fox 

HEX 2000 ORG   \ this must be set before compiling any code 

INCLUDE DSK7.ITC-FORTH \ preamble for indirect threaded Forth
INCLUDE DSK7.STD-OUT0  \ no code, only forth

IMPORT: ?TERMINAL 

COMPILER DECIMAL 

TARGET 
: VFILLTEST
   32 >R  
   BEGIN   
      0 C/SCR @ R@ VFILL
      R> 1+ DUP>R 
   127 = UNTIL  
   R> DROP 
;

TARGET
: MAIN  ( -- ) 
   768 C/SCR !  
   VFILLTEST 
   BEGIN 
     S" VDP Driver in Forth, " TYPE  
     S" BEGIN loops only.   " TYPE 
    ?TERMINAL 
   UNTIL
   BYE 
;

COMPILER 
AUTOSTART MAIN 
SAVE DSK7.VFILLTEST

HOST 

