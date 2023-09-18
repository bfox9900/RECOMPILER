\ vfillTEST.FTH   using FOR NEXT loop FOR VFILL and TYPE 

HEX 2000 ORG   \ this must be set before compiling any code 

INCLUDE DSK7.ITC-FORTH \ preamble for indirect threaded Forth
INCLUDE DSK7.STD-OUT  \ uses For/next & EMIT+ as CODE 

IMPORT: ?TERMINAL 

COMPILER DECIMAL 

TARGET 
: VFILLTEST  58 FOR   0 C/SCR @ R@ BL + VFILL  NEXT ;

TARGET
: MAIN  ( -- ) 
   767 C/SCR !  
   VFILLTEST 
   PAGE
   BEGIN 
     S"   VFILL in Forth. " TYPE  
     S"   FOR/NEXT loops and EMIT+ is CODE" TYPE 
    ?TERMINAL 
   UNTIL
   BYE 
;

COMPILER 
AUTOSTART MAIN 
SAVE DSK7.VFILLTEST2

HOST 

