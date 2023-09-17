\ vfillTEST.FTH   using FOR NEXT loop FOR VFILL and TYPE 

HEX 2000 ORG   \ this must be set before compiling any code 

INCLUDE DSK7.ITC-FORTH \ preamble for indirect threaded Forth
INCLUDE DSK7.STD-OUT \ uses For/next & EMIT+ as CODE 

IMPORT: ?TERMINAL 

COMPILER DECIMAL 2

TARGET 
: STOP ;
: VFILLTEST  58 FOR   0 C/SCR @  R@ BL +  VFILL   NEXT STOP ;

TARGET
: MAIN  ( -- ) 
   768 C/SCR !  
   VFILLTEST 
   PAGE
   BEGIN 
     S"   VFILL in Forth  " TYPE  
     ?TERMINAL
   UNTIL
   BYE 
;

COMPILER 
AUTOSTART MAIN 
SAVE DSK7.VFILLTEST3
HOST 

