\ vfillTEST.FTH   using FOR NEXT loop FOR VFILL and TYPE 

HEX 2000 ORG   \ this must be set before compiling any code 

INCLUDE DSK7.ITC-FORTH \ preamble for indirect threaded Forth

INCLUDE DSK7.STD-OUT1A3 \ uses ALC VFILL 

IMPORT: ?TERMINAL 

COMPILER DECIMAL 

TARGET 
: VFILLTEST
   95 FOR   
      0  C/SCR @  R@ 33 + VFILL
   NEXT
;

TARGET
: MAIN  ( -- ) 
   32 C/L !   768 C/SCR !  \ init the variables   
   
   VFILLTEST 
   5 12 AT-XY ."   VFILL in Assembler  "   
   5 13 AT-XY ."   in FOR NEXT loop    "  
   BEGIN 
     ."  Help meeeee    "
    ?TERMINAL 
   UNTIL
   BYE 
;

COMPILER 
AUTOSTART MAIN 
SAVE DSK7.VFILLTEST

HOST 

