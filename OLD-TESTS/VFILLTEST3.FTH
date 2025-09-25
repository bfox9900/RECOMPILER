\ VFILLTEST.FTH   using FOR NEXT loop FOR VFILL and TYPE 

HEX 2000 ORG   \ this must be set before compiling any code 

INCLUDE DSK7.ITC-FORTH  \ preamble for indirect threaded Forth
INCLUDE DSK7.STD-OUT1A1 \ uses for/next VFILL 

IMPORT: ?TERMINAL 

COMPILER DECIMAL 

TARGET 
: VFILLTEST
   95 FOR   
      0  C/SCR @  R@ 33 + VFILL
   NEXT
;

: DELAY ( n --  ) FOR  R@ DROP NEXT ;

TARGET
: MAIN  ( -- ) 
   768 C/SCR !  
   VFILLTEST 
   5 12 AT-XY ."     VFILL in Forth    "   
   5 13 AT-XY ."   FOR  DUP VC!+  NEXT "  
   5000 DELAY 
   0 0 AT-XY 
   BEGIN 
    ." Hello metacompiling world!   "
    ?TERMINAL 
   UNTIL
   BYE 
;

COMPILER 
AUTOSTART MAIN 
SAVE DSK7.VFILLTEST3

HOST 

