\ TESTPROG2.FTH  Demo IMPORT: CODE  loops and AUTOSTART   Sep 2023 Fox 

HEX 2000 ORG   \ this must be set before compiling any code 

INCLUDE DSK7.ITC-FORTH \ preamble for indirect threaded Forth

INCLUDE DSK7.STD-OUT

\ runtime action of S" moves IP past the length of the string
\ This is the first colon word in the system
TARGET
: (S")  ( -- c-addr u) R>  COUNT  2DUP + ALIGNED >R ; \ run-time for S" 


COMPILER ALSO META DEFINITIONS 
HOST: S"   [CHAR] " PARSE  TCOMPILE (S")  TS, ;HOST IMMEDIATE 


COMPILER DECIMAL 

TARGET
: MAIN  ( -- ) 
   767 32 - C/SCR !  

   0 C/SCR @ 42 VFILL  \ speed test
   0 C/SCR @ 43 VFILL 
   
   PAGE 
   BEGIN 
   S" HELLO WORLD    " TYPE 
   AGAIN 
;

COMPILER 
AUTOSTART MAIN 
SAVE DSK7.HELLOWORLD
