\ TEST-DOLOOP.FTH  Sep 2023 Fox 

HEX 2000 ORG   \ this must be set before compiling any code 

INCLUDE DSK7.ITC-FORTH \ preamble for indirect threaded Forth

\ INCLUDE DSK7.DOLOOP 

COMPILER DECIMAL 

: MAIN  
     32000 DO  LOOP 
;

COMPILER 
AUTOSTART MAIN 
SAVE DSK7.DOLOOPTEST

HOST 

