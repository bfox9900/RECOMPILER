\ TESTPROG1.FTH   minimal program for RECOMPILER  2023 Fox 
HEX 2000 ORG   \ this must be set before compiling any code 

INCLUDE DSK7.ITC-FORTH \ preamble for indirect threaded Forth

INCLUDE DSK7.FORNEXT 

COMPILER HEX 

TARGET 
: MAIN  
    BEEF  FOR NEXT 
    BYE 
;

COMPILER 
AUTOSTART MAIN 
SAVE DSK7.TESTFORNXT

