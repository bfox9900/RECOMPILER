\ TESTPROG1.FTH   minimal program for RECOMPILER  2023 Fox 

COMPILER 

HEX 2000 ORG 

INCLUDE DSK7.ITC-FORTH  \ ITC preamble package 

TARGET 
: MAIN  
    BYE  
;

\ technically we don't need MAIN. We could AUTOSTART BYE 
COMPILER 
AUTOSTART MAIN 
SAVE DSK7.TEST1
