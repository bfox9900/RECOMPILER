\ TESTPROG1.FTH   minimal program for RECOMPILER  2023 Fox 

COMPILER 

HEX 2000 ORG 
INCLUDE DSK7.ITC-FORTH 

TARGET 

: MAIN  
    BYE  
;

\ Postlude commands: SET start word, save, END does checks 
COMPILER 
AUTOSTART MAIN 
SAVE DSK7.TEST1
END.
