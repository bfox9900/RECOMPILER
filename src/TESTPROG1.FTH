\ TESTPROG1.FTH   minimal program for RECOMPILER  2023 Fox 

COMPILER 

HEX 2000 ORG 

INCLUDE DSK7.ITC-FORTH  \ ITC preamble package 

\ bye does not end with NEXT so we can' import it 
TARGET
CODE BYE   0 LIMI,  0 @@ BLWP,  NEXT,  ENDCODE 

: MAIN  
    BYE  
;

\ technically we don't need MAIN. We could AUTOSTART BYE 
COMPILER 
AUTOSTART MAIN 
SAVE DSK7.TEST1
