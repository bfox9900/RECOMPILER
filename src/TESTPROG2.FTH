\ TESTPROG2.FTH  IMPORT:, while loop, UNTIL loop and BYE   Sep 2023 Fox 

HEX 2000 ORG   \ this must be set before compiling any code 

INCLUDE DSK7.ITC-FORTH

IMPORT: DUP DROP  1- 0=

COMPILER HEX 

TARGET
CODE BYE   
  0 LIMI, 
  0 @@ BLWP,  
  NEXT,  
ENDCODE 

: LOOP2    
  5000 
  BEGIN  
    1- DUP 
  0= UNTIL 
  DROP 
;

: MAIN  
    5000 
    BEGIN  1-  
    DUP WHILE 
    REPEAT 
    DROP 
    LOOP2 
    BYE
;

COMPILER 
T' MAIN  BOOT T!   \ set the boot variable 
SAVE DSK7.TESTPROG2
