\ TESTPROG3.FTH  test if else then 
\ You see BEEF in R4 on the Classic99 debug screen 

COMPILER 
HEX 2000 ORG 
INCLUDE DSK7.ITC-FORTH

COMPILER HEX 
\ bye does not end with NEXT so we can' import it 

TARGET
CODE BYE   0 LIMI,  0 @@ BLWP,  NEXT,  ENDCODE 

: MAIN  
    FFFF
    IF   BEEF 
    ELSE DEAD
    THEN 
    BEGIN AGAIN 
;

COMPILER 
T' MAIN  BOOT T!   \ set the boot variable 
SAVE DSK7.TEST3
