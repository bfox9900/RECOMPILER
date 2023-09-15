\ TESTPROG3.FTH  test if else then 

COMPILER 
HEX 2000 ORG 
INCLUDE DSK7.ITC-FORTH
\ bye does not end with NEXT so we can' import it 
CODE BYE   0 LIMI,  0 @@ BLWP,  NEXT,  ENDCODE 

TARGET
: MAIN  
    FFFF
    IF   BEEF 
    ELSE DEAD
    THEN 
    BEGIN AGAIN 
;

COMPILER 
T' MAIN  BOOT T!   \ set the boot variable 
SAVE DSK7.TESTPROG3
