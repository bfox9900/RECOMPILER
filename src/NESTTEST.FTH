\ NESTTEST.FTH  test nested calls 

COMPILER 
NEW
HEX 2000 ORG 

WARNINGS OFF 
INCLUDE DSK7.EXECUTORS  \ load EXIT DOCOL DOVAR ETC.
INCLUDE DSK7.BRANCHING  \ compilers: IF THEN BEGIN AGAIN...
INCLUDE DSK7.ITCTYPES   \ CONSTANT VARIABLE : ;  etc.
WARNINGS ON 

COMPILER HEX

TARGET 
VARIABLE BOOT  \ hold the cfa of the word that boots from COLD 

L: COLD               \ COLD runs at boot time to build the Forth VM 
      8300 LWPI,      \ set 9900 workspace
      SP 83FE LI,     \ data stack in scratchpad
      RP 83D0 LI,     \ return stack in scratchpad 
      IP BOOT LI,     \ load interpreter pointer with boot word

      R10 _NEXT LI,   \ inner interpreter stays in R10 
      *R10 B,         \ jump into the interpreter 

TARGET ALSO FORTH 
IMPORT: DUP DROP 1- 

TARGET
CODE BYE    0 LIMI, 0 @@ BLWP, ENDCODE 

 : SUB3  BEEF ;
 : SUB2  SUB3 ;
 : SUB1  SUB2 ;

: MAIN  
    4000 
    BEGIN  
      1- DUP 
    WHILE    
      SUB1  DROP 
    REPEAT
    DROP 
    BYE 
;

COMPILER HEX  
COLD 2002 T!       \ jumps into cold on startup
T' MAIN  BOOT T!   \ set the boot variable 

SAVE DSK7.NESTTEST
