\ TEST PROGRAM 1

COMPILER 
NEW
HEX 2000 ORG 

WARNINGS OFF 
INCLUDE DSK7.EXECUTORS  \ load EXIT DOCOL DOVAR ETC.
INCLUDE DSK7.BRANCHING   \ compilers: IF THEN BEGIN AGAIN...
INCLUDE DSK7.ITCTYPES   \ CONSTANT VARIABLE : ; ETC,,
WARNINGS ON 

TARGET 
\ import needs to see Forth to find kernel primitives 
ALSO FORTH   
IMPORT: + 

COMPILER HEX

TARGET 
VARIABLE BOOT  \ hold the cfa of the first word that RUN

\ bye does not end with NEXT so we can' import it 
CODE BYE   0 LIMI,  0 @@ BLWP,  NEXT,  ENDCODE 

\ COLD is a key primitive to setup the Forth VM 
CODE COLD
      8300 LWPI,               \ set 9900 workspace
      SP 3FFE LI,              \ data stack
      RP 3FE0 LI,              \ return stack
      IP  BOOT  LI,            \ load interpreter pointer with boot word

      R10 EXIT CELL+ LI,       \ EXIT + 2 = NEXT -> R10
      *R10 B,                  \ jump to NEXT (inner interpreter)
ENDCODE

TARGET
: MAIN  
    BYE  
;

COMPILER HEX  T' MAIN  BOOT HOST !  \ set the boot variable 
COMPILER      T' COLD >BODY 2002 HOST !

SAVE DSK7.TESTPROG1
