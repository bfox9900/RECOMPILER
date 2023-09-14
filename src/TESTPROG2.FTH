\ TEST PROGRAM 1

COMPILER 
NEW
HEX 2000 ORG 

WARNINGS OFF 
INCLUDE DSK7.EXECUTORS  \ load EXIT DOCOL DOVAR ETC.
INCLUDE DSK7.BRANCHING  \ compilers: IF THEN BEGIN AGAIN...
INCLUDE DSK7.ITCTYPES   \ CONSTANT VARIABLE : ; ETC,,
WARNINGS ON 


COMPILER HEX

TARGET ALSO FORTH 
IMPORT: DUP DROP  1-  0= 

TARGET 
VARIABLE BOOT  \ hold the cfa of the word that boots from COLD 

\ bye does not end with NEXT so we can' import it 
CODE BYE   0 LIMI,  0 @@ BLWP,  NEXT,  ENDCODE 

CODE COLD   \ COLD is a key primitive that builds the Forth VM 
      8300 LWPI,               \ set 9900 workspace
      SP 83FE LI,              \ data stack
      RP 83D0 LI,              \ return stack
      IP  BOOT  LI,            \ load interpreter pointer with boot word

      R10 EXIT CELL+ LI,       \ EXIT + 2 = NEXT -> R10
      *R10 B,                  \ jump to NEXT (inner interpreter)
ENDCODE

TARGET

: MAIN  
  FFFF BEGIN  1-  DUP WHILE REPEAT DROP 
  7FFF BEGIN  1- DUP 0= UNTIL DROP 
  BYE
;

COMPILER HEX  T' MAIN  BOOT HOST !       \ set the boot variable 
COMPILER      T' COLD >BODY 2002 HOST !  

SAVE DSK7.TESTPROG2
