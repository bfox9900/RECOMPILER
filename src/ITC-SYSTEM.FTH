\ ITC-SYSTEM.FTH  preamble to make an indirect threaded system  Sep 2023 Fox

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

COMPILER
COLD 2002 T!          \ COLD will run at startup now  

CR .( ITC Preamble loaded )