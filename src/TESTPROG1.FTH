\ TEST PROGRAM 1

COMPILER 
NEW
\ we MUST load Executors first
\ EXIT DOCON DOVAR DOCOL ?BRANCH and BRANCH
INCLUDE DSK7.EXECUTORS  

\ Now we can make the reset of the compiler
INCLUDE DSK7.BRANCHING   \ compilers for: IF THEN BEGIN AGAIN...
INCLUDE DSK7.DATATYPES   \ CONSTANT VARIABLE : ;

COMPILER HEX 

\ import needs to see Forth to find kernel primitives 
TARGET ALSO FORTH   
IMPORT: + BYE 

TARGET CODE COLD
      8300 LWPI,
      4000  SP0  LI,           \ data stack
      RP  RP0  LI,             \ return stack
      R10 T' EXIT CELL+ LI,    \ NEXT, the inner interpreter
      IP  BOOT  LI,            \ load interpreter pointer with boot word
      *R10 B,                  \ run Forth NEXT (inner interpreter)
ENDCODE


TARGET   \ now we can't look at Forth vocabuarly
: TEST  2  4  +  BYE  ;

COMPILER HEX 
T' TEST  
