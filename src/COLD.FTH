\ COLD.FTH   sets up the Forth virtual machine 
CODE COLD
      8300 LWPI,
      4000  SP0  LI,           \ data stack
      RP  RP0  LI,             \ return stack
      R10 T' EXIT CELL+ LI,    \ NEXT, the inner interpreter
      IP  BOOT  LI,            \ load interpreter pointer with boot word
      *R10 B,                  \ run Forth NEXT (inner interpreter)
ENDCODE
