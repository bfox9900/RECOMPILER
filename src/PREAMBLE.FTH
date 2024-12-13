\ PREAMBLE.FTH   starts the Forth Virtual Machine 
\ note: compiler default values can be changed 
\       before INCLUDing this file  

COMPILER HEX

TARGET VARIABLE BOOT  \ hold the cfa of the word that boots from COLD

CODE COLD             \ COLD runs at boot time to build the Forth VM
      WKSPACE LWPI,   \ set 9900 workspace
      SP DSTACK LI,   \ data stack in scratchpad
      RP RSTACK LI,   \ return stack in scratchpad
      IP BOOT LI,     \ load interpreter pointer with boot word
 
      R10 _NEXT LI,   \ inner interpreter stays in R10
      NEXT,           \ jump into the interpreter
ENDCODE 

COMPILER
 

 
