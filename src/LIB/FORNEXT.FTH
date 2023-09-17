\ FORNEXT.FTH  for RECOMPILER                   Sept 15 2023 Fox 
\ waaay smaller code than DO LOOP 

IMPORT: >R 

TARGET
CODE  <NEXT>
   *RP DEC,         \ decr top of R-stack
    NC IF,
        *IP IP ADD, \ take the jump 
        NEXT,       \ back to the interpreter
    ENDIF,
    RP INCT,        \ clean the Rstack 
    IP INCT,        \ move IP past the jump offset cell
 ENDCODE

COMPILER ALSO META DEFINITIONS 
: FOR  ( n --)  TCOMPILE >R   THERE ; IMMEDIATE
: NEXT ( -- )   TCOMPILE <NEXT> <BACK ; IMMEDIATE

COMPILER 
