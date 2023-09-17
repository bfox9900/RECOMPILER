\ FORNEXT.FTH  for RECOMPILER                   Sept 15 2023 Fox 
\ waaay smaller code than DO LOOP 
TARGET
HEX
CODE (NEXT)
    *RP DEC,        \ dec counter value
    OC IF,          
        *IP IP ADD, \ jump back: ADD *IP,IP
        NEXT,
    ENDIF, 
    RP INCT,        \ remove counter from Rstack 
    R9 INCT,        \ move past (LOOP)'s in-line parameter
    NEXT, 
ENDCODE


COMPILER ALSO META DEFINITIONS 
: FOR  ( n --)  TCOMPILE >R   THERE ; IMMEDIATE
: NEXT ( -- )   TCOMPILE (NEXT) <BACK ; IMMEDIATE

COMPILER 
