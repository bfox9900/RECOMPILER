\ FORNEXT.FTH  for RECOMPILER                   Sept 15 2023 Fox 
\ waaay smaller code than DO LOOP 
TARGET
HEX
CODE (NEXT)
    *RP DEC,            \ decrement loop ON RSTACK or R15
    OC IF,              \ if NO carry then exit loop
        R9 ** R9 ADD,   \ jump back: ADD *IP,IP
        NEXT,
    ENDIF, 
    R9 INCT,       \ move past (LOOP)'s in-line parameter
    R15 RPOP,
    NEXT, 
ENDCODE


COMPILER ALSO META DEFINITIONS 
: FOR  ( n --)  TCOMPILE >R   THERE ; IMMEDIATE
: NEXT ( -- )   TCOMPILE (NEXT) <BACK ; IMMEDIATE

COMPILER 
