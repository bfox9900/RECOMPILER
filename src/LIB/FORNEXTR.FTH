\ FORNEXTR.FTH  for RECOMPILER   counter in R15          Sept 20 2023 Fox 

\ **************************************
\ *  R15  NOT MULTI-TASKER COMPATIBLE  *
\ **************************************
CODE (FOR)                \ FOR using R15
      R15 RPUSH,
      TOS R15 MOV,
      TOS POP, 
      NEXT,
      ENDCODE

HEX
CODE (NEXT)
    R15 DEC,            \ decrement loop ON RSTACK or R15
    OC IF,              \ if NO carry then exit loop
        IP ** IP ADD,   \ jump back: ADD *IP,IP
        NEXT,
    ENDIF, 
    IP INCT,       \ move past (LOOP)'s in-line parameter
    R15 RPOP,
    NEXT, 
ENDCODE

CODE i   ( -- n) \ n is the loop index for R15 For loops
     TOS PUSH,
     R15 TOS  MOV,
     NEXT,
ENDCODE


COMPILER ALSO META DEFINITIONS 
: FOR  ( n --)  TCOMPILE (FOR)  THERE ; IMMEDIATE
: NEXT ( -- )   TCOMPILE (NEXT) THERE  -  T,  ; IMMEDIATE

COMPILER 
