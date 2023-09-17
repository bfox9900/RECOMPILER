\ TESTPROG1.FTH   minimal program for RECOMPILER  2023 Fox 
HEX 2000 ORG   \ this must be set before compiling any code 

INCLUDE DSK7.ITC-FORTH \ preamble for indirect threaded Forth
INCLUDE DSK7.STD-OUT

COMPILER HEX 

TARGET
CODE (FOR)                \ FOR using R15
      R15 RPUSH,
      TOS R15 MOV,
      TOS POP, 
      NEXT,
ENDCODE

CODE (NEXT)
    R15 DEC,            \ decrement loop ON RSTACK or R15
    OC IF,              \ test carry flag
        R9 ** R9 ADD,   \ jump back: ADD *IP,IP
        NEXT,
    ENDIF, 
    R9 INCT,            \ move past (LOOP)'s in-line parameter
    R15 RPOP,           \ restore R15
    NEXT, 
ENDCODE

CODE i   ( -- n) \ n is the loop index for R15 For loops
     TOS PUSH,
     R15 TOS  MOV,
     NEXT,
ENDCODE

COMPILER ALSO META DEFINITIONS 
: <BACK    ( addr --) THERE  -  T, ;
: FOR       ( n -- )  TCOMPILE (FOR) THERE  ;  IMMEDIATE
: NEXT      ( -- )    TCOMPILE (NEXT) <BACK ; IMMEDIATE

TARGET 
: MAIN  
    FFFF FOR NEXT 
    BYE 


\ technically we don't need MAIN. We could AUTOSTART BYE 
COMPILER 
AUTOSTART MAIN 
SAVE DSK7.TESTFORNXT
