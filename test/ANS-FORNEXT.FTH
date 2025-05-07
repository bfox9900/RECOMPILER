

CODE  <NEXT>
   *RP DEC,         \ decr top of R-stack
    NC IF,
        *IP IP ADD, \ take the jump 
        NEXT,       \ back to the interpreter
    ENDIF,
    RP INCT,        \ clean the Rstack 
    IP INCT,        \ move IP past the jump offset cell
ENDCODE

: FOR  ( n --)  POSTPONE >R  HERE ; IMMEDIATE
: NEXT ( -- )   POSTPONE <NEXT> HERE  -  ,  ; IMMEDIATE

