[CC] CR .( CROSS-COMPILING colon semi-colon )

COMPILER ALSO META DEFINTIONS
HOST: :                \ this is the CROSS compiler's ':'
    !CSP               \ record the "current stack position"
    TCREATE
    TCOMPILE DOCOL
    HIDE               \ can't find this word during compilation
    STATE ON           \ turn on compiler
;HOST

HOST: ;
    REVEAL             \ can be found in the dictionary now
    TCOMPILE EXIT
    ?CSP               \ check stack
    STATE OFF          \ interpreting mode
;HOST  IMMEDIATE

COMPILER
