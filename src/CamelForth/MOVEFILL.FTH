[CC] CR .( Forth 2012 MOVE CMOVE CMOVE>)
HEX

TARGET
CODE MOVE  ( src dst u -- ) \ u = no. of *CELLS* to move
    R0 POP,
    R1 POP,
    BEGIN,
       TOS DEC,
    OC WHILE,
       *R1+ *R0+ MOV, \ 9900 auto increments by 2 with MOV instruction
    REPEAT,
    TOS POP,
    NEXT,
ENDCODE

\ FORTH 2012: CMOVE and CMOVE> are part of the optional string word set
CODE CMOVE  ( src dst n -- )   \ forward character move
    R0 POP,       \ pop DEST into R0
    R1 POP,       \ pop source into R1
    BEGIN,
      TOS DEC,          \ test it before the loop starts
    OC WHILE,
      *R1+ *R0+ MOVB, \ byte move, with auto increment by 1.
    REPEAT,
    TOS POP,
    NEXT,
    ENDCODE

\ OCT 5 2020 Fixed computation for end of strings and inner loop.
CODE CMOVE>  ( src dst n -- )  \ move chars from end of string and go backwards
    R2 POP,      \ DEST into R2
    R1 POP,      \ source into R1
    TOS W MOV,   \ dup n
    W DEC,       \ compute n-1
    W R1 ADD,    \ point to end of source
    W R2 ADD,    \ point to end of destination
    BEGIN,
       TOS DEC,  \ decr the counter in TOS (R5)
    OC WHILE,
       *R1 *R2 MOVB,
        R1 DEC,   \ move pointers back (NO auto decrement, damn)
        R2 DEC,
    REPEAT,
    TOS POP,
    NEXT,
    ENDCODE

CODE FILL   ( addr cnt char -- )
    *SP+ R0 MOV,      \ pop cnt->R0                   22
    *SP+ R1 MOV,      \ pop addr->R1                  22
    TOS SWPB,         \                               10
    BEGIN,
      R0 DEC,         \ decr. count                   10
    OC WHILE,
      TOS *R1+ MOVB,  \ char is in TOS                20
    REPEAT,
    TOS POP,          \ refill the TOS register       22
    NEXT,             \                             =116
    ENDCODE

COMPILER
