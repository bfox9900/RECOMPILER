[CC] CR .( SKIP SCAN )

TARGET
CODE SKIP  ( c-addr u char -- c-addr' u')    \ skip matching chars
    TOS SWPB,          \ char stays in TOS
    R1  POP,           \ POP count into R1,
   *SP W MOV,         \ address->w
    BEGIN,
      R1 R1 MOV,
    NE WHILE, ( len<>0)
      *W TOS CMPB,
    EQ WHILE, ( *W=tos)
    ( do: 1 \STRING )
        W INC,        \ inc. adr
        R1 DEC,       \ dec. len
    REPEAT,
    ENDIF,
    W *SP  MOV,        \ store updated address on stack
    R1 TOS MOV,        \ updated count to TOS
    NEXT,              \ 26 bytes
    ENDCODE

CODE SCAN  ( adr len char -- adr' len' )    \ find matching char
    TOS SWPB,           \ char stays in TOS
    R1  POP,            \ POP count into R1,
    *SP W MOV,          \ address->w
    BEGIN,
      R1 R1 MOV,
    NE WHILE, ( len<>0)
        *W TOS CMPB,
    NE WHILE, ( *w<>tos)
    ( do: 1 \STRING )
        W INC,        \ inc. adr
        R1 DEC,       \ dec. len
    REPEAT,
    ENDIF,
    W *SP  MOV,        \ store updated address on stack
    R1 TOS MOV,        \ updated count to TOS
    NEXT,              \ 26 bytes
    ENDCODE

COMPILER
