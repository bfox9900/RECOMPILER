[CC] CR .( MOVE FILL )

TARGET 
CODE MOVE   ( src dst n -- )   \ forward character move
            *SP+ R0  MOV,      \ pop DEST into R0
            *SP+ R1  MOV,      \ pop source into R1
            TOS TOS MOV,
            NE IF,            \ if n=0 we are done
\ need some copies
                R0  R2 MOV, \ dup dest
                R0  R3 MOV, \ dup dest
                TOS R3 ADD, \ R3=dest+n
\ test window:  src  dst dst+n WITHIN
                R0  R3 SUB,
                R1  R2 SUB,
                R3  R2 CMP,
                HI IF, \ do cmove> ............
                
                    TOS W MOV,      \ dup n
                        W DEC,      \ compute n-1
                    W  R1 ADD,      \ point to end of source
                    W  R0 ADD,      \ point to end of destination
                    BEGIN,
                      *R1 *R0 MOVB,
                       R1 DEC,     \ dec source
                       R0 DEC,     \ dec dest
                       TOS DEC,    \ dec the counter in TOS (R4)
                    EQ UNTIL,

                ELSE,  \ do cmove .............
                    BEGIN,
                      *R1+ *R0+ MOVB, \ byte move, with auto increment by 1.
                       TOS DEC,        \ we can test it before the loop starts
                    EQ UNTIL,
                ENDIF,
            ENDIF,
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
