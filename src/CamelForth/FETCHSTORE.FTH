CR .( MEMORY FETCH & STORE)

CODE !      ( n addr -- )
            *SP+ *TOS MOV,   \ 26
            TOS POP,         \ 22
            NEXT,           \ = 48
            ENDCODE

CODE @      ( a -- w )
\            _fetch >HSRAM CFA!
            *TOS TOS MOV,
            NEXT,
            ENDCODE

\ Aug 4 2018: fixed order of data to be ANS compliant
CODE 2!     ( d addr -- )
            *SP+ *TOS  MOV,    \ the top # of D is stored at the lower adrs
            *SP+ 2 (TOS) MOV,  \ next # stored 1 cell higher (addr+2)
            TOS POP,
            NEXT,
            ENDCODE

\ Aug 4 2018: fixed order of data to be ANS compliant
CODE 2@     ( addr -- d)  \ the lower address will appear on top of stack
            2 (TOS) PUSH, \    38
            *TOS TOS MOV, \    28
            NEXT,         \ =  66
            ENDCODE

CODE C!     ( c addr -- )
            1 (SP) *TOS MOVB,     \  26
            SP INCT,              \  10   inc. stack pointer by 2
            TOS POP,              \  22
            NEXT,                 \ =58
            ENDCODE

CODE COUNT  ( addr -- addr' u)
            TOS PUSH,             \ make a copy of addr
            *SP INC,              \ inc. addr past the count byte
L: _C@      *TOS TOS MOVB,        \ put C@ inline to save space
            TOS 8 SRL,            \ **BUG FIX** used to be SRA :-(
            NEXT,
            ENDCODE

CODE C@     _C@  -2 TALLOT THERE T! ENDCODE      \ give _C@ a dictionary header

CODE +!     ( n addr --)
            *SP+ *TOS ADD,         \ 14+8+4 = 26 cycles
            TOS POP,               \          22 cycles
            NEXT,                  \ -------------------
            ENDCODE                \          48 cycles

CODE C+!    ( c addr -- ) \ 8 bytes versus 12 bytes in Forth
            1 (SP) *TOS ADDB,      \ 26
            SP INCT,               \ 10
            TOS POP,               \ 22
            NEXT,                 \ =58
            ENDCODE
