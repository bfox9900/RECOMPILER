CR .( DATA STACK)

CODE SP@    ( -- a )
            TOS PUSH,
            SP TOS MOV,
            NEXT,
            ENDCODE

CODE SP!    ( a -- )
            TOS SP MOV,
            TOS POP,
            NEXT,
            ENDCODE

CODE DROP   ( w -- )
\            _drop >HSRAM CFA!
           TOS POP,             \ 22 *SP+ TOS MOV,
           NEXT,
            ENDCODE

CODE NIP    ( n n' -- n')
           SP INCT,             \ 10
           NEXT,
           ENDCODE

CODE ?DUP   ( x -- 0 | x x)       \ ?DUP interleaved with DUP to save space
            TOS TOS MOV,          \ test for TOS<>0
            NE IF,                \ if TOS<>0
                TOS PUSH,        \ dup it
            ENDIF,
            NEXT,
            ENDCODE

CODE DUP    ( w -- w w )          \ create a Forth Header for DUP
\            _DUP >HSRAM CFA!       \ use hi-speed ram version
           TOS PUSH,
           NEXT,
            ENDCODE

CODE SWAP   ( w1 w2 -- w2 w1 )
            TOS   W MOV,          \ 14
            *SP  TOS MOV,          \ 18
            W   *SP MOV,          \ 18
            NEXT,                 \ 50
            ENDCODE

CODE OVER   ( w1 w2 -- w1 w2 w1 )
            TOS PUSH,             \ 28
            2 (SP) TOS MOV,       \ 22
            NEXT,                 \ 50
            ENDCODE

\ CODE OVERSWAP  ( w1 w2 -- w1 w1 w2)
\              SP  DECT,
\              2 (SP)  *SP MOV,
\              NEXT,
\              ENDCODE

CODE ROT    ( n1 n2 n3 --  n2 n3 n1)
            2 (SP)    W MOV, 
            *SP  2 (SP) MOV, 
            TOS     *SP MOV, 
            W       TOS MOV,  
            NEXT,   
            ENDCODE

CODE -ROT   ( n1 n2 n3 --  n2 n3 n1)
            TOS       W MOV,
            *SP      TOS MOV,
            2 (SP)  *SP MOV,
            W    2 (SP) MOV,
            NEXT,
            ENDCODE

\ byte/word conversions
CODE ><     ( n -- n )           \ swap bytes in n
            TOS SWPB,            \ handy and only 1 instruction
            NEXT,
            ENDCODE

\ used in number conversion. Same size as DROP DROP 3X faster
CODE 2DROP  ( n n -- )
            SP INCT,           \ 10
            TOS POP,           \ 28
            NEXT,            \ = 38
            ENDCODE

\ 3 instructions 62 clocks vs 6 instructions 100 clocks for OVER OVER 
CODE 2DUP  ( n1 n2 -- n1 n2 n1 n2 )
            SP -4 AI,       \  14   make room for 2 cells
            4 (SP) *SP MOV,   \  26   n1->*SP
            TOS  2 (SP) MOV,  \  22   n2 -> next stack location
            NEXT,            \   62   
            ENDCODE

CODE 2SWAP  ( n1 n2 n3 n4-- n3 n4 n1 n2)
             TOS       R0 MOV,    \ n4  14
            *SP        R1 MOV,    \ n3  14
            2 (SP)   TOS MOV,    \ n2  22
            4 (SP)   *SP MOV,    \ n1  26
            R0    2 (SP) MOV,    \     22
            R1    4 (SP) MOV,    \     22
            NEXT,                \    120
            ENDCODE

CODE PICK   ( n -- n)   \ GForth ITC takes 8 intel instructions for PICK
              TOS  1 SLA,   \ 14  (" n CELLS")
              SP TOS ADD,    \ 14
             *TOS TOS MOV,   \ 18
              NEXT,         \  46
              ENDCODE

