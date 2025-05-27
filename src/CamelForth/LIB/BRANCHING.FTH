CR .( branch cross-compilers if then else etc...)

\ branching primitives go into the program image
\ *branches are IP relative (not absolute)
TARGET
CODE ?BRANCH \ branch if TOS = 0
        TOS DEC,
        TOS POP,
        NC IF,
          *IP IP ADD,
           NEXT,
        ENDIF,
        IP INCT,
        NEXT,
ENDCODE

CODE BRANCH \ unconditional branch
        *IP IP ADD,
        NEXT,
ENDCODE

COMPILER

\ NoteHOST:
\ This is one of the thornier bits of a Forth cross-compiler
\ that took a while to get straight in my head.
\ TCOMPILE is used to compile a word into a TARGET definition.
\ POSTPONE must be used to compile COMPILER words into COMPILER definitions

\                        >> important <<
\ *DURING TARGET COMPILATION META WORDS ARE FOUND FIRST IN THE SEARCH ORDER*
COMPILER  ALSO META DEFINITIONS \ put these names in META
HOST: <BACK    ( addr --) THERE  -    T, ;HOST
HOST: AHEAD    ( -- addr) THERE     0 T, ;HOST

HOST: BEGIN         THERE  ;HOST IMMEDIATE
HOST: IF            TCOMPILE ?BRANCH  AHEAD ;HOST IMMEDIATE
HOST: THEN          THERE OVER - SWAP T! ;HOST IMMEDIATE \ resolves a branch
HOST: ELSE          TCOMPILE  BRANCH  AHEAD SWAP POSTPONE THEN ;HOST IMMEDIATE
HOST: UNTIL         TCOMPILE ?BRANCH  <BACK ;HOST IMMEDIATE
HOST: AGAIN         TCOMPILE  BRANCH  <BACK ;HOST IMMEDIATE
HOST: WHILE         POSTPONE IF  SWAP  ;HOST IMMEDIATE
HOST: REPEAT        POSTPONE AGAIN  POSTPONE THEN ;HOST IMMEDIATE

\ "NOT-UNTIL"   faster than 0= UNTIL
HOST: -UNTIL        POSTPONE WHILE POSTPONE REPEAT ;HOST IMMEDIATE


COMPILER
