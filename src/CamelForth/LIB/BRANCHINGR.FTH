CR .( branch cross-compilers if then else etc...)

COMPILER 
HOST: <BACK    ( addr --) THERE  -    T, ;HOST
HOST: AHEAD    ( -- addr) THERE     0 T, ;HOST

\ Note:
\ This is one of the thornier bits of a Forth cross-compiler
\ that took a while to get straight in my head. 
\ TCOMPILE is used to compile a word into a TARGET definition.
\ POSTPONE must be used to compile COMPILER words into COMPILER definitions

\                        >> important <<
\ *DURING TARGET COMPILATION META WORDS ARE FOUND FIRST IN THE SEARCH ORDER*
COMPILER ALSO META DEFINITIONS \ put these names in META 
HOST: BEGIN         THERE  ;HOST IMMEDIATE
HOST: IF            TCOMPILE ?BRANCH  AHEAD ;HOST IMMEDIATE
HOST: THEN          THERE OVER - SWAP T! ;HOST IMMEDIATE \ resolves a branch
HOST: ELSE          TCOMPILE  BRANCH  AHEAD SWAP POSTPONE THEN ;HOST IMMEDIATE
HOST: UNTIL         TCOMPILE ?BRANCH  <BACK ;HOST IMMEDIATE
HOST: AGAIN         TCOMPILE  BRANCH  <BACK ;HOST IMMEDIATE

HOST: FOR  ( n --)  TCOMPILE  >R   THERE ;HOST IMMEDIATE
HOST: NEXT ( -- )   TCOMPILE (NEXT)  <BACK  ;HOST IMMEDIATE

\ See the use of POSTPONE 
HOST: WHILE         POSTPONE IF  SWAP  ;HOST IMMEDIATE
HOST: REPEAT        POSTPONE AGAIN  POSTPONE THEN ;HOST IMMEDIATE

\ "NOT-UNTIL"   faster than 0= UNTIL 
HOST: -UNTIL        POSTPONE WHILE POSTPONE REPEAT ;HOST IMMEDIATE 


COMPILER 
