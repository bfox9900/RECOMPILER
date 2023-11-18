CR .( branch cross-compilers if then else etc...)

COMPILER 
: <BACK    ( addr --) THERE  -    T, ;
: AHEAD    ( -- addr) THERE     0 T, ;

\ Note:
\ This is one of the thornier bits of a Forth cross-compiler
\ that took a while to get straight in my head. 
\ TCOMPILE is used to compile a word into a TARGET definition.
\ POSTPONE must be used to compile COMPILER words into COMPILER definitions

\                        >> important <<
\ *DURING TARGET COMPILATION META WORDS ARE FOUND FIRST IN THE SEARCH ORDER*
COMPILER ALSO META DEFINITIONS \ put these names in META 
: BEGIN         THERE  ; IMMEDIATE
: IF            TCOMPILE ?BRANCH  AHEAD ; IMMEDIATE
: THEN          THERE OVER - SWAP T! ; IMMEDIATE \ resolves a branch
: ELSE          TCOMPILE  BRANCH  AHEAD SWAP POSTPONE THEN ; IMMEDIATE
: UNTIL         TCOMPILE ?BRANCH  <BACK ; IMMEDIATE
: AGAIN         TCOMPILE  BRANCH  <BACK ; IMMEDIATE

\ See the use of POSTPONE 
: WHILE         POSTPONE IF  SWAP  ; IMMEDIATE
: REPEAT        POSTPONE AGAIN  POSTPONE THEN ; IMMEDIATE

\ "NOT-UNTIL"   faster than 0= UNTIL 
: -UNTIL        POSTPONE WHILE POSTPONE REPEAT ; IMMEDIATE 


COMPILER 
