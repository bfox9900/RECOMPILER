CR .( branch cross-compilers if then else begin ...)

COMPILER 
: <BACK    ( addr --) THERE  -    T, ;
: FWD>     ( -- addr) THERE     0 T, ;

\ Note:
\ This is one of the thornier bits of a Forth cross-compiler
\ that took a while to get straight in my head. 
\ TCOMPILE is used to compile a word into a TARGET definition.
\ POSTPONE must be used to compile COMPILER words into COMPILER definitions

\                        >> important <<
\ *DURING TARGET COMPILATION META WORDS ARE FOUND FIRST IN THE SEARCH ORDER*
COMPILER 
: BEGIN         THERE  ; IMMEDIATE
: IF            TCOMPILE ?BRANCH  FWD> ; IMMEDIATE
: THEN          THERE OVER - SWAP T! ; IMMEDIATE \ resolves a branch
: ELSE          TCOMPILE  BRANCH  FWD> SWAP POSTPONE THEN ; IMMEDIATE
: UNTIL         TCOMPILE ?BRANCH  <BACK ; IMMEDIATE
: AGAIN         TCOMPILE  BRANCH  <BACK ; IMMEDIATE

\ Note the use of host's POSTPONE 
: WHILE         POSTPONE IF  SWAP  ; IMMEDIATE
: REPEAT        POSTPONE AGAIN  POSTPONE THEN ; IMMEDIATE
