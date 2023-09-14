CR .( branch cross-compilers if then else etc...)

COMPILER 
: <BACK    ( addr --) THERE  -    T, ;
: AHEAD    ( -- addr) THERE     0 T, ;
: (THEN) ( addr -- ) THERE OVER - SWAP T! ; \ resolve a branch

\ ** important ** 
COMPILER ALSO META DEFINITIONS \ put these names in META 
: BEGIN         THERE  ; IMMEDIATE
: THEN          (THEN) ; IMMEDIATE
: IF            TCOMPILE ?BRANCH  AHEAD ; IMMEDIATE
: ELSE          TCOMPILE  BRANCH  AHEAD SWAP POSTPONE THEN  ; IMMEDIATE
: UNTIL         TCOMPILE ?BRANCH  <BACK ; IMMEDIATE
: AGAIN         TCOMPILE  BRANCH  <BACK ; IMMEDIATE

: WHILE         POSTPONE IF  SWAP  ; IMMEDIATE
: REPEAT        POSTPONE AGAIN  (THEN)  ; IMMEDIATE

COMPILER 
