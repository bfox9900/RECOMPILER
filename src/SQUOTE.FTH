\ Words to perform S" and ." in the target memory 

COMPILER 
FORWARD '(S")
FORWARD 'TYPE

: "PARSE  [CHAR] " PARSE ; 
: TS"      ?COMP  '(S") T,  "PARSE TS, ; IMMEDIATE 
: T."      ?COMP  '(S") T,  "PARSE TS, 'TYPE T, ; IMMEDIATE   

\ interactive debugging tools 
: TCOUNT  ( sams$ -- sams len) DUP 1+ SWAP C@L  ;
: TTYPE   ( sams len -- ) OVER + SWAP ?DO I C@L EMIT LOOP  ;
