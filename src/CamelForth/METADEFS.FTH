\                  M E T A  D E F I N I T I O N S

\ META definitions look like Forth words but do cross-compiler actions
\ META wordlist is searched first while TARGET compiling, so these IMMEDIATE
\ words always run first.

COMPILER ALSO META DEFINITIONS
CR .( compiling META Definitions )

: S"      ?COMP  '(S") T,  [CHAR] " PARSE TS, ; IMMEDIATE 
: ."      ?COMP  '(S") T,  [CHAR] " PARSE TS, 'TYPE T, ; IMMEDIATE   

: [']         T['] ; IMMEDIATE

: [CHAR]      ?COMP CHAR  POSTPONE TLITERAL ; IMMEDIATE

: [COMPILE]   POSTPONE  T[COMPILE] ; IMMEDIATE

\ : CREATE     TCREATE ;   
\ : ,        POSTPONE T,  ; IMMEDIATE 
\ : C,       POSTPONE TC, ; IMMEDIATE 

: TRUE   -1 ; IMMEDIATE 
: FALSE   0 ; IMMEDIATE 

COMPILER 
