\                  M E T A  D E F I N I T I O N S

\ META definitions look like Forth words but do cross-compiler actions
\ META wordlist is searched first while TARGET compiling, so these IMMEDIATE
\ words always run first.

COMPILER
CR .( compiling META Definitions )

COMPILER ALSO META DEFINITIONS
HOST: S"      ?COMP  '(S") T,  [CHAR] " PARSE TS, ;HOST IMMEDIATE 
HOST: ."      ?COMP  '(S") T,  [CHAR] " PARSE TS, 'TYPE T, ;HOST IMMEDIATE   

HOST: [CHAR]      CHAR   TCOMPILE LIT  T, ;HOST IMMEDIATE 

HOST: [COMPILE]   POSTPONE T[COMPILE]  ;HOST IMMEDIATE

HOST: TRUE   -1 ;HOST IMMEDIATE 
HOST: FALSE   0 ;HOST IMMEDIATE 


COMPILER 
