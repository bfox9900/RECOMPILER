\  DSK9.METADEFS      M E T A  D E F I N I T I O N S

\ META definitions look like Forth words but do cross-compiler actions
\ META wordlist is searched first while TARGET compiling, so these IMMEDIATE
\ words always run first.

COMPILER
CR .( compiling META Definitions )

COMPILER ALSO META DEFINITIONS

: HEX      HEX  ;
: DECIMAL  DECIMAL ;

HOST: S"      ?COMP  '(S") T,  [CHAR] " PARSE  TS, ;HOST IMMEDIATE
HOST: ."      ?COMP  '(S") T,  [CHAR] " PARSE  TS, 'TYPE T, ;HOST IMMEDIATE

HOST: [CHAR]      CHAR   TCOMPILE LIT  T, ;HOST IMMEDIATE

HOST: [COMPILE]   POSTPONE T[COMPILE]  ;HOST IMMEDIATE

HOST: [']         ( -- <name>)
    ?COMP
    T'             \ look up the XT of the TARGET word in the input stream
    'LIT T,  T,    \ store the address as a literal in the TARGET space
;HOST  IMMEDIATE


COMPILER
