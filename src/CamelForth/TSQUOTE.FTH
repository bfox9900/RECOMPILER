\ Words to perform S" and ." in the target memory

COMPILER
\ moved to RECOMPILER core.
\ FORWARD '(S")
\ FORWARD 'TYPE

: "PARSE  [CHAR] " PARSE ;
: TS"      ?COMP  '(S") T,  "PARSE TS, ; IMMEDIATE
: T."      ?COMP  '(S") T,  "PARSE TS, 'TYPE T, ; IMMEDIATE
