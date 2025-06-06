\ C R O S S - C O M P I L E R  B O O T - S T R A P P I N G
\ Final additions to the cross-compiler

\ NOTE: After all the primitives are compiled we use them to create the
\       final cross-compiler words for branching, looping and cross-
\       compiling versions of colon and semi-colon


COMPILER 
CR .( branch cross-compilers if then else etc...)
 : FWD>     ( -- addr) THERE    0 T, ;
 : <BACK    ( addr --) THERE  -   T, ;

: THEN          THERE OVER - SWAP T! ; IMMEDIATE
: BEGIN         THERE  ; IMMEDIATE
: IF            TCOMPILE ?BRANCH FWD> ; IMMEDIATE
: ELSE          TCOMPILE  BRANCH FWD> SWAP [COMPILE] THEN  ; IMMEDIATE
: UNTIL         TCOMPILE ?BRANCH <BACK ; IMMEDIATE
: AGAIN         TCOMPILE  BRANCH <BACK ; IMMEDIATE
: WHILE         [COMPILE] IF  SWAP  ; IMMEDIATE
: REPEAT        [COMPILE] AGAIN [COMPILE] THEN ; IMMEDIATE

\ finite loop cross-compilers
: DO            TCOMPILE <DO>  THERE ; IMMEDIATE
: ?DO           TCOMPILE <?DO> THERE ; IMMEDIATE

: LOOP          TCOMPILE <LOOP>  <BACK ; IMMEDIATE
: +LOOP         TCOMPILE <+LOOP> <BACK ; IMMEDIATE

\ : FOR         TCOMPILE >R  THERE ;  IMMEDIATE
\ : NEXT        TCOMPILE <NEXT> <BACK ;  IMMEDIATE

\ ========================================================================
\ C R O S S - C O M P I L I N G   C O L O N   &   S E M I - C O L O N
cr .( cross compiler ':' and ';')

\ In the following definition we use the HOST: ;HOST compiler words to create
\ new versions of : and ; that cross-compile FORTH words into the TARGET

host: :                     \ this is the CROSS compiler's ':'
        !CSP                \ record the "current stack position" (used later)
        ]T                  \ set XSTATE to true ie: turn on the compiler
        <colon:>            \ put the word name in the Target dictionary and the mirror vocabulary
        THIDE               \ set the 'smudge' bit so we can't find this word during compilation
        TINTERPRET          \ run the cross compiler/interpreter on the input stream
;host

?stk


host: ;                            \ this the CROSS-compiler's ';'
                TREVEAL            \ un-smudge the word so it can be found in the dictionary
                TCOMPILE EXIT 
                ?CSP               \ check is stack is the same place as when we started the definition
                T[                 \ reset XSTATE (TARGET STATE) to interpreting
;host   immediate

\ tail call optimizing semi-colon. 
\ Programmer discretion for when a word ends with another high-level Forth word. 
\ >>>DOES NOT WORK<<<
 [CC]
 host: -;  ( -- ) \ programmer controlled
      TREVEAL
      THERE 2- @ cell+           \ get previous XT, compute PFA
     -2 TALLOT                    \ erase the previous XT
      TCOMPILE BRANCH THERE - T,
      ?CSP
      T[                          \ turn off compiler
;host  IMMEDIATE

\ ========================================================================
\ F I N A L   T A R G E T -  C O M P I L I N G   C O L O N  &  S E M I - C O L O N
\ These alias names are used at the very end of the code to define the
\ FINAL ':' and ';' that will be the versions that actually run in the
\ TARGET Forth. (yes it's confusing) Renaming them allows me to remain sane.
\
SYNONYM x:   :                     \ "TARGET-compiling colon"
SYNONYM ;x   ;  IMMEDIATE          \ "TARGET-compiling semi-colon"

