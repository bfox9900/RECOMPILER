\ CROSS COMPILER Extensions for ITC system and data types

COMPILER

: NFA>LFA       ( nfa -- lfa)   3 - ;
: NFA>IMM       ( nfa -- precedence-field) NFA>LFA  TCELL+ ;
: >TBODY        ( cfa -- pfa ) CELL+ ;

\ TARGET WORD precedence field control
HOST: XIMMEDIATE  ( -- ) 1  TLAST @ 1- TC! ;HOST

\ test if a TARGET word is immediate
HOST: XIMMEDIATE? ( nfa -- ?) 1- TC@ 01 AND 0> ;HOST

COMPILER
CR .( TARGET DEFINING WORDS, 16 BIT MACHINE)

\ TEST
\ CODE DROP    *SP+ TOS MOV,  NEXT, ENDCODE
\ CODE DUP     TOS PUSH,       NEXT, ENDCODE

\ *************************************************************

\ RESOLVE TLITERAL for target compiler loop
COMPILER
:NONAME  STATE @ IF  TCOMPILE LIT  T,  THEN ; IS TLITERAL

\ These words are in the META vocabulary. When TARGET is invoked
\ they will be found before the same words in the HOST Forth system.
COMPILER
ALSO META DEFINITIONS
: CREATE     TCREATE  TCOMPILE DOVAR               DOES> @ >TBODY ;
: VARIABLE   TCREATE  TCOMPILE DOVAR    0 T,       DOES> @ >TBODY ;
: 2VARIABLE  TCREATE  TCOMPILE DOVAR    0 T, 0 T,  DOES> @ >TBODY ;
: CONSTANT   TCREATE  TCOMPILE DOCON ( n) T,       DOES> @ >TBODY T@ ;


\ USER variables are used in Multi-tasking Forth systems.
\ They are "local" to a specific task and usually reference by a pointer
\ called 'UP' (user pointer)
\ CAMEL99 does not have a user pointer. It uses the TMS9900 WP register
\ Cool trick, but it means we can't read a value from the TARGET system
\ for the cross-compiler to use.

\ So we have to add the value in the USER var to the workspace address.
\ In this case it is an EQU called WRKSP0 >8300
COMPILER   ALSO META DEFINITIONS
: USER ( n --)
  TCREATE  TCOMPILE DOUSER  T,  DOES> @ >TBODY T@  WRKSP0 + ;

COMPILER
