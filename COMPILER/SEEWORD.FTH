\ simple "see" utility to dump XTs and the word header.

\ we don't want to see TARGETS words
 ONLY FORTH  ALSO COMPILERS DEFINITIONS  FORTH-REPL

HEX 045A CONSTANT 'NEXT'
T' EXIT  VALUE 'EXIT' \ executor must be loaded ??

: XT>NFA  ( cfa -- nfa | 0 )
\            2-           \ cfa-2 gets to end of name text
            BEGIN
              2- DUP C@L  \ dect the address and fetch the byte
              0FC AND    \ mask out immediate bit and check byte count
            0= UNTIL     \ 0 means we found immediate field address
            1+ ;         \ +1 byte puts us at nfa
HEX
: U.####   0 <#  # # # #  #> TYPE ;
: .DATA    U.####  [CHAR] , EMIT ;
: END? ( u -- ) DUP 'NEXT' = SWAP 'EXIT' = OR ;

\ : .LINK  ( addr -- addr') CR ." Link= " @L U. ;
: T.ID  ( Tnfa -- ) TCOUNT 1F AND  TTYPE CR ;
\ : .NAME  ( addr -- addr)  ." Name= "  T.ID ;
\ : .HEADER ( addr -- addr) XT>NFA DUP>R NFA>LFA .LINK  3 SPACES R> .NAME ;

: CODEWORD? ( xt -- ?)  DUP @L - -2 = ;

: SEECODE ( xt --) \ crude dump of the contents of a word up to NEXT or EXIT
  DUP CR ." CODE " XT>NFA T.ID
  CR
  BEGIN
     DUP @L \ read data from SAM memory
     END?
  0= WHILE
     DUP @L .DATA VCOL @ 20 > IF CR THEN
     CELL+
     ?TERMINAL ABORT" SEE halted"
  REPEAT
  @L CR .DATA
  CR ." ENDCODE"
;

: SEECOLON ( xt -- ) \ crude dump of the contents of a word up to NEXT or EXIT
  DUP  @L  T['] DOCOL <> ABORT" Not a colon definition"
  CR ." : "  DUP @L XT>NFA T.ID
  CR
  BEGIN
     DUP @L \ read data from SAM memory
     END?
  0= WHILE
     DUP @L .DATA VCOL @ 20 > IF CR THEN
     CELL+
     ?TERMINAL ABORT" SEE halted"
  REPEAT
  @L .DATA
  CR ." ;"
;

: SEE ( <name> )
    T'
    DUP CODEWORD? IF SEECODE ELSE SEECOLON THEN ;

COMPILER
