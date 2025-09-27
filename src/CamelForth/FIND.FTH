[CC] CR .( DICTIONARY SEARCH)

\ We found the Camel Forth dictionary search to be too slow on TI-99.
\ Creating (FIND) in assembler is smaller and about 5X faster than using
\ S= plus hi-level looping and address calculation

\ Usage:
\  FIND ( c-addr -- caddr  0) if not found
\                   xt     1) if immediate,
\                   xt    -1) if "normal"
\             LATEST @ (FIND) ;

\ We chose to take advantage of the large TMS9900 register set.
\ Therefore the input arguments are held in 3 registers and are
\ transferred to 3 new registers for the string compare loop.
\ This guarantees the fastest reload for each new string compare.

\ Register Usage
\ Inputs:  R3 = traverses NFAs in the Forth dictionary
\          R8 = address of the counted string we are looking for
\          R5 = length of the counted string in R8 + 1 byte

\ string compare loop
\          R0 = number of characters to compare(search string length+1)
\          R1 = address of the 1st string to compare
\          R2 = address of the second string to compare

\ Outputs: R2 = address of found string -OR- address of search string on stack
\          R4 = Forth TOS register. Holds the true/false result flag

\ For debugging
\ INCLUDE DSK1.LOWTOOLS

\ : *R8  R8 ** ;
\ : *R1+  R1 *+ ;
\ : *R2+  R2 *+ ;
\ : *R3   R3 ** ;
\ : (R3)  R3 () ;

\ : EXIT$  S" EXIT" PAD PLACE  PAD ;
\ : NFA    LATEST @ ;

TARGET
CODE FINDNAME ( Caddr wid -- Caddr nfa | 0 )
  *SP R8 MOV,             \ R8 = caddr which is a counted string
  *R8 R5 MOVB,            \ caddr C@ -> R5 is string length
  R5 8 SRL,               \ get the byte on the correct side right
  R5 INC,                 \ skip length byte
  BEGIN,
  \ load char compare registers
    R5 R0 MOV,           \ load R0 with length of caddr string
    R8 R1 MOV,           \ load R1 with caddr string address
    TOS R2 MOV,          \ load R2 with the NFA to compare
    \ inner character comparator loop
    BEGIN,
      *R1+ *R2+ CMPB,    \ compare char by char including the length byte
    EQ WHILE,
      R0 DEC,            \ decr. loop counter
    EQ UNTIL,            \ loop until R0=0
    NEXT,                \ we found the word, get out
    ENDIF,               \ mis-match. link to next word
    -3 (TOS) TOS MOV,    \ traverse link list to next NFA
  EQ UNTIL,              \ try again until link=0
  NEXT,
ENDCODE

: (FIND)
    FINDNAME
    DUP IF
       NIP DUP NFA>CFA       \ -- nfa xt
       SWAP 1- C@ 1 AND      \ -- xt immbit
       0= 1 OR               \ -- xt -1 | 1
    THEN ;
