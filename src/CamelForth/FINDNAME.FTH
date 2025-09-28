COMPILER
CR .( FINDNAME in assembler)

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
