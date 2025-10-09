COMPILER

CR .( FINDNAME in assembler)
NEEDS RPUSH,  FROM DSK1.LOWTOOLS

\ TARGET
CREATE _COMP$ ( str1,n,str2 n -- 0:SAME, 1:GT, -1:LT)
    R1 POP,   \ str2
    R0 POP,   \ len
    R2 POP,   \ str1
    TOS R0 CMP, \ compare lengths for fast escape
    TOS CLR,    \ clear output flag does not change CPU status
    1 $ JNE,    \ if lengths not same jump out
\ compare loop
    BEGIN,
        R0 DEC,
    OC WHILE,
        R1 *+ R2 *+ CMPB, \ compare & auto increment
    NE UNTIL,
\ mistmatch found
1 $:   LO IF, TOS INC, \ $1<$2
       ELSE,  TOS DEC, \ $1>$2
       ENDIF,
    ENDIF,  \ test all chars, match was good
    RT,

CODE COMPARE   _COMP$ @@ BL,  NEXT,  ENDCODE

: A$ S" NOW IS THE TIME FOR ALL GOOD MEN" ;
: B$ S" TO COME TO THE AID OF THEIR COUNTRY" ;


CODE FINDNAME ( Caddr wid -- Caddr nfa | 0 )
  *SP R8 MOV,             \ R8 = caddr which is a counted string
  R8 ** R5 MOVB,            \ caddr C@ -> R5 is string length
  R5 8 SRL,               \ get the byte on the correct side right
  R5 INC,                 \ include the length byte in the search
  BEGIN,
  \ load char compare registers
    R5 R0 MOV,           \ load R0 with length of caddr string
    R8 R1 MOV,           \ load R1 with caddr string address
    TOS R2 MOV,          \ load R2 with the NFA to compare
    \ inner character comparator loop
    _COMP$ @@ BL,
    NEXT,                \ we found the word, get out
    ENDIF,               \ mis-match. link to next word
    -3 (TOS) TOS MOV,    \ traverse link list to next NFA
  EQ UNTIL,              \ try again until link=0
  NE IF,
\ get precedent field into free register R5
        -1 (TOS) R5 MOVB, \ TOS has the NFA. Fetch NFA-1 (immediate field)

\ convert NFA in TOS to CFA -> *sp
        *TOS R0 MOVB,  \ get byte count
        R0 SWPB,       \ fix the byte order
        R0 TOS ADD,    \ add length to ADDR, gets past the string to the CFA
        TOS INCT,      \ inc 1 count byte, +1 for address evaluation
        TOS -2 ANDI,   \ align R2 to even address boundary

        TOS *SP MOV,

\ test for immediate or normal word -> TOS
        TOS SETO,  \ TOS = -1
        R5 0100 ANDI,  \ test the immediate bit, (mask the hash code)
        NE IF,
            TOS ABS, \ if non zero change TOS from -1 to 1
        ENDIF,       \ and head for home
    ENDIF,
    NEXT,        \ Return to Forth
ENDCODE

: EXIT$  S" EXIT" PAD PLACE  PAD ;
: TEST$  S" TEST" PAD PLACE  PAD ;
