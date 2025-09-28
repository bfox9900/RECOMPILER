\ original Camel Forth code re-write to use COMPARE

TARGET

INCLUDE DSK1.LOWTOOLS

CODE COMPARE ( str1,n,str2 n -- 0:SAME, 1:GT, -1:LT)
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
    NEXT,
 ENDCODE

CODE NFA>NFA ( nfa -- nfa')  -3 (TOS) TOS MOV,  NEXT, ENDCODE

\ find string in dictionary
\                        xt     1  if immediate
\                        xt    -1  if "normal"
: (FIND) ( c-addr wid -- c-addr 0) \ if not found
    BEGIN                  \ -- a nfa
       2DUP                \ -- a nfa a nfa
       COUNT ROT COUNT     \ -- a nfa  nfa n a n
       COMPARE             \ -- a nfa f
    WHILE
        NFA>NFA            \ -- a nfa'
    REPEAT                 \ -- a nfa  OR  a 0

    DUP IF
       NIP DUP NFA>CFA     \ -- nfa xt
       SWAP  1- C@ 1 AND   \ -- xt iflag
       0= 1 OR
    THEN ;

: EXIT$  S" EXIT" PAD PLACE  PAD ;
: NFA    LATEST @ ;
