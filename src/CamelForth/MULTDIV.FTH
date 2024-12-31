[CC] CR .( MULTIPLY AND DIVIDE)

TARGET 
CODE UM*    ( n n -- d)     \ 2 cells in -- 2 cells out
            *SP TOS MPY,    \ 52+4=56
            R5  *SP MOV,    \ 18
            NEXT,           \ 74
            ENDCODE

CODE *      ( n n -- n)      \ same size as  : *  UM* DROP ; but faster
            *SP+ R3 MOV,     \ 22
            TOS R3 MPY,      \ 52  cool trick ! result goes to R4
            NEXT,          \ = 74
            ENDCODE

CODE UM/MOD ( ud u1 -- u2 u3 ) \ numerator(32bits), divisor -- rem,quot
              TOS  R0 MOV,     \ divisor->R0                 14
             *SP+ TOS MOV,     \ POP high word into TOS      22
             *SP   R5 MOV,     \ MOVE low word to r5         18
              R0  TOS DIV,     \ perform unsigned division  124
              R5  *SP MOV,     \ push remainder              22
              NEXT,            \                            200
              ENDCODE

\ Signed divide using either Floored or Symmetric Integer Division.
\ Adapted with permission, from FBForth by Lee Stewart
\ This routine first does Symmetric Integer Division, then checks FLOOR
\ for whether we are doing Floored Integer Division.
\
\ Divides a 32 bit value in R1 and R2 by a 16 bit value in R0
\ Inputs:
\   TOS  denominator (divisor)
\   R1   MSB of numerator (dividend)
\   R2   LSB of numerator

\   TEMP   R3 sign of denominator
\   TEMP   W  sign of numerator
\   TEMP   R5 copy of numerator

\ VARIABLE
\    floor =floored/symmetric division flag passed by caller
\     0 = symmetric division
\    -1 = floored division

\ Outputs:
\   TOS=16-bit quotient (quot)
\   R2=16-bit remainder (rem)
\   set flags to reflect signs of operands, and force operands positive...


\ target variable to control floored or symmetrical division
VARIABLE FLOOR

CODE M/MOD  ( lsb msb n3 -- rem quot)
       TOS R3 MOV,             \ DUP for sign of denominator
       R1     POP,             \ POP the high word of ud to r1
       R1  W  MOV,             \ DUP for sign of numerator
       R1  R5 MOV,             \ DUP 2nd copy symmetric sign
      *SP  R2 MOV,             \ move low word of ud to r2 (keep stack pos.)

          TOS ABS,             \ force denominator positive
       R1  -1 CI,            \ check sign of numerator
       2 $ JGT,
           R1 INV,             \ DABS: invert numerator MSB and..
           R2 NEG,             \ ..negate numerator LSB
           OC IF,              \ if carry=TRUE
               R1 INC,         \ increment numerator MSB
           ENDIF,
2 $:   TOS R1 DIV,             \ perform the division. R1=quot, R2=rem

\ * Test for negative quotient
       R3 W  XOR,              \ compare signs of den and num
       1 $ JGT,
       R1  NEG,                 \ negate quotient
\ * check for remainder
1 $:   R2 0 CI,
       NE IF,                  \ if <>0
           R5  8000 ANDI,      \ test for negative numerator
           NE IF,              \ if signbit<>0
               R2 NEG,         \ rem. takes sign of num(symmetric)
           ENDIF,
\ * Handle floored division, if enabled
           FLOOR @@ R0 MOV,    \ symmetric or floored division?
           NE IF,              \ if <>0, do flooring
               W  8000 ANDI,   \ use XOR result to check num and den signs
               NE IF,
                   R1 DEC,     \ signs different, so floor quot
                   R3 R2 ADD,  \ rem = den + rem
               ENDIF,
           ENDIF,
       ENDIF,
       R1 TOS MOV,     \ quotient to tos
       R2 *SP MOV,     \ put remainder on open stack location
       NEXT,           \ we're outta here!
ENDCODE        \ 72 bytes
