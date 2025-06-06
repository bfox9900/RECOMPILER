\ Make Forth kernel primivives by stealing code from HOST 
CR .( Stealing code from the kernel ...)

COMPILER WARNINGS OFF 

\   **********************************************************
CR .( FORTH PRIMITIVES BEGIN)

\ DODOES is the code action of a DOES> clause.
\ For ITC Forth:
\ defined word:  CFA: doescode
\                PFA: parameter field
\
\ Note: In V2.5 implementation we used BL @DODOES. (branch and link)
\ DODOES is entered with W=PFA (parameter field address)
\ DODOES moves register W to the TOP of Stack register. (R4 is TOS)
\ So the high-level Forth code begins with the address of the parameter
\ field on top of stack.
\ Using BL automatically computed the new parameter field into R11
\ which is exactly what we need to be the IP so we just do one MOV.
TARGET
TCREATE DODOES  ( -- a-addr)
            TOS PUSH,       \ save TOS reg on data stack      28
            W TOS MOV,      \ put defined word's PFA in TOS   14
            IP RPUSH,       \ push old IP onto return stack   28
            R11 IP MOV,     \ R11 has the new PFA -> IP       14
            NEXT,                                       \     84
            ENDCODE
FORWARD 'DODOES
T' DODOES RESOLVES 'DODOES    \ 'DODOES is used by the cross-compiler

\ ***********************end of executors *************************

\ =============[ Forth code primitives begin ] ====================

CODE PERFORM  ( xt -- )
            *TOS W MOV,
            1 $ JMP,
+CODE EXECUTE ( 'xt -- )     \ fetch the XT from an address
            TOS W MOV,       \ move the execution token in TOS into temp
1 $:        TOS POP,         \ refill TOS
            *W+ R5 MOV,      \ move contents of the XT (the address of the doer routine) to R5
            *R5 B,           \ branch to the address of the DOER routine
             ENDCODE

\ these run from high speed RAM
\ CODE ?BRANCH
\            _?BRANCH >HSRAM CFA!  \ put fast address into the CFA
\            ENDCODE

\ CODE BRANCH
\            _BRANCH >HSRAM CFA!   \ put fast address into the CFA
\            ENDCODE

\ ==================================================================
CR .( MEMORY FETCH & STORE)

CODE !      ( n addr -- )
            *SP+ *TOS MOV,   \ 26
            TOS POP,         \ 22
             NEXT,           \ = 48
             ENDCODE

CODE @      ( a -- w )
\            _fetch >HSRAM CFA! 
            *TOS TOS MOV,
            NEXT,
            ENDCODE

\ Aug 4 2018: fixed order of data to be ANS compliant
CODE 2!     ( d addr -- )
            *SP+ *TOS  MOV,    \ the top # of D is stored at the lower adrs
            *SP+ 2 (TOS) MOV,  \ next # stored 1 cell higher (addr+2)
            TOS POP,
            NEXT,
            ENDCODE

\ Aug 4 2018: fixed order of data to be ANS compliant
CODE 2@     ( addr -- d)  \ the lower address will appear on top of stack
            2 (TOS) PUSH, \    38
            *TOS TOS MOV, \    28
            NEXT,         \ =  66
            ENDCODE

CODE C!     ( c addr -- )
            1 (SP) *TOS MOVB,     \  26
            SP INCT,              \  10   inc. stack pointer by 2
            TOS POP,              \  22
            NEXT,                 \ =58
            ENDCODE

CODE COUNT  ( addr -- addr' u)
            TOS PUSH,             \ make a copy of addr
            *SP INC,              \ inc. addr past the count byte
L: _C@      *TOS TOS MOVB,        \ put C@ inline to save space
            TOS 8 SRL,            \ **BUG FIX** used to be SRA :-(
            NEXT,
            ENDCODE

CODE C@     _C@  -2 TALLOT THERE T! ENDCODE      \ give _C@ a dictionary header

CODE +!     ( n addr --)
            *SP+ *TOS ADD,         \ 14+8+4 = 26 cycles
            TOS POP,               \          22 cycles
            NEXT,                  \ -------------------
            ENDCODE                \          48 cycles

CODE C+!    ( c addr -- ) \ 8 bytes versus 12 bytes in Forth
            1 (SP) *TOS ADDB,      \ 26
            SP INCT,               \ 10
            TOS POP,               \ 22
            NEXT,                 \ =58
            ENDCODE

\ ==================================================================
CR .( RETURN STACK)

CODE RP@    ( -- a )
            TOS PUSH,
            RP TOS MOV,
            NEXT,
            ENDCODE

CODE RP!    ( a -- )
            TOS RP MOV,
            TOS POP,
            NEXT,
            ENDCODE

CODE >R     ( w -- )
            TOS RPUSH,        \ 28
            TOS POP,          \ 22
            NEXT,
            ENDCODE          \ 50

CODE R>     ( -- w )
            TOS PUSH,         \ 28
            TOS RPOP,         \ 22
            NEXT,
            ENDCODE          \ 50

\ CODE RDROP  RP INCT, NEXT, ENDCODE            

CODE R@     ( -- w )
            TOS PUSH,
            *RP TOS MOV,
            NEXT,
            ENDCODE

\ double Rstack Forth2012
CODE 2>R    ( d -- ) ( r-- n n)
      RP  -4 AI,          \ 14
      TOS    *RP MOV,      \ 22
      *SP+ 2 (RP) MOV,     \ 26
      TOS POP,             \ 22
      NEXT,              \ = 84
ENDCODE

CODE 2R>     ( -- d )
      TOS PUSH,            \ 28
      SP DECT,             \ 10
      TOS RPOP,            \ 22
      *SP  RPOP,            \ 26
      NEXT,              \ = 88
ENDCODE

\ ==================================================================
CR .( DATA STACK)

CODE SP@    ( -- a )
            TOS PUSH,
            SP TOS MOV,
            NEXT,
            ENDCODE

CODE SP!    ( a -- )
            TOS SP MOV,
            TOS POP,
            NEXT,
            ENDCODE

CODE DROP   ( w -- )
\            _drop >HSRAM CFA!
           TOS POP,             \ 22 *SP+ TOS MOV,
           NEXT,
            ENDCODE

CODE NIP    ( n n' -- n')
           SP INCT,             \ 10
           NEXT,
           ENDCODE

CODE ?DUP   ( x -- 0 | x x)       \ ?DUP interleaved with DUP to save space
            TOS TOS MOV,          \ test for TOS<>0
            NE IF,                \ if TOS<>0
                TOS PUSH,        \ dup it
            ENDIF,
            NEXT,
            ENDCODE

CODE DUP    ( w -- w w )          \ create a Forth Header for DUP
\            _DUP >HSRAM CFA!       \ use hi-speed ram version
           TOS PUSH,
           NEXT,
            ENDCODE

CODE SWAP   ( w1 w2 -- w2 w1 )
            TOS   W MOV,          \ 14
            *SP  TOS MOV,          \ 18
            W   *SP MOV,          \ 18
            NEXT,                 \ 50
            ENDCODE

CODE OVER   ( w1 w2 -- w1 w2 w1 )
            TOS PUSH,             \ 28
            2 (SP) TOS MOV,       \ 22
            NEXT,                 \ 50
            ENDCODE

\ CODE OVERSWAP  ( w1 w2 -- w1 w1 w2)
\              SP  DECT,
\              2 (SP)  *SP MOV,
\              NEXT,
\              ENDCODE

CODE ROT    ( n1 n2 n3 --  n2 n3 n1)
            2 (SP)    W MOV,      \ move bottom n1 to TEMP             14+8 =   22
            *SP  2 (SP) MOV,      \ move n2 to bottom position         14+4+8 = 26
            TOS     *SP MOV,      \ n3 moves to second                 14+4   = 18
            W       TOS MOV,      \ n1 goes to top of stack register   14+0   = 14
            NEXT,                 \                                             80
            ENDCODE

CODE -ROT   ( n1 n2 n3 --  n2 n3 n1)
            TOS       W MOV,
            *SP      TOS MOV,
            2 (SP)  *SP MOV,
            W    2 (SP) MOV,
            NEXT,
            ENDCODE

\ byte/word conversions
CODE ><     ( n -- n )           \ swap bytes in n
            TOS SWPB,            \ handy and only 1 instruction
            NEXT,
            ENDCODE

\ used in number conversion. Same size as DROP DROP 3X faster
CODE 2DROP  ( n n -- )
            SP INCT,           \ 10
            TOS POP,           \ 28
            NEXT,            \ = 38
            ENDCODE

\ 3 instructions 62 clocks vs 6 instructions 100 clocks for OVER OVER 
CODE 2DUP  ( n1 n2 -- n1 n2 n1 n2 )
            SP -4 ADDI,       \  14   make room for 2 cells
            4 (SP) *SP MOV,   \  26   n1->*SP
            TOS  2 (SP) MOV,  \  22   n2 -> next stack location
            NEXT,            \   62   
            ENDCODE

CODE 2SWAP  ( n1 n2 n3 n4-- n3 n4 n1 n2)
             TOS       R0 MOV,    \ n4  14
            *SP        R1 MOV,    \ n3  14
            2 (SP)   TOS MOV,    \ n2  22
            4 (SP)   *SP MOV,    \ n1  26
            R0    2 (SP) MOV,    \     22
            R1    4 (SP) MOV,    \     22
            NEXT,                \    120
            ENDCODE

CODE PICK   ( n -- n)   \ GForth ITC takes 8 intel instructions for PICK
              TOS  1 SLA,   \ 14  (" n CELLS")
              SP TOS ADD,    \ 14
             *TOS TOS MOV,   \ 18
              NEXT,         \  46
              ENDCODE

0 [IF] \ From Wil Baden's TOOLBELT  Thanks Wil. RIP

CODE 3RD  ( a b c d --  a b c d b)    \ 3X faster than 3 PICK
              TOS PUSH,         \ 28
              4 (SP) TOS MOV,   \ 22
              NEXT,            \  50   \ same speed as OVER
              ENDCODE

CODE 4TH
              TOS PUSH,
              6 (SP) TOS MOV,
              NEXT,             \ same speed as OVER
              ENDCODE
[THEN]

\ ==================================================================
CR .( LOGICAL OPERATIONS)

CODE AND    ( w w -- w )
            *SP INV,                  \ thanks to Mark Wills!   10
            *SP+ TOS SZC,             \ RTFM for TMS9900 BF     22
            NEXT,                                            \ =32
            ENDCODE

CODE OR     ( w w -- w )  *SP+ TOS OR,   NEXT, ENDCODE
CODE XOR    ( w w -- w )  *SP+ TOS XOR,  NEXT, ENDCODE

\ ==================================================================
CR .( ARITHMETIC)
\ simple math is about 40% faster with TOS in register
CODE 1+     ( n -- n') TOS INC,       NEXT, ENDCODE
CODE 1-     ( n -- n') TOS DEC,       NEXT, ENDCODE
CODE 2+     ( n -- n') TOS INCT,      NEXT, ENDCODE
CODE 2-     ( n -- n') TOS DECT,      NEXT, ENDCODE

CODE 2*     ( n -- n)   TOS 1 SLA,    NEXT, ENDCODE
CODE 4*     ( n -- nx4) TOS 2 SLA,    NEXT, ENDCODE
CODE 8*     ( n -- nx8) TOS 3 SLA,    NEXT, ENDCODE

CODE 2/     ( n -- n)   TOS 1 SRA,    NEXT, ENDCODE

\ =================================================================
CR .( ANS Shift operations)

CODE RSHIFT ( n bits -- n')  \ shift right logical. ANS/ISO Forth requirement 
            TOS R0 MOV,      \ the shift bits MUST be in R0 to do this
            TOS POP,
            R0 R0 MOV,       \ ANS:  1 0 LSHIFT -> 1
            NE IF,           \       so skip the shift if R0=1
               TOS R0 SRL,
            ENDIF,
            NEXT,
            ENDCODE

CODE LSHIFT ( n bits -- n') \ shift left arithmetic
            TOS R0  MOV,    \ the shift bits MUST be in R0 to do this
            TOS POP,
            R0 R0 MOV,
            NE IF,
               TOS R0 SLA,
            ENDIF,
            NEXT,
            ENDCODE

CODE INVERT  ( u -- w)
              TOS INV,              \ 10
              NEXT,
              ENDCODE

\ : +   ( u1 u2 -- u ) UM+ DROP ;  original Camel Forth code is bigger
CODE +      ( u1 u2 -- u )
             _PLUS >HSRAM CFA!
\            *SP+ TOS ADD,       \ ADD 2nd item to TOS and incr stack pointer.
\           NEXT,
            ENDCODE

CODE -       ( u1 u2 -- u )
            *SP+ TOS SUB,
            TOS NEG,    \ sign is wrong when subtracting memory from register
            NEXT,
            ENDCODE

\ D+ is a better primitive for 9900 CPU than M+
CODE D+   ( lo hi lo' hi' -- d)
            *SP+    R0  MOV,
            *SP+    TOS ADD,  \ add hi #s
            R0     *SP  ADD,  \ add lo #s
            OC IF,            \ carry set?
               TOS INC,       \ incr hi
            ENDIF,
            NEXT,
            ENDCODE

CODE ABS    ( n -- n )
            TOS ABS,
            NEXT,
            ENDCODE

CODE NEGATE ( n -- n )
            TOS NEG,
            NEXT,
            ENDCODE

CODE ALIGNED ( n -- n)
            TOS INC,
            TOS FFFE ANDI,
            NEXT,
            ENDCODE

\ ==================================================================
CR .( MULTIPLY AND DIVIDE)

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


\ cross-compiler creates a variable to control floored or symmetrical division
VARIABLE FLOOR

CODE M/MOD  ( lsb msb n3 -- rem quot)
       TOS R3 MOV,             \ DUP for sign of denominator
       R1     POP,             \ POP the high word of ud to r1
       R1  W  MOV,             \ DUP for sign of numerator
       R1  R5 MOV,             \ DUP 2nd copy symmetric sign
      *SP  R2 MOV,             \ move low word of ud to r2 (keep stack pos.)

          TOS ABS,             \ force denominator positive
       R1  -1 CMPI,            \ check sign of numerator
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
1 $:   R2 0 CMPI,
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

\ ==================================================================
\ array operation nice in theory, but not that important
0 [IF]
CODE *+     ( addr ndx size -- addr')  \ array address calculator
             *SP+ TOS MPY,              \ 60
             *SP+ TOS MOV,              \ 22
              R5  TOS ADD,              \ 14
              NEXT,                    \ =96
              ENDCODE
[THEN]

\ ==================================================================
cr .( COMPARISONS)

CODE 0=     ( n -- ?)         \ used 6X in kernel
            TOS NEG,
            NE IF,
                TOS SETO,
            ENDIF,
            TOS INV,
            NEXT,
            ENDCODE

CODE OVER=  ( n1 n2 -- n1 ?)
            *SP TOS CMP,     \ used 2x in kernel & in CASE
            1 $ JMP,
+CODE =     ( n n -- ?)     \ Used 4X in Kernel
            *SP+ TOS CMP,
1 $:        TOS CLR,
            EQ IF,
               TOS SETO,
            ENDIF,
            NEXT,
ENDCODE

\ CODE 2DUPXOR  ( u u -- u u ? ) \ useful for strings in WHILE loops
\            TOS PUSH,
\            2 (SP) TOS XOR,
\            NEXT,
\            ENDCODE

CODE 0<     ( n -- flag )
            TOS TOS MOV,  \ **MUST USE MOV HERE**
            TOS SETO,
            1 $ JLT,
            TOS CLR,
1 $:        NEXT,
            ENDCODE

CODE U<     ( n1 n2 -- flag)
            *SP+ TOS CMP,
            TOS SETO,
            1 $ JL,
            TOS CLR,
1 $:        NEXT,
            ENDCODE

CODE >      ( n1 n2 -- flag)
            *SP+ TOS CMP,
            TOS SETO,
            1 $ JGT,
            TOS CLR,
1 $:        NEXT,
            ENDCODE

CODE <      ( n1 n2 -- flag)
            *SP+ TOS CMP,
            TOS SETO,
            1 $ JLT,
            TOS CLR,
1 $:        NEXT,
             ENDCODE

\ ==================================================================
CR .( MIN & MAX )  \ added NIP HEADER. saves 6 bytes 
CODE MIN    ( n1 n2 -- n)
            *SP TOS CMP,    \ compare n1 & n2
            1 $ JLT,       \ if n1 < n2 goto 1 $:
            3 $ JMP,
+CODE MAX   ( n1 n2 -- n)
            *SP  TOS CMP,   \ compare n1 & n2
            2 $ JGT,       \ if n1 > n2 goto 1 $:
3 $:        SP INCT,       \ otherwise NIP n1
            NEXT,
1 $: 2 $:   TOS POP,       \ DROP n1, refill TOS
            NEXT,
            ENDCODE

\ ==================================================================
CR .( MOVE FILL )
CODE MOVE   ( src dst n -- )   \ forward character move
            *SP+ R0  MOV,      \ pop DEST into R0
            *SP+ R1  MOV,      \ pop source into R1
            TOS TOS MOV,
            NE IF,            \ if n=0 we are done
\ need some copies
                R0  R2 MOV, \ dup dest
                R0  R3 MOV, \ dup dest
                TOS R3 ADD, \ R3=dest+n
\ test window:  src  dst dst+n WITHIN
                R0  R3 SUB,
                R1  R2 SUB,
                R3  R2 CMP,
                HI IF, \ do cmove> ............
                
                    TOS W MOV,      \ dup n
                        W DEC,      \ compute n-1
                    W  R1 ADD,      \ point to end of source
                    W  R0 ADD,      \ point to end of destination
                    BEGIN,
                      *R1 *R0 MOVB,
                       R1 DEC,     \ dec source
                       R0 DEC,     \ dec dest
                       TOS DEC,    \ dec the counter in TOS (R4)
                    EQ UNTIL,

                ELSE,  \ do cmove .............
                    BEGIN,
                      *R1+ *R0+ MOVB, \ byte move, with auto increment by 1.
                       TOS DEC,        \ we can test it before the loop starts
                    EQ UNTIL,
                ENDIF,
            ENDIF,
            TOS POP,
            NEXT,
            ENDCODE
0 [IF]
CODE FILL   ( addr cnt char -- )
            *SP+ R0 MOV,      \ pop cnt->R0                   22
            *SP+ R1 MOV,      \ pop addr->R1                  22
            TOS SWPB,         \                               10
            BEGIN,
              TOS *R1+ MOVB,  \ char is in TOS                20
              R0 DEC,         \ decr. count                   10
            EQ UNTIL,         \ loop until r0=0               10
            TOS POP,          \ refill the TOS register       22
            NEXT,             \                             =116
            ENDCODE
[ELSE]            
CODE FILL   ( addr cnt char -- )
            *SP+ R0 MOV,      \ pop cnt->R0                   22
            *SP+ R1 MOV,      \ pop addr->R1                  22
            TOS SWPB,         \                               10
            BEGIN,
              R0 DEC,         \ decr. count                   10
            OC WHILE, 
              TOS *R1+ MOVB,  \ char is in TOS                20
            REPEAT, 
            TOS POP,          \ refill the TOS register       22
            NEXT,             \                             =116
            ENDCODE
[THEN]

0 [IF]
CODE ERASE ( addr cnt  -- )  \ 2x faster than 0 FILL
           *SP+ R1 MOV,
           BEGIN,
              TOS DECT,
           OC WHILE,   
              *R1+ CLR,
           REPEAT,
           TOS POP,
           NEXT,
           ENDCODE
[THEN]

\ =================================================================
cr .( ANS Forth DO/LOOP )

\  D O   L O O P S

\ Adapted from CAMEL Forth MSP430
\ ; '83 and ANSI standard loops terminate when the boundary of
\ ; limit-1 and limit is crossed, in either direction.  This can
\ ; be conveniently implemented by making the limit 8000h, so that
\ ; arithmetic overflow logic can detect crossing.  I learned this
\ ; trick from Laxen & Perry F83.

\ ===========================================================
\ conventional do loops use 2 cells on the RSTACK
[CC] cr .( Rstack based DO/LOOP ) [TC]

CODE <?DO>  ( limit ndx -- )
            *SP TOS CMP,        \ compare 2 #s
            1 $ JNE,            \ if they are not the same jump to regular 'do.' (BELOW)
            TOS POP,            \ remove limit
            TOS POP,            \ refill TOS
            IP RPOP,
            NEXT,

+CODE <DO>  ( limit indx -- )
1 $:        R0  8000 LI,        \ load "fudge factor" to LIMIT
            *SP+ R0  SUB,       \ Pop limit, compute 8000h-limit "fudge factor"
            R0  TOS ADD,        \ loop ctr = index+fudge
            R0  RPUSH,
            TOS RPUSH,
            TOS POP,            \ refill TOS
            NEXT,
ENDCODE

CODE <+LOOP>
            TOS *RP ADD,        \ save space by jumping into <loop>
            TOS POP,            \ refill TOS, (does not change overflow flag)
            2 $ JMP,
+CODE <LOOP>
            *RP INC,            \ increment loop
2 $:        1 $ JNO,            \ if no overflow then loop again
            IP INCT,            \ move past (LOOP)'s in-line parameter
            3 $ JMP,            \ jump to UNLOOP
1 $:        *IP IP ADD,         \ jump back
            NEXT,

+CODE UNLOOP
3 $:        RP  4 ADDI,         \ collapse rstack frame
            NEXT,
ENDCODE

CODE I      ( -- n)
            TOS PUSH,        
            *RP    TOS MOV, 
            2 (RP) TOS SUB,    
            NEXT,             
            ENDCODE

CODE J      ( -- n)
            TOS PUSH,
            4 (RP) TOS MOV,   \ outer loop index is on the rstack
            6 (RP) TOS SUB,   \ index = loopindex - fudge
            NEXT,
            ENDCODE
[THEN]

CODE BOUNDS ( adr len -- adr2 adr1)  \ same size as Forth version
            *SP R1 MOV,
            TOS *SP ADD,
            R1 TOS MOV,
            NEXT,
            ENDCODE


\ ===========================================================

CR .( SKIP SCAN ) 

\ ; Although SKIP, SCAN, and S= are perhaps not the ideal factors
\ ; of WORD and FIND, they closely follow the string operations
\ ; available on many CPUs, and so are easy to implement and fast.

TARGET
CODE SKIP  ( c-addr u char -- c-addr' u')    \ skip matching chars
        TOS SWPB,           \ char stays in TOS
        R1  POP,            \ POP count into R1,
        *SP W MOV,         \ address->w
        BEGIN,
          R1 R1 MOV,
        NE WHILE, ( len<>0)
            *W TOS CMPB,
        EQ WHILE, ( *W=tos)
         ( do: 1 \STRING )
             W INC,        \ inc. adr
             R1 DEC,       \ dec. len
        REPEAT,
        ENDIF,
        W *SP  MOV,        \ store updated address on stack
        R1 TOS MOV,        \ updated count to TOS
        NEXT,              \ 26 bytes
        ENDCODE

CODE SCAN  ( adr len char -- adr' len' )    \ find matching char
        TOS SWPB,           \ char stays in TOS
        R1  POP,            \ POP count into R1,
        *SP  W  MOV,        \ address->w
        BEGIN,
          R1 R1 MOV,
        NE WHILE, ( len<>0)
            *W TOS CMPB,
        NE WHILE, ( *w<>tos)
         ( do: 1 \STRING )
             W INC,        \ inc. adr
             R1 DEC,       \ dec. len
        REPEAT,
        ENDIF,
        W *SP  MOV,        \ store updated address on stack
        R1 TOS MOV,        \ updated count to TOS
        NEXT,              \ 26 bytes
        ENDCODE

\ ===========================================================
[CC] CR .( DICTIONARY SEARCH)

\ We found the Camel Forth dictionary search to be a little slow on TI-99.
\ Creating (FIND) in assembler is smaller and about 5X faster than using
\ S= plus hi-level looping and address calculation

\ Usage:
\  FIND ( c-addr -- caddr  0) if not found
\                   xt     1) if immediate,
\                   xt    -1) if "normal"
\             LATEST @ (FIND) ;

\ By using counted strings for input we get the bonus of comparing
\ the length bytes AND exiting the string compare loop after 1 compare
\ if the lengths are not equal. This makes for a fast traversal of
\ the linked list.

\ We also chose to take advantage of the large TMS9900 register set.
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

CODE (FIND) ( Caddr NFA -- XT ? )
      TOS R3 MOV,                 \ R3 = NFA which is a counted string
      TOS CLR,                    \ TOS is the output flag, init to zero
     *SP R8 MOV,                  \ R8 = caddr which is a counted string
     *R8 R5 MOVB,                 \ caddr C@ -> R5 is string length
\      NE IF,                      \ if count<>0 do the search
          R5 8 SRL,               \ get the byte on the correct side right
          R5 INC,                 \ skip length byte
          BEGIN,
               \ load char compare registers
                 R5 R0 MOV,           \ load R0 with length of caddr string
                 R8 R1 MOV,           \ load R1 with caddr string address
                 R3 R2 MOV,           \ load R2 with the NFA to compare
               \ inner character comparator loop
                 BEGIN,
                   *R1+ *R2+ CMPB,    \ compare char by char including the length byte
                    1 $ JNE,          \ ANY mismatch found, goto 1 $
                    R0 DEC, EQ        \ decr. loop counter
                 UNTIL,               \ loop until R0=0
                 \ we found the word !!!
                 \ convert NFA in R3 to CFA -> R2
                  R3 R2 MOV,              \ if found R3 has a name field address (NFA), copy to R2
                 *R3 R0 MOVB,             \ get the length of the name to R0
                  R0 SWPB,                \ fix the #$%!@$ byte order again
                  R0 R2 ADD,              \ add length to R2, gets past the string to the CFA
                  R2 INCT,                \ inc 1 for the count byte, 1 more for even address evaluation
                  R2 -2 ANDI,             \ align R2 to even address boundary

                \ test for immediate or normal word -> TOS
                  TOS SETO,               \ we found a word so set TOS to true
                 -1 (R3) R0 MOVB,        \ R3 has the NFA. Fetch NFA-1 (immediate field)

                \ Oct 2020 added mask to support hashed threads
                  R0 0100 ANDI,          \ test the immediate bit, (mask the hash code)
                  NE IF,
                       TOS NEG,          \ if non zero negate the TOS from -1 to 1
                  ENDIF,                 \ and head for home
                  R2 *SP MOV,            \ replace Caddr with the found XT in R2
                  NEXT,                  \ Return to Forth

                \ traverse link list to next NFA
1 $:            -3 (R3) R3 MOV, EQ    \ fetch LFA, R3 now has new NFA
          UNTIL,
 \     ENDIF,
      NEXT,                    \ we got zero. End of the list! Go back to Forth
      ENDCODE                  \ 42 BYTES

\ ===========================================================
CR .( CAMEL99 custom primitives)
\ ===========================================================
[CC] CR .( ON / OFF) 

TARGET 
CODE ON   ( adr -- )
          *TOS SETO, 
          TOS POP,
          NEXT,
          ENDCODE

CODE OFF  ( adr -- )
          *TOS CLR, 
          TOS POP, 
          NEXT, 
          ENDCODE 

\ multi-tasking support
[TC]
CODE PAUSE  ( -- )  
            NEXT2 CFA! 
            ENDCODE

\ used in sprite control and very handy for byte/cell manipulation
 CODE SPLIT ( AABB --  BB AA )  \ split cell into 2 bytes
            TOS W MOV,  
            TOS 8 SRL, 
            W FF ANDI,  
            W PUSH,
            NEXT,
            ENDCODE

 CODE FUSE  ( BB  AA -- AABB )  
            TOS SWPB,
            *SP+ TOS ADD,
            NEXT,
            ENDCODE


CR .( Code primitives complete) 

COMPILER WARNINGS ON  