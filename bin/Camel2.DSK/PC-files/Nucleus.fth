CROSS-ASSEMBLING

        START.               \ sets a timer
        NEW.                 \ init target memory segment to FFFF
        CODESEG ORIGIN.
        TI-99.EA5

\ ==============================================================
\ Compiler extensions to manage copying certain primitives into hi-speed (HS) RAM
ORG-ADDR@ 2 cells + equ HSprims   \ beginning of the code to be copied to HS ram
               8388 equ HSstart   \ where we copy the code to in HS ram

: >HSRAM  ( label -- addr') HSprims -  HSstart + ;
\ *G converts assembler label to HS RAM address
: CFA!    ( addr -- )   -2 TALLOT  DATA,  ;
\ *G move dictionary pointer back 1 cell, patch 'addr' into CFA field

TALIGN
NEW-JMPTABLE
XSTATE OFF

l: _exit    IP RPOP,         \ >8388
l: _next                     \ Forth ITC NEXT routine (>838A)
@@9:        *IP+ W  MOV,     \ move CFA into Working register & incr IP ( 26)
            *W+  R5 MOV,     \ move contents of CFA to R5 & INCR W      ( 26)
            *R5  B,          \ branch to the address in R5              ( 12)
                                                                     (  = 64)

l: _enter   IP RPUSH,       \ push IP register onto the return stack    ( 36)
            W IP MOV,       \ move PFA into Forth IP register           ( 14)
            @@9 JMP,                                                    ( 10)

\ This code is for the most critical primitives.
\ They are copied into 16 bit RAM to run at full CPU speed.
l: _?branch \ Faster way to avoid penalty of TOS refill
            TOS DEC,        \ After DEC if tos=0, this will cause a carry flag                     10
            TOS POP,        \ refill TOS changes L> A> EQ flags, BUT does not change carry flag    22
            @@2 JOC,        \                                                                      10
l: _branch  *IP IP ADD,     \ *IP points to an offset number in memory. Add to IP causes a jump.
            @@9 JMP,        \ *replaced NEXT, with short JMP. 20% faster! same size                10
@@2:        IP INCT,        \ move IP forward past branch offset (don't branch)                    10
            @@9 JMP,        \ JMP to _next

l: _lit          TOS PUSH,   \ 28
            *IP+ TOS MOV,    \ 22
             @@9 JMP,

l: _fetch   *TOS TOS MOV,
            @@9 JMP,

l: _drop    TOS POP,
            @@9 JMP,

l: _DUP     TOS PUSH,
            @@9 JMP,

\ L: _incr   TOS INC,
\            @@9 JMP,

l: _PLUS    *SP+ TOS ADD,     \ ADD 2nd item to TOS and incr stack pointer.
            @@9 JMP,

l: _HSend    \ this label tells the compiler where the high speed code ends

\ ==============================================================
\ calculate and name the entry points for the fast memory code

\ * Address NEXT2 must be loaded into R10 at startup *
  ( see COLD in HILEVEL.HSF)
  _next  >HSRAM equ NEXT2   \ entry address of NEXT in fast RAM
  _HSend >HSRAM equ HSend   \ end of hi-speed memory code. Needed by copy loop in INIT

\ ==============================================================
\   ********  C O M P I L E R   C O N T R O L ****************

FALSE VALUE REGISTERS  \ do loop in R13,R14

\   **********************************************************
CR .( FORTH PRIMITIVES BEGIN)


\ ******************** EXECUTORS ARE SPECIAL CASES *************************
\ *           EXECUTOR word's CFA MUST start with runnable CODE            *
\ **************************************************************************

CODE EXIT    ( -- )         \ compiled when ';' is executed
      _exit >HSRAM CFA!
ENDCODE
 T' EXIT RESOLVES 'EXIT

CODE DOCOL   ( a -- )         \ Executor that executes a "colon" definition.
    _enter >HSRAM CFA!
ENDCODE
_enter >HSRAM  RESOLVES 'DOCOL

CODE LIT     ( -- w )         \ Execute a LITERAL number in the Forth program
           _LIT >HSRAM CFA!
ENDCODE
T' LIT RESOLVES 'LIT

( TCREATE: puts runnable code in the CFA. These are NOT Forth CODE words )
TCREATE: DOVAR  ( -- addr)    \ Executor that executes a "VARIABLE"
            TOS PUSH,        \ make room in TOS
            W TOS MOV,       \ contents of PFA -> TOS
            NEXT,
            ENDCODE
T' DOVAR  RESOLVES 'DOVAR     \ 'DOVAR is used by the cross-compiler


TCREATE: DOCON   ( -- n )     \ Executor that executes a "CONSTANT"
            TOS PUSH,
            *W TOS MOV,     \ Fetch contents of PFA->TOS
            NEXT,
            ENDCODE
T' DOCON RESOLVES 'DOCON     \ 'DOCON is used by the cross-compiler


\ In CAMEL99 the 9900 Workspace register (WP) doubles as USER pointer
\ USER variables are local to each task
TCREATE: DOUSER ( -- addr)
            TOS PUSH,      \ Executor that executes a "USER VARIABLE"
            TOS STWP,
            *W TOS ADD,    \ add USER VAR's offset to Workspace pointer
            NEXT,
            ENDCODE
T' DOUSER  RESOLVES 'DOUSER  \ 'DOUSER is used by the cross-compiler


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

TCREATE: DODOES  ( -- a-addr)
            TOS PUSH,       \ save TOS reg on data stack      28
            W TOS MOV,      \ put defined word's PFA in TOS   14
            IP RPUSH,       \ push old IP onto return stack   28
            R11 IP MOV,     \ R11 has the new PFA -> IP       14
            NEXT,                                       \     84
            ENDCODE
T' DODOES RESOLVES 'DODOES    \ 'DODOES is used by the cross-compiler

\ ***********************end of executors *************************

\ =============[ Forth code primitives begin ] ====================

CODE PERFORM  ( xt -- )
            *TOS W MOV,
            @@1 JMP,
+CODE EXECUTE ( 'xt -- )     \ fetch the XT from an address
            TOS W MOV,       \ move the execution token in TOS into temp
@@1:        TOS POP,         \ refill TOS
            *W+ R5 MOV,      \ move contents of the XT (the address of the doer routine) to R5
            *R5 B,           \ branch to the address of the DOER routine
             ENDCODE

\ these run from high speed RAM
CODE ?BRANCH
            _?BRANCH >HSRAM CFA!  \ put fast address into the CFA
            ENDCODE

CODE BRANCH
            _BRANCH >HSRAM CFA!   \ put fast address into the CFA
            ENDCODE

\ ==================================================================
CR .( MEMORY FETCH & STORE)

CODE !      ( n addr -- )
            *SP+ *TOS MOV,   \ 26
            TOS POP,         \ 22
             NEXT,           \ = 48
             ENDCODE

CODE @      ( a -- w )
            _fetch >HSRAM CFA!
\            *TOS TOS MOV,
\            NEXT,
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

l: _C@      *TOS TOS MOVB,        \ put C@ inline to save space
             TOS 8 SRL,          \ 58 CLOCKS TOTAL
            NEXT,
            ENDCODE

CODE C@     _C@  CFA! ENDCODE      \ give _C@ a dictionary header

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
      RP -4 ADDI,          \ 14
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

CODE DROP   ( w -- )
            _drop >HSRAM CFA!
\           TOS POP,             \ 22 *SP+ TOS MOV,
\           NEXT,
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
            _DUP >HSRAM CFA!       \ use hi-speed ram version
\           TOS PUSH,
\           NEXT,
            ENDCODE

CODE SWAP   ( w1 w2 -- w2 w1 )
            TOS   W MOV,          \ 14
            *SP TOS MOV,          \ 18
            W   *SP MOV,          \ 18
            NEXT,                 \ 50
            ENDCODE

CODE OVER   ( w1 w2 -- w1 w2 w1 )
            TOS PUSH,             \ 28
            2 (SP) TOS MOV,       \ 22
            NEXT,                 \ 50
            ENDCODE

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
             TOS      R0 MOV,    \ n4  14
            *SP       R1 MOV,    \ n3  14
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
CODE 1+     ( n -- n') TOS INC,      NEXT, ENDCODE
CODE 1-     ( n -- n') TOS DEC,      NEXT, ENDCODE
CODE 2+     ( n -- n') TOS INCT,     NEXT, ENDCODE
CODE 2-     ( n -- n') TOS DECT,     NEXT, ENDCODE

CODE 2*     ( n -- n)   TOS 1 SLA,    NEXT, ENDCODE
CODE 2/     ( n -- n)   TOS 1 SRA,    NEXT, ENDCODE
\ CODE 4*     ( n -- nx4) TOS 2 SLA,    NEXT, ENDCODE
\ CODE 8*     ( n -- nx8) TOS 3 SLA,    NEXT, ENDCODE
CODE ABS    ( n -- n )   TOS ABS,    NEXT, ENDCODE
CODE NEGATE ( n -- n )   TOS NEG,    NEXT, ENDCODE
CODE INVERT ( u -- w)    TOS INV,    NEXT, ENDCODE

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
            @@1 JMP,
+CODE =     ( n n -- ?)     \ Used 4X in Kernel
            *SP+ TOS CMP,
@@1:        TOS CLR,
            EQ IF,
               TOS SETO,
            ENDIF,
            NEXT,
ENDCODE


CODE 0<     ( n -- flag )
            TOS TOS MOV,  \ **MUST USE MOV HERE**
            TOS SETO,
            @@1 JLT,
            TOS CLR,
@@1:        NEXT,
            ENDCODE

CODE U<     ( n1 n2 -- flag)
            *SP+ TOS CMP,
            TOS SETO,
            @@1 JL,
            TOS CLR,
@@1:        NEXT,
            ENDCODE

CODE >      ( n1 n2 -- flag)
            *SP+ TOS CMP,
            TOS SETO,
            @@1 JGT,
            TOS CLR,
@@1:        NEXT,
            ENDCODE

CODE <      ( n1 n2 -- flag)
            *SP+ TOS CMP,
            TOS SETO,
            @@1 JLT,
            TOS CLR,
@@1:        NEXT,
             ENDCODE

\ ==================================================================
CR .( MIN & MAX )  \ added NIP HEADER. saves 6 bytes
CODE MIN    ( n1 n2 -- n)
            *SP TOS CMP,    \ compare n1 & n2
            @@1 JLT,       \ if n1 < n2 goto @@1:
            SP INCT,       \ otherwise NIP n1
            NEXT,
+CODE MAX   ( n1 n2 -- n)
            *SP  TOS CMP,   \ compare n1 & n2
            @@2 JGT,       \ if n1 > n2 goto @@1:
            SP INCT,       \ otherwise NIP n1
            NEXT,
@@1: @@2:   TOS POP,       \ DROP n2, refill TOS
            NEXT,
            ENDCODE

CR .( Forth 2012 MOVE CMOVE CMOVE>)
HEX
CODE MOVE  ( src dst u -- ) \ u = no. of *CELLS* to move
    R0 POP,
    R1 POP,
    BEGIN,
       TOS DEC,
    OC WHILE,
       *R1+ *R0+ MOV, \ 9900 auto increments by 2 with MOV instruction
    REPEAT,
    TOS POP,
    NEXT,
ENDCODE

\ FORTH 2012: CMOVE and CMOVE> are part of the optional string word set
CODE CMOVE  ( src dst n -- )   \ forward character move
      R0 POP,       \ pop DEST into R0
      R1 POP,       \ pop source into R1
      BEGIN,
         TOS DEC,          \ test it before the loop starts
      OC WHILE,
         *R1+ *R0+ MOVB, \ byte move, with auto increment by 1.
      REPEAT,
      TOS POP,
      NEXT,
      ENDCODE

\ OCT 5 2020 Fixed computation for end of strings and inner loop.
CODE CMOVE>  ( src dst n -- )  \ move chars from end of string and go backwards
      R2 POP,      \ DEST into R2
      R1 POP,      \ source into R1
      TOS W MOV,   \ dup n
      W DEC,       \ compute n-1
      W R1 ADD,    \ point to end of source
      W R2 ADD,    \ point to end of destination
      BEGIN,
         TOS DEC,  \ decr the counter in TOS (R5)
      OC WHILE,
         *R1 *R2 MOVB,
          R1 DEC,   \ move pointers back (NO auto decrement, damn)
          R2 DEC,
      REPEAT,
      TOS POP,
      NEXT,
      ENDCODE

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

\ used CAMEL Forth MSP430 as reference code
[TC]
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

CODE (FIND) ( Caddr NFA -- XT ? )
    TOS R3 MOV,                 \ R3 = NFA which is a counted string
    TOS CLR,                    \ TOS is the output flag, init to zero
    *SP R8 MOV,                 \ R8 = caddr which is a counted string
    *R8 R5 MOVB,                \ caddr C@ -> R5 is string length
    R5 8 SRL,                   \ get the byte on the correct side right
    BEGIN,
\ Oct 10 2025: ** this comparison improves compile times by 14%
        *R8 *R3 CMPB,          \ TEST length bytes
        EQ IF,
           \ load char compare registers
            R5 R0 MOV,           \ load R0 with length of caddr string
            R8 R1 MOV, R1 INC,   \ load R1 with caddr of 1ST char
            R3 R2 MOV, R2 INC,   \ load R2 with NFA first char
\ inner character comparator loop
            BEGIN,
              *R1+ *R2+ CMPB,    \ compare char by char
              @@1 JNE,           \ ANY mismatch found, goto @@1
              R0 DEC,            \ decr. loop counter
            EQ UNTIL,            \ loop until R0=0
\ we found the word !!!

\ convert NFA in R3 to CFA -> R2
            R3 R2 MOV,            \ if found R3 has a name field address (NFA), copy to R2
            *R3 R0 MOVB,          \ get the length of the name to R0
            R0 SWPB,              \ fix the #$%!@$ byte order again
            R0 R2 ADD,            \ add length to R2, gets past the string to the CFA
            R2 INCT,              \ inc 1 for the count byte, 1 more for even address evaluation
            R2 -2 ANDI,           \ align R2 to even address boundary
            R2 *SP MOV,           \ replace Caddr with the XT in R2

\ test for immediate or normal word -> TOS
            TOS SETO,               \ we found a word so set TOS to -1
            -1 (R3) R0 MOVB,        \ R3 has the NFA. Fetch NFA-1 (immediate field)
            \ Oct 2020 added mask to support hashed threads
            R0 0100 ANDI,           \ test the immediate bit, (mask the hash code)
            NE IF,
                TOS ABS,            \ if non zero change TOS from -1 to 1
            ENDIF,                  \ and head for home
            NEXT,                   \ Return to Forth
        ENDIF,
@@1:    -3 (R3) R3 MOV,      \ fetch LFA, R3 now has new NFA
    EQ UNTIL,
    NEXT,                    \ we got zero. End of the list! Go back to Forth
    ENDCODE
