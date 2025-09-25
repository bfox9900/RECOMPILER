COMPILER  CR .( LOGICAL OPERATIONS)

TARGET
CODE AND    ( w w -- w )
            *SP INV,                  \ thanks to Mark Wills!   10
            *SP+ TOS SZC,             \ RTFM for TMS9900 BF     22
            NEXT,                                            \ =32
            ENDCODE

CODE OR     ( w w -- w )  *SP+ TOS SOC,  NEXT, ENDCODE
CODE XOR    ( w w -- w )  *SP+ TOS XOR,  NEXT, ENDCODE

COMPILER CR .( ARITHMETIC)
\ simple math is about 40% faster with TOS in register
TARGET
CODE 1+     ( n -- n') TOS INC,       NEXT, ENDCODE
CODE 1-     ( n -- n') TOS DEC,       NEXT, ENDCODE
CODE 2+     ( n -- n') TOS INCT,      NEXT, ENDCODE
CODE 2-     ( n -- n') TOS DECT,      NEXT, ENDCODE

CODE 2*     ( n -- n)   TOS 1 SLA,    NEXT, ENDCODE
CODE 4*     ( n -- nx4) TOS 2 SLA,    NEXT, ENDCODE
CODE 8*     ( n -- nx8) TOS 3 SLA,    NEXT, ENDCODE

CODE 2/     ( n -- n)   TOS 1 SRA,    NEXT, ENDCODE

\ *G NOT standard forth. Nice native 9900 instructions
CODE 1+! ( addr -- )  *TOS INC,  TOS POP,  NEXT,   ENDCODE
CODE 1-! ( addr -- )  *TOS DEC,  TOS POP,  NEXT,   ENDCODE

COMPILER CR .( ANS Shift operations)
TARGET
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

COMPILER
CR .( math ...)
\ : +   ( u1 u2 -- u ) UM+ DROP ;  original Camel Forth code is bigger
TARGET
CODE +      ( u1 u2 -- u )
\             _PLUS >HSRAM CFA!
            *SP+ TOS ADD,       \ ADD 2nd item to TOS and incr stack pointer.
           NEXT,
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
COMPILER HEX
TARGET
CODE ALIGNED ( n -- n)
            TOS INC,
            TOS FFFE ANDI,
            NEXT,
            ENDCODE

COMPILER DECIMAL