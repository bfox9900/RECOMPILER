CR .( TI-99 Cross Assembler V 1.2 ...)
CR .( For Camel99 Machine Forth)
CR .( *NOTES*)
CR .( Compare instruction is changed to CMP,)
CR .( to remove name confusion with C,)
CR .( Changed A, and S, to ADD, SUB,)
CR .( With TI syntax for all address modes)

NEEDS FORTH  FROM DSK1.WORDLISTS

ONLY FORTH DEFINITIONS

NEEDS .S     FROM DSK1.TOOLS
NEEDS CASE   FROM DSK1.CASE

\ see: RECOMPILER.FTH 
ONLY FORTH ALSO COMPILERS DEFINITIONS 

\    ** THE CROSS COMPILER **  :-)
HEX

\ Target versions of HERE and ALLOT
: THERE  ( -- addr) H @ ;
: TALLOT ( n -- )   H +! ;

\ integer and byte "Target" compilers
: T,     ( n -- )   THERE !  2 TALLOT ;
: TC,    ( c -- )   THERE C! 1 TALLOT ;

\ ===================================================
CR .( Loading at ) DP @ HEX U.

ONLY FORTH ALSO COMPILERS ALSO ASSEMBLER DEFINITIONS
DECIMAL
 0 CONSTANT  R0    1 CONSTANT  R1
 2 CONSTANT  R2    3 CONSTANT  R3
 4 CONSTANT  R4    5 CONSTANT  R5
 6 CONSTANT  R6    7 CONSTANT  R7
 8 CONSTANT  R8    9 CONSTANT  R9
10 CONSTANT R10   11 CONSTANT R11
12 CONSTANT R12   13 CONSTANT R13
14 CONSTANT R14   15 CONSTANT R15
HEX
\ Wycove assembler register syntax:
: @@        020    ;  \ symbolic addressing
: **        010 +  ;  \ indirect addressing
: *+        030 +  ;  \ indirect addressing, auto-increment
: ()        20  +  ;  \ indexed addressing
 
\ INDIRECT addressing register names
: *R0    R0 ** ;
: *R1    R1 ** ;    : *R2    R2 ** ;   : *R3    R3 ** ;
: *R4    R4 ** ;    : *R5    R5 ** ;   : *R6    R6 ** ;
: *R7    R7 ** ;    : *R8    R8 ** ;   : *R9    R9 ** ;
: *R10   R10 ** ;   : *R11  R11 ** ;   : *R12  R12 ** ;
: *R13   R13 ** ;   : *R14  R14 ** ;   : *R15  R15 ** ;
 
\ INDIRECT addressing, auto-increment register names
: *R0+    R0 *+ ;
: *R1+    R1 *+ ;    : *R2+    R2 *+ ;   : *R3+    R3 *+ ;
: *R4+    R4 *+ ;    : *R5+    R5 *+ ;   : *R6+    R6 *+ ;
: *R7+    R7 *+ ;    : *R8+    R8 *+ ;   : *R9+    R9 *+ ;
: *R10+   R10 *+ ;   : *R11+  R11 *+ ;   : *R12+  R12 *+ ;
 
\ indexed addressing register names
: (R0)    TRUE ABORT" R0 cannot be indexed" ;
: (R1)    R1 () ;   : (R2)    R2 () ;   : (R3)    R3 () ;
: (R4)    R4 () ;   : (R5)    R5 () ;   : (R6)    R6 () ;
: (R7)    R7 () ;   : (R8)    R8 () ;   : (R9)    R9 () ;
: (R10)  R10 () ;   : (R11)  R11 () ;   : (R12)  R12 () ;
: (R13)  R13 () ;   : (R14)  R14 () ;   : (R15)  R15 () ;
 
 \ ============================================================
CR .( CAMEL99 Forth named registers)
R4 CONSTANT TOS   R6 CONSTANT SP    R8 CONSTANT W
R7 CONSTANT RP    R9 CONSTANT IP
 
: *TOS     TOS ** ;  : *TOS+    TOS *+ ;  : (TOS)    TOS () ;
: *SP       SP ** ;  : *SP+      SP *+ ;  : (SP)      SP () ;
: *RP       RP ** ;  : *RP+      RP *+ ;  : (RP)      RP () ;
: *W        W **  ;  : *W+       W *+  ;  : (W)       W ()  ;
: *IP       IP ** ;  : *IP+      IP *+ ;  : (IP)      IP () ;

HEX
: ADDRESS? ( n -- ?) DUP 1F > SWAP 30 < AND ;
: GOP'     OVER ADDRESS?   \ address or register?
           IF   + T, T,    \ compile instruction & address
           ELSE + T,       \ compile register
           THEN ;

: GOP      CREATE , DOES> @ GOP' ;

0440 GOP B,     0680 GOP BL,    0400 GOP BLWP,
04C0 GOP CLR,   0700 GOP SETO,  0540 GOP INV,
0500 GOP NEG,   0740 GOP ABS,   06C0 GOP SWPB,
0580 GOP INC,   05C0 GOP INCT,  0600 GOP DEC,
0640 GOP DECT,  0480 GOP X,

: GROP     CREATE , DOES> @ SWAP 40 * + GOP' ;

2000 GROP COC,  2400 GROP CZC,  2800 GROP XOR,
3800 GROP MPY,  3C00 GROP DIV,  2C00 GROP XOP,

: GGOP     CREATE ,
           DOES> @ SWAP DUP ADDRESS?
                 IF   40 * + SWAP >R GOP' R> T,
                 ELSE 40 * + GOP'  THEN ;
HEX
A000 GGOP ADD,  B000 GGOP ADDB, 8000 GGOP CMP, 9000 GGOP CMPB,
6000 GGOP SUB,  7000 GGOP SUBB, E000 GGOP SOC, F000 GGOP SOCB,
4000 GGOP SZC,  5000 GGOP SZCB, C000 GGOP MOV, D000 GGOP MOVB,

: 0OP     CREATE ,  DOES> @ T, ;

0340 0OP IDLE,   
0360 0OP RSET,  
03C0 0OP CKOF,
03A0 0OP CKON,   
03E0 0OP LREX,  
0380 0OP RTWP,

: ROP     CREATE , DOES> @ + T, ;
02C0 ROP STST,
02A0 ROP STWP,

: IOP     CREATE , DOES> @ T, T, ;
02E0 IOP LWPI,
0300 IOP LIMI,

: RIOP    CREATE , DOES> @ ROT + T, T, ;
0220 RIOP AI,
0240 RIOP ANDI,
0280 RIOP CI,
0200 RIOP LI,
0260 RIOP ORI,

: RCOP    CREATE , DOES> @ SWAP 10 * + + T, ;
0A00 RCOP SLA,
0800 RCOP SRA,
0B00 RCOP SRC,
0900 RCOP SRL,

CR .( jump instructions...)
: DOP     CREATE ,   DOES> @ SWAP 00FF AND OR T, ;
1300 DOP JEQ,  1500 DOP JGT,  1B00 DOP JH,   1400 DOP JHE,
1A00 DOP JL,   1200 DOP JLE,  1100 DOP JLT,  1000 DOP JMP,
1700 DOP JNC,  1600 DOP JNE,  1900 DOP JNO,  1800 DOP JOC,
1C00 DOP JOP,  1D00 DOP SBO,  1E00 DOP SBZ,  1F00 DOP TB,
CR .( Jump tokens...)
HEX                  \ Action if TRUE
 01 CONSTANT GTE     \ JLT to ENDIF,
 02 CONSTANT HI      \ JLE to ENDIF,
 03 CONSTANT NE      \ JEQ to ENDIF,
 04 CONSTANT LO      \ JHE to ENDIF,
 05 CONSTANT LTE     \ JGT to ENDIF,
 06 CONSTANT EQ      \ JNE to ENDIF,
 07 CONSTANT OC      \ JNC to ENDIF,
 08 CONSTANT NC      \ JOC to ENDIF,
 09 CONSTANT OO      \ JNO to ENDIF,
 0A CONSTANT HE      \ JLO to ENDIF,
 0B CONSTANT LE      \ JH  to ENDIF,
 0C CONSTANT NP      \ JOP to ENDIF,

HEX
CR .( Structured branches and loops...)
: AJUMP,  ( token --) >< 1000 +  T, ;
: RESOLVE ( byte --)  2- 2/ SWAP 1+ C! ;
: <BACK   ( addr addr' -- ) TUCK -  RESOLVE ;

: IF,     ( token -- addr) THERE SWAP AJUMP, ;
: ENDIF,  ( addr --)  THERE OVER -  RESOLVE ;
: ELSE,   ( -- addr ) THERE 0 JMP, SWAP ENDIF, ;

: BEGIN,  ( -- addr)  THERE ;
: WHILE,  ( token -- *while *begin) IF, SWAP ;
: AGAIN,  ( *begin --)  THERE  0 JMP, <BACK ;
: UNTIL,  ( *begin token --) THERE SWAP AJUMP, <BACK ;
: REPEAT, ( *while *begin -- ) AGAIN, ENDIF, ;


: GCOP    CREATE , DOES> @ SWAP 000F AND 40 * + GOP' ;

3000 GCOP LDCR,
3400 GCOP STCR,

CR .( Pseudo instructions...)
: RT,     ( -- )  R11 ** B, ;
: NOP,    ( -- )  0 JMP, ;
: NEXT,   ( -- )  R10 **  B, ;
: L:      ( <text> ) CREATE ;

\ PUSH & POP macros for DATA stack
: PUSH,   ( src -- )  SP DECT,  *SP  MOV, ;
: POP,    ( dst -- )  *SP+      SWAP MOV, ;

\ PUSH & POP macros for RETURN stack
: RPUSH,  ( src -- ) RP DECT,  *RP   MOV, ;
: RPOP,   ( dst -- ) *RP+      SWAP  MOV, ;

: CALL  ( addr --)  *TOS BL, TOS POP, ;


CR CR .( Cross Assembler loaded)
