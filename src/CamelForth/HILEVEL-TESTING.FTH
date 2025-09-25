COMPILER CR .( Constants and VARIABLEs...)
\ CAMEL99 Forth HI-LEVEL WORDs RE-COMPILED
\ must be compile via  ITC-MAKE.FTH

\ BANDAIDS because I can't get [CHAR]  to work @#$!$%
COMPILER HEX
22 EQU '"'
29 EQU ')'
2D EQU '-'

\ these 3 address equates are set in ITC-MAKE
\ Make them constants in the target system
TARGET
'SP0 CONSTANT SP0
'SP0 CONSTANT RP0
'TIB CONSTANT TIB

\ Utility constants
    0 CONSTANT FALSE
   -1 CONSTANT TRUE
    0 CONSTANT 0
    1 CONSTANT 1
   20 CONSTANT BL

COMPILER DECIMAL

TARGET
   0024 CONSTANT L/SCR


\ ======================================================================
\ U S E R   V A R I A B L E S
\ CAMEL99 uses space after workspace for user vars.
COMPILER HEX TARGET
\ *G User VARIABLEs begin at >8320 for the primary Forth task
\ ** User VARIABLE 0 .. 1F are workspace registers.

      20 USER TFLAG \ used for multi-tasker
      22 USER JOB   \ used for multi-tasker
      24 USER DP
      26 USER HP
      28 USER CSP
      2A USER BASE
      2C USER >IN
      2E USER C/L
      30 USER OUT
      32 USER VROW
      34 USER VCOL
\      36 USER 'KEY     \ for vectored char input
\      38 USER 'EMIT    \ for vectored char output
      3A USER LP        \ LEAVE stack pointer
      3C USER SOURCE-ID
      3E USER 'SOURCE
\     40 USER 'SOURCE   \ uses 2 locations

      46 USER TPAD      \ holds offset from HERE for TASK PADs
      7E USER VPG       \ Each task can have it's own video page

COMPILER
CR .( system variables)
HEX
TARGET
    VARIABLE STATE
    VARIABLE LATEST

    VARIABLE ORGDP
    VARIABLE ORGLAST
    VARIABLE BOOT

    VARIABLE VPG      \ declared in TI99 VDP driver code TI99IO.HSF
    VARIABLE VMODE
    VARIABLE L0       COMPILER  4 CELLS TALLOT TARGET
    VARIABLE ^PAB
    VARIABLE LINES
    VARIABLE C/SCR
    VARIABLE 'IV      \ *G interpretor vector. Holds address of <INTERP>
    VARIABLE H
    VARIABLE VP
    VARIABLE CURS
    VARIABLE VTOP
    VARIABLE WARNINGS

COMPILER CR .( Hi-level FORTH Primitives)

TARGET

: HERE      ( -- addr) DP @  ;
: ALLOT     ( n --)   DP +! ;
: ,         ( n -- ) HERE  ! 2 ALLOT ;
: C,        ( n -- ) HERE C! 1 ALLOT  ;
: COMPILE,  ( n -- )  , ;
: ALIGN     ( -- )   HERE ALIGNED DP ! ;
: PAD       ( -- addr) HERE TPAD @ + ;
: COMPILE   ( -- )  R> DUP 2+ >R @  , ;
: IMMEDIATE ( --)   01 LATEST @ 1-  C! ;


: LITERAL   ( n -- n|~) STATE @ IF  COMPILE LIT ,  THEN ;  XIMMEDIATE
: ]         ( -- ) STATE ON  ;
: [         ( -- ) STATE OFF ;  XIMMEDIATE
: DEPTH     ( -- n ) SP0 SP@ 2+ - 2/ ; \ ** needs signed shift

\ PAB Base Address
: VDPTOP  ( -- n) 8370 @ 1- ;

COMPILER CR .( Stack primitives ...)

TARGET
: TUCK  ( w1 w2 --  w2 w1 w2 ) SWAP OVER ;

: U>  ( n n -- ?)  SWAP U< ;
: 0>  ( n -- ?)    1- 0< INVERT ;
: <>  ( n n -- ?)  =  INVERT ;

\ : UMIN ( u1 u2 -- u )  2DUP U> IF SWAP THEN DROP ;
\ : UMAX ( u1 u2 -- u )  2DUP U< IF SWAP THEN DROP ;
: WITHIN ( u lo hi -- t ) OVER - -ROT - U> ;

\ M I X E D  (32BIT/16BIT)   M A T H   O P E R A T I O N S
: */MOD  ( n1 n2 n3 -- n4 n5) >R UM* R> M/MOD ;
: S>D    ( n -- d)  DUP 0< ;
: M+     ( d n -- d) S>D  D+ ;    \ * change from V2.67
: /MOD   ( n1 n2 -- n3 n4) >R S>D R> M/MOD ;
: /      ( n n -- n)   /MOD NIP  ;
: MOD    ( n n -- n)   /MOD DROP ;
: */     ( n n n -- n) */MOD NIP ;

\ =====================================================================
\ H E A D E R   N A V I G A T I O N

\ : NFA>LFA   ( nfa -- lfa)  3 - ;
TARGET
CODE NFA>LFA    TOS -3 AI,  NEXT, ENDCODE  ( faster, same size)

\ Changed 7F to 1F . 31 character max name length.
\ Other bits for future use
: NFA>CFA     ( nfa -- cfa ) COUNT  1F AND + ALIGNED ;
: RECURSE     ( -- ) LATEST @ NFA>CFA , ; XIMMEDIATE

\ smudge bit control in the Camel Forth
: HIDE        ( -- )  LATEST @ ( nfa) DUP C@ 80 OR  SWAP C! ;
: REVEAL      ( -- )  LATEST @ ( nfa) DUP C@ 7F AND SWAP C! ;

\ S T R I N G   T H I N G S
TARGET
: PLACE   ( src n dst -- ) 2DUP C! 1+ SWAP MOVE ;
: /STRING ( caddr1 u1 n - caddr2 u2 ) TUCK - >R + R> ;
: S,       ( c-addr u -- ) HERE OVER 1+ ALLOT PLACE  ALIGN ;

\ =====================================================================
\ P A R S E   W O R D
COMPILER CR .( Parsing...)

TARGET

: SOURCE   ( -- addr len) 'SOURCE 2@ ;

: PARSE    ( char -- c-addr u )  \ gForth
      >R
      SOURCE  >IN @ OVER MIN /STRING
      OVER SWAP R>  SCAN >R
      OVER - DUP
      R> IF 1+ THEN  >IN +! ;

: PARSE-WORD  ( char -- c-addr n)  \ Camel/BFox common factor for WORD
      DUP SOURCE >IN @ /STRING
      ROT SKIP
      DROP SOURCE  -ROT -  MIN  0 MAX >IN !
      PARSE ;

: WORD     ( char -- c-addr)
      PARSE-WORD HERE PLACE
      HERE BL OVER COUNT + C!  \ append blank character
;

\ =====================================================================
COMPILER CR .( CAMEL FORTH Number conversion)
 HEX

TARGET
: BASE@   BASE @ ;

: UD*      ( ud1 u2 -- ud3) DUP>R * SWAP R> UM* ROT + ;

: >NUMBER  ( ud adr u -- ud' adr' u' )
      BEGIN
        DUP WHILE
      OVER C@ DIGIT?
      0= IF DROP EXIT  THEN
            >R 2SWAP BASE@ UD*
            R> M+ 2SWAP
            1 /STRING
      REPEAT ;

: NUMBER?  ( addr len -- n ?)      \ ?=0 is good conversion
           (          -- addr len) \ bad conversion
      OVER C@   '-' = DUP>R    \ save flag for later
      IF 1 /STRING THEN             \ remove minus sign
      0 0  2SWAP >NUMBER NIP NIP    \ convert the number
      R> IF SWAP NEGATE SWAP THEN   \ negate if needed
;

\ S T R I N G   L I T E R A L S
COMPILER HEX

TARGET
: (S")    ( -- c-addr u) R>  COUNT  2DUP + ALIGNED >R ;
 T' (S") RESOLVES '(S")

\ ======================================================================
\  S C R E E N   D R I V E R
COMPILER
CR .( Console output)
S" CONSOLE" INCLUDED

\ ======================================================================
\ N U M B E R   T O   S T R I N G   C O N V E R S I O N
COMPILER
HEX

TARGET
\ : >DIGIT   DUP 9 > IF 7 + THEN T[CHAR] 0 + ;  \ 20 BYTES :)
CODE >DIGIT  ( n -- c)   \ ASM is 9 bytes, 4X faster
      TOS 9 CI,
      HI IF,             \ if n>9
            TOS 7 AI,  \ number is not base 10, add 7
      ENDIF,
      TOS CHAR 0 AI,  \ add ASCII 0 to TOS, create char value
      NEXT,
      ENDCODE

: <#    ( --) PAD HP ! ;
: HOLD  ( char -- )  HP DUP 1-! @ C! ;
: #     ( u -- ud2 ) 0 BASE@ UM/MOD >R  BASE@ UM/MOD SWAP >DIGIT HOLD R> ;
: #S    ( ud1 -- ud2)  BEGIN  # 2DUP OR  WHILE REPEAT ;
: #>    ( ud1 -- c-addr u) 2DROP HP @ PAD OVER - ;
: SIGN  ( n -- ) 0< IF  '-'  HOLD  THEN ;

\ ======================================================================
\ N U M B E R  O U T P U T
TARGET
: UD.    ( d -- ) <#  #S  #> TYPE SPACE ;
: U.     ( u -- ) 0 UD. ;
: (.)    ( n -- caddr len)  DUP ABS 0 <#  #S ROT SIGN  #> ;
: .      ( n -- ) (.)  TYPE SPACE ;
