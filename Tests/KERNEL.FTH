\ MAKE CAMEL99 ITC Forth with FXCC cross-compiler  Nov 2024  Brian Fox

\    *** TIB AND STACK LOCATIONS ***
COMPILER
HEX
        FFFC    EQU EMEM     \ EMEM = "end of memory"
        0050    EQU TIBSIZE  \ 80 bytes is the longest file string
EMEM TIBSIZE -  EQU 'TIB     \ Terminal input buffer just below
     'TIB  2 -  EQU 'SP0     \ FORTH DATA stack base address
     'SP0 6E -  EQU 'RP0     \ FORTH RETURN stack base address

      8300      EQU WRKSP0   \ primary workspace in 16 bit RAM

\   *** TARGET MEMORY LOCATION ***

    A000 VALUE CODESEG
    2000 VALUE HEAPSEG

\ setup the build
    NEW
    CODESEG ORG

HEADERS  ON \ include dictionary
WARNINGS ON  \ prevents endless "<xxxx> is redefined"

\ Add the fundamentals to the compiler and kernel image
CHDIR DSK9.   \ camel99 source code disk
S" ITC-CODE"  INCLUDED   \ code word definer
S" EXECUTORS" INCLUDED   \ fundamental runtime code for Forth
S" ITC-TYPES" INCLUDED   \ CONSTANT VARIABLE : ; etc.
S" BRANCHING" INCLUDED   \ cross-compiler IF ELSE THEN BEGIN etc.
\ compiler extensions and forward references for (S") and TYPE
S" TSQUOTE"   INCLUDED

\ build the target image ...
COMPILER HEX
\ ====================================
CHDIR DSK9.
CR .(  "CODE" words )
S" FETCHSTORE"  INCLUDED
S" RSTACK"      INCLUDED
S" DODOES"      INCLUDED
S" DATASTACK"   INCLUDED
S" LOGICMATH"   INCLUDED
S" MULTDIV"     INCLUDED
S" COMPARES"    INCLUDED
S" MOVEFILL"    INCLUDED
S" SKIPSCAN"    INCLUDED
S" FIND"        INCLUDED
S" MISC"        INCLUDED  \ contains PAUSE EXECUTE
S" DOLOOP"      INCLUDED  \ cross-compiler DO/LOOP
S" VDP-API"     INCLUDED
S" ITC-COLON"   INCLUDED
S" TICKTOCK"    INCLUDED   \ 9901 hardware timer

\ Meta words look like Forth but compile to TARGET
 S" METADEFS"    INCLUDED

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
'RP0 CONSTANT RP0
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
    VARIABLE CURS     COMPILER  205F CURS T!  TARGET
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
\ ======================================================================
\ S I M P L E   S O U N D  I N T E R F A C E
COMPILER HEX
TARGET
\ write a byte to address of TMS9919 chip
\ added PAUSE for multi-tasking improvement
: SND!   ( c -- ) PAUSE  8400 C!  ;  \ 4 bytes, 277 uS

TARGET
: BEEP     ( -- )
      80 SND! 5 SND!    \ pre-calculated values for OSC1 1390Hz
      91 SND!           \ turn on OSC1 at -2 dB level
      AA MS             \ Delay ~ 170 mS
      9F SND! ;         \ turn off OSC1

\ We use the HONK sound for ABORT like TI-BASIC does on errors
: HONK     ( -- )
      81 SND! 20 SND!   \ pre-calculated values for OSC1 218Hz
      90 SND!           \ turn on OSC1 at 0 dB level
      AA MS             \ Delay ~ 170 mS
      9F SND! ;         \ turn off OSC1

\ ======================================================================
\ I N T E R P R E T E R
\ forward reference done via a variable and PERFORM

: INTERPRET  ( addr len -- ) 'IV PERFORM ;

\ ======================================================================
\ Q U I T :  The  O U T E R   I N T E R P R E T E R

: QUIT     ( -- )
      RP0 RP! L0 LP !
      SOURCE-ID OFF
      T[COMPILE] [
      BEGIN
        TIB DUP 52 ACCEPT SPACE
        ( addr len) INTERPRET
        STATE @ 0= IF  T."  ok"  THEN CR
      AGAIN ;

: EVALUATE ( c-addr u -- j*x)
      SOURCE-ID DUP @ >IN @ 2>R  ON
      SOURCE 2>R
      INTERPRET
      2R> 'SOURCE 2!
      2R> >IN ! SOURCE-ID !
;

\ ======================================================================
\ E R R O R   H A N D L I N G
\
COMPILER HEX
TARGET
: DECIMAL     ( -- ) 0A BASE ! ;
: HEX         ( -- ) 10 BASE ! ;

\ E R R O R   H A N D L I N G
: ABORT    ( -- )
      SP0 SP!
      VDPTOP ^PAB !   \ set base pab pointer
      CR QUIT ;

: ?ABORT   ( f caddr u --)
      ROT
      IF
        CR
        CR T." ? " TYPE  HONK
        SOURCE-ID @ 0>  ( source is NOT console)
        IF  T."  Line " LINES @ DECIMAL U.
            CR CR SOURCE TYPE
        THEN ABORT
      THEN 2DROP ;

                  \ flag        addr length
: ?ERR     ( ? -- )             HERE COUNT ?ABORT ;
: ?EXEC    ( -- ) STATE @       TS" Interpret only" ?ABORT ;
: ?COMP    ( -- ) STATE @ 0=    TS" Compile only"   ?ABORT ;
: ?CSP     ( -- ) SP@ CSP @ -   TS" Unfinished"     ?ABORT ;
: !CSP     ( -- ) SP@ CSP ! ;

\ ======================================================================
\ S T R I N G   L I T E R A L

: S"  ( cccc" -- )          \ compiling action
\ *G Non-standard: when interpreting S" puts the string at PAD + >IN
\ ** and returns the address.
      ( cccc" --- addr len) \ interpreting action *NON-STANDARD*
      '"' PARSE
      STATE @
      IF   COMPILE (S")  S,  EXIT
      THEN PAD >IN @ + DUP>R PLACE R> COUNT
; XIMMEDIATE

: ABORT"   ( i*x 0  -- i*x)    \ R: j*x -- j*x  x1=0
      T[COMPILE] S"
      COMPILE ?ABORT ; XIMMEDIATE

\ ======================================================================
\ D I C T I O N A R Y   S E A R C H
TARGET

VARIABLE CONTEXT  COMPILER  0 T, 0 T, 0 T, 0 T, 0 T, 0 T, 0 T, 0 T, TARGET
\ *G Array of Root + 8 wordlists to control search order

VARIABLE CURRENT \ ** wordlist where definitions will be added
: <FIND>  ( caddr --  caddr  0  if not found)
\                      xt    1  if XIMMEDIATE,
\                      xt   -1  if "normal"
      CONTEXT @ ( HASH) @ (FIND) ;

VARIABLE 'FIND   \ *G vector for the action of find

: FIND     ( caddr -- ) 'FIND PERFORM ;
: '        ( -- xt) BL WORD FIND 0= ?ERR ;
: [']      ( -- <name> ) ?COMP  '  T[COMPILE] LITERAL ; XIMMEDIATE

 : POSTPONE ( <name> -- ) \ *G replaces COMPILE and [COMPILE]
      ?COMP
      BL WORD FIND DUP 0= ?ERR
      0< IF COMPILE COMPILE
      THEN  COMPILE, ; XIMMEDIATE

\ ======================================================================
\ T E X T   O U T P U T
: ."  (  ccc" -- )
      T[COMPILE] S"                ( -- str len)
      STATE @
      IF   COMPILE TYPE
      ELSE TYPE
      THEN ; XIMMEDIATE

: .(   ')' PARSE TYPE ; XIMMEDIATE


\ ======================================================================
\ I N T E R P R E T E R  /  C O M P I L E R

COMPILER CR .( Interpreter/compiler loop)
TARGET
: <INTERP>  ( i*x c-addr u -- j*x )
      'SOURCE 2!  >IN OFF
      BEGIN
        BL WORD  DUP C@   ( -- caddr len)
      WHILE
        FIND ?DUP
        IF ( it's a word)
            1+ STATE @ 0= OR
            IF   EXECUTE
            ELSE COMPILE,
            THEN
        ELSE ( it's a number)
            COUNT NUMBER? ?ERR
            T[COMPILE] LITERAL
        THEN
        DEPTH 0< TS" Short stack" ?ABORT
      REPEAT
      DROP ;


\ ================================================================
COMPILER HEX
TARGET
: TEXT    ( -- )
      F0 DUP 83D4 C!
  ( F0) 01 VWTR
       0 2 VWTR   \ set VDP screen page
      17 7 VWTR   \ sets FG & BG color

      VTOP OFF    \ topline VDP offset
      VPG  OFF    \ VDP screen page offset
      28 C/L!     \ set chars per line & C/SCR
      0 0 AT-XY
      3C0 C/SCR !
      2 VMODE !   \ 2=ID for 40 column "TEXT" mode
      PAGE
;

TARGET
CODE COLD
      WRKSP0 LWPI,
\ copy primitives into 16 bit ram
\      R0 HSprims LI,   \ source
\      R1 HSstart LI,   \ destination
\      BEGIN,           \ Copy hi-speed routines to fast RAM
\        *R0+ *R1+ MOV,
\         R1 HSend CI,
\      EQ UNTIL,
      SP  'SP0  LI,    \ data stack
      RP  'RP0  LI,    \ return stack
      R10 _NEXT LI,    \ inner interpreter
      IP  BOOT  LI,    \ load interpreter pointer with boot word
      *R10 B,          \ run Forth NEXT (inner interpreter)
ENDCODE

\ compile BYE last, means HOST BYE is available
\ if compile crashes before new
TARGET ( this is required)
CODE BYE   ( -- )
    83C4 @@ CLR,
    0000 @@ BLWP,
ENDCODE

CODE HALT  BEGIN,  AGAIN,  NEXT, ENDCODE

: TESTER
      HEX
      HEX
      PAGE ." Camel99 Built Itself"
      CR
      CR ." Press key to put EXIT in PAD"
      KEY DROP
      TS" EXIT" PAD PLACE
      CR PAD COUNT TYPE
      CR ." DP    = " DP @ U.
      CR ." LATEST= " LATEST @ U.
      CR ." PAD   = " PAD U.
      CR
      CR ." Press key to call Find"
      KEY DROP
      HEX
      CR ." FIND RESULT= " PAD LATEST @ (FIND)  SWAP U. U.
      KEY DROP
;

: WARM  ( -- )
      80 83C2 C!
      26 TPAD !
      1000 VP !  ( Free VDP memory start )
      2000 H !   ( heap starts in low RAM)
      FFAC TIB !
      3FFF TMR!
      VDPTOP ^PAB !
      L0 LP !
      FLOOR ON
      SOURCE-ID OFF
      205F CURS !
      TEXT
      ['] <INTERP> 'IV ! ( ) \ >B762 -> >ABDA
      ['] <FIND>   'FIND !   \ B6AE  -> B6C6
      ORGDP @ DP !
      ORGLAST @ LATEST !
      LATEST DUP CONTEXT ! CURRENT !
      TESTER
      BYE
;

COMPILER
\ LATEST = B857
\ ORGLAST = B857
TLAST  HOST @  DUP  COMPILER LATEST T! ORGLAST T!

COMPILER
THERE ORGDP T!

PREAMBLE COLD
BOOT AUTOSTART WARM

END.   ( report compile time and stats)

SAVE DSK8.KERNEL

ONLY FORTH DEFINITIONS
 CR ." === COMPILE ENDED PROPERLY ==="
