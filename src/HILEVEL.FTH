\ CAMEL99 Forth HI-LEVEL WORDs 

COMPILER 
HEX
CR .( Constants and VARIABLEs...)

TARGET
  4000 CONSTANT SP0   
  3FE0 CONSTANT RP0  
  FF00 CONSTANT TIB  

\ Utility constants
      0 CONSTANT FALSE
     -1 CONSTANT TRUE
      0 CONSTANT 0
      1 CONSTANT 1
     20 CONSTANT BL

\ S Y S T E M   V A R I A B L E S
COMPILER HEX 

TARGET
    VARIABLE STATE
    VARIABLE LATEST
    VARIABLE BOOT

    VARIABLE TFLAG
    VARIABLE JOB
    VARIABLE DP
    VARIABLE HP
    VARIABLE CSP
    VARIABLE BASE
    VARIABLE >IN
    VARIABLE C/L
    VARIABLE OUT
    VARIABLE VROW
    VARIABLE VCOL
    VARIABLE 'KEY     \ for vectored char input
    VARIABLE 'EMIT    \ for vectored char output
    VARIABLE LP
    VARIABLE VPG       \ declared in TI99 VDP driver code TI99IO.HSF

COMPILER DECIMAL

TARGET
   0024 CONSTANT L/SCR

COMPILER HEX 

TARGET
  VARIABLE VMODE
  VARIABLE L0       COMPILER  4 CELLS TALLOT

 TARGET 
  VARIABLE ^PAB
  VARIABLE LINES
  VARIABLE C/SCR
  VARIABLE 'IV    \ *G interpretor vector. Holds address of <INTERP>
  VARIABLE H
  VARIABLE VP
  VARIABLE CURS     COMPILER 205F CURS T! 

  TARGET
  VARIABLE VTOP
 \  VARIABLE WARNINGS COMPILER -1 WARNINGS T! 

\ Hi-level FORTH Primitives

TARGET
: HERE      ( -- addr) DP @  ;
: ALLOT     ( n --)   DP +! ;
: ,         ( n -- )  HERE ! 2 ALLOT ;
: C,        ( n -- ) HERE C! 1 ALLOT  ;
: COMPILE,  ( n -- )  , ;
: ALIGN     ( -- )   HERE ALIGNED DP ! ;
: PAD       ( -- addr) HERE TPAD @ + ;
: COMPILE   ( -- )  R> DUP 2+ >R @  , ;
: IMMEDIATE ( --)   01 LATEST @ 1-  C! ;
: LITERAL   ( n -- n|~) STATE @ IF  COMPILE LIT ,  THEN ;  IMMEDIATE
: ]         ( -- ) STATE ON  ;
: [         ( -- ) STATE OFF ;  IMMEDIATE
: DEPTH     ( -- n ) SP0 SP@ 2+ - 2/ ; \ ** needs signed shift

\ PAB Base Address
: VDPTOP  ( -- n) 8370 @ 1- ;

COMPILER CR .( Stack primitives ...)  

TARGET
: TUCK  ( w1 w2 --  w2 w1 w2 ) SWAP OVER ;

: U>  ( n n -- ?)  SWAP U< ;
: 0>  ( n -- ?)    1- 0< INVERT ;
: <>  ( n n -- ?)  =  INVERT ;

: UMIN ( u1 u2 -- u )  2DUP U> IF SWAP THEN DROP ;
: UMAX ( u1 u2 -- u )  2DUP U< IF SWAP THEN DROP ;
: WITHIN ( u lo hi -- t ) OVER - -ROT - U> ;

\ M I X E D  (32BIT/16BIT)   M A T H   O P E R A T I O N S
: */MOD  ( n1 n2 n3 -- n4 n5) >R UM* R> M/MOD ;
: S>D    ( n -- d)  DUP 0< ;
: M+     ( d n -- d) S>D  D+ ;    \ * change from V2.67
: /MOD   ( n1 n2 -- n3 n4) >R S>D R> M/MOD ;
: /      ( n n -- n)   /MOD NIP  ;
: MOD    ( n n -- n)   /MOD DROP ;
: */     ( n n n -- n) */MOD NIP ;

\ S T R I N G   T H I N G S

TARGET

: PLACE   ( src n dst -- ) 2DUP C! 1+ SWAP MOVE ;
: /STRING ( caddr1 u1 n - caddr2 u2 ) TUCK - >R + R> ;
: S,       ( c-addr u -- ) HERE OVER 1+ ALLOT PLACE  ALIGN ;

COMPILER CR .( CAMEL FORTH Number conversion)
 HEX

TARGET
: BASE@   BASE @ ;

 : DIGIT?  ( c -- n -1)   \ if c is a valid digit
 \             -- x  0   \ otherwise
  DUP  39 > 100 AND +    \ silly looking
  DUP 140 > 107 AND -  [CHAR] 0 -   \ but it works!
  DUP BASE@ U< ;         \ 48 Bytes

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
      OVER C@   [CHAR] - = DUP>R    \ save flag for later
      IF 1 /STRING THEN             \ remove minus sign
      0 0  2SWAP >NUMBER NIP NIP    \ convert the number
      R> IF SWAP NEGATE SWAP THEN   \ negate if needed
;

\ S I M P L E   S O U N D  I N T E R F A C E
TARGET
\ write a byte to address of TMS9919 chip
\ added PAUSE for multi-tasking improvement 
: SND!   ( c -- ) PAUSE 8400 C!  ;  \ 4 bytes, 277 uS

COMPILER HEX

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

COMPILER CR .( Console output)
HEX 

TARGET 
: C/L!     ( c/l -- ) 
     DUP C/L !           \ set chars per line
     L/SCR *  C/SCR ! ;  \ calc. chars per screen

: VPOS   ( -- vaddr) VROW 2@ >VPOS ;
: VPUT   ( char -- ) VPOS VC! ;
: AT-XY  ( col row -- ) VROW 2! ;  \ set VDP address for Fast type

: TOPLN   ( -- vaddr)  VPG @ VTOP @ + ; 
: LASTLN  ( -- vaddr)  VPG @ C/SCR @ + ;

: SCROLL ( -- )
      PAUSE
      HERE 100 +     \ grab some space above Forth PAD
      TOPLN C/SCR @  ( -- buffer Vstart len)
      BOUNDS  ( -- buffer SCRend SCRstart)
      DO
        I  C/L@ +  OVER  C/L@ 8* VREAD
        DUP  I           C/L@ 8* VWRITE
      C/L@ 8* +LOOP
      DROP
      0 17 AT-XY VPOS C/L@ BL VFILL
;

\ ======================================================================
\ V D P   T E X T   O U T P U T

COMPILER HEX 

TARGET
: CR    ( -- )  (CR) L/SCR = IF  SCROLL  THEN ;
: (EMIT) ( char -- ) CPUT IF  CR  THEN ;
: PAGE   ( -- ) TOPLN LASTLN OVER -  BL VFILL   0 0 AT-XY ;
: BS     ( --)  VCOL DUP @ 1- 0 MAX  SWAP ! ;

: EMIT   ( char -- )  \ handles some control characters
      PAUSE
      0D OVER= IF  DROP CR     EXIT THEN
      08 OVER= IF  DROP BS     EXIT THEN
      (EMIT) ;

: TYPE   ( addr cnt --)  PAUSE BOUNDS ?DO  I C@ (EMIT) LOOP ;

: SPACE  ( -- )   BL (EMIT) ;
: SPACES ( n -- ) 0 MAX  0 ?DO  SPACE  LOOP ;

COMPILER CR .( Console input)

TARGET
: KEY      ( -- char)
      BEGIN                 \ start the loop
        PAUSE               \ Essential for Multi-tasking with Console
        CURS @              \ fetch 2 char cursor (space & _ )
        TMR@ 1FFF <         \ compare hardware timer to 1FFF
        IF >< THEN VPUT     \ swap cursor bytes & write
            KEY?            \ check the keyboard
            ?DUP            \ DUP IF <> 0
      UNTIL                 \ loop until a key pressed
      BL VPUT ;             \ put the space char on screen

: ACCEPT     ( caddr +n -- +n')
      OVER + OVER          \ removed 1-  to accept all chars
      BEGIN
        KEY DUP 0D <>
      WHILE
        DUP EMIT
        DUP 8 =
        IF   DROP 1-  3 PICK  UMAX  \ changed to use: 3 PICK   B.F.
        ELSE OVER C!  1+ OVER UMIN
        THEN
      REPEAT
      DROP NIP SWAP - ;

\ S T R I N G   L I T E R A L S
COMPILER HEX 

TARGET
: (S")    ( -- c-addr u) R>  COUNT  2DUP + ALIGNED >R ;


\ N U M B E R   T O   S T R I N G   C O N V E R S I O N
TARGET
: >DIGIT   DUP 9 > IF 7 + THEN [CHAR] 0 + ;  \ 20 BYTES :)
: <#     ( --) PAD HP ! ;
: HOLD   ( char -- )  HP DUP 1-! @ C! ;
: #      ( u -- ud2 ) 0 BASE@ UM/MOD >R  BASE@ UM/MOD SWAP >DIGIT HOLD R> ;
: #S     ( ud1 -- ud2)  BEGIN  # 2DUP OR  WHILE REPEAT ;
: #>     ( ud1 -- c-addr u) 2DROP HP @ PAD OVER - ;
: SIGN   ( n -- ) 0< IF  [CHAR] -  HOLD  THEN ;
: UD.    ( d -- ) <#  #S  #> TYPE SPACE ;
: U.     ( u -- ) 0 UD. ;
: (.)    ( n -- caddr len)  DUP ABS 0 <#  #S ROT SIGN  #> ;
: .      ( n -- ) (.)  TYPE SPACE ;

\ M I S C E L L A N E O U S
COMPILER HEX 

TARGET
: RECURSE     ( -- ) LATEST @ NFA>CFA , ; IMMEDIATE
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
        CR ." ? " TYPE  HONK
        SOURCE-ID @ 0>  ( source is NOT console)
        IF  ."  Line " LINES @ DECIMAL U.
            CR CR SOURCE TYPE
        THEN ABORT
      THEN 2DROP ;

                  \ flag        addr length
: ?ERR     ( ? -- )             HERE COUNT ?ABORT ;

\ S T R I N G   L I T E R A L
COMPILER 
: S" ( cccc" -- )  \ OLD VERSION
      [CHAR] " PARSE
      STATE @
      IF  COMPILE (S")  S,
      ELSE PAD DUP>R PLACE R> COUNT
      THEN ; IMMEDIATE

: ABORT"   ( i*x 0  -- i*x)    \ R: j*x -- j*x  x1=0
      ?COMP
      [COMPILE] S"
      COMPILE ?ABORT ; IMMEDIATE


\ ======================================================================
\ T E X T   O U T P U T

: ."  (  ccc" -- )
      POSTPONE S"                ( -- str len)
      STATE @
      IF   COMPILE TYPE
      ELSE TYPE
      THEN ; IMMEDIATE

\ T I - 9 9   T E X T   M O D E   C O N T R O L
TARGET-COMPILING

: TEXT    ( -- )
      F0 DUP 83D4 C!
      ( -- F0) 01 VWTR
      0  2 VWTR  \ set VDP screen page
      VTOP OFF  \ topline VDP offset
      VPG  OFF  \ VDP screen page offset
      17  7 VWTR  \ sets FG & BG color
      28 C/L!
      0 0 AT-XY
      2 VMODE !   \ 2=ID for 40 column "TEXT" mode
      PAGE
;


\ (:noname) from studying gforth. It's a nice factor.
 : (:NONAME) ( -- )  ['] DOCOL @ COMPILE,  HIDE  ]  ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\  =====[ CANNOT DEFINE ANY CONSTANT, VARIABLE OR VARIABLE AFTER THIS ]=====
\ //////////////////////////////////////////////////////////////////////


\ ======================================================================
\ D O E S   S U P P O R T
: (;CODE) ( -- )  R> LATEST @ NFA>CFA !  ;

\ 06A0 = BL @XXXX   0460 = B @XXXX
: DOES>    ( -- )
      COMPILE (;CODE)
      06A0 COMPILE,  ['] DODOES COMPILE,   \ compiles: BL @DODOES
; IMMEDIATE


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\  ===[ CANNOT COMPILE IF, ELSE, THEN, BEGIN UNTIL ETC. AFTER THIS ]===
\ //////////////////////////////////////////////////////////////////////
\ ======================================================================
\ COLD start. Sets Workspace, copy code to scratch pad, set stacks, run BOOT

COMPILER HEX

TARGET-COMPILING
CODE COLD
      WRKSP0 LWPI,
      R0 HSprims LI,   \ source
      R1 HSstart LI,   \ destination
      BEGIN,           \ Copy hi-speed routines to fast RAM
        *R0+ *R1+ MOV,
         R1 HSend CMPI,
      EQ UNTIL,
      SP  SP0  LI,     \ data stack
      RP  RP0  LI,     \ return stack
      R10 NEXT2 LI,    \ inner interpreter
      IP  BOOT  LI,    \ load interpreter pointer with boot word
      *R10 B,          \ run Forth NEXT (inner interpreter)
ENDCODE

COMPILER
\ ======================================================================
\ B O O T   U P   C O D E
HEX
TARGET-COMPILING

\ *G  WARM initializes variables and vectors
: WARM  ( -- )
        COLD 
        80 83C2 C!
        26 TPAD !
        1000 VP !
        H @ 2000 MAX 3FFF MIN  H !
        3FFF TMR!
        VDPTOP ^PAB !
        L0 LP !
        FLOOR ON
        DECIMAL
;

: GO   <you application goes here> ;
