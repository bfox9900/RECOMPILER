\ CAMEL99 Forth for the TI-99  First build 11Apr2019
\ Copyright (c) 2025 Brian Fox
\ KILWORTH Ontario Canada
\ brian.fox@brianfox.ca

\ This program is free software; you can redistribute it and/or modify
\ it under the terms of the GNU General Public License as published by
\ the Free Software Foundation; either version 3 of the License, or
\ (at your option) any later version.
\ You should have received a copy of the GNU General Public License
\ along with this program.  If not, see <http://www.gnu.org/licenses/>.
\
\ The work derived from CAMEL Forth under the GNU General Public License.
\ CamelForth (c) 2009 Bradford J. Rodriguez.
\ Commercial inquiries for Camel Forth should be directed to:
\    Dr. Bradford J. Rodriguez
\    115 First St., #105, Collingwood, Ontario L9Y 4W3 Canada
\    or via email to bj@camelforth.com

\ **********************************************************************
\ *                                                                    *
\ *            THIS VERSION COMPILES WITH RECOMPILER FXCC              *
\ *                        FOR TI-99                                   *
\ *                                                                    *
\ *  It builds a kernel of CORE Forth words for building applications  *
\ *                                                                    *
\ **********************************************************************

TARGET
\ ======================================================================
\ S Y S T E M   C O N S T A N T S

COMPILER HEX
CR .( Constants and VARIABLEs...)

TARGET
  'SP0  CONSTANT SP0    \ ^^^  data stack, 28 cells deep,
  'RP0  CONSTANT RP0    \ ^^^ Return stack 96 cells max, shares space with TIB
  'TIB  CONSTANT TIB    \ tib grows up towards RP0. Never more that

\ Utility constants
     0 CONSTANT FALSE
    -1 CONSTANT TRUE

\ These are 3% faster but 4 bytes bigger each 
\ CODE FALSE   TOS PUSH,  TOS CLR,  NEXT, ENDCODE  
\ CODE TRUE    TOS PUSH,  TOS SETO, NEXT, ENDCODE 

      0 CONSTANT 0 
      1 CONSTANT 1
     20 CONSTANT BL

COMPILER 
\ ======================================================================
\ U S E R   V A R I A B L E S
\ CAMEL99 uses space after workspace for user vars.
\ *G User VARIABLEs begin at >8320 for the primary Forth task
\ ** User VARIABLE 0 .. 1F are workspace registers.
HEX 

TARGET
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
      36 USER 'KEY     \ for vectored char input
      38 USER 'EMIT    \ for vectored char output
      3A USER LP
      3C USER SOURCE-ID
      3E USER 'SOURCE
\     40 USER 'SOURCE   \ uses 2 locations 

      46 USER TPAD      \ holds offset from HERE for TASK PADs
      7E USER VPG       \ Each task can have it's own video page 

\ ======================================================================
\ V A R I A B L E S

  VARIABLE STATE
  VARIABLE LATEST

\ *G These system VARIABLEs control cold starting the system
  VARIABLE ORGDP
  VARIABLE ORGLAST
  VARIABLE BOOT

COMPILER DECIMAL 
TARGET
   0024 CONSTANT L/SCR

COMPILER HEX 

TARGET
  VARIABLE VMODE
  VARIABLE L0       COMPILER 4 CELLS TALLOT  TARGET
  VARIABLE ^PAB
  VARIABLE LINES
  VARIABLE C/SCR
  VARIABLE 'IV    \ *G interpretor vector. Holds address of <INTERP>
  VARIABLE H
  VARIABLE VP
  VARIABLE CURS     COMPILER 205F CURS T! TARGET  \ BLANK and '_' in one VARIABLE
  VARIABLE VTOP
  VARIABLE WARNINGS COMPILER -1 WARNINGS T! TARGET

\ ======================================================================
COMPILER CR .( Hi-level FORTH Primitives...)
\ SLOWER 
\  : HERE      ( -- addr) DP @  ;
\  : ALLOT     ( n --)   DP +! ;
\  : ,         ( n -- )  HERE ! 2 ALLOT ;

TARGET
( faster HERE speeds up the compiler)
CODE HERE   ( -- addr) 
      TOS PUSH,
      TOS STWP,
      24 (TOS) TOS MOV,
      NEXT,
ENDCODE

CODE ALLOT  ( n --)
      R1 STWP,
      TOS 24 (R1) ADD,
      TOS POP,
      NEXT,
ENDCODE

CODE ,  ( n --)
      R1 STWP,
      24 (R1) R2 MOV,
      TOS *R2 MOV,
      24 (R1) INCT,
      TOS POP,
      NEXT,
ENDCODE

: C,        ( n -- ) HERE C! 1 ALLOT  ;
: COMPILE,  ( n -- )  , ;
: ALIGN     ( -- )   HERE ALIGNED DP ! ;
: PAD       ( -- addr) HERE TPAD @ + ;
: COMPILE   ( -- )  R> DUP 2+ >R @  , ;
: IMMEDIATE ( --)   01 LATEST @ 1-  C! ;
: LITERAL   ( n -- n|~) STATE @ IF  COMPILE LIT  ,  THEN ;  IMMEDIATE
: ]         ( -- ) STATE ON  ;
: [         ( -- ) STATE OFF ;  IMMEDIATE
: DEPTH     ( -- n ) SP0 SP@ 2+ - 2/ ; \ ** needs signed shift

COMPILER HEX 
TARGET
: DECIMAL     ( -- ) 0A BASE ! ;
: HEX         ( -- ) 10 BASE ! ;

\ ======================================================================
\ PAB Base Address
: VDPTOP  ( -- n) 8370 @ 1- ;

\ ======================================================================
\ S T A C K   P R I M I T I V E S
COMPILER CR .( Stack primitives ...)  

TARGET
: TUCK  ( w1 w2 --  w2 w1 w2 ) SWAP OVER ; \ 4 bytes, Go Forth!
\ *G NOT standard forth. Nice native 9900 instructions
CODE 1+! ( addr -- )  *TOS INC,  TOS POP,  NEXT,   ENDCODE
CODE 1-! ( addr -- )  *TOS DEC,  TOS POP,  NEXT,   ENDCODE

\ =====================================================================
\ C O M P A R I S O N   O P E R A T O R S

TARGET
: U>  ( n n -- ?)  SWAP U< ;
: 0>  ( n -- ?)    1- 0< INVERT ;
: <>  ( n n -- ?)  =  INVERT ;

\ SLOWER 
( Forth is bigger than CODE versions :-)
\ : UMIN ( u1 u2 -- u )  2DUP U> IF SWAP THEN DROP ;
\ : UMAX ( u1 u2 -- u )  2DUP U< IF SWAP THEN DROP ;

CODE UMIN     ( n1 n2 -- n)
      *SP TOS CMP,
      1 $ JL,
      SP INCT,
      NEXT,
+CODE UMAX   ( n1 n2 -- n)
      *SP  TOS CMP,
      1 $ JH,
      SP INCT,
      NEXT,
1 $:  TOS POP,
      NEXT,
ENDCODE

\ SLOWER 
\ : WITHIN ( u lo hi -- t ) OVER - -ROT - U> ;
CODE WITHIN   ( n  lo  hi -- flag )
      *SP  TOS  SUB,
      *SP+ *SP  SUB,
      TOS  *SP+ SUB,
      TOS CLR,
      NC IF,  TOS SETO, ENDIF,
      NEXT,
ENDCODE  \ 2 bytes bigger than Forth

\ =====================================================================
\ M I X E D  (32BIT/16BIT)   M A T H   O P E R A T I O N S

: */MOD  ( n1 n2 n3 -- n4 n5) >R UM* R> M/MOD ;
: S>D    ( n -- d)  DUP 0< ;
: M+     ( d n -- d) S>D  D+ ;    \ * removed S>D AUG2024 
: /MOD   ( n1 n2 -- n3 n4) >R S>D R> M/MOD ;
: /      ( n n -- n)   /MOD NIP  ;
: MOD    ( n n -- n)   /MOD DROP ;
: */     ( n n n -- n) */MOD NIP ;

\ =====================================================================
\ S T R I N G   T H I N G S
TARGET
: PLACE   ( src n dst -- ) 2DUP C! 1+ SWAP MOVE ;

\ SLOWER 
\ : /STRING ( caddr1 u1 n - caddr2 u2 ) TUCK - >R + R> ;  \ 10 bytes
CODE /STRING ( c-addr1 u1 n -- c-addr2 u2 ) \ ~20uS!!       Clks
    TOS   *SP  SUB,                               \ 18
    TOS 2 (SP) ADD,                               \ 22
    TOS POP,                         \ refill TOS   22
    NEXT,                            \ 8 bytes      62  ~20uS

: S,       ( c-addr u -- ) HERE OVER 1+ ALLOT PLACE  ALIGN ;

\ =====================================================================
\ H E A D E R   N A V I G A T I O N

TARGET
\ : NFA>LFA   ( nfa -- lfa)  3 - ;
CODE NFA>LFA    TOS -3 ADDI, NEXT, ENDCODE  ( faster, same size)

\ Changed 7F to 1F . 31 character max name length. 
\ Other bits for future use
: NFA>CFA     ( nfa -- cfa ) COUNT  1F AND + ALIGNED ;
: RECURSE     ( -- ) LATEST @ NFA>CFA , ; IMMEDIATE

\ smudge bit control in the Camel Forth
: HIDE        ( -- )  LATEST @ ( nfa) DUP C@ 80 OR  SWAP C! ;
: REVEAL      ( -- )  LATEST @ ( nfa) DUP C@ 7F AND SWAP C! ;

\ =====================================================================
\ P A R S E   W O R D
COMPILER CR .( Parsing...)

TARGET
CODE DUP>R  ( n -- n) TOS RPUSH,  NEXT, ENDCODE  \ used 3 times

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
\ S T R I N G  T O  N U M B E R   C O N V E R S I O N
COMPILER CR .( CAMEL FORTH Number conversion)
HEX

TARGET
\ SLOWER
: BASE@   BASE @ ;
 CODE BASE@
      TOS PUSH,
      R1 STWP,
      2A (R1) TOS MOV,
      NEXT,
      ENDCODE

\ SLOWER
\ : DIGIT?  ( c -- n -1)   \ if c is a valid digit
\             -- x  0   \ otherwise
\  DUP  39 > 100 AND +    \ silly looking
\  DUP 140 > 107 AND -  [CHAR] 0 -   \ but it works!
\  DUP BASE@ U< ;         \ 48 Bytes

CODE DIGIT?   ( char -- n f )
      R1 STWP,           \ multi-tasking friendly for USER VARS
      TOS PUSH,          \  dup char
      TOS -30 ADDI,      \  convert char to number
      TOS 9 CMPI,
      HI IF,             \ > 9 ?
          TOS -7 ADDI,
          TOS 9 CMPI,
          1 $ JL,        \ less than 9, jump out
      ENDIF,
      TOS 2A (R1) CMP,   \ USER var 2A (BASE)
      1 $ JHE,           \ tos>base, jump out
      TOS *SP MOV,       \ replace char with no.
      TOS SETO,          \ set flag to true
      NEXT,
( error out here)
1 $:  TOS CLR,
      NEXT,
      ENDCODE    \ 36 bytes, much faster

: UD*      ( ud1 u2 -- ud3) DUP>R * SWAP R> UM* ROT + ;

\ simplified with double while construct B. Fox 
: >NUMBER  ( ud adr u -- ud' adr' u' )
      BEGIN
        DUP 
      WHILE
        OVER C@ DIGIT?
      WHILE
           >R 
           2SWAP 
           BASE@ UD*  R> M+ 
           2SWAP
           1 /STRING
      REPEAT
      DROP 
      THEN
;

\ *G This is smaller than original ?NUMBER but ***FLAG LOGIC IS REVERSED***
: NUMBER?  ( addr len -- n ?)      \ ?=0 is good conversion
           (          -- addr len) \ bad conversion
      OVER C@   [CHAR] - = DUP>R    \ save flag for later
      IF 1 /STRING THEN             \ remove minus sign
      0 0  2SWAP >NUMBER NIP NIP    \ convert the number
      R> IF SWAP NEGATE SWAP THEN   \ negate if needed
;

\ ======================================================================
\ S T R I N G   L I T E R A L S
COMPILER HEX 

TARGET
\ run-time action of S"
: (S")    ( -- c-addr u) R>  COUNT  2DUP + ALIGNED >R ;
T' (S")  RESOLVES '(S")

COMPILER CR .( Number printing)
\ =====================================================================
\ N U M B E R   T O   S T R I N G   C O N V E R S I O N

TARGET

\ : >DIGIT   DUP 9 > IF 7 + THEN [CHAR] 0 + ;  \ 20 BYTES :)
CODE >DIGIT  ( n -- c)   \ ASM is 9 bytes, 4X faster
      TOS 9 CMPI,
      HI IF,             \ if n>9
            TOS 7 ADDI,  \ number is not base 10, add 7
      ENDIF,
      TOS CHAR 0 ADDI,  \ add ASCII 0 to TOS, create char value
      NEXT,
      ENDCODE

: <#     ( --) PAD HP ! ;

\ : HOLD   ( char -- ) HP DUP 1-! @ C! ; \ 10 bytes 
 CODE HOLD  ( char -- )
 \ *G HOLD is CODE. 4..9% faster number output than Forth version.
      R1 STWP,        \ get workspace pointer
      26 (R1) DEC,    \ DEC address in HP user variable
      26 (R1) R1 MOV, \ put the address into R1
      TOS SWPB,
      TOS *R1 MOVB,   \ store char in address in R1
      TOS POP,
      NEXT,
      ENDCODE  \ 16 bytes 

: #      ( u -- ud2 ) 0 BASE@ UM/MOD >R  BASE@ UM/MOD SWAP >DIGIT HOLD R> ;
: #S     ( ud1 -- ud2)  BEGIN  # 2DUP OR  WHILE REPEAT ;
: #>     ( ud1 -- c-addr u) 2DROP HP @ PAD OVER - ;
: SIGN   ( n -- ) 0< IF  [CHAR] -  HOLD  THEN ;

COMPILER CR .( Console output)
\ ======================================================================
\  S C R E E N   D R I V E R

COMPILER INCLUDE CC9900\SRC.ITC\CONSOLE2.FTH

\ ======================================================================
\ N U M B E R  O U T P U T 
TARGET 
: UD.    ( d -- ) <#  #S  #> TYPE SPACE ;
: U.     ( u -- ) 0 UD. ;
: (.)    ( n -- caddr len)  DUP ABS 0 <#  #S ROT SIGN  #> ;
: .      ( n -- ) (.)  TYPE SPACE ;

\ ======================================================================
\ S I M P L E   S O U N D  I N T E R F A C E

TARGET
\ write a byte to address of TMS9919 chip
\ added PAUSE for multi-tasking improvement 
: SND!   ( c -- ) PAUSE 8400 C!  ;  

COMPILER HEX 
TARGET
: BEEP     ( -- )
        80 SND! 5 SND!    \ pre-calculated values for OSC1 1390Hz
        91 SND!           \ turn on OSC1 at -2 dB level
        AA MS             \ Delay ~ 170 mS
        9F SND! ;        \ turn off OSC1

\ We use the HONK sound for ABORT like TI-BASIC does on errors
: HONK     ( -- )
      81 SND! 20 SND!   \ pre-calculated values for OSC1 218Hz
      90 SND!           \ turn on OSC1 at 0 dB level
      AA MS             \ Delay ~ 170 mS
      9F SND! ;        \ turn off OSC1

\ ======================================================================
\ I N T E R P R E T E R

: INTERPRET  ( addr len -- ) 'IV PERFORM ;

\ ======================================================================
\ Q U I T :  The  O U T E R   I N T E R P R E T E R

: QUIT     ( -- )
      RP0 RP! L0 LP !
      SOURCE-ID OFF
      [COMPILE] [
      BEGIN
        TIB DUP 52 ACCEPT SPACE
        ( addr len) INTERPRET
        STATE @ 0= IF  ."  ok"  THEN CR
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
: ABORT    ( -- )
      SP0 SP!
      VDPTOP ^PAB !   \ set base pab pointer
      CR QUIT ;

: ?ABORT   ( f caddr u --)
      ROT
      IF
        CR
        CR ." ? " TYPE  HONK
        SOURCE-ID @ ( source is NOT console)
        IF  ."  Line " LINES @ DECIMAL U.
            CR CR SOURCE TYPE
        THEN ABORT
      THEN 2DROP ;

                  \ flag        addr length
: ?ERR     ( ? -- )             HERE COUNT ?ABORT ;
: ?EXEC    ( -- ) STATE @       S" Interpret only" ?ABORT ;
: ?COMP    ( -- ) STATE @ 0=    S" Compile only"   ?ABORT ;
: ?CSP     ( -- ) SP@ CSP @ -   S" Unfinished"     ?ABORT ;
: !CSP     ( -- ) SP@ CSP ! ;

\ ======================================================================
\ S T R I N G   L I T E R A L

: S"  ( cccc" -- )          \ compiling action
\ *G Non-standard: when interpreting S" puts the string at PAD + >IN
\ ** and returns the address.
      ( cccc" --- addr len) \ interpreting action *NON-STANDARD*
      [CHAR] " PARSE
      STATE @
      IF  COMPILE (S")  S,  EXIT
      THEN PAD >IN @ + DUP>R PLACE R> COUNT
; IMMEDIATE

\ : S" ( cccc" -- )  \ OLD VERSION
\      [CHAR] " PARSE
\      STATE @
\      IF  COMPILE (S")  S,  EXIT 
\      THEN PAD DUP>R PLACE R> COUNT
\ ; IMMEDIATE

: ABORT"   ( i*x 0  -- i*x)    \ R: j*x -- j*x  x1=0
      ?COMP
      [COMPILE] S"
      COMPILE ?ABORT ; IMMEDIATE

COMPILER cr .( FIND )
\ ======================================================================
\ D I C T I O N A R Y   S E A R C H


TARGET
HASHING [IF]

 VARIABLE CONTEXT  COMPILER  0 T, 0 T, 0 T, 0 T, 0 T, 0 T, 0 T, 0 T, TARGET
\ *G Array of Root + 8 wordlists to control search order

 VARIABLE CURRENT
\ ** wordlist where definitions will be added

: <FIND>  ( caddr --  caddr  0  if not found)
\                      xt    1  if immediate,
\                      xt   -1  if "normal"
      CONTEXT @ ( HASH) @ (FIND) ;

VARIABLE 'FIND   \ *G vector for the action of find

: FIND     ( caddr -- ) 'FIND PERFORM ;
: '        ( -- xt) BL WORD FIND 0= ?ERR ;
: [']      ( -- <name> ) ?COMP  '  [COMPILE] LITERAL ; IMMEDIATE

 : POSTPONE ( <name> -- ) \ *G replaces COMPILE and [COMPILE]
      ?COMP
      BL WORD FIND DUP 0= ?ERR
      0< IF COMPILE COMPILE
      THEN  COMPILE, ; IMMEDIATE

\ ======================================================================
\ T E X T   O U T P U T

: ."  (  ccc" -- )
      [COMPILE] S"                ( -- str len)
      STATE @
      IF   COMPILE TYPE  EXIT
      THEN TYPE ; IMMEDIATE

: .(  [CHAR] ) PARSE TYPE ; IMMEDIATE

COMPILER cr .( Interpreter/compiler loop)
\ ======================================================================
\ I N T E R P R E T E R  /  C O M P I L E R

TARGET
: <INTERP>  ( i*x c-addr u -- j*x )
      'SOURCE 2!  >IN OFF
      BEGIN
        BL WORD  DUP C@   ( -- addr len)
      WHILE
        FIND ?DUP
        IF ( it's a word)
            1+ STATE @ 0= OR
            IF   EXECUTE
            ELSE COMPILE,
            THEN
        ELSE ( it's a number)
            COUNT NUMBER? ?ERR
            [COMPILE] LITERAL
        THEN
        DEPTH 0< S" Short stack" ?ABORT
      REPEAT
      DROP ;

\ ======================================================================
\ T I - 9 9   T E X T   M O D E   C O N T R O L
TARGET
VDPI/O
 [IF]
: TEXT    ( -- )
      F0 DUP 83D4 C!
      ( -- F0) 01 VWTR
      0  2 VWTR   \ set VDP screen page
      VTOP OFF    \ topline VDP offset
      VPG  OFF    \ VDP screen page offset
      17  7 VWTR  \ sets FG & BG color
      28 C/L!
      0 0 AT-XY
      2 VMODE !   \ 2=ID for 40 column "TEXT" mode
      PAGE
;
[THEN]

\ ======================================================================
\ TI-99 F I L E   S Y S T E M   I N T E R F A C E

COMPILER include CC9900\SRC.itc\DSRLINKB.hsf  \ VER B saves GPL R15
COMPILER include CC9900\SRC.itc\filesyX2.hsf

\ ======================================================================
\ D I C T I O N A R Y   C R E A T I O N

TARGET

: HEADER, ( addr len --)
      ALIGN
      CURRENT @ @ ,        \ get last NFA & compile in this LFA field
      0 C,                 \ compile the precedence byte (immediate flag)
      HERE >R              \ save HERE (ie: new NFA location)
      S,                   \ compile (addr len) as counted string
      WARNINGS @
      IF
        R@ FIND ( xt ?) NIP ( ?)
        IF
          SPACE  R@ COUNT 1F AND TYPE  ."  redefined "
        THEN
      THEN
      R> DUP LATEST !     \ HERE is NFA of last word defined
             CURRENT @ !  \ Also store in the current 'WID'
;

: HEADER ( <TEXT> )  BL PARSE-WORD HEADER, ; 

\ =======================================================
\ T A R G E T   S Y S T E M   D E F I N I N G   W O R D S
\                    text    runtime-action   parameter
\                   -------  --------------- -----------
 : CONSTANT  ( n --)  HEADER  COMPILE DOCON     COMPILE, ;
 : USER      ( n --)  HEADER  COMPILE DOUSER    COMPILE, ;
 : CREATE    ( -- )   HEADER  COMPILE DOVAR              ;
 : VARIABLE  ( -- )   CREATE                  0 COMPILE, ;

\ (:noname) from studying gforth. It's a nice factor.
 : (:NONAME) ( -- )  ['] DOCOL @ COMPILE,  HIDE  ]  ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\  =====[ CANNOT DEFINE ANY CONSTANT, VARIABLE OR USER AFTER THIS ]=====
\ //////////////////////////////////////////////////////////////////////


\ ======================================================================
\ D O E S   S U P P O R T
: (;CODE) ( -- )  R> LATEST @ NFA>CFA !  ;

\ 06A0 = BL @XXXX   0460 = B @XXXX
: DOES>    ( -- )
      COMPILE (;CODE)
      06A0 COMPILE,  ['] DODOES COMPILE,   \ compiles: BL @DODOES
; IMMEDIATE

\ ======================================================================
\ LOOPS AND BRANCH COMPILERS FOR THE TI-99 SYSTEM

COMPILER  include cc9900\SRC.itc\ISOLOOPS.HSF

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\  ===[ CANNOT COMPILE IF, ELSE, THEN, BEGIN UNTIL ETC. AFTER THIS ]===
\ //////////////////////////////////////////////////////////////////////
\ ======================================================================
\ COLD start. Sets Workspace, copy code to scratch pad, set stacks, run BOOT

COMPILER HEX

TARGET
CODE COLD
\     0 LIMI,    ( don't seem to need this)
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

CODE BYE   ( -- )
      83C4 @@ CLR,        \ clear interrupt vector
      0000 @@ BLWP,       \ ROM reset vector is at 0000
ENDCODE

\ These are defined in DSK1.SYSTEM file
\ : CODE      ( -- )  HEADER  HERE 2+ , !CSP ;
\ : NEXT,     ( -- )  045A , ;  \ B *R10
\ : ENDCODE   ( -- )  ?CSP  ;

\ : ;CODE
\   COMPILE (;CODE)
\   ?CSP COMPILE [
\   REVEAL
\ ; IMMEDIATE


COMPILER CODESEG 6000 =
 [IF]    TARGET : .VER   ." V2.78 2MAR25 SuperCart" ;
 [ELSE]  TARGET : .VER   ." V2.78 2MAR25" ;
 [THEN]

COMPILER 
\ ======================================================================
\ B O O T   U P   C O D E
\ *G  WARM initializes variables and vectors

\ debugging word
\ CODE HALT  BEGIN, R13 INC, AGAIN,  NEXT, ENDCODE 

HEX
VDPI/O [IF]
TARGET
: WARM  ( -- )
        80 83C2 C!
        26 TPAD !
        1000 VP !  ( Free VDP memory start ) 
        2000 H !   ( heap starts in low RAM)
        3FFF TMR!
        VDPTOP ^PAB !
        L0 LP !
        FLOOR ON
        SOURCE-ID OFF

        ['] <INTERP> 'IV !
        ['] <FIND>   'FIND !

        DECIMAL
        ORGDP @ DP !
        ORGLAST @ LATEST !
        LATEST DUP CONTEXT ! CURRENT !
        TEXT S" CAMEL99 Forth " TYPE  .VER
;

[ELSE]
\ RS232 WARM BOOT is slightly different over RS232
\ NOTE: Change baud rate to 4800 because that is the fastest speed 
\       That TI can handle with zero character delays. 
\       Teraterm has a minimum delay of 1mS which is too slow.

TARGET
\ write to CRT and over RS232 

: ECHO  ( addr len -- ) CR 2DUP  VROW @ 20 *  SWAP VWRITE TYPE ;

: WARM  ( -- ) 
      26 TPAD !
      1000 VP !
      50 C/L !  
      H @ 2000 MAX 3FFF MIN  H !
      3FFF TMR!
      VDPTOP ^PAB !
      L0 LP !
      FLOOR ON
      SOURCE-ID OFF

      ['] <INTERP> 'IV !
      ['] <FIND>   'FIND !
     
      RS232 /TTY1  BA=4800 8,N,1 OPEN-TTY         
      
      DECIMAL
      ORGDP @ DP !
      ORGLAST @ LATEST !
      LATEST DUP CONTEXT ! CURRENT !
      
      VROW OFF  VCOL OFF 
      CR 
      S" CAMEL99 Forth TTY -e " ECHO .VER
      S" 4800,8,n,1 "          ECHO 
;
[THEN]

\ G*  LOADSYS is the primary boot word that starts Forth and loads extensions
: LOADSYS
      WARM
      S" DSK1.START" INCLUDED
      CR QUIT 
;

\ =====================================================================
\ define target comment words
TARGET
: (         [CHAR] ) PARSE 2DROP ; IMMEDIATE
: \                1 PARSE 2DROP ; IMMEDIATE

COMPILER
\ =====================================================================
\ TARGET Colon, :NONAME and Semi-colon  definitions
\ X: ;X are aliases for the cross-compiler : and ;  (to keep me sane)

TARGET
 X: :         !CSP  HEADER (:NONAME)  ;X

 X: :NONAME   HERE  !CSP   (:NONAME)  ;X

 X: ;        [  REVEAL COMPILE EXIT ?CSP ;X  IMMEDIATE


COMPILER
\           F O R T H   S Y S T E M   C O D E   E N D S
\ /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
cr .( Forth Kernel compiled completely")
