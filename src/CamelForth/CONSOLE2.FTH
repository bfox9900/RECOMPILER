\ console.fth provides I/O for the TI-99 console    May 2024  Brian Fox
\ Changed to reduce size by using more Forth code
\ Mar 2 2025: used ?DO in "SPACES"

COMPILER CR .( Console output/input)

HEX

TARGET
\ FAST user variable for screen driver
CODE C/L@   ( -- n )        \ used 8 times in the KERNEL. Saves 24 bytes
         TOS         PUSH,  \ and is 50% faster than C/L @
         TOS         STWP,
         2E (TOS) TOS MOV,
         NEXT,
ENDCODE

: C/L!  ( c/l -- )  \ pronounced "SEE-PER-ELL-STORE"
        DUP C/L !           \ set chars per line
        L/SCR *  C/SCR !    \ calc. chars per screen
;

\ : >VPOS  ( x y -- Vaddr) C/L@ * + VPG @ + ; \ 12 bytes
CODE >VPOS ( col row -- vaddr) \ compute a VDP screen address
         R1         STWP,
         TOS     R3  MOV,   \ this move to make best use of MPY
         2E (R1) R3  MPY,   \ multiply by chars/line. result goes to R4 ie: TOS
        *SP+     TOS ADD,   \ add col value to TOS
         7E (R1) TOS ADD,   \ add Video page offset
         NEXT,
ENDCODE \ 14 BYTES

: TOPLN   ( -- vaddr)  VPG @ VTOP @ + ;
: VPOS    ( -- Vaddr) VROW 2@ >VPOS ;
: VPUT    ( char -- ) VPOS VC! ;
: AT-XY   ( col row -- ) VROW 2! ;

\ =====================================================================
\ *G Scrolling has been implemented in Forth using VREAD & VWRITE
\ ** Uses un-allocated Dictionary as a temporary buffer to hold lines of text
COMPILER DECIMAL

TARGET
\ compromise for speed/buffer_size
: SCROLL ( -- )
   HERE 80 +  TOPLN   ( -- buffer screen)
   6 0 DO                 \ 6 loops x 4 lines = 24 line scroll
      2DUP 2DUP  C/L@ +
      SWAP  C/L@ 4* DUP>R VREAD  R@ VWRITE  R> +
   LOOP
   2DROP ( 50 bytes )
   0 23 AT-XY  VPOS C/L@ BL VFILL
;

\ full screen buffer, smallest and fastest
\ :  SCROLL ( -- )
\   HERE 80 +  TOPLN   ( -- buffer screen)
\   2DUP C/L@ + SWAP  C/SCR @ C/L@ - DUP>R VREAD  R> VWRITE
\   0 23 2DUP >VPOS C/L@ BL VFILL  AT-XY
\ ;

\ ======================================================================
\ V D P   T E X T   O U T P U T

COMPILER HEX

TARGET
: PAGE   ( -- ) TOPLN  C/SCR @  BL VFILL   0 0 AT-XY ;

\ : ++@   ( addr -- n)  DUP 1+! @ ;
CODE ++@ ( addr -- n) *TOS INC,  *TOS TOS MOV,  NEXT, ENDCODE

\ CODE WMODE  _WMODE @@ BL, TOS POP,   NEXT, ENDCODE
: CR     (  -- ) VCOL OFF  VROW ++@  L/SCR = IF SCROLL THEN ;

: (EMIT) ( char -- ) VPOS VC!  VCOL ++@ C/L@ = IF CR THEN ;

: BS     ( -- )   VCOL DUP @ 1- 0 MAX  SWAP ! ;

: EMIT   ( char -- )  \ handles some control characters
      PAUSE
      0D OVER= IF  DROP CR     EXIT THEN
      08 OVER= IF  DROP BS     EXIT THEN
      (EMIT) ;

: TYPE   ( addr cnt --) PAUSE BOUNDS ?DO  I C@ (EMIT)  LOOP ;
T' TYPE  RESOLVES 'TYPE

: SPACE  ( -- )   BL (EMIT) ;
: SPACES ( n -- ) 0 MAX 0 ?DO  SPACE  LOOP ;

COMPILER CR .( Console input)
\ ======================================================================
\ C H A R A C T E R   I N P U T
COMPILER CR .( KSCAN KEY?  and  ?TERMINAL)

TARGET
\ Camel99 interface to KSCAN ROM code
CODE KEY? ( -- char)  \ *WARNING* it takes 1,128uS for KEY? scan to run
        TOS PUSH,
        TOS CLR,            \ TOS will be our true/false flag
        0 LIMI,             \ disable interrupts
        TOS 837C @@ MOVB,   \ clear GPL flags
        83E0 LWPI,          \ switch to GPL workspace
        000E @@ BL,         \ call ROM keyboard scanning routine
        WRKSP0 LWPI,        \ return to Forth's workspace
        837C @@ R1 MOVB,    \ read GPL status byte (=2000 if key pressed)
        R1  3 SLA,          \ check the key bit
        OC IF,              \ if carry flag set
          \  TOS SETO,
           8375 @@ TOS MOV, \ read the key ( LEGACY reasons)
        ENDIF,
        2 LIMI,
        NEXT,
        ENDCODE

\ *G Cursor flash control is done by reading the 9901 timer.
\ ** Counts down from >3FFF in 349mS. If the timer > 1FFF we show the cursor.
\ ** If < 1FFF show the screen char. Flash rate is about 3 per second.

: KEY      ( -- char)
        BEGIN
           PAUSE              \ Essential for Multi-tasking with Console
           CURS @             \ fetch 2 char cursor (space & _ )
           TMR@ 1FFF <
           IF >< THEN VPOS VC!
           KEY?
        UNTIL                 \ loop until a key pressed
        8375 C@               \ read key code from RAM buffer
        BL VPOS VC! ;         \ put the space char on screen

\ High level: input/output          (c) 31mar95 bjr
: ACCEPT     ( caddr +n -- +n')
        OVER + OVER         \ removed 1-  to accept all chars
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

\ *---------------------------------------------
\ * Call very fast built-in Fctn-4 test in console ROM
\   http://www.unige.ch/medecine/nouspikel/ti99/keyboard.htm
\ *---------------------------------------------
CODE ?TERMINAL ( -- ?)  \ ~200uS, 16 bit ROM code. Same name as TI-Forth
        R12     RPUSH,       \ save CRU address makes life simpler
        TOS     PUSH,
        TOS     CLR,
        0020 @@ BL,          \ Call ROM code, test FNCT 4 key press
        EQ IF,               \ if flag=0
           TOS  SETO,        \ KEY is pressed, TOS = -1
           BEGIN,
              0020 @@ BL,    \ call rom routine
           NE UNTIL,         \ until key is released
        ENDIF,
        R12     RPOP,        \ Restore CRU address used by CAMEL Forth
        NEXT,
        ENDCODE
