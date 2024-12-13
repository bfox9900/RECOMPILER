\ console.fth provides I/O for the TI-99 console    May 2024  Brian Fox 

\ *MODIFIED: remove all USER variables

COMPILER CR .( Console driver)

DECIMAL 
\ dependancies in this file 
IMPORT: 2@ 2!  
IMPORT: DUP>R R> >R R@
IMPORT: + - 4*  * 1- 
IMPORT: DUP DROP SWAP OVER 2DROP 2DUP ><
IMPORT: @ !  C@ C! 

TARGET

TCREATE VROW 0 T, 0 T, 

VROW CELL+ CONSTANT VCOL 
VARIABLE C/L     32 C/L T!  ( needs initial value)

TARGET 
VARIABLE C/SCR  768 C/SCR T! 

TARGET 
VARIABLE VPG 
VARIABLE VTOP  

24 CONSTANT L/SCR 
32 CONSTANT BL 

: C/L@   C/L @ ;

: C/L!  ( c/l -- )  \ pronounced "SEE-PER-ELL-STORE"
        DUP C/L !           \ set chars per line
        L/SCR *  C/SCR !    \ calc. chars per screen
;

: TOPLN   ( -- vaddr)  VPG @ VTOP @ + ; 

 CODE >VPOS ( col row -- vaddr) \ compute a VDP screen address
      TOS     R3  MOV,   \ this move to make best use of MPY
      C/L @@  R3  MPY,   \ multiply by chars/line. result goes to R4 ie: TOS
     *SP+     TOS ADD,   \ add col value to TOS
      VPG @@  TOS ADD,   \ add Video page offset
      NEXT,
      ENDCODE \ 14 BYTES

: VPOS    ( -- Vaddr) VROW 2@ >VPOS ; 
: VPUT    ( char -- ) VPOS VC! ;
: AT-XY   ( col row -- ) VROW 2! ; 

\ =====================================================================
\ *G Scrolling has been implemented in Forth using VREAD & VWRITE
\ ** Uses un-allocated Dictionary as a temporary buffer to hold lines of text
COMPILER DECIMAL TARGET

\ BOGUS buffer for testing 
8192 CONSTANT HERE 

: SCROLL ( -- )
   HERE 80 +  TOPLN   ( -- buffer screen)
   6 0 DO                 \ 6 loops x 4 lines = 24 line scroll
      2DUP 2DUP  C/L@ +   
      SWAP  C/L@ 4* DUP>R VREAD  R@ VWRITE  R> +         
   LOOP
   2DROP ( 50 bytes )

   0 23 AT-XY  VPOS C/L@ BL VFILL  
;

\ ======================================================================
\ V D P   T E X T   O U T P U T

COMPILER HEX TARGET

: PAGE   ( -- )   0 300 BL VFILL  ;

\ : ++@   ( addr -- n)  DUP 1+! @ ; 
CODE ++@ ( addr -- n) *TOS INC,  *TOS TOS MOV,  NEXT, ENDCODE 

: CR     (  -- ) VCOL OFF  VROW ++@  L/SCR = IF SCROLL THEN ;

: (EMIT) ( char -- ) VPOS VC!  VCOL ++@ C/L@ = IF CR THEN ;

: BS     ( -- )   VCOL DUP @ 1- 0 MAX  SWAP ! ;

: EMIT   ( char -- )  \ handles some control characters
 \     PAUSE
      0D OVER= IF  DROP CR     EXIT THEN
      08 OVER= IF  DROP BS     EXIT THEN
      (EMIT) ;

IMPORT: BOUNDS

TARGET 
: TYPE   ( addr cnt --) ( PAUSE) BOUNDS ?DO  I C@ (EMIT)  LOOP ;

: SPACE  ( -- )   BL (EMIT) ; 
: SPACES ( n -- ) 0 MAX  0 ?DO  SPACE  LOOP ;

COMPILER  CR .( Console input)
HEX 
TARGET
CODE KEY? ( -- char)  \ *WARNING* it takes 1,128uS for KEY? scan to run
        TOS PUSH,
        TOS CLR,            \ TOS will be our true/false flag
        0 LIMI,             \ disable interrupts
        TOS 837C @@ MOVB,   \ clear GPL flags
        83E0 LWPI,          \ swiTCh to GPL workspace
        000E @@ BL,         \ call ROM keyboard scanning routine
        8300 LWPI,        \ return to Forth's workspace
        837C @@ R1 MOVB,    \ read GPL status byte (=2000 if key pressed)
        R1  3 SLA,          \ check the key bit
        OC IF,              \ if carry flag set
           8375 @@ TOS MOV, \ read the key ( LEGACY reasons)
        ENDIF,
        2 LIMI, 
        NEXT,          
        ENDCODE

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


\ *G Cursor flash control is done by reading the 9901 timer.
\ ** It counts down from >3FFF in 349mS. If the timer > 1FFF we show the cursor.
\ ** If < 1FFF show the screen char. Flash rate is about 3 per second.

VARIABLE CURS   5F20 CURS T!  \ space and '_' chars in one cell 

: KEY      ( -- char)
        BEGIN          
   \        PAUSE              \ Essential for Multi-tasking with Console
           CURS @             \ feTCh 2 char cursor (space & _ )
           TMR@ 1FFF < 
           IF >< THEN VPOS VC! 
           KEY?
        UNTIL                 \ loop until a key pressed
        8375 C@               \ read key code from RAM buffer 
        BL VPOS VC! ;         \ put the space char on screen


IMPORT: PICK UMAX UMIN NIP INVERT 1- 1+ 

TARGET

: <>  ( n n -- ?)  =  INVERT ;

\ High level: input/output          (c) 31mar95 bjr
: ACCEPT     ( caddr +n -- +n')
        OVER + OVER         \ removed 1-  to aCCept all chars
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

