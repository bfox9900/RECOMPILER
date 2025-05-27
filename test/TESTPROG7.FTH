\ TESTPROG3.FTH  test if else then 

\ You see T on 1st line and F on 3rd line.


COMPILER 
NEW 

HEX A000 ORG 

\ ===============[ THIS WILL BE PUT INTO A SINGLE FILE ]==================
\ target code starts here
\ build:;  EXIT NEXT DOCOL DOVAR DOCON LIT ?BRANCH BRANCH (NEXT)
WARNINGS OFF 
INCLUDE DSK7.EXECUTORS  

\ IMPORT:  needs the word DOVAR and so must be loaded after EXECUTORS 
INCLUDE DSK7.IMPORT     \ teach cross-compiler how "steal" host code words
INCLUDE DSK7.ITCTYPES   \ teach compiler CONSTANT VARIABLE : ; etc. 
INCLUDE DSK7.BRANCHING  \ teach cross-compiler IF ELSE THEN BEGIN FOR NEXT 

INCLUDE DSK7.DOLOOP 
INCLUDE DSK7.COMPARISON

\ need the VDP words 
INCLUDE DSK7.VDP-API
INCLUDE DSK7.TICKTOCK

COMPILER WARNINGS ON 
\ ========================================================================
COMPILER HEX 

IMPORT:  ?TERMINAL 

TARGET
CODE BYE   ( -- )
    0 LIMI, 
    83C4 @@ CLR,  
    0000 @@ BLWP, 
ENDCODE

\ console.fth provides I/O for the TI-99 console    May 2024  Brian Fox 

\ *MODIFIED: remove all USER variables

COMPILER CR .( Console driver)

DECIMAL 
\ dependancies in this file 
IMPORT: 2@ 2!  
IMPORT: DUP>R R> >R R@
IMPORT: + - 4*  * 1- = 
IMPORT: DUP DROP SWAP OVER 2DROP 2DUP >< OVER=
IMPORT: @ !  C@ C! BOUNDS 

TARGET

TCREATE VROW  COMPILER 0 T, 0 T, 


VROW CELL+ TARGET CONSTANT VCOL 
VARIABLE C/L    COMPILER 32 C/L T!  ( needs initial value)

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

TARGET 
: TYPE   ( addr cnt --) ( PAUSE) BOUNDS ?DO  I C@ (EMIT)  LOOP ;

: SPACE  ( -- )   BL (EMIT) ; 
: SPACES ( n -- ) 0 MAX  0 ?DO  SPACE  LOOP ;


COMPILER DECIMAL 

TARGET 
: MAIN  
    32 C/L!   
    0 0 AT-XY   65 (EMIT)
    BEGIN 
       ?TERMINAL 
    UNTIL 
    BYE 
    
;

\ we can change the default stack location and workspace here 
COMPILER  
HEX 
8300 TO WKSPACE 
83BE TO DSTACK 
836E TO RSTACK 

\ Using the values above we create the program entry word: COLD
\ Also creates a target variable called BOOT 
INCLUDE DSK7.PREAMBLE 

COMPILER 
PREAMBLE COLD 
BOOT AUTOSTART MAIN 
END. 

SAVE DSK7.ITCTEST7

HOST 
