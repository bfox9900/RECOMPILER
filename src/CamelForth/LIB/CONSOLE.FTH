COMPILER cr .( Console output)

HEX TARGET
\ FAST user variable for screen driver
CODE C/L@   ( -- n )        \ used 8 times in the KERNEL. Saves 24 bytes
      TOS         PUSH,  \ and is 50% faster than C/L @
      TOS         STWP,
      2E (TOS) TOS MOV,
      NEXT,
ENDCODE

: C/L!     ( c/l -- )  \ pronounced "SEE-PER-ELL-STORE"
      DUP C/L !           \ set chars per line
      L/SCR *  C/SCR ! ;  \ calc. chars per screen

\ : >VPOS  ( x y -- Vaddr) C/L@ * + VPG @ + ; \ 12 bytes
CODE >VPOS ( col row -- vaddr) \ compute a VDP screen address
      R1         STWP,
      TOS     R3  MOV,   \ this move to make best use of MPY
      2E (R1) R3  MPY,   \ mult by chars/line. result goes to R4 ie: TOS
     *SP+     TOS ADD,   \ add col value to TOS
      7E (R1) TOS ADD,   \ add Video page offset
      NEXT,
ENDCODE \ 14 BYTES

: VPOS   ( -- vaddr) VROW 2@ >VPOS ;
: VPUT   ( char -- ) VPOS VC! ;
: AT-XY  ( col row -- ) VROW 2! ;  \ set VDP address for Fast type

\  : TOPLN   ( -- vaddr)  VPG @ VTOP @ + ;  \ 10 bytes
CODE TOPLN ( -- vaddr)
      TOS PUSH,
      VPG @@  TOS MOV,
      VTOP @@ TOS ADD,
      NEXT,
      ENDCODE  \ 12 bytes

 \ : LASTLN  ( -- vaddr)  VPG @ C/SCR @ + ; \ 10 bytes
CODE LASTLN ( -- vaddr)
      TOS PUSH,
      VPG   @@ TOS MOV,
      C/SCR @@ TOS ADD,
      NEXT,
      ENDCODE  \ 12 bytes

\ =====================================================================
\ *G Scrolling has been implemented in Forth using VREAD & VWRITE
\ ** Uses un-allocated Dictionary as a temporary buffer to hold lines of text

\ 8 line buffer is faster
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

COMPILER HEX TARGET
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
