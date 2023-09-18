\ STD-OUT1C.FTH output words in Forth + minimal code Sept 17 2023

COMPILER HEX 

TARGET
8800 CONSTANT VDPRD               \ vdp ram read data
\ 8802 CONSTANT VDPSTS            \ vdp status
8C00 CONSTANT VDPWD               \ vdp ram write data
8C02 CONSTANT VDPWA               \ vdp ram read/write address

\ VDP set-address sub-routines
\ CODE 0LIMI   0 LIMI,   NEXT,  ENDCODE
\ : RMODE ( vdpaddr -- ) DUP 0LIMI VDPWA C! >< VDPWA C! ;
\ : WMODE ( vdpaddr -- ) 4000 OR RMODE ;

\ this replaces the Forth words above and is 10 bytes smaller  
 CODE WMODE 
    TOS 4000 ORI,   
+CODE RMODE ( we can fall thru because there is no dictionary)   
    0 LIMI,
    TOS SWPB,  TOS VDPWA @@ MOVB, 
    TOS SWPB,  TOS VDPWA @@ MOVB,
    TOS POP, 
    NEXT, 
ENDCODE 

VARIABLE C/L      COMPILER 20 C/L T!

TARGET
VARIABLE COL
VARIABLE ROW
VARIABLE CURSOR
VARIABLE C/SCR     COMPILER 3C0 C/SCR T!

TARGET
20 CONSTANT BL

\ : EMIT+  ( c --) VDPWD C!  COL 1+! ; \ write & inc. address
 CODE EMIT+ ( c --)
    TOS SWPB, 
    TOS VDPWD @@ MOVB, 
    COL @@ INC, 
    TOS POP, 
    NEXT, 
 ENDCODE  \ 4 bytes bigger but way faster

CODE (VFILL) ( len c -- )
    TOS SWPB, 
    R0 POP,   \ len -> R0 from data stack memory
    BEGIN,
      TOS VDPWD @@ MOVB,  
      R0 DEC, 
    EQ UNTIL, 
    TOS POP, 
    NEXT,
ENDCODE  ( 14 BYTES)

: VFILL   ( vaddr len c -- ) ROT WMODE (VFILL) ;

: CLIP   ( n lo hi -- n) ROT MIN MAX ;
: >VPOS  ( col row -- vaddr) C/L @ * + ;
: CURSOR ( -- Vaddr) COL @ ROW @ >VPOS ;
: AT-XY  ( col row -- ) ROW ! COL ! CURSOR WMODE ;

: ?WRAP ( -- )
  COL @ C/SCR @ 1- >  IF  0 0 AT-XY  THEN  ;

: ROW+!  ( n -- ) ROW @ +  0 23 CLIP ROW ! ;
: CR     ( -- )   1 ROW+!  COL OFF  CURSOR WMODE ;
: EMIT   ( c --)  ?WRAP EMIT+ ; 
: TYPE   ( addr len -- ) CURSOR WMODE  FOR  COUNT EMIT+ ?WRAP  NEXT DROP ; 

: SPACE  ( -- )   BL EMIT ;
: PAGE   0  C/SCR @ BL VFILL  0 0 AT-XY ;

