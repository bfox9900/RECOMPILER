\ Standard Forth output words in minimal Forth Sept 16 2023
COMPILER HEX 

TARGET
8800 CONSTANT VDPRD               \ vdp ram read data
\ 8802 CONSTANT VDPSTS            \ vdp status
8C00 CONSTANT VDPWD               \ vdp ram write data
8C02 CONSTANT VDPWA               \ vdp ram read/write address

\ VDP set-address sub-routines
 CODE 0LIMI   0 LIMI,   NEXT,  ENDCODE
: RMODE ( vdpaddr -- ) DUP 0LIMI VDPWA C! >< VDPWA C! ;
: WMODE ( vdpaddr -- ) 4000 OR RMODE ;

: VFILL   ( vaddr len c -- )
    ROT WMODE 
    SWAP 
    BEGIN
      OVER VDPWD C!
      1- DUP
    -UNTIL
    2DROP ;

\ SCREEN I/O -----------
COMPILER DECIMAL 
TARGET 
VARIABLE C/L       COMPILER 32 C/L T!

TARGET
VARIABLE COL
VARIABLE ROW
VARIABLE CURSOR
VARIABLE C/SCR     COMPILER 766 C/SCR T!

TARGET
20 CONSTANT BL

: CLIP   ( n lo hi -- n) ROT MIN MAX ;
: >VPOS  ( col row -- vaddr) C/L @ * + ;
: CURSOR ( -- Vaddr) COL @ ROW @ >VPOS  ;
: AT-XY  ( col row -- ) ROW ! COL ! CURSOR WMODE ;

: ?WRAP ( -- )
  COL @ C/SCR @ 1- >  IF  0 0 AT-XY  THEN  ;

: ROW+!  ( n -- ) 
  ROW @ + DUP 24 < IF  ROW ! EXIT  THEN ROW OFF ;

: EMIT   ( c --) CURSOR WMODE VDPWD C! ?WRAP ; 
: CR     ( -- )  1 ROW+! COL OFF CURSOR WMODE ;
: SPACE  ( -- )   BL EMIT ;

: TYPE   ( addr len -- )
    CURSOR WMODE 
    >R
    BEGIN
      COUNT VDPWD C! ?WRAP   
      R> 1- DUP>R
    -UNTIL
    R> 2DROP ;

: PAGE   0  C/SCR @ BL VFILL  0 0 AT-XY ;

