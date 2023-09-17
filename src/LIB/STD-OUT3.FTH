\ Standard Forth output words in Forth w/ FORNEXT Sept 16 2023
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
: EMIT+  ( c --) VDPWD C! ; \ write & inc. address

: VFILL   ( vaddr len c -- )
    ROT WMODE  SWAP FOR   OVER EMIT+  NEXT DROP  ;

VARIABLE C/L      COMPILER 20 C/L T!

TARGET
VARIABLE COL
VARIABLE ROW
VARIABLE CURSOR
VARIABLE C/SCR     COMPILER 3C0 C/SCR T!

TARGET
20 CONSTANT BL

: CLIP   ( n lo hi -- n) ROT MIN MAX ;
: >VPOS  ( col row -- vaddr) C/L @ * + ;
: CURSOR ( -- Vaddr) COL @ ROW @ >VPOS  0 C/SCR @ CLIP ;

: COL+!  ( n -- )
  COL @ +  DUP C/SCR @ >
  IF DROP 
    COL OFF  ROW OFF 
    EXIT

  THEN COL ! ;

: ROW+!  ( n -- ) ROW @ +  0 23 CLIP ROW ! ;
: EMIT   ( c --)  CURSOR WMODE EMIT+ 1 COL+! ; 
: CR     ( -- )   1 ROW+!  COL OFF ;
: SPACE  ( -- )   BL EMIT ;

: TYPE   ( addr len -- )
    CURSOR WMODE 
    >R
    BEGIN
      COUNT EMIT  
      R> 1- DUP>R
    -UNTIL
    R> 2DROP ;

: AT-XY  ( col row -- ) ROW ! COL ! CURSOR WMODE ;
: PAGE   0  C/SCR @ BL VFILL  0 0 AT-XY ;

