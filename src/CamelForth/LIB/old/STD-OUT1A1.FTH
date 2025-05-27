\ STD-OUT1A.FTH output words in Forth + minimal code Sept 17 2023
 
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
 
 
VARIABLE C/L      COMPILER 20 C/L T!
TARGET
VARIABLE COL
VARIABLE ROW
VARIABLE CURSOR
VARIABLE C/SCR     COMPILER 3C0 C/SCR T!
 
TARGET
20 CONSTANT BL
 
\ : EMIT+  ( c --) VDPWD C! ; \ write & inc. address
 CODE VC!+ ( c --)
    TOS SWPB,
    TOS VDPWD @@ MOVB,
    TOS POP,
    NEXT,
 ENDCODE

: VFILL   ( vaddr len c -- ) 
  ROT WMODE SWAP FOR  DUP VC!+  NEXT DROP  ;
 
: >VPOS  ( col row -- vaddr) C/L @ * + ;
: CURSOR ( -- Vaddr) COL @ ROW @ >VPOS  ;
: AT-XY  ( col row -- ) ROW ! COL ! CURSOR WMODE ;
: PAGE   ( -- ) 0  C/SCR @ BL VFILL  0 0 AT-XY ;
 
: ?WRAP  ( -- ) COL @ C/SCR @ 1- > IF  0 0 AT-XY  THEN  ;
 
: ROW+!  ( n -- ) ROW @ +  23 > IF  ROW OFF EXIT  THEN ROW ! ;
: EMIT+  ( c -- ) VC!+ COL 1+! ?WRAP ;
: EMIT   ( c --)  CURSOR WMODE EMIT+ ;
: CR     ( -- )   1 ROW+!  COL OFF ;
: SPACE  ( -- )   BL EMIT ;
 
: TYPE   ( addr len -- ) 1- CURSOR WMODE FOR  COUNT EMIT+  NEXT DROP ;
 
COMPILER ALSO META DEFINITIONS 
HOST: ."   
   [CHAR] " PARSE  
   TCOMPILE (S")  TS, 
   TCOMPILE TYPE 
;HOST IMMEDIATE 


