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

\ screen management variables 
VARIABLE C/L      COMPILER 20 C/L T!
TARGET
VARIABLE COL
VARIABLE ROW
VARIABLE CURSOR
VARIABLE C/SCR     COMPILER 3C0 C/SCR T!

TARGET
: CLIP   ( n lo hi -- n) ROT MIN MAX ;
: >VPOS  ( col row -- vaddr) C/L @ * + ;
: CURSOR ( -- Vaddr) COL @ ROW @ >VPOS  ( 0  C/SCR @ 1- CLIP) ;
: AT-XY  ( col row -- ) ROW ! COL ! CURSOR WMODE ;

: ?WRAP ( -- )
  COL @ C/SCR @ 1- > IF  0 0 AT-XY  THEN  ;

: ROW+!  ( n -- ) 
  ROW @ + DUP 24 > 
  IF   ROW OFF  EXIT  
  THEN ROW ! ;

CODE VC!+ ( c --) \ write char,  inc. address
    TOS SWPB, 
    TOS VDPWD @@ MOVB, 
    TOS POP, 
    NEXT, 
ENDCODE     

: EMIT+  ( c-- ) VC!+ COL 1+! ?WRAP ;
: EMIT   ( c --)  CURSOR WMODE EMIT+ ; 
: CR     ( -- )   1 ROW+!  COL OFF ;

20 CONSTANT BL
: SPACE  ( -- )   BL EMIT ;

: VFILL   ( vaddr len c -- )
  ROT WMODE  SWAP FOR  DUP VC!+  NEXT DROP  ;

: TYPE   ( addr len -- ) 
  CURSOR WMODE  
  1- FOR  COUNT EMIT+  NEXT DROP ; 
 
 COMPILER  T' TYPE CONSTANT 'TYPE

TARGET 
: PAGE   0  C/SCR @ BL VFILL  0 0 AT-XY ;

\ META-compiling "dot-quote" uses TYPE so put here 
COMPILER ALSO META DEFINITIONS 
HOST: ."        
    [CHAR] "  PARSE  
    TCOMPILE (S")  TS,     
    'TYPE T, 
;HOST IMMEDIATE 

COMPILER 


