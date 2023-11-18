\ STD-OUT1A.FTH output words in Forth + minimal code Sept 17 2023
 
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

: >VPOS  ( col row -- vaddr) C/L @ * + ;
: CURSOR ( -- Vaddr) COL @ ROW @ >VPOS  ;

\ **********[ VDP byte READ/WRITE ]****************
CODE VC@+ (  -- c) \ VDP read byte & inc. address
    VDPWD @@ TOS MOVB,
    TOS 8 SRA,
    NEXT,
ENDCODE     

: VC@  ( Vaddr -- c) CURSOR RMODE VC@+ ;
 
 CODE VC!+ ( c --) \ VDP write byte & inc. address
    TOS SWPB,
    TOS VDPWD @@ MOVB,
    TOS POP,
    NEXT,
 ENDCODE
: VC!  ( c Vaddr --) CURSOR WMODE VC!+ ;

\ **********[ VDP BLOCK READ/WRITE/FILL ]****************
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

CODE (VREAD) ( addr len -- )
    R0 POP,  \ addr->R0
    VDPRD R1 LI, 
    BEGIN, 
      *R1 *R0+ MOVB,
      TOS DEC, 
    EQ UNTIL, 
    TOS POP,
    NEXT,
ENDCODE       

CODE (VWRITE) ( addr len --)
    R0 POP, 
    VDPWD R1 LI, 
    BEGIN, 
       *R1+ *R0 MOVB,
       TOS DEC, 
    EQ UNTIL,    
    TOS POP, 
    NEXT,
ENDCODE 

: VREAD   ( Vaddr addr len --) ROT  RMODE (VREAD) ;
: VWRITE  ( addr Vaddr len --) SWAP WMODE (VWRITE) ;
: VFILL   ( vaddr len c -- )   ROT  WMODE (VFILL) ;

\ **************************************************
: AT-XY  ( col row -- ) ROW ! COL ! CURSOR WMODE ;
: PAGE   ( -- ) 0  C/SCR @ BL VFILL  0 0 AT-XY ;
 
0    CONSTANT 1STLINE 
FD00 CONSTANT LNBUFFER

: SCROLL8 ( Vaddr -- Vaddr')
    DUP             \ keep copy of 1st line   
    C/L @ + ( -- 1st 2nd ) LNBUFFER  C/L @ 8*  VREAD 
    LNBUFFER  OVER  C/L @ 8* VWRITE 
    C/L @ 8* +
;

: SCROLL
    1STLINE  SCROLL8 SCROLL8 SCROLL8 DROP
    0 17 AT-XY  C/L @ BL (VFILL) \ AT-XY sets the VDP address
;

: ?WRAP  ( -- ) COL @ C/SCR @ 1- > IF SCROLL THEN ;
 
: ROW+!  ( n -- ) ROW @ +  23 > IF  ROW OFF EXIT  THEN ROW ! ;
: EMIT+  ( c -- ) VC!+ COL 1+! ?WRAP ;
: EMIT   ( c --)  CURSOR WMODE EMIT+ ;
: CR     ( -- )   1 ROW+!  COL OFF ;
: SPACE  ( -- )   BL EMIT ;
 
: TYPE   ( addr len -- ) 
  1- CURSOR WMODE 
  FOR  
    COUNT EMIT+  
  NEXT 
  DROP ;

COMPILER ALSO META DEFINITIONS 
HOST: ."   
   [CHAR] " PARSE  
   TCOMPILE (S")  TS, 
   TCOMPILE TYPE 
;HOST IMMEDIATE 


