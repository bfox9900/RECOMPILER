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
 
 
VARIABLE C/L   
VARIABLE COL
VARIABLE ROW
VARIABLE CURSOR
VARIABLE C/SCR   
 
TARGET
20 CONSTANT BL

: >VPOS  ( col row -- vaddr)  C/L @ * + ;

\ CODE >VPOS  ( col row -- vaddr) 
\         TOS     R3  MOV,   \ this move to make best use of MPY
\         C/L @@  R3  MPY,   \ multiply by chars/line. Result->TOS
\        *SP+     TOS ADD,   \ col value & add to TOS
\         NEXT,
\ ENDCODE

CODE CURSOR ( -- Vaddr)  COL @ ROW @ >VPOS  ;

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
ENDCODE \ 12 BYTES

: VREAD   ( Vaddr addr len --) ROT  RMODE (VREAD) ;
: VWRITE  ( addr Vaddr len --) SWAP WMODE (VWRITE) ;
: VFILL   ( vaddr len c -- )   ROT  WMODE (VFILL) ;

\ **************************************************
: AT-XY  ( col row -- ) ROW ! COL ! CURSOR WMODE ;
: PAGE   ( -- ) 0  C/SCR @ BL VFILL  0 0 AT-XY ;
 
: ?WRAP  ( -- ) COL @ C/SCR @ 1- > IF  0 0 AT-XY  THEN  ;
 
: ROW+!  ( n -- ) ROW @ +  23 > IF  ROW OFF EXIT  THEN ROW ! ;
: EMIT+  ( c -- ) VC!+ COL 1+! ?WRAP ;
: EMIT   ( c --)  CURSOR WMODE EMIT+ ;
: CR     ( -- )   1 ROW+!  COL OFF ;
: SPACE  ( -- )   BL EMIT ;
 
: TYPE   ( addr len -- ) 
  1- 
  CURSOR WMODE 
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


