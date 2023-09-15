COMPILER CR ." V D P  C H I P  I N T E R F A C E"
\ word set in Forth to manage the tms9918 chip

NEEDS ?DO  FROM DSK7.DOLOOP 

COMPILER HEX 

TARGET

HERE 
HEX 
8800 CONSTANT VDPRD               \ vdp ram read data
8802 CONSTANT VDPSTS              \ vdp status
8C00 CONSTANT VDPWD               \ vdp ram write data
8C02 CONSTANT VDPWA               \ vdp ram read/write address

\ VDP set-address sub-routines
CODE 0LIMI   0300 , 0000 ,   NEXT,  ENDCODE 

: RMODE ( vdpaddr -- ) DUP 0LIMI VDPWA C! >< VDPWA C! ;
: WMODE ( vdpaddr -- ) 4000 OR RMODE ;      

: VC@+  ( Vdpaddr -- c) VDPRD C@ ; \ read & inc. address
: VC@   ( VDP-adr -- char ) RMODE VDPRD C@ ; 

: VC!+  ( c --) VDPWD C! ;         \ write & inc. address 
: (TYPE) ( addr len --) 0 DO  COUNT VC!+  LOOP DROP ;

: VC!   ( c vaddr --) WMODE VC!+ ; \ set address and write 

\ VDP integer fetch & store
: V@    ( VDPadr -- n) VC@  VC@+  FUSE  ; 
: V!    ( n vaddr --) >R  SPLIT R> VC! VC!+ ;

: VWRITE ( RAM-addr VDP-addr cnt -- ) SWAP WMODE (TYPE) ; 

: VREAD  ( Vaddr Ram cnt --)
  ROT RMODE  0 DO  VC@+ OVER C!  LOOP DROP ;    
          
: VFILL  ( vaddr cnt char --)
    ROT WMODE  SWAP 0 DO  DUP VC!+ LOOP DROP ;

VARIABLE C/L   
\ different approach. Just calcuate the VDP address
: >VPOS ( col row -- vaddr) C/L @ * + ; 

VARIABLE COL 
VARIABLE ROW 
VARIABLE C/SCR  

: AT-XY  ( col row -- ) ROW ! COL ! ;

: TYPE  ( addr len -- ) \ no scroll, wraps to top of screen
    COL @ ROW @ >VPOS 
    C/SCR @ OVER < IF DROP 0 THEN WMODE (TYPE) ;

HERE SWAP - DECIMAL . 
