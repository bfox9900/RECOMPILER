COMPILER 
CR .( TMS9918 driver in Forth)

HEX 

TARGET
8800 CONSTANT VDPRD               \ vdp ram read data
8802 CONSTANT VDPSTS              \ vdp status
8C00 CONSTANT VDPWD               \ vdp ram write data
8C02 CONSTANT VDPWA               \ vdp ram read/write address

\ VDP set-address sub-routines
CODE 0LIMI   0 LIMI,   NEXT,  ENDCODE 

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
: >VPOS ( col row -- vaddr) C/L @ * + ; 

VARIABLE COL   VARIABLE ROW 
: AT-XY  ( col row -- ) ROW ! COL ! ;

VARIABLE C/SCR  
: TYPE  ( addr len -- ) \ no scroll, wraps to top of screen
    COL @ ROW @ >VPOS OVER +  \ max VDP address this string could hit
    C/SCR @ OVER < IF DROP 0 THEN WMODE (TYPE) ;
