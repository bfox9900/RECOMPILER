\ NESTTEST.FTH  test nested calls on METAFORTH compiler  Sept 2023 Fox 

HEX 2000 ORG 
INCLUDE DSK7.ITC-FORTH

TARGET ALSO FORTH 
IMPORT: DUP DROP 1- 

COMPILER 
HEX 
8800 CONSTANT VDPRD               \ vdp ram read data
8802 CONSTANT VDPSTS              \ vdp status
8C00 CONSTANT VDPWD               \ vdp ram write data
8C02 CONSTANT VDPWA               \ vdp ram read/write address

TARGET
CODE BYE    0 LIMI, 0 @@ BLWP, ENDCODE 

\ VDP DRIVER in Forth, slow but small 
CODE 0LIMI   0 LIMI,   NEXT,  ENDCODE
: RMODE ( vdpaddr -- ) DUP 0LIMI VDPWA C! >< VDPWA C! ;
: WMODE ( vdpaddr -- ) 4000 OR RMODE ;

: VC@+  ( Vdpaddr -- c) VDPRD C@ ; \ read & inc. address
: VC!+  ( c Vdpaddr --) VDPWD C! ; \ write & inc. address

: VC@   ( VDP-adr -- char ) RMODE VDPRD C@ ;
: VC!   ( c vaddr --) WMODE VC!+ ; \ set address and write

\ VDP integer fetch & store
: V@    ( VDPadr -- n) VC@  VC@+  FUSE  ;
: V!    ( n vaddr --) >R SPLIT R> VC! VC!+ ;

\ nested sub-routines 
 : SUB3  BEEF ;
 : SUB2  SUB3 ;
 : SUB1  SUB2 ;

: MAIN  
    1000 
    BEGIN  
      1- DUP 
    WHILE   
      DUP 0 VC!  ( show screen activity)
      SUB1 DROP 
    REPEAT
    DROP 
    BYE 
;
\ PROGRAM ENDS 

COMPILER 
T' MAIN  BOOT T!   \ set the boot variable 
SAVE DSK7.NESTTEST

HOST 
CR .( compile complete)
CR .( Press key to exit compiler)
KEY DROP BYE 
