TARGET

8800 CONSTANT VDPRD               \ vdp ram read data
\ 8802 CONSTANT VDPSTS              \ vdp status
8C00 CONSTANT VDPWD               \ vdp ram write data
8C02 CONSTANT VDPWA               \ vdp ram read/write address

\ VDP set-address sub-routines
CODE 0LIMI   0 LIMI,   NEXT,  ENDCODE

: RMODE ( vdpaddr -- ) DUP 0LIMI VDPWA C! >< VDPWA C! ;
: WMODE ( vdpaddr -- ) 4000 OR RMODE ;

: VC@+  ( Vdpaddr -- c) VDPRD C@ ; \ read & inc. address
: VC!+  ( c --) VDPWD C! ; \ write & inc. address

: VC@   ( VDP-adr -- char ) RMODE VDPRD C@ ;
: VC!   ( c vaddr --) WMODE VC!+ ; \ set address and write

\ VDP integer fetch & store
: V@    ( VDPadr -- n) VC@  VC@+  FUSE  ;
: V!    ( n vaddr --) >R  SPLIT R> VC! VC!+ ;

: VWRITE ( RAM-addr VDP-addr cnt -- )
        SWAP WMODE
        >R
        BEGIN
          COUNT VC!+
          R> 1- DUP>R
        -UNTIL
        R> 2DROP ;


: VREAD  ( Vaddr Ram cnt --)
         >R
         SWAP RMODE
         BEGIN
           VC@+ OVER C!
           R> 1- DUP>R
         -UNTIL
         R> 2DROP ;

: VFILL  ( vaddr cnt char --)
         SWAP >R
         SWAP WMODE
         BEGIN
           DUP VC!+
           R> 1- DUP>R
         -UNTIL
         R> 2DROP ;