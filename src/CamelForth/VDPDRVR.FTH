\ VDPDRVR.FTH    tms9918 video chip API

COMPILER HEX

IMPORT: OR C@ C! SPLIT FUSE >R R> SWAP ROT

TARGET
8800 CONSTANT VDPRD               \ vdp ram read data
8802 CONSTANT VDPSTS              \ vdp status
8C00 CONSTANT VDPWD               \ vdp ram write data
8C02 CONSTANT VDPWA               \ vdp ram read/write address

\ VDP set-address sub-routines
CODE RMODE
    R1 STWP,               \ avoids 2 SWPB instructions
    0 LIMI,
    9 (R1) 8C02 @@ MOVB,   \ write odd byte from TOS (ie: R4)
    TOS 8C02 @@ MOV,       \ MOV writes the even byte to the port
    NEXT,
ENDCODE

: WMODE ( vdpaddr -- ) 4000 OR RMODE ;

\  VC@+  ( Vdpaddr -- c) VDPRD C@ ; \ read & inc. address
\ : VC!+  ( c --) VDPWD C! ; \ write & inc. address

: VC@   ( VDP-adr -- char ) RMODE VDPRD C@ ;
: VC!   ( c vaddr --) WMODE VDPWD C! ; \ set address and write

\ VDP integer fetch & store
: V@    ( VDPadr -- n) VC@  VDPRD C@  FUSE  ;
: V!    ( n vaddr --) >R  SPLIT R> VC! VDPWD C! ;

[CC] CR ." VWRITE VREAD VFILL"
TARGET
CODE (VMBW)  ( RAM-addr cnt -- ) \ must be used with WMODE
        TOS R0 MOV,         \ cnt in R0
        TOS POP,            \ cpu addr to TOS
        R3 VDPWD LI,        \ vdp addr. in a reg. makes this 12.9% faster
        BEGIN,
             R0 DEC,
        OC WHILE,
            *TOS+ *R3 MOVB, \ write byte to vdp write port
        REPEAT,
        2 LIMI,
        TOS POP,            \ refill top of stack cache register
        NEXT,               \ return to Forth
ENDCODE

: VWRITE ( RAM-addr VDP-addr cnt -- ) SWAP WMODE (VMBW) ;

CODE (VMBR) ( RAM-addr cnt  -- ) \ MUST be used with RMODE
        TOS R0 MOV,        \ cnt->R0
        TOS POP,           \ RAM address to TOS
        R3 VDPRD LI,       \ put VDP read port address in R3. 12% faster
        BEGIN,
           R0 DEC,
        OC WHILE,
           *R3 *TOS+ MOVB,  \ READ from VDP RAM into CPU RAM , autoINC
        REPEAT,
        2 LIMI,
        TOS POP,           \ refill TOS
        NEXT,
ENDCODE

: VREAD ( VDP-adr RAM-addr cnt  -- ) ROT RMODE (VMBR) ;

CODE (VFILL) ( count char-- )
        TOS W MOV,
            W SWPB,
          TOS POP,            \ TOS:=count
        R3 VDPWD LI,          \ vdp addr. in a reg. makes this 12.9% faster
        BEGIN,
           W *R3 MOVB,        \ write byte to vdp ram
           TOS DEC,           \ dec the byte counter
        EQ UNTIL,             \ jump back if not done
        2 LIMI,
        TOS POP,
        NEXT,
ENDCODE

: VFILL ( Vaddr count char  --) ROT WMODE (VFILL) ;

\ * VDP write to register. Kept the TI name
CODE VWTR   ( c reg -- )        \ Usage: 5 7 VWTR
        TOS SWPB,              \ register goes to left side
        *SP+ TOS ADD,
        0 LIMI,
        R1 STWP,
        9 (R1) 8C02 @@ MOVB,   \ write odd byte from R4
        TOS 8000 ORI,          \ set bit for write to VDP register
        TOS 8C02 @@ MOVB,      \ write the even byte
        2 LIMI,
        TOS POP,
        NEXT,
ENDCODE
