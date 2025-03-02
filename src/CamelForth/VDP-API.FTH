COMPILER CR .( VDP CHIP API)
HEX 

COMPILER 
8800 EQU VDPRD               \ vdp ram read data port 
8C00 EQU VDPWD               \ vdp ram write data port 
8C02 EQU VDPWA               \ vdp ram read/write address port 

\ VDP set-address sub-routines

\ NOTE: Workspace held in R1 for other VDP routines to access user variables 
\ == LEAF SUB-ROUTINE  *NOT NESTABLE*
TARGET
L: WMODE  ( Vaddr -- Vaddr)
         TOS 4000 ORI,
L: RMODE R1 STWP,               \ avoids 2 SWPB instructions
         0 LIMI,
         9 (R1) 8C02 @@ MOVB,   \ write odd byte from TOS (ie: R4)
         TOS 8C02 @@ MOV,       \ MOV writes the even byte to the port
         RT,  \ 22 BYTES 

\ ================== FORTH WORDS ======================
\ Register Usage:
\ R0  down counter
\ R1  current workspace
\ R3  VDP data port
\ R4  VDP memory address
CODE VC@ ( VDP-adr -- char )   \ Video CHAR fetch
         RMODE @@ BL,
         VDPRD @@ TOS MOVB,
         TOS 8 SRL, 
         2 LIMI,
         NEXT,
         ENDCODE \ 16 BYTES

CODE V@  ( VDPadr -- n)        \ video fetch 16bit int
         RMODE @@ BL,
         R3 VDPRD LI,          \ VDP port address in R3 faster reads
         *R3  TOS MOVB,        \ READ char from VDP RAM into TOS
         *R3  9 (R1) MOVB,     \ READ 2nd char from VDP RAM into TOS
         2 LIMI,
         NEXT,
         ENDCODE

CODE VREAD ( VDP-adr RAM-addr cnt  -- )
         TOS R0 MOV,        \ cnt->R0
         R2 POP,            \ pop ram addr. to R2
         TOS POP,           \ VDP address to TOS
         RMODE @@ BL,
         R3 VDPRD LI,       \ put VDP read port address in R3. 12% faster
         BEGIN,
            R0 DEC,
         OC WHILE,                 
            *R3 *R2+ MOVB,  \ READ from VDP RAM into CPU RAM , autoINC
         REPEAT,
         2 LIMI,
         TOS POP,           \ refill TOS
         NEXT, 
         ENDCODE

\ ===================================================================
COMPILER CR .( WRITE VC! V!) 

TARGET
CODE VWRITE  ( RAM-addr VDP-addr cnt -- ) 
         TOS R0 MOV,         \ cnt in R0
         TOS POP,            \ vaddr to TOS
         R2 POP,             \ cpu addr to R2
         WMODE @@ BL,
         R3 VDPWD LI,        \ vdp addr. in a reg. makes this 12.9% faster
         BEGIN,
              R0 DEC,
         OC WHILE,      
             *R2+ *R3 MOVB, \ write byte to vdp write port
         REPEAT,
         2 LIMI,
         TOS POP,            \ refill top of stack cache register
         NEXT,               \ return to Forth
         ENDCODE

CODE VC! ( char vaddr --)
         WMODE @@ BL,
         TOS POP,
         9 (R1) VDPWD @@ MOVB,  \ Odd byte R4, write to screen
         2 LIMI,
         TOS POP,               \ refill TOS
         NEXT,
         ENDCODE

CODE V! ( n Vaddr  -- )       \ Video store 16BIT int
         WMODE @@ BL,         \ branch to setup VDP write address
         TOS POP,
         R3 VDPWD LI,         \ VDP port in R3
         TOS    *R3 MOVB,     \ even byte
         9 (R1) *R3 MOVB,     \ Odd byte R4 to VDP port 
         2 LIMI,
         TOS POP,             \ refill TOS
         NEXT,
         ENDCODE

CODE VFILL ( VDP-addr count char-- )
         TOS SWPB,             \ fix the TMS9900 byte order
         TOS W MOV,
         R0 POP,               \ R0=count
         TOS POP,              \ VDP-addr popped into TOS
         WMODE @@ BL,          \ setup VDP write address in TOS register
         R3 VDPWD LI,          \ vdp addr. in a reg. makes this 12.9% faster
         BEGIN,
            W *R3 MOVB,        \ write byte to vdp ram
            R0 DEC,            \ dec the byte counter
         EQ UNTIL,             \ jump back if not done
         2 LIMI,
         TOS POP,
         NEXT,
         ENDCODE

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
