\ T I - 9 9   S C R E E N   C O N T R O L  P R I M I T I V E S
\ For RECOMPILER 1.0  2023 


COMPILER CR .( VDP CHIP INTERFACE)
HEX 
8800 CONSTANT VDPRD               \ vdp ram read data
8802 CONSTANT VDPSTS              \ vdp status
8C00 CONSTANT VDPWD               \ vdp ram write data
8C02 CONSTANT VDPWA               \ vdp ram read/write address


\ Register Usage:
\ R0  down counter
\ R1  current workspace
\ R3  VDP data port
\ R4  VDP memory address

\
\ ================ LEAF SUB-ROUTINE  ** NOT NESTABLE ** ===================
TARGET
\ VDP set-address sub-routines
L: WMODE  ( Vaddr -- Vaddr)
        TOS 4000 ORI,
L: RMODE R1 STWP,               \ avoids 2 SWPB instructions
        0 LIMI,
        9 (R1) 8C02 @@ MOVB,   \ write odd byte from TOS (ie: R4)
        TOS    8C02 @@ MOV,    \ MOV writes the even byte to the port
        RT,


CODE VC@ ( VDP-adr -- char )   \ Video CHAR fetch
        RMODE @@ BL,
+CODE VC@+ ( -- char )   \ Video CHAR fetch, inc. VDP address 
        VDPRD @@ TOS MOVB, \ READ char from VDP RAM into TOS 
        TOS 8 SRA, 
        2 LIMI,
        NEXT,
ENDCODE

CODE V@  ( VDPadr -- n)        \ video fetch 16bit int
        RMODE @@ BL,
        R3 VDPRD LI,         \ VDP port address in R3 faster reads
        *R3  TOS MOVB,        \ READ char from VDP RAM into TOS
        *R3  9 (R1) MOVB,     \ READ 2nd char from VDP RAM into TOS
        2 LIMI,
        NEXT,
 ENDCODE

CODE VREAD   ( VDP-adr RAM-addr cnt  -- )
         TOS R0 MOV,           \ cnt->R0
         R2 POP,               \ ram addr. to R2
         TOS POP,              \ VDP address to TOS
         R0 R0 MOV,            \ test cnt<>0
         NE IF,
            RMODE @@ BL,
            R3 VDPRD LI,      \ put VDP read port address in R3. 12% faster
            BEGIN,
               *R3 *R2+ MOVB,  
                R0 DEC,
            EQ UNTIL,
            2 LIMI,
         ENDIF,
         TOS POP,  
         NEXT,
ENDCODE \ 28 bytes

\ ===================================================================
COMPILER CR .( VDP  WRITE  {VMBW}) 

TARGET
CODE VWRITE  ( RAM-addr VDP-addr cnt -- )
         TOS R0 MOV,         \ cnt in R0
         TOS POP,            \ vaddr to TOS
         R2 POP,             \ cpu addr to R2
         R0 R0 MOV,
         NE IF,
            WMODE @@ BL,
            R3 VDPWD LI,     \ vdp addr. in a reg. makes this 12.9% faster
            BEGIN,
             *R2+ *R3 MOVB, \ write byte to vdp write port
              R0 DEC,
            EQ UNTIL,
            2 LIMI,
         ENDIF,
         TOS POP,            \ refill top of stack cache register
         NEXT,               \ return to Forth
         ENDCODE \ 28 BYTES

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
         R3 VDPWD LI,
         TOS POP,
         TOS  *R3 MOVB,       \ even byte
         9 (R1) *R3 MOVB,     \ Odd byte R4, write to screen
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
            W *R3 MOVB,     \ write byte to vdp ram
            R0 DEC,         \ dec the byte counter
         EQ UNTIL,             \ jump back if not done
         2 LIMI,
         TOS POP,
         NEXT,
         ENDCODE


\ For reference, these are the offset values of the user vars.
\      2E USER: C/L
\      30 USER: OUT
\      32 USER: VROW
\      34 USER: VCOL

\       7E USER VPG       \ Each task can have it's own video page 


CODE >VPOS ( col row -- vaddr) \ compute a VDP screen address
         R1         STWP,
         TOS     R3  MOV,   \ this move to make best use of MPY
         2E (R1) R3  MPY,   \ multiply by chars/line. result goes to R4 ie: TOS
        *SP+     TOS ADD,   \ add col value to TOS
         7E (R1) TOS ADD,   \ add Video page offset
         NEXT,
         ENDCODE

CODE CPUT ( char --  n) \ write to screen, return VCOL value 
         TOS     R2  MOV,
                 R1  STWP,    \ workspace is USER area base address
         32 (R1) R3  MOV,     \ vrow->r3
         2E (R1) R3  MPY,     \ vrow*c/l->R4
         34 (R1) TOS ADD,     \ add vcol
         7E (R1) TOS ADD,     \ add video page# VPG. tos=vdp cursor address 
            WMODE @@ BL,      
         5 (R1) VDPWD @@ MOVB,  \ !! Odd byte of R2, write to screen
         34 (R1)  INC,          \ bump VCOL
         34 (R1)  2E (R1) CMP,  \ compare VCOL = C/L
         TOS CLR, 
         EQ IF,
            TOS SETO,           \ set true flag
         ENDIF,
         2 LIMI,
         NEXT,
         ENDCODE

 CODE (CR) ( -- n)   \ inc VROW, return VROW value
         R1 STWP,
         34 (R1) CLR,     \ VCOL OFF
         32 (R1) INC,     \ vrow 1+! 
         TOS PUSH,
         32 (R1) TOS MOV, \ vrow @ 
         NEXT,
         ENDCODE

\ * VDP write to register. Kept the TI name
CODE VWTR   ( c reg -- )          \ Usage: 5 7 VWTR
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

CODE VDPADDR  ( Vaddr -- ) \ set vdp read address
         RMODE @@ BL,
         TOS POP,
         NEXT,
         ENDCODE

