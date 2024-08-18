\ DSRLNKA.FTH for METACOMP cross-compiler for CAMEL99 Forth  Aug 2, 2021 Fox
\ Source:
\ posted by InsaneMultitasker via Thierry Nouspikel

\ ******************************************************************                            
\                     *** VERY IMPORTANT *** 
\ Feb 2024 save GPL workspace R15, set to 8C02 and restore after 
\          calling DSR. This allows ISR RS232 to work reliably
\ *******************************************************************


\ PASSES error code back to Forth workspace, TOS register
ONLY FORTH ALSO ASSEMBLER DEFINITIONS
HEX
\ MACRO to simplify the VDP code
HOST: VDPWA, ( reg -- )
       DUP           SWPB,   \ setup VDP address
       DUP VDPWA @@  MOVB,   \ write 1st byte of address to VDP chip
       DUP           SWPB,
           VDPWA @@  MOVB,   \ write 2nd byte of address to VDP chip
;HOST

HOST: [TOS]      8 (R13)  ;HOST  \ access to Forth top of stack register

L: HEX20   20 TC,
L: HEXAA   AA TC,
L: PERIOD  2E TC,      \ '.'
        TALIGN

L: H2000   2000 T,
L: CYC1    0000 T,
L: H1300   1300 T,
l: R15SAV  0000 T,    \ @insanemultitasker says we need to save R15 

\ use memory below Forth RETURN stack for workspace & name buffer
 RP0 80 -      EQU DREGS
 4 2* DREGS +  EQU DREG(4)  \ address of dsr wksp R4
 5 2* DREGS +  EQU DREG(5)  \ compute address of DREGS register 5
 DREGS 8 -     EQU NAMBUF   \ small buffer to parse the device name

\ === DSR ENTRY POINT ===
\ ** HEADLESS CODE TO SAVE SPACE **
NEWLABELS                   \ init labels. NEEDS resolver at end of CODE
L: DSR1                     \ label is in CC wordlist not in TARGET dictionary
      *R14+     R5  MOV,    \ fetch '8' from program ->R5, inc PC for return
       HEX20 @@ R15 SZCB,   \ >20 eq flag=0
       8356 @@  R0  MOV,    \ [PAB FNAME] to R0
       R0       R9  MOV,    \ dup R0 to R9
       R9       -8  AI,     \ R9-8 = [PAB FLG]
       R0          VDPWA,   \ set the VDP address to use
       VDPRD @@ R1  MOVB,   \ read length of FNAME -> R1

\ setup to copy VDP FNAME ->namebuf to '.' character
       R1       R3  MOVB,   \ DUP length byte to R3
       R3       08  SRL,    \ swap the byte to other side
       R2   NAMBUF  LI,     \ R2 is ^namebuf
       R4           SETO,   \ length counter, R4 = -1
       BEGIN,
         R0            INC,    \ point to next fname VDP address
         R4            INC,    \ counter starts at -1
         R4       R3   CMP,    \ is counter = fnamelength
         1 $           JEQ,    \ if true goto @@1:
         R0          VDPWA,    \ set VDP address
         VDPRD @@ R1  MOVB,    \ read next VDP char from fname
         R1      *R2+ MOVB,    \ copy to namebuf & inc pointer
         R1 PERIOD @@ CMPB,    \ is it a '.'
       EQ UNTIL,               \ until '.' found  34 bytes!!!

1 $:   R4        R4  MOV,    \ test R4(device name length)=0
                6 $  JEQ,    \ if so, goto ERROR6
       R4       07   CI,     \ is dev name length>7
                6 $  JGT,    \ if so, goto 6$ (ERROR6)
\ -------- ENTRY POINT SHOULD BE HERE -------------
       83D0 @@       CLR,    \ erase magic CRU addr. holder
       R4   8354 @@  MOV,    \ put length in magic address
       R4            INC,    \ +1 points to '.' character
       R4   8356 @@  ADD,    \ add offset to PAB address (makes "real PAB")

\ ==== GPL WORKSPACE ====
       83E0         LWPI,    \ SROM (search ROM device list)

\ ** new ** dsr needs 8C02 in R15 
                  0 LIMI,  
       R15  R15SAV @@ MOV,  \ save R15 
       R15  8C02    LI, 

       R1           CLR,     \ MAGIC GPL REG. 1 to call DSR, returns error
       R2   4000    LI,    \ ROM start addr -> R2
       H2000 @@ CYC1 @@ MOV, \ init the CYC1 variable ??
       R12     0F00 LI,      \ init CRU base to 0F00
       0A $ JMP,

9 $:    \ scan for I/O cards
       R12   1000   LI,      \ init CRU address
       H1300 @@ CYC1 @@ MOV,
      BEGIN,
0A $:    R12   R12   MOV,
         NE IF,              \ if card address<>0
              00 SBZ,        \ turn off card
         ENDIF,
         R12    0100  AI,  \ advance CRU to next card
         83D0 @@      CLR,   \ erase magic addres
         R12    2000  CI,
         9 $         JEQ,   \ Scan ROM
         R12  CYC1 @@ CMP,
         5 $         JEQ,   \ no more cards. goto ERROR5
\ card activation...
         R12  83D0 @@ MOV,   \ save card CRU in magic address
         00           SBO,   \ turn on the card
        *R2  HEXAA @@ CMPB,  \ test for card present
       EQ UNTIL,             \ loop until card is found
       DREG(5) @@ R2 ADD,    \ add '8'+4000= >4008 DSR ROM list
       0B $          JMP,

3 $:  \ scan ROM linked list for code address
      BEGIN,
         BEGIN,
           83D2 @@   R2 MOV,   \ start of ROM device list -> R2
           00           SBO,   \ turn card on
0B $:      *R2       R2  MOV,   \ Fetch next link
           0A $          JEQ,   \ if link=0 goto @@A (NEXT CARD)
           R2  83D2 @@  MOV,   \ save link address in magic address
           R2           INCT,  \ R2 = code pointer
          *R2+      R9  MOV,   \ fetch code address ->R9
           8355 @@  R5  MOVB,  \ dev length->R5
           4 $          JEQ,   \ if 0 we have a string match
           R5      *R2+ CMPB,
         EQ UNTIL,

         \ find dev string match
         R5       08  SRL,     \ shift length byte
         R6   NAMBUF  LI,      \ R6 hold ^nambuf
         BEGIN,
           *R6+   *R2+ CMPB,   \ compare namebuf to ROM string
            3 $        JNE,    \ if mismatch goto @@3
            R5         DEC,    \ dec the counter register
         EQ UNTIL,
4 $:    \ run DSR code
         R1        INC,        \ count entries into the DSR ?
        *R9         BL,        \ call the DSR code
      AGAIN,                   \ try next card

\   -- DSR returns here if we are done --
       00            SBZ,  \ Turn off the card
       R15SAV @@ R15 MOV,  \ restore saved R15  Version B. 2024
       DREGS         LWPI, \ ==== DSR Workspace ====
       R9           VDPWA, \ set vdp address
       VDPRD @@  R1  MOVB, \ read error value to DREGS R1
       R1 0D         SRL,  \ shift error to correct range
       7 $           JNE,  \ if error<>0 goto @@7
                     RTWP, \ else return to Forth workspace

\ error condition handlers
5 $:   DREGS         LWPI, \ we came from GPL workspace, restore DREGS

\ device name length errors
6 $:  R1            SETO, \  error code in R1. *THIS SEEMS TO MATTER*

\ device not found error
7 $:  R1      [TOS] MOV,  \ Move error code to Forth TOS

\ GPL error test
      GPLSTAT @@  R0 MOVB, \ get gpl status byte
                 R0 SWPB,
      R0       0020 ANDI,  \ mask to get GPL error bit
      R0      [TOS] SOC,   \ "OR" GPL & DSR error codes
      HEX20 @@ R15  SOCB,  \ set Forth's workspace 'EQ' flag to 1
                    RTWP,  \ return to Forth
      RESOLVER  \ resolve jumps cuz we didn't use CODE/ENDCODE

\    ====== DSR LINK ENDS======
\ /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

\ create the vector for BLWP in target memory
L: DLNK      DREGS T,  DSR1  T,

\ words that appear in the dictionary
TARGET-COMPILER
CODE DSRLNK  ( [pab_fname] -- ior)
      TOS  8356 @@ MOV,
               TOS CLR,
                 0 LIMI,    \ disable interrupts here
     TOS GPLSTAT @@ MOVB,   \ clear GPL status register
           DLNK @@ BLWP,
                   8 T,     \ Offset to DSR linked list in card ROM
                 2 LIMI,
                   NEXT,
ENDCODE
