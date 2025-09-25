\ TESTPROG4.FTH  for 

COMPILER 
NEW 

HEX A000 ORG 

\ ==========================[ prequisite files ]==========================
\ target code starts here
\ DSK7.EXECUTORS builds: EXIT NEXT DOCOL DOVAR DOCON LIT ?BRANCH BRANCH (NEXT)
INCLUDE DSK7.EXECUTORS  

\ IMPORT:  needs the word DOVAR and so must be loaded after EXECUTORS 
INCLUDE DSK7.IMPORT     \ teach cross-compiler how "steal" host code words
INCLUDE DSK7.ITCTYPES   \ teach compiler CONSTANT VARIABLE : ; etc. 
INCLUDE DSK7.BRANCHING  \ teach cross-compiler IF ELSE THEN BEGIN FOR NEXT 
\ ========================================================================

\ create program entry word, COLD, which builds the Forth stacks
\ Also creates the target variable called BOOT 

\ over-ride the default boot values BEFORE including preamble 
COMPILER HEX 
 8400 TO DSTACK       \ dstack in 16 bit RAM 
 83BE TO RSTACK       \ rstack in 16 bit RAM 
\ 8300 TO WKSPACE     \ default is 8300 
INCLUDE DSK7.PREAMBLE 

INCLUDE DSK7.FORNEXT 

IMPORT: C@ C! 
IMPORT: SWAP DROP COUNT DUP ROT OVER 1+ PICK 
IMPORT: OR ?TERMINAL 

COMPILER HEX 

TARGET
8800 CONSTANT VDPRD               \ vdp ram read data
8C00 CONSTANT VDPWD               \ vdp ram write data
8C02 CONSTANT VDPWA               \ vdp ram read/write address

CODE BYE   ( -- )
    0 LIMI, 
    83C4 @@ CLR,  
    0000 @@ BLWP, 
ENDCODE

\ VDP interface in Forth 
TARGET 
\ faster & smaller to make VDP address setter in code 
CODE WMODE ( Vaddr --)
    TOS 4000 ORI, 
    1 $ JMP, 
+CODE RMODE ( Vaddr --)
1 $: R1 STWP,               \ avoids 2 SWPB instructions
    0 LIMI,
    9 (R1) 8C02 @@ MOVB,   \ write odd byte from TOS (ie: R4)
    TOS    8C02 @@ MOV,    \ MOV writes the even byte to the port
    TOS POP, 
    NEXT,
ENDCODE     

: VC@+   ( addr -- c) VDPRD C@ ;
: VC@    ( Vaddr -- char ) RMODE VC@+ ;

: EMIT+  ( c --) VDPWD C! ; \ write C and inc. address
: VC!    ( c vaddr --) WMODE EMIT+ ; \ set address and write

COMPILER HEX 
TARGET 
: VWRITE ( addr Vaddr cnt -- ) 
    SWAP WMODE 
    FOR  
        COUNT EMIT+        
    NEXT
    DROP ;

: VREAD  ( Vaddr Ram cnt --)
    ROT RMODE      
    FOR  
      VC@+ OVER C! 1+  
    NEXT 
    DROP ;

: VFILL  ( Vaddr cnt char --)  
    ROT WMODE SWAP 
    FOR  
        DUP EMIT+        
    NEXT 
    DROP ;

COMPILER DECIMAL 
TARGET 
32 CONSTANT BL 
: PAGE       0  H# 300 BL VFILL ;

CHAR A CONSTANT 'A'

TARGET 
: MAIN  
    'A' 
    25 FOR 
        0 768 2 PICK VFILL 1+ 
    NEXT 
    BYE 
;

\ =====[ finalize image ]=====
COMPILER 
PREAMBLE COLD 
BOOT AUTOSTART MAIN 
END. 

SAVE DSK7.ITCTEST4c

HOST 

