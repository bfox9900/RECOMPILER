\ TESTPROG3.FTH  test if else then 

\ You see T on 1st line and F on 3rd line.


COMPILER 
NEW 

HEX A000 ORG 

\ ===============[ THIS WILL BE PUT INTO A SINGLE FILE ]==================
\ target code starts here
\ build:;  EXIT NEXT DOCOL DOVAR DOCON LIT ?BRANCH BRANCH (NEXT)
INCLUDE DSK7.EXECUTORS  

\ IMPORT:  needs the word DOVAR and so must be loaded after EXECUTORS 
INCLUDE DSK7.IMPORT     \ teach cross-compiler how "steal" host code words
INCLUDE DSK7.ITCTYPES   \ teach compiler CONSTANT VARIABLE : ; etc. 
INCLUDE DSK7.BRANCHING  \ teach cross-compiler IF ELSE THEN BEGIN FOR NEXT 

\ create program entry word, COLD, which builds the Forth stacks
\ Also creates the target variable called BOOT 
INCLUDE DSK7.PREAMBLE 

\ ========================================================================

IMPORT: OR C! ?TERMINAL 

COMPILER HEX 

TARGET
8800 CONSTANT VDPRD               \ vdp ram read data
8C00 CONSTANT VDPWD               \ vdp ram write data
8C02 CONSTANT VDPWA               \ vdp ram read/write address

CODE RMODE ( Vaddr --)
    R1 STWP,               \ avoids 2 SWPB instructions
    0 LIMI,
    9 (R1) 8C02 @@ MOVB,   \ write odd byte from TOS (ie: R4)
    TOS    8C02 @@ MOV,    \ MOV writes the even byte to the port
    TOS POP, 
    NEXT,
ENDCODE     

CODE BYE   ( -- )
    0 LIMI, 
    83C4 @@ CLR,  
    0000 @@ BLWP, 
ENDCODE

\ simpler to now use Forth for write mode 
: WMODE  4000 OR RMODE ;

IMPORT: C@ C! 
TARGET 
\ : VC@+  ( Vdpaddr -- c) VDPRD C@ ; \ read & inc. address
\ : VC!+   ( c --) VDPWD C! ; \ write & inc. address
\ code versions ~2X faster 


: EMIT+  ( c --) VDPWD C! ; \ write C and inc. address
: VC@+  ( addr -- c) VDPRD C@ ;

: VC!   ( c vaddr --) WMODE EMIT+ ; \ set address and write
: VC@   ( Vaddr -- char ) RMODE VC@+ ;

\ VDP integer fetch & store
IMPORT: FUSE SPLIT >R R> 

TARGET 

IMPORT: SWAP DROP COUNT DUP ROT OVER 1+ C@ C! 

INCLUDE DSK7.FORNEXT 
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


20 CONSTANT BL 
: PAGE       0  H# 300 BL VFILL ;

CHAR T CONSTANT 'T'
CHAR F CONSTANT 'F'

\ IMPORT: FILL 

TARGET 
: MAIN  
    BEGIN 
      PAGE 
      0 H# 300  'T' VFILL 
      0 H# 2000 H# 300 VREAD

      PAGE 
      0 H# 300  'F' VFILL
      0 H# 2000 H# 300 VREAD
 
      ?TERMINAL 
    UNTIL 
    BYE 
;

COMPILER 
PREAMBLE COLD 
BOOT AUTOSTART MAIN 
END. 

SAVE DSK7.ITCTEST4

HOST 

