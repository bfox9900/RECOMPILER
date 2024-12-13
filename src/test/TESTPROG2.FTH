\ TESTPROG2.FTH  Demo IMPORT: CODE  loops and AUTOSTART  30Oct2024 Fox

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
INCLUDE DSK7.BRANCHING  \ teach cross-compiler IF ELSE THEN BEGIN etc.

\ create program entry word, COLD, which builds the Forth stacks
\ Also creates the target variable called BOOT 
INCLUDE DSK7.PREAMBLE 
\ ========================================================================

\ steal some code from the host Forth system 
IMPORT: 1- DUP DROP OR C! 0= 

COMPILER HEX 

TARGET
8800 CONSTANT VDPRD               \ vdp ram read data
8C00 CONSTANT VDPWD               \ vdp ram write data
8C02 CONSTANT VDPWA               \ vdp ram read/write address

CODE RMODE ( Vaddr --) \ set VDP read mode address 
    0 LIMI,
    TOS SWPB,  TOS VDPWA @@ MOVB, 
    TOS SWPB,  TOS VDPWA @@ MOVB,
    TOS POP, 
    NEXT, 
ENDCODE 

\ simpler to now use Forth for write mode 
: WMODE  4000 OR RMODE ;

\ need the word BYE to exit program.
\ BYE in the kernel does not end with NEXT, so we can't use IMPORT: 
CODE BYE   ( -- )
    83C4 @@ CLR,  
    0000 @@ BLWP, 
ENDCODE

\ give us a primitive screen output word
: EMIT+  ( c --) VDPWD C! ; \ write c & inc. VDP address

TARGET
: LOOP1 ( -- ) 4FFF BEGIN  1- DUP WHILE REPEAT DROP ; 
: LOOP2 ( -- ) 3FFF BEGIN  1- DUP   0= UNTIL   DROP ; 

COMPILER DECIMAL 
TARGET 
: MAIN  ( -- ) 
    0 WMODE         \ set VDP write address 
    65 EMIT+ LOOP1 
    66 EMIT+ LOOP2 
    67 EMIT+ LOOP2 
    68 EMIT+ LOOP2 
    BYE ;

COMPILER 
 PREAMBLE COLD        \ program start up code 
 BOOT AUTOSTART MAIN  \ set BOOT variable to MAIN 
END. 

SAVE DSK7.ITCTEST2

HOST 

