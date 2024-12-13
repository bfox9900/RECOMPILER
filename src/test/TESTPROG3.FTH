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
INCLUDE DSK7.BRANCHING  \ teach cross-compiler IF ELSE THEN BEGIN etc.

\ create program entry word, COLD, which builds the Forth stacks
\ Also creates the target variable called BOOT 
INCLUDE DSK7.PREAMBLE 

\ ========================================================================
INCLUDE DSK7.FORNEXT 

IMPORT: OR C! ?TERMINAL 

COMPILER HEX 

TARGET
8800 CONSTANT VDPRD               \ vdp ram read data
8C00 CONSTANT VDPWD               \ vdp ram write data
8C02 CONSTANT VDPWA               \ vdp ram read/write address

CODE RMODE 
    R1 STWP,               \ avoids 2 SWPB instructions
    0 LIMI,
    9 (R1) 8C02 @@ MOVB,   \ write odd byte from TOS (ie: R4)
    TOS    8C02 @@ MOV,    \ MOV writes the even byte to the port
    NEXT,
ENDCODE     

CODE BYE   ( -- )
    83C4 @@ CLR,  
    0000 @@ BLWP, 
ENDCODE

\ simpler to now use Forth for write mode 
: WMODE  4000 OR RMODE ;

: EMIT+  ( c --) VDPWD C! ; \ write C and inc. address

54 CONSTANT 'T'
46 CONSTANT 'F'

: .TRUE     10 FOR 'T' EMIT+  NEXT ;
: .FALSE    10 FOR 'F' EMIT+  NEXT ;

: TESTFLAG  IF  .TRUE   ELSE .FALSE  THEN ;

: MAIN  
    0 WMODE        \ Set to line 0 col 0 
    FFFF TESTFLAG 

    40 WMODE       \ set VDP address to line 2 col 0 
    0 TESTFLAG

    FFFF FOR  
         ?TERMINAL IF BYE THEN  
    NEXT 
    BYE 
;

COMPILER 
PREAMBLE COLD 
BOOT AUTOSTART MAIN 
END. 

SAVE DSK7.ITCTEST3

HOST 
