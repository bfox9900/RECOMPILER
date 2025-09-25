\ TESTPROG5.FTH

COMPILER
NEW

HEX A000 ORG

\ ===============[ THIS WILL BE PUT INTO A SINGLE FILE ]==================
\ target code starts here
\ build:;  EXIT NEXT DOCOL DOVAR DOCON LIT ?BRANCH BRANCH (NEXT)
COMPILER WARNINGS OFF
INCLUDE DSK7.EXECUTORS

\ IMPORT:  needs the word DOVAR and so must be loaded after EXECUTORS
INCLUDE DSK7.IMPORT     \ teach cross-compiler how "steal" host code words
INCLUDE DSK7.ITCTYPES   \ teach compiler CONSTANT VARIABLE : ; etc.
INCLUDE DSK7.BRANCHING  \ teach cross-compiler IF ELSE THEN BEGIN FOR NEXT
COMPILER WARNINGS ON

\ create program entry word, COLD, which builds the Forth stacks
\ Also creates the target variable called BOOT
\ we can change the default stack location and workspace here
COMPILER
HEX
8300 TO WKSPACE
837E TO RSTACK
8400 TO DSTACK
INCLUDE DSK7.PREAMBLE

\ ========================[ PROGRAM BEGINS ]=============================
COMPILER HEX

TARGET
CODE BYE   ( -- )
    0 LIMI,
    83C4 @@ CLR,
    0000 @@ BLWP,
ENDCODE

IMPORT: OR C! ?TERMINAL

COMPILER HEX

TARGET
8800 CONSTANT VDPRD               \ vdp ram read data port
8C00 CONSTANT VDPWD               \ vdp ram write data port
8C02 CONSTANT VDPWA               \ vdp ram read/write address port

IMPORT: C@ C! !
IMPORT: >R R> DUP

\ VDP interface in Forth
TARGET
CODE 0LIMI   0 LIMI,  NEXT, ENDCODE

: RMODE ( Vaddr --) 0LIMI  DUP VDPWA C! VDPWA ! ;

: VC@+  ( addr -- c) VDPRD C@ ;
: VC@   ( Vaddr -- char ) RMODE VC@+ ;

: WMODE  4000 OR RMODE ;
: EMIT+  ( c --) VDPWD C! ; \ write C and inc. address
: VC!    ( c vaddr --) WMODE EMIT+ ; \ set address and write

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

COMPILER DECIMAL
TARGET
     32 CONSTANT BL
CHAR  A CONSTANT 'A'

: PAGE     0 768 BL VFILL ;

IMPORT:  ?TERMINAL PICK 1+

INCLUDE DSK7.FORNEXT

TARGET
: MAIN
    BEGIN
      'A'
      25 FOR
          0 768 2 PICK VFILL
          1+  ( inc. character )
      NEXT
      DROP
      ?TERMINAL
    UNTIL
    BYE
;

COMPILER
PREAMBLE COLD
BOOT AUTOSTART MAIN
END.

SAVE DSK7.ITCTEST5

HOST
