\ MAKE CAMEL99 ITC Forth with FXCC cross-compiler  Nov 2024  Brian Fox

\    *** TIB AND STACK LOCATIONS ***
COMPILER
HEX
        FFFC    EQU EMEM     \ EMEM = "end of memory"
        0050    EQU TIBSIZE  \ 80 bytes is the longest file string
EMEM TIBSIZE -  EQU 'TIB     \ Terminal input buffer just below
     'TIB  2 -  EQU 'SP0     \ FORTH DATA stack base address
     'SP0 6E -  EQU 'RP0     \ FORTH RETURN stack base address

      8300      EQU WRKSP0   \ primary workspace in 16 bit RAM

\   *** TARGET MEMORY LOCATION ***

    A000 VALUE CODESEG
    2000 VALUE HEAPSEG

\ setup the build
    NEW
    CODESEG ORG

HEADERS  ON \ include dictionary
WARNINGS OFF  \ prevents endless "<xxxx> is redefined"

CHDIR DSK9.   \ camel99 source code disk
S" ITC-CODE"  INCLUDED   \ #1 code word definer
S" EXECUTORS" INCLUDED   \ fundamental runtime code for Forth
S" ITC-TYPES" INCLUDED   \ CONSTANT VARIABLE : ; etc.
S" BRANCHING" INCLUDED   \ cross-compiler IF ELSE THEN BEGIN etc.
\ compiler extensions and forward references for (S") and TYPE
S" TSQUOTE"   INCLUDED

\ build the target image ...
COMPILER HEX
\ ====================================
CHDIR DSK9.
CR .(  "CODE" words )
S" FETCHSTORE"  INCLUDED
S" RSTACK"      INCLUDED
S" DODOES"      INCLUDED
S" DATASTACK"   INCLUDED
S" LOGICMATH"   INCLUDED
S" MULTDIV"     INCLUDED
S" COMPARES"    INCLUDED
S" MOVEFILL"    INCLUDED
S" SKIPSCAN"    INCLUDED
S" FIND"        INCLUDED
S" MISC"        INCLUDED  \ contains PAUSE EXECUTE
\ ======================================

S" DOLOOP"      INCLUDED  \ cross-compiler DO/LOOP

\ TI-99  I/O drivers
S" VDP-API"     INCLUDED
\ =====================

\ Meta words look like Forth but compile to TARGET
\ S" METADEFS"    INCLUDED

\ add cross-compiler colon/semi-colon
S" ITC-COLON"   INCLUDED

\ *****************************************************************
\ >>> Compiler definitions must use host: ;HOST after this line <<<
\ *****************************************************************

\ 1st Forth "secondary" definitions
S" TICKTOCK"    INCLUDED   \ 9901 hardware timer

\ S" IMMEDIATE"    INCLUDED

\ Camel Forth System
COMPILER WARNINGS OFF
CHDIR DSK9.


S" HILEVEL"    INCLUDED  \ includes: CONSOLE DSRLINKB FILESYS

\ ======================================================================
\  P A T C H   T H E   T A R G E T  S Y S T E M   V A R I A B L E S
\                            *IMPORTANT*
\        Explicately select WORDLIST for variables and actions
\

COMPILER
TLAST  HOST @ DUP   COMPILER LATEST T! ORGLAST T!

COMPILER
THERE  ORGDP T!

PREAMBLE COLD
BOOT AUTOSTART WARM

END.   ( report compile time and stats)

SAVE DSK8.NEWCAMEL

HOST
CR ." === COMPILE ENDED PROPERLY ==="
