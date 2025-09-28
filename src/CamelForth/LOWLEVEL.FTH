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

HEADERS  ON  \ include dictionary
WARNINGS ON  \ prevents endless "<xxxx> is redefined"

CR .(  Compiler extensions & Virtual machine)
CHDIR DSK9.   \ camel99 source code disk
S" ITC-CODE"  INCLUDED   \ code word definer
S" EXECUTORS" INCLUDED   \ fundamental runtime code for Forth
S" ITC-TYPES" INCLUDED   \ CONSTANT VARIABLE : ; etc.
S" BRANCHING" INCLUDED   \ cross-compiler IF ELSE THEN BEGIN etc.
\ compiler extensions and forward references for (S") and TYPE
S" TSQUOTE"   INCLUDED

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
S" FIND"    INCLUDED
S" MISC"        INCLUDED  \ contains PAUSE EXECUTE
S" DOLOOP"      INCLUDED  \ cross-compiler DO/LOOP
S" VDP-API"     INCLUDED
S" ITC-COLON"   INCLUDED
S" TICKTOCK"    INCLUDED   \ 9901 hardware timer


\ Meta words look like Forth but compile to TARGET
 S" METADEFS"    INCLUDED
