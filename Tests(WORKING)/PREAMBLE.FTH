\ preamble.fth IS a minimal set of includes we need to make a simple program.

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
