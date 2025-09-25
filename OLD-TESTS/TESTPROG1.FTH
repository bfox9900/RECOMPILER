\ TESTPROG1.FTH   minimal program for FXCC CROSS-COMPILER Ict 2024 Fox
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

HEADERS  OFF \ include dictionary
WARNINGS ON  \ prevents endless "<xxxx> is redefined"

CHDIR DSK9.   \ camel99 source code disk
S" ITC-CODE"  INCLUDED   \ #1 code word definer
S" EXECUTORS" INCLUDED   \ fundamental runtime code for Forth
S" ITC-TYPES" INCLUDED   \ CONSTANT VARIABLE : ; etc.
S" BRANCHING" INCLUDED   \ cross-compiler IF ELSE THEN BEGIN etc.
\ compiler extensions and forward references for (S") and TYPE
S" TSQUOTE"   INCLUDED

INCLUDE DSK7.IMPORT

S" ITC-COLON" INCLUDED
\ your program starts hers
\ need the word BYE to exit program.
\ BYE in the kernel does not end with NEXT, so we can't use IMPORT:
COMPILER HEX
TARGET
CODE BYE   ( -- )
    83C4 @@ CLR,
    0000 @@ BLWP,
ENDCODE

\ need these primitives
IMPORT: 1- 0= DUP ?TERMINAL


TARGET
: MAIN
  FFFF
  BEGIN
    1-
  ?TERMINAL
  UNTIL
  BYE ;

COMPILER
PREAMBLE COLD
BOOT AUTOSTART MAIN
END.   ( report compile time and stats)

SAVE DSK7.TEST1

HOST