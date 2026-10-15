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

\ EXTEND THE BARE BONES COMPILER
CHDIR DSK9.   \ camel99 source code disk
S" ITC-CODE"  INCLUDED   \ #1 must be the CODE ENDCODE definer
S" EXECUTORS" INCLUDED   \ fundamental runtime code for Forth
S" ITC-TYPES" INCLUDED   \ CONSTANT VARIABLE USER : ; etc.
S" BRANCHING" INCLUDED   \ cross-compiler IF ELSE THEN BEGIN etc.

\ compiler extensions and forward references for (S") and TYPE
S" TSQUOTE"   INCLUDED

INCLUDE DSK7.IMPORT

S" ITC-COLON" INCLUDED

TARGET
VARIABLE BOOT          \ holds address of first COLON word to run

\ this is the first code to run when code is loaded.
\ It creates the Forth virtual machine. Other code can be added here.
CODE COLD
      WRKSP0 LWPI,
      SP  'SP0  LI,    \ data stack
      RP  'RP0  LI,    \ return stack
      R10 _NEXT LI,    \ inner interpreter: _NEXT is in EXECUTORS
      IP  BOOT  LI,    \ load interpreter pointer with boot word
      *R10 B,          \ run Forth NEXT (inner interpreter)
ENDCODE


\ your program starts hers
\ need the word BYE to exit program.
\ BYE in the kernel does not end with NEXT, so we can't use IMPORT:
COMPILER HEX
TARGET
CODE BYE   ( -- )
    83C4 @@ CLR,
    0000 @@ BLWP,
ENDCODE

\ need these primitives from Camel Forth kernel
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

SAVE DSK8.TEST1

HOST