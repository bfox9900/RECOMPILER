\ TESTBYE.FTH   ** BARE MINUMUM ITC Forth PROGRAM **
COMPILER
HEX
\    *** TIB AND STACK LOCATIONS ***
        FFFC    EQU EMEM     \ EMEM = "end of memory"
        0050    EQU TIBSIZE  \ 80 bytes is the longest file string
EMEM TIBSIZE -  EQU 'TIB     \ Terminal input buffer just below

\ These equates are used by cold. They could be hard coded as numbers in COLD
\      8300      EQU WRKSP0   \ primary workspace in 16 bit RAM
\     'TIB  2 -  EQU 'SP0     \ FORTH DATA stack base address
\     'SP0 6E -  EQU 'RP0     \ FORTH RETURN stack base address

\ If the program is simple we can put data and return stacks in PAD RAM
       8300  EQU WRKSP0   \ primary workspace in 16 bit RAM
       8400  EQU 'SP0     \ DATA stack in 16 bit RAM
       83A0  EQU 'RP0     \ RETURN stack in 16 bit RAM


\   *** TARGET MEMORY LOCATION ***

    A000 VALUE CODESEG
    2000 VALUE HEAPSEG

\ setup the build
    NEW
    CODESEG ORG

HEADERS  OFF \ include dictionary
WARNINGS OFF  \ prevents endless "<xxxx> is redefined"

CHDIR DSK9.   \ DSK9. is the Camel99 source code disk
S" ITC-CODE"  INCLUDED   \ CODE for ITC Forth
S" EXECUTORS" INCLUDED   \ fundamental runtime code for Forth
S" ITC-TYPES" INCLUDED   \ CONSTANT VARIABLE : ; etc.
S" BRANCHING" INCLUDED   \ cross-compiler IF ELSE THEN BEGIN etc.
S" ITC-COLON" INCLUDED   \ cross-compiler colon/semi-colon
S" DATASTACK" INCLUDED
S" LOGICMATH" INCLUDED
S" FETCHSTORE" INCLUDED
S" MISC"      INCLUDED
S" RSTACK"    INCLUDED
S" DOLOOP"    INCLUDED
S" VDPDRVR"   INCLUDED

TARGET
: (S")    ( -- c-addr u) R>  COUNT  2DUP + ALIGNED >R ;
T' (S")  RESOLVES '(S")

S" METADEFS"  INCLUDED

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

COMPILER HEX

TARGET
CODE BYE   ( -- )
    83C4 @@ CLR,
    0000 @@ BLWP,
ENDCODE


VARIABLE CURPOS

: TYPE  ( addr len --) DUP>R CURPOS @ SWAP  VWRITE R> CURPOS +! ;

: WAIT   FFFF 0 DO LOOP ;

TARGET
: GO
    40 CURPOS !
    S" HELLO WORLD, "  TYPE
    S" this is FORTH" TYPE
    WAIT
    BYE ;

PREAMBLE COLD
BOOT AUTOSTART GO

END.   ( report compile time and stats)

SAVE DSK8.HELLO1

HOST BYE