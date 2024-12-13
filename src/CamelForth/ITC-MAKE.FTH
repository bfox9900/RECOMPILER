\ MAKE CAMEL99 ITC Forth with FXCC cross-compiler  Nov 2024  Brian Fox

\            ***   TIB AND STACK LOCATIONS ***
COMPILER 
HEX
        FFFC    EQU EMEM     \ EMEM = "end of memory"
        0050    EQU TIBSIZE  \ 80 bytes is the longest file string
EMEM TIBSIZE -  EQU 'TIB     \ Terminal input buffer just below

     'TIB  2 -  EQU 'SP0     \ FORTH DATA stack base address
     'SP0 6E -  EQU 'RP0     \ FORTH RETURN stack base address


\ ======================================================================
\             *** TARGET MEMORY LOCATION ***

    A000 VALUE CODESEG
    2000 VALUE HEAPSEG

    CODESEG ORG  

COMPILER 
NEW

\ ** We MUST load Executors first ** 
\ EXIT DOCON DOVAR DOCOL ?BRANCH and BRANCH
\ This
CHDIR DSK7.  \ FXCC system disk 
S" EXECUTORS"  +PATH INCLUDED   \ Lowest level primitives 
S" IMPORT"     +PATH INCLUDED   \ compiler extenstion to import code words 

CHDIR DSK9.   \ camel99 source code disk 
S" ITC-TYPES"  +PATH INCLUDED   \ teach compiler CONSTANT VARIABLE : ; etc. 
S" BRANCHING"  +PATH INCLUDED   \ teach cross-compiler IF ELSE THEN BEGIN etc.


\ *******************************************************************
\ build the target image ...

COMPILER HEX 
CHDIR DSK9.
S" LOWLEVEL" +PATH INCLUDED    \ primitives 
S" DOLOOP"   +PATH INCLUDED   \ code primitives and compiling words 

S" TI99IOY"      +PATH INCLUDED   \ ALL vdp primitives
S" TICKTOCK"     +PATH INCLUDED   \ 9901 hardware timer

S" HILEVEL"      +PATH INCLUDED   \ core forth words

\ hand code any more that you need here 

COMPILER HEX 
TARGET ( this is required)

CODE BYE   ( -- )
    83C4 @@ CLR,  
    0000 @@ BLWP, 
ENDCODE


PREAMBLE COLD 
BOOT AUTOSTART WARM 

END.   ( report compile time and stats)

SAVE DSK8.NEWCAMEL 

CR ." === COMPILE ENDED PROPERLY ==="
QUIT
