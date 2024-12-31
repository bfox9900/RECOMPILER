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

HEADERS ON  
WARNINGS OFF 

CHDIR DSK9.   \ camel99 source code disk 
S" ITC-TYPES"  +PATH INCLUDED   \ teach compiler CONSTANT VARIABLE : ; etc. 
S" BRANCHING"  +PATH INCLUDED   \ cross-compiler IF ELSE THEN BEGIN etc.

\ build the target image ...

COMPILER HEX 
CHDIR DSK9.

\ FORTH "CODE" words === 
S" FETCHSTORE" +PATH INCLUDED   
S" RSTACK"     +PATH INCLUDED
S" DODOES"     +PATH INCLUDED 
S" DATASTACK"  +PATH INCLUDED 
S" LOGICMATH"  +PATH INCLUDED 
S" MULTDIV"    +PATH INCLUDED 
S" COMPARES"   +PATH INCLUDED 
S" MOVEFILL"   +PATH INCLUDED 
S" SKIPSCAN"   +PATH INCLUDED
S" FIND"       +PATH INCLUDED 
S" MISC"       +PATH INCLUDED  \ contains PAUSE EXECUTE 
S" DOLOOP1"    +PATH INCLUDED  \ primitives for DO/LOOP

\ TI-99  low level I/O drivers 
S" VDP-API"    +PATH INCLUDED  
S" KEYSCAN"    +PATH INCLUDED  \ has ?TERMINAL 
\ =====================

\ compiler extensions and forward references for (S") and TYPE 
S" TSQUOTE"     +PATH INCLUDED  

\ Meta words look like Forth but compile to TARGET 
S" METADEFS"   +PATH INCLUDED 

\ add cross-compiler colon/semi-colon 
S" COLON"      +PATH INCLUDED   

\ 1st Forth "secondary" definitions 
S" TICKTOCK"   +PATH INCLUDED   \ 9901 hardware timer


\ Camel Forth System 
COMPILER 
S" HILEVEL"    +PATH INCLUDED  

COMPILER HEX 

\ compile BYE last, means HOST BYE is always available 
TARGET ( this is required)
CODE BYE   ( -- )
    83C4 @@ CLR,  
    0000 @@ BLWP, 
ENDCODE

COMPILER 
PREAMBLE COLD 
BOOT AUTOSTART WARM 

END.   ( report compile time and stats)

SAVE DSK8.NEWCAMEL 

CR ." === COMPILE ENDED PROPERLY ==="
QUIT
