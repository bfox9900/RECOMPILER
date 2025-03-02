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

\      ********* CROSS-COMPILER EXTENSIONS GO HERE *********
S" ITC-TYPES"  +PATH INCLUDED   \ teach compiler CONSTANT VARIABLE : ; etc. 
S" BRANCHING"  +PATH INCLUDED   \ cross-compiler IF ELSE THEN BEGIN etc.

\ compiler extensions and forward references for (S") and TYPE 
S" TSQUOTE"     +PATH INCLUDED  


\ Meta words look like Forth but compile to TARGET 
\ S" METADEFS"   +PATH INCLUDED 



\      ********** TARGET IMAGE CODE *****************
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
S" DOLOOP"     +PATH INCLUDED  \ primitives & compilers for DO/LOOP

\ TI-99  low level I/O drivers 
S" VDP-API"    +PATH INCLUDED  
S" KEYSCAN"    +PATH INCLUDED  \ has ?TERMINAL 
\ =====================


\ Now we can add the *CROSS-COMPILER* colon & semi-colon 
S" COLON"      +PATH INCLUDED   

\ 1st Forth "secondary" definitions 
S" TICKTOCK"   +PATH INCLUDED   \ 9901 hardware timer



COMPILER HEX

TARGET
VARIABLE BOOT 
CODE COLD
      WRKSP0 LWPI,
      SP  'SP0  LI,    \ data stack
      RP  'RP0  LI,    \ return stack
      R10 _NEXT LI,    \ inner interpreter
      IP  BOOT  LI,    \ load interpreter pointer with boot word
      *R10 B,          \ run Forth NEXT (inner interpreter)
ENDCODE

COMPILER HEX 
TARGET
CODE BYE   ( -- )
    83C4 @@ CLR,  
    0000 @@ BLWP, 
ENDCODE

TARGET 
: DELAY   7FFF 0 DO LOOP ;

: DLY2    5FFF BEGIN 1- DUP 0= UNTIL ;

20 CONSTANT BL 

: TWOS  0 300  32 VFILL ;
: PAGE  0 300  BL VFILL ;

: TEST    
   31 0 VC!  DELAY  
   TWOS   DELAY 
   PAGE 
   33 0 VC!  DLY2  
   BYE ;


COMPILER 
TLAST  HOST @ DUP   COMPILER LATEST T! ORGLAST T!

COMPILER 
THERE TCELL+  DP T!     \ set current dictionary pointer 
THERE TCELL+  ORGDP T!  \ set restart dictionary pointer (same)

PREAMBLE COLD           \ set code word that inits the system 
BOOT AUTOSTART TEST     \ set the BOOT variable to the 1st Forth word 

END.   ( report compile time and stats)

SAVE DSK8.NEWCAMEL 

HOST 
CR ." === COMPILE ENDED PROPERLY ==="
QUIT


