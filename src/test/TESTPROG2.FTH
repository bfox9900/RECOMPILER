\ TESTPROG2.FTH  Demo IMPORT: CODE  loops and AUTOSTART   Sep 2023 Fox 
COMPILER 
HEX 2000 ORG   \ this must be set before compiling any code 

INCLUDE DSK7.ITC-FORTH \ preamble for indirect threaded Forth

\ give us a primitive screen output word ( EMIT+ )
COMPILER HEX 
TARGET
8800 CONSTANT VDPRD               \ vdp ram read data
8C00 CONSTANT VDPWD               \ vdp ram write data
8C02 CONSTANT VDPWA               \ vdp ram read/write address

CODE WMODE 
    TOS 4000 ORI,   
+CODE RMODE ( we can fall thru because there is no dictionary)   
    0 LIMI,
    TOS SWPB,  TOS VDPWA @@ MOVB, 
    TOS SWPB,  TOS VDPWA @@ MOVB,
    TOS POP, 
    NEXT, 
ENDCODE 

: EMIT+  ( c --) VDPWD C! ; \ write c & inc. address

TARGET
: LOOP1 ( -- ) FFFF BEGIN  1-  DUP WHILE REPEAT DROP ; \ 10 secs
: LOOP2 ( -- ) FFFF BEGIN  1- DUP   0= UNTIL   DROP ;  \ 11 secs 
: LOOP3 ( -- ) FFFF FOR  NEXT ;                        \  3 secs

COMPILER DECIMAL 
TARGET 
: MAIN  ( -- ) 
    0 WMODE         \ set VDP write address 
    65 EMIT+ LOOP1 
    66 EMIT+ LOOP2 
    67 EMIT+ LOOP3 
    BYE ;

COMPILER 
AUTOSTART MAIN 
SAVE DSK7.ITCTEST2

HOST 

