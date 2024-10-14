\ TESTPROG3.FTH  test if else then 

\ You see T on 1st line and F on 3rd line.

COMPILER 
HEX 2000 ORG 
INCLUDE DSK7.ITC-FORTH

IMPORT: ?TERMINAL 

\ Gives us primitive screen output 
COMPILER HEX 
TARGET
8800 CONSTANT VDPRD               \ vdp ram read data
8C00 CONSTANT VDPWD               \ vdp ram write data
8C02 CONSTANT VDPWA               \ vdp ram read/write address

\ set VDP address to write mode or read mode 
CODE WMODE 
    TOS 4000 ORI,   
+CODE RMODE ( we can fall thru because there is no dictionary)   
    0 LIMI,
    TOS SWPB,  TOS VDPWA @@ MOVB, 
    TOS SWPB,  TOS VDPWA @@ MOVB,
    TOS POP, 
    NEXT, 
ENDCODE 

: EMIT+  ( c --) VDPWD C! ; \ write C and inc. address

54 CONSTANT 'T'
46 CONSTANT 'F'

: MAIN  
    0 WMODE        \ Set to line 0 col 0 
    FFFF           \ Test this true flag 
    IF   'T' EMIT+ \ Shows T 
    ELSE 'F' EMIT+
    THEN 

    40 WMODE       \ set VDP address to line 2 col 0 
    0              \ test this number 
    IF   'T' EMIT+
    ELSE 'F' EMIT+ \ shows F
    THEN    

    BEGIN ?TERMINAL UNTIL 
    BYE 
;

COMPILER 
T' MAIN  BOOT T!   \ set the boot variable 
SAVE DSK7.TEST3

HOST 
