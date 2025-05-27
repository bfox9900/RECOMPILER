\ SAMS card memory management  Oct 15 2024 Brian Fox

NEEDS VIRT>REAL  FROM DSK1.SAMS
NEEDS FILLW      FROM DSK1.FILLW 

1 SEGMENT 

HERE 
VARIABLE SDP   \ sams dictionary pointer 

: SAMS.HERE   SDP @ ;
: SAMS.ALLOT  SDP +! ;
: ,L       SAMS.HERE !L  2 SAMS.ALLOT ;
: C,L      SAMS.HERE C!L 1 SAMS.ALLOT ; 

HEX
: BOUNDARY  ( addr -- addr addr') F000 AND  1000 + ;
: CROSSING? ( addr len -- addr len ?) 2DUP OVER BOUNDARY -ROT +  U< ;
: ?PAGE     ( addr len -- ) CROSSING? ABORT" SAMS page crossed"  ;

\ read,write,fill within 1 page ONLY 
: SAMS.WRITE ( addr sams len ) SWAP VIRT>REAL SWAP ?PAGE MOVE ;
: SAMS.READ  ( sams addr len ) 2>R  VIRT>REAL 2R>  ?PAGE MOVE ;
: SAMS.FILL  ( sams len char --) >R >R VIRT>REAL R> ?PAGE R> FILL ; 

HEX
CODE MOVEW  ( src dst u -- ) \ u= no. of bytes to move
   C036 , \ *SP+ R0 MOV,
   C076 , \ *SP+ R1 MOV,
          \  BEGIN,
   CC31 , \     R1 *+ R0 *+ MOV,
   0644 , \     TOS DECT,
   16FD , \  EQ UNTIL,
   C136 , \  TOS POP,
   NEXT,
ENDCODE

DECIMAL 
16 CONSTANT #16 
HEX 
CODE 16+   0224 , #16 , NEXT, ENDCODE \ TOS #16 AI,  

\ SLOWER but can cross page boundaries, 16 byte boundaries
DECIMAL 
: PARA.WRITE ( addr Sams len --) 
    BOUNDS DO  DUP  I VIRT>REAL #16 MOVEW 16+  #16 +LOOP DROP ;

: PARA.READ  ( sams addr len --) 
    BOUNDS DO  DUP VIRT>REAL  I #16 MOVEW 16+  #16 +LOOP DROP ;

: PARA.FILL  ( addr size char -- ) 
    DUP >< + 
    -ROT BOUNDS DO I VIRT>REAL OVER #16 SWAP FILLW  #16 +LOOP DROP ; 

CREATE TEMP #16 ALLOT 

: PARA.MOVE ( a1 a2 u -- ) \ moves in 16 byte blocks 
    BOUNDS DO  
       DUP VIRT>REAL TEMP #16 MOVEW
       TEMP I VIRT>REAL #16 MOVEW  
       16+   
    #16 +LOOP 
    DROP ;

\ move within SAMS segment 
: SAMS.MOVE   ( a1 a2 u -- ) BOUNDS DO  DUP @L  I !L   CELL+  2 +LOOP DROP ;
: SAMS.CMOVE  ( a1 a2 u -- ) BOUNDS DO  DUP C@L I C!L  CHAR+     LOOP DROP ;

\ SAMS string operators move memory BYTE by BYTE. Slow but can cross pages 
: SAMS.$WRITE  ( a1 a2 u -- )  BOUNDS DO  DUP C@  I C!L  CHAR+   LOOP DROP ;
: SAMS.$READ   ( sams a2 u --) BOUNDS DO  DUP C@L  I C!  CHAR+   LOOP DROP ;

: SAMS.PLACE ( addr len sams) 2DUP C!L  1+ SWAP SAMS.$WRITE ;

: SAMS>VDP ( sams vdp U --)  \ writes in 16 byte chunks   
    BOUNDS DO  DUP VIRT>REAL  I  #16 VWRITE  16+ #16 +LOOP ; 

: VDP>SAMS ( vdp sams u --) 
    BOUNDS DO  DUP   I VIRT>REAL #16 VREAD   16+ #16 +LOOP ; 

HERE SWAP -
CR DECIMAL . .( bytes)
