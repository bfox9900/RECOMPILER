\ SAMS card memory management like Forth dictionary

NEEDS VIRT>REAL  FROM DSK1.SAMS

1 SEGMENT 

VARIABLE SDP   \ sams dictionary pointer

: SAMS.HERE   SDP @ ;
: SAMS.ALLOT  SDP +! ;
: ,L       SAMS.HERE !L  2 SAMS.ALLOT ;
: C,L      SAMS.HERE C!L 1 SAMS.ALLOT ; 

HEX
: BOUNDARY  ( addr -- addr addr') F000 AND  1000 + ;
: CROSSING? ( addr len -- addr len ?) 2DUP OVER BOUNDARY -ROT +  U< ;
: ?PAGE     ( addr len -- ) CROSSING? ABORT" SAMS page limit"  ;

: SAMS.BLOCK ( n -- addr) 400 * >REAL ; \ 1K blocks in SAMS 
: SAMS.REC   ( n -- addr)  80 * >REAL ; \ 128 byte records in SAMS 
: SAMS.PARA  ( n -- addr)  10 * >REAL ;

: SAMS.WRITE ( addr len Virtaddr) >REAL OVER ?PAGE MOVE ;
: SAMS.READ  ( Virtaddr len addr )>R  SWAP >REAL SWAP ?PAGE  R> MOVE ;

: SAMS.MOVE  ( Virt1 Virt2 u --) \ slow move but can cross pages 
    BOUNDS DO  DUP @L I !L  2 +LOOP ;


HEX
CODE FILLW ( src dst u --)
  C036 , C236 , CE04 , 0640 , 18FD ,
  C136 , NEXT,
ENDCODE
 

\  fill 4k pages at a time. *NO PROTECTION* 
NEEDS FILLW  FROM DSK1.FILLW 
HEX 1000 CONSTANT 4K 
: PAGE.FILL  ( addr len char --)
    -ROT 
    BOUNDS 
    DO 
       I >REAL OVER 4K SWAP FILLW 
    4K +LOOP    
; 

: PARA.FILL  ( addr size char -- ) 
    -ROT 
    BOUNDS 
    DO 
       I >REAL OVER 10 SWAP FILLW 
    10 +LOOP    
; 

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
: PARA.MOVE ( a1 a2 u -- ) \ moves in 16 byte blocks 
    BOUNDS 
    DO  
       DUP >REAL  I >REAL 16 MOVEW  
       16 +   
    16 +LOOP 
    DROP 
;

: SAMS.MOVE   ( a1 a2 u -- ) BOUNDS DO  DUP @L  I !L   2 +LOOP DROP ;
: SAMS.CMOVE  ( a1 a2 u -- ) BOUNDS DO  DUP C@L I C!L  LOOP DROP ;
: FORTH.CMOVE ( a1 a2 u -- ) BOUNDS DO  DUP C@ I C!  LOOP DROP ;
