\ primitives for the TARGET system    Oct 26 2024 Brian Fox
\ This is much of Forth's "runtime"

\ steal primitives code from the Kernel and copy into target
IMPORT: DUP DROP SWAP OVER ROT -ROT
IMPORT: C!  C@  @ !  +! C+!  2!  2@
IMPORT: SP@  SP! RP@  RP!
IMPORT: DUP>R  >R  R>  R@   2>R 2R>
IMPORT: ?DUP  ><  2DROP 2DUP  2SWAP PICK
IMPORT: AND OR XOR
IMPORT: 1+  1-  2+  2-  2*  4*  8*  2/
IMPORT: 1+!  1-!
IMPORT: + -  D+
IMPORT: RSHIFT LSHIFT INVERT ABS  NEGATE  ALIGNED
IMPORT: UM*  *  UM/MOD  M/MOD
IMPORT: =  0= OVER= 0<  U<  >  <
IMPORT: MIN  MAX SPLIT FUSE
IMPORT: MOVE FILL SKIP  SCAN
IMPORT: ON OFF
IMPORT: BOUNDS /STRING
IMPORT: MOVE
IMPORT: (FIND)

\ hand code any more that you need here
TARGET ( this is required)
CODE EXECUTE ( xt -- )
    TOS W MOV,
    TOS POP,
    *W+ R5 MOV,
    R5 ** B,
ENDCODE

COMPILER HEX

TARGET
CODE BYE   ( -- )
    83C4 @@ CLR,
    0000 @@ BLWP,
ENDCODE
