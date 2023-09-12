\ primitives for the TARGET system 

TARGET ALSO FORTH 
\ steal some primitives code from the Kernel and copy into target
IMPORT: C!  2!  2@  COUNT  +! C+! 
IMPORT: RP@  RP! DUP>R  >R  R>  R@  SP@  SP!  2>R 2R>  
IMPORT: NIP  ?DUP  SWAP  OVER  ROT -ROT  ><  2DROP 2DUP  2SWAP PICK  
IMPORT: AND  OR  XOR 
IMPORT: 1+  1-  2+  2-  2*  4*  8*  2/   
IMPORT: 1+!  1-! 
IMPORT: -  D+  RSHIFT LSHIFT INVERT ABS  NEGATE  ALIGNED 
IMPORT: UM*  *  UM/MOD  M/MOD  
IMPORT: =  OVER= 0<  U<  >  < 
IMPORT: MIN  MAX SPLIT FUSE 
IMPORT: MOVE FILL SKIP  SCAN 
IMPORT: ON OFF 

CODE EXECUTE ( xt -- )
    TOS W MOV,
    TOS POP,  
    *W+ R5 MOV,  
    R5 ** B, 
ENDCODE

IMPORT: BOUNDS /STRING 
IMPORT: SKIP  SCAN  

PREVIOUS 



