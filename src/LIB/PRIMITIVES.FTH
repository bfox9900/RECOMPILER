\ primitives for the TARGET system    Sept 16 2023  Brian Fox 
\ This is much Forth's "runtime"

\ These primitives are in SCRATCH-PAD RAM in Camel99
\ It's simpler to just write them over
CODE EXIT                  \ return from a colon definition 
               IP RPOP,    \ get previous XT into IP register
L: _NEXT  *IP+ W  MOV,     \ This is the Forth address list interpreter
          *W+  R5 MOV,   
              *R5 B,  
ENDCODE  ( _NEXT is put into R10 later) 

CODE LIT 
    TOS PUSH,  
    *IP+ TOS MOV,  
    NEXT,
ENDCODE   

CODE ?BRANCH  
    TOS DEC, 
    TOS POP,         \ drop number on data stack 
    NC IF,           \ if no carry flag set 
        *IP IP ADD,  \ take the jump 
        NEXT, 
    ENDIF, 
    IP INCT,         \ move forward in the program 
    NEXT,      
ENDCODE 

CODE BRANCH ( -- ) \ jump unconditionally 
    *IP IP ADD,     
    NEXT, 
ENDCODE 


\ steal some primitives code from the Kernel and copy into target
IMPORT: DUP DROP SWAP OVER ROT -ROT NIP 
\ IMPORT: C!  C@ COUNT  @ !  +! C+!  2!  2@   
IMPORT: SP@  SP! RP@  RP!
\  IMPORT: DUP>R  >R  R>  R@   2>R 2R>  
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


