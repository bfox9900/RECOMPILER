[CC] CR .( ON OFF PAUSE FUSE SPLIT) 

TARGET 
CODE ON   ( adr -- )
        *TOS SETO, 
        TOS POP,
        NEXT,
        ENDCODE

CODE OFF  ( adr -- )
        *TOS CLR, 
        TOS POP, 
        NEXT, 
        ENDCODE 

\ multi-tasking support
CODE PAUSE  ( -- )  
        NEXT, 
        ENDCODE

\ used in sprite control and very handy for byte/cell manipulation
 CODE SPLIT ( AABB --  BB AA )  \ split cell into 2 bytes
        TOS W MOV,  
        TOS 8 SRL, 
        W FF ANDI,  
        W PUSH,
        NEXT,
        ENDCODE

 CODE FUSE  ( BB  AA -- AABB )  \ fuse two bytes to one cell 
        TOS SWPB,
        *SP+ TOS ADD,
        NEXT,
        ENDCODE
            
[CC] CR .( PERFORM  EXECUTE)
TARGET 
CODE PERFORM ( addr -- )
        *TOS W MOV,       \ fetch the XT from an address
        1 $ JMP,
+CODE EXECUTE ( 'xt -- )    
        TOS W MOV,       \ move the execution token in TOS into temp
1 $:    TOS POP,         \ refill TOS
        *W+ R5 MOV,      \ move contents of the XT to R5
        *R5 B,           \ branch to the address of the DOER routine
        ENDCODE

\ : DIGIT?  ( c -- n -1)   \ if c is a valid digit
\              -- x  0   \ otherwise
\  DUP  39 > 100 AND +    \ silly looking
\  DUP 140 > 107 AND -  [CHAR] 0 -   \ but it works!
\  DUP BASE@ U< ;         \ 48 Bytes

CODE DIGIT?   ( char -- n f )  \ 36 bytes, much faster
      R1 STWP,           \ multi-tasking friendly for USER VARS
      TOS PUSH,          \  dup char
      TOS -30 AI,      \  convert char to number
      TOS 9 CI,
      HI IF,             \ > 9 ?
          TOS -7 AI,
          TOS 9 CI,
          1 $ JL,        \ less than 9, jump out
      ENDIF,
      TOS 2A (R1) CMP,   \ compare to USER var 2A (BASE)
      1 $ JHE,           \ tos>base, jump out
      TOS *SP MOV,       \ replace char with no.
      TOS SETO,          \ set flag to true
      NEXT,
\ error out here
1 $:  TOS CLR,
      NEXT,
ENDCODE  



CR .( Code primitives complete) 
