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

CR .( Code primitives complete) 
