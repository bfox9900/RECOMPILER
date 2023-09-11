\ doloop.FTH 

\ Could not IMPORT these words because they jump into each other a lot
\ and some have multiple NEXT points. 
TARGET 
CODE <?DO>  ( limit ndx -- )
        *SP TOS CMP, 
        1 $ JNE, 
        TOS POP,
        TOS POP, 
        IP RPOP,
        NEXT,

+CODE <DO>  ( limit indx -- )
1 $:    R0  8000 LI,  
        *SP+ R0  SUB,
        R0  TOS ADD, 
        R0  RPUSH,
        TOS RPUSH,
        TOS POP, 
        NEXT,
ENDCODE

CODE <+LOOP>
        TOS *RP ADD, 
        TOS POP,   
        2 $ JMP,
        
+CODE <LOOP>
        *RP INC,      
2 $:    1 $ JNO,  
        IP INCT, 
        3 $ JMP, 

1 $:    *IP IP ADD, 
        NEXT,

+CODE UNLOOP
3 $:    RP  4 AI, 
        NEXT,
ENDCODE

IMPORT: I J 

ALSO META DEFINITIONS 
: DO        TCOMPILE <DO>  THERE ; IMMEDIATE
: ?DO       TCOMPILE <?DO> THERE ; IMMEDIATE

: LOOP      TCOMPILE <LOOP>  <BACK ; IMMEDIATE
: +LOOP     TCOMPILE <+LOOP> <BACK ; IMMEDIATE 

PREVIOUS DEFINITIONS 
