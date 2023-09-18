\ doloop.FTH  adds 104 bytes to the recompiled program 

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

CODE I  ( -- n)
        TOS PUSH,        
        *RP    TOS MOV, 
        2 (RP) TOS SUB,    
        NEXT,             
ENDCODE

CODE J      ( -- n)
        TOS PUSH,
        4 (RP) TOS MOV,   \ outer loop index is on the rstack
        6 (RP) TOS SUB,   \ index = loopindex - fudge
        NEXT,
ENDCODE

VARIABLE LP 
VARIABLE L0       COMPILER  4 CELLS TALLOT 

TARGET 
: >L        ( x -- ) ( L: -- x ) 2 LP +!   LP @ ! ;     \ LP stack grows up
: L>        ( -- x ) ( L: x -- ) LP @ @  -2 LP +! ;

: RAKE  ( -- ) ( L: 0 a1 a2 .. aN -- )
        BEGIN  L> ?DUP WHILE   (THEN)  REPEAT ;


COMPILER ALSO META DEFINITIONS 
: DO        ( n n -- adr)  TCOMPILE <DO>   0 >L  THERE  ; IMMEDIATE
: ?DO       ( n n -- adr)  TCOMPILE <?DO>  0 >L  THERE  ; IMMEDIATE
: LEAVE     ( -- ) TCOMPILE UNLOOP  TCOMPILE BRANCH AHEAD  >L ; IMMEDIATE

\ complete a DO loop
: LOOP      ( -- )  TCOMPILE <LOOP>  <BACK  RAKE ; IMMEDIATE
: +LOOP     ( -- )  TCOMPILE <+LOOP> <BACK  RAKE ; IMMEDIATE
PREVIOUS DEFINITIONS 
