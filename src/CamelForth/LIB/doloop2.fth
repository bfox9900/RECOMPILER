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

\ These words must reside in the target for use by DO LOOPs
\ make space for a "LEAVE" stack in target memory 
TCREATE LP  6 CELLS TALLOT 

IMPORT: @ +! ! 
: >L        ( x -- ) ( L: -- x ) 2 LP +!   LP @ ! ;     \ LP stack grows up 
: L>        ( -- x ) ( L: x -- ) LP @ @  -2 LP +! ;

: RAKE  ( -- ) ( L: 0 a1 a2 .. aN -- ) \ it RAKEs the LEAVEs :-) 
        BEGIN  L> ?DUP WHILE  TPOSTPONE THEN   REPEAT ;

\ ------------------
COMPILER ALSO META DEFINITIONS 
\ These words are META compilers. They are in the HOST Forth. 
\ They look like Forth words but they COMPILE code into the TARGET image 
HOST: DO        ( n n -- adr)  TCOMPILE <DO>   0 >L  THERE  ;HOST IMMEDIATE

IMPORT: <> 2DUP 
HOST: ?DO       ( n n -- adr) 
    TCOMPILE 2DUP 
    TCOMPILE -
    TCOMPILE ?BRANCH THERE  0 T, >L  
    TCOMPILE <DO>  
    0 >L  
    THERE  
;HOST IMMEDIATE


HOST: LEAVE     ( -- ) 
  TCOMPILE UNLOOP  TCOMPILE BRANCH  THERE  0 T, >L  ;HOST IMMEDIATE

\ complete a DO loop
HOST: LOOP      ( -- )  TCOMPILE <LOOP>  THERE - T, RAKE ;HOST IMMEDIATE
HOST: +LOOP     ( -- )  TCOMPILE <+LOOP> THERE - T, RAKE ;HOST IMMEDIATE

PREVIOUS DEFINITIONS 

