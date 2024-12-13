\ doloop.FTH  adds 104 bytes to the recompiled program 

\ Could not IMPORT these words because they jump into each other a lot
\ and some have multiple NEXT points. 

COMPILER HEX

\ Runtime code for DO LOOP
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
        TOS *RP ADD,   \ save space by jumping into <loop>
        TOS POP,       \ refill TOS, (does'nt change overflow flag)
        2 $ JMP,
+CODE <LOOP>
        *RP INC,       \ increment loop
2 $:    OO IF,   
             IP INCT,  \ skip past jump back value 
             1 $ JMP, 
        ENDIF, 
        *IP IP ADD,    \ jump back
        NEXT,
+CODE UNLOOP
1 $:    RP  4 AI,      \ collapse rstack frame
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

\ COMPILER words for DO LOOP 
\ These versions are used to compile internal words that use DO LOOP
\ ** LEAVE is NOT supported in the Meta compiler **
\ For a live kernel you need to also compile ISOLOOPS.FTH 
\ ------------------
COMPILER ALSO META DEFINITIONS 
\ These words are META compilers. They are in the HOST Forth. 
\ They look like Forth words but they COMPILE code into the TARGET image 
HOST: DO        ( n n -- adr)  TCOMPILE <DO>    THERE  ;HOST IMMEDIATE
HOST: ?DO       ( n n -- adr)  TCOMPILE <?DO>   THERE  ;HOST IMMEDIATE
HOST: LEAVE     ( -- ) 
  TCOMPILE UNLOOP  TCOMPILE BRANCH  THERE 0 T,   ;HOST IMMEDIATE

\ complete a DO loop
HOST: LOOP      ( -- )  TCOMPILE <LOOP>  THERE - T, ;HOST IMMEDIATE
HOST: +LOOP     ( -- )  TCOMPILE <+LOOP> THERE - T, ;HOST IMMEDIATE

PREVIOUS DEFINITIONS 
DECIMAL 
