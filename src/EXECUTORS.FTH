\ executors.fth  are the primary words for the Forth VM    Sept 2023
\      >> This file must be loaded first on every build <<
\

\ build the internal Executors of Forth in the TARGETS wordlists 
\ Can't use TARGET directive yet because TLITERAL is not resolved 

ONLY FORTH ALSO COMPILERS ALSO ASSEMBLER ALSO TARGETS DEFINITIONS
\ Executors are not "code" words. They are code snippets
L: EXIT                  \ return from a colon definition 
     IP RPOP,  
    *IP+ W  MOV,           \ T' EXIT CELL+ = NEXT, goes into R10
    *W+  R5 MOV,   
     R5 **  B,       

L: DOVAR 
    TOS PUSH,        \ make room in TOS 
    W TOS MOV,       \ contents of PFA -> TOS
    NEXT,

L: DOCON 
    TOS PUSH,
    *W TOS MOV,
    NEXT,

L: DOCOL         \ enter a colon definition 
    IP RPUSH,  
    W IP MOV, 
    NEXT,

\ ========================================
CODE LIT 
    TOS PUSH,       \ 28
    *IP+ TOS MOV,   \ 22
    NEXT,
ENDCODE   

CODE ?BRANCH  
    TOS DEC, 
    TOS POP,         \ consume the number on the data stack 
    NC IF,           \ if no carry flag set 
        *IP IP ADD,  \ take the jump 
        NEXT, 
    ENDIF, 
    IP INCT, 
    NEXT,      
ENDCODE 

CODE BRANCH ( -- )   \ jump unconditionally 
    *IP IP ADD,     
    NEXT, 
ENDCODE 

