\ executors.fth  are the primary words for the Forth VM    Sept 2023
\      >> This file must be loaded first on every build <<
\

\ build the internal Executors of Forth in the TARGETS wordlists 
\ Can't use TARGET directive yet because TLITERAL is not resolved 

ONLY FORTH ALSO COMPILERS ALSO ASSEMBLER 

ALSO TARGETS DEFINITIONS
\ Forth data interpreters
L: DOCON        TOS PUSH,   \ make room in TOS
            *R11 TOS MOV,   \ move contents of PFA into IP register 
             NEXT,

\ Executor that executes a "VARIABLE"
L: DOVAR         TOS PUSH,  \ make room in TOS                   28
             R11  TOS MOV,  \ move PFA into Forth IP register    14
             NEXT,

L: DOCOL    \ enter a colon definition 
    IP RPUSH,  
    R11 IP MOV,    
    NEXT,
    
\ ===========================================
CODE EXIT                  \ return from a colon definition 
           IP RPOP,        \ get previous XT into IP register
L: _NEXT  *IP+ W  MOV,     \ This is the Forth address list interpreter
          *W+  B,  
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

