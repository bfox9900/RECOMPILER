\ executors.fth  are the primary words for the Forth VM    Sept 2023
\      >> This file must be loaded first on every build <<
\

\ build the internal Executors of Forth in the TARGETS wordlists 
\ Can't use TARGET directive yet because TLITERAL is not resolved 

ONLY FORTH ALSO COMPILERS ALSO ASSEMBLER 

ALSO TARGETS DEFINITIONS
\ Forth data interpreters
L: DOVAR
    TOS PUSH,      \ make room in TOS 
    W TOS MOV,       \ contents of PFA -> TOS
    NEXT,

L: DOCON 
    TOS PUSH,
    *W TOS MOV,
    NEXT,

L: DOCOL    \ enter a colon definition 
    IP RPUSH,  
    W IP MOV, 
    NEXT,
\ ===========================================
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

\ CODE (FOR)    \ same as >R but could change to a register 
\     TOS RPUSH, 
\     TOS POP,
\     NEXT, 
\ ENDCODE      

CODE (NEXT)
    *RP DEC,          \ decrement loop ON RSTACK or R15
    OC IF,            \ test carry flag
        *IP IP ADD,   \ take the jump 
        NEXT,
    ENDIF, 
    RP INCT,       \ remove counter from Rstack 
    R9 INCT,       \ move past (LOOP)'s in-line parameter
    NEXT, 
ENDCODE

