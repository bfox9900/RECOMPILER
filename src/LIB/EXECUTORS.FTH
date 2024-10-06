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
    
