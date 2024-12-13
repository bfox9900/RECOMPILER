\ executors.fth  are the primary words for the Forth VM    Sept 2023

TARGET 
\ EXIT from a colon definition 
CODE EXIT    IP RPOP,     
\ the Forth interpreter "NEXT: is 3 instructions for ITC 
L: _NEXT    *IP+ W  MOV,  
            *W+  R5 MOV,  
            *R5  B,     
ENDCODE 

\ Forth data interpreters are code fragments not "words"
\ action of a variable 
L: DOVAR    TOS PUSH,      \ make room in TOS 
            W TOS MOV,     \ contents of PFA -> TOS
            NEXT,

\ action of a constant 
L: DOCON    TOS PUSH,
            *W TOS MOV,
            NEXT,

\ enter a colon definition 
L: DOCOL    IP RPUSH,  
            W IP MOV, 
            NEXT,

\ action of a literal number 
CODE LIT 
        TOS PUSH, 
        *IP+ TOS MOV, 
        NEXT, 
ENDCODE     

\ *branches are IP relative (not absolute)

\ branch if TOS = 0 
CODE ?BRANCH 
        TOS DEC, 
        TOS POP, 
        NC IF,
          *IP IP ADD, 
           NEXT, 
        ENDIF, 
        IP INCT, 
        NEXT,
ENDCODE 

\ unconditional branch 
CODE BRANCH      
        *IP IP ADD, 
        NEXT,  
ENDCODE 

\ RUNTIME of FOR/NEXT loop 
CODE (NEXT)
    *RP DEC,        \ dec counter value
    OC IF,          
        *IP IP ADD, \ jump back: ADD *IP,IP
        NEXT,
    ENDIF, 
    RP INCT,        \ remove counter from Rstack 
    IP INCT,        \ move past (LOOP)'s in-line parameter
    NEXT, 
ENDCODE
