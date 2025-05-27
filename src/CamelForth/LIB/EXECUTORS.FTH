\ executors.fth  are the primary words for the Forth VM    Sept 2023
CR .( executors. runtime code for data types)

TARGET
\ EXIT from a colon definition
CODE EXIT
L: _EXIT         IP RPOP,  \ placed inline for speed
\ the Forth interpreter "NEXT: is 3 instructions for ITC
L: _NEXT    *IP+ W  MOV,
            *W+  R5 MOV,
            *R5  B,

\ Forth data interpreters are code fragments not "words"
\ action of a variable
TCREATE DOVAR
           TOS PUSH,      \ make room in TOS
            W TOS MOV,     \ contents of PFA -> TOS
            NEXT,
T' DOVAR RESOLVES 'DOVAR

\ action of a constant
TCREATE DOCON
            TOS PUSH,
            *W TOS MOV,
            NEXT,
T' DOCON RESOLVES 'DOCON

\ enter a colon definition
TCREATE DOCOL
              IP RPUSH,
            W IP MOV,
            NEXT,
T' DOCOL  RESOLVES 'DOCOL

L: DOUSER ( -- addr)
            TOS PUSH,      \ Executor that executes a "USER VARIABLE"
            TOS STWP,
            *W TOS ADD,    \ add USER VAR's offset to Workspace pointer
            NEXT,
T' DOUSER  RESOLVES 'DOUSER

\ action of a literal number
CODE LIT
        TOS PUSH,
        *IP+ TOS MOV,
        NEXT,
ENDCODE
T' LIT  RESOLVES 'LIT
