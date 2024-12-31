[CC] CR .( DODOES )

\ DODOES is the code action of a DOES> clause.
\ For ITC Forth:
\ defined word:  CFA: doescode
\                PFA: parameter field
\
\ Note: In V2.5 implementation we used BL @DODOES. (branch and link)
\ DODOES is entered with W=PFA (parameter field address)
\ DODOES moves register W to the TOP of Stack register. (R4 is TOS)
\ So the high-level Forth code begins with the address of the parameter
\ field on top of stack.
\ Using BL automatically computed the new parameter field into R11
\ which is exactly what we need to be the IP so we just do one MOV.

COMPILER FORWARD 'DODOES

TARGET
TCREATE DODOES  ( -- a-addr)
            TOS PUSH,       \ save TOS reg on data stack      28
            W TOS MOV,      \ put defined word's PFA in TOS   14
            IP RPUSH,       \ push old IP onto return stack   28
            R11 IP MOV,     \ R11 has the new PFA -> IP       14
            NEXT,                                       \     84
            ENDCODE

T' DODOES RESOLVES 'DODOES    \ 'DODOES is used by the cross-compiler
