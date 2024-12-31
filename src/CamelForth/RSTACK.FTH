CR .( RETURN STACK)

CODE RP@    ( -- a )
            TOS PUSH,
            RP TOS MOV,
            NEXT,
            ENDCODE

CODE RP!    ( a -- )
            TOS RP MOV,
            TOS POP,
            NEXT,
            ENDCODE

CODE >R     ( w -- )
            TOS RPUSH,        \ 28
            TOS POP,          \ 22
            NEXT,
            ENDCODE          \ 50

CODE R>     ( -- w )
            TOS PUSH,         \ 28
            TOS RPOP,         \ 22
            NEXT,
            ENDCODE          \ 50

\ CODE RDROP  RP INCT, NEXT, ENDCODE            

CODE R@     ( -- w )
            TOS PUSH,
            *RP TOS MOV,
            NEXT,
            ENDCODE

\ double Rstack Forth2012
CODE 2>R    ( d -- ) ( r-- n n)
      RP  -4 AI,          \ 14
      TOS    *RP MOV,      \ 22
      *SP+ 2 (RP) MOV,     \ 26
      TOS POP,             \ 22
      NEXT,              \ = 84
ENDCODE

CODE 2R>     ( -- d )
      TOS PUSH,            \ 28
      SP DECT,             \ 10
      TOS RPOP,            \ 22
      *SP  RPOP,            \ 26
      NEXT,              \ = 88
ENDCODE

\ NON-STANDARD optimizing word 
CODE DUP>R ( n --) TOS RPUSH,  NEXT, ENDCODE 
