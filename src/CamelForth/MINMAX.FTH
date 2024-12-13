CR .( MIN & MAX )

CODE MIN    ( n1 n2 -- n)
    *SP TOS CMP,    \ compare n1 & n2
    GTE IF,         \ if n1 >= n2
        SP INCT,    \ NIP n1
        NEXT, 
    ENDIF, 
    TOS POP,         \ DROP n1
    NEXT,
ENDCODE 

CODE MAX   ( n1 n2 -- n)
    *SP  TOS CMP,   \ compare n1 & n2
    LTE IF,          \ if n1 <= n2 
        SP INCT,    \ NIP n1
        NEXT, 
    ENDIF, 
    TOS POP,      
    NEXT,
ENDCODE


9 0 MIN . 
9 0 MAX .
