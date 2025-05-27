[CC] CR .( COMPARISON operators )

TARGET
CODE 0=     ( n -- ?)         \ used 6X in kernel
            TOS NEG,
            NE IF,
                TOS SETO,
            ENDIF,
            TOS INV,
            NEXT,
            ENDCODE

CODE OVER=  ( n1 n2 -- n1 ?)
            *SP TOS CMP,     \ used 2x in kernel & in CASE
            1 $ JMP,
+CODE =     ( n n -- ?)     \ Used 4X in Kernel
            *SP+ TOS CMP,
1 $:        TOS CLR,
            EQ IF,
               TOS SETO,
            ENDIF,
            NEXT,
ENDCODE

CODE 0<     ( n -- flag )
            TOS TOS MOV,  \ **MUST USE MOV HERE**
            TOS SETO,
            1 $ JLT,
            TOS CLR,
1 $:        NEXT,
            ENDCODE

CODE U<     ( n1 n2 -- flag)
            *SP+ TOS CMP,
            TOS SETO,
            1 $ JL,
            TOS CLR,
1 $:        NEXT,
            ENDCODE

CODE >      ( n1 n2 -- flag)
            *SP+ TOS CMP,
            TOS SETO,
            1 $ JGT,
            TOS CLR,
1 $:        NEXT,
            ENDCODE

CODE <      ( n1 n2 -- flag)
            *SP+ TOS CMP,
            TOS SETO,
            1 $ JLT,
            TOS CLR,
1 $:        NEXT,
             ENDCODE

[CC] CR .( MIN & MAX )

TARGET
CODE MIN    ( n1 n2 -- n)
        *SP TOS CMP,   \ compare n1 & n2
        1 $ JLT,       \ if n1 < n2 goto 1 $:
        SP INCT,
        NEXT,
+CODE MAX   ( n1 n2 -- n)
        *SP  TOS CMP,  \ compare n1 & n2
        1 $ JGT,       \ if n1 > n2 goto 1 $:
        SP INCT,       \ otherwise NIP n1
        NEXT,
1 $:    TOS POP,       \ DROP n1, refill TOS
        NEXT,
ENDCODE

CODE UMIN     ( n1 n2 -- n) \ used by ACCEPT
        *SP TOS CMP,
        1 $ JL,             \ unsigned comparisons
        SP INCT,
        NEXT,
+CODE UMAX   ( n1 n2 -- n)
        *SP  TOS CMP,
        1 $ JH,
        SP INCT,
        NEXT,
1 $:    TOS POP,
        NEXT,
ENDCODE
