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
            *SP TOS CMP,    \ compare n1 & n2
            1 $ JLT,       \ if n1 < n2 goto 1 $:
            3 $ JMP,
+CODE MAX   ( n1 n2 -- n)
            *SP  TOS CMP,   \ compare n1 & n2
            2 $ JGT,       \ if n1 > n2 goto 1 $:
3 $:        SP INCT,       \ otherwise NIP n1
            NEXT,
1 $: 2 $:   TOS POP,       \ DROP n1, refill TOS
            NEXT,
            ENDCODE

