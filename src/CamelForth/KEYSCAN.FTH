COMPILER  CR .( Console input)
HEX 

TARGET
CODE KEY? ( -- char)  \ *WARNING* it takes 1,128uS for KEY? scan to run
        TOS PUSH,
        TOS CLR,            \ TOS will be our true/false flag
        0 LIMI,             \ disable interrupts
        TOS 837C @@ MOVB,   \ clear GPL flags
        83E0 LWPI,          \ swiTCh to GPL workspace
        000E @@ BL,         \ call ROM keyboard scanning routine
        8300 LWPI,          \ return to Forth's workspace
        837C @@ R1 MOVB,    \ read GPL status byte (=2000 if key pressed)
        R1  3 SLA,          \ check the key bit
        OC IF,              \ if carry flag set
           8375 @@ TOS MOV, \ read the key ( LEGACY reasons)
        ENDIF,
        2 LIMI, 
        NEXT,          
        ENDCODE

\ * Very fast built-in Fctn-4 test in console ROM
\   http://www.unige.ch/medecine/nouspikel/ti99/keyboard.htm
CODE ?TERMINAL ( -- ?)  \ ~200uS, 16 bit ROM code. Same name as TI-Forth
        R12     RPUSH,       \ save CRU address makes life simpler
        TOS     PUSH,
        TOS     CLR,
        0020 @@ BL,          \ Call ROM code, test FNCT 4 key press
        EQ IF,               \ if flag=0
           TOS  SETO,        \ KEY is pressed, TOS = -1
           BEGIN,
              0020 @@ BL,    \ call rom routine
           NE UNTIL,         \ until key is released
        ENDIF,
        R12     RPOP,        \ Restore CRU address used by CAMEL Forth
        NEXT,
        ENDCODE

