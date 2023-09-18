COMPILER CR .( KSCAN KEY?  and  ?TERMINAL) 

\ Camel99 interface to KSCAN. *Modified 13Jun2020 to save/restore GPL R11
\ Removed R11 save. It's in the ROM code. Jan 2022
\ See: KEY  in the Camel99 Kernel

TARGET 
CODE KEY? ( -- ?)  \ *WARNING* it takes 1,128uS for KEY? scan to run
         TOS PUSH,
         TOS CLR,            \ TOS will be our true/false flag
         0 LIMI,             \ disable interrupts, ALL VDP routines restore them 
         TOS 837C @@ MOVB,   \ clear GPL flags
         83E0 LWPI,          \ switch to GPL workspace
         000E @@ BL,         \ call ROM keyboard scanning routine
         WRKSP0 LWPI,        \ return to Forth's workspace
         837C @@ R1 MOVB,    \ read GPL status byte (=2000 if key pressed)
         R1  3 SLA,          \ check the key bit
         OC IF,              \ if carry flag set
            8375 @@ TOS MOV, \ read the key
         ENDIF,
\         2 LIMI,            \ I think removing this improves RKEY reliability 
         NEXT,               \ return
         ENDCODE

\ *---------------------------------------------
\ * Call very fast built-in Fctn-4 test in console ROM
\   http://www.unige.ch/medecine/nouspikel/ti99/keyboard.htm
\ *---------------------------------------------
CODE ?TERMINAL ( -- ?)    \ ~200uS, 16 bit ROM speed. Same name as TI-Forth
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
