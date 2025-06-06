\ TICKTOCK.HSF  TMS9901 hardware timer interface for Camel 99 Forth

\ credit to: http://www.unige.ch/medecine/nouspikel/ti99/tms9901.htm#Timer
\ improvements based on code from Tursi Atariage
\ TMR! now loads from the Forth stack
\ Apr 2023  Went back to a new TICKS word that is more efficient.
\           MS min= 32 milliseconds

\ timer resolution:  64 clock periods, thus 64*333 = 21.3 microseconds
\ Max Duration    :  ($3FFF) 16383 *64*333 ns = 349.2 milliseconds

COMPILER DECIMAL

TARGET
CODE TMR!   ( n -- )         \ load TMS9901 timer from stack
             0 LIMI,
             R12 CLR,        \ CRU addr of TMS9901 = 0
             0   SBO,        \ SET bit 0 to 1, Enter timer mode
             R12 INCT,       \ CRU Address of bit 1 = 2 , I'm not kidding
             TOS 14 LDCR,    \ Load 14 BITs from TOS into timer
            -1  SBZ,         \ reset bit 0, Exit clock mode, start decrementer
             2 LIMI,
             TOS POP,
             NEXT,
             ENDCODE

CODE TMR@   ( -- n)         \ read the TMS9901 timer
             0 LIMI,
             TOS PUSH,
             R12 2 LI,      \ cru = 1 (honest, 2=1)
            -1 SBO,         \ SET bit 0 TO 1, Enter timer mode
             TOS 14 STCR,   \ READ TIMER (14 bits)
            -1 SBZ,         \ RESET bit 1, exit timer mode
             2 LIMI,
             NEXT,
             ENDCODE

CODE DT ( T1 T2 -- n)
 *SP TOS SUB,  \ t1-t2
     TOS ABS,
 *RP TOS SUB,  \ subtract from value on Return stack
  NEXT,
ENDCODE

: TICKS ( n -- ) \ ** n(max) = 4000 ~= 100 ms **
  >R
  TMR@
  BEGIN
    TMR@ DT 0<
  WHILE
    PAUSE
  REPEAT
  R> 2DROP
;

\ 1500 TICKS ~= to 32mS. Miniumum time is 32mS
: MS ( n -- ) 5 RSHIFT 1 MAX 0 DO  1500 TICKS  LOOP ;

COMPILER HEX
