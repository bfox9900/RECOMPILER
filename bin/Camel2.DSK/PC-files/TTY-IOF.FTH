\ TTY-IO.HSF   RS232/1 DRIVER with H/W handshake         19MAY2023 B Fox

\ These routines push the value in R12 onto the return stack
\ then restore it when returning to Forth.
\ This supports accessing other I/O devices while using the serial port.

\ Feb 2024: updated to support vectored I/O via 'KEY and 'EMIT 

\ for reference...
[CC] HEX [TC]
\    1300 CONSTANT RS232/1    \ RS232/1 card address
\    1500 CONSTANT RS232/2    \ RS232/2 card address
\      40 CONSTANT TTY1       \ 40 = uart#1
\      80 CONSTANT TTY2       \ 80 = uart#2

\ card control bits. CRU base (R12) set to 1340/1380
\       5 EQU -CTS    \ "negative" CTS OUTPUT (low=clear to send)
\       7 EQU LED     \ CARD LED on/off 


\ 9902 UART control bits. CRU base (R12) set to 1340/1380
[CC] DECIMAL
       13 EQU LDIR     \ "load interval register"
\      16 EQU RTS      \ request to send
\      18 EQU RIENB    \ rcv interrupt enable
\      21 EQU RXRL     \ receive register loaded bit
\      22 EQU TXRE     \ transmit register empty bit
\      27 EQU -DSR     \ NOT data set ready
\      28 EQU CTS      \ clear to send INPUT 
       31 EQU RESET    \ 9902 reset bit

[CC] HEX

TARGET-COMPILING
\ these variables are the data needed for 1 comm UART connection
VARIABLE CARD   \ CRU address of rs232 card. Default to >1300
VARIABLE UART   \ /tty1 = >40  /tty2 = ?80
VARIABLE BPS    \ 0034 BPS T!   \ 9600 baud
VARIABLE PROTO  \ 9300 PROTO T! = 8 bits, no parity, 1 stops


\ 9900 sub-routines. *NOT* Forth words.
l: CARD-ON
        CARD @@ R12 MOV,    \ select the card
        7 SBO,              \ turn LED on
        RT,

l: CARD-OFF
        CARD @@ R12 MOV,    \ select the card
        7 SBZ,              \ turn LED off
        RT,

\ *variables CARD, UART, BPS and PROTO must be set correctly before using OPEN-TTY*
CODE OPEN-TTY  ( -- ) \ Usage: /TTY1 OPEN-TTY
         R12 RPUSH,
         CARD-ON @@ BL,     \ load the card address
         UART @@ R12 ADD,   \ add 9902 port address
         RESET SBO,         \ reset UART
         PROTO @@ 8 LDCR,   \ set protocol
         LDIR SBZ,          \ disable 9902 timer
         BPS @@ 0C LDCR,    \ set baud rate
         CARD-OFF @@ BL,
         R12 RPOP,          \ restore R12
         NEXT,
         ENDCODE

[CC] DECIMAL [TC]
\ raw output of 1 char with hardware handshake 
\ this word turns on the LED when sending
CODE CEMIT ( c -- )  \ 'com-emit"
        R12 RPUSH,
        CARD-ON @@ BL,
        UART @@ R12 ADD,   \ add UART offset
\   *** DSR handshake  ***
        BEGIN, 27 TB, EQ UNTIL,   \ wait for -DSR=0

\  *** RTS/CTS handshake ***
        16 SBO,        \ set RTS
\       BEGIN, 28 TB, EQ UNTIL,  \ wait for CTS line
\  ******************************
        BEGIN, 22 TB, EQ UNTIL,   \ wait XBRE empty
        TOS SWPB,      \ put byte on the other side
        TOS 8 LDCR,    \ send 8 bits
        16 SBZ,        \ reset RTS
        CARD-OFF @@ BL,
        R12 RPOP,
        TOS POP,
        NEXT,
        ENDCODE

[CC] DECIMAL [TC]
CODE CKEY? ( -- n ?)         \  "com-key"
        0 LIMI,
        R12 RPUSH,           \ save R12 on return stack
        CARD @@ R12 MOV,     \ set base address of CARD
        TOS PUSH,            \ give us a new TOS register (R4)
        TOS CLR,             \ erase it
        TOS PUSH,            \ dup the zero 

\  *** handshake hardware ON ***
        5 SBZ,               \ CARD CTS line LOW. You are clear to send
        UART @@ R12 ADD,     \ add UART, >1300+40 = CRU address
        21 TB,               \ test if char ready
        EQ IF,
            1 (SP) 8 STCR,   \ read the char into right side of NOS 
            18 SBZ,          \ reset 9902 rcv buffer
            TOS SETO,   
        ENDIF,
\  *** handshake hardware off ***
        CARD @@ R12 MOV,     \ select card
        5 SBO,               \ CTS line HIGH. I am busy!
\  ******************************
        R12 RPOP,            \ restore old R12
        2 LIMI,
        NEXT,
        ENDCODE

\ primary character output word 
: EMIT   ( c -- ) PAUSE CEMIT  VCOL 1+!  OUT 1+! ;  

[CC] HEX 
TARGET-COMPILING 
: C/L!     ( c/l -- )  \ pronounced "SEE-PER-ELL-STORE"
           DUP C/L !           \ set chars per line
           L/SCR *  C/SCR ! ;  \ calc. chars per screen

: CR   ( -- ?)   \ comm port carriage return
       0D CEMIT 0A CEMIT
       VCOL OFF  OUT OFF 
       VROW @ 1+  17 MIN VROW !  ; \ don't count past last line

\ (type) does not update vcol & out for speed
: (TYPE)  ( addr cnt --)  BOUNDS ?DO  I C@ CEMIT LOOP ;

: TYPE   PAUSE  TUCK (TYPE)  DUP VCOL +!  OUT +! ; 
T' TYPE  RESOLVES 'TYPE

: SPACE  ( -- )   BL EMIT ;
: SPACES ( n -- ) 0 MAX  0 ?DO  SPACE  LOOP ;

[CC] CR .( VT100 terminal control, [0,0] Home coordinates  May 2020 )
DECIMAL

TARGET-COMPILING
\ *** use (TYPE) to transmit control sequences so VCOL & OUT are not changed. 
\ type 'n' as a two digit number in base 10, with no space
: <##>   ( n -- )
         BASE @ >R                     \ save radix
         0 <#  DECIMAL # #  #> (TYPE)  \ convert to 2 digits & print
         R> BASE ! ;                   \ restore radix

\ markup language for terminal control codes
 : <ESC>[  ( -- )   27 EMIT  91 EMIT  ;
 : <HOME>  ( -- )   <ESC>[ S" H" (TYPE)  0 0 VROW 2! ;

\ : <UP>    ( n -- ) <ESC>[ <##> S" A" (TYPE) ;
\ : <DOWN>  ( n -- ) <ESC>[ <##> S" B" (TYPE) ;
\ : <RIGHT> ( n -- ) <ESC>[ <##> S" C" (TYPE) ;
\ : <BACK>  ( n -- ) <ESC>[ <##> S" D" (TYPE) ;

\ define Forth words using markup words
: PAGE    ( n -- ) <ESC>[ S" 2J" (TYPE)  <HOME> ;

: AT-XY   ( col row --)
    2DUP 
    <ESC>[ 1+ <##> ." ;" 1+ <##> ." f" 
    VROW 2! ;  \ !! store col,row AFTER sending the control codes

[CC] HEX [TC]

\ RS232 setup commands
\ Usage:
\ RS232 /TTY1 BA=9600 8,N,1 OPEN-TTY

: RS232    ( -- ) 1300  CARD ! ;   \ primary card
\ : RS232/2  ( -- ) 1500  CARD ! ;

: /TTY1    ( -- ) 040  UART ! ;
\ : /TTY2    ( -- ) 080  UART ! ;

\ : BA=2400        00D0 BPS ! ;
: BA=4800  ( -- )  0068 BPS ! ;
\ : BA=9600  ( -- )  0034 BPS ! ;
\ : BA=19200 ( -- )  001A BPS ! ;

: 8,N,1    ( -- ) 9300 PROTO ! ;

TARGET-COMPILING
: KEY  ( -- char)  
     BEGIN 
       CKEY? 0= 
     WHILE 
        PAUSE 
        DROP 
     REPEAT ;  

\ High level: input/output          (c) 31mar95 bjr
: ACCEPT     ( caddr +n -- +n')
    OVER + OVER         \ removed 1-  to accept all chars
    BEGIN
        KEY DUP 0D <>
    WHILE
        DUP EMIT
        DUP 8 =
        IF   DROP 1-  3 PICK  UMAX  \ changed to use: 3 PICK   B.F.
        ELSE OVER C!  1+ OVER UMIN
        THEN
    REPEAT
    DROP NIP SWAP -  ;

: ?TERMINAL ( -- ?) CKEY? AND 3 = ; 

CROSS-COMPILING
