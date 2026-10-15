\ ckey.fth can receive binary data  Mar 3 2024 Fox 

NEEDS MOV,  FROM DSK1.ASM9900
NEEDS DUMP  FROM DSK1.TOOLS 

DECIMAL
\ changed to comply with Forth 2012.  
\ CKEY? only returns a flag. 
\ Character is stuffed into a memory buffer 
VARIABLE KBUFF               \ single byte buffer for key 
\ changed CKEY? to take a buffer argument 
CODE CKEY? ( buffer -- ?)         
        0 LIMI,
        R12 RPUSH,           \ save R12 on return stack  *Needed?*
        CARD @@ R12 MOV,     \ set base address of CARD
        UART @@ R12 ADD,     \ add UART, >1300+40 = CRU address
        TOS W MOV, 
        TOS CLR, 
\ *** handshake hardware ON ***
\ use negative CRU address to reach back to CARD base address >1300
        -27 SBZ,             \ CARD CTS line LOW. You are clear to send
        21 TB,               \ test if char ready
        EQ IF,
            *W 8 STCR,       \ read the char into 
            TOS SETO, 
        ENDIF,
\  *** handshake hardware off ***
        -27 SBO,             \ CTS line HIGH. I am busy!
        18 SBZ,              \ reset 9902 rcv buffer
        R12 RPOP,            \ restore old R12  *Needed?*
        2 LIMI,
        NEXT,
        ENDCODE

: CKEY  ( -- char)  BEGIN KBUFF CKEY? UNTIL  KBUFF C@ ;
HEX 
: SPEEDTEST 
    2000 H ! 
    BEGIN 
       CKEY DUP EMIT
       3 = 
    UNTIL ;    


DECIMAL
\ *NEW* TKEY waits for a time arg, return true/false if data is received
\ DATA is store in KBUFF 
: TKEY? ( wait-time -- ?)  \ 1000 ~= 1000mS on TI-99
\ waits for a key until counter hits zero
      BEGIN 
          1-    \ decrement wait-time
          DUP 
       WHILE ( wait-time > 0 )
          KBUFF CKEY?
          ?DUP 
       UNTIL 
       THEN 
;

: STRAIGHT ( addr len -- addr len)
        BOUNDS TUCK  ( -- start end start )
        CKEY OVER C! 1+  \ wait for 1st key, store & bump address
        DO   
           1000 TKEY? 0= IF LEAVE THEN
           KBUFF C@ I C! 
        LOOP ;

