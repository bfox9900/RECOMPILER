
INCLUDE DSK1.TOOLS

: SCROLL 
    TOPLN C/SCR @   ( Vdst len)
    BOUNDS          ( end start)
    BEGIN 
        2DUP > 
    WHILE 
        DUP 
        C/L@ + HERE ( src buff) C/L@ 8* VREAD  
        HERE   OVER ( buff dst) C/L@ 8* VWRITE 
        C/L@ 8* +     \ this line controls no. lines moved
    REPEAT   
    2DROP
    0 23 2DUP >VPOS C/L@ BL VFILL 
    AT-XY 
;


: TEST 24 0 DO SCROLL LOOP ;