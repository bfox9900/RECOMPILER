

: VFILLTEST  58 0 DO   0 C/SCR @ I BL + VFILL  LOOP ;


: MAIN  ( -- ) 
   VFILLTEST 
   PAGE
   BEGIN 
     S"   VFILL in aNS-Forth. " TYPE  
     S"   FOR/NEXT loops and EMIT+ is CODE" TYPE 
    ?TERMINAL 
   UNTIL
;
