\ warmboot.fth  for the recompiler  Sept 18 2023  Fox

TARGET 
 
 : .VER   ." Recompiled Sept2023" ;

\ *G  WARM initializes variables and vectors
: WARM  ( -- )
        80 83C2 C!     ( enable Interupts, disable QUIT )
        26 TPAD !
        1000 VP !      ( start of free VDP RAM )
        VDPTOP ^PAB !  ( init the PAB stack )
        3FFF TMR!      ( init the 9901 timer )
        L0 LP !        ( init the LEAVE stack for DO/LOOP) 
        FLOOR ON       ( floored division is default)
        DECIMAL
        A000 @ DP !    ( dictionary set to HI RAM )
;

\ The simplest program must have a loop. There is no INTERPRETER!
: MAIN   
    WARM  
    TEXT  S" Forth99 " TYPE .VER
    BEGIN  
      ?TERMINAL 
    UNTIL  
    BYE ;


