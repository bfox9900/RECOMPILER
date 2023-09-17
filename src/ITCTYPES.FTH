\ DATATYPES.FTH for recompiler Sep 2023 
HOST
: ?RUNTARGET  STATE @ ABORT" Can't run TARGET words" ;

COMPILER
\ resolve TLITERAL ( neeed a place to put this)
:NONAME ( n -- n|~) 
    STATE @ IF  TCOMPILE LIT   T,  THEN ; IS TLITERAL 

\ TCREATE makes a dictionary header in HOST Forth,
\ and records the TARGET XT address ie: where the code was compiled
: TCREATE  CREATE  THERE , ;

\ NOW we can compile the addresses of the EXECUTORS and primitives 

\ In compile mode these words compile 4 bytes in TARGET memory
\ In Interpret mode they work like normal VARIABLE.  
: VARIABLE         
    TCREATE                     \ name in Forth & record TARGET XT address 
    TCOMPILE DOVAR  0 T,        \ compile DOVAR and init memory location   
    DOES> ?RUNTARGET @ >BODY ; \ returns DATA address 

: CONSTANT ( n --) 
    TCREATE  
    TCOMPILE DOCON  ( n) T,  
    DOES> ?RUNTARGET @ >BODY @ ; \ returns 'n'  in TARGET memory

: (:NONAME) ( -- )  TCOMPILE DOCOL  HIDE  ] ;

HOST: :    !CSP TCREATE  (:NONAME)  ;HOST
HOST: :NONAME  THERE  !CSP  (:NONAME)  ;HOST

\ This requires comments :-)
HOST: ;    
    REVEAL            \ make this word visible to FIND 
    TCOMPILE EXIT     \ compile EXIT in the TARGET word
    ?CSP
    STATE OFF        \ turn the Forth compiler off 
                     ( mystery:  [  does not work here ??? )
;HOST  IMMEDIATE 

