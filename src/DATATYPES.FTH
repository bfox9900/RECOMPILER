\ DATATYPES.FTH for recompiler Sep 2023 
COMPILER 
: ?RUNTARGET  STATE @ IF  TRUE ABORT" Can't run TARGET words" THEN ;

\ resolve TLITERAL ( neeed a place to put this)
COMPILER
:NONAME ( n -- n|~) 
    STATE @ IF  TCOMPILE LIT   T,  THEN ; IS TLITERAL 

\ TCREATE makes a dictionary header in HOST Forth,
\ and records the TARGET XT address ie: where the code was compiled
: TCREATE          CREATE  THERE , ;

\ NOW we can compile the addresses of the EXECUTORS and primitives 
COMPILER 
\ In compile mode these words compile 4 bytes in TARGET memory
\ In Interpret mode they return the CFA (XT) in TARGET memory
: VARIABLE         
    TCREATE  TCOMPILE DOVAR  0 T,     
    DOES> ?RUNTARGET @ >BODY @ ;

: CONSTANT ( n --) 
    TCREATE  TCOMPILE DOCON  ( n) T,  
    DOES> ?RUNTARGET @ >BODY @ ;

: (:NONAME) ( -- )  TCOMPILE DOCOL  HIDE  ] ;

HOST: :       !CSP  TCREATE  (:NONAME)  <TINTERP>  ;HOST
HOST: ;        REVEAL TCOMPILE EXIT ?CSP [ ;HOST  IMMEDIATE
HOST: :NONAME  THERE  !CSP  (:NONAME)  ;HOST


