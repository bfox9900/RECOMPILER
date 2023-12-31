\ DATATYPES.FTH for recompiler Sep 2023 

HOST
: ?RUNTARGET  STATE @ ABORT" Can't run TARGET words" ;

COMPILER
\ resolve TLITERAL 
:NONAME ( n -- n|~) 
    STATE @ IF  TCOMPILE LIT   T,  THEN ; IS TLITERAL 


\ TCREATE makes a dictionary header in HOST Forth,
\ and records the TARGET XT address ie: where the code was compiled
: TCREATE  CREATE  THERE , ;

\ DTC code is different. There is no CFA.
: +CODE  
    CREATE              \  name is in HOST Forth      
      THERE ,           \ and remember where the word is in TARGET 
      !CSP 
    DOES> STATE @ 
          IF  TRUE ABORT" Can't run TARGET code word"   
          THEN  @ ;

: CODE    ( <name> ) NEWLABELS +CODE ;  \ start new label scope 

\ NOW we can compile the addresses of the EXECUTORS and primitives 

\ In compile mode these words compile 4 bytes in TARGET memory
\ In Interpret mode they work like normal VARIABLE.  
: VARIABLE         
    TCREATE                     \ name in Forth & record TARGET XT address 
    DOVAR  @@ BL,   0 T,        \ compile BL to DOVAR and init memory location   
    DOES> ?RUNTARGET @ >BODY ; \ returns DATA address 

: CONSTANT ( n --) 
    TCREATE  
    DOCON @@ BL ( n) T,  
    DOES> ?RUNTARGET @ >BODY @ ; \ returns 'n'  in TARGET memory

: (:NONAME) ( -- )  DOCOL @@ BL  HIDE ] ;

HOST: :    !CSP TCREATE  (:NONAME)  ;HOST
HOST: :NONAME  THERE  !CSP  (:NONAME)  ;HOST

\ This requires comments :-)
HOST: ;    
    REVEAL            \ make this word visible to FIND 
    TCOMPILE EXIT     \ compile EXIT in the TARGET word
 \   ?CSP
    STATE OFF        \ turn the Forth compiler off 
                     ( mystery:  [  does not work here ??? )
;HOST  IMMEDIATE 

