\ DATATYPES.FTH for recompiler Oct 2024

COMPILER 

: ?RUNTARGET  STATE @ ABORT" Can't run TARGET words" ;

\ resolve TLITERAL 
: (TLITERAL) ( n -- n|~) 
\ tcompile searches the TARGETS vocab first. 
\ ** IF LIT NOT DEFINED THIS WILL COMPILE THE WRONG LIT **
    STATE @ IF  TCOMPILE LIT   T,  THEN ; IMMEDIATE

COMPILER     
' (TLITERAL) IS TLITERAL 

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
    DOES> ?RUNTARGET @ >BODY T@ ; \ returns 'n'  from TARGET memory
\ *REMEMBER* T@ converts SAMS address to real address.    

: (:NONAME) ( -- )  TCOMPILE DOCOL  HIDE  ] ;

\ *NEW*  colon now switches to TARGET search order automagically 
\        semi-colon  swithes back to COMPILER search order.  

HOST: :   !CSP TCREATE  (:NONAME)  ;HOST
HOST: :NONAME  THERE  !CSP  (:NONAME)  ;HOST

\ This requires comments :-)
HOST: ;    
    REVEAL            \ make this word visible to FIND 
\ *note* EXIT must be defined in the TARGETS vocabylary     
    TCOMPILE EXIT     \ compile EXIT in the TARGET word
 \   ?CSP
    STATE OFF        \ turn the Forth compiler off 
                     ( mystery:  [  does not work here ??? )
;HOST  IMMEDIATE 

