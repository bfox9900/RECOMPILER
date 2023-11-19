\ DATATYPES.FTH for recompiler Sep 2023 
COMPILER 
: ?RUNTARGET  STATE @ ABORT" Can't run TARGET words" ;

: TALIGN    HERE ALIGNED H ! ;

: TS,       ( c-addr u -- ) THERE OVER 1+ TALLOT PLACE  TALIGN ;
HEX 
: >NAME ( nfa -- addr len)  COUNT 1F AND ;
DECIMAL 

\ CREATES A CAMEL FORTH HEADER
: THEADER, ( addr len --)
      ALIGN
      CURRENT @ @ T,        \ get last NFA & compile in this LFA field
      0 TC,                 \ compile the precedence byte (immediate flag)
      HERE >R               \ save HERE (ie: new NFA location)
      TS,                   \ compile (addr len) as counted string
      WARNINGS @
      IF
        R@ FIND ( xt ?) NIP ( ?)
        IF
          SPACE  R@ COUNT 1F AND TYPE  ."  isn't unique "
        THEN
      THEN
      R>   DUP LATEST !     \ HERE is NFA of last word defined
            CURRENT @ !     \ Also store in the current 'WID'
;

: THEADER ( <TEXT> )  BL PARSE-WORD HEADER, ;

COMPILER
\ TCREATE makes a dictionary header in HOST Forth,
\ and records the TARGET XT address ie: where the code was compiled
: TCREATE  
    CREATE THERE ,          \ create the name in HOST forth 
    LATEST @ >NAME THEADER, \ create the name in the TARGET Forth 
    DOVAR T,                \ compile the executor for a variable 
;



\ resolve TLITERAL 
:NONAME ( n -- n|~) STATE @ IF  TCOMPILE LIT T,  THEN ; IS TLITERAL 

: +CODE     !CSP              \ used to allow jumping between CODE/ENDCODE
            TCREATE
            THERE 2+ T,
            DOES> ?RUN-TARGET ;


: CODE      NEW-JMPTABLE  +CODE ;

: ENDCODE   ?CSP ;


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
 \   ?CSP
    STATE OFF        \ turn the Forth compiler off 
                     ( mystery:  [  does not work here ??? )
;HOST  IMMEDIATE 

