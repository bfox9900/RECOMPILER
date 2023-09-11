\ STC Forth CROSS COMPILER with inline optimizing 

NEEDS CASE FROM DSK1.CASE
NEEDS DUMP  FROM DSK1.TOOLS 
NEEDS WORDLIST FROM DSK1.WORDLISTS 

VOCABULARY ASSEMBLER 


ONLY FORTH ALSO ASSEMBLER DEFINITIONS
NEEDS MOV,  FROM DSK1.XASM9900

ONLY FORTH ALSO ASSEMBLER ALSO FORTH DEFINITIONS 

HERE

HEX 
045A CONSTANT 'NEXT'  \ 9900 CODE for B *R10   Camel99 Forth's NEXT code

: NEW ( -- ) 2000 2000 0 FILL   2000 TDP ! ; \ reset HEAP

: CODE,  ( xt --)  \ Read code word from kernel, compile into target memory
    >BODY 80 CELLS  ( -- addr len)
    BOUNDS ( -- IPend IPstart)
    BEGIN
      DUP @ 'NEXT' <>  \ the instruction is not 'RET'
    WHILE
      DUP @        ( -- IP instruction)
      T,           \ compile instruction
      CELL+        \ advance IP
      2DUP < ABORT" End of code not found"
    REPEAT
    2DROP
;

: INLINE?   ( prec -- ?)  CFA>PREC C@ %INLINE AND  0> ;

\ smart compiler: does a  call or inlines code 
: COMPILE,  ( cfa --) 
    DUP  INLINE? 
    IF    CODE,    \ copy code from kernel inline 
    ELSE  @@ BL,   \ compile a sub-routine call
    THEN 
;



\ compile code to push a number onto Forth data stack 
: LITERAL,   STATE @ IF TOS PUSH,  TOS SWAP LI,  THEN  ;  

: <INTERPSTC>  ( c-addr u -- )
    'SOURCE 2!  >IN OFF
    BEGIN
        BL WORD
        DUP C@  ( -- addr len)
    WHILE
        FIND ?DUP
        IF ( it's a word)
            1+ STATE @ 0= OR
            IF   EXECUTE
            ELSE COMPILE,
            THEN
        ELSE ( it's a number)
            COUNT NUMBER? ?ERR
            LITERAL,  \ compile code to push literal onto Forth stack 
        THEN
        DEPTH 0< ABORT" Short stack"
    REPEAT
    DROP ;

: FORTH-COMPILE   ['] <INTERP> 'IV ! ; IMMEDIATE 

: RECOMPILE       ['] <INTERPSTC>  'IV ! ; 

\ HOST system compiler are just aliases 
: ;H  POSTPONE ; ;  IMMEDIATE 
: H:  : ;  

VOCABULARY TARGETS

\ RE-COMPILER Target system compiler  
H: :   RECOMPILE  H:  ;H
H: ;   FORTH-COMPILE POSTPONE ;  ;H  IMMEDIATE 

\ create primitives with a name in TARGETS but they copy
\ code from the kernel into the target inline. 
\ 

\ CFA of these words contains the XT
\        Target name  inline flag   code location 
: INLINE: HEADER      TRUE ,        '  ?CODE  ,   ; 



\ -----------------------------------------------------------------
\ SUB:  makes run-time code routines, that reside in the target 
\       by copying code from the HOST Forth into the target image
\       Typically used for larger primitives like MOVE, FILL, etc. 

\ ***********************************************************
\ Sub-routine threaded words have a 2 instruction 'enter' 
\ and a 2 instruction 'return'

\ ENTER a word 
\   0647  dect R7          ( make room on Rstack)         
\   C5CB  mov  R11,*R7     ( save the linkage register )          

\  <code goes here>

\ 'RET'  return from sub-routine 
\   C2F7  mov  *R7+,R11    ( pop R into link register)          
\   045B  b    *R11        
\ ***********************************************************

HEX
: SUB: 
\         inline field    address in the target 
    CREATE FALSE ,        THERE ,  

    R11 RPUSH,      \ compile code to save R11
    '  ?CODE CODE,  \ test if code word, compile code from kernel
    DOES> 
;     

: ;SUB    
    R11 RPOP,     \ compile code to restore R11 
    RT,           \ B *R11 is return 
;

NEW
