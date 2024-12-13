CR .( import.fth  import machine code from kernel to target 2024 Brian Fox)
 
\ recompile a threaded Forth primitives in low RAM
\ Dictionary headers are kept in the regular Forth dictionary

COMPILER 
HEX
045A CONSTANT 'NEXT'  \ 9900 CODE for B *R10   Camel99 Forth's NEXT code
 
: NEXT,    'NEXT'  T, ;
 
\ Read code word from kernel, compile into target memory
: TCODE,  ( xt --)
    >BODY 80 CELLS  ( -- addr len)
    BOUNDS ( -- IPend IPstart)
    BEGIN
      DUP @ 'NEXT' <>  \ the instruction is not 'NEXT'
    WHILE
      DUP @        ( -- IP instruction)
      T,           \ compile instruction
      CELL+        \ advance IP
      2DUP < ABORT" End of code not found"
    REPEAT
    2DROP
;
 
: ?CODE   ( xt --) DUP @ 2- - ABORT" Not a CODE word" ;
 
\ like CREATE but takes the name from a stack string
: TCREATE, ( addr len -- )  TARGET HEADER,  POSTPONE DOVAR   COMPILER ;

\ ========================================================================
\                  >>> WORKS FOR ITC CODE ONLY <<<< 
\ IMPORT, finds a code word in the kernel and copies the code into TARGET
\ It creates a CONSTANT in the Forth dictioinary that returns the target
\ execution token.

\ Oct 29 2024: IMPORT, now does not import a word more than once.
\              This means import: statements can be used where needed

\ test if $word is not in target vocabulary 
: IMPORTED? ( $addr -- ?) 
    ONLY TARGETS FIND NIP   \ search only the TARGETS wordlist 
    COMPILER                \ go back to compiler mode 
; 

:  IMPORT,  ( addr len -- )
    PAD PLACE    \ save the name in PAD
    PAD HOST FIND 0= ABORT" IMPORT, can't find"
    ( xt) DUP ?CODE
    
    PAD IMPORTED? 
    IF  
      DROP 
    ELSE 
    \ CREATE/DOES> but with a string argument
      PAD COUNT TCREATE,
      THERE ,        \ remember address in HOST 

      THERE CELL+ T, \ put ITC code entry in TARGET 
      TCODE,         \ compile code from Kernel into the target
      NEXT,          \ append NEXT to the TARGET code
      DOES>
        STATE @ ABORT" Can't run TARGET word"
        @   ( interpret mode, return the address )
    THEN 
;
 
: IMPORT:
    BEGIN 
     PARSE-NAME DUP 
    WHILE
      IMPORT,
    REPEAT
    2DROP
; 
 
 
 
