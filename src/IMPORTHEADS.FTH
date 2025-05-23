\ importHEADS.fth    ** FOR ITC FORTH ONLY ** 
 
\ recompile a threaded Forth primitives in low RAM
\ Dictionary headers are kept in the regular Forth dictionary

\ *BUT*  this version also compiles a header in the target memory.
\ In theory we could change the dictionarys structure. 

NEEDS TCREATE FROM DSK1.ITCHEADERS 


ONLY FORTH ALSO ASSEMBLER ALSO COMPILERS DEFINITIONS 
 
HEX
045A CONSTANT 'NEXT'  \ 9900 CODE for B *R10   Camel99 Forth's NEXT code
 
: TNEXT,    'NEXT'  T, ;
 
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
: CREATE, ( addr len -- ) HEADER,  POSTPONE DOVAR ;

\ ========================================================================
\                  >>> WORKS FOR ITC CODE ONLY <<<< 
\ IMPORT, finds a code word in the kernel and copies the code into TARGET
\ It creates a CONSTANT in the Forth dictioinary that returns the target
\ execution token.

 
:  ITC-IMPORT,  ( addr len -- )
    PAD PLACE    \ save the name in PAD
    PAD FIND 0= ABORT" IMPORT, can't find"
    ( xt) DUP ?CODE
\ CREATE/DOES> but with a string argument
    PAD COUNT CREATE,
    THERE ,
    THERE CELL+ T, \ ITC code entry points to next cell  
    TCODE,         \ compile code from Kernel into the target
    TNEXT,         \ append NEXT to the TARGET code
    DOES>
    STATE @ ABORT" Can't run TARGET word"
    @   ( interpret mode, return the address )
;
 
: IMPORT:
    ONLY ASSEMBLER ALSO COMPILERS  ALSO TARGETS DEFINITIONS  ALSO FORTH 
    BEGIN 
      PARSE-NAME 
    DUP WHILE
      IMPORT,
    REPEAT
    2DROP
; 
 
 
 
