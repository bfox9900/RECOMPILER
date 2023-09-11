\ TEST PROGRAM 1

COMPILER 
NEW
\ we MUST load Executors first
\ EXIT DOCON DOVAR DOCOL ?BRANCH and BRANCH
INCLUDE DSK7.EXECUTORS  

\ Now we can make the reset of the compiler
INCLUDE DSK7.BRANCHING   \ compilers for: IF THEN BEGIN AGAIN...
INCLUDE DSK7.DATATYPES   \ CONSTANT VARIABLE : ;

COMPILER HEX 

\ import needs to see Forth to find kernel primitives 
TARGET ALSO FORTH   
IMPORT: +

TARGET   \ now we can't look at Forth vocabuarly
: TEST  2  4  +  ;
