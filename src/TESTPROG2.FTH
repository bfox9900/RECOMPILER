\ TEST PROGRAM 1

COMPILER 
NEW
\ we MUST load Executors first
\ EXIT DOCON DOVAR DOCOL ?BRANCH and BRANCH
INCLUDE DSK7.EXECUTORS  

\ Now we can make the rest of the compiler
INCLUDE DSK7.BRANCHING   \ compilers for: IF THEN BEGIN AGAIN...
INCLUDE DSK7.DATATYPES   \ CONSTANT VARIABLE : ;

COMPILER HEX 

\ ..........
TARGET ALSO FORTH  \
IMPORT:  DUP  1-  0= 

PREVIOUS \ alternate way to remove Forth Voc. from search order 
: TEST2   -1 BEGIN  1- DUP 0= UNTIL ;
