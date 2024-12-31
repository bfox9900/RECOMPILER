\ this is the CROSS-compiler's colon/semi-colon pair 

COMPILER ALSO META DEFINITIONS 
HOST: :                 
   !CSP                \ record the "current stack position" (used later)
   TCREATE             \ create headers 
   TCOMPILE DOCOL      \ compile the "executor" in the target 
   HIDE                \ HIDE this word in TARGETS when compiling 
   ]                   \ turn on the compiler
   DOES> TRUE ABORT" Can't run TARGET words" ;      
;HOST

\ CROSS-compiler's ';'
HOST: ;                    
   REVEAL               \ so it can be found in the dictionary
   TCOMPILE EXIT 
   ?CSP                 \ check is stack is clean
   [                    \ turn off the compiler 
;HOST   IMMEDIATE

COMPILER 
CR .( ITC foundation loaded)
