
\ Add S"  string compiler Yikes!
 COMPILER ALSO META DEFINITIONS ( META holds "immediate" words and support )
 HOST: TALIGN     ( -- )   THERE ALIGNED H ! ;HOST
 HOST: TS,       ( c-addr u -- ) THERE OVER 1+ TALLOT PLACE  TALIGN ;HOST
 
COMPILER WARNINGS OFF
  INCLUDE DSK7.PRIMITIVES \ TARGET code words
COMPILER WARNINGS ON
 
COMPILER DECIMAL 
TARGET
\ run-time for S" 
\ 1st colon definition in the system. (could be a code word)
: (S")  ( -- c-addr u) R>  COUNT  2DUP + ALIGNED >R ; 

\ META-compiling S" 
COMPILER ALSO META DEFINITIONS 
HOST: S"   
   [CHAR] " PARSE  
   TCOMPILE (S")  TS, 
;HOST IMMEDIATE 


CR .( ITC FORTH Preamble loaded )
