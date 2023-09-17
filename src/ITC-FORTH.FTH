\ ITC-SYSTEM.FTH  preamble to make an indirect threaded system  Sep 2023 Fox
 
COMPILER
NEW
 
WARNINGS OFF
INCLUDE DSK7.EXECUTORS  \ load EXIT DOCOL DOVAR ETC.
INCLUDE DSK7.BRANCHING  \ compilers for IF THEN BEGIN AGAIN...
INCLUDE DSK7.ITCTYPES   \ CONSTANT VARIABLE : ;  S"
WARNINGS ON
 
COMPILER HEX
 
TARGET
VARIABLE BOOT  \ hold the cfa of the word that boots from COLD
[
L: COLD               \ COLD runs at boot time to build the Forth VM
      8300 LWPI,      \ set 9900 workspace
      SP 83FE LI,     \ data stack in scratchpad
      RP 83D0 LI,     \ return stack in scratchpad
      IP BOOT LI,     \ load interpreter pointer with boot word
 
      R10 _NEXT LI,   \ inner interpreter stays in R10
      *R10 B,         \ jump into the interpreter
 
COMPILER
COLD 2002 T!          \ COLD will run at startup now
 
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
HOST: S"   [CHAR] " PARSE  TCOMPILE (S")  TS, ;HOST IMMEDIATE 


CR .( ITC FORTH Preamble loaded )
 
 
