\ PARSE-NAME  from https://forth-standard.org/standard/core/PARSE-NAME

: white?  ( c -- f )  BL 1+ U< ; \ space and below are white chars
: -white? ( c -- f ) white? 0= ; \ everything above are 

: xt-skip ( addr1 n1 xt -- addr2 n2 )
 \ skip all characters satisfying xt ( c -- f )
   >R
   BEGIN
     DUP
   WHILE
     OVER C@ R@ EXECUTE
   WHILE
     1 /STRING
   REPEAT THEN
   R> DROP ;

: PARSE-NAME ( "name" -- c-addr u )
   SOURCE >IN @ /STRING
   [']  white? xt-skip OVER >R
   ['] -white? xt-skip ( -- end-word restlen) ( r: start-word )
   2DUP 1 MIN + SOURCE DROP - >IN !
   DROP R> TUCK - ;

