\ misc.fth  for recompiler 

: RECURSE     ( -- ) LATEST @ NFA>CFA , ; IMMEDIATE
: DECIMAL     ( -- ) 0A BASE ! ;
: HEX         ( -- ) 10 BASE ! ;
