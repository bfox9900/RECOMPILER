\ ANS/ISO  T A R G E T   B R A N C H   C O M P I L E R S   Aug 18 2021 B Fox

COMPILER HEX

TARGET
\ THEN resolves forward branches
\ computes offset: (HERE -  addr)  & stores result in addr
: THEN  ( addr -- )  THERE OVER - SWAP T! ;  IMMEDIATE

: AHEAD    ( -- addr)  THERE 0 T, ;  \ compile empty jump forward


\ CAMEL Forth LEAVE stack pointer is called LP. It is initialized by QUIT
\ SLOWER [if]
: >L        ( x -- ) ( L: -- x ) 2 LP +!   LP @ ! ;     \ LP stack grows up
: L>        ( -- x ) ( L: x -- ) LP @ @  -2 LP +! ;

\ -----------------------------------------------------------------------
\ RAKE  resolves branching out of DO/LOOPS that use LEAVE
\       CAMEL Forth calls this ENDLOOP... but it RAKEs the LEAVEs. Get it?

\ NOTE: We MUST put RAKE here because it uses the cross-compiler's
\       WHILE/REPEAT loop. We cannot CROSS COMPILE loops in our code AFTER we
\       define the TARGET system LOOPS below.

: RAKE  ( -- ) ( L: 0 a1 a2 .. aN -- )
         BEGIN  L> ?DUP WHILE   TPOSTPONE THEN  REPEAT ;
\ -----------------------------------------------------------------------
\ SIMPLIFIED LOOPS
\ Posted by dxforth comp.lang.forth
\ Modded for FCC99 cross-compiler that has no POSTPONE

: BEGIN   HERE ;                                IMMEDIATE
: IF      TPOSTPONE ?BRANCH AHEAD ;               IMMEDIATE
: ELSE    TPOSTPONE BRANCH AHEAD SWAP TPOSTPONE THEN ; IMMEDIATE
: AGAIN   TPOSTPONE BRANCH THERE - T,  ;          IMMEDIATE
: UNTIL   TPOSTPONE ?BRANCH THERE - T,  ;         IMMEDIATE
: WHILE   TPOSTPONE IF SWAP ;                   IMMEDIATE
: REPEAT  TPOSTPONE AGAIN TPOSTPONE THEN ;      IMMEDIATE

\ ========================================================================
\ DO/LOOP  COMPILERS
\                          -compile this-    - run this now-
: DO        ( n n -- adr) ?COMP TPOSTPONE <DO>   0 >L  HERE  ; IMMEDIATE
: ?DO       ( n n -- adr) ?COMP TPOSTPONE <?DO>  0 >L  HERE  ; IMMEDIATE
: LEAVE     ( -- ) TPOSTPONE UNLOOP  TPOSTPONE BRANCH AHEAD >L ; IMMEDIATE

\ complete a DO loop
: LOOP      ( -- )  COMPILE <LOOP>  THERE - T,    RAKE ; IMMEDIATE
: +LOOP     ( -- )  COMPILE <+LOOP> THERE - T,    RAKE ; IMMEDIATE
