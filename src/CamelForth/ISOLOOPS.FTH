\ ANS/ISO  T A R G E T   B R A N C H   C O M P I L E R S   Aug 18 2021 B Fox

\ These versions RUN IN THE TARGET system to compile loops
\ and branches in the TI-99.
\ These versions do not use compile-time checking like Fig-Forth.
\ This improves compile time speed

COMPILER HEX

TARGET
\ THEN resolves forward branches
\ computes offset: (HERE -  addr)  & stores result in addr
\ *note* IMMEDIATE in TARGET context is a META word that affects Target words
: THEN  ( addr -- )  HERE OVER - SWAP ! ; XIMMEDIATE
: AHEAD    ( -- addr)   HERE 0 , ;  \ compile empty jump forward


\ CAMEL Forth LEAVE stack pointer is called LP. It is initialized by QUIT
\ : >L        ( x -- ) ( L: -- x ) 2 LP +!   LP @ ! ;     \ LP stack grows up
\ : L>        ( -- x ) ( L: x -- ) LP @ @  -2 LP +! ;
\ code is same size but faster
 CODE >L
    R1 STWP,
    3A (R1) INCT,
    3A (R1) R0 MOV,
    TOS *R0 MOV,
    TOS POP,
    NEXT,
 ENDCODE

 CODE L>
     TOS PUSH,
     R1 STWP,
     3A (R1) TOS MOV,
     *TOS TOS MOV,
     3A (R1) DECT,
     NEXT,
 ENDCODE

\ -----------------------------------------------------------------------
\ RAKE  resolves branching out of DO/LOOPS that use LEAVE
\       CAMEL Forth calls this ENDLOOP... but it RAKEs the LEAVEs. Get it?

\ NOTE: We MUST put RAKE here because it uses the cross-compiler's
\       WHILE/REPEAT loop. We cannot CROSS COMPILE loops in our code AFTER we
\       define the TARGET system LOOPS below.

: RAKE  ( -- ) ( L: 0 a1 a2 .. aN -- )
      BEGIN  L> ?DUP WHILE    T[COMPILE] THEN  REPEAT ;

\ -----------------------------------------------------------------------
\ SIMPLIFIED LOOPS
\ Posted by dxforth comp.lang.forth, modified for FXCC cross-compiler
: <BACK   HERE - , ;
: BEGIN   HERE ;                                XIMMEDIATE
: IF      COMPILE ?BRANCH AHEAD ;               XIMMEDIATE
: ELSE    COMPILE BRANCH AHEAD SWAP T[COMPILE] THEN ; XIMMEDIATE
: AGAIN   COMPILE BRANCH  <BACK  ;              XIMMEDIATE
: UNTIL   COMPILE ?BRANCH <BACK  ;              XIMMEDIATE
: WHILE   T[COMPILE] IF SWAP ;                  XIMMEDIATE
: REPEAT  T[COMPILE] AGAIN T[COMPILE] THEN ;    XIMMEDIATE

\ ========================================================================
\ DO/LOOP  COMPILERS
\                          -compile this-    - run this now-
: DO        ( n n -- adr) ?COMP COMPILE <DO>   0 >L  HERE  ; XIMMEDIATE
: ?DO       ( n n -- adr) ?COMP COMPILE <?DO>  0 >L  HERE  ; XIMMEDIATE
: LEAVE     ( -- ) COMPILE UNLOOP  COMPILE BRANCH AHEAD >L ; XIMMEDIATE

\ complete a DO loop
: LOOP      ( -- )  COMPILE <LOOP>  HERE - ,    RAKE ; XIMMEDIATE
: +LOOP     ( -- )  COMPILE <+LOOP> HERE - ,    RAKE ; XIMMEDIATE
