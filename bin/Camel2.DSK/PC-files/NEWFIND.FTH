\ loking for a better find 

\ Put compare in the kernel and use it for finding words. 

\ Original Camel Forth was very slow. 
\ FIND   c-addr -- xt  0   if not found
\                  xt  1      if immediate
\                  xt -1      if "normal"
\   LATEST @ BEGIN             -- a nfa
\       2DUP OVER C@ CHAR+     -- a nfa a nfa n+1
\       N=                     -- a nfa f
\       DUP IF
\           DROP
\           NFA>LFA H@ DUP     -- a link link
\       THEN
\   0= UNTIL                   -- a nfa  OR  a 0

\   DUP IF
\       NIP DUP NFA>CFA        -- nfa xt
\       SWAP IMMED?            -- xt iflag
\       0= 1 OR                -- xt 1/-1
\   THEN ;

INCLUDE DSK1.COMPARE 
INCLUDE DSK1.3RD4TH
HEX
: ?BREAK  ( -- ) ?TERMINAL ABORT" *BREAK*" ;
: SPACEBAR ( -- ) KEY? BL = IF  KEY DROP  THEN ;
: GET-CURRENT ( -- addr) CURRENT @ @ ;

\ Remove wid and xt from the stack. Execute xt once for every word in the
\ wordlist wid, passing the name token nt of the word to xt, until the wordlist
\ is exhausted or until xt returns false.

\ ** This code is not fully compliant because the Name token is held on then
\    DATA stack.

CODE >NFA  ( nfa -- nfa') TOS -3 AI, *TOS TOS MOV,  NEXT, ENDCODE 

CODE DUPR@ 
    TOS PUSH,
    TOS PUSH, 
    *RP TOS MOV, 
    NEXT, 
    ENDCODE 

: TRAVERSE-WORDLIST ( xt wid -- addr )
        SWAP >R
        BEGIN
            DUP
        WHILE
            DUPR@ EXECUTE
            >NFA
        REPEAT
        R> 
;

\ prints the names of words in the current compilation wordlist.
HEX 
: NAME>STRING ( nt -- ) COUNT 1F AND ;
DECIMAL 

: .ID      ( NFAaddr -- )  NAME>STRING  TYPE SPACE  ?BREAK SPACEBAR ;
: WORDS    ['] .ID   CONTEXT @ @ TRAVERSE-WORDLIST ;

: BREAKPOINT  CR .S   KEY DROP  ;

: SEARCH-WORDLIST ( addr len wid -- NFA )
        >R
        BEGIN
            2DUP R@ COUNT DUP 
        WHILE     
            COMPARE 
        WHILE    
            R> >NFA >R  
        REPEAT
        THEN 
        2DROP 
        R> 
;

: NFA>XT ( nfa -- XT -1|0|1)
          ?DUP 0= IF 0 EXIT THEN
          DUP NFA>CFA
          SWAP 1- C@ DUP 0= OR
;
