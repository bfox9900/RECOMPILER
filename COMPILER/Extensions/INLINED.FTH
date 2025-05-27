\ inlined.fth   allows setting the inline bit in a code word  Jul 2023 Fox

NEEDS CFA>NFA   FROM DSK1.FORGET 

HEX 80 CONSTANT %INLINE  \ bit 7 used to mark for inlining primitives 

: (INLINED) ( prec -- )
  DUP C@        \ ( -- addr byte)            
  %INLINE OR    \ set bit 7  
  SWAP C! ;     \ put it back  

: INLINED ( -- ) \ set a bit in the precedence field in last definition 
  LATEST @ 1-  (INLINED) ;
\ use INLINED like IMMEDIATE 
\ CODE PRIM1   < BLAH BLAH >  NEXT, ENDCODE  INLINED

\ INLINED sets an extra bit in the precedence field 
\ CFA>PREC needs to ignore the IMMEDIATE bit *AND* INLINED bit.
\ To find the precedence field we need mask the bits:
\ IMMEDIATE  01 
\ INLINED    80
\ -also we must mask out the length byte in the Name field which can be 1F MAX
\ OR them together and we get:  811F . Invert that and we get the mask 7EE0   
HEX
\ : CFA>NFA ( cfa -- nfa | 0 ) BEGIN  DUP @ 7EE0 AND WHILE 2- REPEAT 1+ ;

: CFA>PREC  ( XT -- prec)  CFA>NFA 1-  ;
: INLINED?  ( xt -- ) CFA>PREC %INLINE AND 0>  ;

: ?CODE   ( xt -- xt )  DUP  DUP @ 2-  <> ABORT" Not a code word"  ;
: INLINE:    '  ?CODE   CFA>PREC  (INLINED) ;  \ make existing word inlined 
