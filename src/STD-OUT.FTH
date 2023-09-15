\ STD-OUT.FTH  standard Forth output words   Sept 15 2023 Fox 

VARIABLE C/L       C/L@  C/L !
VARIABLE COL
VARIABLE ROW
VARIABLE CURSOR
VARIABLE C/SCR     3C0 C/SCR !

20 CONSTANT BL

: >VPOS  ( col row -- vaddr) C/L @ * + ;

: CLIP   ( n lo hi -- n) ROT MIN MAX ;

: CURSOR ( -- Vaddr) COL @ ROW @ >VPOS  0 C/SCR @ CLIP ;

: COL+!  ( n -- )
  COL @ +  DUP C/SCR @ >
  IF DROP COL OFF EXIT
  THEN COL ! ;

: ROW+!  ( n -- ) ROW @ +  0 23 CLIP ROW ! ;

: EMIT   ( c --) CURSOR VC! 1 COL+! ;
: CR     ( -- ) 1 ROW+!  COL OFF ;
 : SPACE   BL EMIT ;

: TYPE   ( addr len -- ) 0 DO  COUNT EMIT LOOP DROP ;

: AT-XY  ( col row -- ) ROW ! COL ! CURSOR WMODE  ;

: PAGE   0 C/SCR @ 20 VFILL  0 0 AT-XY ;

: VDPTYPE  ( addr len --) TUCK CURSOR SWAP VWRITE  COL+! ;