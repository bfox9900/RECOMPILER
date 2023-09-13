\ ASMLABELS.FTH   numbered labels for ASM9900           Apr 3 2021 Fox
\ Original idea from DxForth. Complete rewrite uses a stack for forward refs.
\ Feb 2023 FS> error resets label stack with NEWLABELS 

CR .( Enumerated labels)

ONLY FORTH ALSO ALSO COMPILERS 

ALSO ASSEMBLER DEFINITIONS 

DECIMAL
        15 CONSTANT #LABELS
#LABELS 2* CONSTANT #FWD

\ Make a stack to handle jumps
CREATE FS0    #FWD CELLS ALLOT
CREATE LABELS   #LABELS CELLS ALLOT

: ]LBL  ( n -- addr) CELLS LABELS + ;  \ array of label addresses

FS0 CREATE FSP ,   \ fwd stack pointer, initialzed to FS0

: NEWLABELS  ( -- )
    LABELS  #LABELS CELLS 0 FILL  \ clear label array
    FS0 FSP !   \ reset fwd stack pointer to base address
;

: FSDEPTH ( -- n) FS0 FSP @ -  2/ ;
: >FS     ( addr --) 2 FSP +!   FSP @ ! ;
: FS>     ( -- addr)
    FSP @  DUP FS0 = 
    IF NEWLABELS 
       TRUE ABORT" Label stack underflow"
    THEN              
    ( stkaddr) @  -2 FSP +! ;

\ ** CHANGED HERE TO THERE FOR CROSS-COMPILER 
: $:  ( n -- )  THERE SWAP ]LBL !  ;  \ code label creator
: $   ( n -- 0) THERE >FS  >FS  0  ;  \ jump label creator

: ?LABEL  ( addr -- addr) DUP 0= ABORT" Un-resolved jump" ;

: RESOLVER ( -- )
      BEGIN
        FSDEPTH 
      WHILE
        FS> ]LBL @ ?LABEL
        ( lbladdress ) FS> TUCK - RESOLVE
      REPEAT
;

\ TARGET CODE labels for BRANCH or Branch & Link return target "HERE" address
: L:      ( <text> ) THERE CONSTANT ;

