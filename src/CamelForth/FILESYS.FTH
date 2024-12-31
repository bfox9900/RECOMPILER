 \ FilesyX2.hsf                                        Apr 10 2021  Fox
 \ *New: [PAB FNAME] is passed to DSRLNK on the Forth stack
 \ Added FOPEN and FGET, common factors for ANS files

COMPILER HEX

\ To save kernel space we are reading/writing VDP directly using Forth words
\ There is NO PAB duplicate in CPU RAM.

TARGET
VARIABLE ^PAB            \ active PAB pointer

\ fast fetch of pab base pointer. (*This is also the OPCODE field)
CODE [PAB   ( -- adr) TOS PUSH,  ^PAB @@ TOS MOV,  NEXT, ENDCODE

\ PAB field words calculate offset from [PAB base in ** VDP RAM **
\ CODE is 16 BYTES smaller than Forth equivalent and faster
\ Trailing square bracket is to indicate they are used with [PAB
\ Read/write the PAB data fields with:  V@/V!  VC@/VC! VREAD/VWRITE

\ Example:  [PAB FBUFF] V@   [PAB CHARS] VC@
\                                                                      data  
 CODE FLG]    ( vaddr -- vaddr') TOS INC,         NEXT, ENDCODE \ 1+   byte
 CODE FBUFF]  ( vaddr -- vaddr') TOS INCT,        NEXT, ENDCODE \ 2+   cell 
 CODE RECLEN] ( vaddr -- vaddr') *TOS+ *TOS+ MOV, NEXT, ENDCODE \ 4+   byte 
 CODE CHARS]  ( vaddr -- vaddr') TOS  5 AI,       NEXT, ENDCODE \ 5+   byte
 CODE REC#]   ( vaddr -- vaddr') TOS  6 AI,       NEXT, ENDCODE \ 6+   cell 
 CODE STAT]   ( vaddr -- vaddr') TOS  8 AI,       NEXT, ENDCODE \ 8+   byte 
 CODE FNAME]  ( vaddr -- vaddr') TOS  9 AI,       NEXT, ENDCODE \ 9+   string

\ control the PAB stack
12C CONSTANT PSZ       \ size of PAB+BUFFER

\ generic file error handler
: ?FILERR  ( ior -- ) ?DUP IF CR CR T." * File Err #" .  ABORT THEN ;
: VPLACE   ( $addr len Vaddr -- ) 2DUP VC! 1+ SWAP VWRITE ;

\ === High Level File functions ====
: FILEOP  ( opcode -- err)           \ TI99 O/S call
         PAUSE                       \ better multi-tasking
        [PAB VC!                     \ write opcode byte to VDP PAB
        [PAB DUP FLG] DUP VC@ 1F AND SWAP VC! \ *CRITICAL* clear ERR code bits
        ( Vaddr) FNAME] DSRLNK ( -- err)      \ DSRLINK with parameter
;

\ primitive file open sets up PAB with given arguments
: FOPEN ( $addr len rec-size fam -- err)
        [PAB BL  0 VFILL          \ erase PAB ( 32 bytes)
        [PAB FLG]    VC!          \ set file access mode
        [PAB RECLEN] VC!          \ set bytes/rec
        [PAB FNAME] DUP           \ -- addr len Vaddr Vaddr
        020 + [PAB FBUFF] V!      \ FBUFF=32 bytes past fname
        VPLACE                    \ write string to [PAB FNAME]
        0 FILEOP                  \ open the file, return err code
;

: ?FILE ( n -- ) DUP 0= TS" Filename" ?ABORT ;   \ tests string length only

\ *G see E/A Manual page 298 for meaning of PAB status field
: FSTAT ( -- c)  9 FILEOP DROP [PAB STAT] VC@ ;

: FGET  ( buffer -- len ) \ read PABbuffer->CPU ram, returns length
         [PAB FBUFF] V@ SWAP [PAB CHARS] VC@  DUP>R  VREAD R> ;

\ *G INCLUDED can bootstrap the system on startup. It is nestable.
\ ** Saves all state info and allocates a new PAB in VDP RAM
\ ** Uses TIB as the RAM buffer
\ Cross-compiler usage: S" DSK1.START" INCLUDED
: INCLUDED  ( caddr len -- )
        ?FILE
        CR T." Loading: " 2DUP TYPE
        SOURCE-ID @ >IN @ 2>R           \ save source-ID, input pointer
        PSZ NEGATE ^PAB +!              \ make new PAB, on pab stack
        ( $ len ) 50 14 FOPEN ?FILERR   \ OPEN as 80 FIXED DV80 INPUT
        SOURCE-ID 1+!                   \ incr. source ID (1st file is 1)
        LINES OFF                       \ reset the line counter
        BEGIN
          2 FILEOP 0=                   \ file read operation
        WHILE
          TIB DUP FGET INTERPRET        \ interpret the buffer
          LINES 1+!                     \ count the line
        REPEAT
        PSZ ^PAB +!                     \ remove PAB from pab stack
        2R> >IN !  SOURCE-ID !          \ restore >IN, SOURCE-ID
;
