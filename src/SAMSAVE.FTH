CR .( SAMSAVE.FTH saves program in SAMS as EA5 BINARY.  B Fox Oct 2024) 
 
ONLY FORTH ALSO ASSEMBLER ALSO COMPILERS ALSO FORTH DEFINITIONS

NEEDS LOAD-FILE FROM DSK1.LOADSAVE  \ we use SAVE-FILE from this library
NEEDS SAMS.HERE FROM DSK7.SAMSMEM 

HEX
\ 2000 CONSTANT 'ORG   \ start of program
1000 CONSTANT VDPBUFF  \ Programs write to file from VDP Ram
2000 CONSTANT 8K
  13 CONSTANT .PROG.   \ file mode for Program files
 
\ define the file header fields *THESE ARE VDP ADDRESSES*
VDPBUFF  CONSTANT MULTIFLAG
VDPBUFF  1 CELLS + CONSTANT PROGSIZE
VDPBUFF  2 CELLS + CONSTANT LOADADDR
VDPBUFF  3 CELLS + CONSTANT CODEORG     \ COPY 8K program chunks to here
         3 CELLS   CONSTANT HEADLEN
 
\ words to compute Forth system properties
: SYS-SIZE    ( -- n) CDATA THERE SWAP - ;
: #FILES      ( -- n)  SYS-SIZE 8K /MOD SWAP IF  1+ THEN ;
: CODECHUNK   ( n -- SAMSaddr) 8K *  CDATA + ;
: CHUNKSIZE   ( n -- bytes ) CODECHUNK THERE SWAP -  8K MIN ;

: LASTCHAR++  ( Caddr --) COUNT 1- +  1 SWAP C+! ;
 
: ?PATH    ( addr len -- addr len )
    2DUP  [CHAR] . SCAN NIP 0= ABORT" Path expected" ;

HEX 
: SAVE  ( -- <textpath> )
  BL PARSE-WORD  ?PATH  PAD PLACE
  #FILES 0
  DO
     CR ." Writing file " I . ." of " #FILES .
     CR ." Init file header " I  . ." : " 
     I 1+ #FILES <> DUP U.  MULTIFLAG V!
     I CHUNKSIZE    DUP U.  PROGSIZE V!
     I CODECHUNK    DUP U.  LOADADDR V!
     CR ." Copy to VDP & write to disk"
\ erase 8K of VDP RAM      
     CODEORG  8K 0 VFILL
\ copy code memory chunk to VDP RAM
     I CODECHUNK CODEORG  PROGSIZE V@ HEADLEN +  SAMS>VDP 
\ save the chunk to disk 
     PAD COUNT   VDPBUFF  PROGSIZE V@ HEADLEN +  .PROG. SAVE-FILE
     PAD LASTCHAR++   \ Update file name
     CR
  LOOP
  CR ." System size=" DECIMAL SYS-SIZE U. ." bytes"
  CR ." Saved in " #FILES .  ." EA5 files"
  CR
;
 
 
