CR .( ITC CODE ENDCODE  May 2025)

\ primary target dictionary creator
\ TCREATE makes 2 headers
\ 1. in TARGETS vocabulary,
\ 2. in the TARGET image in SAMS memory -if- HEADERS = TRUE
COMPILER \ commands for dictionary word creation

VARIABLE TLAST                \ target dictionary: last word's NFA

HEX
\ compile (addr len) as counted string
: ?TNAME, ( addr len --)
      HEADERS @
      IF
           TS,   \ compile the name
      ELSE
           2DROP  \ drop name
           0 TC, TALIGN
      THEN ;

: THEADER, ( addr len --)
      ALIGN
      HEADERS @ IF TLAST @ T, THEN  \ get last NFA & compile in LINK fld.
      0 TC,             \ compile the precedence byte (immediate flag)

      THERE >R          \ save HERE (ie: new NFA location)
      ?TNAME,
      WARNINGS @
      IF
        R@ FIND ( xt ?) NIP ( ?)
        IF
          SPACE  R@ COUNT 1F AND TYPE ." WARNING: TARGET word redefined"
        THEN
      THEN
      R> TLAST !     \ THERE is NFA of last TARGET word defined
;


\ create SMART "code" words in the Target segment
\ They CANNOT run on the HOST system but abort with an error
\ **FIXED TCREATE** to record the NFA of the TARGET word.
COMPILER
HEX
: TCREATE  ( -- SAMS_CFA)
   CREATE               \ create word in the host forth system
      LATEST @ COUNT 1F AND THEADER,  \ make same name in the TARGET
                                      \ no EXECUTOR is compiled
      THERE ,     \ record CFA of this TARGET word
  \ when invoked return the TARGET XT address of the word
   DOES> @ ;

DECIMAL
\ For TI-994a our primary workspace is at at HEX 8300
COMPILER VARIABLE TCODEWORDS

: CSP?  ( -- ?) SP@ CSP @ - ; \ test is stack is clean from !CSP

: ?CODE-ERR
      CSP?
      IF
         CR
         CR ." CODE word incomplete!"
         CR ." DATA Stack -> " .S
         TRUE ABORT" Compile halted"
      THEN ;

\ +CODE used to permit jumps inside of CODE/ENDCODE
: +CODE
 \     ALSO ASSEMBLER
      !CSP
      TCREATE   THERE CELL+ T,
      TCODEWORDS 1+!
      DOES> TRUE ABORT" Can't run TARGET code word" ;

: CODE       NEWLABELS  +CODE ;
: ENDCODE    ?CODE-ERR RESOLVER  ( PREVIOUS)  ;
