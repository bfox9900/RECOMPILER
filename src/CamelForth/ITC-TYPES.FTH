\ CROSS COMPILER Extensions for ITC system and data types 

COMPILER \ commands for dictionary word creation 

VARIABLE TLAST                \ target dictionary: last word's NFA 

HEX 
\ compile (addr len) as counted string
: ?TNAME, ( addr len --) HEADERS @ IF TS, ELSE 2DROP THEN ; 

: THEADER, ( addr len --)
      ALIGN
      TLAST @ T,              \ get last NFA & compile in this LINK field
      0 TC,                   \ compile the precedence byte (immediate flag)
      THERE >R                \ save HERE (ie: new NFA location)
      ?TNAME, 
      WARNINGS @
      IF
        R@ FIND ( xt ?) NIP ( ?)
        IF
          SPACE  R@ COUNT 1F AND TYPE  ." WARNING: TARGET word redefined"
        THEN
      THEN
      R> TLAST !         \ HERE is NFA of last word defined
;

: NFA>LFA       ( nfa -- lfa)   3 - ;
: NFA>IMM       ( nfa -- precedence-field) NFA>LFA  TCELL+ ;
: >TBODY        ( cfa -- pfa ) TCELL+ ;
: NFA>PFA       ( nfa -- pfa )  NFA>CFA >TBODY ;

\ interpreted words for the cross compiler go in META vocabulary 
ALSO META DEFINITIONS 
: IMMEDIATE    ( -- )   01 TLAST @ NFA>IMM TC! ;  
\ test if a TARGET word is immediate
: IMMEDIATE?   ( nfa -- ?) NFA>IMM TC@ 01 AND 0> ;  

COMPILER 
CR .( TARGET DEFINING WORDS, 16 BIT MACHINE)
\ =======================================================================
\ primary target dictionary creator 
\ TCREATE makes 2 headers
\ 1. in TARGETS vocabulary, 
\ 2. in the TARGET image in SAMS memory -if- HEADERS = TRUE 
\ TCREATE is not "create" in TARGET. It's actually <BUILDS 
\ There is no runtime executor compiled by default 

\ create SMART "code" words in the Target segment
\ They CANNOT run on the HOST system but abort with an error
COMPILER 
HEX 
: TCREATE  ( -- SAMSaddr)
   CREATE               \ create word in the host forth system 
      THERE ,           \ record where it is in TARGET memory
      LATEST @ COUNT 1F AND THEADER,  \ make same name in the TARGET 
                                      \ no EXECUTOR is compile
  \ when invoked return the TARGET address of the word 
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
      ALSO ASSEMBLER 
      !CSP TCREATE   
         THERE CELL+ T,  
         TCODEWORDS 1+! 
      DOES> TRUE ABORT" Can't run TARGET code word" ;

: CODE       NEWLABELS  +CODE ;
: ENDCODE    ?CODE-ERR RESOLVER  PREVIOUS  ; 
\ +++++ good to here '

\ TEST 
\ CODE DROP    *SP+ TOS MOV,  NEXT, ENDCODE 
\ CODE DUP     TOS PUSH,       NEXT, ENDCODE 

\ *************************************************************
\ we need the most primitive code for indirect threaded code now
\ Approximately 127 bytes 
   
  INCLUDE DSK9.EXECUTORS   

\ *************************************************************

\ RESOLVE TLITERAL for target compiler loop 
COMPILER 
: TLIT  STATE @ IF  TCOMPILE LIT  T,  THEN ;   
' TLIT IS TLITERAL 

\ These words are in the COMPILERS vocabulary. When TARGET is invoked
\ they will be found before the same words in the HOST Forth system.
COMPILER 
ALSO META DEFINITIONS 
: CREATE     TCREATE  TCOMPILE DOVAR               DOES> @ NFA>PFA ;  
: VARIABLE   TCREATE  TCOMPILE DOVAR    0 T,       DOES> @ NFA>PFA ;  
: 2VARIABLE  TCREATE  TCOMPILE DOVAR    0 T, 0 T,  DOES> @ NFA>PFA ; 
: CONSTANT   TCREATE  TCOMPILE DOCON ( n) T,       DOES> @ NFA>PFA T@ ; 

\ USER variables are used in Multi-tasking Forth systems.
\ They are "local" to a specific task and usually reference by a pointer
\ called 'UP' (user pointer)
\ CAMEL99 does not have a user pointer. It uses the TMS9900 WP register
\ Cool trick, but it means we can't read a value from the TARGET system
\ for the cross-compiler to use.

\ So we have to create a variable UP(t) and assign the value of the for
\ of the WP register for the Cross compiler to calculate a USER variable's
\ Address.
COMPILER VARIABLE UP(t)   HEX 8300 UP(t) !

: USER       TCREATE  TCOMPILE DOUSER  T,  DOES> @ NFA>PFA T@  UP(t) @ + ;  
