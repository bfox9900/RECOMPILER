\ CROSS COMPILER Extensions for ITC system and data types 

COMPILER \ commands for dictionary word creation 

VARIABLE TLAST                \ target dictionary: last word's NFA 

HEX 
\ compile (addr len) as counted string
: ?TNAME, ( addr len --) HEADERS @ IF  TS,  ELSE 2DROP  THEN ; 

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
          SPACE  R@ COUNT 1F AND TYPE ." WARNING: TARGET word redefined"
        THEN
      THEN
      R> TLAST !         \ HERE is NFA of last word defined
;



: NFA>LFA       ( nfa -- lfa)   3 - ;
: NFA>IMM       ( nfa -- precedence-field) NFA>LFA  TCELL+ ;
: >TBODY        ( cfa -- pfa ) TCELL+ ;


\ TARGET WORD precedence field control  
COMPILER 
HOST: XIMMEDIATE  ( -- ) 1  TLAST @ 1- TC! ;HOST

\ test if a TARGET word is immediate
HOST: XIMMEDIATE? ( nfa -- ?) 1- TC@ 01 AND 0> ;HOST


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
      ALSO ASSEMBLER 
      !CSP 
      TCREATE   THERE CELL+ T,  
      TCODEWORDS 1+! 
      DOES> TRUE ABORT" Can't run TARGET code word" ;

: CODE       NEWLABELS  +CODE ;
: ENDCODE    ?CODE-ERR RESOLVER  PREVIOUS  ; 
\ +++++ good to here '

\ TEST 
\ CODE DROP    *SP+ TOS MOV,  NEXT, ENDCODE 
\ CODE DUP     TOS PUSH,       NEXT, ENDCODE 

\ *************************************************************
\ We need the most primitive words for indirect threaded code now
\ to build the compiling words for the rest of the system. 
\ I call them EXECUTORS 
\ EXECUTORS are:  EXIT  DOVAR  DOCOL  DOCON  DOUSER  LIT 
\ Approximately 127 bytes 
   
  INCLUDE DSK9.EXECUTORS   

\ *************************************************************

\ RESOLVE TLITERAL for target compiler loop 
COMPILER 
:NONAME  STATE @ IF  TCOMPILE LIT  T,  THEN ; IS TLITERAL 

\ These words are in the META vocabulary. When TARGET is invoked
\ they will be found before the same words in the HOST Forth system.
COMPILER 
ALSO META DEFINITIONS 
: CREATE     TCREATE  TCOMPILE DOVAR               DOES> @ >TBODY ;  
: VARIABLE   TCREATE  TCOMPILE DOVAR    0 T,       DOES> @ >TBODY ;  
: 2VARIABLE  TCREATE  TCOMPILE DOVAR    0 T, 0 T,  DOES> @ >TBODY ; 
: CONSTANT   TCREATE  TCOMPILE DOCON ( n) T,       DOES> @ >TBODY T@ ; 

HOST: T[']   TCOMPILE LIT  T'   T,  ;HOST  IMMEDIATE  

\ USER variables are used in Multi-tasking Forth systems.
\ They are "local" to a specific task and usually reference by a pointer
\ called 'UP' (user pointer)
\ CAMEL99 does not have a user pointer. It uses the TMS9900 WP register
\ Cool trick, but it means we can't read a value from the TARGET system
\ for the cross-compiler to use.

\ So we have to create a faux user variable, UP(t) 
\ and we assign the value of the default Forth workspace. >8300
\ Now we can use UP(t) to calculate a USER variable's address

COMPILER VARIABLE UP(t)   HEX 8300 UP(t) !
ALSO META DEFINITIONS 
: USER ( n --) 
  TCREATE  TCOMPILE DOUSER  T,  DOES> @ >TBODY T@  UP(t) @ + ;

COMPILER   
