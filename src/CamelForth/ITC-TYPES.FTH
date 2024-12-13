\ CROSS COMPILER Extensions tell the compiler how to build ITC data types

CR .( TARGET DEFINING WORDS, 16 BIT MACHINE)
\ =======================================================================
\ TMS9900 and user variables under CAMEL99

\ USER variables are used in Multi-tasking Forth systems.
\ They are "local" to a specific task and usually reference by a pointer
\ called 'UP' (user pointer)
\ CAMEL99 does not have a user pointer. It uses the TMS9900 WP register
\ Cool trick, but it means we can't read a value from the TARGET system
\ for the cross-compiler to use.

\ So we have to create a variable UP(t) and assign the value of the for
\ of the WP register for the Cross compiler to calculate a USER variable's
\ Address.

\ For TI-994a our primary workspace is at at HEX 8300
COMPILER 
VARIABLE UP(t)   HEX 8300 UP(t) !


2 CONSTANT TCELL     \ for 16 bit machines 
: TCELL+   TCELL + ;
: TCELLS   TCELL * ;

VARIABLE TLAST            \ target dictionary last word's NFA 

HEX 
: THEADER, ( addr len --)
      ALIGN
      TLAST @ T,          \ get last NFA & compile in this LFA field
      0 TC,               \ compile the precedence byte (immediate flag)
      THERE >R            \ save HERE (ie: new NFA location)
      TS,                 \ compile (addr len) as counted string
      WARNINGS @
      IF
        R@ FIND ( xt ?) NIP ( ?)
        IF
          SPACE  R@ COUNT 1F AND TYPE  ." !! TARGET word redefined"
        THEN
      THEN
      R> TLAST !         \ HERE is NFA of last word defined
;


\ TCREATE makes 2 headers
\ 1. in TARGETS vocabulary, 
\ 2. in the TARGET image in SAMS memory 
: TCREATE    PARSE-NAME  2DUP  THEADER, HEADER, ;

: TCODE-ERR   TRUE ABORT" Host cannot run Target CODE words" ;

: ?CODE-ERR   SP@ CSP @ -
              IF
                 CR 
                 CR ." CODE word incomplete!"
                 CR ." DATA Stack -> " .S
                 TRUE ABORT" Compile halted"
              THEN ;

VARIABLE TCODEWORDS 
\ create SMART "code" words in the Target segment
\ They CANNOT run on the HOST system but abort with an error
\ +CODE used to permit jumps inside of CODE/ENDCODE
: +CODE  !CSP TCREATE   THERE 2+ T,  TCODEWORDS 1+!  DOES> TCODE-ERR ;
: CODE         NEWLABELS  +CODE ;
: ENDCODE      ?CODE-ERR  ;


\ These words are in the COMPILERS vocabulary. When TARGET-COMPILING is invoked
\ they will be found before the same words in the HOST Forth system.

COMPILER 
: >TBODY         ( cfa -- pfa ) TCELL+ ;
: NFA>PFA       ( nfa -- pfa )  NFA>CFA >TBODY ;

: CREATE     TCREATE    TCOMPILE DOVAR   T,             DOES> @ NFA>PFA ;  
: VARIABLE   TCREATE    TCOMPILE DOVAR   T,  0 T,       DOES> @ NFA>PFA ;  
: 2VARIABLE  TCREATE    TCOMPILE DOVAR   T,  0 T, 0 T,  DOES> @ NFA>PFA ; 
: CONSTANT   TCREATE    TCOMPILE DOCON   T, ( n) T,     DOES> @ NFA>PFA T@ ; 
: USER       TCREATE  TCOMPILE DOUSER    T,  DOES> @ NFA>PFA T@  UP(t) @ + ;  


\ create smart TARGET colon words that abort if you try to run them
: <colon:>  
   TCREATE  TCOMPILE DOCOL
   DOES> TRUE ABORT" Cannot run a TARGET colon definition" ;

   
