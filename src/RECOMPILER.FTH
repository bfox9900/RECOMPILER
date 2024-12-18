\ recompiler.fth   Oct 2024   Brian Fox 

\ ** this version builds on Camel99 Supercart version **

\ recompile a threaded Forth program in SAMS memory 
\ Dictionary headers are kept in the regular Forth dictionary

NEEDS WORDLIST FROM DSK1.WORDLISTS 

ONLY FORTH DEFINITIONS 
NEEDS DUMP  FROM DSK1.TOOLS 
\ NEEDS DEFER FROM DSK1.DEFER 
NEEDS VALUE FROM DSK1.VALUES 
NEEDS LOCK  FROM DSK1.MARKER 
NEEDS VIRT>REAL FROM DSK1.SAMS 
NEEDS SAMS.FILL FROM DSK7.SAMSMEM
NEEDS DIR      FROM DSK1.DIR 

\ we need clean copies of words that are weird in the Camel99 kernel
\ for IMPORT: to access 
ONLY FORTH DEFINITIONS 
\ We use machine code because the Assembler compiles to target memory only 
HEX 
CODE DUP    0646 , C584 ,  NEXT, ENDCODE  
CODE DROP   C136 ,         NEXT, ENDCODE  
CODE C@     D114 , 0984 ,  NEXT, ENDCODE
CODE +      A136 ,         NEXT, ENDCODE   

\ string management 
HEX
: +PLACE ( addr n $ -- ) 2DUP 2>R  COUNT +  SWAP MOVE 2R> C+! ;
: BETWEEN 1+ WITHIN ;
: LOWER? ( c -- ?) [CHAR] a [CHAR] z BETWEEN ;
: UPPER? ( c -- ?) [CHAR] A [CHAR] Z BETWEEN ; 

: UPCASE ( c -- c') DUP LOWER? IF 05F AND THEN ;
: LOCASE ( c -- c') DUP UPPER? IF 060 OR  THEN ;

: TOLOWER ( addr len -- addr len) 2DUP BOUNDS ?DO  I C@ LOCASE I C!  LOOP ;
: TOUPPER ( addr len -- addr len) 2DUP BOUNDS ?DO  I C@ UPCASE I C!  LOOP ;

: ?DOT   ( $ u --) [CHAR] . SCAN NIP 0= ABORT" '.' expected in path" ;



ONLY FORTH DEFINITIONS 
: CODE    TRUE ABORT" Can't use HOST 'CODE'" ;
: ENDCODE TRUE ABORT" Can't to use HOST 'ENDCODE'" ; 

VOCABULARY COMPILERS  \ words that make the cross-compiler
VOCABULARY TARGETS    \ *New* Forth system words 
VOCABULARY META       \ words that look like Forth but cross-compile 
VOCABULARY ASSEMBLER 

ALSO ASSEMBLER DEFINITIONS 
INCLUDE DSK7.SAMASM9900   \ cross assemble into SAMS segment (64K max)
INCLUDE DSK7.MASMLABELS   \ handy to have labels for jumping around

 ONLY FORTH  ALSO ASSEMBLER ALSO COMPILERS DEFINITIONS 
 \ =================================================
\ duplicate some Forth words in COMPILERS VOCABULARY 
\ so they are visible in TARGET mode 
: \    1 PARSE 2DROP ; IMMEDIATE
: (    POSTPONE (  ;  IMMEDIATE  ( borrowed from Forth )
: .(   POSTPONE .(     ; IMMEDIATE
: CR     CR    ;
: HEX        HEX ;
: DECIMAL    DECIMAL ;
: INCLUDE    INCLUDE ;
: WORDS      WORDS  ;
: .S         .S    ;
: DUMP       DUMP  ;
: PAGE       PAGE  ;   \ don't conflict with compiler PAGE
: ALSO       ALSO ;
: DEFINITIONS   DEFINITIONS ;

: TWORDS   ALSO TARGETS  WORDS   PREVIOUS ; ( searches TARGETS wordlist)
: CELL+  CELL+ ;
: CELLS  CELLS ;

: >BODY  >BODY ;

\ THESE were important !!
: STATE  STATE ;
: [     STATE OFF ; 
: ]     POSTPONE ] ; IMMEDIATE 
\ : EXIT   EXIT ;
: ON   ON ;
: OFF  OFF ;
: IMMEDIATE  IMMEDIATE ;

\ Host forth's colon/semi renamed so we can use it anywhere 
: ;HOST     POSTPONE ;  ;  IMMEDIATE
: HOST:     :  ;
: CHAR    ?EXEC CHAR ;  \ interpreting only 
: [CHAR]  ?COMP POSTPONE [CHAR] ; IMMEDIATE 
: S"      POSTPONE S" ; IMMEDIATE 
: INCLUDED   INCLUDED ;

\ add words to the cros-compiler 
ONLY FORTH  ALSO COMPILERS DEFINITIONS 

\ directives 
HEX 

VARIABLE  LDADDR   \ program's load address in TI-99 memory 
ALSO ASSEMBLER
: ORG   ( addr -- ) 
    DUP LDADDR !  
    SDP !  
    DEAD @@ B, ;  \ compile the jump to code in first 2 bytes 
PREVIOUS    

\ location of the code buffer in SAMS memory
: CDATA  ( -- addr) LDADDR @ ; 
: 'PREAM ( -- addr) CDATA CELL+ ; \ address of first code word to run 

\ target image fetch and store take relocated addresses and convert them
HEX 
: T!      ( n relocated -- ) !L ;
: T@      ( relocated -- n)  @L ;

: TALIGN  THERE ALIGNED SDP ! ;
: TPLACE  SAMS.PLACE ;


\ SAVEIMG can save our programs as E/A 5 binary file 
INCLUDE DSK7.SAMSAVE 

ONLY FORTH ALSO ASSEMBLER ALSO COMPILERS DEFINITIONS 
HEX 
\ erase entire 64k segment in 4K chunks
: PURGE  FFFF 0 DO I VIRT>REAL 1000 FFFF FILLW  1000 +LOOP ;

: NEW   
    CR ." Erasing 64K SAMS segment ..."
    PURGE    
    LDADDR OFF   
    ['] TARGETS >BODY OFF  \ erase all the targets words 
    CR 
;

\ support for "home" disk 
CREATE $HOME 16 ALLOT 


\ path control ...
: CHDIR  ( <path> )  PARSE-NAME 2DUP ?DOT TOUPPER $HOME PLACE ;
: +PATH  ( $ u -- $' u' ) $HOME COUNT PAD PLACE   PAD +PLACE  PAD COUNT ;
 

ONLY FORTH ALSO COMPILERS DEFINITIONS 

: EQU     CONSTANT ; 
: equ     EQU ;

\ Note: The TARGET XT is held in the "body" (PFA) of the HOST word
\ Below we take a HOST XT and fetch the TARGET XT
: >TARG-XT ( xt -- xt')  >BODY @ ;

: T'      ( <text> -- XT)  ALSO TARGETS  '  >TARG-XT    PREVIOUS ;

\ Now we can build the rest of the weapons
: TCOMPILE, ( Host-XT -- )  >TARG-XT  T,  ;

\ Brain stretcher: 
\ Retain a TARGET XT in the compiler as a literal
\ so you can give it to a TARGET definition
: T[']    T'   POSTPONE LITERAL  ;  IMMEDIATE 

\ Same as above but now take the TARGET XT from the compiler
\ and "compile" it into the TARGET definition 
: TCOMPILE   ( -- <name>) 
    T'   POSTPONE LITERAL   POSTPONE T, ;  IMMEDIATE  

: T[COMPILE]    ( <name>) 
    ?COMP  T'    T,  ; IMMEDIATE

: TPOSTPONE ( <name> -- ) \ *G replaces COMPILE and [COMPILE]
      ?COMP
      BL WORD FIND DUP 0= ?ERR
      0< IF POSTPONE TCOMPILE 
      THEN  TCOMPILE, ; IMMEDIATE  

\ compile a stack string into target memory
: TS,    ( adr len -- ) THERE OVER 1+  TALLOT TPLACE TALIGN ;

\ parse input string into TARGET memory
: T,"         ( <text>" -- ) [CHAR] " PARSE TS, ; 

\ create a name for a forward reference
: FORWARD 
    CREATE  0 , 
    DOES>  @  DUP 0= ABORT" Unresolved FORWARD"
; IMMEDIATE 

\ pass an XT to a forward reference word and store in the DATA field 
: RESOLVES  ( xt -- ) T'  >BODY  ! ;

FORWARD (TS")  \ resolve this later in the kernel code 
: TS,    ( adr len -- ) THERE OVER 1+  TALLOT TPLACE TALIGN ; 
\ : TS"          (TS") T,  [CHAR] " PARSE TS, ; IMMEDIATE

\ T[']  and T' can now be put into the META vocabulary
\ and given names that look "normal".
\ This makes the cross-compiler source code backwards compatible
\ with standard forth but hides what's going on. 
\ Keeping both names helps my pea-brain remember.
\ ALSO META DEFINITIONS 
\ : '       T'   ;
\ : [']     POSTPONE T['] ; IMMEDIATE 
\ : S"      POSTPONE TS" ; IMMEDIATE

\ : ."          POSTPONE T." ; IMMEDIATE
\ : CHAR        TCHAR ;
\ : [CHAR]      ?XCOMP TCHAR  POSTPONE TLITERAL ; IMMEDIATE
\ : [COMPILE]   POSTPONE  T[COMPILE] ; IMMEDIATE
\ PREVIOUS DEFINITIONS 


DEFER TLITERAL \ resolve this once TARGET LIT is defined 

\ H# (hex number prefix) 
\ Convert the next word in the input stream to a hex number
\ Adapted from Open Firmware by Mitch Bradley

: H#  ( "hexnumber" -- n )
    0 0 PARSE-NAME ( -- 0 0 str len)
    BASE @ >R      \ save the current RADIX 
    HEX  >NUMBER   \ convert the number in BASE 16
    R> BASE !      \ restore previous radix 
    ( -- n flag) ABORT" expected HEX number"  
    2DROP  
    TLITERAL    \ if compiling, compile literal no.
; IMMEDIATE


\ RECOMPILER loop uses TCOMPILE, and TLITERAL 
\ That's the only difference. 
ONLY FORTH ALSO COMPILERS ALSO FORTH DEFINITIONS 
: <TINTERP>  ( i*x c-addr u -- j*x )
      'SOURCE 2!  >IN OFF
      BEGIN
        BL WORD  DUP C@   ( -- addr len)
      WHILE
        FIND ?DUP
        IF ( it's a word)
            1+ STATE @ 0= OR
            IF   EXECUTE
            ELSE TCOMPILE,  \ change #1
            THEN
        ELSE ( it's a number)
            COUNT NUMBER? ?ERR
            TLITERAL        \ change #2 
        THEN
        DEPTH 0< ABORT" Short stack"
      REPEAT
      DROP ;

\ switches for "normal" Forth or cross-compiler Forth interpreters
\ 'IV is CAMEL99 "interpreter vector" variable 
 : FORTH-REPL   ['] <INTERP>  'IV ! ;
 : XFORTH-REPL  ['] <TINTERP> 'IV ! ;

\ name space control is tricky in Forth cross-compilers
\ search order is critical for finding the correct word.
\ These commands simplify getting it right 

: HOST     
    ONLY FORTH  
    ALSO ASSEMBLER 
    ALSO FORTH DEFINITIONS 
    FORTH-REPL  
;    

: COMPILER  
    ONLY FORTH                   \ last searched 
    ALSO ASSEMBLER 
    ALSO TARGETS 
    ALSO COMPILERS DEFINITIONS   \ 1st searched 
    FORTH-REPL                   \ Forth compiler REPL is used 
;

: TARGET    ( Note: TARGET cuts us off from HOST Forth words )
    ONLY ASSEMBLER 
    ALSO COMPILERS 
    ALSO TARGETS DEFINITIONS 
    ALSO META        \ cross-compiler versions found first 
    XFORTH-REPL      \ cross-compiler REPL is used 
;  

COMPILER 
\ immediate forms for when they are needed 
: [HOST]  HOST     ; IMMEDIATE
: [CC]    COMPILER ; IMMEDIATE 
: [TC]    TARGET   ; IMMEDIATE   

\ add duplicate names to the compilers wordlist 
: HOST      HOST ;  
: COMPILER  COMPILER ;  
: TARGET    TARGET ;    

\ REDEFINE CODE for Cross-compiling
\ use +CODE to jump across words within same label scope
\ CROSS-COMPILER CODE +CODE need ASSEMBLER in the search order
\ This is provided with the COMPILER and TARGET commands
COMPILER 
: +CODE  
    CREATE              \  name is in HOST Forth      
      THERE ,           \ and remember where the word is in TARGET 

      THERE CELL+ T,    \ TARGET compile the ITC entry address 
      !CSP 
    DOES> STATE @ 
          IF  TRUE ABORT" Can't run TARGET code word"   
          THEN  @ ;

: CODE    ( <name> ) NEWLABELS +CODE ;  \ start new label scope 
: ENDCODE ( -- ) ?CSP RESOLVER ;



\ *******************************************************
\ *** COMPILER DIRECTIVES TO PATCH THE BINARY PROGRAM ***
\ *******************************************************

\ PREAMBLE default values for stacks and workspace 
\ Change these COMPILER VALUES before loading PREAMBLE 
FFAC VALUE DSTACK     \ data stack base address 
FF3E VALUE RSTACK     \ return stack base address 
8300 VALUE WKSPACE    \ 

\ usage: BOOT AUTOSTART MAIN
\ where the variable BOOT is given the XT of the 1st Forth word to run
: AUTOSTART  ( variable -- ) ?EXEC T' ( -- a1 XT) SWAP T! ;

\ sets the CODE word that the system jumps into to make Forth VM 
: PREAMBLE  ?EXEC T'  >BODY   'PREAM  T! ;

: END.  CDATA 0= ABORT" ORG not set"  
       'PREAM T@ DEAD = ABORT" PREAMBLE not set" 
;

\ /\/\/\/\/\/\/\/\/\/\/\/ COMPILER ENDS /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\//\
COMPILER ALSO FORTH DEFINITIONS 

HEX 
: GO   
    WARM  E4 7 VWTR 
    INIT-WORDLISTS  
    HOST ALSO COMPILERS
    CR ." FXCC Meta Compiler V0.88 Dec 11 2024"
    CR 
    SAMSINI 
    CR COMPILER NEW 
    CR ." Ready"
    ABORT ;

LOCK 
WARNINGS OFF 
INCLUDE DSK1.SUPERSAVE
WARNINGS ON 

' GO SUPERSAVE DSK7.FXCC

