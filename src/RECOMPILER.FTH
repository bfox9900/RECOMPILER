\ recompiler.fth 

\ recompile a threaded Forth program in low ram. 
\ Dictionary headers are kept in the regular Forth dictionary

NEEDS WORDLIST FROM DSK1.WORDLISTS 

ONLY FORTH DEFINITIONS 
NEEDS DUMP  FROM DSK1.TOOLS 
NEEDS DEFER FROM DSK1.DEFER 
NEEDS LOCK  FROM DSK1.MARKER 

\ we need clean copies of words that are weird in the Camel99 kernel
\ for IMPORT: to access 
ONLY FORTH DEFINITIONS 
\ We use machine code because the Assembler compiles to target memory only 
HEX 
CODE DUP    0646 , C584 ,  NEXT, ENDCODE  
CODE DROP   C136 ,         NEXT, ENDCODE  
CODE C@     D114 , 0984 ,  NEXT, ENDCODE
CODE +      A136 ,         NEXT, ENDCODE   


ONLY FORTH DEFINITIONS 
VOCABULARY COMPILERS  \ words that are the cross-compiler
VOCABULARY TARGETS    \ *New* Forth system words 
VOCABULARY META       \ words that look like Forth but cross-compile 

VOCABULARY ASSEMBLER 
INCLUDE DSK7.MFORTHASM    \ contains the cross-compiler guts 
INCLUDE DSK7.MASMLABELS   \ handy to have labels for jumping around

\ REDEFINE CODE for Cross-compiling
\ use +CODE to jump across words within same label scope
\ CROSS-COMPILER CODE +CODE need ASSEMBLER in the search order
\ This is provided with the COMPILER and TARGET commands

 ONLY FORTH  ALSO ASSEMBLER ALSO COMPILERS DEFINITIONS 
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


\ pull in some Forth words to COMPILERS so they are visible in TARGET mode 
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
: >BODY  >BODY ;
\ THESE were important !!
: [      [ ; 
: ]    POSTPONE ]  ; IMMEDIATE 
: EXIT   EXIT ;
: STATE  STATE ;
: ON  ON ;
: OFF  OFF ;


\ Host forth's colon/semi renamed so we can use it anywhere 
: ;HOST     POSTPONE ;  ;  IMMEDIATE
: HOST:     :  ;

INCLUDE DSK7.IMPORT    \ 1st piece of the "recompiler"

\ add words to the cros-compiler 
ONLY FORTH  ALSO COMPILERS DEFINITIONS 

\ directives 
HEX 
2000 CONSTANT CDATA  \ location of the code buffer 

VARIABLE  LDADDR   \ program's load address in TI-99 memory 
: ORG   ( addr -- )  LDADDR ! ;


\ *future* deal with relocated images 
: OFFSET   ( -- n)  LDADDR @ CDATA -  ;
: REL>TARG  ( relocated -- target)  OFFSET - ;

\ target image fetch and store take relocated addresses and convert them
HEX 
: T!      ( n relocated -- ) REL>TARG ! ;
: T@      ( relocated -- n)  REL>TARG @ ;

\ SAVEIMG can save our programs as E/A 5 binary file 
INCLUDE DSK7.SAVEIMG 

ONLY FORTH ALSO COMPILERS DEFINITIONS 

ALSO ASSEMBLER             \ needed to Assembler the branch code
HEX 
: NEW   
    CDATA 2000 FF FILL   
    CDATA H !              \ ASM code will go into CDATA now
    DEAD @@ B,             \ initial instruction branches into start code 
    ['] TARGETS >BODY OFF  \ erase all the targets words 
;

ONLY FORTH ALSO COMPILERS DEFINITIONS 
\ Note: The TARGET XT is held in the "body" (PFA) of the HOST word
\ Below we take a HOST XT and fetch the TARGET XT
: >TARG-XT ( xt -- xt')  >BODY @   ;

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

\ T[']  and T' can now be put into the META vocabulary
\ and given names that look "normal".
\ This makes the cross-compiler source code backwards compatible
\ with standard forth but hides what's going on. 
\ Keeping both names helps my pea-brain remember.
ALSO META DEFINITIONS 
: '       T'   ;
: [']     POSTPONE T['] ; IMMEDIATE 

PREVIOUS DEFINITIONS 

\ ************** FORWARD REFERENCE ********************
\ TLITERAL will be defined later when we know where LIT
\ is compiled, but we need the name NOW for <TINTERP> 


DEFER TLITERAL  IMMEDIATE 

\ *****************************************************


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
            POSTPONE TLITERAL  \ change #2 
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


COMPILER    \ add duplicate names to the compilers wordlist 
: HOST      HOST ;  
: COMPILER  COMPILER ;  
: TARGET    TARGET ;    

\ immediate forms for when they are needed 
: [HOST]      HOST ; IMMEDIATE 
: [COMPILER]  COMPILER ;  IMMEDIATE 
: [TARGET]    TARGET ;  IMMEDIATE   

\ We use a text macro because the variable BOOT has not be created yet
: AUTOSTART  T'   S" BOOT T!" EVALUATE ; 

\ /\/\/\/\/\/\/\/\/\/\/\/ COMPILER ENDS /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\//\
ONLY FORTH DEFINITIONS 

HEX 
: GO   
    WARM  E4 7 VWTR 
    INIT-WORDLISTS  
    CR ." Meta Compiler v0.81 Sept 15 2023" 
    CR S" COMPILER NEW " EVALUATE 
    ABORT ;

LOCK 
INCLUDE DSK1.SAVESYS

' GO SAVESYS DSK7.METAFORTH




