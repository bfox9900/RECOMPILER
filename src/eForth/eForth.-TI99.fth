\ eForth Initial Model (tms9900)

                **** WORK IN PROGRESS **** 

\ ***************************************************************                
\ NOTE: 
\ In cases where TMS9900 has a machine instruction for a specific
\ function, we have replaced the Forth code with Assembler.
\ ***************************************************************

\ Based on bFORTH 1990 by Bill Muench, 1990
\ Donated to eForth Working Group, Silicon Valley FIG Chapter
\ to serve as a model of portable Forth for experimentation.

\ Ported to TI-99 by Brian Fox, Kilworth Ontario Canada 2023 

\ This program can be metacompiled with METAFORTH 1.0 for TI-99
\ and the eForth extension files 

\ Conventions
\
\ characters in the input stream
\
\ a aligned address
\ b byte address
\ c character
\ ca code address
\ cy carry
\ d signed double integer
\ F logical false
\ f flag 0 or non-zero
\ la link address
\ n signed integer
\ na name address
\ T logical true
\ t flag T or F
\ u unsigned integer
\ ud unsigned double integer
\ va vocabulary address
\ w unspecified weighted value

\ Header: token(ptr) link(la) name(na)
\
\ Count-byte and Lexicon bits ioxn nnnn
\ i - immediate
\ o - compile-only
\ x - tag
\ n - string length (31 characters MAX)
\ Compiler does not set bits in the NAME string
\ 0 < la na < .. < la na < va < CONTEXT @
\ 0 < FORTH < .. < vl va < vl va < CURRENT CELL+ @

.( Equates )
COMPILER 
H# FF4E EQU =RP  \ return stack base
H# FF7C EQU =SP  \ data stack base
H# FF80 EQU =TIB \ default Terminal Input Buffer
H# 2000 EQU =UP  \ user base

H# 0080 EQU =IMED \ lexicom immediate bit
H# 0040 EQU =COMP \ lexicom compile-only bit
H# 7F1F EQU =MASK \ lexicon bit mask

H# 0001 EQU =BYTE \ size of a byte
H# 0002 EQU =CELL \ size of a cell

H# 000A EQU =BASE \ default radix (decimal)
H# 0008 EQU =VOCS \ vocabulary stack depth

H# E890 EQU =CALL \ 8086 CALL opcode (NOP CALL)

\ TMS9900 register useage
\
\ ** R0      general purpose register
\ ** R1      general purpose register
\ ** R2      general purpose register
\ ** R3      general purpose register
\ ** R4      general purpose register
\ ** R5      general purpose register
\ ** R6      parameter stack pointer
\ ** R7      return stack pointer
\ ** R8      Forth 'W' register ( holds XT on entry to word)
\ ** R9      Forth VM IP (Interpreter pointer)
\ ** R10     Forth's "NEXT" routine cache
\ ** R11     9900 sub-routine linkage, holds pfa after doLIST (enter)
\ ** R12     9900 CRU register  - OR - general purpose register
\ ** R13     Multi-tasker LINK toNEXT, task
\ ** R14     Multi-tasker Program counter
\ ** R15     Multi-tasker task Status register

\ Behind the curtain:
\ This eForth remains true to Dr. Ting and Bill Meunch's vision.
\ Words in lower case are not for compiling, not interactive use. 

\ Special features in this eForth Model are:;
\ 1. 31 machine dependent words and 193 high level words. 
\    a) TMS9900 has hardware multiply and divide so use them 

\ 2. Direct Threaded Code. (9900 uses BL and R11 holds the PFA)
\ 3. Separated name and code dictionaries. 
\ 4. All system variables are defined as user variables for ROMmability. 
\ 5. Vectored ?KEY, KEY, and EMIT. 
\ 6. File handling through the serial I/O interface. 
\ 7. CATCH-THROW error handler. 
\ 8. Only the single indexed FOR-NEXT loop is provided. 
\ 9. Track the proposed ANS Forth Standard. 
\ 10. Compile-only words are trapped in interpretive mode. 
\ 11. Tools include DUMP, WORDS, SEE and .S . 
\ 12. Flexibility in memory mapping.

\ The Forth inner interpreter
\
\ On the 9990 it is more space efficient to JUMP
\ to the inner interpreter through a register
\ On other processors it may be better
\ to write it as inline code 

\ CROSS COMPILER DIRECTIVES
NEW 
TARGET    
H# 2000 ORG 

L: NEXT ( -- )     \ Direct threaded NEXT
  *IP+ W  MOV,     \ move CFA into Working register 
      *W  B,       \ branch to the address in w

.( Special interpreters )

l: DOLIT 
   SP  DECT,     
  *IP+ R4 MOV,  
   NEXT,

L: doLIST ( a -- ) 
   IP RPUSH,         \ push IP register onto the return stack
   R11 IP MOV,       \ move PFA into Forth IP register
   NEXT,
ENDCODE

\ CODE COLD ( -- )   ORIG @@ B, 

CODE BYE
   83C4 @@ CLR,        \ clear interrupt vector
   0000 @@ BLWP,       \ ROM reset vector is at 0000 
ENDCODE 

CODE EXECUTE ( a -- ) TOS POP, *TOS B,  

CODE EXIT ( -- )   IP RPOP,   NEXT, ENDCODE

.( Loop & Branch 16bit absolute address )

\ : next ( -- ) \ hiLevel model 16bit absolute branch
\   r> r> dup if 1 - >r @ >r exit then drop cell+ >r ;
CODE next ( -- ) COMPILE-ONLY \ single index loop
   *RP DEC,         \ dec counter value
    OC IF,          
        *IP IP ADD, \ jump back: ADD *IP,IP
        NEXT,
    ENDIF, 
    RP INCT,        \ remove counter from Rstack 
    IP INCT,        \ move past (LOOP)'s in-line parameter
    NEXT, 
ENDCODE

CODE ?branch ( f -- ) COMPILE-ONLY
    R4 POP, 
    R4, R4 SOC,      \ test flag
    EQ IF,           \ if R4 = 0
        *IP IP ADD,  \ take the jump 
        NEXT, 
    ENDIF, 
    IP INCT,         \ move forward in the program 
    NEXT,      
ENDCODE 

CODE branch ( -- ) COMPILE-ONLY
   *IP IP ADD,   \ take the jump 
    NEXT,
ENDCODE

.( Memory fetch & store )

CODE ! ( w a -- )  R4 POP,  *SP+ *R4 MOV,  NEXT, ENDCODE
CODE @ ( a -- w )  *SP R4 MOV,  *R4 *SP MOV, NEXT, ENDCODE

CODE C! ( w b -- )
   R4 POP, 
   *SP SWPB,
   *SP+ *R4 MOVB,
  NEXT,
ENDCODE

CODE C@ ( b -- c )
    W POP,     
    R4 CLR,      
   *W R4 MOVB,  
    R4 SWPB,
    R4 PUSH,   
  NEXT,
ENDCODE

.( Return Stack )

CODE RP@ ( -- a )  RP PUSH,            NEXT, ENDCODE
CODE RP! ( a -- )  RP POP,             NEXT, ENDCODE COMPILE-ONLY
CODE R>  ( -- w ) *RP PUSH,   RP INCT, NEXT, ENDCODE COMPILE-ONLY
CODE R@  ( -- w ) *RP PUSH,            NEXT, ENDCODE
CODE >R  ( w -- )  RP DECT,  *RP PUSH, NEXT, ENDCODE COMPILE-ONLY

.( Data Stack )

CODE SP@ ( -- a )  SP DECT, SP *SP MOV, NEXT, ENDCODE
CODE SP! ( a -- )  *SP+ SP MOV,         NEXT, ENDCODE

CODE DROP ( w -- )      SP INCT,NEXT, ENDCODE
CODE DUP  ( w -- w w )  SP DECT,NEXT, ENDCODE

CODE SWAP ( w1 w2 -- w2 w1 )
  *SP      W  MOV, 
   2 (SP) *SP MOV,
   W   2 (SP) MOV, 
  NEXT,
ENDCODE

CODE OVER ( w1 w2 -- w1 w2 w1 ) 
   2 (SP) W MOV, 
         SP DECT, 
   W    *SP MOV,
   NEXT 
ENDCODE

: ?DUP   ( w -- w w, 0 ) DUP IF DUP THEN ;
: NIP    ( w w -- w ) SWAP DROP ;
: ROT    ( w1 w2 w3 -- w2 w3 w1 ) >R SWAP R> SWAP ;
: 2DROP  ( w w -- ) DROP DROP ;
: 2DUP   ( w1 w2 -- w1 w2 w1 w2 ) OVER OVER ;

.( Logic )

CODE 0< ( n -- t )
  *SP W MOV,  
  *SP CLR,
   W 0 CI, LT 
   IF, 
      *SP SETO,  
   ENDIF,
  NEXT,,
ENDCODE

CODE AND ( w w -- w )
  *SP+  R0 MOV, 
        R0 INV,    
   R0  *SP SZC, 
   NEXT,
ENDCODE

CODE OR ( w w -- w )
   *SP+ R0 MOV,   
   *SP  R0 SOC, 
    R0 *SP MOV, 
   NEXT,
ENDCODE

CODE XOR ( w w -- w ) 
   *SP+ R0 MOV,   
   *SP  R0 XOR, 
    R0 *SP MOV, 
   NEXT,    
ENDCODE

\ : INVERT ( w -- w ) -1 XOR ;
CODE INVERT   *SP INV,  NEXT, ENDCODE 

.( Arithmetic )

\ CODE UM+ ( u u -- u cy ) \ or ( u u -- ud )

\ D+ is a better primitive for TMS9900 than M+
CODE D+   ( lo hi lo' hi' -- d)
  *SP+    R0  MOV,
  *SP+    TOS ADD,  \ add hi #s
   R0     *SP ADD,  \ add lo #s
   OC IF,           \ carry set?
         TOS INC,   \ incr hi
   ENDIF,
   NEXT,
ENDCODE

: S>D    ( n -- d)  DUP 0< ;
: M+     ( d n -- d) S>D  D+ ; 

\ : + ( u u -- u ) UM+ DROP ;
CODE +  ( u u -- u ) \ Same size as Forth code
  *SP+ W MOV, 
   W *SP ADD,    
   NEXT,
ENDCODE

\ : NEGATE ( n -- -n ) INVERT 1 + ;
CODE NEGATE   *SP NEG,  NEXT, ENDCODE 

: DNEGATE ( d -- -d ) INVERT >R INVERT 1 UM+ R> + ;

: - ( w w -- w ) NEGATE + ;

\ : ABS ( n -- +n ) DUP 0< IF NEGATE THEN ;
CODE ABS   *SP ABS,  NEXT, ENDCODE 

.( User variables )

: doUSER ( -- a ) R> @ UP @ + ; COMPILE-ONLY

: doVAR ( -- a ) R> ; COMPILE-ONLY

8 \ start offset

DUP USER SP0 1 CELL+ \ initial data stack pointer
DUP USER RP0 1 CELL+ \ initial return stack pointer

DUP USER '?KEY 1 CELL+ \ character input ready vector
DUP USER 'EMIT 1 CELL+ \ character output vector

DUP USER 'EXPECT 1 CELL+ \ line input vector
DUP USER 'TAP 1 CELL+   \ input case vector
DUP USER 'ECHO 1 CELL+  \ input echo vector
DUP USER 'PROMPT 1 CELL+ \ operator prompt vector

DUP USER BASE 1 CELL+ \ number base

DUP USER temp 1 CELL+ \ scratch
DUP USER SPAN 1 CELL+ \ #chars input by EXPECT
DUP USER >IN 1 CELL+ \ input buffer offset
DUP USER #TIB 1 CELL+ \ #chars in the input buffer
1 CELLS ALLOT \ address of input buffer

DUP USER UP 1 CELL+ \ user base pointer
DUP USER CSP 1 CELL+ \ save stack pointers
DUP USER 'EVAL 1 CELL+ \ interpret/compile vector
DUP USER 'NUMBER 1 CELL+ \ numeric input vector
DUP USER HLD 1 CELL+ \ formated numeric string
DUP USER HANDLER 1 CELL+ \ error frame pointer

DUP USER CONTEXT 1 CELL+ \ first search vocabulary
=VOCS CELL+ \ vocabulary stack

DUP USER CURRENT 1 CELL+ \ definitions vocabulary
1 CELL+ \ newest vocabulary

DUP USER CP 1 CELL+ \ dictionary code pointer
1 CELL+ \ dictionary name pointer
1 CELL+ \ last name compiled

?USER

.( Comparison )

: 0= ( w -- t ) IF 0 EXIT THEN -1 ;

: = ( w w -- t ) XOR 0= ;

: U< ( u u -- t ) 2DUP XOR 0< IF  NIP 0< EXIT THEN - 0< ;
: <  ( n n -- t ) 2DUP XOR 0< IF DROP 0< EXIT THEN - 0< ;

: MAX ( n n -- n ) 2DUP < IF SWAP THEN DROP ;
: MIN ( n n -- n ) 2DUP SWAP < IF SWAP THEN DROP ;

: WITHIN ( u ul uh -- t ) OVER - >R - R> U< ;

.( Divide )

\ : UM/MOD ( udl udh un -- ur uq )
\   2DUP U<
\   IF NEGATE 15
\   FOR >R DUP UM+ >R >R DUP UM+ R> + DUP
\   R> R@ SWAP >R UM+ R> OR
\   IF >R DROP 1 + R> ELSE DROP THEN R>
\   NEXT DROP SWAP EXIT
\   THEN DROP 2DROP -1 DUP ; \ 92 bytes 

\ TMS9900 is higher level than eForth 
CODE UM/MOD ( ud u1 -- u2 u3 ) 
   TOS  R0 MOV,     \ divisor->R0                 14
  *SP+ TOS MOV,     \ POP high word into TOS      22
  *SP   R5 MOV,     \ MOVE low word to r5         18
   R0  TOS DIV,     \ perform unsigned division  124
   R5  *SP MOV,     \ push remainder              22
   NEXT,            \                            200
ENDCODE \ 10 bytes :-)

: M/MOD ( d n -- r q ) \ floored
   DUP 0< DUP >R
   IF NEGATE >R DNEGATE R>
   THEN >R DUP 0< IF R@ + THEN R> UM/MOD R>
   IF SWAP NEGATE SWAP THEN ;

: /MOD ( n n -- r q ) OVER 0< SWAP M/MOD ;
: MOD ( n n -- r ) /MOD DROP ;
: / ( n n -- q ) /MOD NIP ;

.( Multiply )

\ : UM* ( u1 u2 -- ud )
\   0 SWAP ( u1 0 u2 ) 15
\   FOR DUP UM+ >R >R DUP UM+ R> + R>
\   IF >R OVER UM+ R> + THEN
\   NEXT ROT DROP ;

CODE UM*    ( n n -- d)     \ 2 cells in -- 2 cells out
  *SP  TOS MPY,    \ 52+4=56
   R5  *SP MOV,    \ 18
   NEXT,           \ 
ENDCODE

: * ( n n -- n ) UM* DROP ;

: M* ( n n -- d )
   2DUP XOR 0< >R ABS SWAP ABS UM* R> IF DNEGATE THEN ;

: */MOD ( n n n -- r q ) >R M* R> M/MOD ;
: */ ( n n n -- q ) */MOD NIP ;

.( Bits & Bytes )

: BYTE+ ( b -- b ) [ =BYTE ] LITERAL + ;
: CELL+ ( a -- a ) [ =CELL ] LITERAL + ;

: CELLS ( n -- n ) [ =CELL ] LITERAL * ;

: BL ( -- 32 ) 32 ;

: >CHAR ( c -- c )
  127 AND DUP 127 BL WITHIN IF [ CHAR _ ] LITERAL NIP THEN ;

: DEPTH ( -- n ) SP@ SP0 @ SWAP - 2 / ;

: PICK ( +n -- w ) 1 + CELLS SP@ + @ ;

: ALIGNED ( b -- a ) ; IMMEDIATE

.( Memory access )

: +! ( n a -- ) SWAP OVER @ + SWAP ! ;

: 2! ( d a -- ) SWAP OVER ! CELL+ ! ;
: 2@ ( a -- d ) DUP CELL+ @ SWAP @ ;

: COUNT ( b -- b +n ) DUP 1 + SWAP C@ ;

: HERE ( -- a ) CP @ ;
: PAD ( -- a ) HERE 80 + ;
: TIB ( -- a ) #TIB CELL+ @ ;

: NP ( -- a ) CP CELL+ ;
: LAST ( -- a ) NP CELL+ ;

: @EXECUTE ( a -- ) @ ?DUP IF EXECUTE THEN ;

: CMOVE ( b b u -- )
  FOR AFT >R COUNT R@ C! R> 1 + THEN NEXT 2DROP ;

: -TRAILING ( b u -- b u )
\ : -TRAILING  ( adr len char -- adr len')  \ might be faster
\         2DUP + 1-
\         BEGIN
\            DUP C@ BL =   \ fetch last character
\         WHILE            \ test for BLANK
\            1-            \ while char is a match, decrement length
\         REPEAT
\         NIP OVER -  ;    \ 30 bytes  12.26 seconds
   FOR 
      AFT DUP R@ + C@ BL XOR
      IF R> 1 + EXIT THEN 
      THEN
   NEXT 0 ;

: FILL ( b u c -- )
  SWAP FOR SWAP AFT 2DUP C! 1 + THEN NEXT 2DROP ;

: ERASE ( b u -- ) 0 FILL ;

: PACK$ ( b u a -- a ) \ null terminated
   DUP >R 2DUP C! 1 + 2DUP + 0 SWAP ! SWAP CMOVE R> ;

.( Numeric Output ) \ single precision

: DIGIT ( u -- c ) 9 OVER < 7 AND + [ CHAR 0 ] LITERAL + ;
: EXTRACT ( n base -- n c ) 0 SWAP UM/MOD SWAP DIGIT ;

: <# ( -- ) PAD HLD ! ;
: HOLD ( c -- ) HLD @ 1 - DUP HLD ! C! ;
: # ( u -- u ) BASE @ EXTRACT HOLD ;
: #S ( u -- 0 ) BEGIN # DUP WHILE REPEAT ;
: SIGN ( n -- ) 0< IF [ CHAR - ] LITERAL HOLD THEN ;
: #> ( w -- b u ) DROP HLD @ PAD OVER - ;
: str ( w -- b u ) DUP >R ABS <# #S R> SIGN #> ;
: HEX ( -- ) 16 BASE ! ;
: DECIMAL ( -- ) 10 BASE ! ;

.( Numeric Input ) \ single precision

: DIGIT? ( c base -- u t )
>R [ CHAR 0 ] LITERAL - 9 OVER <
IF 7 - DUP 10 < OR THEN DUP R> U< ;

: NUMBER? ( a -- n T, a F )
BASE @ >R 0 OVER COUNT ( a 0 b n)
OVER C@ [ CHAR $ ] LITERAL =
IF HEX SWAP BYTE+ SWAP 1 - THEN ( a 0 b' n')
OVER C@ [ CHAR - ] LITERAL = >R ( a 0 b n)
SWAP R@ - SWAP R@ + ( a 0 b" n") ?DUP
IF 1 - ( a 0 b n)
FOR DUP >R C@ BASE @ DIGIT?
WHILE SWAP BASE @ * + R> BYTE+
NEXT R@ ( ?sign) NIP ( b) IF NEGATE THEN SWAP
ELSE R> R> ( b index) 2DROP ( digit number) 2DROP 0
THEN DUP
THEN R> ( n ?sign) 2DROP R> BASE ! ;

.( Basic I/O )

: KEY? ( -- f ) '?KEY @EXECUTE ;
: KEY ( -- c ) BEGIN '?KEY UNTIL ;
: EMIT ( c -- ) 'EMIT @EXECUTE ;

: NUF? ( -- f ) KEY? DUP IF KEY 2DROP KEY 13 = THEN ;

: PACE ( -- ) 11 EMIT ;
: SPACE ( -- ) BL EMIT ;

: CHARS ( +n c -- ) SWAP 0 MAX FOR AFT DUP EMIT THEN NEXT DROP ;

: SPACES ( +n -- ) BL CHARS ;

: do$ ( -- a ) R> R@ R> COUNT + ALIGNED >R SWAP >R ; COMPILE-ONLY

: $"| ( -- a ) do$ ; COMPILE-ONLY

: TYPE ( b u -- ) FOR AFT COUNT EMIT THEN NEXT DROP ;

: .$ ( a -- ) COUNT TYPE ;

: ."| ( -- ) do$ .$ ; COMPILE-ONLY

: CR ( -- ) 13 EMIT 10 EMIT ;

: .R ( n +n -- ) >R str R> OVER - SPACES TYPE ;
: U.R ( u +n -- ) >R <# #S #> R> OVER - SPACES TYPE ;

: U. ( u -- ) <# #S #> SPACE TYPE ;
: . ( w -- ) BASE @ 10 XOR IF U. EXIT THEN str SPACE TYPE ;

: ? ( a -- ) @ . ;

.( Parsing )

: parse ( b u c -- b u delta \ )
   temp ! OVER >R DUP \ b u u
   IF 1 - temp @ BL =
      IF \ b u' \ 'skip'
         FOR COUNT temp @ SWAP - 0< INVERT WHILE
         NEXT ( b) R> DROP 0 DUP EXIT \ all delim
         THEN 1 - R>
   THEN OVER SWAP \ b' b' u' \ 'scan'
   FOR COUNT temp @ SWAP - temp @ BL =
   IF 0< THEN WHILE
   NEXT DUP >R ELSE R> DROP DUP >R 1 -
   THEN OVER - R> R> - EXIT
   THEN ( b u) OVER R> - ;

: PARSE ( c -- b u \ )
  >R TIB >IN @ + #TIB @ >IN @ - R> parse >IN +! ;

: .( ( -- ) [ CHAR ) ] LITERAL PARSE TYPE ; IMMEDIATE
: (  ( -- ) [ CHAR ) ] LITERAL PARSE 2DROP ; IMMEDIATE
: \  ( -- ) #TIB @ >IN ! ; IMMEDIATE

: CHAR ( -- c ) BL PARSE DROP C@ ;
: CTRL ( -- c ) CHAR $001F AND ;

: TOKEN ( -- a \ ) BL PARSE 31 MIN NP @ OVER - 2 - PACK$ ;

: WORD ( c -- a \ ) PARSE HERE PACK$ ;

.( Dictionary Search )

: NAME> ( na -- ca ) 2 CELLS - @ ;

: SAME? ( a a u -- a a f \ -0+ )
   FOR 
      AFT OVER R@ CELLS + @
      OVER R@ CELLS + @ - ?DUP
      IF R> DROP EXIT THEN 
      THEN
   NEXT 0 ;

: find ( a va -- ca na, a F )
   SWAP \ va a
   DUP C@ 2 / temp ! \ va a \ get cell count
   DUP @ >R \ va a \ count byte & 1st char
   CELL+ SWAP \ a' va
   BEGIN @ DUP \ a' na na
      IF DUP @ [ =MASK ] LITERAL AND R@ XOR \ ignore lexicon bits
         IF CELL+ -1 
         ELSE CELL+ temp @ SAME? 
         THEN
      ELSE R> DROP EXIT
      THEN
   WHILE 2 CELLS - \ a' la
   REPEAT 
   R> DROP NIP 1 CELLS - DUP NAME> SWAP ;

: NAME? ( a -- ca na, a F )
   CONTEXT DUP 2@ XOR IF 1 CELLS - THEN >R \ context<>also
   BEGIN R> CELL+ DUP >R @ ?DUP
   WHILE find ?DUP
   UNTIL R> DROP EXIT THEN R> DROP 0 ;

.( Terminal )

: ^H ( b b b -- b b b ) \ backspace
   >R OVER R@ < DUP
   IF [ CTRL H ] LITERAL 'ECHO @EXECUTE 
   THEN R> + ;

: TAP ( bot eot cur key -- bot eot cur )
   DUP 'ECHO @EXECUTE OVER C! 1 + ;

: kTAP ( bot eot cur key -- bot eot cur )
   DUP 13 XOR
   IF [ CTRL H ] LITERAL XOR 
      IF BL TAP 
      ELSE ^H 
      THEN EXIT
   THEN 
   DROP NIP DUP ;

: ACCEPT ( b u -- b u )
   OVER + OVER
   BEGIN 2DUP XOR
   WHILE
      KEY DUP BL - 95 U<
      IF TAP 
      ELSE 'TAP @EXECUTE 
      THEN
   REPEAT 
   DROP OVER - ;

: EXPECT ( b u -- ) 'EXPECT @EXECUTE SPAN ! DROP ;

: QUERY ( -- )
   TIB 80 'EXPECT @EXECUTE #TIB ! 0 NIP >IN ! ;

.( Error handling )

: CATCH ( ca -- err#/0 )
   SP@ >R HANDLER @ >R RP@ HANDLER !
   EXECUTE
   R> HANDLER ! R> DROP 0 ;

: THROW ( err# -- err# )
   HANDLER @ RP! R> HANDLER ! R> SWAP >R SP! DROP R> ;

CREATE NULL$ 0 ,

: ABORT ( -- ) NULL$ THROW ;

: abort" ( f -- ) IF do$ THROW THEN do$ DROP ; COMPILE-ONLY

.( Interpret )

: $INTERPRET ( a -- )
   NAME? ?DUP
   IF @ [ =COMP ] LITERAL AND
      ABORT" compile ONLY" EXECUTE EXIT
   THEN
   'NUMBER @EXECUTE
   IF EXIT THEN THROW ;

: [ ( -- ) [ ' $INTERPRET ] LITERAL 'EVAL ! ; IMMEDIATE

: .OK ( -- ) [ ' $INTERPRET ] LITERAL 'EVAL @ = IF ." ok" THEN CR ;

: ?STACK ( -- ) DEPTH 0< IF $" underflow" THROW THEN ;

: EVAL ( -- )
   BEGIN TOKEN DUP C@
   WHILE 'EVAL @EXECUTE ?STACK
   REPEAT DROP 'PROMPT @EXECUTE ;

.( Device I/O )

CODE IO? ( -- f ) \ FFFF is an impossible character
\ Interface to call ROM KSCAN
   TOS PUSH,
   TOS CLR,            \ TOS will be our true/false flag
   0 LIMI,             \ disable interrupts, ALL VDP routines restore them 
   TOS 837C @@ MOVB,   \ clear GPL flags
   83E0 LWPI,          \ switch to GPL workspace
   000E @@ BL,         \ call ROM keyboard scanning routine
   WRKSP0 LWPI,        \ return to Forth's workspace
   837C @@ R1 MOVB,    \ read GPL status byte (=2000 if key pressed)
   R1  3 SLA,          \ check the key bit
   OC IF,              \ if carry flag set
      8375 @@ TOS MOV, \ read the key
   ENDIF,
   NEXT,               \ return
   ENDCODE

\ TI-99 "video display peripheral" (VDP) port addresses
H# 8800 EQU VDPRD               \ vdp ram read data
H# 8802 EQU VDPSTS              \ vdp status
H# 8C00 EQU VDPWD               \ vdp ram write data
H# 8C02 EQU VDPWA               \ vdp ram read/write address

VARIABLE OUT 

CODE TX! ( c -- )
     OUT @@ R0 MOV, 
     R0 4000 ORI, 
     R0 SWPB,
     R0 VDPWA @@ MOVB, 
     R0 SWPB, 
     R0 VDPWA @@ MOVB,
     TOS SWPB,
     TOS VDPWD @@ MOVB,   
     OUT @@ INC, 
     TOS POP, 
     NEXT, 
ENDCODE

: !IO ( -- ) ; IMMEDIATE \ initialize I/O device

.( Shell )

: PRESET ( -- ) SP0 @ SP! [ =TIB ] LITERAL #TIB CELL+ ! ;

: XIO ( a a a -- ) \ reset 'EXPECT 'TAP 'ECHO 'PROMPT
[ ' ACCEPT ] LITERAL 'EXPECT !
'TAP ! 'ECHO ! 'PROMPT ! ;

: FILE ( -- )
[ ' PACE ] LITERAL [ ' DROP ] LITERAL [ ' kTAP ] LITERAL XIO ;

: HAND ( -- )
[ ' .OK ] LITERAL 'EMIT @ [ ' kTAP ] LITERAL XIO ;

CREATE I/O ' RX? , ' TX! , \ defaults

: CONSOLE ( -- ) I/O 2@ 'KEY? 2! HAND ;

: que ( -- ) QUERY EVAL ;

: QUIT ( -- ) \ clear return stack ONLY
   RP0 @ RP!
   BEGIN [COMPILE] [
      BEGIN 
         [ ' que ] LITERAL CATCH ?DUP
      UNTIL ( a)
      CONSOLE NULL$ OVER XOR
      IF CR TIB #TIB @ TYPE
         CR >IN @ [ CHAR ^ ] LITERAL CHARS
         CR .$ ." ? "
      THEN PRESET
   AGAIN ;

.( Compiler Primitives )

: ' ( -- ca ) TOKEN NAME? IF EXIT THEN THROW ;
: ALLOT ( n -- ) CP +! ;
: , ( w -- ) HERE ALIGNED DUP CELL+ CP ! ! ;
: [COMPILE] ( -- \ ) ' , ; IMMEDIATE
: COMPILE ( -- ) R> DUP @ , CELL+ >R ; COMPILE-ONLY
: LITERAL ( w -- ) COMPILE doLIT , ; IMMEDIATE
: $," ( -- ) [ CHAR " ] LITERAL PARSE HERE PACK$ C@ 1 + ALLOT ;
: RECURSE ( -- ) LAST @ CURRENT @ ! ; IMMEDIATE

.( Structures ) 
( These need to have copies in the META wordlist)
: FOR    ( -- a ) COMPILE >R HERE ; IMMEDIATE
: BEGIN  ( -- a ) HERE ; IMMEDIATE
: NEXT   ( a -- ) COMPILE next , ; IMMEDIATE
: UNTIL  ( a -- ) COMPILE ?branch , ; IMMEDIATE
: AGAIN  ( a -- ) COMPILE branch , ; IMMEDIATE
: IF     ( -- A ) COMPILE ?branch HERE 0 , ; IMMEDIATE
: AHEAD  ( -- A ) COMPILE branch HERE 0 , ; IMMEDIATE
: REPEAT ( A a -- ) [COMPILE] AGAIN HERE SWAP ! ; IMMEDIATE
: THEN   ( A -- ) HERE SWAP ! ; IMMEDIATE
: AFT    ( a -- a A ) DROP [COMPILE] AHEAD [COMPILE] BEGIN SWAP ; IMMEDIATE
: ELSE   ( A -- A ) [COMPILE] AHEAD SWAP [COMPILE] THEN ; IMMEDIATE
: WHILE  ( a -- A a ) [COMPILE] IF SWAP ; IMMEDIATE

: ABORT" ( -- \ ) COMPILE abort" $," ; IMMEDIATE

: $" ( -- \ ) COMPILE $"| $," ; IMMEDIATE
: ." ( -- \ ) COMPILE ."| $," ; IMMEDIATE

.( Name Compiler )

: ?UNIQUE ( a -- a ) DUP NAME? IF ." Redef " OVER .$ THEN DROP ;

: $,n ( na -- )
DUP C@
IF ?UNIQUE
( na) DUP LAST ! \ for OVERT
( na) HERE ALIGNED SWAP
( cp na) 1 CELLS -
( cp la) CURRENT @ @
( cp la na') OVER !
( cp la) 1 CELLS - DUP NP ! ( ptr) ! EXIT
THEN $" name" THROW ;

.( FORTH Compiler )

: $COMPILE ( a -- )
NAME? ?DUP
IF C@ [ =IMED ] LITERAL AND
IF EXECUTE ELSE , THEN EXIT
THEN
'NUMBER @EXECUTE
IF [COMPILE] LITERAL EXIT
THEN THROW ;

: OVERT ( -- ) LAST @ CURRENT @ ! ;

: ; ( -- )
COMPILE EXIT [COMPILE] [ OVERT ; COMPILE-ONLY IMMEDIATE

: ] ( -- ) [ ' $COMPILE ] LITERAL 'EVAL ! ;

: CALL, ( ca -- ) \ DTC 8086 relative call
[ =CALL ] LITERAL , HERE CELL+ - , ;

: : ( -- \ ) TOKEN $,n [ ' doLIST ] LITERAL CALL, ] ;

: IMMEDIATE ( -- ) [ =IMED ] LITERAL LAST @ C@ OR LAST @ C! ;

.( Defining Words )

: USER ( u -- \ ) TOKEN $,n OVERT COMPILE doUSER , ;

: CREATE ( -- \ ) TOKEN $,n OVERT COMPILE doVAR ;

: VARIABLE ( -- \ ) CREATE 0 , ;

.( Tools )

: _TYPE ( b u -- ) FOR AFT COUNT >CHAR EMIT THEN NEXT DROP ;

: dm+ ( b u -- b )
OVER 4 U.R SPACE FOR AFT COUNT 3 U.R THEN NEXT ;

: DUMP ( b u -- )
BASE @ >R HEX 16 /
FOR CR 16 2DUP dm+ -ROT 2 SPACES _TYPE NUF? 0= WHILE
NEXT ELSE R> DROP THEN DROP R> BASE ! ;

: .S ( -- ) CR DEPTH FOR AFT R@ PICK . THEN NEXT ."
: !CSP ( -- ) SP@ CSP ! ;
: ?CSP ( -- ) SP@ CSP @ XOR ABORT" stack depth" ;

: >NAME ( ca -- na, F )
CURRENT
BEGIN CELL+ @ ?DUP WHILE 2DUP
BEGIN @ DUP WHILE 2DUP NAME> XOR
WHILE 1 CELLS -
REPEAT THEN NIP ?DUP
UNTIL NIP NIP EXIT THEN 0 NIP ;

: .ID ( na -- )
?DUP IF COUNT $001F AND TYPE EXIT THEN ." {noName}" ;

: WORDS ( -- )
CR CONTEXT @
BEGIN @ ?DUP
WHILE DUP SPACE .ID 1 CELLS - NUF?
UNTIL DROP THEN ;

.( Hardware reset )

\ version

$100 CONSTANT VER ( -- u )
\ hi byte = major revision in decimal
\ lo byte = minor revision in decimal

: hi ( -- )
!IO \ initialize IO device & sign on
CR ." eForth v1.0"
; COMPILE-ONLY

CREATE 'BOOT ' hi , \ application vector

: COLD ( -- )
\ init CPU
\ init stacks
\ init user area
\ init IP
PRESET 'BOOT @EXECUTE
QUIT ;

