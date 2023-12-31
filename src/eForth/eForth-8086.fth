Output of file : EFORTH.SRC contained in archive : EFORTH.ZIP
\ eForth Initial Model (8086)

\ Based on bFORTH 1990 by Bill Muench, 1990
\ Donated to eForth Working Group, Silicon Valley FIG Chapter
\ to serve as a model of portable Forth for experimentation.

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

$xxxx EQU =RP \ return stack base
$xxxx EQU =SP \ data stack base
$xxxx EQU =UP \ user base
$xxxx EQU =TIB \ default Terminal Input Buffer

$0080 EQU =IMED \ lexicom immediate bit
$0040 EQU =COMP \ lexicom compile-only bit
$7F1F EQU =MASK \ lexicon bit mask

$0001 EQU =BYTE \ size of a byte
$0002 EQU =CELL \ size of a cell

$000A EQU =BASE \ default radix
$0008 EQU =VOCS \ vocabulary stack depth

$E890 EQU =CALL \ 8086 CALL opcode (NOP CALL)

\ 8086 register useage
\
\ AX BX CX DX DI ES free
\ SP data stack pointer
\ BP return stack pointer
\ SI interpreter pointer
\ CS=DS=SS segment pointers
\ IP instruction pointer

\ The Forth inner interpreter
\
\ On the 8086 it is more efficient to compile
\ the inner interpreter as inline code.
\ On other processors it may be better
\ to jump to the routine.

MACRO NEXT ( -- )
LODS WORD \ 1 byte
JMP AX \ 2 bytes
END-MACRO

.( Special interpreters )

CODE doLIT ( -- w ) COMPILE-ONLY
LODS WORD \ r> dup 2+ >r @
PUSH AX
NEXT
END-CODE

CODE doLIST ( a -- ) \ call dolist list..
XCHG BP, SP
PUSH SI \ push on return stack
XCHG BP, SP
POP SI \ new list address
NEXT
END-CODE

CODE COLD ( -- )
JMP ORIG

CODE BYE
INT $20

CODE EXECUTE ( a -- )
POP BX
JMP BX

CODE EXIT ( -- )
XCHG BP, SP
POP SI \ pop from return stack
XCHG BP, SP
NEXT
END-CODE

.( Loop & Branch 16bit absolute address )

\ : next ( -- ) \ hiLevel model 16bit absolute branch
\ r> r> dup if 1 - >r @ >r exit then drop cell+ >r ;

CODE next ( -- ) COMPILE-ONLY \ single index loop
SUB 0 [BP], # 1 WORD \ decrement index
U>= IF \ test index
MOV SI, 0 [SI] \ continue looping, r> @ >r
NEXT
THEN
INC BP INC BP \ drop index (pop return stack)
LABEL noBRAN
INC SI INC SI \ exit loop
NEXT
END-CODE

CODE ?branch ( f -- ) COMPILE-ONLY
POP BX
OR BX, BX \ test flag
JNZ noBRAN
MOV SI, 0 [SI] \ branch, r> @ >r
NEXT
END-CODE

CODE branch ( -- ) COMPILE-ONLY
MOV SI, 0 [SI] \ r> @ >r
NEXT
END-CODE

.( Memory fetch & store )

CODE ! ( w a -- )
POP BX
POP 0 [BX]
NEXT
END-CODE

CODE @ ( a -- w )
POP BX
PUSH 0 [BX]
NEXT
END-CODE

CODE C! ( w b -- )
POP BX
POP AX
MOV 0 [BX], AL
NEXT
END-CODE

CODE C@ ( b -- c )
POP BX
XOR AX, AX
MOV AL, 0 [BX]
PUSH AX
NEXT
END-CODE

.( Return Stack )

CODE RP@ ( -- a )
PUSH BP
NEXT
END-CODE

CODE RP! ( a -- ) COMPILE-ONLY
POP BP
NEXT
END-CODE

CODE R> ( -- w ) COMPILE-ONLY
PUSH 0 [BP]
INC BP INC BP
NEXT
END-CODE

CODE R@ ( -- w )
PUSH 0 [BP]
NEXT
END-CODE

CODE >R ( w -- ) COMPILE-ONLY
DEC BP DEC BP
POP 0 [BP]
NEXT
END-CODE

.( Data Stack )

CODE SP@ ( -- a )
MOV BX, SP
PUSH BX
NEXT
END-CODE

CODE SP! ( a -- )
POP SP
NEXT
END-CODE

CODE DROP ( w -- )
INC SP INC SP
NEXT
END-CODE

CODE DUP ( w -- w w )
MOV BX, SP
PUSH 0 [BX]
NEXT
END-CODE

CODE SWAP ( w1 w2 -- w2 w1 )
POP BX
POP AX
PUSH BX
PUSH AX
NEXT
END-CODE

CODE OVER ( w1 w2 -- w1 w2 w1 )
MOV BX, SP
PUSH 2 [BX]
NEXT
END-CODE

: ?DUP ( w -- w w, 0 ) DUP IF DUP THEN ;

: NIP ( w w -- w ) SWAP DROP ;

: ROT ( w1 w2 w3 -- w2 w3 w1 ) >R SWAP R> SWAP ;

: 2DROP ( w w -- ) DROP DROP ;

: 2DUP ( w1 w2 -- w1 w2 w1 w2 ) OVER OVER ;

.( Logic )

CODE 0< ( n -- t )
POP AX
CWD
PUSH DX
NEXT
END-CODE

CODE AND ( w w -- w )
POP BX
POP AX
AND BX, AX
PUSH BX
NEXT
END-CODE

CODE OR ( w w -- w )
POP BX
POP AX
OR BX, AX
PUSH BX
NEXT
END-CODE

CODE XOR ( w w -- w )
POP BX
POP AX
XOR BX, AX
PUSH BX
NEXT
END-CODE

: INVERT ( w -- w ) -1 XOR ;

.( Arithmetic )

CODE UM+ ( u u -- u cy ) \ or ( u u -- ud )
XOR CX, CX
POP BX
POP AX
ADD AX, BX
RCL CX, # 1 \ pick up carry
PUSH AX
PUSH CX
NEXT
END-CODE

: + ( u u -- u ) UM+ DROP ;

: NEGATE ( n -- -n ) INVERT 1 + ;
: DNEGATE ( d -- -d ) INVERT >R INVERT 1 UM+ R> + ;

: - ( w w -- w ) NEGATE + ;

: ABS ( n -- +n ) DUP 0< IF NEGATE THEN ;

.( User variables )

: doUSER ( -- a ) R> @ UP @ + ; COMPILE-ONLY

: doVAR ( -- a ) R> ; COMPILE-ONLY

8 \ start offset

DUP USER SP0 1 CELL+ \ initial data stack pointer
DUP USER RP0 1 CELL+ \ initial return stack pointer

DUP USER '?KEY 1 CELL+ \ character input ready vector
DUP USER 'EMIT 1 CELL+ \ character output vector

DUP USER 'EXPECT 1 CELL+ \ line input vector
DUP USER 'TAP 1 CELL+ \ input case vector
DUP USER 'ECHO 1 CELL+ \ input echo vector
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

: U< ( u u -- t ) 2DUP XOR 0< IF NIP 0< EXIT THEN - 0< ;
: < ( n n -- t ) 2DUP XOR 0< IF DROP 0< EXIT THEN - 0< ;

: MAX ( n n -- n ) 2DUP < IF SWAP THEN DROP ;
: MIN ( n n -- n ) 2DUP SWAP < IF SWAP THEN DROP ;

: WITHIN ( u ul uh -- t ) OVER - >R - R> U< ;

.( Divide )

: UM/MOD ( udl udh un -- ur uq )
2DUP U<
IF NEGATE 15
FOR >R DUP UM+ >R >R DUP UM+ R> + DUP
R> R@ SWAP >R UM+ R> OR
IF >R DROP 1 + R> ELSE DROP THEN R>
NEXT DROP SWAP EXIT
THEN DROP 2DROP -1 DUP ;

: M/MOD ( d n -- r q ) \ floored
DUP 0< DUP >R
IF NEGATE >R DNEGATE R>
THEN >R DUP 0< IF R@ + THEN R> UM/MOD R>
IF SWAP NEGATE SWAP THEN ;

: /MOD ( n n -- r q ) OVER 0< SWAP M/MOD ;
: MOD ( n n -- r ) /MOD DROP ;
: / ( n n -- q ) /MOD NIP ;

.( Multiply )

: UM* ( u1 u2 -- ud )
0 SWAP ( u1 0 u2 ) 15
FOR DUP UM+ >R >R DUP UM+ R> + R>
IF >R OVER UM+ R> + THEN
NEXT ROT DROP ;

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
FOR AFT DUP R@ + C@ BL XOR
IF R> 1 + EXIT THEN THEN
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

: do$ ( -- a )
R> R@ R> COUNT + ALIGNED >R SWAP >R ; COMPILE-ONLY

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
: ( ( -- ) [ CHAR ) ] LITERAL PARSE 2DROP ; IMMEDIATE
: \ ( -- ) #TIB @ >IN ! ; IMMEDIATE

: CHAR ( -- c ) BL PARSE DROP C@ ;
: CTRL ( -- c ) CHAR $001F AND ;

: TOKEN ( -- a \ )
BL PARSE 31 MIN NP @ OVER - 2 - PACK$ ;

: WORD ( c -- a \ ) PARSE HERE PACK$ ;

.( Dictionary Search )

: NAME> ( na -- ca ) 2 CELLS - @ ;

: SAME? ( a a u -- a a f \ -0+ )
FOR AFT OVER R@ CELLS + @
OVER R@ CELLS + @ - ?DUP
IF R> DROP EXIT THEN THEN
NEXT 0 ;

: find ( a va -- ca na, a F )
SWAP \ va a
DUP C@ 2 / temp ! \ va a \ get cell count
DUP @ >R \ va a \ count byte & 1st char
CELL+ SWAP \ a' va
BEGIN @ DUP \ a' na na
IF DUP @ [ =MASK ] LITERAL AND R@ XOR \ ignore lexicon bits
IF CELL+ -1 ELSE CELL+ temp @ SAME? THEN
ELSE R> DROP EXIT
THEN
WHILE 2 CELLS - \ a' la
REPEAT R> DROP NIP 1 CELLS - DUP NAME> SWAP ;

: NAME? ( a -- ca na, a F )
CONTEXT DUP 2@ XOR IF 1 CELLS - THEN >R \ context<>also
BEGIN R> CELL+ DUP >R @ ?DUP
WHILE find ?DUP
UNTIL R> DROP EXIT THEN R> DROP 0 ;

.( Terminal )

: ^H ( b b b -- b b b ) \ backspace
>R OVER R@ < DUP
IF [ CTRL H ] LITERAL 'ECHO @EXECUTE THEN R> + ;

: TAP ( bot eot cur key -- bot eot cur )
DUP 'ECHO @EXECUTE OVER C! 1 + ;

: kTAP ( bot eot cur key -- bot eot cur )
DUP 13 XOR
IF [ CTRL H ] LITERAL XOR IF BL TAP ELSE ^H THEN EXIT
THEN DROP NIP DUP ;

: accept ( b u -- b u )
OVER + OVER
BEGIN 2DUP XOR
WHILE KEY DUP BL - 95 U<
IF TAP ELSE 'TAP @EXECUTE THEN
REPEAT DROP OVER - ;

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
XOR BX, BX
MOV DL, # $0FF \ input
MOV AH, # 6 \ MS-DOS Direct Console I/O
INT $021
0<> IF \ ?key ready
OR AL, AL
0= IF \ ?extended ascii code
INT $021
MOV BH, AL \ extended code in msb
ELSE MOV BL, AL
THEN
PUSH BX
MOVE BX, # -1
THEN
PUSH BX
NEXT
END-CODE

CODE TX! ( c -- )
POP DX
CMP DL, # $0FF
0= IF \ do NOT allow input
MOV DL, # 32 \ change to blank
THEN
MOV AH, # 6 \ MS-DOS Direct Console I/O
INT $021
NEXT
END-CODE

: !IO ( -- ) ; IMMEDIATE \ initialize I/O device

.( Shell )

: PRESET ( -- ) SP0 @ SP! [ =TIB ] LITERAL #TIB CELL+ ! ;

: XIO ( a a a -- ) \ reset 'EXPECT 'TAP 'ECHO 'PROMPT
[ ' accept ] LITERAL 'EXPECT !
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
BEGIN [ ' que ] LITERAL CATCH ?DUP
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

: FOR ( -- a ) COMPILE >R HERE ; IMMEDIATE
: BEGIN ( -- a ) HERE ; IMMEDIATE
: NEXT ( a -- ) COMPILE next , ; IMMEDIATE
: UNTIL ( a -- ) COMPILE ?branch , ; IMMEDIATE
: AGAIN ( a -- ) COMPILE branch , ; IMMEDIATE
: IF ( -- A ) COMPILE ?branch HERE 0 , ; IMMEDIATE
: AHEAD ( -- A ) COMPILE branch HERE 0 , ; IMMEDIATE
: REPEAT ( A a -- ) [COMPILE] AGAIN HERE SWAP ! ; IMMEDIATE
: THEN ( A -- ) HERE SWAP ! ; IMMEDIATE
: AFT ( a -- a A ) DROP [COMPILE] AHEAD [COMPILE] BEGIN SWAP ; IMMEDIATE
: ELSE ( A -- A ) [COMPILE] AHEAD SWAP [COMPILE] THEN ; IMMEDIATE
: WHILE ( a -- A a ) [COMPILE] IF SWAP ; IMMEDIATE

: ABORT" ( -- \ ) COMPILE abort" $," ; IMMEDIATE

: $" ( -- \ ) COMPILE $"| $," ; IMMEDIATE
: ." ( -- \ ) COMPILE ."| $," ; IMMEDIATE

.( Name Compiler )

: ?UNIQUE ( a -- a )
DUP NAME? IF ." reDef " OVER .$ THEN DROP ;

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

