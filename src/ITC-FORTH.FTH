CR .( ITC-FORTH.FTH  make an indirect threaded system  Oct 2024 Fox)
 
COMPILER WARNINGS OFF
\ minimal system requires these files 
INCLUDE DSK7.EXECUTORS  \ load EXIT DOCOL DOVAR ETC.
INCLUDE DSK7.IMPORT     \ *NEW* import needs DOVAR so goes here
INCLUDE DSK7.ITCTYPES   \ CONSTANT VARIABLE : ;  S"
INCLUDE DSK7.BRANCHING  \ compilers for IF THEN BEGIN AGAIN...
INCLUDE DSK7.PREAMBLE   \ create program entry word, COLD
\ -------------------------------------------------------

\ Extras go here
 INCLUDE DSK7.PRIMITIVES \ TARGET CORE primitives
 INCLUDE DSK7.RSTACK     \ IMPORTs needed for FOR NEXT

COMPILER WARNINGS ON

\ INCLUDE DSK7.FORNEXT    \ runtime code

CR .( ITC FORTH loaded )

