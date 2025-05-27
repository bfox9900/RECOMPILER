\ HELLO.FTH  for the recompiler.  Demo Sep 16 2023 Fox
COMPILER
NEW
HEX A000 ORG   \ this must be set before compiling any code

INCLUDE DSK7.ITC-FORTH \ preamble for indirect threaded Forth
INCLUDE DSK7.STD-OUT
INCLUDE DSK7.FORNEXT

IMPORT: ?TERMINAL

COMPILER DECIMAL
TARGET
: MAIN  ( -- )
   768 C/SCR !     \ init this variable
   PAGE
   BEGIN
     S" Hello world  " TYPE
    ?TERMINAL
   UNTIL
   BYE
;

\ tell the compiler what to do with this
COMPILER
AUTOSTART MAIN
SAVE DSK7.HELLOWORLD

HOST  \ return to HOST Forth

\ you could automatically exit to TI-99 Main page
( BYE )
