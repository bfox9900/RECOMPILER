\ TEST-DOLOOP.FTH  Sep 2023 Fox

HEX A000 ORG   \ this must be set before compiling any code

INCLUDE DSK7.ITC-FORTH \ preamble for indirect threaded Forth

INCLUDE DSK7.FORNEXT

COMPILER HEX
TARGET
CODE BYE   ( -- )
    0 LIMI,
    83C4 @@ CLR,
    0000 @@ BLWP,
ENDCODE

TARGET
: MAIN   FFFF FOR  NEXT BYE ;

\ directives to patch image and save program
COMPILER
PREAMBLE COLD
BOOT AUTOSTART MAIN
END.

SAVE DSK7.TESTFOR

HOST
