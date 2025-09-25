COMPILER
CR .( DO/LOOP primitives & compilers ...)
\ doloop.FTH  adds 104 bytes to the recompiled program

TARGET
CODE <?DO>  ( limit ndx -- )
        *SP TOS CMP,
        1 $ JNE,
        TOS POP,
        TOS POP,
        IP RPOP,
        NEXT,
+CODE <DO>  ( limit indx -- )
1 $:    R0  8000 LI,
        *SP+ R0  SUB,
        R0  TOS ADD,
        R0  RPUSH,
        TOS RPUSH,
        TOS POP,
        NEXT,
ENDCODE

CODE <+LOOP>
        TOS *RP ADD,
        TOS POP,
        2 $ JMP,

+CODE <LOOP>
        *RP INC,
2 $:    1 $ JNO,
        IP INCT,
        3 $ JMP,

1 $:    *IP IP ADD,
        NEXT,

+CODE UNLOOP
3 $:    RP  4 AI,
        NEXT,
ENDCODE

CODE I  ( -- n)
        TOS PUSH,
        *RP    TOS MOV,
        2 (RP) TOS SUB,
        NEXT,
ENDCODE

CODE J      ( -- n)
        TOS PUSH,
        4 (RP) TOS MOV,   \ outer loop index is on the rstack
        6 (RP) TOS SUB,   \ index = loopindex - fudge
        NEXT,
ENDCODE

COMPILER  ALSO META DEFINITIONS
\  do loop cross-compilers
HOST: DO       TCOMPILE <DO>    THERE ;HOST IMMEDIATE
HOST: ?DO      TCOMPILE <?DO>   THERE ;HOST IMMEDIATE
HOST: LOOP     TCOMPILE <LOOP>  <BACK ;HOST IMMEDIATE
HOST: +LOOP    TCOMPILE <+LOOP> <BACK ;HOST IMMEDIATE

COMPILER