

0 [IF]

: LOOKUP
      S" EXIT" PAD PLACE
      CR ." Lookup: "   PAD COUNT TYPE SPACE
      KEY DROP
      PAD FIND SWAP U. U. ;

: TESTER   TEST LOOKUP ;
: TEST-EXECUTE  ['] TESTER JOB !    JOB @ EXECUTE ; \ passed
: TEST-PERFORM  ['] TESTER JOB !    JOB EXECUTE ;   \ passed

[THEN]
COMPILER HEX

TARGET
0 [IF]

: EXPECT     ( caddr +n -- +n')
        OVER + OVER         \ removed 1-  to accept all chars
\        BEGIN
            41 DUP 0D <>
\        WHILE
            DUP EMIT
            DUP 8 =
            IF  ( backspace )
                DROP             \ throw away the key
              \  1-  3 PICK UMAX  \ changed to use: 3 PICK   B.F.
            ELSE
                OVER C!       \ write char to caddr
                1+ OVER    UMIN  \ incr address and limit
            THEN
\        REPEAT
        DROP NIP SWAP - ;

: QUIT-TEST
      BEGIN
        TIB DUP 80 EXPECT
        .S
        KEY DROP
        CR TYPE
        CR ." HELP!!! I'm stuck in a loop ...  "
      AGAIN ;

[THEN]
