\ -TRAILING  remove trailing blanks (way to slow in Forth on 9900)
HEX
 CODE -TRAILING ( addr len -- addr len')  \ 8x FASTER than Forth
   C216 ,        \ *SP  W  MOV,   \ DUP addr in W
   A204 ,        \  TOS W  ADD,   \ add len
   0608 ,        \  W DEC,        \ w=address of last char
   0201 , 2000 , \  R1 2000 LI,   \ space char in R1
                 \  BEGIN,
   9058 ,        \    *W R1 CMPB, \ compare to ascii >20
   1603 ,        \  EQ WHILE,
   0608 ,        \     W   DEC,   \ move back 1 char
   0604 ,        \     TOS DEC,   \ dec len in TOS
   10FB ,        \  REPEAT,       \ 20 bytes
   NEXT,
ENDCODE          

\ : -TRAILING  ( adr len char -- adr len') \ remove trailing chars
\         2DUP + 1-
\         BEGIN
\            DUP C@ BL =   \ fetch last character
\         WHILE            \ test for BLANK
\            1-            \ while char is a match, decrement length
\         REPEAT
\         NIP OVER -      \ 28 bytes 
\ ;
