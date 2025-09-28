[CC] CR .( DICTIONARY SEARCH)

TARGET
CODE (FIND) ( Caddr NFA -- XT ? )
      TOS R3 MOV,                 \ R3 = NFA which is a counted string
      TOS CLR,                    \ TOS is the output flag, init to zero
     *SP R8 MOV,                  \ R8 = caddr which is a counted string
     *R8 R5 MOVB,                 \ caddr C@ -> R5 is string length
      R5 8 SRL,               \ get the byte on the correct side right
      R5 INC,                 \ skip length byte
      BEGIN,
           \ load char compare registers
            R5 R0 MOV,           \ load R0 with length of caddr string
            R8 R1 MOV,           \ load R1 with caddr string address
            R3 R2 MOV,           \ load R2 with the NFA to compare
\ inner character comparator loop
            BEGIN,
              *R1+ *R2+ CMPB,    \ compare char by char including the length byte
            1 $ JNE,          \ ANY mismatch found, goto @@1
              R0 DEC,           \ decr. loop counter
            EQ UNTIL,            \ loop until R0=0
\ we found the word !!!

\ convert NFA in R3 to CFA -> R2
            R3 R2 MOV,
            *R3 R0 MOVB,
            R0 SWPB,    \ fix the #$%!@$ byte order again
            R0 R2 ADD,  \ add length to R2, gets past the string to the CFA
            R2 INCT,    \ inc 1 count byte, +1 for address evaluation
            R2 -2 ANDI,  \ align R2 to even address boundary

\ test for immediate or normal word -> TOS
            TOS SETO,       \ we found a word so set TOS to -1
            -1 (R3) R0 MOVB, \ R3 has the NFA. Fetch NFA-1 (immediate field)
            \ Oct 2020 added mask to support hashed threads
            R0 0100 ANDI,  \ test the immediate bit, (mask the hash code)
            NE IF,
                TOS ABS, \ if non zero change TOS from -1 to 1
            ENDIF,       \ and head for home
            R2 *SP MOV,  \ replace Caddr with the found XT in R2
            NEXT,        \ Return to Forth

\ traverse link list to next NFA
1 $:       -3 (R3) R3 MOV,  \ fetch LFA, R3 now has new NFA
      EQ UNTIL,
      NEXT,
ENDCODE
