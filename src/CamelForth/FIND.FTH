[CC] CR .( DICTIONARY SEARCH)

\ We found the Camel Forth dictionary search to be too slow on TI-99.
\ Creating (FIND) in assembler is smaller and about 5X faster than using
\ S= plus hi-level looping and address calculation

\ Usage:
\  FIND ( c-addr -- caddr  0) if not found
\                   xt     1) if immediate,
\                   xt    -1) if "normal"
\             LATEST @ (FIND) ;

\ We chose to take advantage of the large TMS9900 register set.
\ Therefore the input arguments are held in 3 registers and are
\ transferred to 3 new registers for the string compare loop.
\ This guarantees the fastest reload for each new string compare.

\ Register Usage
\ Inputs:  R3 = traverses NFAs in the Forth dictionary
\          R8 = address of the counted string we are looking for
\          R5 = length of the counted string in R8 + 1 byte

\ string compare loop
\          R0 = number of characters to compare(search string length+1)
\          R1 = address of the 1st string to compare
\          R2 = address of the second string to compare

\ Outputs: R2 = address of found string -OR- address of search string on stack
\          R4 = Forth TOS register. Holds the true/false result flag

TARGET 
CODE (FIND) ( Caddr NFA -- XT ? )
  TOS R3 MOV,             \ R3 = NFA which is a counted string
  TOS CLR,                \ TOS is the output flag, init to zero
  *SP R8 MOV,             \ R8 = caddr which is a counted string
  *R8 R5 MOVB,            \ caddr C@ -> R5 is string length
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
      1 $ JNE,           \ ANY mismatch found, goto 1 $
      R0 DEC, EQ         \ decr. loop counter
    UNTIL,               \ loop until R0=0
  \ we found the word !!!
  \ convert NFA in R3 to CFA -> R2
    R3 R2 MOV,           \ R3 has a name field address (NFA), copy to R2
    *R3 R0 MOVB,         \ length of the name to R0
    R0 SWPB,             \ fix the byte order again
    R0 R2 ADD,           \ add length to R2, gets past the string to the CFA
    R2 INCT,             \ inc 1 for count byte, 1 more for even address
    R2 -2 ANDI,          \ align R2 to even address boundary
  \ test for immediate or normal word -> TOS
    TOS SETO,            \ we found a word so set TOS to true
    -1 (R3) R0 MOVB,     \ R3 has the NFA. Fetch NFA-1 (immediate field)

  \ Oct 2020 added mask to support hashed threads
    R0 0100 ANDI,        \ test the immediate bit, (mask the hash code)
    NE IF,
        TOS NEG,         \ if non zero negate the TOS from -1 to 1
    ENDIF,               \ and head for home
    R2 *SP MOV,          \ replace Caddr with the found XT in R2
    NEXT,                \ Return to Forth

    \ traverse link list to next NFA
1 $: -3 (R3) R3 MOV, EQ  \ fetch LFA, R3 now has new NFA
    UNTIL,
    NEXT,                \ we got zero. End of the list! Go back to Forth
ENDCODE                  \ 42 BYTES

