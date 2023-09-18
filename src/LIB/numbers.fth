\ N U M B E R   T O   S T R I N G   C O N V E R S I O N

TARGET

CODE >DIGIT  ( n -- c)   \ ASM is 9 bytes, 4X faster
      TOS 9 CMPI,
      HI IF,             \ if n>9
            TOS 7 ADDI,  \ number is not base 10, add 7
      ENDIF,
      TOS CHAR 0 ADDI,  \ add ASCII 0 to TOS, create char value
      NEXT,
      ENDCODE

: <#     ( --) PAD HP ! ;
: HOLD   ( char -- )  HP DUP 1-! @ C! ;
: #      ( u -- ud2 ) 0 BASE@ UM/MOD >R  BASE@ UM/MOD SWAP >DIGIT HOLD R> ;
: #S     ( ud1 -- ud2)  BEGIN  # 2DUP OR  WHILE REPEAT ;
: #>     ( ud1 -- c-addr u) 2DROP HP @ PAD OVER - ;
: SIGN   ( n -- ) 0< IF  [CHAR] -  HOLD  THEN ;
: UD.    ( d -- ) <#  #S  #> TYPE SPACE ;
: U.     ( u -- ) 0 UD. ;
: (.)    ( n -- caddr len)  DUP ABS 0 <#  #S ROT SIGN  #> ;
: .      ( n -- ) (.)  TYPE SPACE ;
