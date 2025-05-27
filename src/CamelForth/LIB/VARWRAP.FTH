
CODE 1+WRAP ( variable limit )
      R0 POP, 
      R0 ** INC, 
      R0 ** TOS CMP,
      HI IF, 
           R0 ** CLR, 
      ENDIF,   
      TOS POP,
      NEXT, 
ENDCODE         
