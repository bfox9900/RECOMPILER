\ GROM header in Forth   2024
\ This is just a bit trickier in Forth because we don't have
\ forward referencing. The solution is to use the data stack
\ to record the address that needs to be filled later.

  HERE       \ remember current dictionary pointer

  HEX
  6000 DP !  \ set dictionary pointer to cartridge space

  \ begin compiling the header into Super-Cart space
   AA C,     \ indicates a standard header
   01 C,     \ version number
   00 C,     \ number of programs (optional)
   00 C,     \ not used
   0000 ,    \ pointer to power-up list (can't use in cartridge rom)
   HERE    ( -- addr1 ) \ "pointer to program list" address on DATA stack.
   0000 ,    \ compile an empty pointer for now
   0000 ,    \ pointer to dsr list (unused)

\ Start of subprogram list
   HERE      \ "program chain record" ( -- addr1 addr2)
   0000 ,    \ compile zero ie: No next item.

   SWAP ( -- addr2 addr1) !  \ Patch address2 into address1

  A000 ,                 \ * PROGRAM START ADDRESS FOR THIS MENU ITEM
  S" CAMEL99 FORTH" S,   \ Compile program name as a counted string
  ALIGN                  \ align to even address

  DP !       \ restore original dictionary pointer
