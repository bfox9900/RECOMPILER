\ T I - 9 9   S C R E E N   C O N T R O L  P R I M I T I V E S
\ These assembler routines manage the screen math at machine speed.
\ They are smaller and faster in ASM while still easy to understand.
\ Using these primitives and the VDP words CAMEL99 creates the rest of
\ video screen control in Forth.
\ We took a Forth approach to naming VDP memory routines.
\ So TI name, VSBR, becomes VC@  and VSBW becomes VC!.
\ VMBW and VMBR become VWRITE AND VREAD.
\ We also added a new one V! and V@ that load and store 16 bit numbers to VDP.

\ I embraced the Forth paradigm so these routines are created as
\ Forth words with stack based parameter passing. This gives us the freedom
\ to call these fast routines directly from Forth.
\ With these fast primitives we can write most of the VDP driver
\ in Forth and still get excellent performance.

\ Aug 31 2018,
\ CHANGED to read user variables correctly, via the workspace pointer WP.
\ 9900 Indexed addressing mode makes this almost as fast as indirect addressing
\ the variables.  It's all Multi-tasking friendly now
\ April 2020  On advice of Tursi, Atariage, I disable interrupts when VDP
\             address is set then restore them at the end of the routine.
\ Feb 2021    - Changed all address setting to use index addressing for
\              odd numbered bytes loading into VDP ports.
\ Jan 2022    - use address setter sub-routines for all words. Saved 62 bytes
\             - TOS is VDP address for all routines

[CC] CR ." V D P  C H I P  I N T E R F A C E"

\ Register Usage:
\ R0  down counter
\ R1  current workspace
\ R3  VDP data port
\ R4  VDP memory address

\
\ ================ LEAF SUB-ROUTINE  ** NOT NESTABLE ** ===================
