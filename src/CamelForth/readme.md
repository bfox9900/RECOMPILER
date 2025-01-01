# Camel99 Forth Source

This code will be used to rebuild the Camel99 kernel, using the Camel99 kernel.
The original Camel99 Forth is built using a cross-compiler build on HsForth for
DOS. It has always been my wish to make a Forth that could re-build itself, so
this is it. 

## How to Recompile Camel99 Forth

### Hardware requirements:  
- TI-99 console
- Peripheral expansion box (PEB) with 2 floppy disks
- 1M SAMS memory card
- Editor Assembler "Super" cartridge with 8K RAM available

### Software requirements
- Camel99 Forth diskette, with Library files
- RECOMPILER diskette with source code files 


### Re-build Process
- With the editor Assembler SuperCart plugged in, place the Camel99 diskette in DSK1.
- Press a key to enter the menu. 
- Select 2 for Editor/Assembler 
- Select 5 for RUN PROGRAM FILE 
- At the FILE NAME? prompt type DSK1.CAML99SC  
- You are now in Camel99 Forth. You should see the TI LOGO and the word READY. 

- Insert the RECOMPILER disk into the 2nd disk.
    - Note: If you have more than two disks, change the disk number to the one you use
- At the Forth console type INCLUDE DSK2.RECOMPILER 
- This job will take over 2 minutes...
- The program will compile, save itself as a binary program and then restart itself 
- You see a blue screen and with titles and the word READY 

- Insert the Camel99 source code diskette in DSK2. 
- At the recompiler console type:  INCLUDE DSK2.ITC-MAKE 
- This job will take over 3 minutes 

- When build is complete type FORTH BYE 
- Select option 5 RUN PROGRAM FILE from the menu
- At the prompt type DSK2.NEWCAMEL
- A Forth system should start 




