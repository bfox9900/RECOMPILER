\ TEXTMODE.FTH for the recompiler   Sept 18 2012 Fox

COMPILER HEX 
TARGET 
: TEXT    ( -- )
      F0 DUP 83D4 C!
      ( -- F0) 01 VWTR
      0  2 VWTR  \ set VDP screen page
      VTOP OFF  \ topline VDP offset
      VPG  OFF  \ VDP screen page offset
      17  7 VWTR  \ sets FG & BG color
      28 C/L!
      0 0 AT-XY
      2 VMODE !   \ 2=ID for 40 column "TEXT" mode
      PAGE
;
