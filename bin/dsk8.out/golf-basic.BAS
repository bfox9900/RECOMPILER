1 CALL SCREEN(9)
2 FOR I=1 TO 8
5 CALL COLOR(I,16,9)
8 NEXT I
100 CALL CLEAR
110 PRINT "        COMPU-GOLF": : : : : : : : : :
120 PRINT "  JEFF RICKEL/DAVID ENGEL": : : : :
130 FOR I=1 TO 500
140 NEXT I
150 CALL CLEAR
160 INPUT "INSTRUCTIONS (Y OR N)?":Y$
170 IF Y$="N" THEN 300
180 CALL CLEAR
190 PRINT "    COMPU-GOLF SIMULATES      MINIATURE GOLF.  TO HIT     THE BALL, INPUT FIRST"
200 PRINT "  THE ANGLE (1-8), WITH       EACH HAVING THE FOLLOW-     ING DIRECTIONS:"
210 PRINT "     1 - W      2 - NW           3 - N      4 - NE           5 - E      6 - SE"
220 PRINT "     7 - S      8 - SW": : : : :
230 PRINT "  HIT ANY KEY TO CONTINUE."
240 CALL KEY(0,X,Y)
245 IF Y=0 THEN 240
250 CALL CLEAR
260 PRINT "  NEXT, INPUT THE SPEED       (0-9) OF THE BALL.          NOTE--THE ENTER KEY"
270 PRINT "  NEED NOT BE USED WHEN       ENTERING ANGLE AND          SPEED.  AFTER BOTH PLAY-"
280 PRINT "  ERS HAVE FINISHED THE       HOLE, HIT ANY KEY TO        CONTINUE THE NINE HOLES."
285 PRINT : : : : :"    HIT ANY KEY TO BEGIN"
290 CALL KEY(0,X,Y)
295 IF Y=0 THEN 290
300 CALL CLEAR
310 CALL SCREEN(9)
320 CALL COLOR(1,3,9)
330 CALL COLOR(2,3,9)
340 FOR I=3 TO 8
350 CALL COLOR(I,16,9)
360 NEXT I
370 CALL COLOR(9,16,3)
380 CALL COLOR(10,14,3)
390 CALL CHAR(39,"FFFFFFFFFFFFFFFF")
400 CALL CHAR(40,"0")
410 CALL CHAR(42,"0")
420 CALL CHAR(41,"000103070F1F3F7F")
430 CALL CHAR(43,"FEFCFAF0E0C080")
440 CALL CHAR(44,"0")
450 CALL CHAR(46,"0")
460 CALL CHAR(45,"0080C0E0F0F8FCFE")
470 CALL CHAR(47,"7F3F1F0F070301")
480 CALL CHAR(96,"181E1F1E18101010")
490 CALL CHAR(97,"3C7EFFFFFFFF7E3C")
500 CALL CHAR(104,"00183C7E7E3C18")
510 DIM D(8,2),A$(4,8)
520 DIM F1(2),F2(2)
530 DIM TT(2),SC(9,2)
540 FOR I=1 TO 8
550 READ D(I,1),D(I,2)
560 NEXT I
570 DATA -1,0,-1,-1,0,-1,1,-1,1,0,1,1,0,1,-1,1
580 FOR I=1 TO 4
590 FOR J=1 TO 8
600 READ A$(I,J)
610 NEXT J
620 NEXT I
630 FOR A7=1 TO 9
640 SC(A7,1)=0
650 SC(A7,2)=0
660 NEXT A7
670 F1(1)=0
680 F1(2)=0
690 F2(1)=0
700 F2(2)=0
710 FOR HH=1 TO 9
720 RANDOMIZE
730 CALL SOUND(2400,-4,2)
740 A4=INT(RND*4+1)
750 FOR C=1 TO 8
760 PRINT A$(A4,C);
770 NEXT C
780 CALL HCHAR(20,19,HH+48)
790 DATA ",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,*,,,,,,,,,,,,,,.,,,,,,,,,,,*)''''''''''''''-.,"
800 DATA "((((((((((''''''''''''''''-.(((((((((('''''''''''''''''((((((((((('''''''''''''''''("
810 DATA "((((((((((''''''+,,,,,/''''(((((*,,,,)''''''((*,,,)''''((((*)'''''''''''(*)''''''''("
820 DATA "((*)''''''''''''(('''''''''((*)''''''''''''+*('''''''''(*)''''''''''''+*(('''''''''("
830 DATA "('''''''+,,,,,*((('''''''''(('''''''(((((((((('''''''''(('''''''(((((((((('''''''''("
840 DATA "('''''''(((((((((('''''''''(('''''''(((((((((('''''''''(('''''''(((((((((('''''''''("
850 DATA "('''''''(((((((((./'''''''+*('''''''((((((((((.,,,,,,,*(('''''''(  HOLE     ANGLE   "
860 DATA "('''''''(  PLYR     SPEED   ('''''''(  1                .,,,,,,,*  2                "
870 DATA ",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,*,,,,,,,,,,,,,,,,,,,,,.,,,*)''''''''''''''''''''''-."
880 DATA ",*)''''''''''''''''''''''''(*)'''''''''''''''''''''''''(('''''''''+,,,,,/''''''''''("
890 DATA "('''''''''(((((((''''''''''(('''''''''(((((((''''''''''(('''''''''(((((((''''''''''("
900 DATA "('''''''''(((((((''''''''''(('''''''''(((((((''''''''''(('''''''''(((((((''''''''''("
910 DATA "('''''''''(((((((''''''''''(('''''''''(((((((''''''''''(('''''''''(((((((''''''''''("
920 DATA "('''''''''(((((((''''''''''(('''''''''(((((((''''''''''(('''''''''((((((./''''''''+*"
930 DATA "('''''''''(((((((.,,,,,,,,*,('''''''''(HOLE    ANGLE    ('''''''''(PLYR    SPEED    "
940 DATA "('''''''''(1                .,,,,,,,,,*2                                            "
950 DATA ",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,*,,,,,,,,,.,,,*,,.*,,,,,.,(*)'''''''''-.,*)''-)'''''-."
960 DATA "(('''''''''''((('''''''''''((.,/'''''''''((('''''''''''((((.,/'''''''-,)'''''''''''("
970 DATA "(((((('''''''''''''''''''''((((*,)''''+/'''''''''''''''((*,)''''''(.,/'''''''''''''("
980 DATA "*)''''''''(((.,/'''''''''''(('''''''''(((((('''''''''''(('''''''+,.((((('''''''''''("
990 DATA "('''''+,.((((((('''''''''''(('''''(((((((((('''''''''''(('''''(((((((((('''''''''''("
1000 DATA "('''''-,.((((((('''''''''''(('''''''-,.((((('''''''''''(('''''''''(((((./'''''''''+*"
1010 DATA "('''''''''((((((.,,,,,,,,,*(('''''''''(HOLE     ANGLE   ('''''''''(PLYR     SPEED   "
1020 DATA "('''''''''(1                ('''''''''(2                .,,,,,,,,,*                 "
1030 DATA ",,,,,,,,,,,,,,,,,,,,,,,,,,,,*,,,,,,,,,.,,,,,*,,,,,,,,,.,('''''''''(((((*)'''''''''-."
1040 DATA "('''''''''(((((('''''''''''(('''''''''(((((('''''''''''(('''''''''(((((('''''''''''("
1050 DATA "('''''''''-,,,.('''''''''''(('''''''''''''-('''''''''''((''''''''''''''('''''''''''("
1060 DATA ".,,,,,,,,,,,/''(/''''''''''(,,,,,,,,*,,,)''-)''''''''''(,,,,,,,*)''''''''''''''''''("
1070 DATA "((((((*)'''''''''''''''''''((((((*)'''+,,,,/'''''''''''(((((*)''''(((((('''''''''''("
1080 DATA "(((*)'''''(((((('''''''''''(((*)''''''(((((('''''''''''((*)'''''''(((((./'''''''''+*"
1090 DATA "*)''''''''((((((.,,,,,,,,,*(('''''''''(HOLE     ANGLE   ('''''''''(PLYR     SPEED   "
1100 DATA "('''''''''(1                ('''''''''(2                .,,,,,,,,,*                 "
1110 FOR L1=1 TO 9
1120 IF SC(L1,1)=0 THEN 1160
1130 CALL HCHAR(22,16+L1,SC(L1,1)+48)
1140 CALL HCHAR(23,16+L1,SC(L1,2)+48)
1150 NEXT L1
1160 FOR A8=1 TO 2
1170 CALL HCHAR(21+A8,28,F1(A8)+48)
1180 CALL HCHAR(21+A8,29,F2(A8)+48)
1190 NEXT A8
1200 X=INT(RND*3+23)
1210 Y=INT(RND*5)+11
1220 CALL HCHAR(Y,X,96)
1230 CALL HCHAR(Y+1,X,97)
1240 FOR PP=1 TO 2
1250 CALL HCHAR(21,19,PP+48)
1260 X=INT(RND*5)+5
1270 Y=22
1280 CALL HCHAR(22,X,104)
1290 T=0
1300 S=0
1310 G=3
1320 CALL HCHAR(20,29,T+48)
1330 CALL HCHAR(21,29,S+48)
1340 CALL HCHAR(20,29,32)
1350 CALL HCHAR(20,29,T+48)
1360 CALL KEY(0,N,M)
1370 IF M<1 THEN 1340
1380 T=N-48
REM T = angle **********************
1390 IF T>0 THEN 1420
1400 T=0
1410 GOTO 1340
1420 IF T>8 THEN 1400
1430 CALL HCHAR(20,29,T+48)
1440 CALL HCHAR(21,29,32)
1450 CALL HCHAR(21,29,S+48)
1460 CALL KEY(0,N,M)
1470 IF M<1 THEN 1440
1480 S=N-48
REM S = Speed
1490 IF S>-1 THEN 1520
1500 S=0
1510 GOTO 1440
1520 IF S>9 THEN 1500
1530 CALL HCHAR(21,29,S+48)
1540 IF S>0 THEN 1570
1550 N=3
1560 GOTO 1580
1570 N=INT(RND*4)+1+S*4
1580 FOR C=N TO 1 STEP -1
1590 U=X+D(T,1)
1600 V=Y+D(T,2)
1610 CALL GCHAR(V,U,K)
1620 G=G+1
1630 IF K=39 THEN 2040
1640 Q2=Q
1650 IF K>47 THEN 2140
1660 IF K<>41 THEN 1700
1670 Q=41
1680 K=42
1690 GOTO 1770
1700 IF K<>45 THEN 1740
1710 Q=45
1720 K=46
1730 GOTO 1840
1740 IF K<>43 THEN 1810
1750 Q=43
1760 K=42
1770 IF T=4 THEN 1790
1780 IF T<>8 THEN 1870
1790 G=1
1800 GOTO 1870
1810 IF K<>47 THEN 1870
1820 Q=47
1830 K=46
1840 IF T=2 THEN 1860
1850 IF T<>6 THEN 1870
1860 G=1
1870 Z=K-34 REM ASCII Code 34 DOUBLE QUOTE
1880 T=Z-T
1890 IF T>0 THEN 1910
1900 T=T+8
1910 IF T<9 THEN 1930
1920 T=T-8
1930 U=X+D(T,1)
1940 V=Y+D(T,2)
1950 IF G<>2 THEN 2020
1960 IF C<>N THEN 2020
1970 CALL GCHAR(V,U,K)
1980 IF K=39 THEN 2020
1990 Q=Q2
2000 T=9-T
2010 GOTO 1930
2020 CALL HCHAR(20,29,T+48)
2030 CALL SOUND(50,880,2)
2040 IF G<>2 THEN 2070
2050 CALL HCHAR(Y,X,Q)
2060 GOTO 2080
2070 CALL HCHAR(Y,X,39)
2080 CALL HCHAR(V,U,104)
2090 X=U
2100 Y=V
2110 NEXT C
2120 SC(HH,PP)=SC(HH,PP)+1
2130 GOTO 1320
2140 IF C<6 THEN 2250
2150 T=INT(RND*8)+1
2160 O=U+D(T,1)
2170 P=V+D(T,2)
2180 CALL GCHAR(P,O,K)
2190 CALL SOUND(50,440,2)
2200 IF K=39 THEN 2220
2210 GOTO 2150
2220 U=O
2230 V=P
2240 GOTO 2040
2250 CALL HCHAR(Y,X,39)
2260 FOR F=410 TO 110 STEP -50
2270 CALL SOUND(50,F,2)
2280 NEXT F
2290 SC(HH,PP)=SC(HH,PP)+1
2300 IF SC(HH,PP)<10 THEN 2320
2310 SC(HH,PP)=9
2320 TT(PP)=TT(PP)+SC(HH,PP)
2330 CALL HCHAR(21+PP,16+HH,SC(HH,PP)+48)
2340 F1(PP)=INT(TT(PP)/10)
2350 F2(PP)=TT(PP)-10*F1(PP)
2360 CALL HCHAR(21+PP,28,F1(PP)+48)
2370 CALL HCHAR(21+PP,29,F2(PP)+48)
2380 NEXT PP
2390 CALL KEY(0,J,K)
2400 IF K<1 THEN 2390
2410 NEXT HH
2420 INPUT "PLAY AGAIN?":R$
2430 IF R$="YES" THEN 630
2440 IF R$="NO" THEN 2470
2450 PRINT "YES OR NO!"
2460 GOTO 2420
2470 CALL CLEAR
2480 END
