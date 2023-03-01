      PROGRAM STAN                                                            10
C                                                                             20
C     THIS PROGRAM IS KNOWN AS SAA STAN AND IS A SPECIALLY MODIFIED           30
C     VERSION OF THE PROGRAM AUS STAN DETAILED IN ARRB PUBLICATION            40
C     ARR 106 (1980) WHICH IS THE AUSTRALIAN VERSION OF THE CIE               50
C     STANDARD LUMINANCE PROGRAM, STAN, DETAILED IN CIE PUBLICATION           60
C     30-2 (1982).  SAA STAN HAS BEEN DEVELOPED TO MEET THE SPECIFIC          70
C     NEEDS OF THE STANDARDS ASSOCIATION OF AUSTRALIA.                        80
C                                                                             90
C     FOR INFORMATION ON THE USE OF THIS PROGRAM PLEASE REFER TO             100
C     AUSTRALIAN STANDARD AS 1158.2 (1986).  INPUT/OUTPUT UNIT NUMBERS       110
C     ARE: INPUT= UNIT 5, OUTPUT= UNIT 6, AND UNIT 4 IS A SCRATCH FILE.      120
C                                                                            130
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                    140
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                  150
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                       160
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                        170
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                      180
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                    190
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                           200
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                        210
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                           220
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                  230
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                   240
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                                250
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                  260
      OPEN(4,STATUS='SCRATCH')                                               270
      OPEN(5,FILE='SAADAT')                                                  280
      OPEN(6,FILE='SAAOUT')                                                  290
      REWIND 5                                                               300
      REWIND 6                                                               310
      NRUN=1                                                                 320
C                                                                            330
C         DEFINITION OF VARIOUS CONSTANTS AND SAA DATA                       340
C                                                                            350
      CALL CONS3                                                             360
      CALL SAADAT                                                            370
C                                                                            380
C         INPUT JOB DEFINING DATA                                            390
C                                                                            400
      CALL SAAINP(I,JMAX,KMAX,LMAX,IFLAG)                                    410
      DO 10 J=1,JMAX                                                         420
      INST=1                                                                 430
      DO 20 K=1,KMAX                                                         440
      DO 20 L=1,LMAX                                                         450
      IF(MODE-2) 1,2,3                                                       460
C                                                                            470
C     CREATE SAA TYPE INSTALLATION DATA FOR A MODE 1 JOB                     480
C                                                                            490
    1 CALL CREATE(I,J,K,L,IFLAG)                                             500
    2 LIN=4                                                                  510
C                                                                            520
C     TRANSFORM SAA TYPE INSTALLATION DATA INTO CIE TYPE DATA                530
C                                                                            540
      CALL TFORM                                                             550
C                                                                            560
C     INPUT/READ CIE TYPE INSTALLATION DATA - ALL MODES                      570
C                                                                            580
    3 CALL INPU3                                                             590
      LIN=5                                                                  600
C                                                                            610
C         INPUT R- AND I-TABLES                                              620
C                                                                            630
      IF(MODE.EQ.3) CALL RINP3                                               640
      IF(INST.EQ.1.AND.NRUN.EQ.1) CALL LINP3                                 650
C                                                                            660
C         OUTPUT GEOMETRICAL AND OTHER DATA ON LIST-DEVICE                   670
C                                                                            680
      IF(MODE.GT.1) CALL OUT13                                               690
      IF(MODE.EQ.1.AND.INST.EQ.1) CALL OUT14                                 700
C                                                                            710
C         FOR EACH OBSERVER POSITION AND CORRESPONDING CALCULATION           720
C         POINTS , AND WHERE APPROPRIATE :                                   730
C           1. COMPUTE LUMINANCE DISTRIBUTION (CALL DIST3)                   740
C           2. OUTPUT LUMINANCE DISTRIBUTION (CALL OUT23)                    750
C           3. COMPUTE AND OUTPUT LUMINANCE BASED CRITERIA (CALL QUAL3)      760
C           4. COMPUTE AND OUTPUT SURROUND ILLUMINANCE (CALL ILLUM)          770
C           5. COMPUTE AND OUTPUT GLARE CRITERIA (CALL GLAR3)                780
C           6. OUTPUT SAA LIGHT TECHNICAL PARAMETERS (CALL OUT24)            790
C                                                                            800
      DO 30 M = 1,NOBS                                                       810
      ROBS =OBCAL(M,1)                                                       820
      RCMIN=OBCAL(M,2)                                                       830
      DRC  =OBCAL(M,3)                                                       840
      NRC  =NRCAL(M)                                                         850
      CALL DIST3                                                             860
      IF(NRC.GT.1.AND.MODE.GT.1) CALL OUT23                                  870
      CALL QUAL3                                                             880
      IF(NRC.GT.1) CALL ILLUM                                                890
      IF(NRC.GT.1.AND.IGLAR.GT.0) CALL GLAR3                                 900
   30 CONTINUE                                                               910
      IF(MODE.EQ.1) CALL OUT24(INST)                                         920
      INST=INST+1                                                            930
   20 CONTINUE                                                               940
      NRUN=NRUN+1                                                            950
   10 CONTINUE                                                               960
      STOP                                                                   970
      END                                                                    980
      SUBROUTINE INPU3                                                       990
      DIMENSION A(5)                                                        1000
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                   1010
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                 1020
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                      1030
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                       1040
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                     1050
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                   1060
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                          1070
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                       1080
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                          1090
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                 1100
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                  1110
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                               1120
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                 1130
      IERR = 0                                                              1140
      NOBS = 0                                                              1150
      NRCM = 25                                                             1160
      NSCM = 40                                                             1170
      NOBSM = 5                                                             1180
      ISW1 = 0                                                              1190
      ISW2 = 0                                                              1200
      ISW3 = 0                                                              1210
      IGLAR=0                                                               1220
      EPSIL = 0.001                                                         1230
 1000 READ(LIN,1001)ITYP,(A(J),J=1,5)                                       1240
 1001 FORMAT(I2,5F7.0)                                                      1250
      IF(ITYP) 71,10,7                                                      1260
    7 IF(ITYP-5)8,60,71                                                     1270
    8 GOTO(20,30,40,50),ITYP                                                1280
C                                                                           1290
C         ITYP= 1  ROAD DESCRIPTION                                         1300
C                                                                           1310
   20 RNO = A(1)                                                            1320
      ALANE = A(2)                                                          1330
      LANE  = IFIX(ALANE)                                                   1340
      WLANE = A(3)                                                          1350
      WC    = A(4)                                                          1360
      NC = 5                                                                1370
      IF(A(5)) 21,22,21                                                     1380
   21 NC = 3                                                                1390
   22 ISW1 = 1                                                              1400
      GO TO 1000                                                            1410
C                                                                           1420
C         ITYP= 2   LIGHTING GEOMETRY                                       1430
C                                                                           1440
   30 H = A(1)                                                              1450
      DLUM = A(2)                                                           1460
      RA1 = A(3)                                                            1470
      RA2 = A(4)                                                            1480
      SW  = A(5)                                                            1490
      ISW2 = 1                                                              1500
      GO TO 1000                                                            1510
C                                                                           1520
C         ITYP= 3   LUMINAIRE CONSTANTS                                     1530
C                                                                           1540
   40 LNUMB = IFIX(A(1))                                                    1550
      VH1   = A(2)                                                          1560
      PHI = A(3)                                                            1570
      FACM= A(4)                                                            1580
      SINV  = SIN(RAPDG*VH1)                                                1590
      COSV  = COS(RAPDG*VH1)                                                1600
      ISW3  = 1                                                             1610
      GO TO 1000                                                            1620
C                                                                           1630
C         ITYP = 4  OBSERVATION- AND CALCULATION GEOMETRY (OPTIONAL)        1640
C                                                                           1650
   50 NOBS = NOBS+1                                                         1660
      IF(NOBS-NOBSM) 51,51,52                                               1670
   52 NOBS = NOBS-1                                                         1680
      CALL ERROR(11,LOUT)                                                   1690
      GO TO 1000                                                            1700
   51 OBCAL(NOBS,1) = A(1)                                                  1710
      OBCAL(NOBS,2) = A(2)                                                  1720
      OBCAL(NOBS,3) = A(3)                                                  1730
      IF(A(3)) 53,53,54                                                     1740
   53 NRCAL(NOBS) = 1                                                       1750
      GO TO 1000                                                            1760
   54 NRCAL(NOBS)=IFIX((A(4)-A(2))/A(3) +1.001)                             1770
      GO TO 1000                                                            1780
C                                                                           1790
C     ITYP=5 : GLARE CALCULATIONS WILL BE INCLUDED                          1800
C                                                                           1810
   60 IGLAR=1                                                               1820
      COULR=A(1)                                                            1830
      AREAL=A(2)                                                            1840
      GO TO 1000                                                            1850
C                                                                           1860
C         ITYP < = 0  TERMINATION OF GEOMETRICAL INPUT                      1870
C                                                                           1880
   10 CONTINUE                                                              1890
C     IS ANY OF THE RECORDS TYPE 1,2, OR 3 MISSING?                         1900
C     IF YES ERROR MESSAGE 12 AND STOP                                      1910
      IF(ISW1*ISW2*ISW3) 70,70,80                                           1920
   70 IERR= 1                                                               1930
      CALL ERROR(12,LOUT)                                                   1940
      GO TO 700                                                             1950
   71 IERR=1                                                                1960
      CALL ERROR(13,LOUT)                                                   1970
      GO TO 700                                                             1980
C                                                                           1990
C         INTERPRETATION OF INPUT                                           2000
C                                                                           2010
C         1.  GEOMETRICAL ARRANGEMENT OF LUMINAIRES                         2020
C                                                                           2030
C                                                                           2040
   80 NCAR = 1                                                              2050
      ROADM= 0.5*ALANE*WLANE                                                2060
      GEO(1,1) = RA1                                                        2070
      GEO(1,3) = 60.0                                                       2080
      NROW = 1                                                              2090
      D = RA1-ROADM                                                         2100
      IF(ABS(D)-EPSIL) 110,110,105                                          2110
  105 IF(D) 100,110,120                                                     2120
  100 GEO(1,2) = -1.0                                                       2130
      IF(RA2) 140,140,130                                                   2140
  110 GEO(1,2) = -1.0                                                       2150
      GO TO 140                                                             2160
  120 GEO(1,2) = 1.0                                                        2170
      GO TO 140                                                             2180
C         TWO ROWS                                                          2190
  130 NROW = 2                                                              2200
      GEO(2,1) = RA2                                                        2210
      GEO(2,2) = 1.0                                                        2220
C         STAGGERED?                                                        2230
      IF(SW) 160,160,170                                                    2240
  170 GEO(1,3) = 60.0+0.5*DLUM                                              2250
  160 GEO(2,3) = 60.0                                                       2260
C         CENTRAL RESERVE? IF YES GENERATE SYMMETRICAL ARRANGEMENT          2270
  140 IF(WC) 300,300,190                                                    2280
  190 NR1 = NROW                                                            2290
      NCAR = 2                                                              2300
      CENTM = ALANE*WLANE + 0.5*WC                                          2310
      DO 200 J=1,NR1                                                        2320
      IF(ABS(GEO(J,1)-CENTM) - EPSIL) 200,200,210                           2330
  210 NROW = NROW+1                                                         2340
      GEO(NROW,1) = 2.*CENTM-GEO(J,1)                                       2350
      GEO(NROW,2) = -1.*GEO(J,2)                                            2360
      GEO(NROW,3) = GEO(J,3)                                                2370
  200 CONTINUE                                                              2380
C                                                                           2390
C         2.  POSITIONS OF OBSERVERS AND CALCULATION FIELDS.                2400
C             IF THE TRANSVERSE POSITIONS HAS BEEN DEFINED THROUGH          2410
C             TYPE 4 RECORDS, ONLY THE LONGITUDINAL POSITIONS ARE           2420
C             CALCULATED                                                    2430
  300 SOBS = 0.0                                                            2440
      SCMIN= 60.0                                                           2450
      NSC  = 10                                                             2460
      DSC  = 0.1*DLUM                                                       2470
      IF(DSC-5.001) 310,310,320                                             2480
  320 W = DLUM/5.                                                           2490
      NSC = IFIX(W)                                                         2500
      W=DLUM-5.0*FLOAT(NSC)                                                 2510
      IF(ABS(W)-EPSIL)325,325,335                                           2520
  335 NSC = NSC+1                                                           2530
  325 DSC = DLUM/FLOAT(NSC)                                                 2540
      IF(NSC-NSCM) 310,310,345                                              2550
  345 IERR = 1                                                              2560
      CALL ERROR(5,LOUT)                                                    2570
  310 IF(NOBS) 330,330,500                                                  2580
  330 NOBS = LANE+1                                                         2590
      IF(NCAR-1) 350,350,360                                                2600
  350 IF(LANE-2) 380,370,380                                                2610
  380 NOBS = (LANE+1)/2+1                                                   2620
      GO TO 360                                                             2630
  370 NOBS = 1                                                              2640
  360 OBCAL(1,1) = 0.25*ALANE*WLANE                                         2650
      OBCAL(1,3) = WLANE/FLOAT(NC)                                          2660
      OBCAL(1,2) = 0.5*OBCAL(1,3)                                           2670
      NRCAL(1)   = NC*LANE                                                  2680
      IF(NOBS-1) 500,500,390                                                2690
  390 IF(NOBS-NOBSM) 400,400,410                                            2700
  410 CALL ERROR(11,LOUT)                                                   2710
      NOBS = 5                                                              2720
  400 W =-0.5*WLANE                                                         2730
      DO 340 J = 2,NOBS                                                     2740
      W = W+WLANE                                                           2750
      OBCAL(J,1) = W                                                        2760
      OBCAL(J,2) = W                                                        2770
      OBCAL(J,3)=0.0                                                        2780
      NRCAL(J)   = 1                                                        2790
  340 CONTINUE                                                              2800
  500 DO 490 J = 1,NOBS                                                     2810
      IF(NRCAL(J)-NRCM) 490,490,480                                         2820
  480 IERR=1                                                                2830
      CALL ERROR(4,LOUT)                                                    2840
  490 CONTINUE                                                              2850
C         TERMINATE EXECUTION                                               2860
  700 IF(IERR) 800,800,710                                                  2870
  710 CALL ERROR(8,LOUT)                                                    2880
      STOP                                                                  2890
  800 RETURN                                                                2900
      END                                                                   2910
      SUBROUTINE OUT13                                                      2920
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                   2930
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                 2940
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                      2950
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                       2960
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                     2970
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                   2980
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                          2990
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                       3000
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                          3010
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                 3020
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                  3030
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                               3040
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                 3050
C                                                                           3060
C      THE ROUTINE OUTPUTS AN EDITED LIST OF THE INPUT DATA                 3070
C                                                                           3080
      WRITE(LOUT,10) JOB                                                    3090
   10 FORMAT(1H1,21X,29HSAA STAN  -  AS 1158.2 (1986)/                      3100
     122X,29(1H*)//1X,18A4/1X,72(1H-)///                                    3110
     217H ROAD DESCRIPTION,/,                                               3120
     317H ----------------,/)                                               3130
      I= 1                                                                  3140
      IF(WC) 20,20,25                                                       3150
   25 I= 2                                                                  3160
   20 WRITE(LOUT,100)I                                                      3170
      IF(MODE.EQ.2) GO TO 5                                                 3180
      WRITE(LOUT,101)LANE                                                   3190
      WRITE(LOUT,102)WLANE                                                  3200
      GO TO 15                                                              3210
    5 WRITE(LOUT,99) WK                                                     3220
   99 FORMAT(24H CARRIAGEWAY WIDTH  (WK),9X,F5.2,3H  M)                     3230
   15 IF(I-1) 35,35,45                                                      3240
   45 WRITE(LOUT,103) WC                                                    3250
  100 FORMAT(20H NO. OF CARRIAGEWAYS,13X,I2)                                3260
  101 FORMAT(29H NO. OF LANES PER CARRIAGEWAY,4X,I2)                        3270
  102 FORMAT(14H WIDTH OF LANE,16X,F5.2,2H M)                               3280
  103 FORMAT(25H WIDTH OF CENTRAL MEDIAN ,5X,F5.2,2H M)                     3290
   35 WRITE(LOUT,104) RNO,(RSNAME(I),I=1,5)                                 3300
  104 FORMAT(1X,/,42H REFLECTION PROPERTIES GIVEN BY R-TABLE NO,F8.3,2X,    3310
     15A4,//)                                                               3320
      WRITE(LOUT,40)                                                        3330
   40 FORMAT(22H LIGHTING INSTALLATION,/,                                   3340
     1       22H ---------------------,//)                                  3350
      IF(MODE.EQ.2) GO TO 65                                                3360
      WRITE(LOUT,98) NROW                                                   3370
   98 FORMAT(21H THE INSTALLATION HAS,I3,21H ROW(S) OF LUMINAIRES,//)       3380
      GO TO 75                                                              3390
   65 I=ARR                                                                 3400
      WRITE(LOUT,97) I,(ANAME(I,J),J=1,5)                                   3410
   97 FORMAT(26H DESIGNATED ARRANGEMENT NO,2X,I5,19X,5A4//)                 3420
   75 WRITE(LOUT,105) LNUMB,(RLNAME(I),I=1,5)                               3430
      WRITE(LOUT,106) VH1                                                   3440
      WRITE(LOUT,107) PHI                                                   3450
      IF(MODE.EQ.2) WRITE(LOUT,112) AREAL                                   3460
      IF(MODE.EQ.3) WRITE(LOUT,108) FACM                                    3470
      WRITE(LOUT,109) H                                                     3480
      IF(MODE.EQ.2) GO TO 85                                                3490
      WRITE(LOUT,110) DLUM                                                  3500
      GO TO 95                                                              3510
   85 WRITE(LOUT,110) S                                                     3520
  105 FORMAT(6X,22H LIGHT DISTRIBUTION NO,I5,19X,5A4)                       3530
  106 FORMAT(6X,13H UPCAST ANGLE,11X,F6.2,9H  DEGREES)                      3540
  107 FORMAT(6X,22H LUMINOUS FLUX OF LAMP,F9.3,12H KILO-LUMENS)             3550
  108 FORMAT(6X,19H MAINTENANCE FACTOR,6X,F6.3)                             3560
  109 FORMAT(6X,16H MOUNTING HEIGHT,8X,F6.2,3H  M)                          3570
  110 FORMAT(6X,18H LUMINAIRE SPACING,5X,F7.2,3H  M)                        3580
   95 WRITE(LOUT,111)                                                       3590
  111 FORMAT(/,6X,17H POSITION OF ROWS,/,6X,                                3600
     *41H RELATIVE TO LEFTHAND EDGE OF CARRIAGEWAY//6X,                     3610
     *32H ROW NO   POSITION     C-0 PLANE,/,15X,                            3620
     *20H (METERS)     POINTS,/)                                            3630
  112 FORMAT(6X,13H FLASHED AREA,12X,F6.3,5H SQ-M)                          3640
      ISTAG = 0                                                             3650
      DO 30 J = 1,NROW                                                      3660
      W1  = GEO(J,1)                                                        3670
      W2  = GEO(J,2)                                                        3680
      W3  = GEO(J,3)                                                        3690
      IF(W3-60.0) 55,55,50                                                  3700
   50 ISTAG = 1                                                             3710
   55 IF(W2) 60,60,70                                                       3720
   60 WRITE(LOUT,115) J,W1                                                  3730
      GO TO 30                                                              3740
   70 WRITE(LOUT,116) J,W1                                                  3750
  115 FORMAT(10X,I1,6X,F6.2,5X,17H TOWARDS OBSERVER)                        3760
  116 FORMAT(10X,I1,6X,F6.2,5X,19H AWAY FROM OBSERVER)                      3770
   30 CONTINUE                                                              3780
      IF(MODE.EQ.2) GO TO 80                                                3790
      IF(ISTAG) 71,71,90                                                    3800
   90 WRITE(LOUT,117)                                                       3810
  117 FORMAT(1X//6X,29H THE ARRANGEMENT IS STAGGERED)                       3820
   71 IF(IGLAR) 80,80,72                                                    3830
   72 WRITE(LOUT,119)                                                       3840
  119 FORMAT(1X//18H GLARE CALCULATION/18H -----------------/)              3850
      WRITE(LOUT,120) AREAL,COULR                                           3860
  120 FORMAT(6X,25HFLASHED AREA OF LUMINAIRE,F10.3,6H  SQ-M/                3870
     *6X,13HCOLOUR FACTOR,12X,F10.3//)                                      3880
   80 RETURN                                                                3890
      END                                                                   3900
      SUBROUTINE DIST3                                                      3910
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                   3920
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                 3930
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                      3940
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                       3950
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                     3960
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                   3970
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                          3980
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                       3990
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                          4000
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                 4010
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                  4020
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                               4030
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                 4040
      DO 50 J=1,40                                                          4050
      DO 50 I=1,25                                                          4060
   50 ALUM(I,J)=0.0                                                         4070
      YAMIN = 60.0-5.*H                                                     4080
      YAMAX = 60.0+DLUM+12.0*H                                              4090
      FAC=PHI*FACM/10.0**4/(H*H)                                            4100
C***   BEGINNING OF ROW-LOOP                                                4110
      DO 100 JR = 1,NROW                                                    4120
      XA =  GEO(JR,1)                                                       4130
      YA1=  GEO(JR,3)                                                       4140
      W1 = GEO(JR,2)                                                        4150
      AN12 = W1                                                             4160
      AN21 = -W1*SINV                                                       4170
      AN23 = -COSV                                                          4180
      AN31 = -W1*COSV                                                       4190
      AN33 = SINV                                                           4200
      YA   = YA1-DLUM*FLOAT(IFIX((YA1-YAMIN)/DLUM +1.001))                  4210
C***   BEGINNING  OF  LUMINAIRE-LOOP                                        4220
  500 YA   = YA+DLUM                                                        4230
      IF(YA-YAMAX) 130,130,400                                              4240
C***   BEGINNING  OF  CALCULATION POINT LOOP                                4250
  130 XCOBS = ROBS-RCMIN+DRC                                                4260
      XCLUM = RCMIN-DRC-XA                                                  4270
      DO 600 JRC = 1,NRC                                                    4280
      XCOBS = XCOBS-DRC                                                     4290
      XCLUM = XCLUM+DRC                                                     4300
      YCOBS=SOBS-SCMIN+DSC                                                  4310
      YCLUM = SCMIN-DSC-YA                                                  4320
      DO 600 JSC = 1,NSC                                                    4330
      YCOBS = YCOBS-DSC                                                     4340
      YCLUM = YCLUM+DSC                                                     4350
      CALL BETG3(XCLUM,YCLUM,XCOBS,YCOBS,TGAM,BET)                          4360
      IF(TGAM-12.) 140,140,600                                              4370
  140 IF(TGAM-5.) 141,141,142                                               4380
  142 W1=BET-45.*RAPDG                                                      4390
      IF(W1) 141,600,600                                                    4400
  141 CONTINUE                                                              4410
      CALL FIGA3(XCLUM,YCLUM,FI,GAM)                                        4420
      W4    = CAND3(FI,GAM)                                                 4430
      W5    = RVAL3(TGAM,BET)                                               4440
      W1    = FAC*W4*W5                                                     4450
      ALUM(JRC,JSC) = ALUM(JRC,JSC) +W1                                     4460
  600 CONTINUE                                                              4470
      GO TO 500                                                             4480
  400 CONTINUE                                                              4490
  100 CONTINUE                                                              4500
      RETURN                                                                4510
      END                                                                   4520
      SUBROUTINE LINP3                                                      4530
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                   4540
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                 4550
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                      4560
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                       4570
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                     4580
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                   4590
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                          4600
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                       4610
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                          4620
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                 4630
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                  4640
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                               4650
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                 4660
C                                                                           4670
C         INPUT OF LIGHT DISTRIBUTION WITH ID-NUMBER LNUMB                  4680
C                                                                           4690
C              IW : ID-#   (MUST EQUAL LNUMB)                               4700
C              ISYM :  SYMETRI-CODE (MUST EQUAL 1 OR 0)                     4710
C              IGRID: CODE FOR MEASURING GRID (MUST EQUAL 0,CORRESPON-      4720
C                     DING TO THE STANDARD CIE-GRID)                        4730
C                                                                           4740
      READ(LIN,15) IW,ISYM,IGRID,(RLNAME(I),I=1,5)                          4750
   15 FORMAT(3I4,8X,5A4)                                                    4760
   25 FORMAT(17I4)                                                          4770
      IF(IW-LNUMB)10,30,10                                                  4780
   30 IF(IGRID)10,20,10                                                     4790
   20 IW1 = 36                                                              4800
      IF(ISYM)10,40,50                                                      4810
   40 IW2 = 52                                                              4820
      GO TO 60                                                              4830
   50 IF(ISYM-1)10,70,10                                                    4840
   70 IW2 = 27                                                              4850
   60 READ(LIN,25) ((LIGHT(I,J),J=1,IW2),I=1,IW1)                           4860
      IF(ISYM)80,80,90                                                      4870
   90 IW2=53                                                                4880
      DO 100 J = 2,26                                                       4890
      IW2=IW2 - 1                                                           4900
      DO 100  I = 1,IW1                                                     4910
  100 LIGHT(I,IW2) = LIGHT(I,J)                                             4920
   80 CONTINUE                                                              4930
C                                                                           4940
C     CALCULATION OF SLI (SPECIFIC LUMINAIRE INDEX)                         4950
C                                                                           4960
      ELOG=0.434295                                                         4970
      FI=90.*RAPDG                                                          4980
      GAM=80.*RAPDG                                                         4990
      AI80=CAND3(FI,GAM)*PHI                                                5000
      GAM=88.*RAPDG                                                         5010
      AI88=CAND3(FI,GAM)*PHI                                                5020
      IF(AI88.LT.1.) AI88=1.0                                               5030
      AI=AI80/AI88                                                          5040
      IF(AI.GT.50.) AI=50.0                                                 5050
      SLI= -3.31*ALOG(AI80)+1.975*SQRT(ALOG(AI))                            5060
     1 -0.08*ALOG(AI)+1.29*ALOG(AREAL)                                      5070
      SLI= 13.84+ELOG*SLI+COULR                                             5080
      RETURN                                                                5090
   10 CALL ERROR(10,LOUT)                                                   5100
      STOP                                                                  5110
      END                                                                   5120
      SUBROUTINE GEN(ANGLS,NZON,NZ,DZ,FAC)                                  5130
      DIMENSION NZ(5),DZ(5),ANGLS(52)                                       5140
      IZ= 1                                                                 5150
      ANG = 0.0                                                             5160
      ANGLS(1) = ANG                                                        5170
      DO 10 J= 1,NZON                                                       5180
      NZZ = NZ(J)                                                           5190
      DZZ = DZ(J)                                                           5200
      DO 10 I= 1,NZZ                                                        5210
      ANG = ANG + DZZ                                                       5220
      IZ = IZ+1                                                             5230
   10 ANGLS(IZ) = FAC*ANG                                                   5240
      RETURN                                                                5250
      END                                                                   5260
      FUNCTION CAND3(FI,GM)                                                 5270
      DIMENSION W(3)                                                        5280
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                   5290
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                 5300
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                      5310
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                       5320
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                     5330
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                   5340
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                          5350
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                       5360
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                          5370
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                 5380
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                  5390
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                               5400
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                 5410
      IF(FI-PIHAF) 10,20,30                                                 5420
   10 N1 = 2                                                                5430
      GO TO 50                                                              5440
   30 IF(FI-PI)20,40,40                                                     5450
   20 N1 = 14                                                               5460
      GO TO 50                                                              5470
   40 N1 = 27                                                               5480
   50 DO 60 J=N1,53                                                         5490
      D = CPL(J)-FI                                                         5500
      IF(D) 60,60,70                                                        5510
   60 CONTINUE                                                              5520
      J= 53                                                                 5530
   70 IF( D - (CPL(J)-CPL(J-1))/2.) 80,80,90                                5540
   80 I = J-1                                                               5550
      GO TO 100                                                             5560
   90 I = J-2                                                               5570
  100 IF(I) 110,110,120                                                     5580
  110 I1 = 52                                                               5590
      I2 =  1                                                               5600
      GO TO 150                                                             5610
  120 IF(I-51) 130,140,110                                                  5620
  140 I1 = 51                                                               5630
      I2 = 52                                                               5640
      I3 =  1                                                               5650
      GO TO 160                                                             5660
  130 I1 = I                                                                5670
      I2 = I1+1                                                             5680
  150 I3 = I2+1                                                             5690
  160 DX1= CPL(I2)-CPL(I1)                                                  5700
      DX2= CPL(I3)-CPL(I2)                                                  5710
      DX = FI-CPL(I1)                                                       5720
C           GAMMA                                                           5730
      DO 200 J=2,36                                                         5740
      D =  CONE(J)-GM                                                       5750
      IF(D) 200,200,210                                                     5760
  200 CONTINUE                                                              5770
      J = 36                                                                5780
  210 IF(D - (CONE(J)-CONE(J-1))/2.) 220,220,230                            5790
  220 I = J-1                                                               5800
      GO TO 250                                                             5810
  230 I = J-2                                                               5820
      IF(I) 240,240,250                                                     5830
  240 I = 1                                                                 5840
      GO TO 260                                                             5850
  250 IF(I-35)260,270,270                                                   5860
  270 I = 34                                                                5870
  260 DO 280 J = 1,3                                                        5880
      IG = J-1+I                                                            5890
      Y1 = LIGHT(IG,I1)                                                     5900
      Y2 = LIGHT(IG,I2)                                                     5910
      Y3 = LIGHT(IG,I3)                                                     5920
      W(J) = AIPOL(1,DX,DX1,DX2,Y1,Y2,Y3)                                   5930
  280 CONTINUE                                                              5940
      DX1 = CONE(I+1)-CONE(I)                                               5950
      DX2 = CONE(I+2)-CONE(I+1)                                             5960
      DX  = GM-CONE(I)                                                      5970
      CAND3 = AIPOL(1,DX,DX1,DX2,W(1),W(2),W(3))                            5980
      RETURN                                                                5990
      END                                                                   6000
      SUBROUTINE RINP3                                                      6010
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                   6020
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                 6030
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                      6040
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                       6050
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                     6060
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                   6070
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                          6080
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                       6090
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                          6100
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                 6110
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                  6120
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                               6130
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                 6140
C                                                                           6150
C          INPUT OF R-TABLE                                                 6160
C          IF WRONG ID-NUMBER ERROR-MESSAGE 9 AND STOP                      6170
C                                                                           6180
      DO 10 J=1,20                                                          6190
      DO 10 I=1,29                                                          6200
   10 IREF(I,J)=0                                                           6210
      READ(LIN,25) RNOX,(RSNAME(I),I=1,5)                                   6220
   25 FORMAT(F7.3,13X,5A4)                                                  6230
      IF(RNO-RNOX) 15,30,15                                                 6240
   30 DO 35 J=1,20                                                          6250
      READ(LIN,40) NUMB,(IREF(I,J),I=1,NUMB)                                6260
   35 CONTINUE                                                              6270
   40 FORMAT(I2,/,(17I4))                                                   6280
      RETURN                                                                6290
   15 CALL ERROR(9,LOUT)                                                    6300
      STOP                                                                  6310
      END                                                                   6320
      SUBROUTINE ERROR(NUMB,LOUT)                                           6330
      WRITE(LOUT,10) NUMB                                                   6340
   10 FORMAT(10H ERROR NO ,I2)                                              6350
      RETURN                                                                6360
      END                                                                   6370
      FUNCTION AIPOL(ISW,X,DX1,DX2,Y1,Y2,Y3)                                6380
C                                                                           6390
C         IF ISW <= 0: LINEAR INTERPOLATION ELSE QUADRATIC                  6400
C                                                                           6410
C         THE INTERPOLATION CURVE IS DRAWN THROUGH THE FOLLOWING            6420
C         TWO OR THREE POINTS: (0      , Y1)                                6430
C                              (DX1    , Y2)                                6440
C                              (DX1+DX2, Y3)                                6450
C                                                                           6460
      IF(ISW) 10,10,20                                                      6470
   10 AIPOL=Y1+X*(Y2-Y1)/DX1                                                6480
      RETURN                                                                6490
   20 W=(Y3-Y1)*DX1*(X-DX1)+(Y2-Y1)*(DX1+DX2)*(DX1+DX2-X)                   6500
      AIPOL=Y1+X*W/((DX1+DX2)*DX1*DX2)                                      6510
      RETURN                                                                6520
      END                                                                   6530
      FUNCTION RVAL3(TGAM,BT)                                               6540
      DIMENSION W(3)                                                        6550
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                   6560
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                 6570
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                      6580
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                       6590
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                     6600
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                   6610
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                          6620
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                       6630
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                          6640
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                 6650
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                  6660
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                               6670
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                 6680
C                                                                           6690
C  CALCULATION OF R-VALUE FOR TANGENS(GAMMA)=TGAM AND BETA=BT               6700
C  THE INTERPOLATION PROCEDURE USED IS QUADRATIC (TYPE 1) EXCEPT            6710
C  AT THE LOWER EDGE OF THE TABLE WHERE LINEAR INTERPOLATION IS USED.       6720
C  FURTHER, THE INTERPOLATION CURVES FOR CONSTANT TGAM WILL NEVER CROSS     6730
C  THE BETA=180 DEGREE-LIMIT                                                6740
C                                                                           6750
      DO 10 J=1,20                                                          6760
      IF(BETA(J)-BT) 10,10,30                                               6770
   10 CONTINUE                                                              6780
      J=20                                                                  6790
   30 IB=J-1                                                                6800
      DO 50 J=1,29                                                          6810
      D=TANG(J)-TGAM                                                        6820
      IF(D) 50,50,60                                                        6830
   50 CONTINUE                                                              6840
      IF(D) 100,90,90                                                       6850
   90 J=29                                                                  6860
   60 IT=J-1                                                                6870
      IF(IT-14) 110,120,120                                                 6880
  120 IF(IB-7) 130,140,140                                                  6890
  130 IF(IT-18) 110,150,150                                                 6900
  150 IF(IB-4) 160,140,140                                                  6910
  160 IF(IT-28) 110,140,140                                                 6920
  110 ISW=1                                                                 6930
      N=3                                                                   6940
      IF(IB-19) 180,170,170                                                 6950
  170 IB=IB-1                                                               6960
      GO TO 180                                                             6970
  140 ISW=-1                                                                6980
      N=2                                                                   6990
  180 TX=TGAM-TANG(IT)                                                      7000
      TX1=TANG(IT+1)-TANG(IT)                                               7010
      BX=BT-BETA(IB)                                                        7020
      BX1=BETA(IB+1)-BETA(IB)                                               7030
      IF(ISW) 300,300,310                                                   7040
  310 TX2=TANG(IT+2)-TANG(IT+1)                                             7050
      BX2=BETA(IB+2)-BETA(IB+1)                                             7060
  300 DO 200 J=1,N                                                          7070
      I=J-1+IB                                                              7080
      Y1=IREF(IT,I)                                                         7090
      Y2=IREF(IT+1,I)                                                       7100
      IF(ISW) 320,320,330                                                   7110
  330 Y3=IREF(IT+2,I)                                                       7120
  320 W(J)=AIPOL(ISW,TX,TX1,TX2,Y1,Y2,Y3)                                   7130
  200 CONTINUE                                                              7140
      RVAL3=AIPOL(ISW,BX,BX1,BX2,W(1),W(2),W(3))                            7150
      RETURN                                                                7160
  100 RVAL3=0.0                                                             7170
      RETURN                                                                7180
      END                                                                   7190
      SUBROUTINE CONS3                                                      7200
      DIMENSION NZ(5),DZ(5)                                                 7210
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                   7220
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                 7230
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                      7240
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                       7250
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                     7260
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                   7270
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                          7280
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                       7290
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                          7300
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                 7310
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                  7320
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                               7330
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                 7340
C                                                                           7350
C  DEFINITION OF VARIOUS CONSTANTS                                          7360
C                                                                           7370
C  TANG(29) AND BETA(20) CONTAIN THE MEASURING GRID OF THE R-TABLE          7380
C  THE BETA-VALUES IN RADIAN                                                7390
C                                                                           7400
C          CPL(53) AND CONE(36) CONTAIN THE MEASURING GRID OF               7410
C          THE LIGHTDISTRIBUTION                                            7420
C                                                                           7430
C  LIN AND LOUT ARE THE LOGICAL NUMBERS OF THE INPUT- AND OUTPUT-           7440
C  UNITS RESPECTIVELY                                                       7450
C                                                                           7460
      PI=3.141593                                                           7470
      RAPDG=PI/180.                                                         7480
      LIN=5                                                                 7490
      LOUT=6                                                                7500
      PIHAF = PI/2.                                                         7510
C          VALUES OF TANGENS(GAMMA)                                         7520
      NZON = 2                                                              7530
      NZ(1) = 8                                                             7540
      NZ(2) = 20                                                            7550
      DZ(1) = 0.25                                                          7560
      DZ(2) = 0.50                                                          7570
      FAC = 1.                                                              7580
      CALL GEN(TANG,NZON,NZ,DZ,FAC)                                         7590
C          VALUES OF BETA-ANGLE                                             7600
      NZON = 4                                                              7610
      NZ(1) = 1                                                             7620
      NZ(2) = 1                                                             7630
      NZ(3) = 8                                                             7640
      NZ(4) = 9                                                             7650
      DZ(1) = 2.                                                            7660
      DZ(2) = 3.                                                            7670
      DZ(3) = 5.                                                            7680
      DZ(4) = 15.                                                           7690
      CALL GEN(BETA,NZON,NZ,DZ,RAPDG)                                       7700
C          VALUES OF C-ANGLE                                                7710
      NZON = 5                                                              7720
      NZ(1) = 2                                                             7730
      NZ(2) = 1                                                             7740
      NZ(3) = 20                                                            7750
      NZ(4) = 1                                                             7760
      NZ(5) = 2                                                             7770
      DZ(1) = 15.                                                           7780
      DZ(2) = 10.                                                           7790
      DZ(3) = 5.                                                            7800
      DZ(4) = 10.                                                           7810
      DZ(5) = 15.                                                           7820
      CALL GEN(CPL,NZON,NZ,DZ,RAPDG)                                        7830
      I = 54                                                                7840
      DO 50 J= 1,26                                                         7850
      I = I-1                                                               7860
   50 CPL(I) = 2.*PI-CPL(J)                                                 7870
C          VALUES OF GAMMA-ANGLE                                            7880
      NZON = 4                                                              7890
      NZ(1) = 3                                                             7900
      NZ(2) = 3                                                             7910
      NZ(3) = 24                                                            7920
      NZ(4) = 5                                                             7930
      DZ(1) = 10.                                                           7940
      DZ(2) = 5.                                                            7950
      DZ(3) = 2.5                                                           7960
      DZ(4) = 15.                                                           7970
      CALL GEN(CONE,NZON,NZ,DZ,RAPDG)                                       7980
      RETURN                                                                7990
      END                                                                   8000
      SUBROUTINE BETG3(RX,RY,ROBSX,ROBSY,TGAM,BET)                          8010
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                   8020
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                 8030
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                      8040
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                       8050
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                     8060
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                   8070
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                          8080
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                       8090
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                          8100
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                 8110
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                  8120
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                               8130
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                 8140
C                                                                           8150
C  CALCULATION OF DIRECTION IN R-TABLE EXPRESSED BY BET (=BETA) AND         8160
C  TGAM (=TANGENS(GAMMA)).                                                  8170
C  THEY ARE CALCULATED FROM THE VECTOR (RX,RY,-H) WHICH CONNECTS THE        8180
C  LUMINAIRE AND THE CALCULATION POINT AND THE VECTOR (ROBSX,ROBSY,0)       8190
C  WHICH CONNECTS THE CALCULATION POINT AND THE PROJECTION ON               8200
C  THE ROAD-SURFACE OF THE OBSERVER POINT.                                  8210
C                                                                           8220
      TGAM=SQRT(RX*RX+RY*RY)/H                                              8230
      DENOM=ROBSX*RX+ROBSY*RY                                               8240
      IF(DENOM) 10,20,10                                                    8250
   20 BET=PIHAF                                                             8260
      GO TO 100                                                             8270
   10 ANOM=ABS(ROBSX*RY-ROBSY*RX)                                           8280
      BET= ATAN(ANOM/DENOM)                                                 8290
      IF(DENOM) 30,20,100                                                   8300
   30 BET=BET+PI                                                            8310
  100 RETURN                                                                8320
      END                                                                   8330
      SUBROUTINE FIGA3(RX,RY,FI,GAM)                                        8340
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                   8350
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                 8360
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                      8370
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                       8380
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                     8390
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                   8400
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                          8410
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                       8420
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                          8430
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                 8440
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                  8450
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                               8460
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                 8470
C                                                                           8480
C  CALCULATION OF DIRECTION IN LIGHT-DISTRIBUTION EXPRESSED BY              8490
C  THE ANGLES FI (INDICATING C-PLANE) AND GAM (EQUAL TO GAMMA)              8500
C                                                                           8510
      R1=AN12*RY                                                            8520
      R2=AN21*RX-AN23*H                                                     8530
      R3=AN31*RX-AN33*H                                                     8540
      IF(R2) 10,20,10                                                       8550
   20 GAM=PIHAF                                                             8560
      GO TO 40                                                              8570
   10 GAM=ATAN(SQRT(R1*R1+R3*R3)/R2)                                        8580
      IF(R2) 30,20,40                                                       8590
   30 GAM=GAM+PI                                                            8600
   40 IF(R1) 50,60,50                                                       8610
   60 FI=PIHAF                                                              8620
      IF(R3) 70,100,100                                                     8630
   50 FI=ATAN(R3/R1)                                                        8640
      IF(R1) 70,60,80                                                       8650
   70 FI=FI+PI                                                              8660
      GO TO 100                                                             8670
   80 IF(R3) 90,100,100                                                     8680
   90 FI=FI+2.*PI                                                           8690
  100 CONTINUE                                                              8700
      FI = FI+PIHAF                                                         8710
      FI1= FI-2.*PI                                                         8720
      IF(FI1) 120,110,110                                                   8730
  110 FI = FI1                                                              8740
  120 RETURN                                                                8750
      END                                                                   8760
      SUBROUTINE QUAL3                                                      8770
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                   8780
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                 8790
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                      8800
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                       8810
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                     8820
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                   8830
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                          8840
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                       8850
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                          8860
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                 8870
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                  8880
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                               8890
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                 8900
      EPSIL=0.01                                                            8910
      W2  = RCMIN-DRC                                                       8920
      DO 150 J= 1,NRC                                                       8930
      W2 = W2+DRC                                                           8940
      W3 = ABS(ROBS-W2) - EPSIL                                             8950
      IF(W3) 160,160,150                                                    8960
  150 CONTINUE                                                              8970
      JOBS = -1                                                             8980
      GO TO 145                                                             8990
  160 JOBS = J                                                              9000
  145 SUM = 0.0                                                             9010
      ALARG = 10.0**30                                                      9020
      SMALL= -ALARG                                                         9030
      WMIN = ALARG                                                          9040
      DO 10 J=1,NRC                                                         9050
      ISW  = 0                                                              9060
      IF(J-JOBS) 15,20,15                                                   9070
   20 WMIN2  = ALARG                                                        9080
      WMAX  = SMALL                                                         9090
      ISW   = 1                                                             9100
   15 DO 10  I= 1,NSC                                                       9110
      W = ALUM(J,I)                                                         9120
      SUM = SUM+W                                                           9130
      IF(WMIN-W)25,25,30                                                    9140
   30 WMIN = W                                                              9150
   25 IF(ISW) 10,10,35                                                      9160
   35 IF(WMIN2-W) 55,55,60                                                  9170
   60 WMIN2 = W                                                             9180
   55 IF(WMAX-W)70,10,10                                                    9190
   70 WMAX = W                                                              9200
   10 CONTINUE                                                              9210
      IF(NRC-1)202,202,201                                                  9220
  201 IF(MODE.GT.1) WRITE(LOUT,5)                                           9230
    5 FORMAT(1H1)                                                           9240
      IF(MODE.GT.1) WRITE(LOUT,1)                                           9250
    1 FORMAT(27H LIGHT TECHNICAL PARAMETERS/1X,27(1H-))                     9260
  202 IF(MODE.GT.1) WRITE(LOUT,4)                                           9270
    4 FORMAT(1X///)                                                         9280
      IF(MODE.GT.1) WRITE(LOUT,9) ROBS,SOBS                                 9290
    9 FORMAT(23H OBSERVER POSITION: R =,F7.2,5H  S =,F7.2//)                9300
      IF(NRC-1) 200,200,210                                                 9310
  210 AVLUM =  SUM/FLOAT(NRC*NSC)                                           9320
      UO    = WMIN/AVLUM                                                    9330
      IF(MODE.GT.1) WRITE(LOUT,2) AVLUM                                     9340
      IF(MODE.GT.1) WRITE(LOUT,3) UO                                        9350
  200 IF(JOBS)220,220,230                                                   9360
  230 UL = WMIN2/WMAX                                                       9370
      IF(MODE.GT.1) WRITE(LOUT,6) UL                                        9380
    3 FORMAT(5X,18HOVERALL UNIFORMITY,30X,F7.2,/)                           9390
    2 FORMAT(5X,17HAVERAGE LUMINANCE,31X,F7.2,8H CD/SQ-M/)                  9400
    6 FORMAT(5X,23HLONGITUDINAL UNIFORMITY,25X,F7.2/)                       9410
  220 RETURN                                                                9420
      END                                                                   9430
      SUBROUTINE OUT23                                                      9440
      DIMENSION W(10)                                                       9450
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                   9460
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                 9470
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                      9480
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                       9490
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                     9500
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                   9510
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                          9520
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                       9530
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                          9540
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                 9550
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                  9560
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                               9570
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                 9580
      IW=29-NRC                                                             9590
      NBLOC = NSC/10                                                        9600
      NREM  = NSC-10*NBLOC                                                  9610
      IF(NREM) 20,20,30                                                     9620
   30 NBLOC = NBLOC+1                                                       9630
      GO TO 40                                                              9640
   20 NREM  = 10                                                            9650
   40 DO 50 JB = 1,NBLOC                                                    9660
      J1 = 10*(JB-1)+1                                                      9670
      IF(JB-NBLOC)60,70,60                                                  9680
   70 J2=J1-1+NREM                                                          9690
      IW2= NREM                                                             9700
      GO TO 80                                                              9710
   60 IW2= 10                                                               9720
      J2=J1+9                                                               9730
   80 K = 0                                                                 9740
      DO 90 J=J1,J2                                                         9750
      K=K+1                                                                 9760
   90 W(K) = SCMIN+DSC*FLOAT(J-1)                                           9770
      WRITE(LOUT,5)                                                         9780
    5 FORMAT(1H1)                                                           9790
      WRITE(LOUT,1)                                                         9800
    1 FORMAT(23H LUMINANCE DISTRIBUTION,/,1X,22(1H-),///)                   9810
      WRITE(LOUT,6) ROBS,SOBS                                               9820
    6 FORMAT(23H OBSERVER POSITION: R = ,F7.2,5H  S =,F7.2///)              9830
      WRITE(LOUT,2)(W(J),J=1,K)                                             9840
    2 FORMAT(3X,5HR / S,1X,10F6.1)                                          9850
      WRITE(LOUT,3)                                                         9860
    3 FORMAT(/,1X,71(1H*))                                                  9870
      DO 100 J = 1,NRC                                                      9880
      W1 = RCMIN +DRC*FLOAT(J-1)                                            9890
  100 WRITE(LOUT,4) W1,(ALUM(J,I),I=J1,J2)                                  9900
    4 FORMAT(/1X,F6.2,2H *,10F6.2)                                          9910
      WRITE(LOUT,3)                                                         9920
   50 CONTINUE                                                              9930
      RETURN                                                                9940
      END                                                                   9950
      SUBROUTINE GLAR3                                                      9960
      DIMENSION VL(4),XO(3),YO(3),VLNC(2)                                   9970
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                   9980
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                 9990
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                     10000
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                      10010
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                    10020
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                  10030
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                         10040
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                      10050
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                         10060
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                10070
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                 10080
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                              10090
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                10100
C                                                                          10110
C     CALCULATION OF DISABILITY GLARE                                      10120
C                                                                          10130
      G=0.0                                                                10140
      FRCLS=0.02                                                           10150
      ELOG=0.434295                                                        10160
      HO=1.5                                                               10170
      T20=20.*RAPDG                                                        10180
      NCAL=1                                                               10190
      IF(GEO(1,3)-60.) 5,5,6                                               10200
    6 NCAL=2                                                               10210
    5 AFST=(H-HO)*COS(T20)/SIN(T20)                                        10220
      VLNC(1)=0.0                                                          10230
      VLNC(2)=0.0                                                          10240
      YO(1)=0.0                                                            10250
      YO(2)=-90.0                                                          10260
      YO(3)=HO                                                             10270
      FAK=H/(H-HO)                                                         10280
      RY2=YO(1)**2+YO(2)**2+YO(3)**2                                       10290
      DO 2 NC=1,NCAL                                                       10300
      DO 9 I=1,NROW                                                        10310
    9 VL(I)=0                                                              10320
      DO 3 NR=1,NROW                                                       10330
      XA=GEO(NR,1)                                                         10340
      W1=GEO(NR,2)                                                         10350
      YA1=GEO(NR,3)                                                        10360
      AN12=W1                                                              10370
      AN21=-W1*SINV                                                        10380
      AN23=-COSV                                                           10390
      AN31=-W1*COSV                                                        10400
      AN33=SINV                                                            10410
      XO(1)=ROBS-XA                                                        10420
      XO(3)=HO-H                                                           10430
      AFST0=AFST+ABS(YA1-60.-FLOAT(NC-1)*0.5*DLUM)-DLUM                    10440
      NA=0                                                                 10450
    4 CONTINUE                                                             10460
      NA=NA+1                                                              10470
      XO(2)=-AFST0-FLOAT(NA)*DLUM                                          10480
      RX2=0.0                                                              10490
      COS0=0.0                                                             10500
      DO 10 I=1,3                                                          10510
      RX2=RX2+XO(I)**2                                                     10520
   10 COS0=COS0+XO(I)*YO(I)                                                10530
      IF(RX2-250000.)7,7,8                                                 10540
    7 CONTINUE                                                             10550
      COS0=COS0/SQRT(RX2*RY2)                                              10560
      TAETA=ATAN(SQRT((1.-COS0)*(1.+COS0))/COS0)                           10570
      XLUM=XO(1)*FAK                                                       10580
      YLUM=XO(2)*FAK                                                       10590
      CALL FIGA3(XLUM,YLUM,FI,GAM)                                         10600
      EBL=CAND3(FI,GAM)*PHI*COS0/RX2                                       10610
      VLNR=3.E-3*EBL/TAETA**2                                              10620
      VL(NR)=VL(NR)+VLNR                                                   10630
      VLTST=VL(NR)*FRCLS-VLNR                                              10640
      IF(VLTST)4,4,8                                                       10650
    8 VLNC(NC)=VLNC(NC)+VL(NR)                                             10660
    3 CONTINUE                                                             10670
    2 CONTINUE                                                             10680
      VLMAX=VLNC(1)                                                        10690
      IF(VLNC(1)-VLNC(2))13,14,14                                          10700
   13 VLMAX=VLNC(2)                                                        10710
   14 CONTINUE                                                             10720
      TI=65.*VLMAX/(AVLUM/FACM)**0.8                                       10730
      IF(MODE.EQ.1) GO TO 19                                               10740
      WRITE(LOUT,103) VLMAX                                                10750
  103 FORMAT(5X,17HVEILING LUMINANCE,28X,F10.2,8H CD/SQ-M/)                10760
      WRITE(LOUT,104) TI                                                   10770
  104 FORMAT(5X,19HTHRESHOLD INCREMENT,26X,F10.2,8H PERCENT/)              10780
C                                                                          10790
C     SLI, THE SPECIFIC LUMINAIRE INDEX, IS CALCULATED IN SUBROUTINE LIN   10800
C                                                                          10810
      WRITE(LOUT,105) SLI                                                  10820
  105 FORMAT(5X,24HSPECIFIC LUMINAIRE INDEX,21X,F10.2/)                    10830
   19 CONTINUE                                                             10840
C                                                                          10850
C     CALCULATION OF DISCOMFORT GLARE                                      10860
C                                                                          10870
      IF(NROW-1)17,16,18                                                   10880
   18 IF(ISYM)17,17,16                                                     10890
   17 IF(MODE.GT.1) WRITE(LOUT,102)                                        10900
  102 FORMAT(38H DISCOMFORT GLARE CANNOT BE CALCULATED/)                   10910
      RETURN                                                               10920
   16 G= 0.97*ALOG(AVLUM/FACM)+4.41*ALOG(H-HO)                             10930
     1 -1.46*ALOG(FLOAT(NROW)*1000.0/DLUM)                                 10940
      G= SLI+ELOG*G                                                        10950
      IF(MODE.GT.1) WRITE(LOUT,101) G                                      10960
  101 FORMAT(5X,29HDISCOMFORT GLARE CONTROL MARK,16X,F10.2/)               10970
   20 RETURN                                                               10980
      END                                                                  10990
      SUBROUTINE SAAINP(I,JMAX,KMAX,LMAX,IFLAG)                            11000
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                  11010
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                11020
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                     11030
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                      11040
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                    11050
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                  11060
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                         11070
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                      11080
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                         11090
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                11100
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                 11110
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                              11120
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                11130
      EQUIVALENCE (F,AREAL),(UPCAST,VH1),(WM,WC)                           11140
C                                                                          11150
C     THIS ROUTINE READS INPUT DATA DEFINING THE JOB, VIZ., THE MODE       11160
C     AND NAME OF THE JOB, AND THE VALUES OF RELEVANT LUMINAIRE AND        11170
C     INSTALLATION PARAMETERS                                              11180
C                                                                          11190
      I=1                                                                  11200
      JMAX=1                                                               11210
      KMAX=1                                                               11220
      LMAX=1                                                               11230
      IFLAG=0                                                              11240
      WM=0.0                                                               11250
      READ(LIN,100) MODE,JOB                                               11260
  100 FORMAT(I2,18A4)                                                      11270
      IF(MODE.EQ.3) RETURN                                                 11280
      READ(LIN,90) LNUMB,PHI,F,UPCAST                                      11290
   90 FORMAT(I4,3F7.2)                                                     11300
      IF(MODE.EQ.1) GO TO 10                                               11310
      READ(LIN,80) ARR,H,S,WK,WM,OH1,OH2                                   11320
   80 FORMAT(F4.0,6F7.2)                                                   11330
      RETURN                                                               11340
   10 READ(LIN,80) ARR,H                                                   11350
      I=(H-7.5)/1.5 + 0.001                                                11360
      KMAX=6                                                               11370
      LMAX=NS(I)                                                           11380
      IF(ARR.NE.0.) RETURN                                                 11390
      JMAX=5                                                               11400
      IFLAG=1                                                              11410
      RETURN                                                               11420
      END                                                                  11430
      SUBROUTINE CREATE(I,J,K,L,IFLAG)                                     11440
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                  11450
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                11460
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                     11470
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                      11480
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                    11490
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                  11500
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                         11510
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                      11520
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                         11530
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                11540
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                 11550
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                              11560
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                11570
C                                                                          11580
C     THIS ROUTINE CREATES SAA TYPE INSTALLATION PARAMETER DATA FOR        11590
C     THE SAA DESIGNATED INSTALLATION DEFINED BY THE ARGUMENTS I,J,K & L   11600
C     VALUES OF THE PARAMETERS ARE GIVEN IN SUBROUTINE SAADAT -            11610
C     THIS ROUTINE ONLY CALLED FOR MODE 1                                  11620
C                                                                          11630
C                                                                          11640
C     ARRANGEMENT/MEDIAN                                                   11650
C                                                                          11660
      IF(IFLAG.GT.0) ARR=DARR(J)                                           11670
      WC=0.0                                                               11680
      JP=ARR                                                               11690
      IF(ARR.LE.4.) GO TO 1                                                11700
      JP=JP-4                                                              11710
      WC=3.0                                                               11720
    1 CONTINUE                                                             11730
C                                                                          11740
C     CARRIAGEWAY WIDTH/OVERHANG                                           11750
C                                                                          11760
      FK=K-1                                                               11770
      OH1=0.0                                                              11780
      IF(K.LT.4) GO TO 2                                                   11790
      FK=K-3                                                               11800
      OH1=H/4.0                                                            11810
    2 OH2=OH1                                                              11820
      WK=WKMIN(I,JP)+2.0*FK                                                11830
C                                                                          11840
C     SPACING                                                              11850
C                                                                          11860
      FL=L-1                                                               11870
      S=SMIN(I)+5.0*FL                                                     11880
      RETURN                                                               11890
      END                                                                  11900
      SUBROUTINE TFORM                                                     11910
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                  11920
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                11930
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                     11940
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                      11950
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                    11960
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                  11970
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                         11980
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                      11990
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                         12000
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                12010
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                 12020
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                              12030
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                12040
C                                                                          12050
C     THIS ROUTINE TRANSFORMS THE SAA TYPE INSTALLATION PARAMETER DATA     12060
C     INTO THE CIE TYPE DATA REQUIRED BY SUBROUTINE INPU3  AND WRITES      12070
C     THEM TO LOGICAL UNIT 4                                               12080
C                                                                          12090
      REWIND 4                                                             12100
      ZERO=0.0                                                             12110
      ONE=1.0                                                              12120
      ITYP=1                                                               12130
      RNO=3.                                                               12140
      WRITE(LIN,100) ITYP,RNO,ONE,WK,WC,ZERO                               12150
  100 FORMAT(I2,5F7.2)                                                     12160
      ITYP=2                                                               12170
      ART=ARR                                                              12180
      IF(ARR.GT.4.) ART=ARR-4.                                             12190
      DLUM=S                                                               12200
      STAG=0.                                                              12210
      IF(ART.NE.3.) GO TO 1                                                12220
      DLUM=2.0*S                                                           12230
      STAG=1.                                                              12240
    1 RA1=OH1                                                              12250
      IF(ART.EQ.2.) RA1=WK-OH1                                             12260
      RA2=0.0                                                              12270
      IF(ART.GT.2.) RA2=WK-OH2                                             12280
      WRITE(LIN,100) ITYP,H,DLUM,RA1,RA2,STAG                              12290
      ITYP=3                                                               12300
      RLNO=LNUMB                                                           12310
      WRITE(LIN,100) ITYP,RLNO,VH1,PHI,ONE                                 12320
      ITYP=4                                                               12330
      ROBS=WK/2.0                                                          12340
      NRC=IFIX(WK+0.5)                                                     12350
      RCN=NRC-1                                                            12360
      RMIN=(WK-RCN)/2.0                                                    12370
      RMAX=RMIN+RCN                                                        12380
      WRITE(LIN,100) ITYP,ROBS,RMIN,ONE,RMAX                               12390
      DO 2 I=1,NRC                                                         12400
      RCN=I-1                                                              12410
      RC=RMIN+RCN                                                          12420
      IF(RC.EQ.ROBS) GO TO 3                                               12430
    2 CONTINUE                                                             12440
      WRITE(LIN,100) ITYP,ROBS,ROBS,ZERO,ROBS                              12450
    3 CONTINUE                                                             12460
      ITYP=5                                                               12470
      WRITE(LIN,100) ITYP,ZERO,AREAL                                       12480
      ITYP=0                                                               12490
      WRITE(LIN,100) ITYP                                                  12500
      REWIND 4                                                             12510
      RETURN                                                               12520
      END                                                                  12530
      SUBROUTINE OUT14                                                     12540
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                  12550
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                12560
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                     12570
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                      12580
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                    12590
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                  12600
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                         12610
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                      12620
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                         12630
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                12640
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                 12650
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                              12660
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                12670
C                                                                          12680
C     THIS ROUTINE OUTPUTS AN APPROPRIATE HEADING FOR AS 1158.2 (1986)     12690
C     DESIGNATED INSTALLATIONS INVOKED BY MODE=1                           12700
C                                                                          12710
      WRITE(LOUT,10) JOB                                                   12720
   10 FORMAT(1H1,21X,29HSAA STAN  -  AS 1158.2 (1986)/                     12730
     122X,29(1H*)//,1X,18A4/1X,72(1H-)//)                                  12740
      WRITE(LOUT,20) LNUMB,RLNAME,PHI,AREAL,VH1,SLI                        12750
   20 FORMAT(1H ,9HLUMINAIRE,4X,3HNO ,I4,3X,5A4,4X,12HLAMP FLUX   ,F6.2,   12760
     15H K-LM/1H ,9(1H-),38X,12HFLASHED AREA,F6.2,5H SQ-M/                 12770
     21H ,47X,6HUPCAST,6X,F6.2,4H DEG/1H ,47X,3HSLI,9X,F6.2/)              12780
      I=ARR                                                                12790
      WRITE(LOUT,30) I,(ANAME(I,J),J=1,5),WC                               12800
   30 FORMAT(1H ,11HARRANGEMENT,2X,3HNO ,I4,3X,5A4,4X,12HMEDIAN WIDTH,     12810
     1F6.2,2H M/1H ,11(1H-)/)                                              12820
      WRITE(LOUT,40)                                                       12830
   40 FORMAT(1H ,6X,27H  I  ARR  H   WK   O/H   S ,                        12840
     132H AV-L   UO   UL ESL ESR   G   TI,                                 12850
     2/1H ,6X,60(1H*)/)                                                    12860
      RETURN                                                               12870
      END                                                                  12880
      SUBROUTINE ILLUM                                                     12890
      DIMENSION EV(2),ER(2)                                                12900
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                  12910
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                12920
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                     12930
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                      12940
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                    12950
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                  12960
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                         12970
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                      12980
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                         12990
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                13000
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                 13010
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                              13020
      COMMON /LTPS/  ELUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                13030
C                                                                          13040
C     THIS ROUTINE CALCULATES THE REQUIRED ILLUMINANCE DISTRIBUTION FOR    13050
C     THE DETERMINATION AND OUTPUT OF THE SURROUND ILLUMINANCE VALUES      13060
C     - NOTE ELUM INSTEAD OF ALUM IN LTPS COMMON ABOVE                     13070
C                                                                          13080
      IF(MODE.EQ.3) WK=WLANE*FLOAT(LANE)                                   13090
      RCMIN= -2.5                                                          13100
      DRC= 1.0                                                             13110
      NRC= 12                                                              13120
      NRCQ= 3                                                              13130
      DO 10 I=1,2                                                          13140
      EV(I)= 0.0                                                           13150
   10 ER(I)= 0.0                                                           13160
      DO 50 J=1,40                                                         13170
      DO 50 I=1,25                                                         13180
   50 ELUM(I,J)=0.0                                                        13190
      YAMIN = 60.0-12.0*H                                                  13200
      YAMAX = 60.0+DLUM+12.0*H                                             13210
      FAC=PHI*FACM/(H*H)                                                   13220
C***   BEGINNING OF ROW-LOOP                                               13230
      DO 100 JR = 1,NROW                                                   13240
      XA =  GEO(JR,1)                                                      13250
      YA1=  GEO(JR,3)                                                      13260
      W1 = GEO(JR,2)                                                       13270
      AN12 = W1                                                            13280
      AN21 = -W1*SINV                                                      13290
      AN23 = -COSV                                                         13300
      AN31 = -W1*COSV                                                      13310
      AN33 = SINV                                                          13320
      YA   = YA1-DLUM*FLOAT(IFIX((YA1-YAMIN)/DLUM +1.001))                 13330
C***   BEGINNING  OF  LUMINAIRE-LOOP                                       13340
  500 YA   = YA+DLUM                                                       13350
      IF(YA-YAMAX) 130,130,400                                             13360
C***   BEGINNING  OF  CALCULATION POINT LOOP                               13370
  130 XCLUM = RCMIN-DRC-XA                                                 13380
      DO 600 JRC = 1,NRC                                                   13390
      IF(JRC.EQ.7) XCLUM=WK+RCMIN-DRC-XA                                   13400
      XCLUM = XCLUM+DRC                                                    13410
      YCLUM = SCMIN-DSC-YA                                                 13420
      DO 600 JSC = 1,NSC                                                   13430
      YCLUM = YCLUM+DSC                                                    13440
      CALL FIGA3(XCLUM,YCLUM,FI,GAM)                                       13450
      W4    = CAND3(FI,GAM)                                                13460
      GAM   =ATAN(SQRT(XCLUM*XCLUM+YCLUM*YCLUM)/H)                         13470
      W5    =COS(GAM)**3                                                   13480
      W1    = FAC*W4*W5                                                    13490
      ELUM(JRC,JSC) = ELUM(JRC,JSC) +W1                                    13500
  600 CONTINUE                                                             13510
      GO TO 500                                                            13520
  400 CONTINUE                                                             13530
  100 CONTINUE                                                             13540
      DO 200 ISIDE=1,2                                                     13550
      DO 300 I=1,NRCQ                                                      13560
      DO 300 J=1,NSC                                                       13570
      IP=I+(ISIDE-1)*9                                                     13580
      EV(ISIDE)=EV(ISIDE)+ELUM(IP,J)                                       13590
      IP=I+ISIDE*3                                                         13600
  300 ER(ISIDE)=ER(ISIDE)+ELUM(IP,J)                                       13610
  200 ES(ISIDE)=EV(ISIDE)*100.0/ER(ISIDE)                                  13620
      IF(MODE.GT.1) WRITE(LOUT,700) ES(1),ES(2)                            13630
  700 FORMAT(5X,20HSURROUND ILLUMINANCE,19X,4HLEFT,5X,F7.2,8H PERCENT//    13640
     144X,5HRIGHT,4X,F7.2,8H PERCENT/)                                     13650
      RETURN                                                               13660
      END                                                                  13670
      SUBROUTINE OUT24(INST)                                               13680
      DIMENSION IES(2)                                                     13690
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                  13700
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                13710
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                     13720
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                      13730
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                    13740
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                  13750
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                         13760
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                      13770
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                         13780
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                13790
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                 13800
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                              13810
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                13820
C                                                                          13830
C     THIS ROUTINE OUTPUTS AN ABBREVIATED SINGLE LINE LIST                 13840
C     OF INPUT AND OUTPUT DATA FOR AS 1158.2 (1986) DESIGNATED             13850
C     SPECIFICATIONS FOR COMPUTATION                                       13860
C                                                                          13870
      IARR=ARR                                                             13880
      IWK=WK                                                               13890
      IS=S                                                                 13900
      ITI=TI+0.5                                                           13910
      DO 1 I=1,2                                                           13920
    1 IES(I)=ES(I)+0.5                                                     13930
      WRITE(LOUT,10) INST,IARR,H,IWK,OH1,IS,AVLUM,UO,UL,IES(1),            13940
     1IES(2),G,ITI                                                         13950
   10 FORMAT(1H ,6X,I3,I4,F5.1,I4,F6.2,I4,F6.2,2F5.2,2I4,F5.1,I4)          13960
      RETURN                                                               13970
      END                                                                  13980
      SUBROUTINE SAADAT                                                    13990
      DIMENSION D(5),N(5),SM(5),W(20),A(40),  RS(5),IR01(29),IR02(29)      14000
      DIMENSION IR03(29),IR04(29),IR05(29),IR06(29),IR07(29),IR08(29)      14010
      DIMENSION IR09(29),IR10(29),IR11(29),IR12(29),IR13(29),IR14(29)      14020
      DIMENSION IR15(29),IR16(29),IR17(29),IR18(29),IR19(29),IR20(29)      14030
      CHARACTER*4 A,RS                                                     14040
      CHARACTER*4 JOB,RSNAME,RLNAME,ANAME                                  14050
      COMMON /NAMES/ JOB(18),RSNAME(5),RLNAME(5),ANAME(8,5)                14060
      COMMON /CONS1/ MODE,LIN,LOUT,PI,PIHAF,RAPDG,FACM                     14070
      COMMON /CONS2/ DARR(5),NS(5),SMIN(5),WKMIN(5,4)                      14080
      COMMON /INSTL/ ARR,H,S,OH1,OH2,NROW,DLUM,GEO(4,3)                    14090
      COMMON /ROAD/  RNO,WK,LANE,WLANE,WC                                  14100
      COMMON /RDATA/ IREF(29,20),TANG(29),BETA(20)                         14110
      COMMON /LUMIN/ LNUMB,PHI,ISYM,IGLAR,COULR,AREAL                      14120
      COMMON /IDATA/ LIGHT(36,52),CPL(53),CONE(36)                         14130
      COMMON /TILT/  VH1,SINV,COSV,AN12,AN21,AN23,AN31,AN33                14140
      COMMON /OBS1/  ROBS,SOBS,RCMIN,DRC,NRC,SCMIN,DSC,NSC                 14150
      COMMON /OBS2/  NOBS,OBCAL(5,3),NRCAL(5)                              14160
      COMMON /LTPS/  ALUM(25,40),AVLUM,UO,UL,ES(2),SLI,G,TI                14170
C                                                                          14180
C     THIS ROUTINE ASSIGNS VALUES TO VARIABLES IN COMMON WHICH DEFINE TH   14190
C     RANGE OF SAA DESIGNATED INSTALLATION PARAMETERS AND THE NAME AND     14200
C     REFLECTION CHARACTERISTICS OF CIE STANDARD ROAD SURFACE R3           14210
C                                                                          14220
C                                                                          14230
C     DESIGNATED INSTALLATIONS DATA                                        14240
C                                                                          14250
      DATA D      / 1., 3., 4., 5., 6./                                    14260
      DATA N      / 5, 5, 6, 6, 7/                                         14270
      DATA SM     / 35., 45., 50., 55., 60./                               14280
      DATA W      /  6.,  6.,  8., 10.,                                    14290
     1               8.,  8., 10., 12.,                                    14300
     2              10., 10., 12., 14.,                                    14310
     3              12., 12., 14., 16.,                                    14320
     4              14., 14., 16., 18./                                    14330
      DATA A      / 'S/S ', '(L);', 'SING', ' C/W', 'AY  ',                14340
     1              'S/S ', '(R);', 'SING', ' C/W', 'AY  ',                14350
     2              'STAG', 'GERE', 'D;SI', 'NG C', '/WAY',                14360
     3              'OPPO', 'SITE', ';SIN', 'G C/', 'WAY ',                14370
     4              'OPPO', 'SITE', ';DUA', 'L C/', 'WAY ',                14380
     5              'CENT', 'RAL;', 'DUAL', ' C/W', 'AY  ',                14390
     6              'TWIN', ' STA', 'G;DU', 'AL C', '/WAY',                14400
     7              'TWIN', ' OPP', ';DUA', 'L C/', 'WAY '/                14410
C                                                                          14420
C     CIE R3 ROAD SURFACE DATA                                             14430
C                                                                          14440
      DATA RS     / 'CIE ', 'STAN', 'DARD', ' R3 ', '    '/                14450
      DATA IR01   / 294,326,344,357,362,357,353,339,326,289,253,217,190,   14460
     1  163,145,127,113,104, 95, 87, 83, 78, 73, 69, 65, 62, 59, 56, 53/   14470
      DATA IR02   / 294,326,344,353,362,357,348,335,321,280,235,194,163,   14480
     1  136,109, 94, 77, 68, 60, 53, 47, 42, 38, 34, 32, 29, 26, 24, 22/   14490
      DATA IR03   / 294,321,339,353,352,348,326,303,280,222,163,122, 90,   14500
     1   73, 60, 47, 36, 30, 24, 21, 17, 15, 12, 10,  9,  8,  7,  6,  6/   14510
      DATA IR04   / 294,321,339,339,326,298,267,231,190,127, 85, 60, 43,   14520
     1   31, 24, 18, 15, 11,  9,  7,  6,  5,  4,  4,  3,  3,  3,  2,  2/   14530
      DATA IR05   / 294,317,326,321,276,244,217,172,136, 86, 53, 35, 26,   14540
     1   20, 16, 14, 11,  8,  7,  5,  4,  4,  3,  4,  2,  2,  2,  2,  2/   14550
      DATA IR06   / 294,312,317,303,249,208,176,127,100, 65, 38, 25, 20,   14560
     1   15, 12, 10,  9,  6,  5,  4,  4,  3,  2,  2,  2,  2,  2,  0,  0/   14570
      DATA IR07   / 294,308,308,285,226,176,145,104, 82, 54, 31, 22, 16,   14580
     1   12,  9,  8,  8,  5,  4,  4,  3,  3,  0,  0,  0,  0,  0,  0,  0/   14590
      DATA IR08   / 294,308,298,267,204,154,117, 89, 71, 44, 25, 19, 14,   14600
     1   10,  8,  7,  7,  4,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/   14610
      DATA IR09   / 294,303,289,244,181,136,100, 79, 62, 38, 23, 16, 12,   14620
     1    9,  8,  6,  5,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/   14630
      DATA IR10   / 294,298,276,222,158,118, 86, 70, 54, 34, 20, 15, 10,   14640
     1    8,  7,  6,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/   14650
      DATA IR11   / 294,294,262,204,140,104, 78, 62, 48, 25, 18, 13,  9,   14660
     1    8,  6,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/   14670
      DATA IR12   / 294,280,235,176,118, 83, 72, 51, 39, 23, 15, 10,  7,   14680
     1    5,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/   14690
      DATA IR13   / 294,271,217,158,104, 73, 60, 45, 34, 22, 15,  9,  7,   14700
     1    5,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/   14710
      DATA IR14   / 294,262,204,149,100, 70, 57, 44, 34, 23, 14,  9,  7,   14720
     1    5,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/   14730
      DATA IR15   / 294,258,199,149,100, 71, 58, 45, 34, 24, 15, 10,  8,   14740
     1    5,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/   14750
      DATA IR16   / 294,253,199,149,100, 74, 60, 46, 35, 24, 15, 11,  8,   14760
     1    6,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/   14770
      DATA IR17   / 294,249,199,145,100, 77, 60, 45, 36, 24, 16, 11,  9,   14780
     1    7,  5,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/   14790
      DATA IR18   / 294,244,199,136,100, 77, 60, 45, 36, 24, 16, 12,  9,   14800
     1    8,  7,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/   14810
      DATA IR19   / 294,240,194,136,100, 77, 61, 46, 37, 24, 17, 12,  9,   14820
     1    8,  7,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/   14830
      DATA IR20   / 294,240,194,140,100, 78, 62, 47, 38, 25, 17, 13, 10,   14840
     1    9,  7,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/   14850
C                                                                          14860
C     ASSIGNING VALUES TO VARIABLES IN COMMON                              14870
C                                                                          14880
      DO 1 I=1,5                                                           14890
      DARR(I)=D(I)                                                         14900
      NS(I)=N(I)                                                           14910
      SMIN(I)=SM(I)                                                        14920
      DO 2 J=1,4                                                           14930
      IJ=4*I-4+J                                                           14940
    2 WKMIN(I,J)=W(IJ)                                                     14950
      DO 3 K=1,8                                                           14960
      KI=5*K-5+I                                                           14970
    3 ANAME(K,I)=A(KI)                                                     14980
      RSNAME(I)=RS(I)                                                      14990
    1 CONTINUE                                                             15000
      DO 4 I=1,29                                                          15010
      IREF(I, 1)=IR01(I)                                                   15020
      IREF(I, 2)=IR02(I)                                                   15030
      IREF(I, 3)=IR03(I)                                                   15040
      IREF(I, 4)=IR04(I)                                                   15050
      IREF(I, 5)=IR05(I)                                                   15060
      IREF(I, 6)=IR06(I)                                                   15070
      IREF(I, 7)=IR07(I)                                                   15080
      IREF(I, 8)=IR08(I)                                                   15090
      IREF(I, 9)=IR09(I)                                                   15100
      IREF(I,10)=IR10(I)                                                   15110
      IREF(I,11)=IR11(I)                                                   15120
      IREF(I,12)=IR12(I)                                                   15130
      IREF(I,13)=IR13(I)                                                   15140
      IREF(I,14)=IR14(I)                                                   15150
      IREF(I,15)=IR15(I)                                                   15160
      IREF(I,16)=IR16(I)                                                   15170
      IREF(I,17)=IR17(I)                                                   15180
      IREF(I,18)=IR18(I)                                                   15190
      IREF(I,19)=IR19(I)                                                   15200
      IREF(I,20)=IR20(I)                                                   15210
    4 CONTINUE                                                             15220
      RETURN                                                               15230
      END                                                                  15240
