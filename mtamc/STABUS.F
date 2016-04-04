C-------------------------------------------------------------------
C       WALK, BICYCLE, AND BUS ACCESS TO STATION SELECTION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE STABUS(IZ,BSTA,BUTL,TWALK,BTXFER,imode,SKIM,BUSMODE,
     *                   WDIST,WSTA,LUNRELM,NUNRELM,
     *                   LCROWDM,NCAPACM,WLKLSM,BYCLSM,
     *                   BIKDIST,BIKSTA)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'tpcom.inc'
       include 'control.inc'
       include 'mtamcpar.inc'
C
C DATA DECLARATIONS
C
      INTEGER*2    IMODE,IZ,IC,SC,SC2,IC1,IC2
      INTEGER*2    BSTA(2,2),BTXFER(2,2),BUSMODE(2,6,2)
      INTEGER*2    WSTA(2,5),SC3,SC4,SC5,WINDEX,BIKSTA(2,5)
      INTEGER*2    T,T1(2),T2(2)
      REAL*4       BUSUTL
      REAL*4       BDIST(2,2),BUTL(2,2),WALKACC(MAX_ZONES),TWALK(2,2)
      REAL*4       INVEH(MAX_ZONES),LHIVT(MAX_ZONES),
     *             FARE(MAX_ZONES),TRANSF(MAX_ZONES),
     *             WAIT1(MAX_ZONES),WAIT2(MAX_ZONES),
     *             WALKEGR(MAX_ZONES)
      REAL*4       INVEHL(MAX_ZONES),INVEHR(MAX_ZONES),
     *             INVEHE(MAX_ZONES),INVEHT(MAX_ZONES),
     *             INVEHB(MAX_ZONES),DIRWALK(MAX_ZONES),
     *             INVEHU(MAX_ZONES)
      REAL*4       WALKTFR(MAX_ZONES),SKIM(2,16)
      REAL*4       WDIST(2,5),BIKDIST(2,5)
      REAL*4       LUNRELM(MAX_ZONES,28),LUNRVAL(MAX_ZONES),LUNRDEM
      REAL*4       NUNRELM(MAX_ZONES,28),NUNRVAL(MAX_ZONES),TWAIT
      REAL*4       LCROWDM(MAX_ZONES,28),LCRDVAL(MAX_ZONES)
      REAL*4       NCAPACM(MAX_ZONES,28),NCAPVAL(MAX_ZONES)
      REAL*8       WLKLSM(MAX_ZONES),BYCLSM(MAX_ZONES)
      CHARACTER*13 NAME(2)
      DATA         T1/22,16/,T2/27,20/
      DATA         NAME/'Commuter Rail',
     *                  'Urban Rail   '/
C
C INITIALIZE CHOICE VARIABLES
C
      BDIST(imode,1)=-99999.9
      BDIST(imode,2)=-99999.9
      BUTL(imode,1)=0.0
      BUTL(imode,2)=0.0
      BSTA(imode,1)=0
      BSTA(imode,2)=0
      BTXFER(IMODE,1)=0
      BTXFER(IMODE,2)=0
      WDIST(imode,1)=-99.9
      WSTA(imode,1)=0
      WDIST(imode,2)=-99.9
      WSTA(imode,2)=0
      WDIST(imode,3)=-99.9
      WSTA(imode,3)=0
      WDIST(imode,4)=-99.9
      WSTA(imode,4)=0
      WDIST(imode,5)=-99.9
      WSTA(imode,5)=0
      WINDEX=0
      BIKDIST(imode,1)=-99.9
      BIKSTA(imode,1)=0
      BIKDIST(imode,2)=-99.9
      BIKSTA(imode,2)=0
      BIKDIST(imode,3)=-99.9
      BIKSTA(imode,3)=0
      BIKDIST(imode,4)=-99.9
      BIKSTA(imode,4)=0
      BIKDIST(imode,5)=-99.9
      BIKSTA(imode,5)=0
      IF(IMODE.EQ.1) FILENO=10
      IF(IMODE.EQ.2) FILENO=12
      IF(IMODE.EQ.1) LUNRDEM=LSUM2CR*LSUM1TRN*LSUMT*LSUM3CW
      IF(IMODE.EQ.2) LUNRDEM=LSUM2UR*LSUM1TRN*LSUMT*LSUM3UW
      BUSMODE=0
C
C OBTAIN ACCESS PORTION OF PATH DATA
C
C...1ST WAIT TIME
C      WAIT1
      PURP=1
      call intab(fileno,VAR,iz,PURP,dummy,io)
      DO 105,II=1,MAX_ZONES
 105  WAIT1(II)=FLOAT(VAR(II))/100.0
C...TRANSFER WAIT TIME
C      WAIT2
      PURP=2
      call intab(fileno,VAR,iz,PURP,dummy,io)
      DO 106,II=1,MAX_ZONES
 106  WAIT2(II)=FLOAT(VAR(II))/100.0
C...WALK TIME ACCESS
      PURP=3
      call intab(fileno,VAR,iz,PURP,dummy,io)
      DO 107,II=1,MAX_ZONES
  107 WALKACC(II)=FLOAT(VAR(II))/100.0
C...NUMBER OF TRANSFERS
C      TRANSF
      PURP=4
      call intab(fileno,VAR,iz,PURP,dummy,io)
      DO 104,II=1,MAX_ZONES
 104  TRANSF(II)=FLOAT(VAR(II))
C...LOCAL BUS IN-VEHICLE TIME
      PURP=5
      CALL INTAB(FILENO,VAR,IZ,PURP,DUMMY,IO)
      DO 116 II=1,MAX_ZONES
  116 INVEHL(II)=FLOAT(VAR(II))/100.0
C...ACCESS FARE
C      FARE
      PURP=6
      call intab(fileno,VAR,iz,PURP,dummy,io)
      DO 103,II=1,MAX_ZONES
  103 FARE(II)=FLOAT(VAR(II))
C...RAPID BUS IN-VEHICLE TIME
      PURP=7
      CALL INTAB(FILENO,VAR,IZ,PURP,DUMMY,IO)
      DO 111 II=1,MAX_ZONES
  111 INVEHR(II)=FLOAT(VAR(II))/100.0
C...EXPRESS BUS IN-VEHICLE TIME
      PURP=8
      CALL INTAB(FILENO,VAR,IZ,PURP,DUMMY,IO)
      DO 112 II=1,MAX_ZONES
  112 INVEHE(II)=FLOAT(VAR(II))/100.0
C...TRANSITWAY IN-VEHICLE TIME
      PURP=9
      CALL INTAB(FILENO,VAR,IZ,PURP,DUMMY,IO)
      DO 113 II=1,MAX_ZONES
  113 INVEHT(II)=FLOAT(VAR(II))/100.0
C...URBAN RAIL IN-VEHICLE TIME (CR ONLY)
      IF(IMODE.EQ.1) THEN
      PURP=10
      CALL INTAB(FILENO,VAR,IZ,PURP,DUMMY,IO)
      DO 118 II=1,MAX_ZONES
  118 INVEHU(II)=FLOAT(VAR(II))/100.0
      ELSE
      DO 119 II=1,MAX_ZONES
  119 INVEHU(II)=0.0     
      END IF
C...LINE HAUL IN-VEHICLE TIME
      IF(IMODE.EQ.1) PURP=11
      IF(IMODE.EQ.2) PURP=10
      call intab(fileno,VAR,iz,PURP,dummy,io)
      DO 102,II=1,MAX_ZONES
 102  LHIVT(II)=FLOAT(VAR(II))/100.0
C
C...TOTAL IN-VEHICLE TIME
C
      IF(IMODE.EQ.1) PURP=12
      IF(IMODE.EQ.2) PURP=11
      call intab(fileno,VAR,iz,PURP,dummy,io)
      DO 101,II=1,MAX_ZONES
 101  INVEH(II)=FLOAT(VAR(II))/100.0
C...WALK TIME EGRESS
      IF(IMODE.EQ.1) PURP=13
      IF(IMODE.EQ.2) PURP=12
      call intab(fileno,VAR,iz,PURP,dummy,io)
      DO 108,II=1,MAX_ZONES
  108 WALKEGR(II)=FLOAT(VAR(II))/100.0
C
C...WALK TIME TRANSFER
      IF(IMODE.EQ.1) PURP=14
      IF(IMODE.EQ.2) PURP=13
      call intab(fileno,VAR,iz,PURP,dummy,io)
      DO 109,II=1,MAX_ZONES
  109 WALKTFR(II)=FLOAT(VAR(II))/100.0
C...BRT IN-VEHICLE TIME
      IF(IMODE.EQ.2) THEN
      PURP=14
      CALL INTAB(FILENO,VAR,IZ,PURP,DUMMY,IO)
      DO 114 II=1,MAX_ZONES
  114 INVEHB(II)=FLOAT(VAR(II))/100.0
      ELSE
      DO 115 II=1,MAX_ZONES
  115 INVEHB(II)=0.0
      END IF
C ------------------------------------------------------------------------
C WALK ACCESS TO STATIONS
C -----------------------------------------------------------------------
      DO 210 SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      IF(STANUM(SC).NE.IMODE) GOTO 210
      IF(STADATA(SC,6).Lt.1.0) GOTO 210
      XDIST=WLKLSM(IC)
C
C..EVALUATE FOR FOUR CLOSEST STATIONS
C
      IF(XDIST.GT.WDIST(IMODE,1)) THEN
         WDIST(IMODE,5)=WDIST(IMODE,4)
         WSTA(IMODE,5)=WSTA(IMODE,4)
         WDIST(IMODE,4)=WDIST(IMODE,3)
         WSTA(IMODE,4)=WSTA(IMODE,3)
         WDIST(IMODE,3)=WDIST(IMODE,2)
         WSTA(IMODE,3)=WSTA(IMODE,2)
         WDIST(imode,2)=WDIST(imode,1)
         WSTA(imode,2)=WSTA(imode,1)
         WDIST(imode,1)=XDIST
         WSTA(imode,1)=IC
         GO TO 210
      END IF
      IF(XDIST.GT.WDIST(IMODE,2)) THEN
         WDIST(IMODE,5)=WDIST(IMODE,4)
         WSTA(IMODE,5)=WSTA(IMODE,4)
         WDIST(IMODE,4)=WDIST(IMODE,3)
         WSTA(IMODE,4)=WSTA(IMODE,3)
         WDIST(IMODE,3)=WDIST(IMODE,2)
         WSTA(IMODE,3)=WSTA(IMODE,2)
         WDIST(imode,2)=XDIST
         WSTA(imode,2)=IC
         GO TO 210
      END IF
      IF(XDIST.GT.WDIST(IMODE,3)) THEN
         WDIST(IMODE,5)=WDIST(IMODE,4)
         WSTA(IMODE,5)=WSTA(IMODE,4)
         WDIST(IMODE,4)=WDIST(IMODE,3)
         WSTA(IMODE,4)=WSTA(IMODE,3)
         WDIST(IMODE,3)=XDIST
         WSTA(IMODE,3)=IC
         GO TO 210
      END IF
      IF(XDIST.GT.WDIST(IMODE,4)) THEN
         WDIST(IMODE,5)=WDIST(IMODE,4)
         WSTA(IMODE,5)=WSTA(IMODE,4)
         WDIST(IMODE,4)=XDIST
         WSTA(IMODE,4)=IC
         GO TO 210
      END IF
      IF(XDIST.GT.WDIST(IMODE,5)) THEN
         WDIST(IMODE,5)=XDIST
         WSTA(IMODE,5)=IC
      END IF
  210 CONTINUE
      IF(WSTA(IMODE,1).LE.0) WDIST(IMODE,1)=0.0
      IF(WSTA(IMODE,2).LE.0) WDIST(IMODE,2)=0.0
      IF(WSTA(IMODE,3).LE.0) WDIST(IMODE,3)=0.0
      IF(WSTA(IMODE,4).LE.0) WDIST(IMODE,4)=0.0
      IF(WSTA(IMODE,5).LE.0) WDIST(IMODE,5)=0.0
      IF(WSTA(IMODE,1).GT.0) WINDEX=1
      IF(WSTA(IMODE,2).GT.0) WINDEX=2
      IF(WSTA(IMODE,3).GT.0) WINDEX=3
      IF(WSTA(IMODE,4).GT.0) WINDEX=4
      IF(WSTA(IMODE,5).GT.0) WINDEX=5
C...................................................................      
      SC=WSTA(imode,1)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS 
      SC2=WSTA(imode,2)-MAX_IZONES
      IF(SC2.LT.0) SC2=MAX_STATIONS 
      SC3=WSTA(IMODE,3)-MAX_IZONES
      IF(SC3.LT.0) SC3=MAX_STATIONS
      SC4=WSTA(IMODE,4)-MAX_IZONES
      IF(SC4.LT.0) SC4=MAX_STATIONS
      SC5=WSTA(IMODE,5)-MAX_IZONES
      IF(SC5.LT.0) SC5=MAX_STATIONS
      IF(DEBUG) THEN
      WRITE(26,9007) NAME(IMODE),IZ,WSTA(imode,1),STANAME(SC),
     *               WDIST(imode,1),
     *               WSTA(imode,2),STANAME(SC2),WDIST(imode,2),
     *               WSTA(imode,3),STANAME(SC3),WDIST(imode,3),
     *               WSTA(imode,4),STANAME(SC4),WDIST(imode,4),
     *               WSTA(imode,5),STANAME(SC5),WDIST(imode,5)
 9007 FORMAT(//1X,'Candidate Station Selection (Walk Access) -- ',a13/
     *       1X,'-------------------------------------------'/
     *       1X,'ORIGIN   ZONE   NUMBER  =',I9/  
     *       1X,'CLOSEST STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'SECOND  STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'THIRD   STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'FOURTH  STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'FIFTH   STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3)
      END IF   
C..........................................................................
      IF(WINDEX.LT.1.OR.(.NOT.DEBUG)) GO TO 576
      GO TO (571,572,573,574,575),WINDEX
  571 WRITE((IMODE+40),9001)  IZ,STANAME(SC),WSTA(IMODE,1),
     *                         WDIST(IMODE,1)
 9001 FORMAT(I5,5(1X,A25,'(',I4,',',F8.4,')'))
      GO TO 576
  572 WRITE((IMODE+40),9001)  IZ,STANAME(SC),WSTA(IMODE,1),
     *                         WDIST(IMODE,1),
     *                     STANAME(SC2),WSTA(IMODE,2),WDIST(IMODE,2)
      GO TO 576
  573 WRITE((IMODE+40),9001)  IZ,STANAME(SC),WSTA(IMODE,1),
     *                         WDIST(IMODE,1),
     *                     STANAME(SC2),WSTA(IMODE,2),WDIST(IMODE,2),
     *                     STANAME(SC3),WSTA(IMODE,3),WDIST(IMODE,3)
      GO TO 576
  574 WRITE((IMODE+40),9001)  IZ,STANAME(SC),WSTA(IMODE,1),
     *                         WDIST(IMODE,1),
     *                     STANAME(SC2),WSTA(IMODE,2),WDIST(IMODE,2),
     *                     STANAME(SC3),WSTA(IMODE,3),WDIST(IMODE,3),
     *                     STANAME(SC4),WSTA(IMODE,4),WDIST(IMODE,4)
      GO TO 576
  575 WRITE((IMODE+40),9001)  IZ,STANAME(SC),WSTA(IMODE,1),
     *                         WDIST(IMODE,1),
     *                     STANAME(SC2),WSTA(IMODE,2),WDIST(IMODE,2),
     *                     STANAME(SC3),WSTA(IMODE,3),WDIST(IMODE,3),
     *                     STANAME(SC4),WSTA(IMODE,4),WDIST(IMODE,4),
     *                     STANAME(SC5),WSTA(IMODE,5),WDIST(IMODE,5)  
  576 CONTINUE
C ------------------------------------------------------------------------
C BIKE ACCESS TO STATIONS
C -----------------------------------------------------------------------
      DO 310 SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      IF(STANUM(SC).NE.IMODE) GOTO 310
      IF(STADATA(SC,6).Lt.1.0) GOTO 310
      XDIST=BYCLSM(IC)
C
C..EVALUATE FOR FOUR CLOSEST STATIONS
C
  303 CONTINUE
      IF(XDIST.GT.BIKDIST(IMODE,1)) THEN
         BIKDIST(IMODE,5)=BIKDIST(IMODE,4)
         BIKSTA(IMODE,5)=BIKSTA(IMODE,4)
         BIKDIST(IMODE,4)=BIKDIST(IMODE,3)
         BIKSTA(IMODE,4)=BIKSTA(IMODE,3)
         BIKDIST(IMODE,3)=BIKDIST(IMODE,2)
         BIKSTA(IMODE,3)=BIKSTA(IMODE,2)
         BIKDIST(imode,2)=BIKDIST(imode,1)
         BIKSTA(imode,2)=BIKSTA(imode,1)
         BIKDIST(imode,1)=XDIST
         BIKSTA(imode,1)=IC
         GO TO 310
      END IF
      IF(XDIST.GT.BIKDIST(IMODE,2)) THEN
         BIKDIST(IMODE,5)=BIKDIST(IMODE,4)
         BIKSTA(IMODE,5)=BIKSTA(IMODE,4)
         BIKDIST(IMODE,4)=BIKDIST(IMODE,3)
         BIKSTA(IMODE,4)=BIKSTA(IMODE,3)
         BIKDIST(IMODE,3)=BIKDIST(IMODE,2)
         BIKSTA(IMODE,3)=BIKSTA(IMODE,2)
         BIKDIST(imode,2)=XDIST
         BIKSTA(imode,2)=IC
         GO TO 310
      END IF
      IF(XDIST.GT.BIKDIST(IMODE,3)) THEN
         BIKDIST(IMODE,5)=BIKDIST(IMODE,4)
         BIKSTA(IMODE,5)=BIKSTA(IMODE,4)
         BIKDIST(IMODE,4)=BIKDIST(IMODE,3)
         BIKSTA(IMODE,4)=BIKSTA(IMODE,3)
         BIKDIST(IMODE,3)=XDIST
         BIKSTA(IMODE,3)=IC
         GO TO 310
      END IF
      IF(XDIST.GT.BIKDIST(IMODE,4)) THEN
         BIKDIST(IMODE,5)=BIKDIST(IMODE,4)
         BIKSTA(IMODE,5)=BIKSTA(IMODE,4)
         BIKDIST(IMODE,4)=XDIST
         BIKSTA(IMODE,4)=IC
         GO TO 310
      END IF
      IF(XDIST.GT.BIKDIST(IMODE,5)) THEN
         BIKDIST(IMODE,5)=XDIST
         BIKSTA(IMODE,5)=IC
      END IF
  310 CONTINUE
      IF(BIKSTA(IMODE,1).LE.0) BIKDIST(IMODE,1)=0.0
      IF(BIKSTA(IMODE,2).LE.0) BIKDIST(IMODE,2)=0.0
      IF(BIKSTA(IMODE,3).LE.0) BIKDIST(IMODE,3)=0.0
      IF(BIKSTA(IMODE,4).LE.0) BIKDIST(IMODE,4)=0.0
      IF(BIKSTA(IMODE,5).LE.0) BIKDIST(IMODE,5)=0.0
C...................................................................      
      SC=BIKSTA(imode,1)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS 
      SC2=BIKSTA(imode,2)-MAX_IZONES
      IF(SC2.LT.0) SC2=MAX_STATIONS 
      SC3=BIKSTA(IMODE,3)-MAX_IZONES
      IF(SC3.LT.0) SC3=MAX_STATIONS
      SC4=BIKSTA(IMODE,4)-MAX_IZONES
      IF(SC4.LT.0) SC4=MAX_STATIONS
      SC5=BIKSTA(IMODE,5)-MAX_IZONES
      IF(SC5.LT.0) SC5=MAX_STATIONS
      IF(DEBUG) THEN
      WRITE(26,9027) NAME(IMODE),IZ,BIKSTA(imode,1),STANAME(SC),
     *               BIKDIST(imode,1),
     *               BIKSTA(imode,2),STANAME(SC2),BIKDIST(imode,2),
     *               BIKSTA(imode,3),STANAME(SC3),BIKDIST(imode,3),
     *               BIKSTA(imode,4),STANAME(SC4),BIKDIST(imode,4),
     *               BIKSTA(imode,5),STANAME(SC5),BIKDIST(imode,5)
 9027 FORMAT(//1X,'Candidate Station Selection (Bike Access) -- ',a13/
     *       1X,'-------------------------------------------'/
     *       1X,'ORIGIN   ZONE   NUMBER  =',I9/  
     *       1X,'CLOSEST STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'SECOND  STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'THIRD   STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'FOURTH  STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'FIFTH   STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3)
      END IF   
C----------------------------------------------------------------------------
C FEEDER BUS ACCESS TO STATIONS
C LOOP THROUGH STATIONS
C----------------------------------------------------------------------------
      DO 110 SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      IF(STANUM(SC).NE.IMODE) GOTO 110
      IF(STADATA(SC,6).Lt.1.0) GOTO 110
C
C CHECK FOR IN-VEHICLE TIME < LARGE AND NO BUS IN-VEHICLE TIME
C
      INVEH(IC)=INVEHL(IC)+INVEHR(IC)+INVEHE(IC)+INVEHT(IC)+INVEHB(IC)  
      IF((INVEH(IC).LE.0.0).OR.(INVEH(IC).GE.99999.9)) GO TO 110
C
C CHECK FOR RAIL TIME > 0
C      
      IF(LHIVT(IC).GT.0.0) GO TO 110
C
C  CALCULATE TOTAL LINK UNRELIABILITY
C
      IF(LUNREL) THEN
      LUNRVAL(IC)=0.0
      DO T=T1(IMODE),T2(IMODE)
      LUNRVAL(IC)=LUNRVAL(IC)+LUNRELM(IC,T)
      END DO
      END IF
C
C  CALCULATE TOTAL STOP UNRELIABILITY
C
      IF(NUNREL) THEN
      NUNRVAL(IC)=0.0
      DO T=T1(IMODE),T2(IMODE)
      NUNRVAL(IC)=NUNRVAL(IC)+NUNRELM(IC,T)
      END DO
      END IF
C
C  CALCULATE TOTAL LINK CROWDING
C
      IF(LCROWD) THEN
      LCRDVAL(IC)=0.0
      DO T=T1(IMODE),T2(IMODE)
      LCRDVAL(IC)=LCRDVAL(IC)+LCROWDM(IC,T)
      END DO
      END IF
C
C  CALCULATE TOTAL STOP CAPACITY
C
      IF(NCAPAC) THEN
      NCAPVAL(IC)=0.0
      DO T=T1(IMODE),T2(IMODE)
      NCAPVAL(IC)=NCAPVAL(IC)+NCAPACM(IC,T)
      END DO
      END IF
C
C COMPUTE ACCESS PORTION OF UTILITY
C
C....USING MODEL COEFFICIENTS
      BUSUTL=COEFF(1)*INVEH(IC) + COEFF(3)*WAIT1(IC) + 
     *       COEFF(4)*WAIT2(IC) + COEFF(5)*(TRANSF(IC)) +
     *       COEFF(7)*(WALKTFR(IC)+WALKEGR(IC)) +
     *       COEFF(75)* (LUNRVAL(IC)/LUNRDEM) +
     *       COEFF(76)* (NUNRVAL(IC)/LUNRDEM) +
     *       COEFF(77)* (LCRDVAL(IC)/LUNRDEM) +
     *       COEFF(78)* (NCAPVAL(IC)/LUNRDEM)
C.........................................................................
      IF(SDETAIL) THEN
      WRITE(26,8002) IC,BUSUTL
 8002 FORMAT( ' STATION=',I4,' MODEL UTIL=',F10.2)
      END IF
C........................................................................
C
C..EVALUATE FOR TWO "BEST" STATIONS
C
      IF(BUSUTL.GT.BDIST(imode,2)) THEN
       IF(BUSUTL.GT.BDIST(imode,1)) THEN
       BDIST(imode,2)=BDIST(imode,1)
       BUTL(imode,2)=BUTL(imode,1)
       TWALK(imode,2)=TWALK(imode,1)
       BSTA(imode,2)=BSTA(imode,1)
       BDIST(imode,1)=BUSUTL
       BUTL(imode,1)=BUSUTL
       TWALK(imode,1)=WALKACC(IC)
       BSTA(imode,1)=IC
       ELSE
       BDIST(imode,2)=BUSUTL
       BUTL(imode,2)=BUSUTL
       TWALK(imode,2)=WALKACC(IC)
       BSTA(imode,2)=IC
       END IF
      END IF
C....................................................................
  110 CONTINUE
      IF(BSTA(IMODE,1).GT.0) 
     *      BTXFER(IMODE,1)=IFIX(TRANSF(BSTA(IMODE,1)))
      IF(BSTA(IMODE,2).GT.0) 
     *      BTXFER(IMODE,2)=IFIX(TRANSF(BSTA(IMODE,2)))
C
C STORE SKIM INFORMATION
C
      IF(BSTA(IMODE,1).GT.0) THEN
      IC=BSTA(IMODE,1)
      SKIM(1,1)=INVEH(IC)
      SKIM(1,2)=INVEHL(IC)
      SKIM(1,3)=WAIT1(IC)
      SKIM(1,4)=WAIT2(IC)
      SKIM(1,5)=WALKTFR(IC)
      SKIM(1,6)=FARE(IC)
      SKIM(1,7)=WALKACC(IC)
      SKIM(1,8)=WALKEGR(IC)
      SKIM(1,9)=INVEHR(IC)
      SKIM(1,10)=INVEHE(IC)
      SKIM(1,11)=INVEHT(IC)
      SKIM(1,12)=INVEHB(IC)
      IF(LUNREL) SKIM(1,13)=LUNRVAL(IC)
      IF(NUNREL) SKIM(1,14)=NUNRVAL(IC)
      IF(LCROWD) SKIM(1,15)=LCRDVAL(IC)
      IF(NCAPAC) SKIM(1,16)=NCAPVAL(IC)
      ELSE
      SKIM(1,1)=0
      SKIM(1,2)=0
      SKIM(1,3)=0
      SKIM(1,4)=0
      SKIM(1,5)=0
      SKIM(1,6)=0
      SKIM(1,7)=0
      SKIM(1,8)=0
      SKIM(1,9)=0
      SKIM(1,10)=0
      SKIM(1,11)=0
      SKIM(1,12)=0
      END IF
C....
      IF(BSTA(IMODE,2).GT.0) THEN
      IC=BSTA(IMODE,2)
      SKIM(2,1)=INVEH(IC)
      SKIM(2,2)=INVEHL(IC)
      SKIM(2,3)=WAIT1(IC)
      SKIM(2,4)=WAIT2(IC)
      SKIM(2,5)=WALKTFR(IC)
      SKIM(2,6)=FARE(IC)
      SKIM(2,7)=WALKACC(IC)
      SKIM(2,8)=WALKEGR(IC)
      SKIM(2,9)=INVEHR(IC)
      SKIM(2,10)=INVEHE(IC)
      SKIM(2,11)=INVEHT(IC)
      SKIM(2,12)=INVEHB(IC)   
      IF(LUNREL) SKIM(2,13)=LUNRVAL(IC)
      IF(NUNREL) SKIM(2,14)=NUNRVAL(IC)
      IF(LCROWD) SKIM(2,15)=LCRDVAL(IC)
      IF(NCAPAC) SKIM(2,16)=NCAPVAL(IC)
      ELSE
      SKIM(2,1)=0
      SKIM(2,2)=0
      SKIM(2,3)=0
      SKIM(2,4)=0
      SKIM(2,5)=0
      SKIM(2,6)=0
      SKIM(2,7)=0
      SKIM(2,8)=0
      SKIM(2,9)=0
      SKIM(2,10)=0
      SKIM(2,11)=0
      SKIM(2,12)=0
      END IF
C
C DETERMINE IF BUS MODES ARE USED
C
      IF(BSTA(IMODE,1).GT.0) THEN
      IF(INVEHL(BSTA(IMODE,1)).GT.0) BUSMODE(IMODE,1,1)=1
      IF(INVEHR(BSTA(IMODE,1)).GT.0) BUSMODE(IMODE,2,1)=1
      IF(INVEHE(BSTA(IMODE,1)).GT.0) BUSMODE(IMODE,3,1)=1
      IF(INVEHT(BSTA(IMODE,1)).GT.0) BUSMODE(IMODE,4,1)=1
      IF(INVEHB(BSTA(IMODE,1)).GT.0) BUSMODE(IMODE,5,1)=1
      IF(INVEHU(BSTA(IMODE,1)).GT.0) BUSMODE(IMODE,6,1)=1
      END IF
      IF(BSTA(IMODE,2).GT.0) THEN
      IF(INVEHL(BSTA(IMODE,2)).GT.0) BUSMODE(IMODE,1,2)=1
      IF(INVEHR(BSTA(IMODE,2)).GT.0) BUSMODE(IMODE,2,2)=1
      IF(INVEHE(BSTA(IMODE,2)).GT.0) BUSMODE(IMODE,3,2)=1
      IF(INVEHT(BSTA(IMODE,2)).GT.0) BUSMODE(IMODE,4,2)=1
      IF(INVEHB(BSTA(IMODE,2)).GT.0) BUSMODE(IMODE,5,2)=1
      IF(INVEHU(BSTA(IMODE,2)).GT.0) BUSMODE(IMODE,6,2)=1
      END IF
C...................................................................
      IF(DEBUG) THEN
      SC=BSTA(imode,1)-MAX_IZONES
      IF(SC.LT.0) THEN
      IC1=MAX_ZONES
      ELSE
      IC1=BSTA(IMODE,1)
      END IF
      IF(SC.LT.0) SC=MAX_STATIONS
      SC2=BSTA(imode,2)-MAX_IZONES
      IF(SC2.LT.0) THEN
      IC2=MAX_ZONES
      ELSE
      IC2=BSTA(IMODE,2)
      END IF
      IF(SC2.LT.0) SC2=MAX_STATIONS
      WRITE(26,9017) NAME(IMODE), 
     * BSTA(imode,1),STANAME(SC),(SKIM(1,K1),K1=1,8),
     * INVEHL(IC1),INVEHR(IC1),
     * INVEHE(IC1),INVEHT(IC1),
     * INVEHB(IC1),INVEHU(IC1),
     * BTXFER(IMODE,1),LUNRVAL(IC1),NUNRVAL(IC1),
     * LCRDVAL(IC1),NCAPVAL(IC1),
     * BUTL(imode,1),
     * BSTA(imode,2),STANAME(SC2),
     * (SKIM(2,K2),K2=1,8),
     * INVEHL(IC2),INVEHR(IC2),
     * INVEHE(IC2),INVEHT(IC2),
     * INVEHB(IC2),INVEHU(IC2),
     *  BTXFER(IMODE,2),LUNRVAL(IC2),NUNRVAL(IC2),
     *  LCRDVAL(IC2),NCAPVAL(IC2),
     * BUTL(imode,2)
 9017 FORMAT(//1X,'Candidate Station Selection (Bus Access) -- ',a13/
     *       1X,'------------------------------------------'/
     *       1X,'CLOSEST     STATION NUMBER  =',I10,2X,A29/
     *       1X,'       TOTAL IN-VEHICLE TIME=',F10.3/
     *       1X,'   LOCAL BUS IN-VEHICLE TIME=',F10.3/
     *       1X,'             FIRST WAIT TIME=',F10.3/
     *       1X,'          TRANSFER WAIT TIME=',F10.3/
     *       1X,'          TRANSFER WALK TIME=',F10.3/
     *       1X,'                        FARE=',F10.3/
     *       1X,'            ACCESS WALK TIME=',F10.3/
     *       1X,'            EGRESS WALK TIME=',F10.3/
     *       1X,'   LOCAL BUS IN-VEHICLE TIME=',F10.3/
     *       1X,'   RAPID BUS IN-VEHICLE TIME=',F10.3/
     *       1X,' EXPRESS BUS IN-VEHICLE TIME=',F10.3/
     *       1X,'    TWY  BUS IN-VEHICLE TIME=',F10.3/
     *       1X,'    BRT  BUS IN-VEHICLE TIME=',F10.3/
     *       1X,'  URBAN RAIL IN-VEHICLE TIME=',F10.3/
     *       1X,'         NUMBER OF TRANSFERS=',I10/
     *       1X,'     LINK UNRELIABILITY TIME=',F10.3/
     *       1X,'     STOP UNRELIABILITY TIME=',F10.3/
     *       1X,'     LINK CROWDING      TIME=',F10.3/
     *       1X,'     STOP CAPACITY      TIME=',F10.3/
     *       1X,'            ACCESS  UTILITY =',F10.3//
     *       1X,'SECOND      STATION NUMBER  =',I10,2X,A29/
     *       1X,'       TOTAL IN-VEHICLE TIME=',F10.3/
     *       1X,'   LOCAL BUS IN-VEHICLE TIME=',F10.3/
     *       1X,'             FIRST WAIT TIME=',F10.3/
     *       1X,'          TRANSFER WAIT TIME=',F10.3/
     *       1X,'          TRANSFER WALK TIME=',F10.3/
     *       1X,'                        FARE=',F10.3/
     *       1X,'            ACCESS WALK TIME=',F10.3/
     *       1X,'            EGRESS WALK TIME=',F10.3/
     *       1X,'   LOCAL BUS IN-VEHICLE TIME=',F10.3/
     *       1X,'   RAPID BUS IN-VEHICLE TIME=',F10.3/
     *       1X,' EXPRESS BUS IN-VEHICLE TIME=',F10.3/
     *       1X,'    TWY  BUS IN-VEHICLE TIME=',F10.3/
     *       1X,'    BRT  BUS IN-VEHICLE TIME=',F10.3/
     *       1X,'  URBAN RAIL IN-VEHICLE TIME=',F10.3/
     *       1X,'         NUMBER OF TRANSFERS=',I10/
     *       1X,'     LINK UNRELIABILITY TIME=',F10.3/
     *       1X,'     STOP UNRELIABILITY TIME=',F10.3/
     *       1X,'     LINK CROWDING      TIME=',F10.3/
     *       1X,'     STOP CAPACITY      TIME=',F10.3/
     *       1X,'            ACCESS  UTILITY =',F10.3/)
      END IF
C....................................................................
      RETURN
      END
