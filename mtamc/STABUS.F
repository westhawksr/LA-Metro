C-------------------------------------------------------------------
C       WALK, BICYCLE, AND BUS ACCESS TO STATION SELECTION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE STABUS(IZ,BSTA,BUTL,imode,
     *                   WDIST,WSTA,LUNRVAL,NUNRVAL,
     *                   LCRDVAL,NCAPVAL,WLKLSM,BYCLSM,
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
      INTEGER*2    BSTA(5,2)
      INTEGER*2    WSTA(5,5),SC3,SC4,SC5,WINDEX,BIKSTA(5,5)
      INTEGER*2    T,T1(5),T2(5)
      REAL*4       BUSUTL,KTWY,KRPD
      REAL*4       BDIST(5,2),BUTL(5,2),WALKACC(MAX_ZONES)
      REAL*4       INVEH(MAX_ZONES),LHIVT(MAX_ZONES),
     *             FARE(MAX_ZONES),TRANSF(MAX_ZONES),
     *             WAIT1(MAX_ZONES),WAIT2(MAX_ZONES),
     *             WALKEGR(MAX_ZONES),STAWALK(MAX_ZONES)
      REAL*4       INVEHL(MAX_ZONES),INVEHR(MAX_ZONES),
     *             INVEHE(MAX_ZONES),INVEHT(MAX_ZONES),
     *             INVEHB(MAX_ZONES),DIRWALK(MAX_ZONES)
      REAL*4       WALKTFR(MAX_ZONES)
      REAL*4       WDIST(5,5),BIKDIST(5,5),LUNRDEM,TWAIT
      INTEGER*2    LUNRVAL(MAX_ZONES,5)
      INTEGER*2    NUNRVAL(MAX_ZONES,5)
      INTEGER*2    LCRDVAL(MAX_ZONES,5)
      INTEGER*2    NCAPVAL(MAX_ZONES,5)
      REAL*8       WLKLSM(MAX_ZONES),BYCLSM(MAX_ZONES)
      CHARACTER*13 NAME(5)
      DATA         T1/17,17,0,0,17/,T2/20,20,0,0,20/
      DATA         NAME/ 'Commuter Rail',
     *                   'Urban Rail   ',
     *                   'Express Bus  ',
     *                   'Transitway   ',
     *                   'BRT          '/
C
C INITIALIZE CHOICE VARIABLES
C
      BDIST(imode,1)=-99999.9
      BDIST(imode,2)=-99999.9
      BUTL(imode,1)=0.0
      BUTL(imode,2)=0.0
      BSTA(imode,1)=0
      BSTA(imode,2)=0
      WSTA(imode,1)=0
      WSTA(imode,2)=0
      WSTA(imode,3)=0
      WSTA(imode,4)=0
      WSTA(imode,5)=0
      IF(WALKTIME) THEN
      WDIST(imode,1)=99.9
      WDIST(imode,2)=99.9  
      WDIST(imode,3)=99.9 
      WDIST(imode,4)=99.9   
      WDIST(imode,5)=99.9
      ELSE
      WDIST(imode,1)=-99.9
      WDIST(imode,2)=-99.9  
      WDIST(imode,3)=-99.9 
      WDIST(imode,4)=-99.9   
      WDIST(imode,5)=-99.9
      END IF
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
      FILENO=66
      IF(IMODE.EQ.1) LUNRDEM=LSUM2CR*LSUM1TRN*LSUMT*LSUM3CW
      IF(IMODE.EQ.2) LUNRDEM=LSUM2UR*LSUM1TRN*LSUMT*LSUM3UW
      IF(IMODE.EQ.5) LUNRDEM=LSUM2BR*LSUM1TRN*LSUMT*LSUM3BRW
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
C...TOTAL IN-VEHICLE TIME
      PURP=10
      call intab(fileno,VAR,iz,PURP,dummy,io)
      DO 101,II=1,MAX_ZONES
 101  INVEH(II)=FLOAT(VAR(II))/100.0
C...WALK TIME EGRESS
      PURP=11
      call intab(fileno,VAR,iz,PURP,dummy,io)
      DO 108,II=1,MAX_ZONES
  108 WALKEGR(II)=FLOAT(VAR(II))/100.0
C...WALK TIME TRANSFER
      PURP=12
      call intab(fileno,VAR,iz,PURP,dummy,io)
      DO 109,II=1,MAX_ZONES
  109 WALKTFR(II)=FLOAT(VAR(II))/100.0
C...ZONE TO STATION WALK TIME
      PURP=13
      call intab(fileno,VAR,iz,PURP,dummy,io)
      DO 115,II=1,MAX_ZONES
  115 STAWALK(II)=FLOAT(VAR(II))/100.0
C ------------------------------------------------------------------------
C WALK ACCESS TO STATIONS
C -----------------------------------------------------------------------
      DO 210 SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      IF(STANUM(SC).NE.IMODE) GOTO 210
      IF(STADATA(SC,6).Lt.1.0) GOTO 210
C
C..EVALUATE FOR FOUR CLOSEST STATIONS
C
      IF(WALKTIME) THEN
      XDIST=STAWALK(IC)
      IF(XDIST.LE.0.0) GO TO 210
      IF(XDIST.LT.WDIST(IMODE,1)) THEN
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
      IF(XDIST.LT.WDIST(IMODE,2)) THEN
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
      IF(XDIST.LT.WDIST(IMODE,3)) THEN
         WDIST(IMODE,5)=WDIST(IMODE,4)
         WSTA(IMODE,5)=WSTA(IMODE,4)
         WDIST(IMODE,4)=WDIST(IMODE,3)
         WSTA(IMODE,4)=WSTA(IMODE,3)
         WDIST(IMODE,3)=XDIST
         WSTA(IMODE,3)=IC
         GO TO 210
      END IF
      IF(XDIST.LT.WDIST(IMODE,4)) THEN
         WDIST(IMODE,5)=WDIST(IMODE,4)
         WSTA(IMODE,5)=WSTA(IMODE,4)
         WDIST(IMODE,4)=XDIST
         WSTA(IMODE,4)=IC
         GO TO 210
      END IF
      IF(XDIST.LT.WDIST(IMODE,5)) THEN
         WDIST(IMODE,5)=XDIST
         WSTA(IMODE,5)=IC
      END IF
      ELSE
      XDIST=WLKLSM(IC)
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
     *       1X,'                UTILITY =',F9.3/
     *       1X,'SECOND  STATION NUMBER  =',I9,2X,A29/
     *       1X,'                UTILITY =',F9.3/
     *       1X,'THIRD   STATION NUMBER  =',I9,2X,A29/
     *       1X,'                UTILITY =',F9.3/
     *       1X,'FOURTH  STATION NUMBER  =',I9,2X,A29/
     *       1X,'                UTILITY =',F9.3/
     *       1X,'FIFTH   STATION NUMBER  =',I9,2X,A29/
     *       1X,'                UTILITY =',F9.3)
      END IF   
C..........................................................................
      IF(WINDEX.LT.1.OR.(.NOT.DEBUG)) GO TO 576
      IF(IMODE.GT.2) GO TO 576
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
      IF(BICYCLE) THEN
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
      END IF
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
      IF(DEBUG.AND.BICYCLE) THEN
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
     *       1X,'                UTILITY =',F9.3/
     *       1X,'SECOND  STATION NUMBER  =',I9,2X,A29/
     *       1X,'                UTILITY =',F9.3/
     *       1X,'THIRD   STATION NUMBER  =',I9,2X,A29/
     *       1X,'                UTILITY  ',F9.3/
     *       1X,'FOURTH  STATION NUMBER  =',I9,2X,A29/
     *       1X,'                UTILITY =',F9.3/
     *       1X,'FIFTH   STATION NUMBER  =',I9,2X,A29/
     *       1X,'                UTILITY =',F9.3)
      END IF   
C----------------------------------------------------------------------------
C FEEDER BUS ACCESS TO STATIONS
C LOOP THROUGH STATIONS
C----------------------------------------------------------------------------
      DO 110 SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      IF(STANUM(SC).NE.IMODE) GOTO 110
      IF(STADATA(SC,6).Lt.1.0) GOTO 110
      IF((NOTWYAE).AND.INVEHT(IC).GT.0) GO TO 110
C
C CHECK FOR IN-VEHICLE TIME < LARGE AND NO BUS IN-VEHICLE TIME
C
      IF((INVEH(IC).LE.0.0).OR.(INVEH(IC).GE.99999.9)) GO TO 110
C
C COMPUTE ACCESS PORTION OF UTILITY
C
C....USING MODEL COEFFICIENTS
      KTWY=0.0
      KRPD=0.0
      IF(INVEHT(IC).GT.0) KTWY=KTWYAE
      IF(INVEHR(IC).GT.0) KRPD=KRPDAE
      BUSUTL=COEFF(1)*INVEH(IC) + COEFF(3)*WAIT1(IC) + 
     *       COEFF(4)*WAIT2(IC) + COEFF(5)*(TRANSF(IC)) +
     *       COEFF(7)*(WALKTFR(IC)+WALKEGR(IC)) +
     *       COEFF(75)* (FLOATI(LUNRVAL(IC,5))/(LUNRDEM*100.0)) +
     *       COEFF(76)* (FLOATI(NUNRVAL(IC,5))/(LUNRDEM*100.0)) +
     *       COEFF(77)* (FLOATI(LCRDVAL(IC,5))/(LUNRDEM*100.0)) +
     *       COEFF(78)* (FLOATI(NCAPVAL(IC,5))/(LUNRDEM*100.0)) +
     *       KTWY/LUNRDEM + KRPD/LUNRDEM
      BUSACC(SC)=BUSUTL
      BUSFARE(IMODE,SC)=FARE(IC)
      BUSWALK(IMODE,SC)=WALKACC(IC)
C.........................................................................
C     IF(SDETAIL) THEN
C     WRITE(64,9017) NAME(IMODE),IZ,
C    * IC,STANAME(SC),
C    * INVEH(IC),WAIT1(IC),WAIT2(IC),
C    * WALKTFR(IC),FARE(IC),WALKACC(IC),WALKEGR(IC),
C    * INVEHL(IC),INVEHR(IC),
C    * INVEHE(IC),INVEHT(IC),
C    * TRANSF(IC),LUNRVAL(IC),NUNRVAL(IC),
C    * LCRDVAL(IC),NCAPVAL(IC),BUSUTL
 9017 FORMAT(//1X,'Candidate Station Selection (Bus Access) -- ',a13/
     *       1X,'------------------------------------------'/
     *       1X,'            PRODUCTION ZONE =',I10/
     *       1X,'            STATION NUMBER  =',I10,2X,A29/
     *       1X,'       TOTAL IN-VEHICLE TIME=',F10.3/
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
     *       1X,'         NUMBER OF TRANSFERS=',F10.0/
     *       1X,'     LINK UNRELIABILITY TIME=',F10.3/
     *       1X,'     STOP UNRELIABILITY TIME=',F10.3/
     *       1X,'     LINK CROWDING      TIME=',F10.3/
     *       1X,'     STOP CAPACITY      TIME=',F10.3/
     *       1X,'            ACCESS  UTILITY =',F10.3/)
C     END IF
C........................................................................
C
C..EVALUATE FOR TWO "BEST" STATIONS
C
      IF(BUSUTL.GT.BDIST(imode,2)) THEN
       IF(BUSUTL.GT.BDIST(imode,1)) THEN
       BDIST(imode,2)=BDIST(imode,1)
       BUTL(imode,2)=BUTL(imode,1)
       BSTA(imode,2)=BSTA(imode,1)
       BDIST(imode,1)=BUSUTL
       BUTL(imode,1)=BUSUTL
       BSTA(imode,1)=IC
       ELSE
       BDIST(imode,2)=BUSUTL
       BUTL(imode,2)=BUSUTL
       BSTA(imode,2)=IC
       END IF
      END IF
C....................................................................
  110 CONTINUE
      BUSTWO(IZ,IMODE,1)=BSTA(IMODE,1)
      BUSTWO(IZ,IMODE,2)=BSTA(IMODE,2)
      RETURN
      END
