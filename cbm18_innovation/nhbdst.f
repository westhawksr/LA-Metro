C-------------------------------------------------
C     NHB DIRECT DEMAND MODEL
C     DESTINATION CHOICE APPLICATION
C-------------------------------------------------
      SUBROUTINE NHBDST(SSDIST,NHBTRP,NHBUTL)
      INCLUDE 'param.com'
      INCLUDE 'stadat.com'
      INCLUDE 'mtamcpar.inc'
      INTEGER*2     II
      INTEGER*4     SC,SC2,ROW(MAX_ZONES),INDEX,TX,ITER
      REAL*4        SSDIST(MAX_STATIONS,MAX_STATIONS)
      REAL*4        NHBTRP(MAX_STATIONS,4),WBO,WTW,OTO
      REAL*4        ATTBAL(MAX_STATIONS,3),ESTATR(MAX_STATIONS,3)
      REAL*4        BALFAC(MAX_STATIONS,3)
      REAL*4        NHBUTL(MAX_STATIONS,MAX_STATIONS)
      REAL*4        NUMER(MAX_STATIONS),DENOM
      REAL*4        URSS1(MAX_STATIONS,MAX_STATIONS)
      REAL*4        URSS2(MAX_STATIONS,MAX_STATIONS)
      REAL*4        URSS3(MAX_STATIONS,MAX_STATIONS)
      REAL*4        STASUM(MAX_STATIONS,4)
      REAL*4        STASUM2(MAX_STATIONS,4)
      REAL*4        TESM(4),TESM2(4),TEMP,REM1,REM2,REM3
      REAL*4        TLF(50,3),TXFERS(12,4),TRATE(4)
      REAL*4        VALMIN,MAXVAL
      LOGICAL       CONVERGE
C
      WRITE(*,8000)
 8000 FORMAT(/1X,'NHB Direct Demand Model Destination Choice'/)
C     OPEN(105,FILE='NHBDST_ATTBAL.CSV',STATUS='UNKNOWN',
C    *         FORM='FORMATTED')
      BALFAC=1.0
      VALMIN=0.98
      MAXVAL=1.02
C   
C     SET ATTBAL VALUES TO GENERATION OUTPUT
C
      DO SC=1,MAX_STATIONS
      ATTBAL(SC,1)=NHBTRP(SC,2)
      ATTBAL(SC,2)=NHBTRP(SC,3)/2.0
      ATTBAL(SC,3)=NHBTRP(SC,4)/2.0
      END DO
C
C     ATTRACTION BALANCING ITERATIONS
C
      DO 1000 ITER=1,20
      WRITE(*,8001) ITER
 8001 FORMAT(10X,'Iteration=',I3)
      CONVERGE=.TRUE.
C
C     WBO DESTINATION CHOICE
C
      DO 100 SC=1,MAX_STATIONS
C
      IF(STANUM(SC).NE.2.OR.STADATA(SC,6).NE.1) GO TO 100
      IF(SDETAIL) WRITE(26,9001) (SC+MAX_IZONES)
 9001 FORMAT(' PROD STATION=',I4)
C -----------------    
C     WBO TRIPS
C -----------------
      DENOM=0.0
      NUMER=0.0
C
      DO 200 SC2=1,MAX_STATIONS
C
      IF(SDETAIL) WRITE(26,9006) (SC2+MAX_IZONES),STANUM(SC2),
     *    STADATA(SC2,6),NHBTRP(SC,1),NHBUTL(SC,SC2)
 9006 FORMAT(' SC2=',I4,' STANUM=',I1,' STADATA(6)=',F2.0,
     *       ' NHBTRP(1)=',F8.2,' NHBUTL=',F10.4)
      IF(STANUM(SC2).NE.2.OR.STADATA(SC2,6).NE.1) GO TO 200
      IF(SC.EQ.SC2) GO TO 200
      IF(NHBTRP(SC,1).LE.0) GO TO 200
      IF(NHBUTL(SC,SC2).EQ.0.0) GO TO 200
      NUMER(SC2)=ATTBAL(SC2,1)*
     *    EXP(0.75*NHBUTL(SC,SC2)-0.1*SSDIST(SC,SC2)
     *        -2.0*(SSDIST(SC,SC2)**(-0.5)))
      DENOM=DENOM+NUMER(SC2)
      IF(SDETAIL) WRITE(26,9002) (SC2+MAX_IZONES),NHBTRP(SC2,2),
     *               NHBUTL(SC,SC2),SSDIST(SC,SC2),NUMER(SC2)
 9002 FORMAT(' DEST STA=',I4,' ATTR=',F6.1,' STA-STA UTIL=',F8.4,
     *       ' STA-STA DIST=',F6.2,' NUMER=',F8.4)
  200 CONTINUE
      DO 210 SC2=1,MAX_STATIONS
      IF(STANUM(SC2).NE.2.OR.STADATA(SC2,6).NE.1) GO TO 210
      IF(SC.EQ.SC2) GO TO 210
      IF(NHBTRP(SC,1).LE.0) GO TO 210
      URSS1(SC,SC2)=NHBTRP(SC,1)*(NUMER(SC2)/DENOM)
      STASUM(SC,1)=STASUM(SC,1)+URSS1(SC,SC2)
      STASUM(SC,4)=STASUM(SC,4)+URSS1(SC,SC2)
      TESM(1)=TESM(1)+URSS1(SC,SC2)
      TESM(4)=TESM(4)+URSS1(SC,SC2)
      STASUM2(SC2,1)=STASUM2(SC2,1)+URSS1(SC,SC2)
      STASUM2(SC2,4)=STASUM2(SC2,4)+URSS1(SC,SC2)
      TESM2(1)=TESM2(1)+URSS1(SC,SC2)
      TESM2(4)=TESM2(4)+URSS1(SC,SC2)
      IF(SDETAIL) WRITE(26,9003) (SC2+MAX_IZONES),URSS1(SC,SC2)
 9003 FORMAT(' DEST STA=',I4,' WBO=',F8.2)
      INDEX=IFIX(SSDIST(SC,SC2))+1
      IF(INDEX.EQ.0.OR.INDEX.GT.50) THEN
      WRITE(26,9100) SC,SC2,SSDIST(SC,SC2)
 9100 FORMAT(' STA=',I4,' TO STA=',I4,' DIST=',F6.2,
     *       ' EXCEEDS MAXIMUM ALLOWABLE VALUE OF 50')
      STOP 210
      END IF
      TLF(INDEX,1)=TLF(INDEX,1)+URSS1(SC,SC2)
      ESTATR(SC2,1)=ESTATR(SC2,1)+URSS1(SC,SC2)
  210 CONTINUE
C -----------------    
C     WTW TRIPS
C -----------------
      DENOM=0.0
      NUMER=0.0
C
      DO 300 SC2=1,MAX_STATIONS
C
      IF(STANUM(SC2).NE.2.OR.STADATA(SC2,6).NE.1) GO TO 300
      IF(SC.EQ.SC2) GO TO 300
      IF(NHBTRP(SC,3).LE.0) GO TO 300
      IF(NHBUTL(SC,SC2).EQ.0.0) GO TO 300
      NUMER(SC2)=(ATTBAL(SC2,2)/1.0)*
     *    EXP(0.75*NHBUTL(SC,SC2)-0.1*SSDIST(SC,SC2)
     *        -2.0*(SSDIST(SC,SC2)**(-0.5)))
      DENOM=DENOM+NUMER(SC2)
      IF(SDETAIL) WRITE(26,9002) (SC2+MAX_IZONES),NHBTRP(SC2,3),
     *               NHBUTL(SC,SC2),SSDIST(SC,SC2),NUMER(SC2)
  300 CONTINUE
      DO 310 SC2=1,MAX_STATIONS
      IF(STANUM(SC2).NE.2.OR.STADATA(SC2,6).NE.1) GO TO 310
      IF(SC.EQ.SC2) GO TO 310
      IF(NHBTRP(SC,3).LE.0) GO TO 310
      URSS2(SC,SC2)=(NHBTRP(SC,3)/2.0)*(NUMER(SC2)/DENOM)
      STASUM(SC,2)=STASUM(SC,2)+URSS2(SC,SC2)
      STASUM(SC,4)=STASUM(SC,4)+URSS2(SC,SC2)
      TESM(2)=TESM(2)+URSS2(SC,SC2)
      TESM(4)=TESM(4)+URSS2(SC,SC2)
      STASUM2(SC2,2)=STASUM2(SC2,2)+URSS2(SC,SC2)
      STASUM2(SC2,4)=STASUM2(SC2,4)+URSS2(SC,SC2)
      TESM2(2)=TESM2(2)+URSS2(SC,SC2)
      TESM2(4)=TESM2(4)+URSS2(SC,SC2)
      IF(SDETAIL) WRITE(26,9004) (SC2+MAX_IZONES),URSS2(SC,SC2)
 9004 FORMAT(' DEST STA=',I4,' WTW=',F8.2)
      ESTATR(SC2,2)=ESTATR(SC2,2)+URSS2(SC,SC2)
  310 CONTINUE
C -----------------    
C     OTO TRIPS
C -----------------
      DENOM=0.0
      NUMER=0.0
C
      DO 400 SC2=1,MAX_STATIONS
C
      IF(STANUM(SC2).NE.2.OR.STADATA(SC2,6).NE.1) GO TO 400
      IF(SC.EQ.SC2) GO TO 400
      IF(NHBTRP(SC,4).LE.0) GO TO 400
      IF(NHBUTL(SC,SC2).EQ.0.0) GO TO 400
      NUMER(SC2)=(ATTBAL(SC2,3)/1.0)*
     *    EXP(0.75*NHBUTL(SC,SC2)-0.1*SSDIST(SC,SC2)
     *        -2.0*(SSDIST(SC,SC2)**(-0.5)))
      DENOM=DENOM+NUMER(SC2)
      IF(SDETAIL) WRITE(26,9002) (SC2+MAX_IZONES),NHBTRP(SC2,4),
     *               NHBUTL(SC,SC2),SSDIST(SC,SC2),NUMER(SC2)
  400 CONTINUE
      DO 410 SC2=1,MAX_STATIONS
      IF(STANUM(SC2).NE.2.OR.STADATA(SC2,6).NE.1) GO TO 410
      IF(SC.EQ.SC2) GO TO 410
      IF(NHBTRP(SC,4).LE.0) GO TO 410
      URSS3(SC,SC2)=(NHBTRP(SC,4)/2.0)*(NUMER(SC2)/DENOM)
      STASUM(SC,3)=STASUM(SC,3)+URSS3(SC,SC2)
      STASUM(SC,4)=STASUM(SC,4)+URSS3(SC,SC2)
      TESM(3)=TESM(3)+URSS3(SC,SC2)
      TESM(4)=TESM(4)+URSS3(SC,SC2)
      STASUM2(SC2,3)=STASUM2(SC2,3)+URSS3(SC,SC2)
      STASUM2(SC2,4)=STASUM2(SC2,4)+URSS3(SC,SC2)
      TESM2(3)=TESM2(3)+URSS3(SC,SC2)
      TESM2(4)=TESM2(4)+URSS3(SC,SC2)
      IF(SDETAIL) WRITE(26,9005) (SC2+MAX_IZONES),URSS3(SC,SC2)
 9005 FORMAT(' DEST STA=',I4,' OTO=',F8.2)
      ESTATR(SC2,3)=ESTATR(SC2,3)+URSS3(SC,SC2)
  410 CONTINUE
  100 CONTINUE
C
C     COMPARE DESIRED AND ESTIMATED STATION ATTRACTIONS
C
C     WRITE(105,9316) ITER
 9316 FORMAT('ITERATION,',I3)
      DO SC=1,MAX_STATIONS
      DENOM=ESTATR(SC,1)+ESTATR(SC,2)+ESTATR(SC,3)
      IF(ESTATR(SC,1).GT.0) BALFAC(SC,1)=NHBTRP(SC,2)/ESTATR(SC,1)
      IF(ESTATR(SC,2).GT.0) 
     *    BALFAC(SC,2)=NHBTRP(SC,3)/(2.0*ESTATR(SC,2))
      IF(ESTATR(SC,3).GT.0) 
     *    BALFAC(SC,3)=NHBTRP(SC,4)/(2.0*ESTATR(SC,3))
      ATTBAL(SC,1)=ATTBAL(SC,1)*BALFAC(SC,1)
      ATTBAL(SC,2)=ATTBAL(SC,2)*BALFAC(SC,2)
      ATTBAL(SC,3)=ATTBAL(SC,3)*BALFAC(SC,3)
C
C     CHECK CONVERGENCE
C
      IF(DENOM.GT.0) THEN
      IF(BALFAC(SC,1).LT.VALMIN.OR.BALFAC(SC,1).GT.MAXVAL.OR.
     *   BALFAC(SC,2).LT.VALMIN.OR.BALFAC(SC,2).GT.MAXVAL.OR.
     *   BALFAC(SC,3).LT.VALMIN.OR.BALFAC(SC,3).GT.MAXVAL) 
     *   CONVERGE=.FALSE.
      END IF
      IF(DENOM.GT.0) THEN
C     WRITE(105,9315) (SC+MAX_IZONES),NHBTRP(SC,2),ESTATR(SC,1),
C    *                BALFAC(SC,1),
C    *               (NHBTRP(SC,3)/2.0),ESTATR(SC,2),BALFAC(SC,2),
C    *               (NHBTRP(SC,4)/2.0),ESTATR(SC,3),BALFAC(SC,3)
 9315 FORMAT(I4,3(',',F8.2,',',F8.2,',',F8.4))
      END IF
      END DO
C
C     RESET ARRAYS FOR NEXT ITERATION
C
      IF((ITER.EQ.10).OR.(CONVERGE)) GO TO 1001
      DO SC=1,MAX_STATIONS
      ESTATR(SC,1)=0.0
      ESTATR(SC,2)=0.0
      ESTATR(SC,3)=0.0
      STASUM(SC,1)=0.0
      STASUM(SC,2)=0.0
      STASUM(SC,3)=0.0
      STASUM(SC,4)=0.0
      STASUM2(SC,1)=0.0
      STASUM2(SC,2)=0.0
      STASUM2(SC,3)=0.0
      STASUM2(SC,4)=0.0
      DO SC2=1,MAX_STATIONS
      URSS1(SC,SC2)=0.0
      URSS2(SC,SC2)=0.0
      URSS3(SC,SC2)=0.0
      END DO
      END DO
      TESM=0.0
      TESM2=0.0
      TXFERS=0.0
 1000 CONTINUE
 1001 CONTINUE
      WRITE(26,9310)
 9310 FORMAT(//,30X,'R E P O R T   23',/,
     *          20X,
     *          'SUMMARIZE URBAN RAIL STATION ACCESS VOLUMES',//,
     *       1X,' STATION '/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          '  WBO   ','   WTW  ','  OTO   ','  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','---------')
      DO SC=1,MAX_STATIONS
      IF(STASUM(SC,4).GT.0) THEN
      WRITE(26,9312) (SC+MAX_IZONES),STANAME(SC),(STASUM(SC,K),K=1,4)
      END IF
 9312 FORMAT(2X,I4,3X,A29,1X,4F8.1)
      END DO
      WRITE(26,9313) TESM
 9313 FORMAT(/3X,'TOTAL',31X,4F8.1)
      WRITE(26,9314)
 9314 FORMAT(//,30X,'R E P O R T   24',/,
     *          20X,
     *          'SUMMARIZE URBAN RAIL STATION EGRESS VOLUMES',//,
     *       1X,' STATION '/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          '  WBO   ','   WTW  ','  OTO   ','  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','---------')
      DO SC=1,MAX_STATIONS
      IF(STASUM2(SC,4).GT.0) THEN
      WRITE(26,9312) (SC+MAX_IZONES),STANAME(SC),(STASUM2(SC,K),K=1,4)
      END IF
      END DO
      WRITE(26,9313) TESM2
C
C...URBAN RAIL STATION-TO-STATION TRIP MATRICES
C
      IF(TRIPSOUT) THEN
      DO II=1,MAX_ZONES
      REM1=0.0
      REM2=0.0
      REM3=0.0
      ROW=0.0
      PURP=1
      IF(II.GT.MAX_IZONES) THEN
      IF(STANUM((II-MAX_IZONES)).EQ.2) THEN
      DO 600,IJ=1,MAX_STATIONS
      IF(STANUM(IJ).NE.2) GOTO 600
      JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      IF(URSS1(IN,IJ).gt.0.0) THEN
      TEMP=(URSS1(IN,IJ)*100.0)+REM1
      ROW(JJ)=IFIX(TEMP)
      REM1=TEMP-ROW(JJ)
      ELSE
      ROW(JJ)=0
      END IF
  600 CONTINUE
      ENDIF
      ENDIF
      CALL OUTAB(23,ROW,II,PURP,DUMMY,IO)
      ROW=0.0
      PURP=2
      IF(II.GT.MAX_IZONES) THEN
      IF(STANUM((II-MAX_IZONES)).EQ.2) THEN
      DO 601,IJ=1,MAX_STATIONS
      IF(STANUM(IJ).NE.2) GOTO 601
      JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      IF(URSS2(IN,IJ).gt.0.0) THEN
      TEMP=(URSS2(IN,IJ)*100.0)+REM2
      ROW(JJ)=IFIX(TEMP)
      REM2=TEMP-ROW(JJ)
      ELSE
      ROW(JJ)=0
      END IF
  601 CONTINUE
      ENDIF
      ENDIF
      CALL OUTAB(23,ROW,II,PURP,DUMMY,IO)
      ROW=0.0
      PURP=3
      IF(II.GT.MAX_IZONES) THEN
      IF(STANUM((II-MAX_IZONES)).EQ.2) THEN
      DO 602,IJ=1,MAX_STATIONS
      IF(STANUM(IJ).NE.2) GOTO 602
      JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      IF(URSS3(IN,IJ).gt.0.0) THEN
      TEMP=(URSS3(IN,IJ)*100.0)+REM3
      ROW(JJ)=IFIX(TEMP)
      REM3=TEMP-ROW(JJ)
      ELSE
      ROW(JJ)=0
      END IF
  602 CONTINUE
      ENDIF
      ENDIF
      CALL OUTAB(23,ROW,II,PURP,DUMMY,IO)
      END DO
      END IF
      OPEN(104,FILE='NHBDIR_TLF.CSV',STATUS='UNKNOWN',
     *         FORM='FORMATTED')
      WRITE(104,9103)
 9103 FORMAT('NO_TXFERS,WBO,WTW,WBO,TOTAL')
      DO II=1,11
      WRITE(104,9102) (II-1),(TXFERS(II,K),K=1,4)
 9102 FORMAT(I2,4(',',F8.1))
      DO TX=1,4
      TRATE(TX)=TRATE(TX)+FLOAT(II)*TXFERS(II,TX)
      END DO
      END DO
      WRITE(104,9104) (TXFERS(12,K),K=1,4)
 9104 FORMAT('TOTAL',4(',',F8.1))
      DO TX=1,4
      TRATE(TX)=TRATE(TX)/TXFERS(12,TX)
      END DO
      WRITE(104,9105) TRATE
 9105 FORMAT('RATE',4(',',F6.3))
      DO II=1,50
      IF(TLF(II,1).GT.0) THEN
C     WRITE(104,9101) (II-1),II,TLF(II,1)
 9101 FORMAT(I4,',',I4,',',F8.2)
      END IF
      END DO
      RETURN
      END