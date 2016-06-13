C***********************************************************************
C                                                                      *
C     PROGRAM MSAOTL:  CAPACITY,CROWDING AND RELIABILITY               *
C                      OUTER LOOP MSA PROGRAM                          *
C                                                                      *
C            PARSONS BRINCKERHOFF QUADE & DOUGLAS                      *
C                 SAN FRANCISCO, CALIFORNIA                            *
C                                                                      *
C***********************************************************************
      INCLUDE 'CONTROL.INC'
      INCLUDE 'LUNCOM.INC'
      INCLUDE 'TPCOM.INC'
      INTEGER*2    TIME(4),DATE(3),STAT1
      INTEGER*4    TRIPS(4000),LUNIT(20)
      INTEGER*4    ITER0(4000,29),ITER1(4000,29),ITER2(4000,29)
      INTEGER*4    TESM0(29),TESM1(29),TESM2(29),DIFF,MAXDIF(29)
      INTEGER*2    IZ,JZ,T,TABLEI(50),DUMMY,IO(500),F,FL(8),PURP
      INTEGER*2    PIZ(29),PJZ(29)
      CHARACTER*80 NAMES(13),CONTROL
      REAL*4       CITER1,CITER2,XTRIP,REM
      REAL*4       RTESM0(29),RTESM1(29),RTESM2(29),RMAXDIF(29)
      REAL*4       SQER,RMSE(29),LOBS(29),NOBS(29),RMSEV(29),RMSEP(29)
      LOGICAL      CTLERR
      DATA         TABLEI/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
     *                   16,17,18,19,20,21,22,23,24,25,26,27,28,
     *                   29,30,31,32,33,34,35,36,37,38,39,40,
     *                   41,42,43,44,45,46,47,48,49,50/
      DATA         LUNIT/10*0,11,12,13,14,15,16,17,18,2*0/
      DATA         FL/3,3,19,4,3,3,19,4/
      NAMELIST  /PARAM/ CITER2
C
C     OPEN AND READ THE CONTROL FILE
C
      CALL GETARG(1,CONTROL,STAT1)
      IF(STAT1.LE.0) THEN
        WRITE(*,'('' MSAOTL (F) : CONTROL FILE NOT SPECIFIED'')')
        STOP " ENTER THE CONTROL FILE NAME ON THE COMMAND LINE."
      END IF
      OPEN(10,FILE=CONTROL,STATUS='OLD',FORM='FORMATTED',
     *     ERR=90)
      REWIND 10
      GO TO 91
   90 WRITE(*,9510)
 9510 FORMAT(/,1X,'MSAOTL 9510 (F) ERROR OPENING CONTROL CARD INPUT ',
     *       'FILE')
      STOP 8
   91 CALL RDFILES(CTLERR,NAMES)
      REWIND 10
      READ(10,PARAM,ERR=5)
      REWIND 10
      CITER1=1.0-CITER2
      WRITE(26,9001) CITER1,CITER2
 9001 FORMAT(' PARAM'/
     *       ' -----------------------------'/
     *       ' CITER1=',F6.2/
     *       ' CITER2=',F6.2/)
      GO TO 10
    5 CTLERR=.TRUE.
   10 CONTINUE
      IF(CTLERR) THEN
      WRITE(26,9511)
 9511 FORMAT(/,1X,'MSAOTL 9511 (F) ERROR OPENING INPUT FILE'/)
      STOP 8
      END IF
C
      DO T=9,12
C
C OPEN TRANPLAN MATRIX OUTPUT FILES
C
      OPEN((T+10),FILE=NAMES(T),STATUS='UNKNOWN',
     *        FORM='UNFORMATTED')
      IF(T.EQ.9.OR.T.EQ.10) THEN 
      NUMPUR=3
      TABLES=2**3-1
      END IF
      IF(T.EQ.11) THEN
      NUMPUR=19
      TABLES=2**15-1
      END IF
      IF(T.EQ.12) THEN
      NUMPUR=4
      TABLES=2**4-1
      END IF
      WRITE(T+10) HEAD1,HEAD2
      END DO
C
C ORIGIN ZONE LOOP
C
      DO 100 IZ=1,MAXZON
      IF(MOD(IZ,100).EQ.0) WRITE(*,8001) IZ
 8001 FORMAT(' Processing Origin Zone=',I4)
      IF(IZ.EQ.MAXZON) WRITE(*,8001) IZ
C
C FILE LOOP
C
      DO 400 F=1,8
      IF(F.EQ.1.OR.F.EQ.5) INDEX=0
C
C TABLE LOOP
C
      DO 200 T=1,FL(F)
      CALL INTAB(LUNIT((F+10)),TRIPS,IZ,TABLEI(T),DUMMY,IO)
      INDEX=INDEX+1
      DO 300 JZ=1,MAXZON
      IF(F.LE.4) THEN
      ITER0(JZ,INDEX)=TRIPS(JZ)
      TESM0(INDEX)=TESM0(INDEX)+TRIPS(JZ)
      END IF
      IF(F.GT.4) THEN
      ITER1(JZ,INDEX)=TRIPS(JZ)
      TESM1(INDEX)=TESM1(INDEX)+TRIPS(JZ)
      END IF
  300 CONTINUE
  200 CONTINUE
  400 CONTINUE
C
C     COMPUTE MSA VALUES
C
      DO 500 T=1,29
      REM=0.0
      DO 600 JZ=1,MAXZON
      XTRIP=FLOAT(ITER0(JZ,T))*CITER1+FLOAT(ITER1(JZ,T))*CITER2
      IF(XTRIP.GT.0) THEN
      TRIPS(JZ)=IFIX(XTRIP+REM)
      REM=XTRIP+REM-FLOAT(TRIPS(JZ))
      ELSE
      TRIPS(JZ)=0
      END IF
      TESM2(T)=TESM2(T)+TRIPS(JZ)
C
C     SAVE MAXIMUM DIFFERENCE FOR EACH MATRIX
C
      DIFF=ABS(ITER1(JZ,T)-ITER0(JZ,T))
      IF(DIFF.GT.MAXDIF(T)) THEN
      MAXDIF(T)=DIFF
      PIZ(T)=IZ
      PJZ(T)=JZ
      END IF
C
C     PERCENT ROOT MEAN SQUARE ERROR COMPUTATIONS
C
      IF(ITER0(JZ,T).GT.0.OR.ITER1(JZ,T).GT.0) THEN
      SQER=((FLOAT(ITER0(JZ,T))/100.0)-(FLOAT(ITER1(JZ,T))/100.0))**2
      RMSE(T)=RMSE(T)+SQER
      LOBS(T)=LOBS(T)+FLOAT(ITER0(JZ,T))/100.0
      NOBS(T)=NOBS(T)+1.0
      END IF
  600 CONTINUE
C
C     OUTPUT REVISED MATRICES
C
      IF(T.GT.0.AND.T.LE.3) THEN
      PURP=T
      CALL OUTAB(19,TRIPS,IZ,PURP,DUMMY,IO)
      END IF
      IF(T.GT.3.AND.T.LE.6) THEN
      PURP=T-3
      CALL OUTAB(20,TRIPS,IZ,PURP,DUMMY,IO)
      END IF
      IF(T.GT.6.AND.T.LE.25) THEN
      PURP=T-6
      CALL OUTAB(21,TRIPS,IZ,PURP,DUMMY,IO)
      END IF
      IF(T.GT.25) THEN
      PURP=T-25
      CALL OUTAB(22,TRIPS,IZ,PURP,DUMMY,IO)
      END IF
  500 CONTINUE
  100 CONTINUE    
C
C     SUMMARIZE TRIP TOTALS BY TABLE
C
      DO T=1,29
      RTESM0(T)=FLOAT(TESM0(T))/100.0
      RTESM1(T)=FLOAT(TESM1(T))/100.0
      RTESM2(T)=FLOAT(TESM2(T))/100.0
      RMAXDIF(T)=FLOAT(MAXDIF(T))/100.0
      RMSEV(T)=SQRT(RMSE(T)/(NOBS(T)-1.0))
      IF(LOBS(T).GT.0) THEN
      RMSEP(T)=((RMSEV(T)*NOBS(T))/LOBS(T))*100.0
      ELSE
      RMSEP(T)=0.0
      END IF
      END DO
      WRITE(26,9003) ((RTESM0(K),RTESM1(K),RTESM2(K)),K=1,29)
 9003 FORMAT(' OVERALL TRIP SUMMARY RESULTS'//
     *       ' ----------------------------'/
     * '    MATRIX DESCRIPTION             ',
     * 4X,' ITER 0 ',2X,' ITER 1 ',2X,' ITER 2 '/
     * ' ----------------------------------',
     * 4X,'--------',2X,'--------',2X,'--------'/
     * ' COMMUTER RAIL - ZONE    TO STATION',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' COMMUTER RAIL - STATION TO STATION',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' COMMUTER RAIL - STATION TO ZONE   ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' URBAN    RAIL - ZONE    TO STATION',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' URBAN    RAIL - STATION TO STATION',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' URBAN    RAIL - STATION TO ZONE   ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' LOCAL    BUS  - WALK              ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' LOCAL    BUS  - DRIVE             ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' EXPRESS  BUS  - WALK              ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' EXPRESS  BUS  - DRIVE             ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' COMMUTER RAIL - WALK              ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' COMMUTER RAIL - BUS               ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' COMMUTER RAIL - PARK-N-RIDE       ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' COMMUTER RAIL - KISS-N-RIDE       ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' URBAN    RAIL - WALK              ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' URBAN    RAIL - BUS               ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' URBAN    RAIL - PARK-N-RIDE       ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' URBAN    RAIL - KISS-N-RIDE       ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' TRANSITWAY    - WALK              ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' TRANSITWAY    - DRIVE             ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' RAPID    BUS  - WALK              ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' RAPID    BUS  - DRIVE             ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' BRT      BUS  - WALK              ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' BRT      BUS  - DRIVE             ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' BRT/URB  RAIL - DRIVE             ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' EXPRESS  BUS  - STATION TO ZONE   ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' TRANSITWAY    - STATION TO ZONE   ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' BRT      BUS  - STATION TO ZONE   ',4X,F8.1,2X,F8.1,2X,F8.1/
     * ' ALL           - DRIVE   TO STATION',4X,F8.1,2X,F8.1,2X,F8.1,
     * ///) 
C
C     SUMMARIZE MAXIMUM DIFFERENCE
C
      WRITE(26,9004) ((RMAXDIF(K),PIZ(K),PJZ(K),RMSEP(K)),K=1,29)
 9004 FORMAT(' MAXIMUM DIFFERENCE RESULTS  '/
     *       ' ----------------------------'//
     * '    MATRIX DESCRIPTION             ',
     * 4X,'  DIFF  ',2X,' ORIGIN ',2X,' DESTN  ',2X,' %RMSE'/
     * ' ----------------------------------',
     * 4X,'--------',2X,'--------',2X,'--------',2X,'------'/
     * ' COMMUTER RAIL - ZONE    TO STATION',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' COMMUTER RAIL - STATION TO STATION',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' COMMUTER RAIL - STATION TO ZONE   ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' URBAN    RAIL - ZONE    TO STATION',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' URBAN    RAIL - STATION TO STATION',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' URBAN    RAIL - STATION TO ZONE   ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' LOCAL    BUS  - WALK              ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' LOCAL    BUS  - DRIVE             ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' EXPRESS  BUS  - WALK              ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' EXPRESS  BUS  - DRIVE             ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' COMMUTER RAIL - WALK              ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' COMMUTER RAIL - BUS               ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' COMMUTER RAIL - PARK-N-RIDE       ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' COMMUTER RAIL - KISS-N-RIDE       ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' URBAN    RAIL - WALK              ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' URBAN    RAIL - BUS               ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' URBAN    RAIL - PARK-N-RIDE       ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' URBAN    RAIL - KISS-N-RIDE       ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' TRANSITWAY    - WALK              ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' TRANSITWAY    - DRIVE             ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' RAPID    BUS  - WALK              ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' RAPID    BUS  - DRIVE             ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' BRT      BUS  - WALK              ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' BRT      BUS  - DRIVE             ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' BRT/URB  RAIL - DRIVE             ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' EXPRESS  BUS  - STATION TO ZONE   ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' TRANSITWAY    - STATION TO ZONE   ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' BRT      BUS  - STATION TO ZONE   ',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/
     * ' ALL           - DRIVE   TO STATION',
     * 4X,F8.1,2X,I8,2X,I8,2X,F6.2/) 
      CALL GETTIM(TIME(1),TIME(2),TIME(3),TIME(4))
      WRITE(26,9002) (TIME(T),T=1,3)
      WRITE(*,9002) (TIME(T),T=1,3)
 9002 FORMAT(/'    PROGRAM MSAOTL Completed'/
     *        '     Time: ',I2,':',I2,':',I2,///)
      END
