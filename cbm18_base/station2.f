C-------------------------------------------------------------------
C        STATION --> STATION UTILITY COMPUTATION SUBROUTINE
C        URBAN RAIL - BRT - COMMUTER RAIL COMBINATION
C-------------------------------------------------------------------
       SUBROUTINE STATION2(STASTA,STASTA2)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'tpcom.inc'
       include 'control.inc'
	     include 'mtamcpar.inc'
C
C DATA DECLARATIONS
C
      integer*2     IMODE,ic,sc,sc2,dc,T,XC,MODE1,MODE2
      INTEGER*2     INDAPM(MAX_ZONES,MAX_ZONES)
      INTEGER*4     FILENO2,XFER(MAX_ZONES,10)
      REAL*4        STASTA(5,MAX_STATIONS,MAX_STATIONS)
      REAL*4        STASTA2(3,MAX_STATIONS,MAX_STATIONS)
      REAL*4        INVEHB(MAX_ZONES),FARE(MAX_ZONES),
     *              TRANSF(MAX_ZONES),WAIT1(MAX_ZONES),
     *              WAIT2(MAX_ZONES),WALKTFR(MAX_ZONES),
     *              INVEHR(MAX_ZONES),INVEHC(MAX_ZONES)
      REAL*4        STAUTL,KLINE,WAIT1A,WAIT1B,INVEH
      REAL*4        LUNRELM(MAX_ZONES,28),LUNRVAL
      REAL*4        NUNRELM(MAX_ZONES,28),NUNRVAL
      REAL*4        LCROWDM(MAX_ZONES,28),LCRDVAL  
      REAL*4        NCAPACM(MAX_ZONES,28),NCAPVAL
      CHARACTER*13  NAME(5)
      CHARACTER*37  TMPNAME(10),BLANK
      DATA          BLANK/'   '/
C
      DATA          NAME/'Commuter Rail',
     *                   'Urban Rail   ',
     *                   'Express Bus  ',
     *                   'Transitway   ',
     *                   'BRT          '/
C
      WRITE(*,8000)
 8000 FORMAT(1X,'Station --> Station',
     * ' Utility Computations for Urban Rail, BRT & Commuter Rail')
      IMODE=6
C
C    READ IN APPROPRIATE ZONE RANGES PER MODE
C
      CALL PREPIO(BRTURTCR,72)
      FILENO=72
      CALL PREPIO(XFERBLEND,9)
      FILENO2=9
C
C
C  ORIGIN STATION LOOP 
C
      DO 50 SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      IF(STANUM(SC).EQ.3.OR.STANUM(SC).EQ.4) 
     *    GO TO 50
      IF(STADATA(SC,6).NE.1.0) GOTO 50
C
C
C    USE TRANPLAN I/O TO
C    OBTAIN LEVEL-OF-SERVICE VALUES
C
C.....1ST WAIT TIME
C     WAIT1
      PURP=1
      call intab(FILENO,VAR,ic,PURP,dummy,io)
	    DO 104,II=1,MAX_ZONES
  104 WAIT1(II)=FLOAT(VAR(II))/100.0
C.....TRANSFER WAIT TIME
C     WAIT2
      PURP=2
      call intab(fileno,VAR,ic,PURP,dummy,io)
      DO 105,II=1,MAX_ZONES
 105  WAIT2(II)=FLOAT(VAR(II))/100.0
C.....NUMBER OF TRANSFERS
C     TRANSF
      PURP=3
      call intab(fileno,VAR,ic,PURP,dummy,io)
      DO 103,II=1,MAX_ZONES
  103 TRANSF(II)=FLOAT(VAR(II))
C.....FARE
C     FARE
      PURP=4
      call intab(fileno,VAR,ic,PURP,dummy,io)
      DO 102,II=1,MAX_ZONES
 102  FARE(II)=FLOAT(VAR(II))
C.....BRT IN-VEHICLE TIME
      PURP=5
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 101,II=1,MAX_ZONES
 101  INVEHB(II)=FLOAT(VAR(II))/100.0
C.....URBAN RAIL IN-VEHICLE TIME
      PURP=6
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 106,II=1,MAX_ZONES
 106  INVEHR(II)=FLOAT(VAR(II))/100.0
C.....COMMUTER RAIL IN-VEHICLE TIME
      PURP=7
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 107,II=1,MAX_ZONES
 107  INVEHC(II)=FLOAT(VAR(II))/100.0
C...TRANSFER WALK TIME
      PURP=8
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 109,II=1,MAX_ZONES
 109  WALKTFR(II)=FLOAT(VAR(II))/100.0
C
C   LINK UNRELIABILITY
C
      IF(LUNREL) THEN
      DO T=1,28
      CALL INTAB(90,VAR,IC,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      LUNRELM(II,T)=FLOAT(VAR(II))/100.0
      END DO
      END DO
      END IF
C
C   STOP UNRELIABILITY
C
      IF(NUNREL) THEN
      DO T=1,28
      CALL INTAB(91,VAR,IC,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      NUNRELM(II,T)=FLOAT(VAR(II))/100.0
      END DO
      END DO
      END IF
C
C   CROWDING
C
      IF(LCROWD) THEN
      DO T=1,28
      CALL INTAB(92,VAR,IC,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      LCROWDM(II,T)=FLOAT(VAR(II))/100.0
      END DO
      END DO
      END IF
C
C   STOP CAPACITY
C
      IF(NCAPAC) THEN
      DO T=1,28
      CALL INTAB(93,VAR,IC,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      NCAPACM(II,T)=FLOAT(VAR(II))/100.0
      END DO
      END DO
      END IF
C
C   APM INDICATOR FOR URBAN RAIL
C
C     IF(APMIND.AND.IMODE.EQ.2) THEN
C     PURP=1
C     CALL INTAB(99,VAR,IC,PURP,DUMMY,IO)
C     DO II=1,MAX_ZONES
C     IF(VAR(II).GT.0) INDAPM(IC,II)=1
C     END DO
C     END IF
C
C   TRANSFER NODES 
C
      DO T=1,10
      PURP=T
      CALL INTAB(9,VAR,IC,PURP,DUMMY,IO)
      DO II=1,MAX_ZONES
      XFER(II,T)=VAR(II)
      END DO
      END DO   
C
C   DESTINATION STATION LOOP
C
      DO 60 SC2=1,MAX_STATIONS
      DC=SC2+MAX_IZONES
      IF(IC.EQ.DC) GOTO 60
      IF(STANUM(SC).EQ.STANUM(SC2)) GO TO 60
      IF(STADATA(SC2,6).NE.1.0) GOTO 60
      IF(STANUM(SC2).EQ.3.OR.STANUM(SC2).EQ.4) GO TO 60
      INVEH=INVEHB(DC)+INVEHR(DC)+INVEHC(DC)
      IF((INVEH.LE.0.0.OR.INVEH.GE.99999.9)) THEN
      IF(SDETAIL) WRITE(75,9001) IC,STANAME(SC),DC,STANAME(SC2)
 9001 FORMAT(' STATION2 9001 (W) NO PATH FOUND FOR STATION=',I4,1X,A29,
     *       ' TO STATION=',I4,1X,A29)
      STAUTL=0.0
      GO TO 60
      END IF
      LUNRVAL=0.0
      NUNRVAL=0.0
      LCRDVAL=0.0
      NCAPVAL=0.0
      MODE1=STANUM(SC)
      MODE2=STANUM(SC2)
      IF(MODE1.LE.0.OR.MODE1.GT.5) THEN
      WRITE(26,9002) IC,MODE1
 9002 FORMAT(' STATION2 9002 (F) INVALID INTERNAL MODE CODE FOR ',
     *       'STATION=',I4,' MODE=',I1)
      STOP 9002
      END IF
      IF(MODE2.LE.0.OR.MODE2.GT.5) THEN
      WRITE(26,9002) DC,MODE2
      STOP 9002
      END IF
      IF(INVEHB(DC).GT.0) THEN
      LUNRVAL=LUNRVAL+LUNRELM(DC,15)
      NUNRVAL=NUNRVAL+NUNRELM(DC,15)
      LCRDVAL=LCRDVAL+LCROWDM(DC,15)
      NCAPVAL=NCAPVAL+NCAPACM(DC,15)
      END IF
      IF(INVEHR(DC).GT.0) THEN
      LUNRVAL=LUNRVAL+LUNRELM(DC,21)
      NUNRVAL=NUNRVAL+NUNRELM(DC,21)
      LCRDVAL=LCRDVAL+LCROWDM(DC,21)
      NCAPVAL=NCAPVAL+NCAPACM(DC,21)
      END IF
      IF(INVEHC(DC).GT.0) THEN
      LUNRVAL=LUNRVAL+LUNRELM(DC,28)
      NUNRVAL=NUNRVAL+NUNRELM(DC,28)
      LCRDVAL=LCRDVAL+LCROWDM(DC,28)
      NCAPVAL=NCAPVAL+NCAPACM(DC,28)
      END IF
      WAIT1A=AMIN1(WAIT1(DC),WAITLT)
      WAIT1B=DIM(WAIT1(DC),WAITLT)
      STAUTL=COEFF(1)*INVEH + COEFF(3)*WAIT1A +
     *       COEFF(4)*WAIT2(DC) + COEFF(5)*TRANSF(DC)+
     *       COEFF(7)*WALKTFR(DC)+ COEFF(1)*WAIT1B +
     *       COEFF(75)*LUNRVAL/(LSUM2UR*LSUM1TRN*LSUMT*LSUM3UW) +
     *       COEFF(76)*NUNRVAL/(LSUM2UR*LSUM1TRN*LSUMT*LSUM3UW) +
     *       COEFF(77)*LCRDVAL/(LSUM2UR*LSUM1TRN*LSUMT*LSUM3UW) +
     *       COEFF(78)*NCAPVAL/(LSUM2UR*LSUM1TRN*LSUMT*LSUM3UW) +
     *       KSTAMODE(MODE1,MODE2)/(LSUM1TRN*LSUM2UR*LSUM3UW*LSUMT)
C
C     STORE STATION-TO-STATION VALUES
C
      STASTA(1,SC,SC2)=STAUTL
      STASTA(2,SC,SC2)=TRANSF(DC)
      STASTA(3,SC,SC2)=INVEH
      STASTA(4,SC,SC2)=FARE(DC)
      STASTA(5,SC,SC2)=WAIT1(DC)+WAIT2(DC)
      STASTA2(1,SC,SC2)=INVEHB(DC)
      STASTA2(2,SC,SC2)=INVEHR(DC)
      STASTA2(3,SC,SC2)=INVEHC(DC)
C
C     OBTAIN PSUEDO STATION NUMBERS
C
      CALL XBLEND(DC,XFER)
      DO T=1,10
      XFERSTA(SC,SC2,T)=XFER(DC,T)
      END DO
C....................................................................
      IF((DEBUG).AND.(STAUTL.NE.0.0)) THEN
      WRITE(31) IMODE,IC,DC,WAIT1(DC),WAIT2(DC),
     *                TRANSF(DC),FARE(DC),INVEH,
     *                WALKTFR(DC),
     *                STAUTL,LUNRVAL,NUNRVAL,LCRDVAL,NCAPVAL
      IF(SDETAIL) THEN
      WRITE(104,9029)  IC,STANAME(SC),NAME(STANUM(SC)),
     *                DC,STANAME(SC2),NAME(STANUM(SC2)),
     *                WAIT1(DC),WAIT2(DC),
     *                TRANSF(DC),FARE(DC),INVEH,
     *                INVEHB(DC),INVEHR(DC),INVEHC(DC),
     *                WALKTFR(DC),LUNRVAL,
     *                NUNRVAL,LCRDVAL,NCAPVAL,
     *                KSTAMODE(MODE1,MODE2),STAUTL
 9029 FORMAT(/1X,'STATION --> STATION UTILITY COMPUTATIONS'/
     *       1X,'----------------------------------------'//
     *       1X,'ORIGIN            STATION=',I8,1X,A37,1X,A14/
     *       1X,'DESTINATION       STATION=',I8,1X,A37,1X,A14/
     *       1X,'1ST WAIT             TIME=',F8.2/
     *       1X,'TRANSFER WAIT        TIME=',F8.2/
     *       1X,'NUMBER OF TRANSFERS      =',F8.2/
     *       1X,'FARE                     =',F8.2/
     *       1X,'PRIMARY MODE  IN-VEHICLE =',F8.2/
     *       1X,'BRT           IN-VEHICLE =',F8.2/
     *       1X,'URBAN RAIL    IN-VEHICLE =',F8.2/
     *       1X,'COMMUTER RAIL IN-VEHICLE =',F8.2/
     *       1X,'TRANSFER WALK TIME       =',F8.2//
     *       1X,'LINK UNRELIABILITY TIME  =',F8.2/
     *       1X,'STOP UNRELIABILITY TIME  =',F8.2/
     *       1X,'LINK CROWDING      TIME  =',F8.2/
     *       1X,'STOP CAPACITY      TIME  =',F8.2/
     *       1X,'MODE-TO-MODE CONSTANT    =',F8.4/
     *       1X,'UTILITY VALUE            =',F10.5/)
      TMPNAME=BLANK
      DO T=1,10
      IF(XFER(DC,T).GT.0) THEN
      XC=XFER(DC,T)-MAX_IZONES
      IF(XC.LE.MAX_STATIONS) TMPNAME(T)=STANAME(XC)
      END IF      
      END DO
      WRITE(104,9030) (XFER(DC,K),K=1,2),(TMPNAME(K1),K1=1,2),
     *                (XFER(DC,K),K=3,4),(TMPNAME(K1),K1=3,4),
     *                (XFER(DC,K),K=5,6),(TMPNAME(K1),K1=5,6),
     *                (XFER(DC,K),K=7,8),(TMPNAME(K1),K1=7,8),
     *                (XFER(DC,K),K=9,10),(TMPNAME(K1),K1=9,10)
 9030 FORMAT(1X,' STATION --> STATION TRANSFER NODES'/
     *       1X,' ----------------------------------'/
     *       1X,' 1ST TRANSFER ',I5,' - ',I5,1X,A37,1X,A37/
     *       1X,' 2ND TRANSFER ',I5,' - ',I5,1X,A37,1X,A37/
     *       1X,' 3RD TRANSFER ',I5,' - ',I5,1X,A37,1X,A37/
     *       1X,' 4TH TRANSFER ',I5,' - ',I5,1X,A37,1X,A37/
     *       1X,' 5TH TRANSFER ',I5,' - ',I5,1X,A37,1X,A37)
      END IF
      END IF
C.......................................................................
   60 CONTINUE
   50 CONTINUE
      IF(LUNREL) THEN
      CLOSE(90,STATUS='KEEP')
      CALL PREPIO(FLUNREL,90)
      END IF
      IF(NUNREL) THEN
      CLOSE(91,STATUS='KEEP')
      CALL PREPIO(FNUNREL,91)
      END IF
      IF(LCROWD) THEN
      CLOSE(92,STATUS='KEEP')
      CALL PREPIO(FCROWD,92)
      END IF
      IF(NCAPAC) THEN
      CLOSE(93,STATUS='KEEP')
      CALL PREPIO(FCAPAC,93)
      END IF
      IF(APMIND) THEN
      CLOSE(99,STATUS='KEEP')
      CALL PREPIO(FAPMIND,99)
      END IF
      RETURN
      END
C-------------------------------------------------------------------
C     OBTAIN PSUEDO ZONE FOR TRANSIT TRANSFER NODE
C-------------------------------------------------------------------
      SUBROUTINE XBLEND(DC,XFER)
      INCLUDE 'stadat.com'
      INCLUDE 'param.com'
      include 'tpcom.inc'
      include 'control.inc'
	    include 'mtamcpar.inc'
C
C  DATA DECLARATIONS
C
	    INTEGER*2  DC,T,SC,STA
      INTEGER*4  XFER(MAX_ZONES,10)
C
      DO T=1,10
      SC=XFER(DC,T)
      STA=0
      IF(SC.GT.0) THEN
      DO K=1,MAX_STATIONS
      IF(STANODE1(K).EQ.SC) THEN
      L=K+MAX_IZONES
      STA=L
      GO TO 1000
      END IF
      IF(STANODE2(K).EQ.SC) THEN
      L=K+MAX_IZONES
      STA=L
      GO TO 1000
      END IF 
      END DO
      WRITE(75,1001) SC
 1001 FORMAT(' PSEUDO STATION NODE NOT FOUND FOR NODE=',I5,
     *       ' IN BLENDED STATION XFER MATRIX')
      END IF
 1000 IF(STA.GT.0) XFER(DC,T)=STA
      END DO
      RETURN
      END
