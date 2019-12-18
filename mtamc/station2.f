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
      REAL*4        LUNRVAL,NUNRVAL,LCRDVAL,NCAPVAL
      INTEGER*2     LUNRELM(MAX_ZONES)
      INTEGER*2     NUNRELM(MAX_ZONES)
      INTEGER*2     LCROWDM(MAX_ZONES)  
      INTEGER*2     NCAPACM(MAX_ZONES)
      REAL*4        FARECOST
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
      T=7
      CALL INTAB(90,VAR,IC,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      LUNRELM(II)=IJINT(VAR(II))
      END DO
      END IF
C
C   STOP UNRELIABILITY
C
      IF(NUNREL) THEN
      T=7
      CALL INTAB(91,VAR,IC,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      NUNRELM(II)=IJINT(VAR(II))
      END DO
      END IF
C
C   CROWDING
C
      IF(LCROWD) THEN
      T=7
      CALL INTAB(92,VAR,IC,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      LCROWDM(II)=IJINT(VAR(II))
      END DO
      END IF
C
C   STOP CAPACITY
C
      IF(NCAPAC) THEN
      T=7
      CALL INTAB(93,VAR,IC,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      NCAPACM(II)=IJINT(VAR(II))
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
      IF(SDETAIL) WRITE(135,9001) IC,STANAME(SC),DC,STANAME(SC2)
 9001 FORMAT(' STATION2 9001 (W) NO PATH FOUND FOR STATION=',I4,1X,A29,
     *       ' TO STATION=',I4,1X,A29,' IN BLENDED STS PATH')
      STAUTL=0.0
      GO TO 60
      END IF
C...CCR VALUES
      LUNRVAL=FLOATI(LUNRELM(DC))/100.0
      NUNRVAL=FLOATI(NUNRELM(DC))/100.0
      LCRDVAL=FLOATI(LCROWDM(DC))/100.0
      NCAPVAL=FLOATI(NCAPACM(DC))/100.0
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
C
C     OBTAIN PSUEDO STATION NUMBERS
C
      CALL XBLEND(DC,XFER)
      DO T=1,10
      XFERSTA(SC,SC2,T)=XFER(DC,T)
      END DO
C
C     FARE COMPUTATIONS
C
      CALL CRFARE(SC,SC2,XFER)
      FARECOST=FARE(DC)+STSFARE(SC,SC2)
C
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
      STASTA(4,SC,SC2)=FARECOST
      STASTA(5,SC,SC2)=WAIT1(DC)+WAIT2(DC)
      STASTA2(1,SC,SC2)=INVEHB(DC)
      STASTA2(2,SC,SC2)=INVEHR(DC)
      STASTA2(3,SC,SC2)=INVEHC(DC)
C....................................................................
      IF((DEBUG).AND.(STAUTL.NE.0.0)) THEN
      WRITE(31) IMODE,IC,DC,WAIT1(DC),WAIT2(DC),
     *                TRANSF(DC),FARECOST,INVEH,
     *                WALKTFR(DC),
     *                STAUTL,LUNRVAL,NUNRVAL,LCRDVAL,NCAPVAL
      IF(SDETAIL) THEN
      WRITE(104,9029)  IC,STANAME(SC),NAME(STANUM(SC)),
     *                DC,STANAME(SC2),NAME(STANUM(SC2)),
     *                WAIT1(DC),WAIT2(DC),
     *                TRANSF(DC),FARE(DC),STSFARE(SC,SC2),INVEH,
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
     *       1X,'INPUT MATRIX FARE        =',F8.2/
     *       1X,'STATION-TO-STATION FARE  =',F8.2/
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
C-------------------------------------------------------------------
C     COMMUTER RAIL STATION-TO-STATION FARE
C-------------------------------------------------------------------
      SUBROUTINE CRFARE(SC,SC2,XFER)
      INCLUDE 'stadat.com'
      INCLUDE 'param.com'
      include 'tpcom.inc'
      include 'control.inc'
	    include 'mtamcpar.inc'
C
C  DATA DECLARATIONS
C
	    INTEGER*2  SC,T,SC2,MAXT,T1,T2,IC,DC
	    INTEGER*2  MODEINC(10),STA1,STA2,ENDSTA
      INTEGER*4  XFER(MAX_ZONES,10)
C 
      IC=SC+MAX_IZONES
      DC=SC2+MAX_IZONES
      IF(SC.LE.0.OR.SC2.LE.0) RETURN
      MAXT=0
      MODEINC=0
      DO T=1,5
      T1=(T-1)*2+1
      T2=(T-1)*2+2
      IF(XFER(DC,T1).GT.0.AND.XFER(DC,T2).GT.0) MAXT=T
      END DO
      IF(MAXT.EQ.0) THEN
      WRITE(135,9001) IC,DC
 9001 FORMAT(' CRFARE (F) 9001 NO TRANSFERS FOR STATION=',I4,
     * ' TO STATION=',I4)
      RETURN
      END IF
      DO T=1,MAXT
      T1=(T-1)*2+1
      T2=(T-1)*2+2
      MODEINC(T1)=STANUM(XFER(DC,T1)-MAX_IZONES)
      MODEINC(T2)=STANUM(XFER(DC,T2)-MAX_IZONES)
      END DO
C...................................................................
      IF(SDETAIL) THEN
      WRITE(104,9002) IC,STANAME(SC),
     *                DC,STANAME(SC2),
     * STANUM(SC),MODEINC,STANUM(SC2)
 9002 FORMAT(//,80('=')/
     *       ' COMMUTER RAIL FARE COMPUTATION'/
     *       '      STATION MODE VALUES'/
     *       ' ------------------------------'/
     *       ' ORIGIN            STATION=',I8,1X,A37/
     *       ' DESTINATION       STATION=',I8,1X,A37/
     *       ' ORIGIN STATION      ',I2/
     *       ' 1ST TRANSFER POINT  ',2I2/
     *       ' 2ND TRASNFER POINT  ',2I2/
     *       ' 3RD TRANSFER POINT  ',2I2/
     *       ' 4TH TRASNFER POINT  ',2I2/
     *       ' 5TH TRASNFER POINT  ',2I2/
     *       ' DESTINATION STATION ',I2/)
      END IF
C......................................................................
C..ORIGIN STATION IS COMMUTER RAIL
      ENDSTA=0
      IF(STANUM(SC).EQ.1) THEN
      STA1=SC
      DO T=1,MAXT
      T1=(T-1)*2+1
      T2=(T-1)*2+2      
      IF(MODEINC(T1).EQ.1) ENDSTA=T1
      IF(MODEINC(T2).EQ.1) ENDSTA=T2
      END DO
      STA2=XFER(DC,ENDSTA)-MAX_IZONES
      END IF
C..DESTINATION STATION IS COMMUTER RAIL
      ENDSTA=0
      IF(STANUM(SC2).EQ.1) THEN
      STA2=SC2
      DO T=1,MAXT
      T1=(T-1)*2+1
      T2=(T-1)*2+2      
      IF(MODEINC(T1).EQ.1) ENDSTA=T1
      IF(MODEINC(T2).EQ.1) ENDSTA=T2
      END DO
      STA1=XFER(DC,ENDSTA)-MAX_IZONES
      END IF
C...IN LINE COMMUTER RAIL STATION 
      IF(STANUM(SC).NE.1.AND.STANUM(SC2).NE.1) THEN
      ENDSTA=0
      DO T=1,MAXT
      T1=(T-1)*2+1
      T2=(T-1)*2+2      
      IF(MODEINC(T1).EQ.1) ENDSTA=T1
      IF(MODEINC(T2).EQ.1) ENDSTA=T2
      END DO
      IF(ENDSTA.GT.0) THEN
      WRITE(26,9004) IC,DC,XFER(DC,ENDSTA)
      WRITE(*,9004) IC,DC,XFER(DC,ENDSTA)
 9004 FORMAT(/' CRFARE 9004 (W) ORIGIN STATION=',I4,
     *        ' DESTINATION STATION=',I4,
     *       ' INTERNAL TRANSFER STATION=',I4/)
      STOP 9004
      END IF
      END IF
      IF(STA1.GT.0.AND.STA2.GT.0) THEN
      STSFARE(SC,SC2)=STSFARE(STA1,STA2)
      END IF
C............................................................
      IF(SDETAIL.AND.STA1.GT.0.AND.STA2.GT.0) THEN
      WRITE(104,9003) (STA1+MAX_IZONES),STANAME(STA1),
     *                (STA2+MAX_IZONES),STANAME(STA2),
     *                 STSFARE(STA1,STA2)
 9003 FORMAT(' ORIGIN      STATION     =',I8,1X,A37/
     *       ' DESTINATION STATION     =',I8,1X,A37/
     *       ' STATION-TO-STATION FARE =',F8.2/)
      END IF
C..............................................................
      RETURN
      END
