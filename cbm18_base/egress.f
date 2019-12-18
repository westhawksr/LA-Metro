C-------------------------------------------------------------------
C        STATION --> DESTINATION ZONE UTILITY COMPUTATION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE EGRESS(STAZNE,IMODE,STAZNEI,INDAPM)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'tpcom.inc'
       include 'control.inc'
       include 'mtamcpar.inc'
C
C DATA DECLARATIONS
C
      INTEGER*2     IMODE,SC,IC,JZ,T
      INTEGER*2     T1(5),T2(5)
      INTEGER*2     STAZNEI(MAX_STATIONS,MAX_IZONES,6,4)
      INTEGER*2     LBUS,RBUS,EBUS,TBUS,BBUS,UBUS
      INTEGER*2     INDAPM(MAX_ZONES,MAX_ZONES)
      REAL*4        STAZNE(4,MAX_STATIONS,MAX_IZONES)
      REAL*4        INVEH(MAX_ZONES),FARE(MAX_ZONES),
     *              WAIT2(MAX_ZONES),INVEHL(MAX_ZONES),
     *              INVEHR(MAX_ZONES),INVEHE(MAX_ZONES),
     *              INVEHT(MAX_ZONES),
     *              TRANSF(MAX_ZONES),WAIT1(MAX_ZONES)
      REAL*4        WALKEGR(MAX_ZONES)
      REAL*4        WALKACC(MAX_ZONES),WALKTFR(MAX_ZONES)
      REAL*4        LUNRELM(MAX_ZONES,28),LUNRVAL,LUNRDEM
      REAL*4        NUNRELM(MAX_ZONES,28),NUNRVAL,TWAIT
      REAL*4        LCROWDM(MAX_ZONES,28),LCRDVAL
      REAL*4        NCAPACM(MAX_ZONES,28),NCAPVAL
      REAL*4        TINVEH
      CHARACTER*13  NAME(5)
      DATA          NAME/'Commuter Rail',
     *                   'Urban Rail   ',
     *                   'Express Bus  ',
     *                   'Transitway   ',
     *                   'BRT          '/
      DATA          T1/22,16,0,0,11/,T2/27,20,0,0,14/
      WRITE(*,8000) NAME(IMODE)
 8000 FORMAT(1X,'Egress Station --> Destination Zone',
     *            ' Utility Computations for ',a13)
C
      FILENO=66
      IF(IMODE.EQ.1) THEN
	    LUNRDEM=LSUM2CR*LSUM1TRN*LSUMT*LSUM3CW
	    ELSEIF(IMODE.EQ.2) THEN
      LUNRDEM=LSUM2UR*LSUM1TRN*LSUMT*LSUM3UW
	    ELSEIF(IMODE.EQ.5) THEN
      LUNRDEM=LSUM2BR*LSUM1TRN*LSUMT*LSUM3BW
	    ENDIF
C 
C DESTINATION STATION LOOP 
C
      DO 100 SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      IF(STANUM(SC).NE.IMODE) GO TO 100
      IF(STADATA(SC,6).LE.0.0) GOTO 100
C
C OBTAIN EGRESS PORTION OF PATH DATA
C USING TRANPLAN I/O
C
C
C...1ST WAIT TIME
      PURP=1
      call intab(fileno,VAR,ic,PURP,dummy,io)
      DO 105,II=1,MAX_ZONES
 105  WAIT1(II)=FLOAT(VAR(II))/100.0
C...TRANSFER WAIT TIME
      PURP=2
      call intab(fileno,VAR,ic,PURP,dummy,io)
      DO 106,II=1,MAX_ZONES
 106  WAIT2(II)=FLOAT(VAR(II))/100.0
C...WALK TIME ACCESS
      PURP=3
      call intab(fileno,VAR,ic,PURP,dummy,io)
      DO 107,II=1,MAX_ZONES
 107  WALKACC(II)=FLOAT(VAR(II))/100.0
C...NUMBER OF TRANSFERS
      PURP=4
      call intab(fileno,VAR,ic,PURP,dummy,io)
      DO 104,II=1,MAX_ZONES
 104  TRANSF(II)=FLOAT(VAR(II))
C...LOCAL BUS IN-VEHICLE TIME
      PURP=5
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 110 II=1,MAX_ZONES
  110 INVEHL(II)=FLOAT(VAR(II))/100.0
C...EGRESS FARE
      PURP=6
      call intab(fileno,VAR,ic,PURP,dummy,io)
      DO 103,II=1,MAX_ZONES
  103 FARE(II)=FLOAT(VAR(II))
C...RAPID BUS IN-VEHICLE TIME
      PURP=7
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 111 II=1,MAX_ZONES
  111 INVEHR(II)=FLOAT(VAR(II))/100.0
C...EXPRESS BUS IN-VEHICLE TIME
      PURP=8
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 112 II=1,MAX_ZONES
  112 INVEHE(II)=FLOAT(VAR(II))/100.0
C...TRANSITWAY IN-VEHICLE TIME
      PURP=9
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 113 II=1,MAX_ZONES
  113 INVEHT(II)=FLOAT(VAR(II))/100.0
C...TOTAL IN-VEHICLE TIME
      PURP=10
      call intab(fileno,VAR,ic,PURP,dummy,io)
      DO 101,II=1,MAX_ZONES
 101  INVEH(II)=FLOAT(VAR(II))/100.0
C....WALK TIME EGRESS
      PURP=11
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 108,II=1,MAX_ZONES
 108  WALKEGR(II)=FLOAT(VAR(II))/100.0
C...WALK TIME TRANSFER
      PURP=12
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
C   APM INDICATOR FOR COMMUTER RAIL
C
      IF(APMIND.AND.IMODE.EQ.1) THEN
      PURP=2
      CALL INTAB(99,VAR,IC,PURP,DUMMY,IO)
      DO II=1,MAX_ZONES
      IF(VAR(II).GT.0) INDAPM(IC,II)=1
      END DO
      END IF
C
C EGRESS ZONE LOOP
C
      DO 200 JZ=1,MAX_IZONES
      IF(.NOT.JOI(JZ)) GO TO 200
      STAZNE(1,SC,JZ)=0.0
      STAZNE(2,SC,JZ)=0.0   
      LUNRVAL=0.0
      NUNRVAL=0.0
      LCRDVAL=0.0
      NCAPVAL=0.0
C
C DESTINATION STATION --> EGRESS ZONE VALIDITY CHECKS
C
      IF(WALKEGR(JZ).LE.0.0) THEN
      STAZNE(1,SC,JZ)=-999.9
      GO TO 200
      END IF  
C
C  CALCULATE TOTAL LINK UNRELIABILITY
C
      IF(LUNREL) THEN
      DO T=T1(IMODE),T2(IMODE)
      LUNRVAL=LUNRVAL+LUNRELM(JZ,T)
      END DO
      END IF
C
C  CALCULATE TOTAL STOP UNRELIABILITY
C
      IF(NUNREL) THEN
      DO T=T1(IMODE),T2(IMODE)
      NUNRVAL=NUNRVAL+NUNRELM(JZ,T)
      END DO
      END IF
C
C  CALCULATE TOTAL LINK CROWDING
C
      IF(LCROWD) THEN
      DO T=T1(IMODE),T2(IMODE)
      LCRDVAL=LCRDVAL+LCROWDM(JZ,T)
      END DO
      END IF
C
C  CALCULATE TOTAL STOP CAPACITY
C
      IF(NCAPAC) THEN
      DO T=T1(IMODE),T2(IMODE)
      NCAPVAL=NCAPVAL+NCAPACM(JZ,T)
      END DO
      END IF
C
C COMPUTE IN-VEHICLE TIME COMPUTATIONS
C
      TINVEH=INVEHL(JZ)+INVEHR(JZ)+INVEHE(JZ)+INVEHT(JZ)
C
C COMPUTE EGRESS PORTION OF UTILITY
C
C....USING MODEL COEFFICIENTS
      STAZNE(1,SC,JZ)=COEFF(1)*TINVEH + COEFF(3)*WAIT1(JZ) +
     *       COEFF(4)*WAIT2(JZ) + COEFF(5)*(TRANSF(JZ)) +
     *       COEFF(7)*WALKTFR(JZ) +
     *       COEFF(75)* (LUNRVAL/LUNRDEM) +
     *       COEFF(76)* (NUNRVAL/LUNRDEM) +
     *       COEFF(77)* (LCRDVAL/LUNRDEM) +
     *       COEFF(78)* (NCAPVAL/LUNRDEM)
C.....STORE NUMBER OF TRANSFERS 
      STAZNE(2,SC,JZ)=TRANSF(JZ)
C.....STORE WALK TIME AT DESTINATION
      STAZNE(3,SC,JZ)=WALKEGR(JZ)
C.....STORE TRANSIT FARE
      STAZNE(4,SC,JZ)=FARE(JZ)
C     IF((SC+MAX_IZONES).EQ.UNIONSTA) STAZNE(4,SC,JZ)=0.0 
      IF(INVEH(JZ).LE.0) THEN
      STAIND(SC,JZ)=1
      ELSE
      STAIND(SC,JZ)=2
      END IF
C.....STORE SUBMODE USAGE
      LBUS=0
      RBUS=0
      EBUS=0
      TBUS=0
      IF(INVEHL(JZ).GT.0) LBUS=1
      IF(INVEHR(JZ).GT.0) RBUS=1
      IF(INVEHE(JZ).GT.0) EBUS=1
      IF(INVEHT(JZ).GT.0) TBUS=1
      STAZNEI(SC,JZ,IMODE,1)=LBUS
      STAZNEI(SC,JZ,IMODE,2)=RBUS
      STAZNEI(SC,JZ,IMODE,3)=EBUS
      STAZNEI(SC,JZ,IMODE,4)=TBUS
C....................................................................
      IF((DEBUG).AND.(STAZNE(1,SC,JZ).NE.0.0.OR.STAZNE(3,SC,JZ).GT.0)) 
     *       THEN
      IF(TRANSF(JZ).GT.0) TRANSF(JZ)=TRANSF(JZ)-1.0
      WRITE(33) IMODE,IC,JZ,TINVEH,WAIT1(JZ),WAIT2(JZ),
     *               TRANSF(JZ),FARE(JZ),WALKACC(JZ),
     *               WALKEGR(JZ),
     *               WALKTFR(JZ),
     *               STAZNE(1,SC,JZ),
     *               INVEHL(JZ),INVEHR(JZ),INVEHE(JZ),
     *               INVEHT(JZ),LUNRVAL,NUNRVAL,
     *               LCRDVAL,NCAPVAL
      IF(SDETAIL) THEN
      WRITE(62,9025) NAME(IMODE),IC,STANAME(SC),JZ,
     *               TINVEH,
     *               INVEHL(JZ),INVEHR(JZ),INVEHE(JZ),
     *               INVEHT(JZ),
     *               WAIT1(JZ),WAIT2(JZ),
     *               TRANSF(JZ),FARE(JZ),WALKACC(JZ),
     *               WALKEGR(JZ),
     *               WALKTFR(JZ),LUNRVAL,NUNRVAL,
     *               LCRDVAL,NCAPVAL,STAIND(SC,JZ),
     *               STAZNE(1,SC,JZ)
 9025 FORMAT(1X,'DESTINATION STATION --> EGRESS ZONE COMPUTATIONS',
     *       1X,'(',A13,')'/
     *       1X,'------------------------------------------------'//
     *       1X,'DESTINATION       STATION=',I8,1X,A37/
     *       1X,'EGRESS            ZONE   =',I8/
     *       1X,'IN-VEHICLE           TIME=',F8.2/
     *       1X,'IN-VEHICLE-LOCAL     TIME=',F8.2/
     *       1X,'IN-VEHICLE-RAPID     TIME=',F8.2/
     *       1X,'IN-VEHICLE-EXPRESS   TIME=',F8.2/
     *       1X,'IN-VEHICLE-TWY       TIME=',F8.2/
     *       1X,'1ST WAIT             TIME=',F8.2/
     *       1X,'TRANSFER WAIT        TIME=',F8.2/
     *       1X,'NUMBER OF TRANSFERS      =',F8.2/
     *       1X,'FARE                     =',F8.2/
     *       1X,'WALK ACCESS          TIME=',F8.2/
     *       1X,'WALK EGRESS          TIME=',F8.2/
     *       1X,'WALK TRANSFER        TIME=',F8.2//
     *       1X,'LINK UNRELIABILITY   TIME=',F8.2/
     *       1X,'STOP UNRELIABILITY   TIME=',F8.2/
     *       1X,'LINK CROWDING        TIME=',F8.2/
     *       1X,'CAPACITY             TIME=',F8.2//
     *       1X,'EGRESS INDICATOR         =',I8,' 1=WALK,2=BUS'/
     *       1X,'UTILITY VALUE            =',F10.5/)
      END IF
      END IF
C.......................................................................
  200 CONTINUE
  100 CONTINUE
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
      CLOSE(66,STATUS='KEEP')
      CALL PREPIO(BACCEGR,66)
      RETURN
      END
