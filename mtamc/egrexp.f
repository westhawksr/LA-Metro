C-------------------------------------------------------------------
C        STATION --> DESTINATION ZONE UTILITY COMPUTATION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE EGREXP(STAZNE,IMODE,STAZNEI)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'tpcom.inc'
       include 'control.inc'
       include 'mtamcpar.inc'
C
C DATA DECLARATIONS
C
      INTEGER*2     IMODE,SC,IC,JZ,JZ2,SC2,T
      INTEGER*2     STAZNEI(MAX_STATIONS,MAX_IZONES,6,4)
      INTEGER*2     LBUS,RBUS,EBUS,TBUS
      INTEGER*2     T1(5),T2(5)
      REAL*4        STAZNE(4,MAX_STATIONS,MAX_IZONES)
      REAL*4        INVEHL(MAX_ZONES),FARE5(MAX_ZONES),
     *              TRWAIT(MAX_ZONES),
     *              TRANSF(MAX_ZONES),WAIT1(MAX_ZONES)
      REAL*4        WALKEGR(MAX_ZONES),INVEHT(MAX_ZONES),
     *              INVEHE(MAX_ZONES),INVEHR(MAX_ZONES)
      REAL*4        WALKACC(MAX_ZONES),WALKTFR(MAX_ZONES),
     *              INVEH2(MAX_ZONES)
      REAL*4        LUNRDEM,TWAIT
      REAL*4        LUNRVAL,NUNRVAL,LCRDVAL,NCAPVAL
      INTEGER*2     LUNRELM(MAX_ZONES)
      INTEGER*2     NUNRELM(MAX_ZONES)
      INTEGER*2     LCROWDM(MAX_ZONES)
      INTEGER*2     NCAPACM(MAX_ZONES)
      REAL*4        TINVEH
      CHARACTER*13  NAME(3)
      DATA          T1/0,0,4,7,0/,T2/0,0,6,10,0/
      DATA          NAME/'Express Bus ',
     *                   'Transitway  ',
     *                   'BRT         '/
      WRITE(*,8000) NAME(IMODE-2)
 8000 FORMAT(1X,'Egress Station --> Destination Zone',
     *            ' Utility Computations for ',a13)
C
C  DESTINATION STATION LOOP
C
      IF(IMODE.EQ.3) FILENO=14
      IF(IMODE.EQ.4) FILENO=15
      INVEHT=0.0
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
C...FIRST WAIT
      PURP=1
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 108,II=1,MAX_ZONES
 108  WAIT1(II)=FLOAT(VAR(II))/100.0
C...TRANSFER WAIT
      PURP=2
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 109,II=1,MAX_ZONES
 109  TRWAIT(II)=FLOAT(VAR(II))/100.0
C... WALK TIME ACCESS
      PURP=3
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 101,II=1,MAX_ZONES
 101  WALKACC(II)=FLOAT(VAR(II))/100.0
C...TRANSFERS
      PURP=4
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 107,II=1,MAX_ZONES
 107  TRANSF(II)=FLOAT(VAR(II))
C...LOCAL BUS IVTT
      PURP=5
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 105,II=1,MAX_ZONES
 105  INVEHL(II)=FLOAT(VAR(II))/100.0
C...TOTAL FARE
      PURP=6
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 106,II=1,MAX_ZONES
 106  FARE5(II)=FLOAT(VAR(II))
C...RAPID BUS IVTT
      PURP=7
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 112,II=1,MAX_ZONES
  112 INVEHR(II)=FLOAT(VAR(II))/100.0
C...EXPRESS BUS IVTT
      PURP=8
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 103,II=1,MAX_ZONES
 103  INVEHE(II)=FLOAT(VAR(II))/100.0
C...TRANSITWAY
      IF(IMODE.EQ.3) GOTO 213
      PURP=9
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 104,II=1,MAX_ZONES
 104  INVEHT(II)=FLOAT(VAR(II))/100.0
C...TOTAL IN-VEHICLE TIME
 213  IF(IMODE.EQ.3) PURP=9
      IF(IMODE.EQ.4) PURP=10
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 102,II=1,MAX_ZONES
 102  INVEH2(II)=FLOAT(VAR(II))/100.0
C...WALK EGRESS TIME
      IF(IMODE.EQ.3) PURP=10
      IF(IMODE.EQ.4) PURP=11
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 110,II=1,MAX_ZONES
 110  WALKEGR(II)=FLOAT(VAR(II))/100.0
C... WALK TRANSFER TIME
      IF(IMODE.EQ.3) PURP=11
      IF(IMODE.EQ.4) PURP=12
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 111,II=1,MAX_ZONES
 111  WALKTFR(II)=FLOAT(VAR(II))/100.0
C
C   LINK UNRELIABILITY
C
      IF(LUNREL) THEN
      T=5
      CALL INTAB(90,VAR,IC,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      LUNRELM(II)=IJINT(VAR(II))
      END DO
      END IF
C
C   STOP UNRELIABILITY
C
      IF(NUNREL) THEN
      T=5
      CALL INTAB(91,VAR,IC,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      NUNRELM(II)=IJINT(VAR(II))
      END DO
      END IF
C
C   CROWDING
C
      IF(LCROWD) THEN
      T=5
      CALL INTAB(92,VAR,IC,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      LCROWDM(II)=IJINT(VAR(II))
      END DO
      END IF
C
C   STOP CAPACITY
C
      IF(NCAPAC) THEN
      T=5
      CALL INTAB(93,VAR,IC,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      NCAPACM(II)=IJINT(VAR(II))
      END DO
      END IF
C
C EGRESS ZONE LOOP
C
      DO 200 JZ=1,MAX_IZONES
      IF(.NOT.JOI(JZ)) GO TO 200
      STAZNE(1,SC,JZ)=0.0
      STAZNE(2,SC,JZ)=0.0
      LUNRVAL=FLOATI(LUNRELM(JZ))/100.0
      NUNRVAL=FLOATI(NUNRELM(JZ))/100.0
      LCRDVAL=FLOATI(LCROWDM(JZ))/100.0
      NCAPVAL=FLOATI(NCAPACM(JZ))/100.0
C
C DESTINATION STATION --> EGRESS ZONE VALIDITY CHECKS
C
      IF(IMODE.EQ.3.AND.INVEHE(JZ).LE.0) GO TO 200
      IF(IMODE.EQ.3.AND.(NOTXFR).AND.
     *  (INVEH2(JZ).GT.INVEHE(JZ))) GO TO 200
      IF(IMODE.EQ.4.AND.INVEHT(JZ).LE.0) GO TO 200
      IF(IMODE.EQ.4.AND.(NOTXFR).AND.
     *  (INVEH2(JZ).GT.INVEHT(JZ))) GO TO 200
      IF(TRANSF(JZ).GE.2) GO TO 200
C     IF(IMODE.EQ.4.AND.TRANSF(JZ).GT.0) GO TO 200
      IF(WALKEGR(JZ).LE.0.0) THEN
      STAZNE(1,SC,JZ)=-999.9
      GO TO 200
      END IF
C
C COMPUTE IN-VEHICLE TIME COMPUTATIONS
C
      TINVEH=INVEHL(JZ)+INVEHR(JZ)+INVEHE(JZ)+INVEHT(JZ)
C
C COMPUTE EGRESS PORTION OF UTILITY
C
C....USING MODEL COEFFICIENTS
      IF(IMODE.EQ.3) THEN
      STAZNE(1,SC,JZ)=COEFF(11)*TINVEH + COEFF(13)*WAIT1(JZ) +
     *       COEFF(15)*TRWAIT(JZ) + COEFF(39)*(TRANSF(JZ)) +
     *       COEFF(17)*WALKTFR(JZ) +
     *       COEFF(75)* LUNRVAL +
     *       COEFF(76)* NUNRVAL +
     *       COEFF(77)* LCRDVAL +
     *       COEFF(78)* NCAPVAL -
     *        (1.0-(INVEHE(JZ)/TINVEH))*6.0
      END IF 
	    IF(IMODE.EQ.4) THEN
	    STAZNE(1,SC,JZ)=COEFF(11)*TINVEH + COEFF(13)*WAIT1(JZ) +
     *       COEFF(15)*TRWAIT(JZ) + COEFF(42)*(TRANSF(JZ)) +
     *       COEFF(17)*WALKTFR(JZ) +
     *       COEFF(75)*LUNRVAL +
     *       COEFF(76)*NUNRVAL +
     *       COEFF(77)* LCRDVAL +
     *       COEFF(78)* NCAPVAL -
     *        (1.0-(INVEHT(JZ)/TINVEH))*6.0
	    ENDIF
C
C.....STORE WALK TIME AT DESTINATION
      STAZNE(3,SC,JZ)=WALKEGR(JZ)
C.....STORE TRANSIT FARE
      STAZNE(4,SC,JZ)=FARE5(JZ)
C.....STORE NUMBER OF TRANSFERS
      STAZNE(2,SC,JZ)=TRANSF(JZ)
C.....STORE SUBMODE USAGE
      LBUS=0
      RBUS=0
      EBUS=0
      TBUS=0
      IF(IMODE.EQ.3) THEN
      IF(INVEHL(JZ).GT.0) LBUS=1
      IF(INVEHR(JZ).GT.0) RBUS=1
      STAZNEI(SC,JZ,IMODE,1)=LBUS
      STAZNEI(SC,JZ,IMODE,2)=RBUS
      END IF
      IF(IMODE.EQ.4) THEN
      IF(INVEHL(JZ).GT.0) LBUS=1
      IF(INVEHR(JZ).GT.0) RBUS=1
      IF(INVEHE(JZ).GT.0) EBUS=1
      STAZNEI(SC,JZ,IMODE,1)=LBUS
      STAZNEI(SC,JZ,IMODE,2)=RBUS
      STAZNEI(SC,JZ,IMODE,3)=EBUS
      END IF
C....................................................................
      IF((DEBUG).AND.(STAZNE(1,SC,JZ).NE.0.0)) THEN
      WRITE(34) IMODE,IC,JZ,TINVEH,
     *               INVEHL(JZ),INVEHE(JZ),INVEHT(JZ),
     *               INVEHR(JZ),WAIT1(JZ),TRWAIT(JZ),
     *               TRANSF(JZ),FARE5(JZ),WALKEGR(JZ),
     *               WALKTFR(JZ),WALKACC(JZ),
     *               STAZNE(1,SC,JZ),LUNRVAL,
     *               NUNRVAL,LCRDVAL,NCAPVAL
      IF(SDETAIL) THEN
      WRITE(63,9025) NAME(IMODE-2),IC,STANAME(SC),JZ,TINVEH,
     *               INVEHL(JZ),INVEHE(JZ),INVEHT(JZ),
     *               INVEHR(JZ),WAIT1(JZ),TRWAIT(JZ),
     *               TRANSF(JZ),FARE5(JZ),WALKEGR(JZ),
     *               WALKTFR(JZ),WALKACC(JZ),LUNRVAL,
     *               NUNRVAL,LCRDVAL,NCAPVAL,
     *               STAZNE(1,SC,JZ)
 9025 FORMAT(1X,'ACCESS STATION --> EGRESS ZONE COMPUTATIONS',
     *       1X,'(',A13,')'/
     *       1X,'------------------------------------------------'//
     *       1X,'DESTINATION       STATION=',I8,1X,A37/
     *       1X,'EGRESS            ZONE   =',I8/
     *       1X,'IN-VEHICLE           TIME=',F8.2/
     *       1X,'LOCAL IN-VEHICLE     TIME=',F8.2/
     *       1X,'EXPRESS IN-VEHICLE   TIME=',F8.2/
     *       1X,'TRANSITWAY INVEH     TIME=',F8.2/
     *       1X,'RAPID IN-VEHICLE     TIME=',F8.2/
     *       1X,'1ST WAIT             TIME=',F8.2/
     *       1X,'TRANSFER WAIT        TIME=',F8.2/
     *       1X,'NUMBER OF TRANSFERS      =',F8.2/
     *       1X,'FARE                     =',F8.2/
     *       1X,'WALK EGRESS          TIME=',F8.2/
     *       1X,'WALK TRANSFER        TIME=',F8.2/
     *       1X,'WALK ACCESS          TIME=',F8.2//
     *       1X,'LINK UNRELIABILITY   TIME=',F8.2/
     *       1X,'STOP UNRELIABILITY   TIME=',F8.2/
     *       1X,'LINK CROWDING        TIME=',F8.2/
     *       1X,'STOP CAPACITY        TIME=',F8.2//
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
      RETURN
      END
