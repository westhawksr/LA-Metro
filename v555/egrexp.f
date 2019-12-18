C-------------------------------------------------------------------
C        STATION --> DESTINATION ZONE UTILITY COMPUTATION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE EGREXP(STAZNE,IMODE,EXPZNE,STAZNEI,BRTZNE,
     *                   STAZNED,STASTAD)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'tpcom.inc'
       include 'control.inc'
       include 'mtamcpar.inc'
C
C DATA DECLARATIONS
C
      INTEGER*2     IMODE,SC,IC,JZ,JZ2,SC2
      INTEGER*2     STAZNEI(MAX_STATIONS,MAX_IZONES,6,6)
      INTEGER*2     LBUS,RBUS,EBUS,TBUS
      REAL*4        STAZNE(4,MAX_STATIONS,MAX_IZONES)
      REAL*4        EXPZNE(4,MAX_STATIONS,MAX_STATIONS)
      REAL*4        BRTZNE(4,MAX_STATIONS,MAX_STATIONS)
      REAL*4        INVEHL(MAX_ZONES),FARE5(MAX_ZONES),
     *              TRWAIT(MAX_ZONES),
     *              TRANSF(MAX_ZONES),WAIT1(MAX_ZONES)
      REAL*4        WALKEGR(MAX_ZONES),INVEHT(MAX_ZONES),
     *              INVEHE(MAX_ZONES),INVEHR(MAX_ZONES)
      REAL*4        WALKACC(MAX_ZONES),WALKTFR(MAX_ZONES),
     *              INVEH2(MAX_ZONES)
      REAL*4        STAZNED(5,15,BMAX_STATIONS,BMAX_IZONES)
      REAL*4        STASTAD(3,9,BMAX_STATIONS,BMAX_STATIONS)
      CHARACTER*13  NAME(3)
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
      IF(IMODE.EQ.5) FILENO=17
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
CFIRST WAIT
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
C...TRANSITWAY OR BRT IVTT
      IF(IMODE.EQ.3) GOTO 213
      PURP=9
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 104,II=1,MAX_ZONES
 104  INVEHT(II)=FLOAT(VAR(II))/100.0
C...TOTAL IN-VEHICLE TIME
 213  IF(IMODE.EQ.3) PURP=9
      IF(IMODE.GE.4) PURP=10
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 102,II=1,MAX_ZONES
 102  INVEH2(II)=FLOAT(VAR(II))/100.0
C...WALK EGRESS TIME
      IF(IMODE.EQ.3) PURP=10
      IF(IMODE.GE.4) PURP=11
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 110,II=1,MAX_ZONES
 110  WALKEGR(II)=FLOAT(VAR(II))/100.0
C... WALK TRANSFER TIME
      IF(IMODE.EQ.3) PURP=11
      IF(IMODE.GE.4) PURP=12
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 111,II=1,MAX_ZONES
 111  WALKTFR(II)=FLOAT(VAR(II))/100.0
C
C EGRESS ZONE LOOP
C
      DO 200 JZ=1,MAX_IZONES
      IF(.NOT.JOI(JZ)) GO TO 200
      STAZNE(1,SC,JZ)=99999.9
      STAZNE(2,SC,JZ)=0.0
C
C DESTINATION STATION --> EGRESS ZONE VALIDITY CHECKS
C
      IF(IMODE.EQ.3.AND.INVEHE(JZ).LE.0) GO TO 200
      IF(IMODE.EQ.3.AND.(NOTXFR).AND.
     *  (INVEH2(JZ).GT.INVEHE(JZ))) GO TO 200
      IF(IMODE.GE.4.AND.INVEHT(JZ).LE.0) GO TO 200
C     IF(IMODE.GE.4.AND.(NOTXFR).AND.
C    *  (INVEH2(JZ).GT.INVEHT(JZ))) GO TO 200
      IF(WALKEGR(JZ).LE.0.0) GO TO 200
C
C COMPUTE EGRESS PORTION OF UTILITY
C
C....USING MODEL COEFFICIENTS
      IF(IMODE.EQ.3) THEN
      STAZNE(2,SC,JZ)=COEFF(11)*INVEHL(JZ) + COEFF(13)*WAIT1(JZ) +
     *       COEFF(14)*TRWAIT(JZ) + COEFF(15)*(TRANSF(JZ)) +
     *       COEFF(11)*INVEHE(JZ)+
     *       COEFF(17)*WALKTFR(JZ)
C....USING PATH SELECTION COEFFICIENTS
      STAZNE(1,SC,JZ)=INVEHL(JZ) + 2.0*WAIT1(JZ) +
     *       2.0*TRWAIT(JZ) + 2.0*(TRANSF(JZ)) +
     *       0.22*FARE5(JZ)  + 2.0*WALKEGR(JZ) +
     *       2.0*WALKTFR(JZ)+INVEHE(JZ)
	ELSEIF(IMODE.GE.4) THEN
	STAZNE(2,SC,JZ)=COEFF(11)*INVEHL(JZ) + COEFF(13)*WAIT1(JZ) +
     *       COEFF(14)*TRWAIT(JZ) + COEFF(15)*(TRANSF(JZ)) +
     *       COEFF(11)*INVEHT(JZ)+
     *       COEFF(17)*WALKTFR(JZ)
C....USING PATH SELECTION COEFFICIENTS
      STAZNE(1,SC,JZ)=INVEHL(JZ) + 2.0*WAIT1(JZ) +
     *       2.0*TRWAIT(JZ) + 2.0*(TRANSF(JZ)) +
     *       0.22*FARE5(JZ)  + 2.0*WALKEGR(JZ) +
     *       2.0*WALKTFR(JZ)+INVEHT(JZ)	 
	  ENDIF
C
C.....STORE WALK TIME AT DESTINATION
      STAZNE(3,SC,JZ)=WALKEGR(JZ)
C.....STORE TRANSIT FARE
      STAZNE(4,SC,JZ)=FARE5(JZ)
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
      IF(IMODE.EQ.5) THEN
      IF(INVEHL(JZ).GT.0) LBUS=1
      IF(INVEHR(JZ).GT.0) RBUS=1
      IF(INVEHE(JZ).GT.0) EBUS=1
      STAZNEI(SC,JZ,IMODE,1)=LBUS
      STAZNEI(SC,JZ,IMODE,2)=RBUS
      STAZNEI(SC,JZ,IMODE,3)=EBUS
      END IF
C....................................................................
      IF((DEBUG.OR.BESTPATH).AND.(STAZNE(1,SC,JZ).LT.99999.9)) THEN
      WRITE(34) IMODE,IC,JZ,INVEH2(JZ),
     *               INVEHL(JZ),INVEHE(JZ),INVEHT(JZ),
     *               INVEHR(JZ),WAIT1(JZ),TRWAIT(JZ),
     *               TRANSF(JZ),FARE5(JZ),WALKEGR(JZ),
     *               WALKTFR(JZ),WALKACC(JZ),
     *               STAZNE(1,SC,JZ),STAZNE(2,SC,JZ)
      IF(SDETAIL) THEN
      WRITE(63,9025) NAME(IMODE-2),IC,STANAME(SC),JZ,INVEH2(JZ),
     *               INVEHL(JZ),INVEHE(JZ),INVEHT(JZ),
     *               INVEHR(JZ),WAIT1(JZ),TRWAIT(JZ),
     *               TRANSF(JZ),FARE5(JZ),WALKEGR(JZ),
     *               WALKTFR(JZ),WALKACC(JZ),
     *               STAZNE(1,SC,JZ),STAZNE(2,SC,JZ)
 9025 FORMAT(1X,'ACCESS STATION --> EGRESS ZONE COMPUTATIONS',
     *       1X,'(',A13,')'/
     *       1X,'------------------------------------------------'//
     *       1X,'DESTINATION       STATION=',I8,1X,A37/
     *       1X,'EGRESS            ZONE   =',I8/
     *       1X,'IN-VEHICLE           TIME=',F8.2/
     *       1X,'LOCAL IN-VEHICLE     TIME=',F8.2/
     *       1X,'EXPRESS IN-VEHICLE   TIME=',F8.2/
     *       1X,'TWY/BRT IN-VEHICLE   TIME=',F8.2/
     *       1X,'RAPID IN-VEHICLE     TIME=',F8.2/
     *       1X,'1ST WAIT             TIME=',F8.2/
     *       1X,'TRANSFER WAIT        TIME=',F8.2/
     *       1X,'NUMBER OF TRANSFERS      =',F8.2/
     *       1X,'FARE                     =',F8.2/
     *       1X,'WALK EGRESS          TIME=',F8.2/
     *       1X,'WALK TRANSFER        TIME=',F8.2/
     *       1X,'WALK ACCESS          TIME=',F8.2//
     *       1X,'SELECTION UTILITY VALUE  =',F10.2/
     *       1X,'UTILITY VALUE            =',F10.5/)
      END IF
      IF(BESTPATH) THEN
      STAZNED(IMODE,1,SC,JZ)=INVEH2(JZ)
      STAZNED(IMODE,2,SC,JZ)=INVEHL(JZ)
      STAZNED(IMODE,3,SC,JZ)=INVEHE(JZ)
      STAZNED(IMODE,4,SC,JZ)=INVEHT(JZ)
      STAZNED(IMODE,5,SC,JZ)=INVEHR(JZ)
      STAZNED(IMODE,6,SC,JZ)=WAIT1(JZ)
      STAZNED(IMODE,7,SC,JZ)=TRWAIT(JZ)
      STAZNED(IMODE,8,SC,JZ)=TRANSF(JZ)
      STAZNED(IMODE,9,SC,JZ)=FARE5(JZ)
      STAZNED(IMODE,10,SC,JZ)=WALKEGR(JZ)
      STAZNED(IMODE,11,SC,JZ)=WALKTFR(JZ)
      STAZNED(IMODE,12,SC,JZ)=WALKACC(JZ)
      END IF
      END IF
C.......................................................................
  200 CONTINUE
C 
C SECOND EGRESS ZONE LOOP TO COMMUTER RAIL STATIONS
C FROM EXPRESS BUS
C
      IF(IMODE.NE.3) GO TO 150
      DO 300 JZ=1,MAX_STATIONS
      JZ2=JZ+MAX_IZONES
      EXPZNE(1,SC,JZ)=99999.9
      EXPZNE(2,SC,JZ)=0.0
      IF(STANUM(JZ).NE.1) GO TO 300
      IF(STADATA(SC,8).NE.9.0) GO TO 300
C
C DESTINATION STATION --> EGRESS ZONE VALIDITY CHECKS
C
      IF(INVEHE(JZ2).LE.0) GO TO 300
C
C COMPUTE EGRESS PORTION OF UTILITY
C
C....USING MODEL COEFFICIENTS
      EXPZNE(2,SC,JZ)=COEFF(11)*INVEHL(JZ2) + COEFF(13)*WAIT1(JZ2) +
     *       COEFF(14)*TRWAIT(JZ2) + COEFF(15)*(TRANSF(JZ2)) +
     *       COEFF(11)*INVEHE(JZ2)+
     *       COEFF(17)*WALKTFR(JZ2)
C....USING PATH SELECTION COEFFICIENTS
      EXPZNE(1,SC,JZ)=INVEHL(JZ2) + 2.0*WAIT1(JZ2) +
     *       2.0*TRWAIT(JZ2) + 2.0*(TRANSF(JZ2)) +
     *       0.22*FARE5(JZ2)  + 2.0*WALKEGR(JZ2) +
     *       2.0*WALKTFR(JZ2)+INVEHE(JZ2)
C
C.....STORE WALK TIME AT DESTINATION
      EXPZNE(3,SC,JZ)=WALKEGR(JZ2)
C.....STORE TRANSIT FARE
      EXPZNE(4,SC,JZ)=FARE5(JZ2)
  300 CONTINUE  
C 
C EGRESS ZONE LOOP TO URBAN RAIL STATIONS
C FROM BRT
C
  150 IF(IMODE.NE.5) GO TO 100
      DO 400 JZ=1,MAX_STATIONS
      JZ2=JZ+MAX_IZONES
      BRTZNE(1,SC,JZ)=99999.9
      BRTZNE(2,SC,JZ)=0.0
      IF(STANUM(SC).NE.5) GO TO 400 
      IF(STANUM(JZ).EQ.2) THEN
       SC2=EQUIV3(JZ,1)-MAX_IZONES
       IF(STANUM(SC2).EQ.5) GO TO 450
       SC2=EQUIV3(JZ,2)-MAX_IZONES
       IF(STANUM(sc2).EQ.5) GO TO 450
       SC2=EQUIV3(JZ,3)-MAX_IZONES
       IF(STANUM(SC2).EQ.5) GO TO 450
       GO TO 400
      ELSE
       GO TO 400
      END IF
C
C DESTINATION STATION --> EGRESS ZONE VALIDITY CHECKS
C
  450 IF(INVEHT(JZ2).LE.0) GO TO 400
C
C COMPUTE EGRESS PORTION OF UTILITY
C
C....USING MODEL COEFFICIENTS
      BRTZNE(2,SC,JZ)=COEFF(11)*INVEH2(JZ2) + COEFF(13)*WAIT1(JZ2) +
     *       COEFF(14)*TRWAIT(JZ2) + COEFF(15)*(TRANSF(JZ2)) +
     *       COEFF(17)*WALKTFR(JZ2)
C....USING PATH SELECTION COEFFICIENTS
      BRTZNE(1,SC,JZ)=INVEH2(JZ2) + 2.0*WAIT1(JZ2) +
     *       2.0*TRWAIT(JZ2) + 2.0*(TRANSF(JZ2)) +
     *       0.22*FARE5(JZ2)  + 2.0*WALKEGR(JZ2) +
     *       2.0*WALKTFR(JZ2)
C
C.....STORE WALK TIME AT DESTINATION
      BRTZNE(3,SC,JZ)=WALKEGR(JZ2)
C.....STORE TRANSIT FARE
      BRTZNE(4,SC,JZ)=FARE5(JZ2)
C -------------------------------------------------------------------
      IF(DEBUG.OR.BESTPATH) THEN
      WRITE(32) (SC+MAX_IZONES),JZ2,INVEHT(JZ2),
     *               WAIT1(JZ2),TRWAIT(JZ2),TRANSF(JZ2),
     *               FARE5(JZ2),WALKEGR(JZ2),WALKTFR(JZ2)
      IF(SDETAIL) THEN
      WRITE(64,401) (SC+MAX_IZONES),STANAME(SC),
     *               JZ2,STANAME(JZ),INVEHT(JZ2),INVEHL(JZ2),
     *               INVEHE(JZ2),INVEHR(JZ2),INVEH2(JZ2),
     *               WAIT1(JZ2),TRWAIT(JZ2),TRANSF(JZ2),
     *               FARE5(JZ2),WALKEGR(JZ2),WALKTFR(JZ2),
     *               BRTZNE(1,SC,JZ),BRTZNE(2,SC,JZ),
     *               BRTZNE(3,SC,JZ),BRTZNE(4,SC,JZ)
  401 FORMAT(/' BRT STATION=',I4,1X,A37/
     *        ' UR  STATION=',I4,1X,A37/
     *        ' BRT       IVT =',F10.2/
     *        ' LOCAL     IVT =',F10.2/
     *        ' EXPRESS   IVT =',F10.2/
     *        ' RAPID BUS IVT =',F10.2/
     *        ' TOTAL     IVT =',F10.2/
     *        ' 1ST WAIT TIME =',F10.2/
     *        ' TXF WAIT TIME =',F10.2/
     *        ' TRANSFERS     =',F10.2/
     *        ' FARE          =',F10.2/
     *        ' WALK EGRESS   =',F10.2/
     *        ' TRANSFER WALK =',F10.2/
     *        ' BRTZNE (1)    =',F10.3/
     *        ' BRTZNE (2)    =',F10.3/
     *        ' BRTZNE (3)    =',F10.3/
     *        ' BRTZNE (4)    =',F10.3)
      END IF
      IF(BESTPATH) THEN
      STASTAD(3,1,SC,JZ)=INVEHT(JZ2)
      STASTAD(3,2,SC,JZ)=WAIT1(JZ2)
      STASTAD(3,3,SC,JZ)=TRWAIT(JZ2)
      STASTAD(3,4,SC,JZ)=TRANSF(JZ2)
      STASTAD(3,5,SC,JZ)=FARE5(JZ2)
      STASTAD(3,6,SC,JZ)=WALKEGR(JZ2)
      STASTAD(3,7,SC,JZ)=WALKTFR(JZ2)
      END IF
      END IF
C -------------------------------------------------------------------
  400 CONTINUE     
  100 CONTINUE
      RETURN
      END
