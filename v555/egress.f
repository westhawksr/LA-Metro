C-------------------------------------------------------------------
C        STATION --> DESTINATION ZONE UTILITY COMPUTATION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE EGRESS(STAZNE,IMODE,ZONESTA,STAZNEI,STAZNED)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'tpcom.inc'
       include 'control.inc'
       include 'mtamcpar.inc'
C
C DATA DECLARATIONS
C
      INTEGER*2     IMODE,SC,IC,JZ,ZONESTA(MAX_IZONES)
      INTEGER*2     STAZNEI(MAX_STATIONS,MAX_IZONES,6,6)
      INTEGER*2     LBUS,RBUS,EBUS,TBUS,BBUS,UBUS
      REAL*4        STAZNE(4,MAX_STATIONS,MAX_IZONES)
      REAL*4        INVEH(MAX_ZONES),FARE(MAX_ZONES),
     *              WAIT2(MAX_ZONES),INVEHL(MAX_ZONES),
     *              INVEHR(MAX_ZONES),INVEHE(MAX_ZONES),
     *              INVEHT(MAX_ZONES),INVEHU(MAX_ZONES),
     *              INVEHB(MAX_ZONES),
     *              TRANSF(MAX_ZONES),WAIT1(MAX_ZONES)
      REAL*4        WALKEGR(MAX_ZONES),LHIVT(MAX_ZONES)
      REAL*4        WALKACC(MAX_ZONES),WALKTFR(MAX_ZONES)
      REAL*4        STAZNED(5,15,BMAX_STATIONS,BMAX_IZONES)
      CHARACTER*13  NAME(2)
      DATA          NAME/'Commuter Rail',
     *                   'Urban Rail   '/
      WRITE(*,8000) NAME(IMODE)
 8000 FORMAT(1X,'Egress Station --> Destination Zone',
     *            ' Utility Computations for ',a13)
C
      IF(IMODE.EQ.1) THEN
	FILENO=10
	ELSEIF(IMODE.EQ.2) THEN
	FILENO=12
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
C...URBAN RAIL IN-VEHICLE TIME FOR COMMUTER RAIL PATHS
      IF(IMODE.EQ.1) THEN
      PURP=10
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 114 II=1,MAX_ZONES
  114 INVEHU(II)=FLOAT(VAR(II))/100.0
      END IF
C...RAIL IN-VEHICLE TIME
      IF(IMODE.EQ.1) PURP=11
      IF(IMODE.EQ.2) PURP=10
      call intab(fileno,VAR,ic,PURP,dummy,io)
      DO 102,II=1,MAX_ZONES
 102  LHIVT(II)=FLOAT(VAR(II))/100.0
C...TOTAL IN-VEHICLE TIME
      IF(IMODE.EQ.1) PURP=12
      IF(IMODE.EQ.2) PURP=11
      call intab(fileno,VAR,ic,PURP,dummy,io)
      DO 101,II=1,MAX_ZONES
 101  INVEH(II)=FLOAT(VAR(II))/100.0
C....WALK TIME EGRESS
      IF(IMODE.EQ.1) PURP=13
      IF(IMODE.EQ.2) PURP=12
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 108,II=1,MAX_ZONES
 108  WALKEGR(II)=FLOAT(VAR(II))/100.0
C...WALK TIME TRANSFER
      IF(IMODE.EQ.1) PURP=14
      IF(IMODE.EQ.2) PURP=13
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 109,II=1,MAX_ZONES
 109  WALKTFR(II)=FLOAT(VAR(II))/100.0
C...BRT IN-VEHICLE TIME FOR URBAN RAIL ONLY
      IF(IMODE.EQ.2) THEN
      PURP=14
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 115 II=1,MAX_ZONES
  115 INVEHB(II)=FLOAT(VAR(II))/100.0
      END IF
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
       IF(LHIVT(JZ).GT.0) GO TO 200
      IF(WALKEGR(JZ).LE.0.0) GO TO 200
C
C COMPUTE EGRESS PORTION OF UTILITY
C
C....USING MODEL COEFFICIENTS
      STAZNE(2,SC,JZ)=COEFF(1)*INVEH(JZ) + COEFF(3)*WAIT1(JZ) +
     *       COEFF(4)*WAIT2(JZ) + COEFF(5)*(TRANSF(JZ)) +
     *       COEFF(7)*WALKTFR(JZ)
C....USING PATH SELECTION COEFFICIENTS
      STAZNE(1,SC,JZ)=INVEH(JZ) + 2.0*WAIT1(JZ) +
     *       2.0*WAIT2(JZ) + 2.0*(TRANSF(JZ)) +
     *       0.22*FARE(JZ)  + 2.0*WALKEGR(JZ) +
     *       2.0*WALKTFR(JZ)
C.....STORE WALK TIME AT DESTINATION
      STAZNE(3,SC,JZ)=WALKEGR(JZ)
C.....STORE TRANSIT FARE
      STAZNE(4,SC,JZ)=FARE(JZ)
      IF((SC+MAX_IZONES).EQ.UNIONSTA) STAZNE(4,SC,JZ)=0.0 
C.....STORE NUMBER OF TRANSFERS
      IF(INVEH(JZ).GT.0) TRANSF(JZ)=TRANSF(JZ)+1
      TXFERS2(SC,JZ)=TRANSF(JZ)
      IF(INVEH(JZ).LE.0) THEN
      ZONESTA(JZ)=IC
      STAIND(SC,JZ)=1
      ELSE
      STAIND(SC,JZ)=2
      END IF
C.....STORE SUBMODE USAGE
      LBUS=0
      RBUS=0
      EBUS=0
      TBUS=0
      BBUS=0
      UBUS=0
      IF(INVEHL(JZ).GT.0) LBUS=1
      IF(INVEHR(JZ).GT.0) RBUS=1
      IF(INVEHE(JZ).GT.0) EBUS=1
      IF(INVEHT(JZ).GT.0) TBUS=1
      IF(INVEHB(JZ).GT.0) BBUS=1
      IF(IMODE.EQ.1.AND.INVEHU(JZ).GT.0) UBUS=1
      STAZNEI(SC,JZ,IMODE,1)=LBUS
      STAZNEI(SC,JZ,IMODE,2)=RBUS
      STAZNEI(SC,JZ,IMODE,3)=EBUS
      STAZNEI(SC,JZ,IMODE,4)=TBUS
      STAZNEI(SC,JZ,IMODE,5)=BBUS
      STAZNEI(SC,JZ,IMODE,6)=UBUS
C....................................................................
      IF((DEBUG.OR.BESTPATH).AND.(STAZNE(1,SC,JZ).LT.99999.9)) THEN
      IF(TRANSF(JZ).GT.0) TRANSF(JZ)=TRANSF(JZ)-1.0
      WRITE(33) IMODE,IC,JZ,LHIVT(JZ),INVEH(JZ),WAIT1(JZ),WAIT2(JZ),
     *               TRANSF(JZ),FARE(JZ),WALKACC(JZ),
     *               WALKEGR(JZ),
     *               WALKTFR(JZ),
     *               STAZNE(1,SC,JZ),STAZNE(2,SC,JZ),
     *               INVEHL(JZ),INVEHR(JZ),INVEHE(JZ),
     *               INVEHT(JZ),INVEHB(JZ),INVEHU(JZ)
      IF(SDETAIL) THEN
      WRITE(62,9025) NAME(IMODE),IC,STANAME(SC),JZ,
     *               LHIVT(JZ),INVEH(JZ),
     *               INVEHL(JZ),INVEHR(JZ),INVEHE(JZ),
     *               INVEHT(JZ),INVEHB(JZ),INVEHU(JZ),
     *               WAIT1(JZ),WAIT2(JZ),
     *               TRANSF(JZ),FARE(JZ),WALKACC(JZ),
     *               WALKEGR(JZ),
     *               WALKTFR(JZ),
     *               STAZNE(1,SC,JZ),STAZNE(2,SC,JZ)
 9025 FORMAT(1X,'DESTINATION STATION --> EGRESS ZONE COMPUTATIONS',
     *       1X,'(',A13,')'/
     *       1X,'------------------------------------------------'//
     *       1X,'DESTINATION       STATION=',I8,1X,A37/
     *       1X,'EGRESS            ZONE   =',I8/
     *       1X,'RAIL IN-VEHICLE      TIME=',F8.2/
     *       1X,'IN-VEHICLE           TIME=',F8.2/
     *       1X,'IN-VEHICLE-LOCAL     TIME=',F8.2/
     *       1X,'IN-VEHICLE-RAPID     TIME=',F8.2/
     *       1X,'IN-VEHICLE-EXPRESS   TIME=',F8.2/
     *       1X,'IN-VEHICLE-TWY       TIME=',F8.2/
     *       1X,'IN-VEHICLE-BRT       TIME=',F8.2/
     *       1X,'IN-VEHICLE-URB RAIL  TIME=',F8.2/
     *       1X,'1ST WAIT             TIME=',F8.2/
     *       1X,'TRANSFER WAIT        TIME=',F8.2/
     *       1X,'NUMBER OF TRANSFERS      =',F8.2/
     *       1X,'FARE                     =',F8.2/
     *       1X,'WALK ACCESS          TIME=',F8.2/
     *       1X,'WALK EGRESS          TIME=',F8.2/
     *       1X,'WALK TRANSFER        TIME=',F8.2//
     *       1X,'SELECTION UTILITY VALUE  =',F10.1/
     *       1X,'UTILITY VALUE            =',F10.5/)
      END IF
      IF(BESTPATH) THEN
      STAZNED(IMODE,1,SC,JZ)=LHIVT(JZ)
      STAZNED(IMODE,2,SC,JZ)=INVEH(JZ)
      STAZNED(IMODE,3,SC,JZ)=WAIT1(JZ)
      STAZNED(IMODE,4,SC,JZ)=WAIT2(JZ)
      STAZNED(IMODE,5,SC,JZ)=TRANSF(JZ)
      STAZNED(IMODE,6,SC,JZ)=FARE(JZ)
      STAZNED(IMODE,7,SC,JZ)=WALKACC(JZ)
      STAZNED(IMODE,8,SC,JZ)=WALKEGR(JZ)
      STAZNED(IMODE,9,SC,JZ)=WALKTFR(JZ)
      STAZNED(IMODE,10,SC,JZ)=INVEHL(JZ)
      STAZNED(IMODE,11,SC,JZ)=INVEHR(JZ)
      STAZNED(IMODE,12,SC,JZ)=INVEHE(JZ)
      STAZNED(IMODE,13,SC,JZ)=INVEHT(JZ)
      STAZNED(IMODE,14,SC,JZ)=INVEHB(JZ)
      STAZNED(IMODE,15,SC,JZ)=INVEHU(JZ)
      END IF
      END IF
C.......................................................................
  200 CONTINUE
  100 CONTINUE
      RETURN
      END
