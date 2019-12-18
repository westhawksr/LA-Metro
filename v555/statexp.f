C-------------------------------------------------------------------
C        STATION --> STATION UTILITY COMPUTATION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE STATEXP(STASTA,IMODE)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'tpcom.inc'
       include 'control.inc'
       include 'mtamcpar.inc'
C
C DATA DECLARATIONS
C
      integer*2     IMODE,ic,sc,sc2,dc
      REAL*4        STASTA(4,MAX_STATIONS ,MAX_STATIONS)
      REAL*4        INVEHL(MAX_ZONES),FARE5(MAX_ZONES),
     *              WALKACC(MAX_ZONES),
     *              TRANSF(MAX_ZONES),WAIT1(MAX_ZONES),
     *              WALKEGR(MAX_ZONES),
     *              TRWAIT(MAX_ZONES),
     *              WALKTFR(MAX_ZONES),
     *              INVEHE(MAX_ZONES),INVEHT(MAX_ZONES),
     *              INVEH2(MAX_ZONES)
      REAL*4        STAUTL,SELUTL
      CHARACTER*13  NAME(2)                                             
      DATA          NAME/'Express Bus  ',                               
     *                   'Transitway   '/                               
c
      WRITE(*,8000) NAME(IMODE-2)
 8000 FORMAT(1X,'Station --> Station',                                  
     *            ' Utility Computations for ',a13)
C
C    READ IN APPROPRIATE ZONE RANGES PER MODE
c
     	IF(IMODE.EQ.3) THEN
	FILENO=14
	ELSEIF(IMODE.EQ.4) THEN
	FILENO=15
	ENDIF
C
C  ORIGIN STATION LOOP 
C
      DO 50 SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      IF(STANUM(SC).NE.IMODE) GOTO 50
      IF(STADATA(SC,6).NE.1.0) GOTO 50
C
C    USE TRANPLAN I/O TO
C      OBTAIN LEVEL-OF-SERVICE VALUES
C
C..FIRST WAIT
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
C...EXPRESS BUS IVTT
      PURP=8
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 103,II=1,MAX_ZONES
 103  INVEHE(II)=FLOAT(VAR(II))/100.0
C...TRANSITWAY BUS IVTT
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
      UTIL=0.0
      STAUTL=99999.9
      CDIST=99999.9
      CSTA=0
C
C
C   DESTINATION STATION LOOP
C
      DO 60 SC2=1,MAX_STATIONS
      DC=SC2+MAX_IZONES
      IF(STANUM(SC2).NE.IMODE) GO TO 60
      IF(INVEHL(DC).GT.0) THEN
      STAUTL=0.0
      SELUTL=99999.9
      GO TO 60
      END IF
      IF(IC.EQ.DC) GOTO 60
      IF(IMODE.EQ.3.AND.INVEHE(DC).LE.0.0.OR.INVEHE(DC).GE.99999.9) THEN
      STAUTL=0.0
      SELUTL=99999.9
	ELSEIF(IMODE.EQ.4.AND.INVEHT(DC).LE.0.0.OR.INVEHT(DC).GE.99999.0)
     *	 THEN
	STAUTL=0.0
	SELUTL=99999.9
	ELSE
       IF(IMODE.EQ.3) THEN
	  STAUTL=COEFF(11)*INVEHE(DC) + COEFF(13)*WAIT1(DC) +
     *       COEFF(14)*TRWAIT(DC) +
     *       COEFF(17)*WALKTFR(DC)
        SELUTL=INVEHE(DC) + 2.0*WAIT1(DC) +
     *       2.0*TRWAIT(DC) + 2.0*(TRANSF(DC)) +
     *       0.14*FARE5(DC)  + 2.0*WALKTFR(DC)
	 ELSEIF(IMODE.EQ.4) THEN
       STAUTL=COEFF(11)*INVEHT(DC) + COEFF(13)*WAIT1(DC) +
     *       COEFF(14)*TRWAIT(DC) +
     *       COEFF(17)*WALKTFR(DC)
        SELUTL=INVEHT(DC) + 2.0*WAIT1(DC) +
     *       2.0*TRWAIT(DC) + 2.0*(TRANSF(DC)) +
     *       0.14*FARE5(DC)  + 2.0*WALKTFR(DC)
      END IF
	END IF
      STASTA(1,SC,SC2)=SELUTL
      STASTA(2,SC,SC2)=STAUTL
      STASTA(4,SC,SC2)=FARE5(DC)
C....................................................................
      IF((DEBUG.OR.BESTPATH).AND.(SELUTL.LT.99999.9)) THEN
      WRITE(27) IMODE,IC,DC,INVEHL(DC),INVEHE(DC),INVEHT(DC),
     *                WAIT1(DC),TRWAIT(DC),
     *                TRANSF(DC),FARE5(DC),
     *                INVEH2(DC),WALKACC(DC),WALKEGR(DC),
     *                WALKTFR(DC),SELUTL,STAUTL
      END IF
C.......................................................................
   60 CONTINUE
   50 CONTINUE
      RETURN
      END
