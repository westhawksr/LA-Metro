C-------------------------------------------------------------------
C        STATION --> STATION UTILITY COMPUTATION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE STATION(STASTA,IMODE,STASTAD)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'tpcom.inc'
       include 'control.inc'
	 include 'mtamcpar.inc'
C
C DATA DECLARATIONS
C
      integer*2     IMODE,ic,sc,sc2,dc
      REAL*4        STASTA(4,MAX_STATIONS,MAX_STATIONS)
      REAL*4        INVEH(MAX_ZONES),FARE(MAX_ZONES),
     *              WALKACC(MAX_ZONES),
     *              TRANSF(MAX_ZONES),WAIT1(MAX_ZONES),
     *              WALKEGR(MAX_ZONES),
     *              WAIT2(MAX_ZONES),TIVT(MAX_ZONES),
     *              WALKTFR(MAX_ZONES)
      REAL*4        STASTAD(3,9,BMAX_STATIONS,BMAX_STATIONS)
      REAL*4        WALK,STAUTL,SELUTL,KLINE,WAIT1A,WAIT1B
      CHARACTER*13  NAME(2)
      DATA          NAME/'Commuter Rail',
     *                   'Urban Rail   '/
C
      WRITE(*,8000) NAME(IMODE)
 8000 FORMAT(1X,'Station --> Station',
     *            ' Utility Computations for ',a13)
C
C    READ IN APPROPRIATE ZONE RANGES PER MODE
c
      IF(IMODE.EQ.1) FILENO=10
	IF(IMODE.EQ.2) FILENO=12
C
C
C  ORIGIN STATION LOOP 
C
      DO 50 SC=1,MAX_STATIONS
      IC=sc+MAX_IZONES
      IF(STANUM(SC).NE.IMODE) GO TO 50
      IF(STADATA(SC,6).NE.1.0) GOTO 50
C
C
C    USE TRANPLAN I/O TO
C    OBTAIN LEVEL-OF-SERVICE VALUES
C
C
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
C.....ACCESS WALK TIME
      PURP=3
      call intab(fileno,VAR,ic,PURP,dummy,io)
      DO 106,II=1,MAX_ZONES
  106 WALKACC(II)=FLOAT(VAR(II))/100.0
C.....NUMBER OF TRANSFERS
C     TRANSF
      PURP=4
      call intab(fileno,VAR,ic,PURP,dummy,io)
      DO 103,II=1,MAX_ZONES
  103 TRANSF(II)=FLOAT(VAR(II))
C.....FARE
C     FARE
      PURP=6
      call intab(fileno,VAR,ic,PURP,dummy,io)
      DO 102,II=1,MAX_ZONES
 102  FARE(II)=FLOAT(VAR(II))
C.....COMMUTER RAIL/URBAN RAIL IN-VEHICLE TIME
      IF(IMODE.EQ.1) PURP=11
      IF(IMODE.EQ.2) PURP=10
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 101,II=1,MAX_ZONES
 101  INVEH(II)=FLOAT(VAR(II))/100.0
C.....TOTAL IN-VEHICLE TIME
      IF(IMODE.EQ.1) PURP=12
      IF(IMODE.EQ.2) PURP=11
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 107,II=1,MAX_ZONES
  107 TIVT(II)=FLOAT(VAR(II))/100.0
C....EGRESS WALK TIME
      IF(IMODE.EQ.1) PURP=13
      IF(IMODE.EQ.2) PURP=12
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 108,II=1,MAX_ZONES
  108 WALKEGR(II)=FLOAT(VAR(II))/100.0
C...TRANSFER WALK TIME
      IF(IMODE.EQ.1) PURP=14
      IF(IMODE.EQ.2) PURP=13
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 109,II=1,MAX_ZONES
 109  WALKTFR(II)=FLOAT(VAR(II))/100.0
C
C
C   DESTINATION STATION LOOP
C
      DO 60 SC2=1,MAX_STATIONS
      DC=SC2+MAX_IZONES
      IF(IC.EQ.DC) GOTO 60
      IF(STANUM(SC2).NE.IMODE) GO TO 60
      IF((INVEH(DC).LE.0.0.OR.INVEH(DC).GE.99999.9).OR.
     * (TIVT(DC).GT.INVEH(DC))) THEN
      STAUTL=0.0
      TD1UTL=0.0
      TD2UTL=0.0
      SELUTL=99999.9
      ELSE
      WALK=WALKEGR(DC)+WALKTFR(DC)
      KLINE=0.0
      IF(STADATA(SC2,8).EQ.1) KLINE=KRED
      IF(STADATA(SC2,8).EQ.2) KLINE=KBLUE
      IF(STADATA(SC2,8).EQ.3) KLINE=KGREEN
      IF(STADATA(SC2,8).EQ.4) KLINE=KGOLD
      IF(STADATA(SC,8).EQ.1) KLINE=KRED
      IF(STADATA(SC,8).EQ.2) KLINE=KBLUE
      IF(STADATA(SC,8).EQ.3) KLINE=KGREEN
      IF(STADATA(SC,8).EQ.4) KLINE=KGOLD
      IF(STADATA(SC,8).EQ.5.AND.STADATA(SC2,8).EQ.5) KLINE=KRED
      KLINE=KLINE/(LSUMS*LSUMA*LSUMT*LSUMSUW)
        if(imode.eq.1) then
        WAIT1A=AMIN1(WAIT1(DC),WAITLT)
        WAIT1B=DIM(WAIT1(DC),WAITLT)
        STAUTL=COEFF(10)*INVEH(DC) + COEFF(3)*WAIT1A +
     *       COEFF(4)*WAIT2(DC) + COEFF(5)*(TRANSF(DC)) +
     *       COEFF(7)*WALKTFR(DC) + COEFF(10)*WAIT1B
        elseif(imode.eq.2) then
        WAIT1A=AMIN1(WAIT1(DC),WAITLT)
        WAIT1B=DIM(WAIT1(DC),WAITLT)
        STAUTL=COEFF(1)*INVEH(DC) + COEFF(3)*WAIT1A +
     *       COEFF(4)*WAIT2(DC) + COEFF(5)*TRANSF(DC)+
     *       COEFF(7)*WALKTFR(DC)+ COEFF(1)*WAIT1B +
     *       KLINE
         endif
      SELUTL=INVEH(DC) + 2.0*WAIT1(DC) +
     *       2.0*WAIT2(DC) + 2.0*(TRANSF(DC)) +
     *       0.14*FARE(DC)  + 2.0*WALK
      END IF
      STASTA(1,SC,SC2)=SELUTL
      STASTA(2,SC,SC2)=STAUTL
      STASTA(3,SC,SC2)=INVEH(DC)
      STASTA(4,SC,SC2)=FARE(DC)
      TXFERS1(SC,SC2)=TRANSF(DC)
C....................................................................
      IF((DEBUG.OR.BESTPATH).AND.(SELUTL.LT.99999.9)) THEN
      WRITE(31) IMODE,IC,DC,WAIT1(DC),WAIT2(DC),
     *                TRANSF(DC),FARE(DC),WALKACC(DC),INVEH(DC),
     *                TIVT(DC),WALKEGR(DC),WALKTFR(DC),
     *                SELUTL,STAUTL
      IF(SDETAIL) THEN
      WRITE(61,9029)  IC,STANAME(SC),DC,STANAME(SC2),
     *                WAIT1(DC),WAIT2(DC),
     *                TRANSF(DC),FARE(DC),WALKACC(DC),INVEH(DC),
     *                TIVT(DC),WALKEGR(DC),WALKTFR(DC),
     *                SELUTL,STAUTL
 9029 FORMAT(/1X,'STATION --> STATION UTILITY COMPUTATIONS'/
     *       1X,'----------------------------------------'//
     *       1X,'ORIGIN            STATION=',I8,1X,A37/
     *       1X,'DESTINATION       STATION=',I8,1X,A37/
     *       1X,'1ST WAIT             TIME=',F8.2/
     *       1X,'TRANSFER WAIT        TIME=',F8.2/
     *       1X,'NUMBER OF TRANSFERS      =',F8.2/
     *       1X,'FARE                     =',F8.2/
     *       1X,'ACCESS WALK TIME         =',F8.2/
     *       1X,'PRIMARY MODE IN-VEHICLE  =',F8.2/
     *       1X,'TOTAL IN-VEHICLE TIME    =',F8.2/
     *       1X,'EGRESS WALK TIME         =',F8.2/
     *       1X,'TRANSFER WALK TIME       =',F8.2//
     *       1X,'SELECTION UTILITY VALUE  =',F10.2/
     *       1X,'UTILITY VALUE            =',F10.5/)
      END IF
      IF(BESTPATH) THEN
      STASTAD(IMODE,1,SC,SC2)=WAIT1(DC)
      STASTAD(IMODE,2,SC,SC2)=WAIT2(DC)
      STASTAD(IMODE,3,SC,SC2)=TRANSF(DC)
      STASTAD(IMODE,4,SC,SC2)=FARE(DC)
      STASTAD(IMODE,5,SC,SC2)=WALKACC(DC)
      STASTAD(IMODE,6,SC,SC2)=INVEH(DC)
      STASTAD(IMODE,7,SC,SC2)=TIVT(DC)
      STASTAD(IMODE,8,SC,SC2)=WALKEGR(DC)
      STASTAD(IMODE,9,SC,SC2)=WALKTFR(DC)
      END IF
      END IF
C.......................................................................
   60 CONTINUE
   50 CONTINUE
      RETURN
      END
