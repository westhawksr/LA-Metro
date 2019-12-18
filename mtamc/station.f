C-------------------------------------------------------------------
C        STATION --> STATION UTILITY COMPUTATION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE STATION(STASTA,IMODE,INDAPM,NHBACC,NHBUTL)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'tpcom.inc'
       include 'control.inc'
	     include 'mtamcpar.inc'
C
C DATA DECLARATIONS
C
      integer*2     IMODE,ic,sc,sc2,dc,T,XC
      INTEGER*2     INDAPM(MAX_ZONES,MAX_ZONES)
      INTEGER*4     FILENO2,XFER(MAX_ZONES,10)
      REAL*4        STASTA(5,MAX_STATIONS,MAX_STATIONS)
      REAL*4        INVEH(MAX_ZONES),FARE(MAX_ZONES),
     *              TRANSF(MAX_ZONES),WAIT1(MAX_ZONES),
     *              WAIT2(MAX_ZONES),WALKTFR(MAX_ZONES)
      REAL*4        STAUTL,KLINE,WAIT1A,WAIT1B
      REAL*4        LUNRVAL,NUNRVAL,LCRDVAL,NCAPVAL
      INTEGER*2     LUNRELM(MAX_ZONES)
      INTEGER*2     NUNRELM(MAX_ZONES)
      INTEGER*2     LCROWDM(MAX_ZONES)
      INTEGER*2     NCAPACM(MAX_ZONES)
      REAL*4        NHBACC(MAX_STATIONS,MAX_STATIONS)
      REAL*4        NHBUTL(MAX_STATIONS,MAX_STATIONS)
      REAL*4        FARECOST
      CHARACTER*13  NAME(5)
      CHARACTER*37  TMPNAME(10),BLANK
      DATA          BLANK/'   '/
      DATA          NAME/'Commuter Rail',
     *                   'Urban Rail   ',
     *                   'Express Bus  ',
     *                   'Transitway   ',
     *                   'BRT          '/
C
      WRITE(*,8000) NAME(IMODE)
 8000 FORMAT(1X,'Station --> Station',
     *            ' Utility Computations for ',a13)
C
C    READ IN APPROPRIATE ZONE RANGES PER MODE
c
      IF(IMODE.EQ.1) THEN
      FILENO=10
	    CLOSE(FILENO,STATUS='KEEP')
	    CALL PREPIO(BCRSK,FILENO)
      END IF
	    IF(IMODE.EQ.2) FILENO=12
	    IF(IMODE.EQ.5) FILENO=17
      IF(IMODE.EQ.1) FILENO2=113
	    IF(IMODE.EQ.2) FILENO2=112
	    IF(IMODE.EQ.5) FILENO2=114

C
C
C  ORIGIN STATION LOOP 
C
      DO 50 SC=1,MAX_STATIONS
      IC=sc+MAX_IZONES
      IF(STANUM(SC).NE.IMODE) GO TO 50
      IF(STADATA(SC,6).NE.1.0) GOTO 50 
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
C.....IN-VEHICLE TIME
      PURP=5
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 101,II=1,MAX_ZONES
 101  INVEH(II)=FLOAT(VAR(II))/100.0
C...TRANSFER WALK TIME
      PURP=6
      CALL INTAB(FILENO,VAR,IC,PURP,DUMMY,IO)
      DO 109,II=1,MAX_ZONES
 109  WALKTFR(II)=FLOAT(VAR(II))/100.0   
C
C   LINK UNRELIABILITY
C
      IF(LUNREL) THEN
      T=6
      CALL INTAB(90,VAR,IC,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      LUNRELM(II)=IJINT(VAR(II))
      END DO
      END IF
C
C   STOP UNRELIABILITY
C
      IF(NUNREL) THEN
      T=6
      CALL INTAB(91,VAR,IC,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      NUNRELM(II)=IJINT(VAR(II))
      END DO
      END IF
C
C   CROWDING
C
      IF(LCROWD) THEN
      T=6
      CALL INTAB(92,VAR,IC,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      LCROWDM(II)=IJINT(VAR(II))
      END DO
      END IF
C
C   STOP CAPACITY
C
      IF(NCAPAC) THEN
      T=6
      CALL INTAB(93,VAR,IC,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      NCAPACM(II)=IJINT(VAR(II))
      END DO
      END IF
C
C   APM INDICATOR FOR URBAN RAIL
C
      IF(APMIND.AND.IMODE.EQ.2) THEN
      PURP=1
      CALL INTAB(99,VAR,IC,PURP,DUMMY,IO)
      DO II=1,MAX_ZONES
      IF(VAR(II).GT.0) INDAPM(IC,II)=1
      END DO
      END IF
C
C   TRANSFER NODES 
C
      DO T=1,10
      PURP=T
      CALL INTAB(FILENO2,VAR,IC,PURP,DUMMY,IO)
      DO II=1,MAX_ZONES
      XFER(II,T)=VAR(II)
      END DO
      END DO 
C
C
C   DESTINATION STATION LOOP
C
      DO 60 SC2=1,MAX_STATIONS
      DC=SC2+MAX_IZONES
      IF(IC.EQ.DC) GOTO 60
      IF(STADATA(SC2,6).NE.1.0) GOTO 60
      IF(STANUM(SC2).NE.IMODE) GO TO 60
      IF((INVEH(DC).LE.0.0.OR.INVEH(DC).GE.99999.9)) THEN
      STAUTL=0.0
      TD1UTL=0.0
      TD2UTL=0.0
      IF(SDETAIL) WRITE(135,9001) IC,STANAME(SC),DC,STANAME(SC2),
     *           NAME(IMODE)
 9001 FORMAT(' STATION 9001 (W) NO PATH FOUND FOR STATION=',I4,1X,A29,
     *       ' TO STATION=',I4,1X,A29,' IN ',A13,' STS PATH')
      ELSE
      KLINE=0.0
      IF(STADATA(SC,8).EQ.1.AND.STADATA(SC2,8).EQ.1) KLINE=KRED
      IF((STADATA(SC,8).EQ.2.AND.STADATA(SC2,8).EQ.2).OR.
     *   (STADATA(SC,8).EQ.2.AND.STADATA(SC2,8).EQ.1)) KLINE=KBLUE
      IF(STADATA(SC,8).EQ.3.AND.STADATA(SC2,8).EQ.3) KLINE=KGREEN
      IF(STADATA(SC,8).EQ.4.AND.STADATA(SC2,8).EQ.4) KLINE=KGOLD
      IF(STADATA(SC,8).EQ.5.AND.STADATA(SC2,8).EQ.5) KLINE=KEXPO
      IF(STADATA(SC,8).EQ.6.AND.STADATA(SC2,8).EQ.6) KLINE=KWSAB
      IF(STADATA(SC,8).EQ.7.AND.STADATA(SC2,8).EQ.7) KLINE=KESFV
      IF(STADATA(SC,8).EQ.8.AND.STADATA(SC2,8).EQ.8) KLINE=KSEPV
      IF(STADATA(SC,8).EQ.9.AND.STADATA(SC2,8).EQ.9) KLINE=KAIRCN
      IF(STADATA(SC,8).EQ.10.AND.STADATA(SC2,8).EQ.10) KLINE=KVERM
      KLINE=KLINE/(LSUM2UR*LSUM1TRN*LSUMR*LSUM3UP)
      FARECOST=FARE(DC)+STSFARE(SC,SC2)
C...CCR VALUES
      LUNRVAL=FLOATI(LUNRELM(DC))/100.0
      NUNRVAL=FLOATI(NUNRELM(DC))/100.0
      LCRDVAL=FLOATI(LCROWDM(DC))/100.0
      NCAPVAL=FLOATI(NCAPACM(DC))/100.0
C...COMMUTER RAIL
        if(imode.eq.1) then
        WAIT1A=AMIN1(WAIT1(DC),WAITLT)
        WAIT1B=DIM(WAIT1(DC),WAITLT)
        STAUTL=COEFF(10)*INVEH(DC) + COEFF(3)*WAIT1A +
     *       COEFF(4)*WAIT2(DC) + COEFF(5)*(TRANSF(DC)) +
     *       COEFF(7)*WALKTFR(DC) + COEFF(10)*WAIT1B +
     *       COEFF(75)*LUNRVAL/(LSUM2CR*LSUM1TRN*LSUMT*LSUM3CW) +
     *       COEFF(76)*NUNRVAL/(LSUM2CR*LSUM1TRN*LSUMT*LSUM3CW) +
     *       COEFF(77)*LCRDVAL/(LSUM2CR*LSUM1TRN*LSUMT*LSUM3CW) +
     *       COEFF(78)*NCAPVAL/(LSUM2CR*LSUM1TRN*LSUMT*LSUM3CW) 
C...URBAN RAIL
        elseif(imode.eq.2) then
        WAIT1A=AMIN1(WAIT1(DC),WAITLT)
        WAIT1B=DIM(WAIT1(DC),WAITLT)
        STAUTL=COEFF(1)*INVEH(DC) + COEFF(3)*WAIT1A +
     *       COEFF(4)*WAIT2(DC) + COEFF(80)*TRANSF(DC)+
     *       COEFF(7)*WALKTFR(DC)+ COEFF(1)*WAIT1B +
     *       KLINE +
     *       COEFF(75)*LUNRVAL/(LSUM2UR*LSUM1TRN*LSUMT*LSUM3UW) +
     *       COEFF(76)*NUNRVAL/(LSUM2UR*LSUM1TRN*LSUMT*LSUM3UW) +
     *       COEFF(77)*LCRDVAL/(LSUM2UR*LSUM1TRN*LSUMT*LSUM3UW) +
     *       COEFF(78)*NCAPVAL/(LSUM2UR*LSUM1TRN*LSUMT*LSUM3UW)
      IF(STADATA(SC,8).EQ.3.AND.STADATA(SC2,8).EQ.3.AND.
     *   INVEH(DC).LT.TLINE3) STAUTL=0.0
      IF(STADATA(SC,8).EQ.4.AND.STADATA(SC2,8).EQ.4.AND.
     *   INVEH(DC).LT.TLINE4) STAUTL=0.0
      IF(STADATA(SC,8).EQ.5.AND.STADATA(SC2,8).EQ.5.AND.
     *   INVEH(DC).LT.TLINE5) STAUTL=0.0
C...BRT
        elseif(imode.eq.5) then
        WAIT1A=AMIN1(WAIT1(DC),WAITLT)
        WAIT1B=DIM(WAIT1(DC),WAITLT)
        STAUTL=COEFF(1)*INVEH(DC) + COEFF(3)*WAIT1A +
     *       COEFF(4)*WAIT2(DC) + COEFF(5)*TRANSF(DC)+
     *       COEFF(7)*WALKTFR(DC)+ COEFF(1)*WAIT1B +
     *       COEFF(75)*LUNRVAL/(LSUM2BR*LSUM1TRN*LSUMT*LSUM3BRW) +
     *       COEFF(76)*NUNRVAL/(LSUM2BR*LSUM1TRN*LSUMT*LSUM3BRW) +
     *       COEFF(77)*LCRDVAL/(LSUM2BR*LSUM1TRN*LSUMT*LSUM3BRW) +
     *       COEFF(78)*NCAPVAL/(LSUM2BR*LSUM1TRN*LSUMT*LSUM3BRW)
         endif
C
C     COMPUTE NHB DIRECT DEMAND ACCESSIBILITY TERM
C
      IF(NHBDIR.AND.IMODE.EQ.2) THEN
      NHBACC(SC,SC2)=INVEH(DC) + 2.0*WAIT1(DC) +
     *       2.0*WAIT2(DC) + 2.0*(TRANSF(DC)) +
     *       0.04*FARECOST  + 2.0*WALKTFR(DC) + LUNRVAL + 2.0*NUNRVAL +
     *       LCRDVAL + 2.0*NCAPVAL
      NHBUTL(SC,SC2)=NHBCOEF(1)*INVEH(DC)+NHBCOEF(2)*FARECOST +
     *               NHBCOEF(3)*WAIT1A+
     *               NHBCOEF(4)*WAIT1B+NHBCOEF(5)*WAIT2(DC)+
     *               NHBCOEF(6)*TRANSF(DC)+NHBCOEF(7)*WALKTFR(DC)
      END IF
      END IF
C
C     STORE STATION-TO-STATION VALUES
C
      STASTA(1,SC,SC2)=STAUTL
      STASTA(2,SC,SC2)=TRANSF(DC)
      STASTA(3,SC,SC2)=INVEH(DC)
      STASTA(4,SC,SC2)=FARECOST
      STASTA(5,SC,SC2)=WAIT1(DC)+WAIT2(DC)
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
     *                TRANSF(DC),FARECOST,INVEH(DC),
     *                WALKTFR(DC),
     *                STAUTL,LUNRVAL,NUNRVAL,LCRDVAL,NCAPVAL
      IF(SDETAIL) THEN
      WRITE(61,9029)  IC,STANAME(SC),NAME(IMODE),DC,STANAME(SC2),
     *                WAIT1(DC),WAIT2(DC),
     *                TRANSF(DC),FARE(DC),STSFARE(SC,SC2),INVEH(DC),
     *                WALKTFR(DC),LUNRVAL,
     *                NUNRVAL,LCRDVAL,NCAPVAL,STAUTL
 9029 FORMAT(/1X,'STATION --> STATION UTILITY COMPUTATIONS'/
     *       1X,'----------------------------------------'//
     *       1X,'ORIGIN            STATION=',I8,1X,A37,
     *          ' FOR ',A13/
     *       1X,'DESTINATION       STATION=',I8,1X,A37/
     *       1X,'1ST WAIT             TIME=',F8.2/
     *       1X,'TRANSFER WAIT        TIME=',F8.2/
     *       1X,'NUMBER OF TRANSFERS      =',F8.2/
     *       1X,'INPUT MATRIX FARE        =',F8.2/
     *       1X,'STATION-TO-STATION FARE  =',F8.2/
     *       1X,'PRIMARY MODE IN-VEHICLE  =',F8.2/
     *       1X,'TRANSFER WALK TIME       =',F8.2//
     *       1X,'LINK UNRELIABILITY TIME  =',F8.2/
     *       1X,'STOP UNRELIABILITY TIME  =',F8.2/
     *       1X,'LINK CROWDING      TIME  =',F8.2/
     *       1X,'STOP CAPACITY      TIME  =',F8.2/
     *       1X,'UTILITY VALUE            =',F10.5/)
      TMPNAME=BLANK
      DO T=1,10
      IF(XFER(DC,T).GT.0) THEN
      XC=XFER(DC,T)-MAX_IZONES
      IF(XC.LE.MAX_STATIONS) TMPNAME(T)=STANAME(XC)
      END IF      
      END DO
      WRITE(61,9030) (XFER(DC,K),K=1,2),(TMPNAME(K1),K1=1,2),
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
