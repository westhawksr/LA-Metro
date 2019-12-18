C-------------------------------------------------------------------
C       DRIVE --> EXPRESS BUS/TRANSITWAY UTILITY COMPUTATION
C-------------------------------------------------------------------
       SUBROUTINE DRVNEW(JZ,UTIL,ADIST,ATIME,CSTA,CDSTA,IMODE,STAZNE)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
	     INCLUDE 'tpcom.inc'
	     include 'control.inc'
	     include 'mtamcpar.inc'
C
C DATA DECLARATIONS
C
      INTEGER*2    IMODE,JZ,CSTA,ISTA,ic,SMODE,SIC,SDC,SJZ
      INTEGER*2    DC,CDSTA,dsta,IC2,ISTA2
      REAL*4       UTIL,ADIST(MAX_ZONES),ATIME(MAX_ZONES),CDIST,
     *             STAUTL,STAZNE(4,MAX_STATIONS,MAX_IZONES)
      REAL*4       INVEHL,INVEHE,INVEHT,
     *             WAIT1,TRWAIT,TRANSF,FARE5,
     *             INVEH2,WALKACC,WALKEGR,
     *             WALKTFR,SELUTL,STAUTL1
      REAL*4       INVEH2J,INVEHLJ,INVEHEJ,INVEHTJ,
     *             INVEHRJ,WAIT1J,TRWAITJ,
     *             TRANSFJ,FARE5J,WALKEGRJ,
     *             WALKTFRJ,WALKACCJ,
     *             STAZNE1,DAR
      REAL*4       LUNRVAL,NUNRVAL,LCRDVAL,NCAPVAL
      CHARACTER*13  NAME(3)
      LOGICAL      EXT
      DATA          NAME/'Express Bus  ',
     *                   'Transitway   ',
     *                   'Bus Rapid Trn'/
C
C USE HIGHWAY TIME TO DETERMINE DRIVE TIME
C
C
C   DO BEST STATION SELECTION
C
      UTIL=0.0
      STAUTL=0.0
      CDIST=-99999.9
      CSTA=MAX_ZONES
      CDSTA=MAX_ZONES
C
      DO 40,IC=1,MAX_STATIONS
	    ISTA=IC+MAX_IZONES
	    IF(STANUM(IC).NE.IMODE) GOTO 40
      if(stadata(ic,6).le.0.0) goto 40
      IF((ADIST(ISTA).LE.0.0).OR.(ADIST(ISTA).GE.99999.9)) GO TO 45
      IF((ADIST(ISTA).GT.EXPACCD).AND.IMODE.EQ.3) GO TO 45
      IF((ADIST(ISTA).GT.TWYACCD).AND.IMODE.EQ.4) GO TO 45
      IF(STAZNE(1,IC,JZ).EQ.0.0) GO TO 45
      STAUTL=COEFF(16)*ATIME(ISTA)+ STAZNE(1,IC,JZ)
C.............................................................
      IF(SDETAIL) THEN
      WRITE(26,9001) ISTA,STANAME(IC),ADIST(ISTA),ATIME(ISTA),
     *               STAZNE(1,IC,JZ),STAUTL,CDIST,CSTA
 9001 FORMAT(' STATION=',I4,1X,A37,' ACCESS DIST=',F8.2,
     *       ' ACCESS TIME=',F8.2,
     *       ' STA->ZNE UTIL=',F8.4,' UTIL=',F8.4,
     *       ' CURRENT BEST=',F8.4,' FOR STATION=',I4)
      END IF
C..............................................................
      IF(STAUTL.GT.CDIST) THEN
      CDIST=STAUTL
      CSTA=ISTA
      ENDIF
  45  CONTINUE
  40  CONTINUE
C
      IF(STANUM((CSTA-MAX_IZONES)).EQ.IMODE) THEN
C
C
C   COMPUTE MODE CHOICE MODEL UTILITY VALUE - ADD AUTO TIME/COST
C
      IC=CSTA-MAX_IZONES
	    DAR=0.0
	    IF(ADIST(JZ).GT.0) DAR=(ADIST(CSTA)/ADIST(JZ))-0.5
	    DAR=AMAX1(DAR,0.0)
      UTIL=COEFF(16)*ATIME(CSTA)+opcost*COEFF(14)*ADIST(CSTA)+
     *      STAZNE(1,IC,JZ)+CDAR*DAR+COEFF(17)*STADATA(IC,11)+
     *      COEFF(11)*STADATA(IC,12)
C
C....................................................................
      IF(DEBUG) THEN
      IF(CSTA.NE.MAX_ZONES) THEN
      WRITE(26,9220) NAME(IMODE-2)
 9220 FORMAT(//1X,'DRIVE ACCESS TO ',A13/
     *         1X,'==========================================='/)
C
C OBTAIN STATION-TO-ZONE DETAILS
C
      REWIND 34
  300 READ(34,END=310) SMODE,SIC,SJZ,INVEH2J,
     *               INVEHLJ,INVEHEJ,INVEHTJ,
     *               INVEHRJ,WAIT1J,TRWAITJ,
     *               TRANSFJ,FARE5J,WALKEGRJ,
     *               WALKTFRJ,WALKACCJ,
     *               STAZNE1,LUNRVAL,
     *               NUNRVAL,LCRDVAL,NCAPVAL
      IF(SMODE.EQ.IMODE.AND.CSTA.EQ.SIC.AND.SJZ.EQ.JZ) GO TO 350
      GO TO 300
  310 WRITE(26,9028) IMODE,CSTA,JZ
 9028 FORMAT(//' NO MATCH FOR IMODE=',I1,' CSTA=',I4,' JZ=',I4/)
      RETURN
  350 CONTINUE                                        
      WRITE(26,9025) SIC,SJZ,INVEH2J,
     *               INVEHLJ,INVEHEJ,INVEHTJ,
     *               INVEHRJ,WAIT1J,TRWAITJ,
     *               TRANSFJ,FARE5J,WALKEGRJ,
     *               WALKTFRJ,WALKACCJ,
     *               LUNRVAL,NUNRVAL,LCRDVAL,NCAPVAL,
     *               STAZNE1
 9025 FORMAT(1X,'ACCESS STATION --> EGRESS ZONE COMPUTATIONS'/
     *       1X,'------------------------------------------------'//
     *       1X,'ACCESS            STATION=',I8/
     *       1X,'EGRESS            ZONE   =',I8/
     *       1X,'IN-VEHICLE           TIME=',F8.2/
     *       1X,'LOCAL   BUS IN-VEHICLE   =',F8.2/
     *       1X,'EXPRESS BUS IN-VEHICLE   =',F8.2/
     *       1X,'TRANSITWAY  IN-VEHICLE   =',F8.2/
     *       1X,'RAPID   BUS IN-VEHICLE   =',F8.2/
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
     *       1X,'CAPACITY             TIME=',F8.2//
     *       1X,'UTILITY VALUE            =',F10.5/) 
      IF(IC.LT.0) IC=MAX_STATIONS
      IF(DC.LT.0) DC=MAX_STATIONS
      WRITE(26,9047) NAME(IMODE-2),CSTA,STANAME(IC),ATIME(csta),
     *  ADIST(csta),ADIST(JZ),DAR,
     * 	STAZNE(1,IC,JZ),STADATA(IC,11),STADATA(IC,12),
     *  UTIL
 9047 FORMAT(//1X,' Drive Access Station LOS Data -- ',A13/
     *       1X,'------------------------------------------------'/
     *       1X,'ACCESS STATION NUMBER     =',I10,5X,A37/
     *       1X,'HIGHWAY TIME              =',F10.2/
     *       1X,'HIGHWAY DISTANCE (STA)    =',F10.2/
     *       1X,'HIGHWAY DISTANCE (JZ)     =',F10.2/
     *       1X,'DRIVE ACCESS RATIO        =',F10.2//
     *       1X,'STATION-DESTINATION UTIL  =',F10.4/
     *       1X,'STATION WALK ACCESS TIME  =',F10.4/
     *       1X,'STATION IVT  ACCESS TIME  =',F10.4/
     *       1X,'MODE CHOICE MODEL UTILITY =',F10.4/)
      END IF
      END IF
      ELSE
      IF(DEBUG) THEN
      WRITE(26,9220) NAME(IMODE-2)
      WRITE(26,9221)
 9221 FORMAT(' ***** NO PATH AVAILABLE ******')
      END IF
      END IF
C -----------------------------------------------------------------
	RETURN
	END
