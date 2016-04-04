C-------------------------------------------------------------------
C       DRIVE --> EXPRESS BUS/TRANSITWAY UTILITY COMPUTATION
C-------------------------------------------------------------------
       SUBROUTINE DRVNEW(JZ,UTIL,ADIST,ATIME,CSTA,CDSTA,IMODE,STAZNE,
     *            EXPZNE,EXPCR,UTILCR,CCSTA,CCDSTA,EXPTXF)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
	     INCLUDE 'tpcom.inc'
	     include 'control.inc'
	     include 'mtamcpar.inc'
C
C DATA DECLARATIONS
C
      INTEGER*2    IMODE,JZ,CSTA,ISTA,ic,SMODE,SIC,SDC,SJZ
      INTEGER*2    DC,CDSTA,dsta,IC2,ISTA2,CCSTA,CCDSTA
      INTEGER*2    EXPTXF(MAX_STATIONS,MAX_IZONES)
      REAL*4       UTIL,ADIST(MAX_ZONES),ATIME(MAX_ZONES),CDIST,
     *             STAUTL,STAZNE(4,MAX_STATIONS,MAX_IZONES)
      REAL*4       EXPZNE(4,MAX_STATIONS,MAX_STATIONS)
      REAL*4       CCDIST,UTILCR
      REAL*4       INVEHL,INVEHE,INVEHT,
     *             WAIT1,TRWAIT,TRANSF,FARE5,
     *             INVEH2,WALKACC,WALKEGR,
     *             WALKTFR,SELUTL,STAUTL1
      REAL*4       INVEH2J,INVEHLJ,INVEHEJ,INVEHTJ,
     *             INVEHRJ,WAIT1J,TRWAITJ,
     *             TRANSFJ,FARE5J,WALKEGRJ,
     *             WALKTFRJ,WALKACCJ,
     *             STAZNE1,STAZNE2,DAR
      REAL*4       LUNRVAL,NUNRVAL,LCRDVAL,NCAPVAL
      CHARACTER*13  NAME(3)
      LOGICAL      EXT,EXPCR
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
      CCDIST=-99999.9
      CSTA=MAX_ZONES
      CCSTA=MAX_ZONES
      CDSTA=MAX_ZONES
      CCDSTA=MAX_ZONES
      EXPCR=.FALSE.
C
      DO 40,IC=1,MAX_STATIONS
	    ISTA=IC+MAX_IZONES
	    IF(STANUM(IC).NE.IMODE) GOTO 40
      if(stadata(ic,6).le.0.0) goto 40
      IF((ADIST(ISTA).LE.0.0).OR.(ADIST(ISTA).GE.99999.9)) GO TO 45
      IF((ADIST(ISTA).GT.EXPACCD).AND.IMODE.EQ.3) GO TO 45
      IF((ADIST(ISTA).GT.TWYACCD).AND.IMODE.EQ.4) GO TO 45
      IF((ADIST(ISTA).GT.BRTACCD).AND.IMODE.EQ.5) GO TO 45
      IF(STAZNE(1,IC,JZ).EQ.0.0) GO TO 45
C---------------------------------------------------------------------
C   CHECK FOR BRT TO URBAN RAIL ?
      IF(IMODE.EQ.5) THEN
C...SEARCH FOR PATH TO AN URBAN RAIL STATION
      DO 145 IC2=1,MAX_STATIONS
      IF(STANUM(IC2).NE.2) GO TO 145
      IF(EXPZNE(1,IC,IC2).EQ.0) GO TO 145
      STAUTL=2.0*ATIME(ISTA)+EXPZNE(1,IC,IC2)
      IF(STAUTL.GT.CCDIST) THEN
      CCDIST=STAUTL
      CCSTA=ISTA
      CCDSTA=IC2+MAX_IZONES
      EXPCR=.TRUE.
      ENDIF
  145 CONTINUE 
      END IF
C----------------------------------------------------------------------
      STAUTL=COEFF(11)*ATIME(ISTA)+ STAZNE(1,IC,JZ)
C.............................................................
      IF(SDETAIL) THEN
      WRITE(26,9001) ISTA,STANAME(IC),ATIME(ISTA),STAZNE(1,IC,JZ),
     *               STAUTL,CDIST,CSTA
 9001 FORMAT(' STATION=',I4,1X,A37,' ACCESS TIME=',F8.2,
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
C  FINAL SELECTION
C
      IF(EXPCR) THEN
C..BRT TO URBAN RAIL
      IC=CCSTA-MAX_IZONES
      DC=CCDSTA-MAX_IZONES
      UTILCR=COEFF(1)*ATIME(CCSTA)+EXPZNE(2,IC,DC)+
     *      COEFF(7)*STADATA(IC,11)+
     *      COEFF(1)*STADATA(IC,12)  +
     *      COEFF(30+IMODE*3)*EXPTXF(IC,JZ)
C..................................................................
      IF(DEBUG) THEN
      IF(IMODE.EQ.5) THEN
      WRITE(26,9222) NAME(IMODE-2)
 9222 FORMAT(//1X,'DRIVE ACCESS TO ',A13,' To Urban Rail Option'/
     *         1X,'==========================================='/)
      REWIND 32
  260 READ(32,END=270) SIC,SDC,INVEHB,WAIT1,WAIT2,TRANSF,
     *                 FARE,WALKEGR,WALKTFR
      IF(CCSTA.EQ.SIC.AND.CCDSTA.EQ.SDC) GO TO 400
      GO TO 260
  270 WRITE(26,9029) CCSTA,CCDSTA
 9029 FORMAT(//' (RAILPATH) NO MATCH FOR BRT STA=',I4,
     *         ' UR ACCESS STA=',I4/)
      STOP 16
  400 WRITE(26,7004) JZ,CCSTA,STANAME(IC),CCDSTA,STANAME(DC),
     *               WAIT1,WAIT2,TRANSF,WALKEGR,WALKTFR,
     *               FARE,INVEHB,UTILCR
 7004 FORMAT(1X,'DESTINATION ZONE         =',I8/
     *       1X,'BRT        STATION       =',I8,5X,A29/
     *       1X,'URBAN RAIL STATION       =',I8,5X,A29/
     *       1X,'1ST WAIT TIME            =',F8.2/
     *       1X,'TRANSFER WAIT TIME       =',F8.2/
     *       1X,'NO OF TRANSFERS          =',F8.2/
     *       1X,'WALK EGRESS TIME         =',F8.2/
     *       1X,'WALK TRANSFER TIME       =',F8.2/
     *       1X,'FARE                     =',F8.2/
     *       1X,'BRT IN-VEHICLE TIME      =',F8.2//
     *       1X,'PARTIAL UTILITY          =',F10.5/)
      END IF
      END IF
C..................................................................
      END IF
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
      UTIL=COEFF(11)*ATIME(CSTA)+opcost*COEFF(16)*ADIST(CSTA)+
     *      STAZNE(2,IC,JZ)+CDAR*DAR+COEFF(17)*STADATA(IC,11)+
     *      COEFF(11)*STADATA(IC,12) +
     *      COEFF(30+IMODE*3)*EXPTXF(IC,JZ)
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
     *               STAZNE1,STAZNE2,LUNRVAL,
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
     *               STAZNE2
 9025 FORMAT(1X,'ACCESS STATION --> EGRESS ZONE COMPUTATIONS'/
     *       1X,'------------------------------------------------'//
     *       1X,'ACCESS            STATION=',I8/
     *       1X,'EGRESS            ZONE   =',I8/
     *       1X,'IN-VEHICLE           TIME=',F8.2/
     *       1X,'LOCAL   BUS IN-VEHICLE   =',F8.2/
     *       1X,'EXPRESS BUS IN-VEHICLE   =',F8.2/
     *       1X,'TWY OR BRT  IN-VEHICLE   =',F8.2/
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
     * 	STAZNE(2,IC,JZ),STADATA(IC,11),STADATA(IC,12),
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
