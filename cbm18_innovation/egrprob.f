C-------------------------------------------------------------------
C        STATION --> DESTINATION ZONE UTILITY COMPUTATION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE EGRPROB(STAZNE,IMODE,STAZNEI,INDAPM,STAEGR,STAZONE,
     *                   ZHHD)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'tpcom.inc'
       include 'control.inc'
       include 'mtamcpar.inc'
C
C DATA DECLARATIONS
C
      INTEGER*2     IMODE,SC,IC,JZ,T,IZ
      INTEGER*2     T1(5),T2(5),XEQUIV
      INTEGER*2     STAZNEI(MAX_STATIONS,MAX_IZONES,6,4)
      INTEGER*2     LBUS,RBUS,EBUS,TBUS,BBUS,UBUS
      INTEGER*2     INDAPM(MAX_ZONES,MAX_ZONES)
      INTEGER*4     STAZONE(MAX_STATIONS,MAX_IZONES,2)
      INTEGER*4     ZINDEX
      REAL*4        STAZNE(4,MAX_STATIONS,MAX_IZONES)
      REAL*4        STAEGR(4,MAX_STATIONS,MAX_IZONES)
      REAL*4        INVEH(MAX_ZONES),FARE(MAX_ZONES),
     *              WAIT2(MAX_ZONES),INVEHL(MAX_ZONES),
     *              INVEHR(MAX_ZONES),INVEHE(MAX_ZONES),
     *              INVEHT(MAX_ZONES),
     *              TRANSF(MAX_ZONES),WAIT1(MAX_ZONES)
      REAL*4        WALKEGR(MAX_ZONES)
      REAL*4        STAWALK(MAX_STATIONS,MAX_ZONES)
      REAL*4        WALKACC(MAX_ZONES),WALKTFR(MAX_ZONES)
      REAL*4        LUNRELM(MAX_ZONES,28),LUNRVAL,LUNRDEM
      REAL*4        NUNRELM(MAX_ZONES,28),NUNRVAL,TWAIT
      REAL*4        LCROWDM(MAX_ZONES,28),LCRDVAL
      REAL*4        NCAPACM(MAX_ZONES,28),NCAPVAL
      REAL*4        TINVEH,XDIST,KTWY,KRPD
      REAL*4        EWALK,EEBUS,EUBER,DENOM,ESHAR(4),LSEGR
      REAL*4        UTILWALK,BIKEUTIL,BUSUTIL
      REAL*4        KXEGR,ZHHD(34,MAX_IZONES)
      REAL*4        KWAIT,UBERUTIL,UBERCOST,UBERACC,HTIME,HDIST
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
      LUNRDEM=LSUM2BR*LSUM1TRN*LSUMT*LSUM3BRW
	    ENDIF
C
C READ & STORE DIRECT STATION WALK TIME
C
      IF(WALKTIME) THEN
      PURP=13
      DO IZ=1,MAX_IZONES
      call intab(fileno,VAR,IZ,PURP,dummy,io)
      DO SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      STAWALK(SC,IZ)=FLOAT(VAR(IC))/100.0
      END DO
      END DO    
      CLOSE(66,STATUS='KEEP')
      CALL PREPIO(BACCEGR,66)
      END IF
C 
C DESTINATION STATION LOOP 
C
      DO 100 SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      IF(STANUM(SC).NE.IMODE) GO TO 100
      IF(STADATA(SC,6).LE.0.0) GOTO 100
      XDIST=999.9
      KTWY=0.0
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
C DETERMINE CLOSEST ZONE FOR EACH STATION
C
      DO JZ=1,MAX_IZONES
      IF(STAWALK(SC,JZ).LT.XDIST.AND.STAWALK(SC,JZ).GT.0) THEN
      XEQUIV=JZ
      XDIST=STAWALK(SC,JZ)
      END IF
      END DO
C....................................................................
      IF(DEBUG.AND.SDETAIL) THEN
      WRITE(62,9031) IC,XEQUIV,XDIST
 9031 FORMAT(1X,'CLOSEST ZONE TO STATION'/
     *       1X,'-----------------------'/
     *       1X,'STATION     =',I6/
     *       1X,'CLOSEST ZONE=',I6/
     *       1X,'TIME        =',F6.2/)
      END IF
C.................................................................... 
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
C ***********************************************************************
C  WALK EGRESS
C ***********************************************************************
      KXEGR=0.0
      IF(STADATA(SC,8).EQ.5) KXEGR=-0.125
      KXEGR=KXEGR/LUNRDEM
      IF(STAWALK(SC,JZ).GT.0) THEN
      UTILWALK=COEFF(7)*STAWALK(SC,JZ)+KXEGR
      ELSE
      UTILWALK=0.0
      END IF
C....................................................................
      IF(DEBUG.AND.SDETAIL) THEN
      WRITE(62,9026) NAME(IMODE),IC,STANAME(SC),JZ,
     *               STAWALK(SC,JZ),UTILWALK
 9026 FORMAT(1X,'DESTINATION STATION --> EGRESS ZONE COMPUTATIONS',
     *       1X,'(',A13,')'/
     *       1X,'------------------------------------------------'//
     *       1X,'DESTINATION       STATION=',I10,1X,A37/
     *       1X,'EGRESS            ZONE   =',I10/
     *       1X,'DIRECT WALK       TIME   =',F10.2/
     *       1X,'DIRECT WALK    UTILITY   =',F10.5/)
      END IF
C....................................................................
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
C DESTINATION STATION --> EGRESS ZONE VALIDITY CHECKS
C
      IF(WALKEGR(JZ).LE.0.0.OR.TINVEH.LE.0.0) THEN
      STAZNE(1,SC,JZ)=-999.9
      BUSUTIL=0.0
      EEBUS=0.0
      GO TO 250
      END IF  
      IF((NOTWYAE).AND.INVEHT(JZ).GT.0) THEN
      STAZNE(1,SC,JZ)=-999.9
      BUSUTIL=0.0
      EEBUS=0.0
      GO TO 250
      END IF  
C
C COMPUTE EGRESS PORTION OF UTILITY
C
C....USING MODEL COEFFICIENTS
      KTWY=0.0
      KRPD=0.0
      IF(INVEHT(JZ).GT.0) KTWY=KTWYAE
      IF(INVEHR(JZ).GT.0) KRPD=KRPDAE
      STAZNE(1,SC,JZ)=COEFF(1)*TINVEH + COEFF(3)*WAIT1(JZ) +
     *       COEFF(4)*WAIT2(JZ) + COEFF(5)*(TRANSF(JZ)) +
     *       COEFF(7)*WALKTFR(JZ) +
     *       COEFF(75)* (LUNRVAL/LUNRDEM) +
     *       COEFF(76)* (NUNRVAL/LUNRDEM) +
     *       COEFF(77)* (LCRDVAL/LUNRDEM) +
     *       COEFF(78)* (NCAPVAL/LUNRDEM) +
     *       KTWY/LUNRDEM + KRPD/LUNRDEM
C.....STORE NUMBER OF TRANSFERS 
      STAZNE(2,SC,JZ)=TRANSF(JZ)
C.....STORE WALK TIME AT DESTINATION
      STAZNE(3,SC,JZ)=WALKEGR(JZ)
C.....STORE TRANSIT FARE
      STAZNE(4,SC,JZ)=FARE(JZ)
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
      BUSUTIL=STAZNE(1,SC,JZ)+COEFF(6)*FARE(JZ)+
     *        COEFF(7)*STAZNE(3,SC,JZ)
      IF(IMODE.EQ.5) THEN
      BUSUTIL=BUSUTIL + KBRTEGR/(LSUM1TRN*LSUM2BR*LSUM3BRW)
      END IF
      IF(STAZNE(1,SC,JZ).NE.0.0) THEN
      EEBUS=EXP(BUSUTIL)
      ELSE
      EEBUS=0.0
      END IF
C *************************************************************************
C     TNC
C *************************************************************************
  250 HTIME=FLOAT(STAZONE(SC,JZ,1))/100.0
      HDIST=FLOAT(STAZONE(SC,JZ,2))/100.0
      IF(UBERTRN) THEN
      CALL UBERCOMP(JZ,HTIME,HDIST,COEFF(3),COEFF(2),
     *              UBERCOST,UBERACC,KWAIT,ZHHD)
      UBERUTIL=UBERACC+(COEFF(6)*UBERCOST*100.0)
      IF(HTIME.LE.0) UBERUTIL=0.0
      IF(UBERUTIL.NE.0.0) THEN
      EUBER=EXP(UBERUTIL)
      ELSE
      EUBER=0.0
      END IF
C.....................................................................
      IF(DEBUG.AND.SDETAIL) THEN
      zindex=ifix(zhhd(29,xequiv))
      if(zindex.eq.0) zindex=6
      WRITE(62,9029) (SC+MAX_IZONES),STANAME(SC),JZ,ZINDEX,
     *               HDIST,HTIME,
     *               KWAIT,UBERCOST,UBERACC,UBERUTIL
 9029 FORMAT(/1X,'                  UBER EGRESS'/
     *       1X,'--------------------------------------------------'/
     *       1X,'STATION             =',I10,1X,A37/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'DENSITY CATEGORY    =',I10/
     *       1X,'STATION-TO-ZONE DIST=',F10.2/
     *       1X,'STATION-TO-ZONE TIME=',F10.2/
     *       1X,'UBER WAIT TIME      =',F10.2/
     *       1X,'UBER COST (DOLLARS) =',F10.2/
     *       1X,'UBER EGRESS UTILITY =',F10.5/
     *       1X,'UBER TOTAL  UTILITY =',F10.5/)
      END IF
C.....................................................................
      ELSE
      UBERUTIL=0.0
      EUBER=0.0
      END IF
C ************************************************************************
C...BICYCLE
C ************************************************************************
      BIKEUTIL=0.0
      EBIKE=0.0
      IF(UTILWALK.NE.0.0) THEN
      EWALK=EXP(UTILWALK)
      ELSE
      EWALK=0.0
      END IF
      DENOM=EWALK+EEBUS+EBIKE+EUBER
      ESHAR=0.0
      LSEGR=0.0
      IF(DENOM.GT.0.0) THEN
      ESHAR(1)=EWALK/DENOM
      ESHAR(3)=EBIKE/DENOM
      ESHAR(4)=EUBER/DENOM
      ESHAR(2)=1.0-ESHAR(1)-ESHAR(3)-ESHAR(4)
      LSEGR=LOG(DENOM)
      END IF
      STAEGR(1,SC,JZ)=ESHAR(1)
      STAEGR(2,SC,JZ)=ESHAR(2)
      STAEGR(3,SC,JZ)=LSEGR
      STAEGR(4,SC,JZ)=ESHAR(4)
C......................................................................
      IF(DEBUG.AND.SDETAIL) THEN
      WRITE(62,9030) UTILWALK,EWALK,BUSUTIL,EEBUS,BIKEUTIL,
     *               EBIKE,UBERUTIL,EUBER,ESHAR,LSEGR
 9030 FORMAT(/1X,'SUMMARY OF EGRESS UTILITIES & PROBABILITIES'/
     *       1X, '-------------------------------------------'/
     *       1X,'                     ',5X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',2X,'----------',5X,
     *          '----------'/      
     *       1X,'WALK EGRESS            ',F10.5,3X,E12.5/
     *       1X,'BUS  EGRESS            ',F10.5,3X,E12.5/
     *       1X,'BIKE EGRESS            ',F10.5,3X,E12.5/
     *       1X,'UBER EGRESS            ',F10.5,3X,E12.5//
     *       1X,'WALK SHARE             ',F10.5/
     *       1X,'BUS  SHARE             ',F10.5/
     *       1X,'BIKE SHARE             ',F10.5/
     *       1X,'UBER SHARE             ',F10.5//
     *       1X,'EGRESS LOGSUM          ',F10.5/)      
      END IF
C......................................................................
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
