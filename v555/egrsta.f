C-------------------------------------------------------------------
C        EGRESS STATION --> DESTINATION ZONE SELECTION SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE EGRSTA(JZ,STA,STASTA,STAZNE,DSTA,IMODE,
     *                  ZONESTA,STAZONE)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'mtamcpar.inc'
      integer*2     jz,imode,SMODE,SJZ
      INTEGER*2     ZONESTA(MAX_IZONES)
      INTEGER*2     STA,DS,DSTA,ic,dc,sic,sdc
      INTEGER*4     STAZONE(MAX_STATIONS,MAX_IZONES,2)
      REAL*4        STASTA(4,MAX_STATIONS,MAX_STATIONS),
     *              STAZNE(4,MAX_STATIONS,MAX_IZONES),
     *              DDIST,UTIL,UTIL2,UTIL3
      REAL*4        WAIT1,WAIT2,
     *              TRANSF,FARE,WALKACC,INVEH,
     *              TIVT,WALKEGR,WALKTFR,
     *              SELUTL,STAUTL  
      REAL*4        INVEHL,INVEHR,INVEHE,INVEHT,INVEHB,INVEHU
      REAL*4        LHIVTJ,INVEHJ,WAIT1J,WAIT2J,
     *               TRANSFJ,FAREJ,WALKACCJ,
     *               WALKEGRJ,WALKTFRJ,SELUTLJ,STAUTLJ
      REAL*4        DISTEGR
      CHARACTER*13  NAME(2)
      DATA          NAME/'Commuter Rail',
     *                   'Urban Rail   '/
      DDIST=99999.9
      DSTA=0
      IC=STA-MAX_IZONES
      IF(IC.LT.0) RETURN
C
C
C DESTINATION STATION LOOP
C
      DO 100 DC=1,MAX_STATIONS
      DS=DC+MAX_IZONES
      IF(DC.EQ.IC) GOTO 100
      IF(STANUM(DC).NE.IMODE) GOTO 100
      IF(STADATA(DC,6).LE.0.0) GOTO 100
C
C COMPUTE SIMPLIFIED UTILITY EXPRESSION FOR COMPARISON
C
      UTIL=STASTA(1,IC,DC)+STAZNE(1,DC,JZ)
C.............................................................
      IF(SDETAIL) THEN
      WRITE(26,9001) DS,STANAME(DC),STAIND(DC,JZ),UTIL,STASTA(1,IC,DC),
     *               STAZNE(1,DC,JZ),DDIST,DSTA
 9001 FORMAT(' STATION=',I4,1X,A37,' STAIND=',I1,
     *       ' UTIL=',F8.1,
     *       ' STA->STA=',F8.1,' STA->ZNE=',F8.1,
     *       ' CURRENT BEST=',F8.1,' FOR STATION=',I4)
      END IF
C..............................................................
      IF(STAIND(DC,JZ).EQ.3) UTIL=STASTA(1,IC,DC)+99999.9
      IF(UTIL.LT.DDIST) THEN
      DDIST=UTIL
      DSTA=DS
      END IF
 100  continue
      DC=DSTA-MAX_IZONES
      IF(DC.LT.0) DC=MAX_STATIONS
C
C IS SELECTED STATION WITHIN WALKING DISTANCE
C OR WAS A BUS EGRESS PATH POSSIBLE ?
C
      IF(IMODE.EQ.1.AND.(STACAR)) THEN
       DC=DSTA-MAX_IZONES
       IF(DC.LT.0) DC=MAX_STATIONS
       IF(STAIND(DC,JZ).EQ.1.OR.STAIND(DC,JZ).EQ.2) GO TO 150
C
C CONSIDER DRIVE EGRESS IF NOT WALK DIRECT OR BUS
C
      DDIST=99999.9
      DSTA=0
      DO 125 DC=1,MAX_STATIONS
      DS=DC+MAX_IZONES
      IF(DC.EQ.IC) GOTO 125
      IF(STANUM(DC).NE.IMODE) GOTO 125
      IF(STADATA(DC,6).LE.0.0) GOTO 125
      IF(STADATA(DC,7).LT.1) GO TO 125
      IF(STAZONE(DC,JZ,1).LE.0) GO TO 125
      DISTEGR=FLOAT(STAZONE(DC,JZ,2))/100.0
      IF(DISTEGR.GT.EGRDIST) GO TO 125
C
C COMPUTE SIMPLIFIED UTILITY EXPRESSION FOR COMPARISON
C
      UTIL=STASTA(1,IC,DC)+2.0*FLOAT(STAZONE(DC,JZ,1))/100.0
      IF(UTIL.LT.DDIST) THEN
      DDIST=UTIL
      DSTA=DS
      END IF
 125  CONTINUE
      IF(DSTA.LE.0) GO TO 150
      DC=DSTA-MAX_IZONES
      STAZNE(1,DC,JZ)=2.0*FLOAT(STAZONE(DC,JZ,1))/100.0
      STAZNE(2,DC,JZ)=COEFF(9)*FLOAT(STAZONE(DC,JZ,1))/100.0
      STAZNE(3,DC,JZ)=0.0
      STAZNE(4,DC,JZ)=0.0
      STAIND(DC,JZ)=3
      END IF
  150 CONTINUE
C....................................................................
      IF(DEBUG.AND.(JOI(JZ))) THEN
      DC=DSTA-MAX_IZONES
      IF(DC.GT.0) THEN
C 
C OBTAIN STATION-TO-STATION DETAILS 
C
      REWIND 31
  200 READ(31,END=210) SMODE,SIC,SDC,WAIT1,WAIT2,
     *                TRANSF,FARE,WALKACC,INVEH,
     *                TIVT,WALKEGR,WALKTFR,
     *                SELUTL,STAUTL
      IF(SMODE.EQ.IMODE.AND.STA.EQ.SIC.AND.DSTA.EQ.SDC) GO TO 250
      GO TO 200
  210 WRITE(26,9027) IMODE,STA,DSTA
 9027 FORMAT(//' NO STA-STA MATCH FOR IMODE=',I1,' STA=',I4,
     *         ' DSTA=',I4/)
      STOP 16
  250 CONTINUE
      WRITE(26,9029) SIC,SDC,WAIT1,WAIT2,
     *                TRANSF,FARE,WALKACC,INVEH,
     *                TIVT,WALKEGR,WALKTFR,
     *                SELUTL,STAUTL
 9029 FORMAT(/1X,'STATION --> STATION UTILITY COMPUTATIONS'/
     *       1X,'----------------------------------------'//
     *       1X,'ORIGIN            STATION=',I8/
     *       1X,'DESTINATION       STATION=',I8/
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
C
C OBTAIN STATION-TO-ZONE DETAILS
C
      IF(STAIND(DC,JZ).LT.3) THEN
      REWIND 33
  300 READ(33,END=310) SMODE,SIC,SJZ,LHIVTJ,INVEHJ,WAIT1J,WAIT2J,
     *               TRANSFJ,FAREJ,WALKACCJ,
     *               WALKEGRJ,
     *               WALKTFRJ,
     *               SELUTLJ,STAUTLJ,
     *               INVEHL,INVEHR,INVEHE,INVEHT,INVEHB,INVEHU
      IF(SMODE.EQ.IMODE.AND.DSTA.EQ.SIC.AND.SJZ.EQ.JZ) GO TO 350
      GO TO 300
  310 WRITE(26,9028) IMODE,DSTA,JZ
 9028 FORMAT(//' NO STA-ZNE MATCH FOR IMODE=',I1,' DSTA=',I4,
     *         ' JZ=',I4/)
      STOP 16
  350 CONTINUE                                        
      WRITE(26,9025) SIC,SJZ,LHIVTJ,INVEHJ,
     *               INVEHL,INVEHR,INVEHE,INVEHT,INVEHB,INVEHU,
     *               WAIT1J,WAIT2J,
     *               TRANSFJ,FAREJ,WALKACCJ,
     *               WALKEGRJ,
     *               WALKTFRJ,
     *               SELUTLJ,STAUTLJ
 9025 FORMAT(1X,'DESTINATION STATION --> EGRESS ZONE COMPUTATIONS'/
     *       1X,'------------------------------------------------'//
     *       1X,'DESTINATION       STATION=',I8/
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
     *       1X,'SELECTION UTILITY VALUE  =',F10.5/
     *       1X,'UTILITY VALUE            =',F10.5/)
      ELSE
      WRITE(26,9030) DSTA,STAZONE(DC,JZ,1)
 9030 FORMAT(1X,'DESTINATION STATION --> EGRESS ZONE COMPUTATIONS'/
     *       1X,'------------------------------------------------'//
     *       1X,'DESTINATION       STATION=',I8/
     *       1X,'HIGHWAY TRAVEL      TIME =',I8)
      END IF      
      ELSE                                                              
      WRITE(26,9026) NAME(IMODE),STA,JZ,DSTA                                        
 9026 FORMAT(/1X,'EGRESS STATION SELECTION -- ',A13/                            
     *       1X,'------------------------'/                             
     *       1X,'ACCESS STATION    =',I10/                              
     *       1X,'DESTINATION ZONE  =',I10/                              
     *       1X,'DEST   STATION    =',I10)                              
      ENDIF                                                             
      ENDIF
C.....................................................................
      RETURN
      END
