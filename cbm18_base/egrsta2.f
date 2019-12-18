C-------------------------------------------------------------------
C        EGRESS STATION --> DESTINATION ZONE SELECTION SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE EGRSTA2(JZ,STA,STASTA,STAZNE,DSTA,IMODE,STAZONE,
     *                   STAEGR)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'mtamcpar.inc'
      integer*2     jz,imode,SMODE,SJZ
      INTEGER*2     STA,DS,DSTA,ic,dc,sic,sdc
      INTEGER*4     STAZONE(MAX_STATIONS,MAX_IZONES,2)
      REAL*4        STASTA(5,MAX_STATIONS,MAX_STATIONS),
     *              STAZNE(4,MAX_STATIONS,MAX_IZONES),
     *              DDIST,UTIL,UTIL2,UTIL3
      REAL*4        STAEGR(3,MAX_STATIONS,MAX_IZONES)
      REAL*4        WAIT1,WAIT2,
     *              TRANSF,FARE,WALKACC,INVEH,
     *              TIVT,WALKEGR,WALKTFR,
     *              SELUTL,STAUTL  
      REAL*4        INVEHL,INVEHR,INVEHE,INVEHT,INVEHB,INVEHU
      REAL*4        LHIVTJ,INVEHJ,WAIT1J,WAIT2J,
     *               TRANSFJ,FAREJ,WALKACCJ,
     *               WALKEGRJ,WALKTFRJ,SELUTLJ,STAUTLJ
      REAL*4        DISTEGR
      REAL*4        LUNRVAL,NUNRVAL,LCRDVAL,NCAPVAL
      REAL*4        LUNRVALJ,NUNRVALJ,LCRDVALJ,NCAPVALJ
      REAL*4        EGRUTIL
      CHARACTER*13  NAME(6)
      DATA          NAME/'Commuter Rail',
     *                   'Urban Rail   ',
     *                   'Express Bus  ',
     *                   'Transitway   ',
     *                   'BRT          ',
     *                   'CR/UR/BRT    '/
      DDIST=-99999.9
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
      IF(STANUM(DC).EQ.IMODE) GOTO 100
      IF(STANUM(DC).EQ.3.OR.STANUM(DC).EQ.4) GOTO 100
      IF(STADATA(DC,6).LE.0.0) GOTO 100
      IF(.NOT.TRNEGR) THEN
      IF(STASTA(1,IC,DC).EQ.0.OR.
     *   (STAZNE(1,DC,JZ).EQ.0.AND.STAZNE(3,DC,JZ).LE.0).OR.
     *   (STAZNE(1,DC,JZ).LE.-999.9)) GO TO 100
      ELSE
      IF(STASTA(1,IC,DC).EQ.0.OR.STAEGR(3,DC,JZ).EQ.0.0) GO TO 100      
      END IF
C
C COMPUTE UTILITY EXPRESSION FOR COMPARISON
C
      EGRUTIL=COEFF(7)*STAZNE(3,DC,JZ)
      UTIL=STASTA(1,IC,DC)+STAZNE(1,DC,JZ)+EGRUTIL
      IF(TRNEGR) UTIL=STASTA(1,IC,DC)+STAEGR(3,DC,JZ)
C.............................................................
      IF(SDETAIL) THEN
      IF(.NOT.TRNEGR) THEN
      WRITE(26,9001) DS,STANAME(DC),STAIND(DC,JZ),UTIL,STASTA(1,IC,DC),
     *               STAZNE(1,DC,JZ),EGRUTIL,DDIST,DSTA
 9001 FORMAT(' STATION=',I4,1X,A37,' STAIND=',I1,
     *       ' UTIL=',F8.1,
     *       ' STA->STA=',F8.1,' STA->ZNE=',F8.1,
     *       ' WALK EGRESS=',F8.2,
     *       ' CURRENT BEST=',F8.1,' FOR STATION=',I4)
      ELSE
      WRITE(26,9003) DS,STANAME(DC),UTIL,
     *               STASTA(2,IC,DC),
     *               STAEGR(3,DC,JZ),DDIST,DSTA  
 9003 FORMAT(' STATION=',I4,1X,A37,' UTIL=',F8.1,
     *       ' STA->STA=',F8.1,' STA->ZNE=',F8.1,
     *       ' CURRENT BEST=',F8.1,' FOR STATION=',I4)   
      END IF
      END IF
C..............................................................
      IF(UTIL.GT.DDIST) THEN
      DDIST=UTIL
      DSTA=DS
      END IF
 100  continue
      DC=DSTA-MAX_IZONES
      IF(DC.LT.0) DC=MAX_STATIONS
C
C IS SELECTED STATION WITHIN WALKING DISTANCE
C OR WAS A BUS EGRESS PATH POSSIBLE ?
C IF NOT --  CONSIDER STATION CAR
C
      IF(IMODE.EQ.1.AND.(STACAR)) THEN
       DC=DSTA-MAX_IZONES
       IF(DC.LT.0) DC=MAX_STATIONS
       IF((STAIND(DC,JZ).EQ.1.OR.STAIND(DC,JZ).EQ.2).AND.
     *   (.NOT.TRNEGR)) GO TO 150
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
      UTIL=STASTA(1,IC,DC)+COEFF(9)*FLOAT(STAZONE(DC,JZ,1))/100.0
      IF(UTIL.LT.DDIST) THEN
      DDIST=UTIL
      DSTA=DS
      END IF
 125  CONTINUE
      IF(DSTA.LE.0) GO TO 150
      DC=DSTA-MAX_IZONES
      STAZNE(1,DC,JZ)=COEFF(9)*FLOAT(STAZONE(DC,JZ,1))/100.0
      STAZNE(2,DC,JZ)=0.0
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
     *                TRANSF,FARE,INVEH,WALKTFR,
     *                STAUTL,LUNRVAL,NUNRVAL,LCRDVAL,NCAPVAL
      IF(SMODE.EQ.6.AND.STA.EQ.SIC.AND.DSTA.EQ.SDC) GO TO 250
      GO TO 200
  210 WRITE(26,9027) IMODE,STA,DSTA
 9027 FORMAT(//' NO STA-STA MATCH FOR IMODE=',I1,' STA=',I4,
     *         ' DSTA=',I4/)
      STOP 16
  250 CONTINUE
      WRITE(26,9029) SIC,SDC,WAIT1,WAIT2,
     *                TRANSF,FARE,INVEH,WALKTFR,
     *                LUNRVAL,NUNRVAL,LCRDVAL,NCAPVAL,STAUTL
 9029 FORMAT(/1X,'****************************'/
     *       1X,'* MULTIPLE PATH EVALUATION *'/
     *       1X,'****************************'//
     *       1X,'STATION --> STATION UTILITY COMPUTATIONS'/
     *       1X,'----------------------------------------'//
     *       1X,'ORIGIN            STATION=',I8/
     *       1X,'DESTINATION       STATION=',I8/
     *       1X,'1ST WAIT             TIME=',F8.2/
     *       1X,'TRANSFER WAIT        TIME=',F8.2/
     *       1X,'NUMBER OF TRANSFERS      =',F8.2/
     *       1X,'FARE                     =',F8.2/
     *       1X,'TOTAL IN-VEHICLE TIME    =',F8.2/
     *       1X,'TRANSFER WALK TIME       =',F8.2//
     *       1X,'LINK UNRELIABILITY TIME  =',F8.2/
     *       1X,'STOP UNRELIABILITY TIME  =',F8.2/
     *       1X,'LINK CROWDING      TIME  =',F8.2/
     *       1X,'STOP CAPACITY      TIME  =',F8.2/
     *       1X,'UTILITY VALUE            =',F10.5/)
C
C OBTAIN STATION-TO-ZONE DETAILS
C
      IF(.NOT.TRNEGR) THEN
      IF(STAIND(DC,JZ).EQ.1) THEN
      WRITE(26,9030) (DC+MAX_IZONES),JZ,
     *               STAZNE(3,DC,JZ)
 9030 FORMAT(1X,'DESTINATION STATION --> EGRESS ZONE COMPUTATIONS'/
     *       1X,'------------------------------------------------'//
     *       1X,'DESTINATION       STATION=',I8/
     *       1X,'EGRESS            ZONE   =',I8/
     *       1X,'DESTINATION WALK  TIME   =',F8.2/)
      RETURN
      END IF
      REWIND 33
  300 READ(33,END=310) SMODE,SIC,SJZ,INVEHJ,WAIT1J,WAIT2J,
     *               TRANSFJ,FAREJ,WALKACCJ,
     *               WALKEGRJ,WALKTFRJ,STAUTLJ,
     *               INVEHL,INVEHR,INVEHE,INVEHT,
     *               LUNRVALJ,NUNRVALJ,LCRDVALJ,NCAPVALJ
      IF(DSTA.EQ.SIC.AND.SJZ.EQ.JZ) GO TO 350
      GO TO 300
  310 WRITE(26,9028) IMODE,DSTA,JZ
 9028 FORMAT(//' NO STA-ZNE MATCH FOR IMODE=',I1,' DSTA=',I4,
     *         ' JZ=',I4/)
      STOP 16
  350 CONTINUE                                        
      WRITE(26,9025) SIC,SJZ,INVEHJ,
     *               INVEHL,INVEHR,INVEHE,INVEHT,
     *               WAIT1J,WAIT2J,
     *               TRANSFJ,FAREJ,WALKACCJ,
     *               WALKEGRJ,WALKTFRJ,
     *               LUNRVALJ,NUNRVALJ,LCRDVALJ,NCAPVALJ,
     *               STAUTLJ
 9025 FORMAT(1X,'DESTINATION STATION --> EGRESS ZONE COMPUTATIONS'/
     *       1X,'------------------------------------------------'//
     *       1X,'DESTINATION       STATION=',I8/
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
     *       1X,'LINK UNRELIABILITY TIME  =',F8.2/
     *       1X,'STOP UNRELIABILITY TIME  =',F8.2/
     *       1X,'LINK CROWDING      TIME  =',F8.2/
     *       1X,'STOP CAPACITY      TIME  =',F8.2/
     *       1X,'UTILITY VALUE            =',F10.5/)
      ELSE
      WRITE(26,9031) (DC+MAX_IZONES),JZ,
     *               STAEGR(3,DC,JZ),STAEGR(1,DC,JZ),
     *               STAEGR(2,DC,JZ)
 9031 FORMAT(1X,'DESTINATION STATION --> EGRESS ZONE COMPUTATIONS'/
     *       1X,'------------------------------------------------'//
     *       1X,'DESTINATION       STATION=',I8/
     *       1X,'EGRESS            ZONE   =',I8/
     *       1X,'LOGSUM            VALUE  =',F10.5/
     *       1X,'WALK EGRESS       SHARE  =',F10.5/
     *       1X,'BUS  EGRESS       SHARE  =',F10.5/)
      END IF      
      ELSE                                                              
      WRITE(26,9026) NAME(6),STA,JZ                                       
 9026 FORMAT(/1X,'****************************'/
     *       1X,'* MULTIPLE PATH EVALUATION *'/
     *       1X,'****************************'//
     *       1X,'EGRESS STATION SELECTION -- ',A13/                            
     *       1X,'------------------------'/                             
     *       1X,'ACCESS STATION    =',I10/                              
     *       1X,'DESTINATION ZONE  =',I10/                              
     *       1X,'--> NO PATH AVAILABLE <--'/)                              
      ENDIF                                                             
      ENDIF
C.....................................................................
      RETURN
      END
