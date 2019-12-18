C-------------------------------------------------------------------
C        KISS&RIDE ACCESS --> LINE-HAUL UTILITY CALCULATION SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE KUTL(IX,IZ,JZ,PDIST,IC,PDSTA,PUTIL,
     *                    STASTA,STAZNE,HTIME,HDIST,IMODE)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'mtamcpar.inc'
      INTEGER*2     IZ,JZ,IC,DC,IMODE
      INTEGER*2     IX,PSTA,PDSTA
      REAL*4        PDIST,PUTIL,HTIME(MAX_ZONES),HDIST(MAX_ZONES)
      REAL*4        STASTA(4,MAX_STATIONS,MAX_STATIONS),
     *              STAZNE(4,MAX_STATIONS,MAX_IZONES)
      REAL*4        EGRWALK,EGRIVT
      CHARACTER*13  NAME(2)
      DATA          NAME/'Commuter Rail',
     *                   'Urban Rail   '/
C
C CALCULATE TOTAL UTILITY VALUE
C
      PSTA=IC+MAX_IZONES
      DC=PDSTA-MAX_IZONES
      PDIST=99999.9
      PUTIL=0.0
C
C DETERMINE IF KISS-N-RIDE INDICATOR = 0
C
      IF(IC.GT.0) THEN
      IF(STADATA(IC,7).EQ.0) THEN
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9026) IX,NAME(IMODE),IZ,JZ,PSTA,PDSTA,STADATA(IC,4),
     *               STADATA(IC,7),PDIST,PUTIL
 9026 FORMAT(/1X,'KISS&RIDE ACCESS #',I2,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'--------------------------------------------------'/
     *       1X,'ORIGIN ZONE          =',I10/
     *       1X,'DESTINATION ZONE     =',I10/
     *       1X,'ACCESS STATION       =',I10/
     *       1X,'EGRESS STATION       =',I10//
     *       1X,'PARKING CAPACITY     =',F10.2,'<---'/
     *       1X,'PARK-N-RIDE INDICATOR=',F10.2,'<---'/
     *       1X,'TOTAL SIM UTILITY    =',F10.2/
     *       1X,'TOTAL     UTILITY    =',F10.5)
      END IF
C.....................................................................
      RETURN
      END IF
      END IF
C
C
C CHECK FOR VALID ACCESS/EGRESS STATION VALUES
C
      IF(IC.LT.0.OR.DC.LT.0) THEN
C....................................................................
      IF(DEBUG) THEN
	WRITE(26,9024) IX,NAME(IMODE),IZ,JZ,PSTA,PDSTA,PDIST,PUTIL
 9024 FORMAT(/1X,'KISS&RIDE ACCESS #',I2,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'--------------------------------------------------'/
     *       1X,'*************** PATH UNAVAILABLE ****************'/
     *       1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10/
     *       1X,'EGRESS STATION      =',I10//
     *       1X,'TOTAL SIM UTILITY   =',F10.2/
     *       1X,'TOTAL     UTILITY   =',F10.2)
      END IF
C.....................................................................
      RETURN
      END IF
C
C COMPUTE UTILITY VALUES
C
C...SIMPLIFIED VALUE
      PDIST=STASTA(1,IC,DC)+STAZNE(1,DC,JZ)+
     *  2.0*HTIME(PSTA) + 0.14*opcost*HDIST(PSTA)
C...MODEL VALUE
      PUTIL=STASTA(2,IC,DC) + STAZNE(2,DC,JZ) +
     *  COEFF(2)*HTIME(PSTA) + COEFF(6)*HDIST(PSTA)*opcost
      EGRIVT=0.0
      EGRWALK=STADATA(DC,9)
      IF(STAIND(DC,JZ).EQ.2) EGRWALK=STADATA(DC,10)
      IF(STAIND(DC,JZ).EQ.3) THEN
      EGRWALK=STADATA(DC,11)
      EGRIVT=STADATA(DC,12)
      END IF
      PUTIL=PUTIL + COEFF(7) * (STADATA(IC,13) + EGRWALK) +
     *              COEFF(1) * (EGRIVT + STADATA(IC,14))
C.....................................................................
      IF(DEBUG) THEN
      WRITE(26,9025) IX,NAME(IMODE),IZ,JZ,PSTA,PDSTA,PDIST,
     *               STASTA(1,IC,DC),STAZNE(1,DC,JZ),
     *               HTIME(PSTA),HDIST(PSTA),
     *               STASTA(2,IC,DC),STAZNE(2,DC,JZ),
     *               STADATA(IC,13),
     *               STADATA(IC,14),STAIND(DC,JZ),EGRWALK,EGRIVT,PUTIL
 9025 FORMAT(/1X,'KISS&RIDE ACCESS #',I2,
     *           ' --> RAIL COMPUTATION -- ',A13/
     *       1X,'--------------------------------------------------'/
     *       1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10/
     *       1X,'EGRESS STATION      =',I10//
     *       1X,'TOTAL SIM UTILITY   =',F10.2/
     *       1X,'STA-->STA UTILITY   =',F10.2/
     *       1X,'STA-->ZNE UTILITY   =',F10.2//
     *       1X,'HIGHWAY ACCESS TIME =',F10.2/
     *       1X,'HIGHWAY ACCESS DIST =',F10.2//
     *       1X,'STA-->STA UTILITY   =',F10.5/
     *       1X,'STA-->ZNE UTILITY   =',F10.5/
     *       1X,'ACCESS STA WALK TIME=',F10.5/
     *       1X,'ACCESS STA IVT  TIME=',F10.5/
     *       1X,'EGRESS INDICATOR    =',I10,
     *          ' DIRECT WALK=1,BUS TRANSFER=2,DRIVE EGRESS=3'/
     *       1X,'EGRESS STA WALK TIME=',F10.5/
     *       1X,'EGRESS STA IVT  TIME=',F10.5/
     *       1X,'TOTAL     UTILITY   =',F10.5)
      END IF
C.....................................................................
      RETURN
      END
