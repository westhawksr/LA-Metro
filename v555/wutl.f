C-------------------------------------------------------------------
C        WALK ACCESS --> LINE-HAUL UTILITY CALCULATION SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE WUTL(IX,IZ,JZ,WSTA,WDSTA,WUTIL,
     *                    STASTA,STAZNE,imode)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'mtamcpar.inc'
      INTEGER*2     IX,IZ,JZ,IC,DC,imode
      INTEGER*2     WSTA,WDSTA
      REAL*4        WUTIL
      REAL*4        EGRWALK,EGRIVT
      REAL*4        STASTA(4,MAX_STATIONS,MAX_STATIONS),
     *              STAZNE(4,MAX_STATIONS,MAX_IZONES)
      CHARACTER*13  NAME(2)
      DATA          NAME/'Commuter Rail',
     *                   'Urban Rail   '/
C
C CALCULATE TOTAL UTILITY VALUE
C
      IF(WSTA.LE.0.OR.WDSTA.LE.0) THEN
      WUTIL=0.0
C...................................................................
      IF(DEBUG) THEN
      WRITE(26,9024) IX,NAME(IMODE),IZ,JZ,WSTA,WDSTA,WUTIL
 9024 FORMAT(/1X,'WALK ACCESS #',I1,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'-----------------------------------------'/
     *       1X,'********* PATH UNAVAILABLE *************'/
     *       1X,'ORIGIN ZONE       =',I10/
     *       1X,'DESTINATION ZONE  =',I10/
     *       1X,'ACCESS STATION    =',I10/
     *       1X,'EGRESS STATION    =',I10/
     *       1X,'TOTAL UTILITY     =',F10.2/)
	END IF
C...................................................................
      RETURN
	ELSE
      IC=WSTA-MAX_IZONES
      DC=WDSTA-MAX_IZONES
      EGRIVT=0.0
      EGRWALK=STADATA(DC,9)
      IF(STAIND(DC,JZ).EQ.2) EGRWALK=STADATA(DC,10)
      IF(STAIND(DC,JZ).EQ.3) THEN
      EGRWALK=STADATA(DC,11)
      EGRIVT=STADATA(DC,12)
      END IF
      WUTIL=STASTA(2,IC,DC) +  STAZNE(2,DC,JZ) + 
     *      COEFF(7)* (STADATA(IC,9) + EGRWALK) +
     *      COEFF(1)* EGRIVT
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9025) IX,NAME(imode),IZ,JZ,WSTA,STANAME(IC),
     *   WDSTA,STANAME(DC),STASTA(2,IC,DC),
     *   STAZNE(2,DC,JZ),STADATA(IC,9),STAIND(DC,JZ),
     *   EGRWALK,EGRIVT,WUTIL
 9025 FORMAT(/1X,'WALK ACCESS #',I1,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'-----------------------------------------'/
     *       1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10,5X,A37/
     *       1X,'EGRESS STATION      =',I10,5X,A37/
     *       1X,'STA-->STA UTILITY   =',F10.5/
     *       1X,'STA-->ZNE UTILITY   =',F10.5/
     *       1X,'ACCESS STA WALK TIME=',F10.5/
     *       1X,'EGRESS INDICATOR    =',I10,
     *          ' DIRECT WALK=1,BUS TRANSFER=2,DRIVE EGRESS=3'/
     *       1X,'EGRESS STA WALK TIME=',F10.5/
     *       1X,'EGRESS STA IVT  TIME=',F10.5/
     *       1X,'TOTAL     UTILITY   =',F10.5)
      END IF
C.....................................................................
      END IF
      RETURN
      END
