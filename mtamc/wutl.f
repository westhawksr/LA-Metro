C-------------------------------------------------------------------
C        WALK ACCESS --> LINE-HAUL UTILITY CALCULATION SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE WUTL(IX,INDC,IZ,JZ,WSTA,WDSTA,WUTIL,
     *                    STASTA,STAZNE,imode,STAEGR,WDIST)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'mtamcpar.inc'
      INTEGER*2     IX,IZ,JZ,IC,DC,imode,INDC
      INTEGER*2     WSTA,WDSTA
      REAL*4        WUTIL,WDIST,TUTIL
      REAL*4        EGRWALK,EGRIVT,STAWLK
      REAL*4        STASTA(5,MAX_STATIONS,MAX_STATIONS),
     *              STAZNE(4,MAX_STATIONS,MAX_IZONES)
      REAL*4        STAEGR(4,MAX_STATIONS,MAX_IZONES)
      CHARACTER*13  NAME(5)
      DATA          NAME/'Commuter Rail',
     *                   'Urban Rail   ',
     *                   'Express Bus  ',
     *                   'Transitway   ',
     *                   'BRT          '/
C
C CALCULATE TOTAL UTILITY VALUE
C
      IF(WSTA.LE.0.OR.WDSTA.LE.0) THEN
      WUTIL=0.0
      TUTIL=0.0
C...................................................................
      IF(DEBUG) THEN
      IF(INDC.EQ.1) THEN
      WRITE(26,9024) IX,NAME(IMODE)
 9024 FORMAT(/1X,'WALK ACCESS #',I1,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'-----------------------------------------')
      ELSE
      WRITE(26,9022) IX,NAME(IMODE)
 9022 FORMAT(/1X,'BIKE ACCESS #',I1,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'-----------------------------------------')
      END IF
      WRITE(26,9023) IZ,JZ,WSTA,WDSTA,WUTIL
 9023 FORMAT(1X,'********* PATH UNAVAILABLE *************'/
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
      EGRWALK=0.0
      STAWLK=STADATA(DC,9)
      IF(.NOT.TRNEGR) THEN
      IF(STAIND(DC,JZ).EQ.3) THEN
      EGRWALK=STADATA(DC,11)
      EGRIVT=STADATA(DC,12)
      END IF
      WUTIL=STASTA(1,IC,DC) +  STAZNE(1,DC,JZ) + 
     *      COEFF(7)* (STADATA(IC,9) + STAWLK + EGRWALK) +
     *      COEFF(1)* EGRIVT 
      ELSE
      WUTIL=STASTA(1,IC,DC) +  STAEGR(3,DC,JZ) + 
     *      COEFF(7)* (STADATA(IC,9) + STAWLK)
      END IF
C....................................................................
      IF(DEBUG) THEN
      IF(INDC.EQ.1) THEN
      WRITE(26,9025) IX,NAME(imode)
 9025 FORMAT(/1X,'WALK ACCESS #',I1,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'-----------------------------------------')
      ELSE
      WRITE(26,9027) IX,NAME(imode)
 9027 FORMAT(/1X,'BIKE ACCESS #',I1,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'-----------------------------------------')      
      END IF
      IF(TRNEGR) THEN
      TUTIL=WUTIL+WDIST
      WRITE(26,9028) IZ,JZ,WSTA,STANAME(IC),
     *   WDSTA,STANAME(DC),STASTA(1,IC,DC),
     *   STAEGR(3,DC,JZ),STADATA(IC,9),
     *   STAWLK,WUTIL,WDIST,TUTIL
 9028 FORMAT(1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10,5X,A37/
     *       1X,'EGRESS STATION      =',I10,5X,A37/
     *       1X,'STA-->STA UTILITY   =',F10.5/
     *       1X,'STA-->ZNE UTILITY   =',F10.5/
     *       1X,'PLATFORM WALK TIME  =',F10.5/
     *       1X,'EGRESS PLATFORM WALK=',F10.5/
     *       1X,'PARTIAL   UTILITY   =',F10.5/
     *       1X,'ACCESS    UTILITY   =',F10.5/
     *       1X,'TOTAL     UTILITY   =',F10.5/
     *       1X,60('='))      
      ELSE
      WRITE(26,9026) IZ,JZ,WSTA,STANAME(IC),
     *   WDSTA,STANAME(DC),STASTA(1,IC,DC),
     *   STAZNE(1,DC,JZ),STADATA(IC,9),STAIND(DC,JZ),
     *   STAWLK,EGRWALK,EGRIVT,WUTIL
 9026 FORMAT(1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10,5X,A37/
     *       1X,'EGRESS STATION      =',I10,5X,A37/
     *       1X,'STA-->STA UTILITY   =',F10.5/
     *       1X,'STA-->ZNE UTILITY   =',F10.5/
     *       1X,'PLATFORM ACCESS TIME=',F10.5/
     *       1X,'EGRESS INDICATOR    =',I10,
     *          ' DIRECT WALK=1,BUS TRANSFER=2,DRIVE EGRESS=3'/
     *       1X,'PLATFORM EGRESS TIME=',F10.5/
     *       1X,'EGRESS STA WALK TIME=',F10.5/
     *       1X,'EGRESS STA IVT  TIME=',F10.5/
     *       1X,'TOTAL     UTILITY   =',F10.5/
     *       1X,60('='))
      END IF
      END IF
C.....................................................................
      END IF
      RETURN
      END
