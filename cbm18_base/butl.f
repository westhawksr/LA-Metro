C-------------------------------------------------------------------
C        BUS ACCESS --> RAIL UTILITY CALCULATION SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE BUTL(IX,IZ,JZ,BDIST,BSTA,BDSTA,BUTIL,
     *                    STASTA,STAZNE,imode,STAEGR)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'mtamcpar.inc'
      INTEGER*2     IX,IZ,JZ,IC,DC,IMODE
      INTEGER*2     BSTA,BDSTA
      REAL*4        BDIST,BUTIL,BUSEGR
      REAL*4        STASTA(5,MAX_STATIONS,MAX_STATIONS),
     *              STAZNE(4,MAX_STATIONS,MAX_IZONES)
      REAL*4        STAEGR(3,MAX_STATIONS,MAX_IZONES)
      REAL*4        EGRWALK,EGRIVT,STAWLK
      REAL*4        UTILEGR
      CHARACTER*13  NAME(5)
      DATA          NAME/'Commuter Rail',
     *                   'Urban Rail   ',
     *                   'Express Bus  ',
     *                   'Transitway   ',
     *                   'BRT          '/
C
C CALCULATE TOTAL UTILITY VALUE
C
      IC=BSTA-MAX_IZONES
      DC=BDSTA-MAX_IZONES
      IF(IC.LT.0.OR.DC.LT.0) THEN
      BUTIL=0.0
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9024) IX,NAME(IMODE),IZ,JZ,BSTA,BDSTA,BUTIL
 9024 FORMAT(/1X,'BUS ACCESS #',I1,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'----------------------------------------'/
     *       1X,'************ PATH UNAVAILABLE *********'/
     *       1X,'ORIGIN ZONE       =',I10/
     *       1X,'DESTINATION ZONE  =',I10/
     *       1X,'ACCESS STATION    =',I10/
     *       1X,'EGRESS STATION    =',I10/
     *       1X,'TOTAL     UTILITY =',F10.2)
      END IF
C.....................................................................
      RETURN
      END IF
      EGRIVT=0.0
      EGRWALK=0.0
      STAWLK=STADATA(DC,9)
      IF(STAIND(DC,JZ).EQ.3) THEN
      EGRWALK=STADATA(DC,11)
      EGRIVT=STADATA(DC,12)
      END IF
      BUTIL=BDIST + STASTA(1,IC,DC) +
     *      STAZNE(1,DC,JZ) +
     *      COEFF(7)* (STADATA(IC,10) + STAWLK + EGRWALK) +
     *      COEFF(1)* EGRIVT
      IF(TRNEGR) THEN
      BUTIL=BDIST + STASTA(1,IC,DC) +
     *      STAEGR(3,DC,JZ) + 
     *      COEFF(7)* (STADATA(IC,10) + STAWLK)    
      END IF
C....................................................................
      IF(DEBUG) THEN
      IF(TRNEGR) THEN
      WRITE(26,9027) IX,NAME(IMODE),IZ,JZ,
     *               BSTA,STANAME(IC),BDSTA,
     *               STANAME(DC),BDIST,STASTA(1,IC,DC),
     *               STAEGR(3,DC,JZ),STADATA(IC,10),
     *               STAWLK,BUTIL
 9027 FORMAT(/1X,'BUS ACCESS #',I1,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'----------------------------------------'/
     *       1X,'ORIGIN ZONE       =',I10/
     *       1X,'DESTINATION ZONE  =',I10/
     *       1X,'ACCESS STATION    =',I10,5X,A37/
     *       1X,'EGRESS STATION    =',I10,5X,A37/
     *       1X,'ACCESS    UTILITY =',F10.5/
     *       1X,'STA-->STA UTILITY =',F10.5/
     *       1X,'STA-->ZNE UTILITY =',F10.5/
     *       1X,'ACCESS PLATFORM TIME=',F10.5/
     *       1X,'EGRESS PLATFORM TIME=',F10.5/
     *       1X,'TOTAL     UTILITY =',F10.5)      
      
      ELSE
      UTILEGR=COEFF(45+IMODE)*BUSEGR/(LSUM3CP*LSUM2CR*LSUM1TRN)
      WRITE(26,9025) IX,NAME(IMODE),IZ,JZ,BSTA,STANAME(IC),BDSTA,
     *               STANAME(DC),BDIST,STASTA(1,IC,DC),
     *               STAZNE(1,DC,JZ),STADATA(IC,10),STAIND(DC,JZ),
     *   STAWLK,EGRWALK,EGRIVT,BUSEGR,UTILEGR,BUTIL
 9025 FORMAT(/1X,'BUS ACCESS #',I1,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'----------------------------------------'/
     *       1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10,5X,A37/
     *       1X,'EGRESS STATION      =',I10,5X,A37/
     *       1X,'ACCESS    UTILITY   =',F10.5/
     *       1X,'STA-->STA UTILITY   =',F10.5/
     *       1X,'STA-->ZNE UTILITY   =',F10.5/
     *       1X,'PLATFORM ACCESS TIME=',F10.5/
     *       1X,'EGRESS INDICATOR    =',I10,
     *          ' DIRECT WALK=1,BUS TRANSFER=2,DRIVE EGRESS=3'/
     *       1X,'PLATFORM EGRESS TIME=',F10.5/
     *       1X,'EGRESS STA WALK TIME=',F10.5/
     *       1X,'EGRESS STA IVT  TIME=',F10.5/
     *       1X,'BUS EGRESS INDICATOR=',F10.5/
     *       1X,'BUS EGRESS UTILITY  =',F10.5/
     *       1X,'TOTAL      UTILITY  =',F10.5)
      END IF
      END IF
C.....................................................................
      RETURN
      END

