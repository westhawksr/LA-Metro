C-------------------------------------------------------------------
C        SORT UTILITY VALUES SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE USORT(INDC,NNDEX,UTIL,STA,ISTA,DSTA,DUTIL,
     *           CRPNR,CRPNR2,imode,UUTIL,UCOST)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'mtamcpar.inc'
C
C DECLARATIONS REQUIRED FOR DRIVE ACCESS STATION CHOICE
C
      INTEGER*2     NNDEX(5,10),JZ,TJZ,STA(MAX_STATIONS),DSTA(10,10),
     *              ISTA(5,10),IX,IMODE,QJZ,SC,INDC
      REAL*4        UTIL(5,10),TUTIL,DUTIL(10,10),QUTIL,CRPNR(10)
      REAL*4        PNRRAT,PNRRAT2,CRPNR2(10)  
      REAL*4        UUTIL(5,10),UCOST(5,10),QUTIL2,QCOST
      LOGICAL       CSORT
      CHARACTER*13  NAME(5)
      CHARACTER*3   TYPE(2)
      DATA          NAME/'Commuter Rail',
     *                   'Urban Rail   ',
     *                   'Express Bus  ',
     *                   'Transitway   ',
     *                   'BRT          '/
      DATA          TYPE/'PNR','KNR'/
C
      DO 100 IX=1,10
         NNDEX(IMODE,IX)=STA(IX)+MAX_IZONES
         DSTA(imode,IX)=ISTA(imode,IX)
      IF(UTIL(IMODE,IX).EQ.0.0) UTIL(IMODE,IX)=-99999.9
  100 CONTINUE
C
C...SORT UTILITY VALUES IN DESCENDING ORDER
C
  160 CSORT=.FALSE.
      DO 161 JZ=2,10
      IF(UTIL(IMODE,JZ).GT.UTIL(IMODE,JZ-1)) THEN
      TJZ=NNDEX(IMODE,JZ)
      QJZ=DSTA(imode,JZ)
      TUTIL=UTIL(IMODE,JZ)
      QUTIL=DUTIL(IMODE,JZ)
      IF(INDC.EQ.2) QUTIL2=UUTIL(IMODE,JZ)
      IF(INDC.EQ.2) QCOST=UCOST(IMODE,JZ)
      PNRRAT=CRPNR(JZ)
      PNRRAT2=CRPNR2(JZ)
      NNDEX(IMODE,JZ)=NNDEX(IMODE,(JZ-1))
      DSTA(imode,JZ)=DSTA(imode,(JZ-1))
      NNDEX(IMODE,(JZ-1))=TJZ
      DSTA(imode,JZ-1)=QJZ
      UTIL(IMODE,JZ)=UTIL(IMODE,JZ-1)
      UTIL(IMODE,JZ-1)=TUTIL
      DUTIL(IMODE,JZ)=DUTIL(IMODE,JZ-1)
      IF(INDC.EQ.2) UUTIL(IMODE,JZ)=UUTIL(IMODE,(JZ-1))
      IF(INDC.EQ.2) UCOST(IMODE,JZ)=UCOST(IMODE,(JZ-1))
      DUTIL(IMODE,JZ-1)=QUTIL
      IF(INDC.EQ.2) UUTIL(IMODE,(JZ-1))=QUTIL2
      IF(INDC.EQ.2) UCOST(IMODE,(JZ-1))=QCOST
      CRPNR(JZ)=CRPNR(JZ-1)
      CRPNR(JZ-1)=PNRRAT
      CRPNR2(JZ)=CRPNR2(JZ-1)
      CRPNR2(JZ-1)=PNRRAT2
      CSORT=.TRUE.
      END IF
  161 CONTINUE
      IF(CSORT) GO TO 160
C
C...................................................................
      IF(DEBUG) THEN
      WRITE(26,9031) NAME(IMODE),TYPE(INDC) 
 9031 FORMAT(//1X,'UTILITY SORT BY STATION -- ',A13,1X,A3/
     *       1X,'------------------------')
      DO 9032 IX=1,10
      SC=NNDEX(IMODE,IX)-MAX_IZONES
      IF(SC.LE.0) SC=MAX_STATIONS
      WRITE(26,9033) NNDEX(IMODE,IX),DSTA(imode,IX),UTIL(IMODE,IX),
     * 	STANAME(SC)
 9033 FORMAT(1X,' MODEL UTIL(',I4,'-->',I4')=',F9.2,5X,A37)
 9032 CONTINUE
      IF(INDC.EQ.2) THEN
      WRITE(26,9034) NAME(IMODE)
 9034 FORMAT(//1X,'UTILITY SORT BY STATION FOR  ',A13,'-- UBER'/
     *       1X,'-------------------------------')
      DO IX=1,10
      SC=NNDEX(IMODE,IX)-MAX_IZONES
      IF(SC.LE.0) SC=MAX_IZONES
      WRITE(26,9033) NNDEX(IMODE,IX),DSTA(imode,IX),
     *               UUTIL(IMODE,IX),STANAME(SC)
      END DO
      END IF
      END IF
C....................................................................
      RETURN
      END
