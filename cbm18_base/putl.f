C-------------------------------------------------------------------
C        PARK&RIDE ACCESS --> LINE-HAUL UTILITY CALCULATION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE PUTL(IX,IZ,JZ,PDIST,IC,PDSTA,PUTIL,
     *                 STASTA,STAZNE,HTIME,HDIST,imode,PNRRAT,PNRRAT2,
     *                 STAEGR)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'mtamcpar.inc'
      INTEGER*2     IZ,JZ,IC,DC,IMODE,IE,SC,SC2
      INTEGER*2     IX,PSTA,PDSTA
      REAL*4        PDIST,PUTIL,HTIME(MAX_ZONES),HDIST(MAX_ZONES)
      REAL*4        STASTA(5,MAX_STATIONS,MAX_STATIONS),
     *              STAZNE(4,MAX_STATIONS,MAX_IZONES)
      REAL*4        STAEGR(3,MAX_STATIONS,MAX_IZONES)
      REAL*4        PNRRAT,PNRRAT2
      REAL*4        EGRWALK,EGRIVT,STAWLK,BUSEGR
      CHARACTER*3   TYPE(3)
      CHARACTER*13  NAME(5)
      DATA          NAME/'Commuter Rail',
     *                   'Urban Rail   ',
     *                   'Express Bus  ',
     *                   'Transitway   ',
     *                   'BRT          '/
      DATA          TYPE/'   ','PNR','KNR'/
C
C CALCULATE TOTAL UTILITY VALUE
C
      PSTA=IC+MAX_IZONES
      DC=PDSTA-MAX_IZONES
C
C DETERMINE IF PARKING AVAILABLE AT STATION
C
      PUTIL=0.0
      PNRRAT=0.0
      PNRRAT2=0.0
      IF(IC.GT.0) THEN
      IF((STADATA(IC,3).LE.0.0).OR.(STADATA(IC,7).NE.1)) THEN
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9026) IX,NAME(IMODE),IZ,JZ,PSTA,PDSTA,STADATA(IC,4),
     *               TYPE(STADATA(IC,7)+1),PUTIL
 9026 FORMAT(/1X,'PARK&RIDE ACCESS #',I2,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'--------------------------------------------------'/
     *       1X,'ORIGIN ZONE          =',I10/
     *       1X,'DESTINATION ZONE     =',I10/
     *       1X,'ACCESS STATION       =',I10/
     *       1X,'EGRESS STATION       =',I10//
     *       1X,'PARKING CAPACITY     =',F10.2,'<---'/
     *       1X,'PARK-N-RIDE INDICATOR=',A10,'<---'/
     *       1X,'TOTAL     UTILITY    =',F10.5)
      END IF
C.....................................................................
      PDIST=0.0
      RETURN
      END IF
      END IF
C
C CHECK FOR VALID ACCESS/EGRESS STATION VALUES
C
      IF(IC.LT.0.OR.DC.LT.0) THEN
      PDIST=0.0
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9024) IX,NAME(IMODE),IZ,JZ,PSTA,PDSTA,PUTIL
 9024 FORMAT(/1X,'PARK&RIDE ACCESS #',I2,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'--------------------------------------------------'/
     *       1X,'*********** PATH UNAVAILABLE *******************'/
     *       1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10/
     *       1X,'EGRESS STATION      =',I10//
     *       1X,'TOTAL     UTILITY   =',F10.5)
      END IF
C.....................................................................
      RETURN
      END IF
C
C COMPUTED UTILITY VALUES
C
C...MODEL VALUE
      IF(.NOT.TRNEGR) THEN
      PUTIL=STASTA(1,IC,DC) + STAZNE(1,DC,JZ)
     *     +  COEFF(2)*HTIME(PSTA)
     *     + COEFF(6)*HDIST(PSTA)*opcost
     *     + COEFF(8)* STADATA(IC,4)
     *     + (COEFF(6)* STADATA(IC,1)*100.0)/2.0
      ELSE
      PUTIL=STASTA(1,IC,DC) + STAEGR(3,DC,JZ)
     *     +  COEFF(2)*HTIME(PSTA)
     *     + COEFF(6)*HDIST(PSTA)*opcost
     *     + COEFF(8)* STADATA(IC,4)
     *     + (COEFF(6)* STADATA(IC,1)*100.0)/2.0      
      
      END IF
      IF(HDIST(JZ).GT.0) THEN
      PNRRAT=(HDIST(PSTA)/HDIST(JZ))-0.5
	    PNRRAT=AMAX1(PNRRAT,0.0)
      IF(IMDOE.EQ.1) PUTIL=PUTIL+CCRPNR*PNRRAT
      IF(IMODE.EQ.2) PUTIL=PUTIL+CURPNR*PNRRAT
      END IF
      EGRIVT=0.0
      EGRWALK=0.0
      STAWLK=STADATA(DC,9)
      IF(STAIND(DC,JZ).EQ.3) THEN
      EGRWALK=STADATA(DC,11)
      EGRIVT=STADATA(DC,12)
      END IF
      PUTIL=PUTIL + COEFF(7) * (STADATA(IC,11) + STAWLK + EGRWALK) +
     *              COEFF(1) * (EGRIVT + STADATA(IC,12))
      PDIST=PUTIL
      IF(HDIST(JZ).GT.0) PNRRAT=HDIST(PSTA)/HDIST(JZ)
      IF(STASTA(3,IC,DC).GT.0) PNRRAT2=HTIME(PSTA)/STASTA(3,IC,DC)
C....................................................................
      IF(DEBUG) THEN
      SC=IC
      SC2=DC
      IF(IC.LT.0) SC=MAX_STATIONS
      IF(DC.LT.0) SC2=MAX_STATIONS
      IF(.NOT.TRNEGR) THEN
      WRITE(26,9025) IX,NAME(IMODE),IZ,JZ,PSTA,STANAME(SC),PDSTA,
     *               STANAME(SC2),
     *               STASTA(1,IC,DC),STAZNE(1,DC,JZ),
     *               HTIME(PSTA),HDIST(PSTA),STADATA(IC,4),
     *               STADATA(IC,1),
     *         STASTA(3,IC,DC),PNRRAT,PNRRAT2,STADATA(IC,11),
     *         STADATA(IC,12),STAIND(DC,JZ),STAWLK,EGRWALK,EGRIVT,PUTIL
 9025 FORMAT(/1X,'PARK&RIDE ACCESS #',I2,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'--------------------------------------------------'/
     *       1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10,1X,A37/
     *       1X,'EGRESS STATION      =',I10,1X,A37//
     *       1X,'STA-->STA UTILITY   =',F10.5/
     *       1X,'STA-->ZNE UTILITY   =',F10.5//
     *       1X,'HIGHWAY ACCESS TIME =',F10.2/
     *       1X,'HIGHWAY ACCESS DIST =',F10.2/
     *       1X,'ADJ PARKING CAPACITY=',F10.2/
     *       1X,'STATION PARKING COST=',F10.2/
     *       1X,'STA-STA IN-VEHICLE  =',F10.5/
     *       1X,'DRIVE DIST/TOT DIST =',F10.5/
     *       1X,'DRIVE TIME/RAIL IVT =',F10.5/
     *       1X,'PLATFORM ACCESS TIME=',F10.5/
     *       1X,'ACCESS STA IVT  TIME=',F10.5/
     *       1X,'EGRESS INDICATOR    =',I10,
     *          ' DIRECT WALK=1,BUS TRANSFER=2,DRIVE EGRESS=3'/
     *       1X,'PLATFORM EGRESS TIME=',F10.5/
     *       1X,'EGRESS STA WALK TIME=',F10.5/
     *       1X,'EGRESS STA IVT  TIME=',F10.5/
     *       1X,'TOTAL     UTILITY   =',F10.5)
      ELSE
      WRITE(26,9027) IX,NAME(IMODE),IZ,JZ,PSTA,STANAME(SC),PDSTA,
     *               STANAME(SC2),
     *               STASTA(1,IC,DC),STAEGR(3,DC,JZ),
     *               HTIME(PSTA),HDIST(PSTA),STADATA(IC,4),
     *               STADATA(IC,1),
     *         STASTA(3,IC,DC),PNRRAT,PNRRAT2,STADATA(IC,11),
     *         STADATA(IC,12),STAWLK,EGRWALK,EGRIVT,PUTIL
 9027 FORMAT(/1X,'PARK&RIDE ACCESS #',I2,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'--------------------------------------------------'/
     *       1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10,1X,A37/
     *       1X,'EGRESS STATION      =',I10,1X,A37//
     *       1X,'STA-->STA UTILITY   =',F10.5/
     *       1X,'STA-->ZNE UTILITY   =',F10.5//
     *       1X,'HIGHWAY ACCESS TIME =',F10.2/
     *       1X,'HIGHWAY ACCESS DIST =',F10.2/
     *       1X,'ADJ PARKING CAPACITY=',F10.2/
     *       1X,'STATION PARKING COST=',F10.2/
     *       1X,'STA-STA IN-VEHICLE  =',F10.5/
     *       1X,'DRIVE DIST/TOT DIST =',F10.5/
     *       1X,'DRIVE TIME/RAIL IVT =',F10.5/
     *       1X,'PLATFORM ACCESS TIME=',F10.5/
     *       1X,'ACCESS STA IVT  TIME=',F10.5/
     *       1X,'PLATFORM EGRESS TIME=',F10.5/
     *       1X,'EGRESS STA WALK TIME=',F10.5/
     *       1X,'EGRESS STA IVT  TIME=',F10.5/
     *       1X,'TOTAL     UTILITY   =',F10.5)
      END IF
      END IF
C.....................................................................
      RETURN
      END
