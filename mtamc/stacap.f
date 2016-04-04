C--------------------------------------------------------------------
C     STATION CAPACITY RESTRAINT ALGORITHM
C--------------------------------------------------------------------
      SUBROUTINE STACAP(ITER,stasum,EQUIV)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'mtamcpar.inc'
C
      INTEGER*2     EQUIV(MAX_STATIONS),ITER
      INTEGER*4     K,KS
      REAL*4        VC,VC2,OBS,RCON
      real*4        TEMP
      real*8        stasum(MAX_STATIONS,23)
      CHARACTER*15  NAME(5)
      LOGICAL       CONVERG
      DATA          NAME/' COMMUTER RAIL ',
     *                   '  URBAN  RAIL  ',
     *                   '  EXPRESS BUS  ',
     *                   '  TRANSITWAY   ',
     *                   '  BUS RAPID    '/
C
C FIRST ADD TOGETHER ALL DRIVE TRIPS USING STATION
C
C  AUTOCC = AUTO OCCUPANCY FOR DRIVE TO EXPRESS AND TRANSITWAY
C            BUS
C  BPFACT = FACTOR FOR SEPERATING PNR FROM KNR TRIPS (TRANSITWAY)
C  BPFACE = FACTOR FOR SEPARATING PNR FROM KNR TRIPS (EXPRESS BUS)
C  BPFACR = FACTOR FOR SEPARATING PNR FROM KNR TRIPS (BUS RAPID TRANSIT)
C
       DO 500,IK=1,MAX_STATIONS
       if(stadata(ik,6).gt.0.0) then
        if(stadata(ik,4).le.0.0.and.
     *    (stasum(ik,11).gt.0.0.or.stasum(ik,13).gt.0.0)) go to 500
       K=EQUIV(IK)
       IF(K.EQ.0) GO TO 500
       STASUM(K,15)=STASUM(K,15)+STASUM(IK,3) + STASUM(IK,8) +
     *              STASUM(IK,12)*(BPFACT/AUTOCC) + STASUM(IK,14)*
     *              (BPFACE/AUTOCC) + STASUM(IK,18)*(BPFACR/AUTOCC)
       STASUM(K,16)=STASUM(K,16) + STASUM(IK,5) + STASUM(IK,10) +
     *              STASUM(IK,11) + STASUM(IK,12)+ STASUM(IK,13) +
     *              STASUM(IK,14) + STASUM(IK,17) + STASUM(IK,18)
	 endif
 500   CONTINUE
C
C SET EQUIV STATIONS EQUAL
C
      DO 510,IK=1,MAX_STATIONS
      K=EQUIV(IK)
      IF(K.EQ.0) GO TO 510
      STASUM(IK,15)=STASUM(K,15)
      STASUM(IK,16)=STASUM(K,16)
  510 CONTINUE
C
C STATION TYPE LOOP
C
      DO 200 IK=1,5
      IF(IK.EQ.1) CONVERG=.TRUE.
      WRITE(26,9000) ITER,NAME(IK)
 9000 FORMAT(//1X,'STATION PARKING CAPACITY ANALYSIS - ITERATION ',I2,
     *            '-- ',A15/
     *       1X,'------------------------------------------------'//
     *       1X,'         ','                             ',
     *          ' SPACE  A','DJUSTED ','  PREV ',' OVERALL',' REMAIN ',
     *          '  REVISED ',' SHADOW'/
     *       1X,'STATION  ','      STATION  NAME          ',
     *          ' DEMAND ',' SPACES ','   USED ','   V/C  ','   V/C  ',
     *          ' CONSTANT','   PRICE'/
     *       1X,'---------','-----------------------------',
     *          '---------','-------','-------','--------','--------',
     *          '---------','---------')
C
C STATION LOOP
C
      DO 100 K=1,MAX_STATIONS
      IF(EQUIV(K).EQ.0) GO TO 100
      IF(STANUM(K).NE.IK) GO TO 100
      ks=MAX_IZONES+k
      VC=0.0
      VC2=0.0
      OBS=0.0
      TEMP=0.0
C
C  COMPUTE OVERALL STATION CAPACITY V/C RATIO
C
        IF(STADATA(K,4).GT.0) THEN
        TEMP=STADATA(K,4)-STADATA(K,3)
        VC=(STASUM(K,15)+(TEMP*2.0))/(STADATA(K,4)*2.0)
        ENDIF
C
C  COMPUTE REMAINING CAPACITY V/C RATIO
C
       IF(stasum(k,15).gt.0.0) then
       OBS=(STADATA(K,3)*2.0)/STASUM(K,15)
       IF(OBS.GT.0.0) VC2=1.0/OBS
       ENDIF
C
C  COMPUTE REVISED CONSTANT TERM
C
        RCON=0.0
        IF(CALIB.AND.NEGSHP) THEN
	  if(OBS.ne.0.0) then
          RCON=ALOG(1.0/OBS)
	  ENDIF
        ELSE
	  if((OBS.LE.1.0).AND.(OBS.ne.0.0)) then
          RCON=ALOG(1.0/OBS)
	  ENDIF
        END IF
        IF((.NOT.EXPCAP).AND.(IK.EQ.3)) RCON=0.0
        IF(IK.EQ.1.AND.VC2.GT.1.10) CONVERG=.FALSE.
C
C  COMPUTE SHADOW PRICE AND SUMMARIZE
C
	if(stadata(k,6).eq.1.0) then
       stadata(k,5)=-0.5*(RCON/COEFF(6)) + STADATA(K,5)
	WRITE(26,101) KS,STANAME(K),STASUM(K,15),STADATA(K,4),TEMP,VC,
     *              VC2,RCON,STADATA(K,5)
  101  FORMAT(2X,I4,3X,A29,3F8.0,2F8.3,1X,F9.3,F8.0)
      endif
 100  CONTINUE
 200  CONTINUE
C
      IF(CONVERG) THEN
C
C  WRITE OUT FINAL SHADOW PRICE & NUMBER OF DRIVE TRIPS PER STATION
C  FOR INPUT IN SUBSEQUENT RUNS
C
       OPEN(11,FILE=SHADOUT,FORM='FORMATTED',STATUS='UNKNOWN')
       OPEN(9,FILE=PKUTOUT,FORM='FORMATTED',STATUS='UNKNOWN')
C..SHADOW PRICE
       DO 411,NS=1,MAX_STATIONS
       NSTA=NS+MAX_IZONES
       IF(STADATA(NS,6).gt.0.0) then
       WRITE(11,8333) NSTA,STADATA(NS,5)
 8333  FORMAT(2x,i4,2x,f6.0)
C..SPACES USED
       USED=(STASUM(NS,15)/2.0)+(STADATA(NS,4)-STADATA(NS,3))
       IUSED=IFIX(USED)
       WRITE(9,8335) NSTA,IUSED
 8335  FORMAT(2X,I4,2X,I5)
       ENDIF
  411  CONTINUE
       close(11,status='keep')
       close(9,status='keep')
C
C PROGRAM COMPLETION
C
       CALL GETTIM(IHR,IMIN,ISEC,I100)
       WRITE(26,8004) IHR,IMIN,ISEC
       WRITE(*,8004) IHR,IMIN,ISEC
 8004  FORMAT(/' Station Capacity Restraint Closure Acheived'/
     *         ' Program Completed: ',I2,':',I2,':',I2/)
      STOP
      END IF
      RETURN
      END
