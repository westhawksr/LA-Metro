C-------------------------------------------------------------------
C     MODE OF ACCESS/EGRESS COMPUTATIONS AND SUMMARY
C-------------------------------------------------------------------
      SUBROUTINE MACCEGR(STASUM,STASUM2,STASUMA)
      INCLUDE 'stadat.com'
      INCLUDE 'param.com'
      INCLUDE 'mtamcpar.inc'
      INTEGER*2 T,K,T1
      REAL*8    STASUM(MAX_STATIONS,24),STASUM2(MAX_STATIONS,4)
      REAL*8    STASUMP(MAX_STATIONS,24),STASUMA(MAX_STATIONS,4)
      REAL*8    TTWLK,TTBUS,TTPNR,TTKNR,TTBIK,TTOTAL
      STASUMP=0.0
      STASUMA=0.0
      IF(CALIB) THEN
C....COMMUTER RAIL
      DO K=1,MAX_STATIONS
      IF(STANUM(K).NE.1) CYCLE
      DO T=1,4
      STASUMP(K,T)=STASUM(K,T)+STASUM6(K,T,3)+STASUM6(K,T,5)
      STASUMA(K,T)=STASUM2(K,T)+STASUM7(K,T,1)+STASUM7(K,T,2)+
     *             STASUM7(K,T,3)+STASUM7(K,T,5)
      END DO
      STASUMP(K,19)=STASUM(K,19)+STASUM6(K,5,3)+STASUM6(K,5,5)  
      END DO
C....URBAN RAIL
      DO K=1,MAX_STATIONS
      IF(STANUM(K).NE.2) CYCLE
      DO T=6,9
      T1=T-5
      STASUMP(K,T)=STASUM(K,T)-STASUM6(K,T1,3)+STASUM6(K,T1,6)
      STASUMA(K,T1)=STASUM2(K,T1)+STASUM7(K,T1,4)+STASUM7(K,T1,6)
      END DO
      STASUMP(K,20)=STASUM(K,20)-STASUM6(K,5,3)+STASUM6(K,5,6)
      END DO
C....BRT
      DO K=1,MAX_STATIONS
      IF(STANUM(K).NE.5) CYCLE
      DO T=15,18
      T1=T-14
      STASUMP(K,T)=STASUM(K,T)-STASUM6(K,T1,5)-STASUM6(K,T1,6)
      STASUMA(K,T1)=STASUM2(K,T1)
      END DO
      STASUMP(K,23)=STASUM(K,23)-STASUM6(K,5,5)-STASUM6(K,5,6)
      END DO
C***************************************************************
      ELSE
C***************************************************************
C....COMMUTER RAIL
      DO K=1,MAX_STATIONS
      IF(STANUM(K).NE.1) CYCLE
      DO T=1,4
      STASUMP(K,T)=STASUM(K,T)+STASUM6(K,T,3)+STASUM6(K,T,5)
      STASUMA(K,T)=STASUM2(K,T)+STASUM7(K,T,1)+STASUM7(K,T,2)+
     *             STASUM7(K,T,3)+STASUM7(K,T,5)
      END DO
      STASUMP(K,19)=STASUM(K,19)+STASUM6(K,5,3)+STASUM6(K,5,5)  
      END DO
C....URBAN RAIL
      DO K=1,MAX_STATIONS
      IF(STANUM(K).NE.2) CYCLE
      DO T=6,9
      T1=T-5
      STASUMP(K,T)=STASUM(K,T)+STASUM6(K,T1,1)+STASUM6(K,T1,6)
      STASUMA(K,T1)=STASUM2(K,T1)+STASUM7(K,T1,4)+STASUM7(K,T1,6)+
     *                            STASUM7(K,T1,1)+STASUM7(K,T1,3)
      END DO
      STASUMP(K,20)=STASUM(K,20)+STASUM6(K,5,1)+STASUM6(K,5,6)
      END DO
C....BRT
      DO K=1,MAX_STATIONS
      IF(STANUM(K).NE.5) CYCLE
      DO T=15,18
      T1=T-14
      STASUMP(K,T)=STASUM(K,T)+STASUM6(K,T1,2)+STASUM6(K,T1,4)
      STASUMA(K,T1)=STASUM2(K,T1)+STASUM7(K,T1,2)+STASUM7(K,T1,4)+
     *              STASUM7(K,T1,5)+STASUM7(K,T1,6)
      END DO
      STASUMP(K,23)=STASUM(K,23)+STASUM6(K,5,2)+STASUM6(K,5,4)
      END DO
      END IF
      DO K=1,MAX_STATIONS
      DO K1=1,4
      IF(STASUMA(K,K1).LT.0) STASUMA(K,K1)=0.0
      END DO
      END DO
C---------------------------------------------------------------
C
C  SUMMARIZE STATION MODE OF ACCESS DATA
C
      WRITE(26,9210)
 9210 FORMAT(//,30X,'R E P O R T   3A',/,
     *          20X,
     *          'SUMMARIZE COMMUTER RAIL STATION ACCESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','  BIKE  ','   BUS  ','  PARK  ','  KISS  ',
     *          '        ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' ACCESS ',' ACCESS ',' ACCESS ','  RIDE  ','  RIDE  ',
     *          '  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------','--------',
     *          '-------  ')
        TTWLK=0.0
	      TTBUS=0.0
	      TTBIK=0.0
	      TTPNR=0.0
	      TTKNR=0.0  
	      TTOTAL=0.0
      WRITE(46,4441)
 4441 FORMAT('COMMUTER_RAIL_ACCESS_VOLUMES'/
     *       'STATION,NAME,WALK,BIKE,BUS,PNR,KNR,TOTAL')
      DO 500 K=1,MAX_STATIONS
      IF(STANUM(K).NE.1) GO TO 500
      KS=K+MAX_IZONES
      STASUMP(K,5)=STASUMP(K,1)+STASUMP(K,2)+STASUMP(K,3)+STASUMP(K,4)+
     *            STASUMP(K,19)
      TTWLK=TTWLK+STASUMP(K,1)
	    TTBUS=TTBUS+STASUMP(K,2)
	    TTBIK=TTBIK+STASUMP(K,19)
	    TTPNR=TTPNR+STASUMP(K,3)
	    TTKNR=TTKNR+STASUMP(K,4)
	    TTOTAL=TTOTAL+STASUMP(K,5)
      IF((STASUMP(K,5).GT.0.1.AND.(.NOT.CALIB)).OR.CALIB) THEN
	    WRITE(26,9211) KS,STANAME(K),STASUMP(K,1),STASUMP(K,19),
     *                  (STASUMP(K,L),L=2,5)
 9211    FORMAT(2X,I4,3X,A29,1X,6F8.0)
      WRITE(46,4440) KS,STANAME(K),STASUMP(K,1),STASUMP(K,19),
     *                  (STASUMP(K,L),L=2,5)
 4440 FORMAT(I4,',',A37,6(',',F8.0))
      END IF
  500 CONTINUE
      WRITE(26,9337) TTWLK,TTBIK,TTBUS,TTPNR,TTKNR,TTOTAL
 9337 FORMAT(/3X,'TOTAL',31X,6F8.0)
      WRITE(46,4442) TTWLK,TTBIK,TTBUS,TTPNR,TTKNR,TTOTAL
 4442 FORMAT('TOTAL,',6(',',F8.0))
      TTWLK=0.0
      TTBUS=0.0
      TTBIK=0.0
      TTPNR=0.0
      TTOTAL=0.0
      DENOM=0.0
      WRITE(46,4443)
 4443 FORMAT('COMMUTER_RAIL_EGRESS_VOLUMES'/
     *       'STATION,NAME,WALK,BUS,BIKE,DRIVE,TOTAL')
      WRITE(26,9212)
 9212 FORMAT(//,30X,'R E P O R T   3A',/,
     *          20X,
     *          'SUMMARIZE COMMUTER RAIL STATION EGRESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','   BUS  ','  BIKE  ',' DRIVE  ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' EGRESS ',' EGRESS ',' EGRESS ',' EGRESS ','  TOTAL '/
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------','------  ')
      DO 1501 K=1,MAX_STATIONS
      IF(STANUM(K).NE.1) GO TO 1501
      KS=K+MAX_IZONES
      DENOM=STASUMA(K,1)+STASUMA(K,2)+STASUMA(K,3)+STASUMA(K,4)
      TTWLK=TTWLK+STASUMA(K,1)
      TTBUS=TTBUS+STASUMA(K,2)
      TTBIK=TTBIK+STASUMA(K,3)
      TTPNR=TTPNR+STASUMA(K,4)
      TTOTAL=TTOTAL+DENOM
      IF((DENOM.GT.0.1.AND.(.NOT.CALIB)).OR.CALIB) THEN
	    WRITE(26,9213) KS,STANAME(K),
     *                  (STASUMA(K,L),L=1,4),DENOM
    	WRITE(46,4440) KS,STANAME(K),
     *                  (STASUMA(K,L),L=1,4),DENOM
 9213 FORMAT(2X,I4,3X,A29,1X,5F8.0)
      END IF
 1501 CONTINUE
      WRITE(26,9337) TTWLK,TTBUS,TTBIK,TTPNR,TTOTAL
      WRITE(46,4442) TTWLK,TTBUS,TTBIK,TTPNR,TTOTAL
C
      WRITE(26,9310)
 9310 FORMAT(//,30X,'R E P O R T   3B',/,
     *          20X,
     *          'SUMMARIZE URBAN RAIL STATION ACCESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','  BIKE  ','   BUS  ','  PARK  ','  KISS  ',
     *          '        ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' ACCESS ',' ACCESS ',' ACCESS ','  RIDE  ','  RIDE  ',
     *          '  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------','--------',
     *          '-------  ')
C
      WRITE(46,4444)
 4444 FORMAT('URBAN_RAIL_ACCESS_VOLUMES'/
     *       'STATION,NAME,WALK,BIKE,BUS,PNR,KNR,TOTAL')
        TTWLK=0.0
        TTBIK=0.0
	      TTBUS=0.0
	      TTPNR=0.0
	      TTKNR=0.0
	      TTOTAL=0.0
      DO 1502 K=1,MAX_STATIONS
      IF(STANUM(K).NE.2) GO TO 1502
      KS=K+MAX_IZONES
      STASUMP(K,10)=STASUMP(K,6)+STASUMP(K,7)+STASUMP(K,8)+STASUMP(K,9)+
     *             STASUMP(K,20)
        TTWLK=TTWLK+STASUMP(K,6)
        TTBIK=TTBIK+STASUMP(K,20)
	      TTBUS=TTBUS+STASUMP(K,7)
	      TTPNR=TTPNR+STASUMP(K,8)
	      TTKNR=TTKNR+STASUMP(K,9)
	      TTOTAL=TTOTAL+STASUMP(K,10)
      IF((STASUMP(K,10).GT.0.1.AND.(.NOT.CALIB)).OR.CALIB) THEN
	    WRITE(26,9211) KS,STANAME(K),STASUMP(K,6),STASUMP(K,20),
     *               (STASUMP(K,L),L=7,10)
      WRITE(46,4440) KS,STANAME(K),STASUMP(K,6),STASUMP(K,20),
     *                  (STASUMP(K,L),L=7,10)
      END IF
 1502 CONTINUE
      WRITE(26,9337) TTWLK,TTBIK,TTBUS,TTPNR,TTKNR,TTOTAL
      WRITE(46,4442) TTWLK,TTBIK,TTBUS,TTPNR,TTKNR,TTOTAL
      TTWLK=0.0
      TTBUS=0.0
      TTBIK=0.0
      TTPNR=0.0
      TTOTAL=0.0
      DENOM=0.0
      WRITE(46,4445)
 4445 FORMAT('URBAN_RAIL_EGRESS_VOLUMES'/
     *       'STATION,NAME,WALK,BUS,BIKE,DRIVE,TOTAL')
      WRITE(26,9311)
 9311 FORMAT(//,30X,'R E P O R T   3B',/,
     *          20X,
     *          'SUMMARIZE URBAN RAIL STATION EGRESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','   BUS  ','  BIKE  ',' DRIVE  ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' EGRESS ',' EGRESS ',' EGRESS ',' EGRESS ','  TOTAL '/
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------','------  ')
      DO 1506 K=1,MAX_STATIONS
      IF(STANUM(K).NE.2) GO TO 1506
      KS=K+MAX_IZONES
      DENOM=STASUMA(K,1)+STASUMA(K,2)+STASUMA(K,3)+STASUMA(K,4)
      TTWLK=TTWLK+STASUMA(K,1)
      TTBUS=TTBUS+STASUMA(K,2)
      TTBIK=TTBIK+STASUMA(K,3)
      TTPNR=TTPNR+STASUMA(K,4)
      TTOTAL=TTOTAL+DENOM
      IF((DENOM.GT.0.1.AND.(.NOT.CALIB)).OR.CALIB) THEN
	    WRITE(26,9213) KS,STANAME(K),
     *                  (STASUMA(K,L),L=1,4),DENOM
    	WRITE(46,4440) KS,STANAME(K),
     *                  (STASUMA(K,L),L=1,4),DENOM
      END IF
 1506 CONTINUE
      WRITE(26,9337) TTWLK,TTBUS,TTBIK,TTPNR,TTOTAL
      WRITE(46,4442) TTWLK,TTBUS,TTBIK,TTPNR,TTOTAL
C
      WRITE(26,9610)
 9610 FORMAT(//,30X,'R E P O R T   3C',/,
     *          20X,
     *          'SUMMARIZE BUS RAPID TRANSIT STATION ACCESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','  BIKE  ','   BUS  ','  PARK  ','  KISS  ',
     *          '        ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' ACCESS ',' ACCESS ',' ACCESS ','  RIDE  ','  RIDE  ',
     *          '  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------','--------',
     *          '-------  ')
C
        TTWLK=0.0
        TTBIK=0.0
	      TTBUS=0.0
	      TTPNR=0.0
	      TTKNR=0.0
	      TTOTAL=0.0
      WRITE(46,4446)
 4446 FORMAT('BRT_ACCESS_VOLUMES'/
     *       'STATION,NAME,WALK,BIKE,BUS,PNR,KNR,TOTAL')
      DO K=1,MAX_STATIONS
      IF(STANUM(K).NE.5) CYCLE
      KS=K+MAX_IZONES
      STASUMP(K,24)=STASUMP(K,15)+STASUMP(K,16)+STASUMP(K,17)
     *             +STASUMP(K,18)+STASUMP(K,23)
        TTWLK=TTWLK+STASUMP(K,15)
        TTBIK=TTBIK+STASUMP(K,23)
	      TTBUS=TTBUS+STASUMP(K,16)
	      TTPNR=TTPNR+STASUMP(K,17)
	      TTKNR=TTKNR+STASUMP(K,18)
	      TTOTAL=TTOTAL+STASUMP(K,24)
      IF((STASUMP(K,24).GT.0.1.AND.(.NOT.CALIB)).OR.CALIB) THEN
	    WRITE(26,9211) KS,STANAME(K),STASUMP(K,15),STASUMP(K,23),
     *               (STASUMP(K,L),L=16,18),STASUMP(K,24)
      WRITE(46,4440) KS,STANAME(K),STASUMP(K,15),STASUMP(K,23),
     *               (STASUMP(K,L),L=16,18),STASUMP(K,24)
      END IF
      END DO
      WRITE(26,9337) TTWLK,TTBIK,TTBUS,TTPNR,TTKNR,TTOTAL
      WRITE(46,4442) TTWLK,TTBIK,TTBUS,TTPNR,TTKNR,TTOTAL
      TTWLK=0.0
      TTBUS=0.0
      TTBIK=0.0
      TTPNR=0.0
      TTOTAL=0.0
      DENOM=0.0
      WRITE(46,4447)
 4447 FORMAT('BRT_EGRESS_VOLUMES'/
     *       'STATION,NAME,WALK,BUS,BIKE,DRIVE,TOTAL')
      WRITE(26,9611)
 9611 FORMAT(//,30X,'R E P O R T   3C',/,
     *          20X,
     *          'SUMMARIZE BUS RAPID TRANSIT STATION EGRESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','   BUS  ','  BIKE  ',' DRIVE  ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' EGRESS ',' EGRESS ',' EGRESS ',' EGRESS ','  TOTAL ',/
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------','------  ')
      DO K=1,MAX_STATIONS
      IF(STANUM(K).NE.5) CYCLE
      KS=K+MAX_IZONES
      DENOM=STASUMA(K,1)+STASUMA(K,2)+STASUMA(K,3)+STASUMA(K,4)
      TTWLK=TTWLK+STASUMA(K,1)
      TTBUS=TTBUS+STASUMA(K,2)
      TTBIK=TTBIK+STASUMA(K,3)
      TTPNR=TTPNR+STASUMA(K,4)
      TTOTAL=TTOTAL+DENOM
      IF((DENOM.GT.0.1.AND.(.NOT.CALIB)).OR.CALIB) THEN
	    WRITE(26,9213) KS,STANAME(K),
     *                  (STASUMA(K,L),L=1,4),DENOM
    	WRITE(46,4440) KS,STANAME(K),
     *                  (STASUMA(K,L),L=1,4),DENOM
      END IF
      END DO
      WRITE(26,9337) TTWLK,TTBUS,TTBIK,TTPNR,TTOTAL
      WRITE(46,4442) TTWLK,TTBUS,TTBIK,TTPNR,TTOTAL
      RETURN
      END
