C-------------------------------------------------
C     READ & STORE URBAN RAIL ACCESS/EGRESS DATA
C     FOR NHB DIRECT DEMAND MODEL
C-------------------------------------------------
      SUBROUTINE ACCEGR
      INCLUDE 'param.com'
      INCLUDE 'stadat.com'
      INCLUDE 'mtamcpar.inc'
      INTEGER*4     COUNT,T
      REAL*8        TOTAL,TSUM(5)
      COUNT=0
      CALL URBREAD(FNHBPKHBW,COUNT,1)
      CALL URBREAD(FNHBPKHBO,COUNT,2)
      CALL URBREAD(FNHBPKHBU,COUNT,3)
      CALL URBREAD(FNHBOPHBW,COUNT,4)
      CALL URBREAD(FNHBOPHBO,COUNT,5)
      CALL URBREAD(FNHBOPHBU,COUNT,6)
      CALL URBREAD(FNHBPKHBS,COUNT,7)
      CALL URBREAD(FNHBOPHBS,COUNT,8)
C 
C     SUMMARIZE STATION LEVEL INPUT
C
      WRITE(26,9001)
 9001 FORMAT(//,30X,'R E P O R T   21',/,
     *          20X,
     *          '        NHB DIRECT DEMAND MODEL'/
     *          20X,
     *          'SUMMARIZE URBAN RAIL STATION INPUT VALUES  ',//,
     *       1X,' STATION ','                              ',
     *          '  HBW   ','   NWK  ','  HBW   ','   NWK  ',
     *          '  HBW   ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          '  PRODS ','  PRODS ',' ATTRS  ','  ATTRS ',
     *          '  INC3  ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','---------',
     *          '------  ')
      DO T=1,MAX_STATIONS
      TOTAL=STANHB(T,1)+STANHB(T,2)+STANHB(T,3)+STANHB(T,4)
      TSUM(1)=TSUM(1)+STANHB(T,1)
      TSUM(2)=TSUM(2)+STANHB(T,2)
      TSUM(3)=TSUM(3)+STANHB(T,3)
      TSUM(4)=TSUM(4)+STANHB(T,4)
      TSUM(5)=TSUM(5)+STANHB(T,5)
      IF(TOTAL.GT.0) THEN
      WRITE(26,9002) (T+MAX_IZONES),STANAME(T),(STANHB(T,K),K=1,5)
 9002 FORMAT(2X,I4,3X,A29,1X,5F8.0)
      END IF
      END DO
      WRITE(26,9003) TSUM
 9003 FORMAT(/3X,'TOTAL',31X,5F8.0)
      ARATIO=TSUM(3)/TSUM(4)
      IF(COUNT.GT.0) THEN
      WRITE(26,8001)
      WRITE(*,8001)
 8001 FORMAT(/1X,'ACCEGR 8001 (F) PROGRAM TERMINATED DUE TO',
     *           ' NHB DIRECT DEMAND FILE INPUT ERROR(S)')
      STOP 16
      ELSE
      RETURN
      END IF
      RETURN
      END
C-------------------------------------------------
C     URBAN RAIL ACCESS/EGRESS DATA READ
C-------------------------------------------------
      SUBROUTINE URBREAD(FILENAME,COUNT,INDEX)
      INCLUDE 'param.com'
      INCLUDE 'stadat.com'
      INCLUDE 'mtamcpar.inc'
      INTEGER*4     COUNT,STA,ZONE,INDEX,SC
      REAL*4        WALK,BUS,PNR,KNR,CR,TOTAL,DIST,BIKE
      LOGICAL       EXISTS
      CHARACTER*80  HEADER,FILENAME
      CHARACTER*9   NFILES(8)
      DATA NFILES/  'FNHBPKHBW','FNHBPKHBO','FNHBPKHBU',
     *              'FNHBOPHBW','FNHBOPHBO','FNHBOPHBU',
     *              'FNHBPKHBS','FNHBOPHBS'/  
C
C.....CHECK FILE AVAILABILITY
C
      INQUIRE (FILE=FILENAME,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9001) NFILES(INDEX),FILENAME
      WRITE(*,9001)  NFILES(INDEX),FILENAME
 9001 FORMAT(/1X,'URBREAD 9001 (F) ',A9,' STATION ACCESS/EGRESS',
     *       ' DATA FILE=',A40,
     *       ' DOES NOT EXIST')
      COUNT=COUNT+1
      RETURN
      ELSE
      OPEN(13,FILE=FILENAME,
     *        STATUS='OLD',FORM='FORMATTED')
      END IF
      READ(13,*,ERR=9100) HEADER
      READ(13,*,ERR=9100) HEADER
      READ(13,*,ERR=9100) HEADER
      IF(DEBUG) WRITE(26,8005) FILENAME
 8005 FORMAT(//' ACCESS VOLUMES FOR FILE ',A40/60('-')/)
    1 READ(13,*,ERR=9101,END=10) STA,WALK,BIKE,BUS,PNR,KNR,CR,TOTAL,
     *                           ZONE,DIST
      IF(STA.EQ.9999) GO TO 100
C.........................................................................
      IF(DEBUG) WRITE(26,8001) STA,WALK,BIKE,BUS,PNR,KNR,CR,TOTAL,ZONE,DIST
 8001 FORMAT(' STA=',I4,' WALK=',F7.1,' BIKE=',F7.1,' BUS=',F7.1,
     *       ' PNR=',F7.1,
     *       ' KNR=',F7.1,' CR=',F7.1,' TOTAL=',F8.1,' ZONE=',I4,
     *       ' DISTANCE=',F6.2)
C.........................................................................
      IF(STANUM(STA-MAX_IZONES).NE.2) GO TO 1
      IF(ZONE.LE.0.OR.ZONE.GT.MAX_IZONES) THEN
      WRITE(26,8002) ZONE,FILENAME
 8002 FORMAT(/1X,'URBREAD 8002 (F) ENCOUNTERED ZONE NUMBER (',I5,
     *           ') OUT OF RANGE IN FILE=',A80)
      COUNT=COUNT+1
      END IF
      IF(STA.LE.0.OR.STA.GT.MAX_ZONES.OR.STA.LT.MAX_IZONES) THEN
      WRITE(26,8003) STA,FILENAME
 8003 FORMAT(/1X,'URBREAD 8003 (F) ENCOUNTERED STATION NUMBER (',I5,
     *           ') OUT OF RANGE IN FILE=',A80)
      COUNT=COUNT+1
      END IF
C.....STORE DATA
      SC=STA-MAX_IZONES
      IF(INDEX.EQ.1.OR.INDEX.EQ.4) THEN
      STANHB(SC,1)=STANHB(SC,1)+WALK+BUS+PNR+KNR+BIKE
      ELSE
      STANHB(SC,2)=STANHB(SC,2)+WALK+BUS+PNR+KNR+BIKE
      END IF
      IF(INDEX.EQ.1) ZNENHB(SC)=ZONE
      GO TO 1
   10 WRITE(26,8004) FILENAME 
      WRITE(*,8004)  FILENAME
 8004 FORMAT(' URBREAD 8004 (F) EARLY END OF FILE REACHED IN FILE ',a80)
      COUNT=COUNT+1
      RETURN
 9100 WRITE(26,9110) FILENAME
 9110 FORMAT(/1X,'URBREAD 9110 (F) ENCOUNTERED ERROR IN',
     *        ' READING ACCESS HEADER FILE IN ',A80)
      COUNT=COUNT+1
 9101 WRITE(26,9111) STA,FILENAME
 9111 FORMAT(/1X,'URBREAD 9111 (F) ENCOUNTERED ERROR IN',
     *        ' READING ACCESS VOLUMES FOR STATION=',I4,
     *        ' IN FILE=',A80)
      COUNT=COUNT+1
C
C.... NOW READ EGRESS VOLUMES
C
  100 CONTINUE
      READ(13,*,ERR=9200) HEADER
      READ(13,*,ERR=9200) HEADER
      READ(13,*,ERR=9200) HEADER
      READ(13,*,ERR=9200) HEADER
      IF(DEBUG) WRITE(26,8105) FILENAME
 8105 FORMAT(//' EGRESS VOLUMES FOR FILE ',A40/60('-')/)      
    2 READ(13,*,ERR=9201,END=20) STA,WALK1,BUS1,CR1,TOT1,WALK2,
     *  BUS2,CR2,TOT2,WALK3,BUS3,CR3,TOT3,WALK4,
     *  BUS4,CR4,TOT4,WALK5,BUS5,CR5,TOT5
C.........................................................................
      IF(DEBUG) WRITE(26,8101) STA,WALK1,BUS1,CR1,TOT1,WALK2,
     *  BUS2,CR2,TOT2,WALK3,BUS3,CR3,TOT3,WALK4,
     *  BUS4,CR4,TOT4,WALK5,BUS5,CR5,TOT5
 8101 FORMAT(' STA=',I4,' WALK1=',F7.1,' BUS1=',F7.1,' CR1=',F7.1,
     *       ' TOT1=',F7.1,' WALK2=',F7.1,' BUS2=',F7.1,' CR2=',F7.1,
     *       ' TOT2=',F7.1,' WALK3=',F7.1,' BUS3=',F7.1,' CR3=',F7.1,
     *       ' TOT3=',F7.1,' WALK4=',F7.1,' BUS4=',F7.1,' CR4=',F7.1,
     *       ' TOT4=',F7.1,' WALK5=',F7.1,' BUS5=',F7.1,' CR5=',F7.1,
     *       ' TOT5=',F7.1) 
C.........................................................................
      IF(STANUM(STA-MAX_IZONES).NE.2) GO TO 2
      IF(STA.LE.0.OR.STA.GT.MAX_ZONES.OR.STA.LT.MAX_IZONES) THEN
      WRITE(26,8103) STA,FILENAME
 8103 FORMAT(/1X,'URBREAD 8103 (F) ENCOUNTERED STATION NUMBER (',I5,
     *           ') OUT OF RANGE FOR EGRESS VOLUMES IN FILE ',A80)
      COUNT=COUNT+1
      END IF
C.....STORE DATA
      SC=STA-MAX_IZONES
      IF(INDEX.EQ.1.OR.INDEX.EQ.4) THEN
      STANHB(SC,3)=STANHB(SC,3)+TOT1+TOT2+TOT3+TOT4+TOT5
      STANHB(SC,5)=STANHB(SC,5)+TOT5
      ELSE
      STANHB(SC,4)=STANHB(SC,4)+TOT1+TOT2+TOT3+TOT4+TOT5
      END IF
      GO TO 2
 9200 WRITE(26,9210) FILENAME
 9210 FORMAT(/1X,'URBREAD 9210 (F) ENCOUNTERED ERROR IN',
     *        ' READING EGRESS HEADER FILE IN ',A80)
      COUNT=COUNT+1
 9201 WRITE(26,9211) STA,FILENAME
 9211 FORMAT(/1X,'URBREAD 9211 (F) ENCOUNTERED ERROR IN',
     *        ' READING EGRESS VOLUMES FOR STATION=',I4,
     *        ' IN FILE=',A80)
      COUNT=COUNT+1
   20 CONTINUE
      CLOSE(13,STATUS='KEEP')
      RETURN
      END