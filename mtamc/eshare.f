C-------------------------------------------------------------------
C      OUTPUT STATION-TO-ZONE EGRESS SHARES
C-------------------------------------------------------------------
       SUBROUTINE ESHARE(STAEGR)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'tpcom.inc'
       include 'control.inc'
       include 'mtamcpar.inc'
C
      INTEGER*2 T,II,IN,IJ,IC,SC
      REAL*4    STAEGR(4,MAX_STATIONS,MAX_IZONES)
      INTEGER*4 ROW(MAX_ZONES),URLINE
      ROW=0
C
C     OUTPUT STATION-TO-ZONE EGRESS SHARE MATRICES
C
      OPEN(146,FILE='EGRPROB.MTX',STATUS='UNKNOWN',
     *         FORM='UNFORMATTED')
      NUMPUR=2
      TABLES=2**10-1
      WRITE(146) HEAD1,HEAD2
C  ZONE LOOP
      DO II=1,MAX_ZONES  
C  TABLE LOOP  
      DO T=1,2
      ROW=0
	    PURP=T
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_IZONES
      IN=II-MAX_IZONES
      IF(STAEGR(T,IN,IJ).GT.0.0) THEN
	    TEMP=(STAEGR(T,IN,IJ)*100.0)+0.5
	    ROW(IJ)=IFIX(TEMP)
      ELSE
      ROW(IJ)=0
      END IF
      END DO
      ENDIF
      CALL OUTAB(146,ROW,II,PURP,DUMMY,IO)
      END DO
      END DO
C
C     OUTPUT SELECTED STATION INFORMATION
C
      OPEN(147,FILE='EGRPROB_STA.CSV',STATUS='UNKNOWN',
     *         FORM='FORMATTED')
      WRITE(147,8001)
 8001 FORMAT('STATION,NAME,MODE,URLINE')
      DO SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      IF(STADATA(SC,6).EQ.1.0) THEN
      URLINE=IDINT(STADATA(SC,8))
      WRITE(147,8002) IC,STANAME(SC),STANUM(SC),URLINE
 8002 FORMAT(I4,',',A37,',',I2,',',I2)
      END IF
      END DO
      CLOSE(146,STATUS='KEEP')
      CLOSE(147,STATUS='KEEP')
      RETURN
      END

