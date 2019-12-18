C-------------------------------------------------------------------
C     OBTAIN PSEUDO ZONE FOR TRANSIT TRANSFER NODE
C-------------------------------------------------------------------
      SUBROUTINE STANODE(NODE,STA)
      INCLUDE 'stadat.com'
      INCLUDE 'param.com'
      include 'tpcom.inc'
      include 'control.inc'
	    include 'mtamcpar.inc'
C
C  DATA DECLARATIONS
C
	    INTEGER*4  NODE,STA
C
      STA=0
      IF(NODE.GT.0) THEN
      DO K=1,MAX_STATIONS
      IF(STANODE1(K).EQ.NODE) THEN
      L=K+MAX_IZONES
      STA=L
      GO TO 1000
      END IF
      IF(STANODE2(K).EQ.NODE) THEN
      L=K+MAX_IZONES
      STA=L
      GO TO 1000
      END IF 
      END DO
      WRITE(26,1001) NODE
 1001 FORMAT(' PSEUDO STATION NODE NOT FOUND FOR NODE=',I5)
      END IF
 1000 CONTINUE
      RETURN
      END