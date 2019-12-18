C-------------------------------------------------------------------
C        DRIVE ACCESS TO STATION SELECTION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE STADRV(IZ,ZINDEX,IMODE,HDIST)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'mtamcpar.inc'
C
C DECLARATIONS REQUIRED FOR DRIVE ACCESS STATION CHOICE
C
      INTEGER*2     IZ,JS,SC,IC,IMODE
      INTEGER*2     ZINDEX(MAX_STATIONS),TJZ,IX,IY
      REAL*4        SDIST(MAX_STATIONS),
     *              TDIST,hdist(MAX_ZONES)
      LOGICAL       CSORT
      CHARACTER*13  NAME(5)
      DATA          NAME/'Commuter Rail',
     *                   'Urban Rail   ',
     *                   'Express Bus  ',
     *                   'Transitway   ',
     *                   'BRT          '/
C
C     INITIALIZE ZINDEX
C
      zindex=0
C
C LOOP THROUGH STATIONS
C
      DO 150 SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      ZINDEX(SC)=SC
      IF(STADATA(SC,6).lt.1.0) HDIST(IC)=99999.9
      IF(HDIST(IC).LE.0.0) HDIST(IC)=99999.9
      IF(STANUM(SC).NE.IMODE) THEN
      SDIST(SC)=99999.9
      ELSE
      SDIST(SC)=HDIST(IC)
      END IF
  150 CONTINUE
C
C...SORT HIGHWAY TRAVEL DISTANCE VALUES IN DESCENDING ORDER
C
  160 CSORT=.FALSE.
	DO 161 JS=2,MAX_STATIONS
	k=js-1
	IF(SDIST(JS).lt.SDIST(JS-1)) THEN
	TJZ=ZINDEX(JS)
      TDIST=SDIST(JS)
      ZINDEX(JS)=ZINDEX(JS-1)
      ZINDEX(JS-1)=TJZ
      SDIST(JS)=SDIST(JS-1)
      SDIST(JS-1)=TDIST
      CSORT=.TRUE.
      END IF
  161 CONTINUE
      IF(CSORT) GO TO 160
C...................................................................
      IF(DEBUG) THEN
      WRITE(26,9031) IZ,NAME(IMODE)
 9031 FORMAT(//1X,'STATION CHOICE SORT BY DISTANCE FOR PRODUCTION ZONE',
     *       '=',I4,' -- ',a13/
     *       1X,'---------------------------------------------------',
     *       '-----'/)
      DO 9032 IX=1,10
	IY=ZINDEX(IX)+MAX_IZONES
        SC=ZINDEX(IX)
        IF(SC.LE.0) SC=MAX_STATIONS
      WRITE(26,9033) IY,SDIST(IX),STANAME(SC)
 9033 FORMAT(1X,'SDIST(',I4,')=',F8.2,5X,A37)
 9032 CONTINUE
      END IF
C....................................................................
      RETURN
      END
