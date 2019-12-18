C-------------------------------------------------------------------
C       RAIL WALK ACCESS STATION SELECTION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE STAWLK(IZ,WSTA,WDIST,imode)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'mtamcpar.inc'
C
C DATA DECLARATIONS
C
      integer*2    imode
      INTEGER*2    WSTA(2,5),SC2,SC3,SC4,SC5,SC,IC,IZ
      integer*4    node
      REAL*4       WDIST(2,5),PXCOORD(MAX_ZONES),PYCOORD(MAX_ZONES),
     *             SXCOORD(MAX_ZONES),SYCOORD(MAX_ZONES),XDIST
      real*4       xcoor,ycoor
      character*13 name(2)
      LOGICAL      EXISTS
      data         name/'Commuter Rail',
     *                  'Urban Rail   '/
C
C IDENTIFY THE TWO CLOSEST STATIONS FOR WALK ACCESS
C
      IF(URBDIST.LE.0.0) URBDIST=1.0
      IF(URGDIST.LE.0.0) URGDIST=1.0
      WDIST(imode,1)=99.9
      WSTA(imode,1)=0
      WDIST(imode,2)=99.9
      WSTA(imode,2)=0
      WDIST(imode,3)=99.9
      WSTA(imode,3)=0
      WDIST(imode,4)=99.9
      WSTA(imode,4)=0
      WDIST(imode,5)=99.9
      WSTA(imode,5)=0
C
C..OBTAIN X-Y COORDINATES FOR PRODUCTION ZONE
C
      INQUIRE (FILE=XYCOORD,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9415) XYCOORD
 9415 FORMAT(1X,'STAWLK 9415 (F) X-Y COORD INPUT FILE XYCOORD=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9415) XYCOORD
      STOP 8
      ELSE
      open(32,file=XYCOORD,form='formatted',status='old')
      END IF
 50    read(32,1011,end=60) node,xcoor,ycoor
C1011  format(i6,t13,f5.0,t24,f5.0)
 1011  format(i6,1x,f10.0,1x,f10.0)
       if(node.le.MAX_IZONES) then
       pycoord(node)=ycoor*scoord
       pxcoord(node)=xcoor*scoord
       elseif(node.gt.MAX_IZONES.and.node.le.MAX_ZONES) then
       sycoord(node)=ycoor*scoord
       sxcoord(node)=xcoor*scoord
       endif
       goto 50
   60 continue
C
C..COMPUTE DISTANCE TO EACH STATION NODE BASED UPON X-Y COORDINATES
C
C
      DO 102 SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      IF(STANUM(SC).NE.IMODE) GOTO 102
	if(stadata(sc,6).ne.1.0) goto 102
	XDIST=SQRT(((PXCOORD(iz)-SXCOORD(ic))**2.0)+
     * ((PYCOORD(iz)-SYCOORD(ic))**2.0))
        IF(STADATA(SC,8).EQ.2.AND.XDIST.LE.URBDIST) GO TO 103
        IF(STADATA(SC,8).EQ.3.AND.XDIST.LE.URGDIST) GO TO 103
	IF(XDIST.GT.1.0) GO TO 102
C
C..EVALUATE FOR FOUR CLOSEST STATIONS
C
  103 CONTINUE
      IF(XDIST.LT.WDIST(IMODE,1)) THEN
         WDIST(IMODE,5)=WDIST(IMODE,4)
         WSTA(IMODE,5)=WSTA(IMODE,4)
         WDIST(IMODE,4)=WDIST(IMODE,3)
         WSTA(IMODE,4)=WSTA(IMODE,3)
         WDIST(IMODE,3)=WDIST(IMODE,2)
         WSTA(IMODE,3)=WSTA(IMODE,2)
         WDIST(imode,2)=WDIST(imode,1)
         WSTA(imode,2)=WSTA(imode,1)
         WDIST(imode,1)=XDIST
         WSTA(imode,1)=IC
         GO TO 102
      END IF
      IF(XDIST.LT.WDIST(IMODE,2)) THEN
         WDIST(IMODE,5)=WDIST(IMODE,4)
         WSTA(IMODE,5)=WSTA(IMODE,4)
         WDIST(IMODE,4)=WDIST(IMODE,3)
         WSTA(IMODE,4)=WSTA(IMODE,3)
         WDIST(IMODE,3)=WDIST(IMODE,2)
         WSTA(IMODE,3)=WSTA(IMODE,2)
         WDIST(imode,2)=XDIST
         WSTA(imode,2)=IC
         GO TO 102
      END IF
      IF(XDIST.LT.WDIST(IMODE,3)) THEN
         WDIST(IMODE,5)=WDIST(IMODE,4)
         WSTA(IMODE,5)=WSTA(IMODE,4)
         WDIST(IMODE,4)=WDIST(IMODE,3)
         WSTA(IMODE,4)=WSTA(IMODE,3)
         WDIST(IMODE,3)=XDIST
         WSTA(IMODE,3)=IC
         GO TO 102
      END IF
      IF(XDIST.LT.WDIST(IMODE,4)) THEN
         WDIST(IMODE,5)=WDIST(IMODE,4)
         WSTA(IMODE,5)=WSTA(IMODE,4)
         WDIST(IMODE,4)=XDIST
         WSTA(IMODE,4)=IC
         GO TO 102
      END IF
      IF(XDIST.LT.WDIST(IMODE,5)) THEN
         WDIST(IMODE,5)=XDIST
         WSTA(IMODE,5)=IC
      END IF
  102 CONTINUE
      IF(WSTA(IMODE,1).LE.0) WDIST(IMODE,1)=0.0
      IF(WSTA(IMODE,2).LE.0) WDIST(IMODE,2)=0.0
      IF(WSTA(IMODE,3).LE.0) WDIST(IMODE,3)=0.0
      IF(WSTA(IMODE,4).LE.0) WDIST(IMODE,4)=0.0
      IF(WSTA(IMODE,5).LE.0) WDIST(IMODE,5)=0.0
C...................................................................
      IF(DEBUG) THEN      
      SC=WSTA(imode,1)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS 
      SC2=WSTA(imode,2)-MAX_IZONES
      IF(SC2.LT.0) SC2=MAX_STATIONS 
      SC3=WSTA(IMODE,3)-MAX_IZONES
      IF(SC3.LT.0) SC3=MAX_STATIONS
      SC4=WSTA(IMODE,4)-MAX_IZONES
      IF(SC4.LT.0) SC4=MAX_STATIONS
      SC5=WSTA(IMODE,5)-MAX_IZONES
      IF(SC5.LT.0) SC5=MAX_STATIONS
      WRITE(26,9007) NAME(IMODE),IZ,WSTA(imode,1),STANAME(SC),
     *               WDIST(imode,1),
     *               WSTA(imode,2),STANAME(SC2),WDIST(imode,2),
     *               WSTA(imode,3),STANAME(SC3),WDIST(imode,3),
     *               WSTA(imode,4),STANAME(SC4),WDIST(imode,4),
     *               WSTA(imode,5),STANAME(SC5),WDIST(imode,5)
 9007 FORMAT(//1X,'Candidate Station Selection (Walk Access) -- ',a13/
     *       1X,'-------------------------------------------'/
     *       1X,'ORIGIN   ZONE   NUMBER  =',I9/  
     *       1X,'CLOSEST STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'SECOND  STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'THIRD   STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'FOURTH  STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'FIFTH   STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3)
      END IF
C....................................................................
 9001 close(32,status='keep')
      RETURN
      END
