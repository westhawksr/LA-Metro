C***********************************************************
C     FIND CLOSEST ZONE FOR EACH STATION                   *
C***********************************************************
      SUBROUTINE STANEAR(SXCOORD,SYCOORD)
      include 'stadat.com'
      include 'param.com'
	    include 'mtamcpar.inc'
c
      integer*2 sc,iz,minzone
      real*4    sxcoord(max_izones),sycoord(max_izones)
      real*4    xdist,mindist
c
      do sc=1,max_stations
      if(stadata(sc,6).ne.1.0) cycle
      mindist=999.9
      minzone=0
c..........................................................
c     if(sdetail) then
c     write(26,9001) (sc+max_izones),staname(sc)
c9001 format(' Closest Zone for Station ',i4,1x,a37)
c     end if
c.........................................................
      do iz=1,max_izones
      xdist=sqrt(((sxcoord(iz)-stadata(sc,23))**2.0)+
     *  ((sycoord(iz)-stadata(sc,24))**2.0))
c.........................................................
c     if(sdetail) then
c     write(26,9002) iz,xdist,mindist,minzone
c9002 format(' iz=',i4,' xdist=',f6.2,' mindist=',f6.2,
c    *       ' minzone=',i4)
c     end if
c.........................................................
      if(xdist.lt.mindist) then
      mindist=xdist
      minzone=iz
      end if
      end do
      staref(sc)=minzone
c........................................................
      if(sdetail) then
      write(26,9003) staref(sc),(sc+max_izones),staname(sc),mindist
 9003 format(' Closest Zone=',i4,' For Station=',i4,1x,a37,
     *       ' distance=',f6.2)
      end if
c.......................................................
      end do
      return
      end
