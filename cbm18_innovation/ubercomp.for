C-------------------------------------------------------------------
C     COMPUTE UBER UTILITY COMPONENTS SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE UBERCOMP(iz,htime,hdist,coeffwait,coefftime,
     *                    ubercost,uberacc,kwait,zhhd)
      include 'stadat.com'
      include 'param.com'
	    include 'mtamcpar.inc'
	    integer*2    iz
	    integer*4    yindex,zindex
	    real*4       coeffwait,coefftime
	    real*4       ranval,kwait,ubercost,uberacc
	    real*4       htime,hdist,zhhd(34,max_izones)
C
	    ubercost=0.0
	    uberacc=0.0
      zindex=ifix(zhhd(29,iz))
      if(zindex.eq.0) zindex=6
      if(zindex.gt.6) zindex=6
C
C..OBTAIN WAIT TIME FROM RANDOM DRAW OF FREQUENCY DISTRIBUTION
C
  100 call random(ranval)
      yindex=ifix(ranval*1000.0)
      if(yindex.le.0) go to 100
      kwait=fdist(yindex,zindex)
C
      ubercost=2.30+0.28*htime+0.80*hdist
      ubercost=amax1(ubercost,5.80)
      uberacc=coeffwait*kwait + coefftime*htime
      return
      end
