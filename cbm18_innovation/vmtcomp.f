C***********************************************************
C     VMT COMPUTATIONS FOR FIXED GUIDEWAY ACCESS           *
C***********************************************************
      SUBROUTINE VMTCOMP(imode,iz,trips,osta,vmt,vmtcode,zhhd)
      include 'stadat.com'
      include 'param.com'
	    include 'mtamcpar.inc'
c
      integer*2   imode,osta(5,14),vmtcode(2,2),iz,smode
      integer*2   t,sta,zone,cntyorg,cntydst,cntyindx,index
      real*4      zhhd(34,max_izones)
      real*8      vmt(48,4),trips(18)
c
      smode=imode
      if(imode.eq.5) smode=3
c...county of origin zone
      cntyorg=ifix(zhhd(6,iz))
      if(cntyorg.gt.2) cntyorg=2
c...park-and-ride
      do t=5,8
c...access station
      sta=osta(imode,t)-max_izones
      zone=staref(sta)
      cntydst=ifix(zhhd(6,zone))
      if(cntydst.gt.2) cntydst=2
      cntyindx=vmtcode(cntyorg,cntydst)
      index=39+(smode-1)*3+1
      vmt(index,cntyindx)=vmt(index,cntyindx)+trips(t)
c     write(26,9001) iz,imode,t,osta(imode,t),cntyorg,zone,
c    *               cntydst,cntyindx,trips(t),index,
c    *               vmt(index,cntyindx)
 9001 format(' iz=',i4,' imode=',i1,' t=',i2,' osta=',i4,
     *       ' cntyorg=',i1,' zone=',i4, ' cntydst=',i4,
     *       ' cntyindx=',i1,
     *       ' trips=',f8.2,' index=',i2,' vmt=',f8.1)
      end do
c...kiss-and-ride
      do t=9,12
c...access station
      sta=osta(imode,t)-max_izones
      zone=staref(sta)
      cntydst=ifix(zhhd(6,zone))
      if(cntydst.gt.2) cntydst=2
      cntyindx=vmtcode(cntyorg,cntydst)
      index=40+(smode-1)*3+1
      vmt(index,cntyindx)=vmt(index,cntyindx)+trips(t)
      end do
c...uber
      do t=9,12
c...access station
      sta=osta(imode,t)-max_izones
      zone=staref(sta)
      cntydst=ifix(zhhd(6,zone))
      if(cntydst.gt.2) cntydst=2
      cntyindx=vmtcode(cntyorg,cntydst)
      index=41+(smode-1)*3+1
      vmt(index,cntyindx)=vmt(index,cntyindx)+trips(t+6)
      end do
      return
      end
     