C***********************************************************
C     parking cost model computation                       *
C***********************************************************
      subroutine parkcost(zhhd,aprkzone,aprkcst)
      include 'stadat.com'
      include 'param.com'
      include 'mtamcpar.inc'
      include 'tpcom.inc'
      include 'control.inc'
	    integer*2   iz,jz,qzone(5),indval
	    integer*2   aprkzone(4000)
      real*4      zhhd(34,max_izones)
	    real*4      hdist(4000),aprkcst(4000)
	    real*4      temp,tarea,sdist,zdensity,rtemp
      real*4      denom,supply,fecost(4000),pcost
      real*4      a,b,c,d,e,y
      real*4      qprkcst(5),ranval,adjval
C
      a=35.0
      b=-83.49
      sdist=0.5
      aprkcst=0.0
      aprkzone=0
c ---------------------------------------------------------------
c     determine alternate parking location
c --------------------------------------------------------------- 
      if(avmdl.and.altpark) then
      write(*,55558)
55558 format(' Alternative Parking Location Model'/)
      if(debug) then
      open(141,file='altpark_model.csv',status='unknown',
     *                   form='formatted')
      write(141,55553)
55553 format('dest_zone,altzone,altcost')
      end if
      call prepio(dask,18) 
      do iz=1,max_izones
      nk=mod(iz,1000)
      if(nk.EQ.0.and.debug) WRITE(*,8001) iz
 8001 FORMAT(' Processing Zone=',I5)
      purp=2
      call intab(18,var,iz,purp,dummy,io)
      do jz=1,max_zones
      hdist(jz)=float(var(jz))/100.0
      end do
      index=0
      qprkcst=0.0
      qzone=0
      do jz=1,max_izones
      if((zhhd(5,jz).gt.0).and.(hdist(jz).lt.5.0)) then
      if(zhhd(15,iz).le.0.0) cycle
      if(zhhd(5,jz).lt.zhhd(5,iz)) then
      index=index+1
      qprkcst(index)=zhhd(5,jz)
      qzone(index)=jz
      if(index.eq.5) go to 600
      end if
      end if
      end do  
  600 continue
      if(debug.and.sdetail.and.zhhd(5,iz).gt.0) 
     *  write(26,55557) iz,index,zhhd(5,iz)
55557 format(' iz=',i4,' index=',i1,' parkcost=',f8.2)
      if(index.eq.0) cycle
      do k=1,5
      if(debug.and.sdetail) write(26,55556) qzone(k),qprkcst(k)
55556 format(' qzone=',I4,' qprkcst=',F8.2)
      end do
      if(index.eq.1) then
      indval=1
      else
  500 call random(ranval)
      adjval=ranval*float(index)
      indval=ifix(adjval)  
      if(indval.le.0) go to 500
      end if
      aprkcst(iz)=qprkcst(indval)
      aprkzone(iz)=qzone(indval)
      if(debug.and.sdetail) then
      write(26,55555) ranval,adjval,indval,aprkcst(iz),
     *                aprkzone(iz)
55555 format(' randval=',f6.2,' adjval=',f6.2,' indval=',i2,
     *       ' aprkcst=',f8.2,' aprkzone=',i4)
      end if
      if(debug) write(141,55554) iz,aprkzone(iz),aprkcst(iz)
55554 format(i4,',',i4,',',f8.2)
      end do
c
      close(18,status='keep')
      if(debug) WRITE(*,8002) max_izones
 8002 FORMAT(' Processing Zone=',I5//)
      end if
      return
      end
