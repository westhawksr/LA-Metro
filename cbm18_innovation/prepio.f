C ********************************************************************
C
C Prepare to read matrices that will be accessed during the zone loop:
C
C 1. Open each matrix file.
C 2. Read TRANPLAN file header for each matrix file.
C 3. Check the number of zones in the file against the number input
C    in the control file.
C
C ********************************************************************
      subroutine prepio(filename,fno)
c
c
      include 'param.com'
      include 'tpcom.inc'
      include 'control.inc'
      include 'mtamcpar.inc'
c
      integer*4 fno
      character*80 filename
      logical   exists
c
c   
      INQUIRE (FILE=filename,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9015) filename,fno
      WRITE(*,9015) filename,fno
 9015 FORMAT(//1X,'PREPIO 9015 (W) INPUT FILE NAMED=',A80,
     *       ' DOES NOT EXIST FOR FILE NO=',I3/)
      STOP 8
      ELSE     
	    open(unit=fno,file=filename,
     *	  form='unformatted',status='old')
      end if
        read(fno) HEAD1, HEAD2
       if(MAXZON.gt.MAX_ZONES) then
         write(*,1001) filename
 1001   format(/,' error: maximum zones exceeded in ',a80)
      stop ' -- '
      endif
      return
      end
      
      
