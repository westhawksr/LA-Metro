C-------------------------------------------------------------------
C      CREATE MODE SHARE VALUES
C-------------------------------------------------------------------
      SUBROUTINE SHAREVAL(OBSVALR,LOW,HIGH)
      INCLUDE 'param.com'
      include 'tpcom.inc'
      include 'control.inc'
	    include 'mtamcpar.inc'
C
C DATA DECLARATIONS
C
      INTEGER*4 LOW,HIGH,DIFF
      REAL*8    DENOM,NUM(4)
      REAL*8    OBSVALR(24,MAX_ZONES)
      INTEGER*2 IZ,JZ
C
      DIFF=HIGH-LOW+1
      DO 100 JZ=1,MAX_ZONES
      NUM(1)=0.0
      NUM(2)=0.0
      NUM(3)=0.0
      NUM(4)=0.0
      DO 200 IZ=1,DIFF
      NUM(IZ)=OBSVALR(((LOW-1)+IZ),JZ)
  200 CONTINUE
      DENOM=NUM(1)+NUM(2)+NUM(3)+NUM(4)
      IF(DENOM.GT.0) THEN
      DO 300 IZ=1,DIFF
      NUM(IZ)=NUM(IZ)/DENOM
      OBSVALR(((LOW-1)+IZ),JZ)=NUM(IZ)
  300 CONTINUE
      END IF
  100 CONTINUE
      RETURN
      END
