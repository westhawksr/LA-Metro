C-------------------------------------------------------------------
C      OUTPUT TRIP MATRIX WITH BUCKET ROUNDING
C-------------------------------------------------------------------
      SUBROUTINE TABOUT(UNIT,IZ,PURP,IMTX)
       include 'mtamcpar.inc'
      INTEGER*4  UNIT,ROW(MAX_ZONES)
      INTEGER*2  IZ,JZ,PURP,DUMMY,IO(500)
      REAL*4     TEMP,REML
      REAL*8     IMTX(MAX_ZONES)
C
      REML=0.0
      DO 1,JZ=1,MAX_ZONES
      IF(IMTX(JZ).GT.0.0) THEN
      TEMP=(IMTX(JZ)*100.0)+ REML
      ROW(JZ)=IFIX(TEMP)
      REML=TEMP-ROW(JZ)
      ELSE
      ROW(JZ)=0
      END IF
    1 CONTINUE
      CALL OUTAB(UNIT,ROW,IZ,PURP,DUMMY,IO)
      RETURN
      END
