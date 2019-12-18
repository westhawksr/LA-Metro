      SUBROUTINE MTXCONV(FILEIN,ZEQUIV,FILEOUT)
C
      INCLUDE 'mtamcpar.inc'
      include 'tpcom.inc'
      include 'control.inc'
C
      CHARACTER*80   FILEIN,FILEOUT
      INTEGER*2      ZEQUIV(3800)
      INTEGER*2      PURP,DUMMY,IO(500),IZ
      INTEGER*2      ZONMAX,PURNUM,IIZ,JJZ
      INTEGER*4      OUTMTX(MAX_ZONES,MAX_ZONES)
      INTEGER*4      ROW(MAX_ZONES),VAR(3800),T
      INTEGER*4      INSUM(35),OUTSUM(35)
      CHARACTER*7    TEMPFILE(35)
      DATA           TEMPFILE/'U01.MTX','U02.MTX','U03.MTX',
     *                        'U04.MTX','U05.MTX','U06.MTX',
     *                        'U07.MTX','U08.MTX','U09.MTX',
     *                        'U10.MTX','U11.MTX','U12.MTX',
     *                        'U13.MTX','U14.MTX','U15.MTX',
     *                        'U16.MTX','U17.MTX','U18.MTX',
     *                        'U19.MTX','U20.MTX','U21.MTX',
     *                        'U22.MTX','U23.MTX','U24.MTX',
     *                        'U25.MTX','U26.MTX','U27.MTX',
     *                        'U28.MTX','U29.MTX','U30.MTX',
     *                        'U31.MTX','U32.MTX','U33.MTX',
     *                        'U34.MTX','U35.MTX'/
C
      INSUM=0
      OUTSUM=0
      OPEN(40,FILE=FILEIN,
     *        STATUS='OLD',FORM='UNFORMATTED')
      READ(40) HEAD1,HEAD2
      ZONMAX=MAXZON
      PURNUM=NUMPUR
      CLOSE(40,STATUS='KEEP')
C...CREATE TEMPORARY FILES
      DO T=1,PURNUM
      OUTMTX=0
      OPEN(40,FILE=FILEIN,
     *        STATUS='OLD',FORM='UNFORMATTED')
      READ(40) HEAD1,HEAD2 
      PURP=T
C..STORE MATRIX VALUE
      DO IZ=1,ZONMAX
      CALL INTAB(40,VAR,IZ,PURP,DUMMY,IO)
      DO JZ=1,ZONMAX
      IIZ=ZEQUIV(IZ)
      JJZ=ZEQUIV(JZ)
      IF(IIZ.EQ.0.OR.JJZ.EQ.0) CYCLE
      OUTMTX(IIZ,JJZ)=OUTMTX(IIZ,JJZ)+VAR(JZ)
      INSUM(T)=INSUM(T)+VAR(JZ)
      END DO
      END DO
      NUMPUR=1
      MAXZON=MAX_ZONES
      OPEN(T,FILE=TEMPFILE(T),STATUS='UNKNOWN',FORM='UNFORMATTED')
      WRITE(T) HEAD1,HEAD2
      PURP=1
C...OUTPUT TEMPORARY FILE
      DO IZ=1,MAX_ZONES
      DO JZ=1,MAX_ZONES
      ROW(JZ)=OUTMTX(IZ,JZ)
      END DO
      CALL OUTAB(T,ROW,IZ,PURP,DUMMY,IO)
      END DO
      CLOSE(T,STATUS='KEEP')
      CLOSE(40,STATUS='KEEP')
      END DO
C -------------------------------------------------------------------
C        MERGE TABLES INTO SINGLE MATRIX
C -------------------------------------------------------------------
      OPEN(41,FILE=FILEOUT,STATUS='UNKNOWN',FORM='UNFORMATTED')
      NUMPUR=PURNUM
      MAXZON=MAX_ZONES
      WRITE(41) HEAD1,HEAD2
      DO T=1,NUMPUR
      OPEN(T,FILE=TEMPFILE(T),STATUS='OLD',
     *       FORM='UNFORMATTED')
      READ(T) HEAD1,HEAD2
      END DO
      DO IZ=1,MAX_ZONES
      DO T=1,PURNUM
      PURP=1
      CALL INTAB(T,ROW,IZ,PURP,DUMMY,IO)
      DO JZ=1,MAX_ZONES
      OUTSUM(T)=OUTSUM(T)+ROW(JZ)
      END DO
      PURP=T
      CALL OUTAB(41,ROW,IZ,PURP,DUMMY,IO)
      END DO
      END DO
      CLOSE(41,STATUS='KEEP')
      WRITE(46,9001)
 9001 FORMAT('  MATRIX TOTAL COMPARISON'/
     *       ' TABLE    INPUT     OUTPUT'/
     *       ' -----  ---------  ---------')
      DO T=1,PURNUM
      WRITE(46,9002) T,INSUM(T),OUTSUM(T)
 9002 FORMAT(2X,I2,4X,I9,2X,I9)
      END DO
C....DELETE TEMPORARY FILES
      DO T=1,PURNUM
      CLOSE(T,STATUS='DELETE')
      END DO
      RETURN
      END
