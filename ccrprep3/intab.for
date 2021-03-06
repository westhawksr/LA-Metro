      SUBROUTINE INTAB(LUN,TABLE,ORIGIN,PURP,NDUMMY,IO)
      IMPLICIT INTEGER*2 (A-Z)
C
C     ROUTINE TO READ LUN INTO IO(500) AND UNPACK INTO TABLE(MAXZON)
C
C     PACKING FORM IS -- 2-BIT CODE: 00 DESTINATION(IF ZERO, THEN LAST)
C                                    01 LOWER 14 BITS
C                                    10 MIDDLE 14 BITS
C                                    11 UPPER 4 BITS
C
C                     -- 14-BIT VALUE
C
C*****************************************************************
      INCLUDE 'TPCOM.INC'
      INCLUDE 'CONTROL.INC'
      INCLUDE 'LUNCOM.INC'
      INTEGER*4 LUN,TABLE(1),VAL4
      INTEGER*2 IO(500)
C
C     DATA MASK14/:37777/ -- OCTAL 37777
      DATA MASK14/16383/
C
C     INITIALIZE
C
      DEST = 0
      DO 2 I=1,MAXZON
    2 TABLE(I) = 0
C
C     READ A BLOCK INTO IO
C
   10 READ (LUN,END=210) N,(IO(I),I=1,N)
      IF (IO(1)-ORIGIN) 10,20,230
   20 IF (IO(2)-PURP) 10,30,240
C
C     GOT THE DESIRED ORIGIN/PURPOSE COMBINATION
C
   30 IP = 2
   50 DEST = DEST+1
   60 IP = IP+1
      IF (IP.GT.N) GO TO 10
C
C     GET A BYTE
C
      IBYTE = IO(IP)
      ICODE = ISHFT(IBYTE,-14)+1
      IBYTE = IAND(IBYTE,MASK14)
      VAL4 = IBYTE
      GO TO (80,90,100,110),ICODE
C
C     (00) CASE -- DESTINATION (IF ZERO, LAST WORD IN RECORD)
C
   80 DEST = IBYTE
      IF (DEST.EQ.0) GO TO 10
      GO TO 60
C
C     (01) CASE -- LOWER 14 BITS
C
   90 TABLE(DEST) = IOR(TABLE(DEST),VAL4)
      IF (DEST.EQ.MAXZON) GO TO 9999
      GO TO 50
C
C     (10) CASE -- MIDDLE 14 BITS
C
  100 TABLE(DEST) = IOR(TABLE(DEST),ISHFT(VAL4,14))
      GO TO 60
C
C     (11) CASE -- UPPER 4 BITS (COULD BE NEGATIVE)
C
  110 TABLE(DEST) = ISHFT(VAL4,28)
      GO TO 60
C
C     ERROR MESSAGES
C
  210 WRITE (TPOUT,211) JTAPE(LUN),ORIGIN,PURP
  211 FORMAT (//' *** IN FILE ',A8,' AT ZONE',I5,' AND TABLE',I3,
     1          ' PREMATURE EOF ***')
      GO TO 250
  230 WRITE (TPOUT,231) LUN,ORIGIN,PURP,IO(1),IO(2)
  231 FORMAT (//' *** IN FILE ',I8,' *** ZONAL ORDER ERROR ***'/
     1          ' LOOKING FOR ZONE',I5,' AND TABLE',I3,
     2          '    -- FOUND ZONE',I5,' AND TABLE',I3)
      GO TO 250
  240 WRITE (TPOUT,241) LUN,ORIGIN,PURP,IO(1),IO(2)
  241 FORMAT (//' *** IN FILE ',I8,' *** TABLE ORDER ERROR ***'/
     1          ' LOOKING FOR ZONE',I5,' AND TABLE',I3,
     2          '    -- FOUND ZONE',I5,' AND TABLE',I3)
  250 REWIND LUN
      STOP                                                              <test_CB>
C
 9999 RETURN
      END
