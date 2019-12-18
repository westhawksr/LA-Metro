      SUBROUTINE OUTAB(LUN,TABLE,ORIGIN,PURP,NDUMMY,IO)
      IMPLICIT INTEGER*2 (A-Z)
C
C     ROUTINE TO PACK FROM TABLE(MAXZON) INTO IO(500) AND WRITE LUN
C
C     PACKING FORM IS -- 2-BIT CODE: 00 DESTINATION(IF ZERO, THEN LAST)
C                                    01 LOWER 14 BITS
C                                    10 MIDDLE 14 BITS
C                                    11 UPPER 4 BITS
C
C                     -- 14-BIT VALUE
C
      INCLUDE 'TPCOM.INC'
      INCLUDE 'CONTROL.INC'
      INTEGER*4 LUN,TABLE(1),IV,MID,IUP,MASK14,ONE,MLOWER,MIDDLE,
     1          MUPPER,TEMP4
      INTEGER*2 IO(1),TEMP2(2)
      EQUIVALENCE (TEMP2(1),TEMP4)
      LOGICAL*2 AGAIN,SKIPD
C
C     DATA MASK14/:37777/,MLOWER/:40000/,MIDDLE/:100000/,MUPPER/:140000/
      DATA MASK14/16383/,ONE/1/
C
C     INITIALIZE
C
      MLOWER = ISHFT(ONE,14)
      MIDDLE = ISHFT(ONE,15)
      MUPPER = IOR(MLOWER,MIDDLE)
      AGAIN = .FALSE.
      DEST = 0
      IO(1) = ORIGIN
      IO(2) = PURP
      N = 3
C
   90 SKIPD = .FALSE.
  100 DEST = DEST+1
      IF (DEST.EQ.MAXZON) GO TO 150
      IV = TABLE(DEST)
      IF (IV.EQ.0) GO TO 90
C
C     NON-ZERO, CHECK IF SPACE IN IO(500) FOR IT
C
  106 IF (N.GT.495) GO TO 200
      IF (SKIPD) GO TO 120
C
C     WRITE DESTINATION NUMBER -- THIS IS ALSO START OF NEW IO FILL
C
  110 IO(N) = DEST
      SKIPD = .TRUE.
      N = N+1
C
C     WRITE VALUES -- TEST FOR LARGE NUMBERS
C
  120 MID = ISHFT(IV,-14)
      IF (MID.EQ.0) GO TO 140
      IUP = ISHFT(MID,-14)
      IF (IUP.EQ.0) GO TO 130
      MID = IAND(MID,MASK14)
C
C     WRITE UPPER VALUE
C
      TEMP4 = IOR(IUP,MUPPER)
      IF (MACHIN.EQ.'SUN'.OR.MACHIN.EQ.'IBMAIX') THEN
          IO(N) = TEMP2(2)
      ELSE
          IO(N) = TEMP2(1)
      ENDIF
      N = N+1
C
C     WRITE MIDDLE VALUE
C
  130 TEMP4 = IOR(MID,MIDDLE)
      IF (MACHIN.EQ.'SUN'.OR.MACHIN.EQ.'IBMAIX') THEN
          IO(N) = TEMP2(2)
      ELSE
          IO(N) = TEMP2(1)
      ENDIF
      IV = IAND(IV,MASK14)
      N = N+1
C
C     WRITE LOWER VALUE
C
  140 TEMP4 = IOR(IV,MLOWER)
      IF (MACHIN.EQ.'SUN'.OR.MACHIN.EQ.'IBMAIX') THEN
          IO(N) = TEMP2(2)
      ELSE
          IO(N) = TEMP2(1)
      ENDIF
      IF (DEST.EQ.MAXZON) GO TO 300
      N = N+1
      GO TO 100
C
C     SET SKIPD = .FALSE. FOR LAST DESTINATION
C
  150 SKIPD = .FALSE.
      IV = TABLE(DEST)
      GO TO 106
C
C     SET FLAG FOR ANOTHER RECORD
C
  200 AGAIN = .TRUE.
      N = N-1
C
C     WRITE A RECORD
C
  300 WRITE (LUN) N,(IO(I),I=1,N)
      IF (.NOT.AGAIN) GO TO 400
      AGAIN = .FALSE.
      GO TO 410
C
C     TEST IF LAST ZONE OR CONTINUE
C
  400 IF (DEST.EQ.MAXZON) GO TO 9999
  410 N = 3
      GO TO 110
C
 9999 RETURN
      END
