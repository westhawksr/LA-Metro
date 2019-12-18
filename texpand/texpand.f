C---------------------------------------------------------------------
C     EXPAND CBM18 TRIP MATRICES TO PHASE 2 ZONE SYSTEM
C
C     DUNBAR TRANSPORTATION, LLC
C       SAN RAMON, CALIFORNIA
C
C---------------------------------------------------------------------
      PROGRAM TEXPAND
C
      INCLUDE 'mtamcpar.inc'
      include 'tpcom.inc'
      include 'control.inc'
c
      INTEGER*2      ZEQUIV(3800),STAT1,IFILERR(9)
      INTEGER*2      PH2ZONE,CBMZONE,K1,K2
      INTEGER*2      PURP,DUMMY,IO(500),IZ
      INTEGER*2      ZONMAX,PURNUM,IIZ,JJZ
      INTEGER*4      ROW(MAX_ZONES),VAR(3800),T
      CHARACTER*8    FILELIST(9)
      CHARACTER*40   NRECORD,SNAME
      CHARACTER*80   FBRT,FBU1,FBU2,FCMR,FCRURBRT
      CHARACTER*80   FHAU,FURR,FZEQUIV,FSEQUIV,FRPORT
      CHARACTER*80   FBRT_OUT,FBU1_OUT,FBU2_OUT,FCMR_OUT
      CHARACTER*80   FCRURBRT_OUT,FHAU_OUT,FURR_OUT
      CHARACTER*80   CONTROL,INFILE,HEADER
      LOGICAL        EXISTS,CHKERR,ZONCHK
      DATA           FILELIST/'FBRT','FBU1','FBU2','FCMR',
     *                        'FCRURBRT','FHAU','FURR',
     *                        'FZEQUIV','FSEQUIV'/
      NAMELIST  /PARAMS/ FBRT,FBU1,FBU2,FCMR,FCRURBRT,
     *                   FHAU,FURR,FZEQUIV,FSEQUIV,FRPORT,
     *                   FBRT_OUT,FBU1_OUT,FBU2_OUT,FCMR_OUT,
     *                   FCRURBRT_OUT,FHAU_OUT,FURR_OUT
      IFILERR=0
      CHKERR=.FALSE.
      ZONCHK=.FALSE.
C
C WRITE PROGRAM BANNER, DATE & TIME
C
      CALL GETDAT(IYR,IMON,IDAY)
      CALL GETTIM(IHR,IMIN,ISEC,I100)
      IYR=IYR-2000
      WRITE(*,8001) IMON,IDAY,IYR,IHR,IMIN,ISEC
 8001 FORMAT(/20X,' PROGRAM TEXPAND (11Oct18)',/,
     *        20X,' -------------------------',/,
     *        20X,'      [Version 0.9]',//,
     *        20X,'     Date:',I2,'/',I2,'/',I2/
     *        20X,'     Time:',I2,':',I2,':',I2//)
C
C     OPEN AND READ THE CONTROL FILE
C
      CALL GETARG(1,CONTROL,STAT1)
      IF(STAT1.LE.0) THEN
        WRITE(*,'('' TEXPAND (F) : CONTROL FILE NOT SPECIFIED'')')
        STOP " ENTER THE CONTROL FILE NAME ON THE COMMAND LINE."
      END IF
      OPEN(10,FILE=CONTROL,STATUS='OLD',FORM='FORMATTED',
     *     ERR=9)
      REWIND 10
      GO TO 8
    9 WRITE(*,9001)
 9001 FORMAT(/,1X,'TEXPAND 9001 (F) ERROR OPENING CONTROL FILE INPUT ',
     *       'OR DATA SET NOT FOUND')
      STOP 8
    8 READ(10,PARAMS)
      CLOSE(10,STATUS='KEEP')
      OPEN(46,FILE=FRPORT,STATUS='UNKNOWN',FORM='FORMATTED')   
      WRITE(46,8001) IMON,IDAY,IYR,IHR,IMIN,ISEC
C
C     READ & STORE ZONE EQUIVALENCE
C
      OPEN(10,FILE=FZEQUIV,STATUS='OLD',FORM='FORMATTED')
      READ(10,'(A80)') HEADER
    1 READ(10,*,ERR=2,END=5) PH2ZONE,CBMZONE
      ZEQUIV(CBMZONE)=PH2ZONE
      GO TO 1
    2 WRITE(*,9004) PH2ZONE
 9004 FORMAT(/,1X,'TEXPAND 9004 (F) ERROR READING ZONE EQUIVALENCE',
     *            ' FILE AT PHASE 2 ZONE=',I4)
      STOP
    5 CONTINUE
      CLOSE(10,STATUS='KEEP')
C
C     CHECK FOR MISSING VALUES
C
      DO IZ=1,3017
      IF(ZEQUIV(IZ).LE.0) ZONCHK=.TRUE.
      END DO
      IF(ZONCHK) THEN
      WRITE(46,4)
    4 FORMAT(' THE FOLLOWING CBM 18 ZONES HAVE NO PHASE 2 EQUIVALENCE'/
     *       ' ------------------------------------------------------')
      DO IZ=1,3017
      IF(ZEQUIV(IZ).LE.0) WRITE(46,3) IZ
    3 FORMAT(5X,I4)
      END DO
      END IF
C
C     READ & STORE STATION EQUIVALENCE
C
      OPEN(10,FILE=FSEQUIV,STATUS='OLD',FORM='FORMATTED')
      READ(10,'(A80)') HEADER
   10 READ(10,'(A40)',ERR=12,END=15) NRECORD
C..PHASE 2 STATION
      K1=1
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(1:K2),'(I4)') PH2ZONE
C..STATION NAME
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),'(A40)') SNAME
C..CBM STATION
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),'(I4)') CBMZONE
      ZEQUIV(CBMZONE)=PH2ZONE
      GO TO 10
   12 WRITE(46,9005) PH2ZONE
 9005 FORMAT(/,1X,'TEXPAND 9005 (F) ERROR READING STATION EQUIVALENCE',
     *            ' FILE AT PHASE 2 STATION=',I4)
   15 CONTINUE
      CLOSE(10,STATUS='KEEP')
C
C     INQUIRE ON FILE AVAILABILITY
C
      INQUIRE (FILE=FBRT,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      IFILERR(1)=1
      END IF
      DO K=1,9
      IF(IFILERR(K).GT.0) THEN
      WRITE(46,9002) FILELIST(K)
 9002 FORMAT(1X,'TEXPAND 9002 (W) ',A8,' INPUT FILE NOT FOUND')
      CHKERR=.TRUE.
      END IF
      END DO
      IF(CHKERR) THEN
      WRITE(46,9003)
 9003 FORMAT(1X,'TEXPAND 9003 (F) ONE OR MORE INPUT FILES',
     *          ' NOT FOUND')
      STOP 9003
      END IF  
C
C     MATRIX CONVERSION
C
      WRITE(46,8002) FBRT
      WRITE(*,8002)  FBRT
 8002 FORMAT(/' MATRIX CONVERSION FOR BRT'/
     *        ' -------------------------'/
     *        ' FBRT=',A80//)
      CALL MTXCONV(FBRT,ZEQUIV,FBRT_OUT)
C.
      WRITE(46,8003) FURR
      WRITE(*,8003)  FURR
 8003 FORMAT(/' MATRIX CONVERSION FOR URR'/
     *        ' -------------------------'/
     *        ' FURR=',A80//)
      CALL MTXCONV(FURR,ZEQUIV,FURR_OUT)
C.
      WRITE(46,8004) FCMR
      WRITE(*,8004)  FCMR
 8004 FORMAT(/' MATRIX CONVERSION FOR CMR'/
     *        ' -------------------------'/
     *        ' CMR=',A80//)
      CALL MTXCONV(FCMR,ZEQUIV,FCMR_OUT)
C
      WRITE(46,8005) FBU1
      WRITE(*,8005)  FBU1
 8005 FORMAT(/' MATRIX CONVERSION FOR BU1'/
     *        ' -------------------------'/
     *        ' FBU1=',A80//)
      CALL MTXCONV(FBU1,ZEQUIV,FBU1_OUT)
C.
      WRITE(46,8006) FBU2
      WRITE(*,8006)  FBU2
 8006 FORMAT(/' MATRIX CONVERSION FOR BU2'/
     *        ' -------------------------'/
     *        ' FBU2=',A80//)
      CALL MTXCONV(FBU2,ZEQUIV,FBU2_OUT)
C.
      WRITE(46,8007) FHAU
      WRITE(*,8007)  FHAU
 8007 FORMAT(/' MATRIX CONVERSION FOR HAU'/
     *        ' -------------------------'/
     *        ' FHAU=',A80//)
      CALL MTXCONV(FHAU,ZEQUIV,FHAU_OUT)
C
      WRITE(46,8008) FCRURBRT
      WRITE(*,8008)  FCRURBRT
 8008 FORMAT(/' MATRIX CONVERSION FOR CRURBRT'/
     *        ' -------------------------'/
     *        ' FCRURBRT=',A80//)
      CALL MTXCONV(FCRURBRT,ZEQUIV,FCRURBRT_OUT)
C
      CALL GETTIM(IHR,IMIN,ISEC,I100)
      WRITE(46,9006) IHR,IMIN,ISEC
      WRITE(*,9006) IHR,IMIN,ISEC
 9006 FORMAT(/' Program Completed: ',I2,':',I2,':',I2)
      END
C-------------------------------------------------------------------
C      READ AND RETURN VALUE FROM RECORD STRING
C-------------------------------------------------------------------
      SUBROUTINE STAREAD(K1,K2,SRECORD)
      INTEGER*2      K1,K2
      CHARACTER*40   SRECORD
      CHARACTER*1    COMMA
      DATA           COMMA/','/
      K2=0
      DO 9000 K=K1,40
      IF(SRECORD(K:K).EQ.COMMA) THEN
       K2=K-1
      RETURN
      END IF
 9000 CONTINUE
      K2=K1+20
      IF(K2.GT.40) K2=40
      RETURN
      END
