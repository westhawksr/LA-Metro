C-------------------------------------------------------------------
C      RAIL SELECT LINK INDICATOR SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE STASEL(SELIND,NAME,ZSTASTA)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'tpcom.inc'
       include 'control.inc'
       include 'mtamcpar.inc'
C
C DATA DECLARATIONS
C
      INTEGER*2     IC,SC,SC2,DC,T,STA1,STA2,T1,T2
      INTEGER*2     SELIND(MAX_STATIONS,MAX_STATIONS)
      INTEGER*2     ZSTASTA(MAX_STATIONS,MAX_STATIONS,2)
      INTEGER*2     URSTA1,URSTA2
      INTEGER*4     VAR2(4000),STA
      LOGICAL       EXISTS
      CHARACTER*13  NAME(5)
      SELIND=0
      ZSTASTA=0
C
C     CHECK FOR STATION SELECT LINK INDICATOR FILE
C
      INQUIRE (FILE=SELRAL,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      RAILSEL=.FALSE.
      RETURN
      ELSE
      OPEN(118,FILE=SELRAL,
     *        STATUS='OLD',FORM='UNFORMATTED')
      READ(118) HEAD1, HEAD2
      RAILSEL=.TRUE.
      IF(TDEBUG) THEN
      OPEN(119,FILE='STASEL_DEBUG.RPT',STATUS='UNKNOWN',
     *         FORM='FORMATTED')
      END IF
      WRITE(*,8000)
 8000 FORMAT(/1X,'Read and Store Select Link Indicator Data')
      END IF
C------------------------------------------------------
C  STATION-TO-STATION -- URBAN RAIL
C------------------------------------------------------
C
C  ORIGIN STATION LOOP 
C
      DO SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      IF(STADATA(SC,6).NE.1.0) CYCLE
C
C.....LINK INDICATOR (NUMBER OF LINKS)
C
      PURP=1
      CALL INTAB(118,VAR,IC,PURP,DUMMY,IO)
C
C   DESTINATION STATION LOOP
C
      DO SC2=1,MAX_STATIONS
      DC=SC2+MAX_IZONES
      IF(STADATA(SC2,6).NE.1.0) CYCLE
      IF(IC.EQ.DC) CYCLE
C
C     STORE VALUE
C
      SELIND(SC,SC2)=VAR(DC)
C....................................................................
      IF(TDEBUG.AND.SELIND(SC,SC2).GT.0) THEN
      WRITE(119,9029)  IC,STANAME(SC),DC,STANAME(SC2),
     *                SELIND(SC,SC2)
 9029 FORMAT(/1X,'STATION-TO-STATION SELECT LINK INDICATOR VALUE',
     *           ' -- URBAN RAIL PATH'/
     *       1X,'-----------------------------------------------'/
     *       1X,'ORIGIN            STATION=',I8,1X,A37/
     *       1X,'DESTINATION       STATION=',I8,1X,A37/
     *       1X,'NUMBER OF LINKS TRAVERSED=',I8/)
      END IF
C.......................................................................
      END DO
      END DO
      CLOSE(118,STATUS='KEEP')
C------------------------------------------------------
C  STATION-TO-STATION -- BLENDED PATH
C------------------------------------------------------
      OPEN(118,FILE=SELRAL,
     *        STATUS='OLD',FORM='UNFORMATTED')
      READ(118) HEAD1, HEAD2      
C
C  ORIGIN STATION LOOP 
C
      DO SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      IF(STADATA(SC,6).NE.1.0) CYCLE
      IF(STANUM(SC).EQ.3.OR.STANUM(SC).EQ.4) CYCLE
C
C.....LINK INDICATOR (NUMBER OF LINKS)
C
      PURP=2
      CALL INTAB(118,VAR,IC,PURP,DUMMY,IO)
C
C   DESTINATION STATION LOOP
C
      DO SC2=1,MAX_STATIONS
      DC=SC2+MAX_IZONES
      IF(STADATA(SC2,6).NE.1.0) CYCLE
      IF(STANUM(SC).EQ.STANUM(SC2)) CYCLE
      IF(STANUM(SC2).EQ.3.OR.STANUM(SC2).EQ.4) CYCLE
      IF(IC.EQ.DC) CYCLE
C
C     STORE VALUE
C
      SELIND(SC,SC2)=VAR(DC)
C....................................................................
      IF(TDEBUG.AND.SELIND(SC,SC2).GT.0) THEN
      WRITE(119,9030)  IC,STANAME(SC),NAME(STANUM(SC)),DC,STANAME(SC2),
     *                 NAME(STANUM(SC2)),SELIND(SC,SC2)
 9030 FORMAT(/1X,'STATION-TO-STATION SELECT LINK INDICATOR VALUE',
     *           ' -- BLENDED PATH'/
     *       1X,'-----------------------------------------------'/
     *       1X,'ORIGIN            STATION=',I8,1X,A37,1X,A13/
     *       1X,'DESTINATION       STATION=',I8,1X,A37,1X,A13/
     *       1X,'NUMBER OF LINKS TRAVERSED=',I8/)
      END IF
C.......................................................................
      IF(SELIND(SC,SC2).GT.0) THEN
      URSTA1=0
      URSTA2=0
C.....URBAN RAIL ORIGIN STATION
      IF(STANUM(SC).EQ.2) THEN
      URSTA1=SC
      DO T=1,5
      T1=(T-1)*2+1
      T2=(T-1)*2+2
      STA1=XFERSTA(SC,SC2,T1)-MAX_IZONES
      STA2=XFERSTA(SC,SC2,T2)-MAX_IZONES
      IF(STA1.GT.0.AND.STA2.GT.0) THEN
      IF(TDEBUG) WRITE(119,9031) T,XFERSTA(SC,SC2,T1),
     *                  STANAME(STA1),STANUM(STA1),
     *                  XFERSTA(SC,SC2,T2),STANAME(STA2),STANUM(STA2)
 9031 FORMAT(1X,I1,2(1X,I4,1X,A37,1X,I1))
      IF(STANUM(STA1).EQ.STANUM(STA2)) CYCLE
      URSTA2=STA1
      END IF
      END DO
      IF(TDEBUG) WRITE(119,9032) (URSTA1+MAX_IZONES),STANAME(URSTA1),
     *                STANUM(URSTA1),
     *            (URSTA2+MAX_IZONES),STANAME(URSTA2),STANUM(URSTA2)
 9032 FORMAT(/2X,2(1X,I4,1X,A37,I2))
      END IF
C
C.....URBAN RAIL DESTINATION STATION
      IF(STANUM(SC2).EQ.2) THEN
      URSTA2=SC2
      DO T=1,5
      T1=(T-1)*2+1
      T2=(T-1)*2+2
      STA1=XFERSTA(SC,SC2,T1)-MAX_IZONES
      STA2=XFERSTA(SC,SC2,T2)-MAX_IZONES
      IF(STA1.GT.0.AND.STA2.GT.0) THEN
      IF(TDEBUG) WRITE(119,9031) T,XFERSTA(SC,SC2,T1),
     *                  STANAME(STA1),STANUM(STA1),
     *                  XFERSTA(SC,SC2,T2),STANAME(STA2),STANUM(STA2)
      IF(STANUM(STA1).EQ.2) THEN
      URSTA1=STA1
      GO TO 1000
      END IF
      IF(STANUM(STA2).EQ.2) THEN
      URSTA1=STA2
      GO TO 1000
      END IF
      END IF
      END DO
 1000 IF(TDEBUG) WRITE(119,9032) (URSTA1+MAX_IZONES),STANAME(URSTA1),
     *                STANUM(URSTA1),
     *            (URSTA2+MAX_IZONES),STANAME(URSTA2),STANUM(URSTA2)
      END IF
C.....URBAN RAIL WITHIN PATH
      IF(STANUM(SC).NE.2.AND.STANUM(SC2).NE.2) THEN
      DO T=1,5
      T1=(T-1)*2+1
      T2=(T-1)*2+2
      STA1=XFERSTA(SC,SC2,T1)-MAX_IZONES
      STA2=XFERSTA(SC,SC2,T2)-MAX_IZONES
      IF(STA1.GT.0.AND.STA2.GT.0) THEN
      IF(TDEBUG) WRITE(119,9031) T,XFERSTA(SC,SC2,T1),
     *                  STANAME(STA1),STANUM(STA1),
     *                  XFERSTA(SC,SC2,T2),STANAME(STA2),STANUM(STA2)

      IF(STANUM(STA1).EQ.STANUM(STA2)) CYCLE
      IF(STANUM(STA2).EQ.2) THEN
      URSTA1=STA2
      CYCLE
      END IF
      IF(URSTA1.GT.0) URSTA2=STA1
      END IF
      END DO
      IF(TDEBUG) WRITE(119,9032) (URSTA1+MAX_IZONES),STANAME(URSTA1),
     *                STANUM(URSTA1),
     *            (URSTA2+MAX_IZONES),STANAME(URSTA2),STANUM(URSTA2)     
      END IF
C
      ZSTASTA(SC,SC2,1)=URSTA1
      ZSTASTA(SC,SC2,2)=URSTA2
      END IF
      END DO
      END DO
C
C     OPEN OUTPUT MATRIX FILES
C
      OPEN(122,FILE=SLINKTRP,STATUS='UNKNOWN',FORM='UNFORMATTED')
      NUMPUR=3
      TABLES=2**3-1
      WRITE(122) HEAD1,HEAD2
      OPEN(124,FILE=SLINKSTA,STATUS='UNKNOWN',FORM='UNFORMATTED')
      WRITE(124) HEAD1,HEAD2
      RETURN
      END
