C-------------------------------------------------------------------
C      RAIL SELECT LINK INDICATOR SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE STASEL(SELIND,ZSTAIND,STAZIND,RAILSEL,
     *            ZSTASTA,STAZSTA)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'tpcom.inc'
       include 'control.inc'
       include 'mtamcpar.inc'
C
C DATA DECLARATIONS
C
      INTEGER*2     IC,SC,SC2,DC,T,STA1,STA2
      INTEGER*2     SELIND(MAX_STATIONS,MAX_STATIONS)
      INTEGER*2     ZSTAIND(MAX_ZONES,MAX_STATIONS)
      INTEGER*2     STAZIND(MAX_STATIONS,MAX_ZONES)
      INTEGER*2     ZSTASTA(MAX_ZONES,MAX_STATIONS,2)
      INTEGER*2     STAZSTA(MAX_STATIONS,MAX_ZONES,2)
      INTEGER*4     VAR2(4000),STA
      LOGICAL       EXISTS,RAILSEL
      ZSTAIND=0
      STAZIND=0
      ZSTASTA=0
      STAZSTA=0
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
C  STATION-TO-STATION
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
 9029 FORMAT(/1X,'STATION-TO-STATION SELECT LINK INDICATOR VALUE'/
     *       1X,'-----------------------------------------------'/
     *       1X,'ORIGIN            STATION=',I8,1X,A37/
     *       1X,'DESTINATION       STATION=',I8,1X,A37/
     *       1X,'NUMBER OF LINKS TRAVERSED=',I8/)
      END IF
C.......................................................................
      END DO
      END DO
C------------------------------------------------
C...ZONE-TO-STATION
C------------------------------------------------
      CLOSE(118,STATUS='KEEP')
      CALL PREPIO(SELRAL,118)
C
C  ORIGIN ZONE LOOP 
C
      DO SC=1,MAX_ZONES
C
C.....LINK INDICATOR (NUMBER OF LINKS)
C
      PURP=2
      CALL INTAB(118,VAR,SC,PURP,DUMMY,IO)
C
C   DESTINATION STATION LOOP
C
      DO SC2=1,MAX_STATIONS
      DC=SC2+MAX_IZONES
      IF(STADATA(SC2,6).NE.1.0) CYCLE
C
C     STORE VALUE
C
      ZSTAIND(SC,SC2)=VAR(DC)
C....................................................................
      IF(TDEBUG.AND.ZSTAIND(SC,SC2).GT.0) THEN
      WRITE(119,9030)  SC,DC,STANAME(SC2),
     *                ZSTAIND(SC,SC2)
 9030 FORMAT(/1X,'ZONE-TO-STATION SELECT LINK INDICATOR VALUE'/
     *       1X,'--------------------------------------------'/
     *       1X,'ORIGIN               ZONE=',I8/
     *       1X,'ORIGIN            STATION=',I8,1X,A37/
     *       1X,'NUMBER OF LINKS TRAVERSED=',I8/)
      END IF
C.......................................................................
      END DO
      END DO
C------------------------------------------------
C...STATION-TO-ZONE
C------------------------------------------------
      CLOSE(118,STATUS='KEEP')
      CALL PREPIO(SELRAL,118)
C
C  ORIGIN STATION LOOP 
C
      DO SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      IF(STADATA(SC,6).NE.1.0) CYCLE
C
C.....LINK INDICATOR (NUMBER OF LINKS)
C
      PURP=2
      CALL INTAB(118,VAR,IC,PURP,DUMMY,IO)
C
C   DESTINATION ZONE LOOP
C
      DO SC2=1,MAX_ZONES
C
C     STORE VALUE
C
      STAZIND(SC,SC2)=VAR(SC2)
C....................................................................
      IF(TDEBUG.AND.STAZIND(SC,SC2).GT.0) THEN
      WRITE(119,9031)  IC,STANAME(SC),SC2,
     *                STAZIND(SC,SC2)
 9031 FORMAT(/1X,'STATION-TO-ZONE SELECT LINK INDICATOR VALUE'/
     *       1X,'--------------------------------------------'/
     *       1X,'DESTINATION       STATION=',I8,1X,A37/
     *       1X,'DESTINATION          ZONE=',I8/
     *       1X,'NUMBER OF LINKS TRAVERSED=',I8/)
      END IF
C.......................................................................
      END DO
      END DO
C ***************************************************************
C     CHECK FOR URBAN RAIL STATION FOR COMMUTER RAIL FILE
C ***************************************************************
      INQUIRE (FILE=FACEG,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,8001)
      WRITE(*,8001)
 8001 FORMAT(/' STASEL 8001 (F) FACEG FILE NOT FOUND'/)
      STOP 8001
      ELSE
      OPEN(120,FILE=FACEG,
     *        STATUS='OLD',FORM='UNFORMATTED')
      READ(120) HEAD1, HEAD2
      WRITE(*,8002)
 8002 FORMAT(/1X,'Read and Store Urban Rail Station Data ',
     *           ' For Commuter Rail')
      END IF
C------------------------------------------------
C...ZONE-TO-STATION
C------------------------------------------------
C
C  ORIGIN ZONE LOOP 
C
      DO SC=1,MAX_ZONES
C
C.....URBAN RAIL STATION NODES
C
      PURP=1
      CALL INTAB(120,VAR,SC,PURP,DUMMY,IO)
      PURP=2
      CALL INTAB(120,VAR2,SC,PURP,DUMMY,IO)     
C
C   DESTINATION STATION LOOP
C
      DO SC2=1,MAX_STATIONS
      DC=SC2+MAX_IZONES
      IF(STANUM(SC2).NE.1) CYCLE
      IF(STADATA(SC2,6).NE.1.0) CYCLE
C
C     CONVERT TO PSUEDO NODE & STORE VALUE
C
      CALL STANODE(VAR(DC),STA)
      ZSTASTA(SC,SC2,1)=STA
      STA1=STA-MAX_IZONES
      CALL STANODE(VAR2(DC),STA)
      ZSTASTA(SC,SC2,2)=STA
      STA2=STA-MAX_IZONES
C....................................................................
      IF(TDEBUG.AND.ZSTASTA(SC,SC2,1).GT.0) THEN
      WRITE(119,9032)  SC,DC,STANAME(SC2),
     *                ZSTASTA(SC,SC2,1),STANAME(STA1),
     *                ZSTASTA(SC,SC2,2),STANAME(STA2)     
 9032 FORMAT(/1X,'ZONE-TO-STATION URBAN RAIL STATIONS'/
     *       1X,'------------------------------------'/
     *       1X,'ORIGIN                    ZONE=',I8/
     *       1X,'ORIGIN   COMMUTER RAIL STATION=',I8,1X,A37/
     *       1X,'ORIGIN      URBAN RAIL STATION=',I8,1X,A37/
     *       1X,'DESTINATION URBAN RAIL STATION=',I8,1X,A37/)
      END IF
C.......................................................................
      END DO
      END DO
C------------------------------------------------
C...STATION-TO-ZONE
C------------------------------------------------
      CLOSE(120,STATUS='KEEP')
      CALL PREPIO(FACEG,120)
C
C  ORIGIN STATION LOOP 
C
      DO SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      IF(STANUM(SC).NE.1) CYCLE
      IF(STADATA(SC,6).NE.1.0) CYCLE
C
C.....URBAN RAIL STATION NODES
C
      PURP=3
      CALL INTAB(120,VAR,IC,PURP,DUMMY,IO)
      PURP=4
      CALL INTAB(120,VAR2,IC,PURP,DUMMY,IO)     
C
C   DESTINATION ZONE LOOP
C
      DO SC2=1,MAX_ZONES
C
C     CONVERT TO PSUEDO NODE & STORE VALUE
C
      CALL STANODE(VAR(SC2),STA)
      STAZSTA(SC,SC2,1)=STA
      STA1=STA-MAX_IZONES
      CALL STANODE(VAR2(SC2),STA)
      STAZSTA(SC,SC2,2)=STA
      STA2=STA-MAX_IZONES
C....................................................................
      IF(TDEBUG.AND.STAZSTA(SC,SC2,1).GT.0.AND.
     *             STAZSTA(SC,SC2,2).GT.0) THEN
      WRITE(119,9033)  IC,STANAME(SC),SC2,
     *                STAZSTA(SC,SC2,1),STANAME(STA1),
     *                STAZSTA(SC,SC2,2),STANAME(STA2)     
 9033 FORMAT(/1X,'STATION-TO-ZONE URBAN RAIL STATIONS'/
     *       1X,'------------------------------------'/
     *       1X,'DESTINATION COMMUTER RAIL STATION=',I8,1X,A37/
     *       1X,'DESTINATION                  ZONE=',I8/
     *       1X,'ORIGIN         URBAN RAIL STATION=',I8,1X,A37/
     *       1X,'DESTINATION    URBAN RAIL STATION=',I8,1X,A37/)
      END IF
C.......................................................................
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
