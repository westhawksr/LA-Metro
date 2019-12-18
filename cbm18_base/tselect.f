C-------------------------------------------------------------------
C      STORE SELECTED LINK TRIP INFORMATION
C-------------------------------------------------------------------
       SUBROUTINE TSELECT(TYPE,IZ,JZ,SC,SC2,TRIPS,SELIND,ZSTAIND,
     *                    STAZIND,ZSTASTA,STAZSTA,
     *                    STATRIPS,STATRIPS2,STATRIPS3,SELTRIPS,
     *                    ZSTATRIPS,ZSTATRIPS2,ZSTATRIPS3,SELDIST)
       INCLUDE 'stadat.com'
       INCLUDE 'param.com'
       include 'tpcom.inc'
       include 'control.inc'
       include 'mtamcpar.inc'
C
C DATA DECLARATIONS
C
      INTEGER*2     IC,SC,SC2,DC,JZ,TYPE,IZ,STA1,STA2,ISTA1,ISTA2
      INTEGER*2     DIZ,DJZ
      INTEGER*2     SELIND(MAX_STATIONS,MAX_STATIONS)
      INTEGER*2     ZSTAIND(MAX_ZONES,MAX_STATIONS)
      INTEGER*2     STAZIND(MAX_STATIONS,MAX_ZONES)
      INTEGER*2     ZSTASTA(MAX_ZONES,MAX_STATIONS,2)
      INTEGER*2     STAZSTA(MAX_STATIONS,MAX_ZONES,2)
      REAL*4        STATRIPS(MAX_STATIONS,MAX_STATIONS)
      REAL*4        STATRIPS2(MAX_STATIONS,MAX_STATIONS)
      REAL*4        STATRIPS3(MAX_STATIONS,MAX_STATIONS)
      REAL*4        SELTRIPS(15,3),SELDIST(21,21,3)
      REAL*4        ZSTATRIPS(MAX_ZONES)
      REAL*4        ZSTATRIPS2(MAX_ZONES)
      REAL*4        ZSTATRIPS3(MAX_ZONES)
      REAL*8        TRIPS
C
      IC=SC+MAX_IZONES
      DC=SC2+MAX_IZONES
      DIZ=DISTEQ(IZ)
      DJZ=DISTEQ(JZ)
C-------------------------------------------------------------
C     URBAN RAIL STATION-TO-STATION
C-------------------------------------------------------------
      IF(TYPE.EQ.1) THEN
      INDC=SELIND(SC,SC2)
      IF(INDC.GT.0.AND.TRIPS.GT.0.0) THEN
      STATRIPS(SC,SC2)=STATRIPS(SC,SC2)+SNGL(TRIPS)
      SELTRIPS(INDC,TYPE)=SELTRIPS(INDC,TYPE)+SNGL(TRIPS)
      ZSTATRIPS(JZ)=ZSTATRIPS(JZ)+SNGL(TRIPS)
      SELDIST(DIZ,DJZ,1)=SELDIST(DIZ,DJZ,1)+SNGL(TRIPS)
C................................................................
      IF(TDEBUG) THEN
      WRITE(26,9001) IC,STANAME(SC),DC,STANAME(SC2),INDC,TRIPS
 9001 FORMAT(' URBAN RAIL STATION-TO-STATION SELECTED TRIPS'/
     *       ' --------------------------------------------'/
     *       ' ORIGIN STATION           =',I4,1X,A37/
     *       ' DESTINATION STATION      =',I4,1X,A37/
     *       ' NUMBER OF TRAVERSED LINKS=',I4/
     *       ' URBAN RAIL TRIPS         =',F8.2/)
      END IF
C....................................................................
      ELSE
C     IF(TDEBUG) WRITE(26,9001) IC,STANAME(SC),DC,STANAME(SC2),
C    *                         INDC,TRIPS
      END IF
      END IF
C--------------------------------------------------------------------
C     URBAN RAIL STATION-TO-STATION FOR COMMUTER RAIL ZONE-TO-STATION
C--------------------------------------------------------------------
      IF(TYPE.EQ.2) THEN
      INDC=ZSTAIND(IZ,SC)
      STA1=ZSTASTA(IZ,SC,1)
      STA2=ZSTASTA(IZ,SC,2)
      ISTA1=STA1-MAX_IZONES
      ISTA2=STA2-MAX_IZONES
      IF(INDC.GT.0.AND.TRIPS.GT.0.0.AND.STA1.GT.0.AND.STA2.GT.0) THEN
      STATRIPS2(ISTA1,ISTA2)=STATRIPS2(ISTA1,ISTA2)+SNGL(TRIPS)
      SELTRIPS(INDC,TYPE)=SELTRIPS(INDC,TYPE)+SNGL(TRIPS)
      ZSTATRIPS2(JZ)=ZSTATRIPS2(JZ)+SNGL(TRIPS)
      SELDIST(DIZ,DJZ,2)=SELDIST(DIZ,DJZ,2)+SNGL(TRIPS)
C................................................................
      IF(DEBUG) THEN
      WRITE(26,9002) IZ,IC,STANAME(SC),
     *               STA1,STANAME(STA1-MAX_IZONES),
     *               STA2,STANAME(STA2-MAX_IZONES),
     *               INDC,TRIPS
 9002 FORMAT(' URBAN RAIL FOR COMMUTER RAIL ZONE-TO-STATION'/
     *       ' --------------------------------------------'/
     *       ' ORIGIN ZONE              =',I4/
     *       ' COMMUTER RAIL STATION    =',I4,1X,A37/
     *       ' URBAN RAIL ACCESS STATION=',I4,1X,A37/
     *       ' URBAN RAIL EGRESS STATION=',I4,1X,A37/
     *       ' NUMBER OF TRAVERSED LINKS=',I4/
     *       ' COMMUTER RAIL TRIPS      =',E12.5/)
      END IF
C....................................................................
      ELSE
C................................................................
      IF(DEBUG.AND.TRIPS.GT.0) THEN
      WRITE(26,9003) IZ,IC,STANAME(SC),
     *               STA1,
     *               STA2,
     *               INDC,TRIPS
 9003 FORMAT(' URBAN RAIL FOR COMMUTER RAIL ZONE-TO-STATION'/
     *       ' --------------------------------------------'/
     *       ' ORIGIN ZONE              =',I4/
     *       ' COMMUTER RAIL STATION    =',I4,1X,A37/
     *       ' URBAN RAIL ACCESS STATION=',I4/
     *       ' URBAN RAIL EGRESS STATION=',I4/
     *       ' NUMBER OF TRAVERSED LINKS=',I4/
     *       ' COMMUTER RAIL TRIPS      =',E12.5/)
      END IF
C....................................................................
      END IF
      END IF
C--------------------------------------------------------------------
C     URBAN RAIL STATION-TO-STATION FOR COMMUTER RAIL STATION-TO-ZONE
C--------------------------------------------------------------------
      IF(TYPE.EQ.3) THEN
      INDC=STAZIND(SC2,JZ)
      STA1=STAZSTA(SC2,JZ,1)
      STA2=STAZSTA(SC2,JZ,2)
      ISTA1=STA1-MAX_IZONES
      ISTA2=STA2-MAX_IZONES
      IF(INDC.GT.0.AND.TRIPS.GT.0.0.AND.STA1.GT.0.AND.STA2.GT.0) THEN
      STATRIPS3(ISTA1,ISTA2)=STATRIPS3(ISTA1,ISTA2)+SNGL(TRIPS)
      SELTRIPS(INDC,TYPE)=SELTRIPS(INDC,TYPE)+SNGL(TRIPS)
      ZSTATRIPS3(JZ)=ZSTATRIPS3(JZ)+SNGL(TRIPS)
      SELDIST(DIZ,DJZ,3)=SELDIST(DIZ,DJZ,3)+SNGL(TRIPS)
C................................................................
      IF(DEBUG) THEN
      WRITE(26,9004) DC,STANAME(SC2),JZ,
     *               STA1,STANAME(STA1-MAX_IZONES),
     *               STA2,STANAME(STA2-MAX_IZONES),
     *               INDC,TRIPS
 9004 FORMAT(' URBAN RAIL FOR COMMUTER RAIL STATION-TO-ZONE'/
     *       ' --------------------------------------------'/
     *       ' COMMUTER RAIL STATION    =',I4,1X,A37/
     *       ' DESTINATION ZONE         =',I4/
     *       ' URBAN RAIL ACCESS STATION=',I4,1X,A37/
     *       ' URBAN RAIL EGRESS STATION=',I4,1X,A37/
     *       ' NUMBER OF TRAVERSED LINKS=',I4/
     *       ' COMMUTER RAIL TRIPS      =',F8.2/)
      END IF
C....................................................................
      ELSE
C................................................................
      IF(DEBUG.AND.TRIPS.GT.0) THEN
      WRITE(26,9005) DC,STANAME(SC2),JZ,
     *               STA1,
     *               STA2,
     *               INDC,TRIPS
 9005 FORMAT(' URBAN RAIL FOR COMMUTER RAIL STATION-TO-ZONE'/
     *       ' --------------------------------------------'/
     *       ' COMMUTER RAIL STATION    =',I4,1X,A37/
     *       ' DESTINATION ZONE         =',I4/
     *       ' URBAN RAIL ACCESS STATION=',I4/
     *       ' URBAN RAIL EGRESS STATION=',I4/
     *       ' NUMBER OF TRAVERSED LINKS=',I4/
     *       ' COMMUTER RAIL TRIPS      =',E12.5/)
      END IF
C....................................................................
      END IF
      END IF
      RETURN
      END
