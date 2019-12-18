C-------------------------------------------------------------------
C       STORE URBAN RAIL SELECTED STATION LINK
C-------------------------------------------------------------------
      SUBROUTINE SELSUM(IMODE,IZ,JZ,OSTA,ASTA,TRIPS,STATRIPS,BLDTRIPS,
     *                  CHWIND,CHBKIND,CHBIND,CHPIND,CHKIND,
     *                  WDSTA,BIKDSTA,BDSTA,PDSTA,KDSTA,
     *                  SELIND,SELTRIPS,SELDIST,ZSTATRIPS,ZSTATRIPS2,
     *                  BLDURTRIPS,ZSTASTA)
      INCLUDE 'stadat.com'
      INCLUDE 'param.com'
      INCLUDE 'mtamcpar.inc'
C
      INTEGER*2 IMODE,ASTA(5,14),STA1,STA2,IZ
      INTEGER*2 OSTA(5,14),JZ,SC,ISTA,IC,DC,SC2
      INTEGER*2 WDSTA(10,5),BIKDSTA(10,5)
      INTEGER*2 BDSTA(10,2),PDSTA(10,10)
      INTEGER*2 KDSTA(10,10),DIZ,DJZ
      INTEGER*2 SELIND(MAX_STATIONS,MAX_STATIONS)
      INTEGER*2 ZSTASTA(MAX_STATIONS,MAX_STATIONS,2)
      REAL*4    STATRIPS(MAX_STATIONS,MAX_STATIONS)
      REAL*4    BLDTRIPS(MAX_STATIONS,MAX_STATIONS)
      REAL*4    BLDURTRIPS(MAX_STATIONS,MAX_STATIONS)
      REAL*4    SELTRIPS(15,2),SELDIST(21,21,3)
      REAL*4    ZSTATRIPS(MAX_ZONES),ZSTATRIPS2(MAX_ZONES)
      REAL*8    TRIPS(14)
      LOGICAL   CHWIND(5,2),CHBKIND(5,2),CHBIND(5,2)
      LOGICAL   CHPIND(5,4),CHKIND(5,4)
      CHARACTER*13  NAME(5)
      DATA      NAME/'Commuter Rail',
     *               'Urban Rail   ',
     *               'Express Bus  ',
     *               'Transitway   ',
     *               'BRT          '/
C
      DIZ=DISTEQ(IZ)
      DJZ=DISTEQ(JZ)
C
C...WALK ACCESS STATIONS
      DO ISTA=1,2
      DC=OSTA(IMODE,ISTA)-MAX_IZONES
      SC=ASTA(IMODE,ISTA)-MAX_IZONES
      IF(CHWIND(IMODE,ISTA)) THEN
      SC2=WDSTA((IMODE+5),ISTA)-MAX_IZONES  
      INDC=SELIND(DC,SC2)
      IF(INDC.LE.0) CYCLE
      BLDTRIPS(DC,SC2)=BLDTRIPS(DC,SC2)+SNGL(TRIPS(ISTA))
      STA1=ZSTASTA(DC,SC2,1)
      STA2=ZSTASTA(DC,SC2,2)
      BLDURTRIPS(STA1,STA2)=BLDURTRIPS(STA1,STA2)+SNGL(TRIPS(ISTA))
      SELTRIPS(INDC,2)=SELTRIPS(INDC,2)+SNGL(TRIPS(ISTA))
      ZSTATRIPS2(JZ)=ZSTATRIPS2(JZ)+SNGL(TRIPS(ISTA))
      SELDIST(DIZ,DJZ,2)=SELDIST(DIZ,DJZ,2)+SNGL(TRIPS(ISTA))
C ------------------------------------------------------------------
      IF(TDEBUG) THEN
      WRITE(26,9001) NAME(IMODE)
      WRITE(26,8001) IZ,JZ,ISTA,OSTA(IMODE,ISTA),
     *               STANAME(DC),
     *               WDSTA((IMODE+5),ISTA),STANAME(SC2),
     *               INDC,TRIPS(ISTA),(STA1+MAX_IZONES),
     *               STANAME(STA1),
     *               (STA2+MAX_IZONES),STANAME(STA2)
 9001 FORMAT(/' SELECTED LINK WALK ACCESS - BLENDED PATH (',A13,')'/
     *       ' ----------------------------------------')
 8001 FORMAT(
     *       ' PRODUCTION ZONE               =',I6/
     *       ' ATTRACTION ZONE               =',I6/
     *       ' ACCESS STATION NUMBER         =',I6/
     *       ' ORIGIN STATION                =',I6,1X,A37/
     *       ' DESTINATION STATION           =',I6,1X,A37/
     *       ' NUMBER OF LINKS               =',I6/
     *       ' TRIPS                         =',F6.2/
     *       ' URBAN RAIL ORIGIN      STATION=',I6,1X,A37/
     *       ' URBAN RAIL DESTINATION STATION=',I6,1X,A37)
      END IF
C ------------------------------------------------------------------
      ELSE
      INDC=SELIND(DC,SC)
      IF(INDC.LE.0) CYCLE
      STATRIPS(DC,SC)=STATRIPS(DC,SC)+SNGL(TRIPS(ISTA))
      SELTRIPS(INDC,1)=SELTRIPS(INDC,1)+SNGL(TRIPS(ISTA))
      ZSTATRIPS(JZ)=ZSTATRIPS(JZ)+SNGL(TRIPS(ISTA))
      SELDIST(DIZ,DJZ,1)=SELDIST(DIZ,DJZ,1)+SNGL(TRIPS(ISTA))
C ------------------------------------------------------------------
      IF(TDEBUG) THEN
      WRITE(26,9002) NAME(IMODE)
      WRITE(26,8002) IZ,JZ,ISTA,OSTA(IMODE,ISTA),
     *               STANAME(DC),
     *               ASTA(IMODE,ISTA),STANAME(SC),
     *               INDC,TRIPS(ISTA)
 9002 FORMAT(/' SELECTED LINK WALK ACCESS - URBAN RAIL PATH (',
     *         A13,')'/
     *       ' -------------------------------------------')
 8002 FORMAT(
     *       ' PRODUCTION ZONE               =',I6/
     *       ' ATTRACTION ZONE               =',I6/
     *       ' ACCESS STATION NUMBER         =',I6/
     *       ' ORIGIN STATION                =',I6,1X,A37/
     *       ' DESTINATION STATION           =',I6,1X,A37/
     *       ' NUMBER OF LINKS               =',I6/
     *       ' TRIPS                         =',F6.2)
      END IF
C ------------------------------------------------------------------
      END IF
      END DO
C...BUS ACCESS STATIONS
      DO ISTA=1,2
      DC=OSTA(IMODE,(ISTA+2))-MAX_IZONES
      SC=ASTA(IMODE,(ISTA+2))-MAX_IZONES
      IF(CHBIND(IMODE,ISTA)) THEN
      SC2=BDSTA((IMODE+5),ISTA)-MAX_IZONES
      INDC=SELIND(DC,SC2)
      IF(INDC.LE.0) CYCLE
      BLDTRIPS(DC,SC2)=BLDTRIPS(DC,SC2)+SNGL(TRIPS(ISTA+2))
      STA1=ZSTASTA(DC,SC2,1)
      STA2=ZSTASTA(DC,SC2,2)
      BLDURTRIPS(STA1,STA2)=BLDURTRIPS(STA1,STA2)+SNGL(TRIPS(ISTA+2))
      SELTRIPS(INDC,2)=SELTRIPS(INDC,2)+SNGL(TRIPS(ISTA+2))
      ZSTATRIPS2(JZ)=ZSTATRIPS2(JZ)+SNGL(TRIPS(ISTA+2))
      SELDIST(DIZ,DJZ,2)=SELDIST(DIZ,DJZ,2)+SNGL(TRIPS(ISTA+2))
C ------------------------------------------------------------------
      IF(TDEBUG) THEN
      WRITE(26,9003) NAME(IMODE)
      WRITE(26,8001) IZ,JZ,ISTA,OSTA(IMODE,(ISTA+2)),
     *               STANAME(DC),
     *               BDSTA((IMODE+5),ISTA),STANAME(SC2),
     *               INDC,TRIPS(ISTA+2),(STA1+MAX_IZONES),
     *               STANAME(STA1),
     *               (STA2+MAX_IZONES),STANAME(STA2)
 9003 FORMAT(/' SELECTED LINK BUS ACCESS - BLENDED PATH (',A13,')'/
     *       ' ---------------------------------------')
      END IF
C ------------------------------------------------------------------
      ELSE
      INDC=SELIND(DC,SC)
      IF(INDC.LE.0) CYCLE
      STATRIPS(DC,SC)=STATRIPS(DC,SC)+SNGL(TRIPS(ISTA+2))
      SELTRIPS(INDC,1)=SELTRIPS(INDC,1)+SNGL(TRIPS(ISTA+2))
      ZSTATRIPS(JZ)=ZSTATRIPS(JZ)+SNGL(TRIPS(ISTA+2))
      SELDIST(DIZ,DJZ,1)=SELDIST(DIZ,DJZ,1)+SNGL(TRIPS(ISTA+2))
C ------------------------------------------------------------------
      IF(TDEBUG) THEN
      WRITE(26,9004) NAME(IMODE)
      WRITE(26,8002) IZ,JZ,ISTA,OSTA(IMODE,(ISTA+2)),
     *               STANAME(DC),
     *               ASTA(IMODE,(ISTA+2)),STANAME(SC),
     *               INDC,TRIPS(ISTA+2)
 9004 FORMAT(/' SELECTED LINK BUS ACCESS - URBAN RAIL PATH (',
     *         A13,')'/
     *       ' ------------------------------------------')
      END IF
C ------------------------------------------------------------------
      END IF
      END DO
C...PNR ACCESS STATIONS
      DO ISTA=1,4
      DC=OSTA(IMODE,(ISTA+4))-MAX_IZONES
      SC=ASTA(IMODE,(ISTA+4))-MAX_IZONES
      IF(CHPIND(IMODE,ISTA)) THEN
      SC2=PDSTA((IMODE+5),ISTA)-MAX_IZONES 
      INDC=SELIND(DC,SC2)
      IF(INDC.LE.0) CYCLE
      BLDTRIPS(DC,SC2)=BLDTRIPS(DC,SC2)+SNGL(TRIPS(ISTA+4))
      STA1=ZSTASTA(DC,SC2,1)
      STA2=ZSTASTA(DC,SC2,2)
      BLDURTRIPS(STA1,STA2)=BLDURTRIPS(STA1,STA2)+SNGL(TRIPS(ISTA+4))
      SELTRIPS(INDC,2)=SELTRIPS(INDC,2)+SNGL(TRIPS(ISTA+4))
      ZSTATRIPS2(JZ)=ZSTATRIPS2(JZ)+SNGL(TRIPS(ISTA+4))
      SELDIST(DIZ,DJZ,2)=SELDIST(DIZ,DJZ,2)+SNGL(TRIPS(ISTA+4))
C ------------------------------------------------------------------
      IF(TDEBUG) THEN
      WRITE(26,9005) NAME(IMODE)
      WRITE(26,8001) IZ,JZ,ISTA,OSTA(IMODE,(ISTA+4)),
     *               STANAME(DC),
     *               PDSTA((IMODE+5),ISTA),STANAME(SC2),
     *               INDC,TRIPS(ISTA+4),(STA1+MAX_IZONES),
     *               STANAME(STA1),
     *               (STA2+MAX_IZONES),STANAME(STA2)
 9005 FORMAT(/' SELECTED LINK PNR ACCESS - BLENDED PATH (',A13,')'/
     *       ' ---------------------------------------')
      END IF
C ------------------------------------------------------------------ 
      ELSE
      INDC=SELIND(DC,SC)
      IF(INDC.LE.0) CYCLE
      STATRIPS(DC,SC)=STATRIPS(DC,SC)+SNGL(TRIPS(ISTA+4))
      SELTRIPS(INDC,1)=SELTRIPS(INDC,1)+SNGL(TRIPS(ISTA+4))
      ZSTATRIPS(JZ)=ZSTATRIPS(JZ)+SNGL(TRIPS(ISTA+4))
      SELDIST(DIZ,DJZ,1)=SELDIST(DIZ,DJZ,1)+SNGL(TRIPS(ISTA+4))
C ------------------------------------------------------------------
      IF(TDEBUG) THEN
      WRITE(26,9006) NAME(IMODE)
      WRITE(26,8002) IZ,JZ,ISTA,OSTA(IMODE,(ISTA+4)),
     *               STANAME(DC),
     *               ASTA(IMODE,(ISTA+4)),STANAME(SC),
     *               INDC,TRIPS(ISTA+4)
 9006 FORMAT(/' SELECTED LINK PNR ACCESS - URBAN RAIL PATH (',
     *         A13,')'/
     *       ' ------------------------------------------')
      END IF
C ------------------------------------------------------------------
      END IF
      END DO
C...KNR ACCESS STATIONS
      DO ISTA=1,4
      DC=OSTA(IMODE,(ISTA+8))-MAX_IZONES
      SC=ASTA(IMODE,(ISTA+8))-MAX_IZONES
      IF(CHKIND(IMODE,ISTA)) THEN
      SC2=KDSTA((IMODE+5),ISTA)-MAX_IZONES  
      INDC=SELIND(DC,SC2)
      IF(INDC.LE.0) CYCLE
      BLDTRIPS(DC,SC2)=BLDTRIPS(DC,SC2)+SNGL(TRIPS(ISTA+8))
      STA1=ZSTASTA(DC,SC2,1)
      STA2=ZSTASTA(DC,SC2,2)
      BLDURTRIPS(STA1,STA2)=BLDURTRIPS(STA1,STA2)+SNGL(TRIPS(ISTA+8))
      SELTRIPS(INDC,2)=SELTRIPS(INDC,2)+SNGL(TRIPS(ISTA+8))
      ZSTATRIPS2(JZ)=ZSTATRIPS2(JZ)+SNGL(TRIPS(ISTA+8))
      SELDIST(DIZ,DJZ,2)=SELDIST(DIZ,DJZ,2)+SNGL(TRIPS(ISTA+8))
C ------------------------------------------------------------------
      IF(TDEBUG) THEN
      WRITE(26,9007) NAME(IMODE)
      WRITE(26,8001) IZ,JZ,ISTA,OSTA(IMODE,(ISTA+8)),
     *               STANAME(DC),
     *               KDSTA((IMODE+5),ISTA),STANAME(SC2),
     *               INDC,TRIPS(ISTA+8),(STA1+MAX_IZONES),
     *               STANAME(STA1),
     *               (STA2+MAX_IZONES),STANAME(STA2)
 9007 FORMAT(/' SELECTED LINK KNR ACCESS - BLENDED PATH (',A13,')'/
     *       ' ---------------------------------------')
      END IF
C ------------------------------------------------------------------ 
      ELSE
      INDC=SELIND(DC,SC)
      IF(INDC.LE.0) CYCLE
      STATRIPS(DC,SC)=STATRIPS(DC,SC)+SNGL(TRIPS(ISTA+8))
      SELTRIPS(INDC,1)=SELTRIPS(INDC,1)+SNGL(TRIPS(ISTA+8))
      ZSTATRIPS(JZ)=ZSTATRIPS(JZ)+SNGL(TRIPS(ISTA+8))
      SELDIST(DIZ,DJZ,1)=SELDIST(DIZ,DJZ,1)+SNGL(TRIPS(ISTA+8))
C ------------------------------------------------------------------
      IF(TDEBUG) THEN
      WRITE(26,9008) NAME(IMODE)
      WRITE(26,8002) IZ,JZ,ISTA,OSTA(IMODE,(ISTA+8)),
     *               STANAME(DC),
     *               ASTA(IMODE,(ISTA+8)),STANAME(SC),
     *               INDC,TRIPS(ISTA+8)
 9008 FORMAT(/' SELECTED LINK KNR ACCESS - URBAN RAIL PATH (',
     *         A13,')'/
     *       ' ------------------------------------------')
      END IF
C ------------------------------------------------------------------
      END IF
      END DO
C...BIKE ACCESS STATIONS
      DO ISTA=1,2
      DC=OSTA(IMODE,(ISTA+12))-MAX_IZONES
      SC=ASTA(IMODE,(ISTA+12))-MAX_IZONES
      IF(CHBKIND(IMODE,ISTA)) THEN
      SC2=BIKDSTA((IMODE+5),ISTA)-MAX_IZONES  
      BLDTRIPS(DC,SC2)=BLDTRIPS(DC,SC2)+SNGL(TRIPS(ISTA+12))
      ELSE
      STATRIPS(DC,SC)=STATRIPS(DC,SC)+SNGL(TRIPS(ISTA+12))
      END IF
      END DO
      RETURN
      END
