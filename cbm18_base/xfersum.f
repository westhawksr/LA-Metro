C-------------------------------------------------------------------
C       STATION-TO-STATION TRANSFER SUMMARY PREPARATION
C-------------------------------------------------------------------
      SUBROUTINE XFERSUM(EGRTYPE,ACCMDE,DC,SC2,TRIPS,RTEMP)
      INCLUDE 'stadat.com'
      INCLUDE 'param.com'
      INCLUDE 'mtamcpar.inc'
C
      INTEGER*2   DC,SC2,T,EGRTYPE,ACCMDE,T1,T2,TX1,TX2,MAXT
      REAL*8      TRIPS,RTEMP
      CHARACTER*4 ACCNAME(5)
      CHARACTER*7 EGRNAME(2)
      
      DATA        ACCNAME/'WALK','BUS','PNR','KNR','BIKE'/
      DATA        EGRNAME/'REGULAR','BLENDED'/
C
C     POPULATE TRANSFER MATRIX
C
      IF(DC.LE.0.OR.SC2.LE.0) RETURN
      MAXT=0
      DO T=1,5
      T1=(T-1)*2+1
      T2=(T-1)*2+2
      IF(XFERSTA(DC,SC2,T1).GT.0.AND.XFERSTA(DC,SC2,T2).GT.0) MAXT=T
      END DO
      IF(DEBUG.AND.SDETAIL) WRITE(26,999) MAXT
  999 FORMAT(/' NUMBER OF TRANSFERS=',I2)
      DO T=1,5
      T1=(T-1)*2+1
      T2=(T-1)*2+2
      IF(XFERSTA(DC,SC2,T1).GT.0.AND.XFERSTA(DC,SC2,T2).GT.0) THEN
      TX1=XFERSTA(DC,SC2,T1)-MAX_IZONES
      TX2=XFERSTA(DC,SC2,T2)-MAX_IZONES
      XFERMATX(TX1,TX2)=XFERMATX(TX1,TX2)+TRIPS
C..............................................................................
      IF(DEBUG.AND.SDETAIL) THEN
      WRITE(26,1000) T,(DC+MAX_IZONES),STANAME(DC),(SC2+MAX_IZONES),
     *               STANAME(SC2),(TX1+MAX_IZONES),STANAME(TX1),
     *               (TX2+MAX_IZONES),STANAME(TX2),
     *               EGRNAME(EGRTYPE),ACCNAME(ACCMDE),TRIPS,
     *               XFERMATX(TX1,TX2)
 1000 FORMAT(/' STATION-TO-STATION TRANSFER MATRIX'/
     *       ' ----------------------------------'/
     *       ' TRANSFER SET         =',I4/
     *       ' ORIGIN   STATION FROM=',I4,1X,A37/
     *       ' DEST     STATION  TO =',I4,1X,A37/
     *       ' TRANSFER STATION FROM=',I4,1X,A37/
     *       ' TRANSFER STATION  TO =',I4,1X,A37/
     *       ' EGRESS TYPE ',A7,' ACCESS MODE ',A4,
     *       ' TRIPS=',F8.2,' ACCUMULATED TRIPS=',F8.2)
      END IF
C................................................................................
C
C     POPULATE BLENDED PATH STATION ACCESS/EGRESS ARRAYS
C
C --------------------------------------------------------
C.....COMMUTER RAIL TO URBAN RAIL
C --------------------------------------------------------
      IF(STANUM(DC).EQ.1.AND.STANUM(SC2).EQ.2) THEN
C...INITIAL AND FINAL BOARDING/ALIGHTING
      IF(T.EQ.1) STASUM6(DC,ACCMDE,1)=STASUM6(DC,ACCMDE,1)+TRIPS
      IF(T.EQ.MAXT) THEN
      STASUM7(SC2,1,1)=STASUM7(SC2,1,1)+RTEMP
      STASUM7(SC2,2,1)=STASUM7(SC2,2,1)+(TRIPS-RTEMP)
      END IF
      IF(STANUM(TX1).EQ.STANUM(TX2)) CYCLE
C...PRODUCTION END
      STASUM6(TX2,2,1)=STASUM6(TX2,2,1)+TRIPS
C...ATTRACTION END
      STASUM7(TX1,2,1)=STASUM7(TX1,2,1)+TRIPS 
      CRURSS(TX2,SC2)=CRURSS(TX2,SC2)+TRIPS
      CR2URSS(DC,TX1)=CR2URSS(DC,TX1)+TRIPS
      END IF
C --------------------------------------------------------
C.....COMMUTER RAIL TO BRT
C --------------------------------------------------------
      IF(STANUM(DC).EQ.1.AND.STANUM(SC2).EQ.5) THEN
C...INITIAL AND FINAL BOARDING/ALIGHTING
      IF(T.EQ.1) STASUM6(DC,ACCMDE,2)=STASUM6(DC,ACCMDE,2)+TRIPS
      IF(T.EQ.MAXT) THEN
      STASUM7(SC2,1,2)=STASUM7(SC2,1,2)+RTEMP
      STASUM7(SC2,2,2)=STASUM7(SC2,2,2)+(TRIPS-RTEMP)
      END IF
      IF(STANUM(TX1).EQ.STANUM(TX2)) CYCLE
C...PRODUCTION END
      STASUM6(TX2,2,2)=STASUM6(TX2,2,2)+TRIPS
C...ATTRACTION END
      STASUM7(TX1,2,2)=STASUM7(TX1,2,2)+TRIPS
      CRBRTSS(DC,TX1)=CRBRTSS(DC,TX1)+TRIPS
      CR2BRTSS(TX2,SC2)=CR2BRTSS(TX2,SC2)+TRIPS
      END IF
C -------------------------------------------------------------
C.....URBAN RAIL TO COMMUTER RAIL
C ------------------------------------------------------------
      IF(STANUM(DC).EQ.2.AND.STANUM(SC2).EQ.1) THEN
C...INITIAL AND FINAL BOARDING/ALIGHTING
      IF(T.EQ.1) STASUM6(DC,ACCMDE,3)=STASUM6(DC,ACCMDE,3)+TRIPS
      IF(T.EQ.MAXT) THEN
      STASUM7(SC2,1,3)=STASUM7(SC2,1,3)+RTEMP
      STASUM7(SC2,2,3)=STASUM7(SC2,2,3)+(TRIPS-RTEMP)
      END IF
      IF(STANUM(TX1).EQ.STANUM(TX2)) CYCLE
C...PRODUCTION END
      STASUM6(TX2,2,3)=STASUM6(TX2,2,3)+TRIPS
C...ATTRACTION END
      STASUM7(TX1,2,3)=STASUM7(TX1,2,3)+TRIPS 
      URCRSS(DC,TX1)=URCRSS(DC,TX1)+TRIPS
      UR2CRSS(TX2,SC2)=UR2CRSS(TX2,SC2)+TRIPS
      END IF
C --------------------------------------------------------------
C.....URBAN RAIL TO BRT
C --------------------------------------------------------------
      IF(STANUM(DC).EQ.2.AND.STANUM(SC2).EQ.5) THEN
C...INITIAL AND FINAL BOARDING/ALIGHTING
      IF(T.EQ.1) STASUM6(DC,ACCMDE,4)=STASUM6(DC,ACCMDE,4)+TRIPS
      IF(T.EQ.MAXT) THEN
      STASUM7(SC2,1,4)=STASUM7(SC2,1,4)+RTEMP
      STASUM7(SC2,2,4)=STASUM7(SC2,2,4)+(TRIPS-RTEMP)
      END IF
      IF(STANUM(TX1).EQ.STANUM(TX2)) CYCLE
C...PRODUCTION END
      STASUM6(TX2,2,4)=STASUM6(TX2,2,4)+TRIPS
C...ATTRACTION END
      STASUM7(TX1,2,4)=STASUM7(TX1,2,4)+TRIPS 
      URBRTSS(DC,TX1)=URBRTSS(DC,TX1)+TRIPS
      UR2BRTSS(TX2,SC2)=UR2BRTSS(TX2,SC2)+TRIPS
      END IF
C ----------------------------------------------------------------
C.....BRT TO COMMUTER RAIL
C ----------------------------------------------------------------
      IF(STANUM(DC).EQ.5.AND.STANUM(SC2).EQ.1) THEN
C...INITIAL AND FINAL BOARDING/ALIGHTING
      IF(T.EQ.1) STASUM6(DC,ACCMDE,5)=STASUM6(DC,ACCMDE,5)+TRIPS
      IF(T.EQ.MAXT) THEN
      STASUM7(SC2,1,5)=STASUM7(SC2,1,5)+RTEMP
      STASUM7(SC2,2,5)=STASUM7(SC2,2,5)+(TRIPS-RTEMP)
      END IF
      IF(STANUM(TX1).EQ.STANUM(TX2)) CYCLE
C...PRODUCTION END
      STASUM6(TX2,2,5)=STASUM6(TX2,2,5)+TRIPS
C...ATTRACTION END
      STASUM7(TX1,2,5)=STASUM7(TX1,2,5)+TRIPS 
      BRTCRSS(TX2,SC2)=BRTCRSS(TX2,SC2)+TRIPS
      BRT2CRSS(DC,TX1)=BRT2CRSS(DC,TX1)+TRIPS
      END IF
C ----------------------------------------------------------------
C.....BRT TO URBAN RAIL
C ----------------------------------------------------------------
      IF(STANUM(DC).EQ.5.AND.STANUM(SC2).EQ.2) THEN
C...INITIAL AND FINAL BOARDING/ALIGHTING
      IF(T.EQ.1) STASUM6(DC,ACCMDE,6)=STASUM6(DC,ACCMDE,6)+TRIPS
      IF(T.EQ.MAXT) THEN
      STASUM7(SC2,1,6)=STASUM7(SC2,1,6)+RTEMP
      STASUM7(SC2,2,6)=STASUM7(SC2,2,6)+(TRIPS-RTEMP)
      END IF
      IF(STANUM(TX1).EQ.STANUM(TX2)) CYCLE
C...PRODUCTION END
      STASUM6(TX2,2,6)=STASUM6(TX2,2,6)+TRIPS
C...ATTRACTION END
      STASUM7(TX1,2,6)=STASUM7(TX1,2,6)+TRIPS 
      BRTURSS(TX2,SC2)=BRTURSS(TX2,SC2)+TRIPS
      BRT2URSS(DC,TX1)=BRT2URSS(DC,TX1)+TRIPS
      END IF
C
      END IF
      END DO
      RETURN
      END
