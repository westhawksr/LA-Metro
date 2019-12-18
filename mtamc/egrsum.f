C-------------------------------------------------------------------
C       STATION EGRESS SUMMARY SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE EGRSUM(IZ,JZ,IMODE,OSTA,ASTA,TRIPS,STASUM2,
     *                  CHWIND,CHBKIND,CHBIND,CHPIND,CHKIND,
     *                  STASUM4,STASUM5,WDSTA,BIKDSTA,BDSTA,
     *                  PDSTA,KDSTA,STAEGR,STAVOL)
      INCLUDE 'stadat.com'
      INCLUDE 'param.com'
      INCLUDE 'mtamcpar.inc'
C
      INTEGER*2 IMODE,ASTA(5,14),CHOICE,DMODE
      INTEGER*2 OSTA(5,14),IZ,JZ,SC,ISTA,IC,DC,SC2
      INTEGER*2 WDSTA(10,5),BIKDSTA(10,5)
      INTEGER*2 BDSTA(10,2),PDSTA(10,10)
      INTEGER*2 KDSTA(10,10)
      REAL*4    STAEGR(4,MAX_STATIONS,MAX_IZONES)
      REAL*8    TRIPS(14),STASUM2(MAX_STATIONS,4)
      REAL*8    STASUM4(MAX_STATIONS,5),RTEMP,RTEMP2,RTEMP3
      REAL*8    STASUM5(MAX_STATIONS,4),STAVOL(5,5)
      LOGICAL   CHWIND(5,2),CHBKIND(5,2),CHBIND(5,2)
      LOGICAL   CHPIND(5,4),CHKIND(5,4)
C
      IF(.NOT.TRNEGR) THEN
C...WALK ACCESS STATIONS
      DO ISTA=1,2
      IF(TRIPS(ISTA).LE.0.0) CYCLE
      IF(CHWIND(IMODE,ISTA)) THEN
      DC=OSTA(IMODE,ISTA)-MAX_IZONES
      SC2=WDSTA((IMODE+5),ISTA)-MAX_IZONES
      IC=STAIND(SC2,JZ)
      IF((IC.LT.1.OR.IC.GT.2).AND.TRIPS(ISTA).GT.0) THEN
      CHOICE=11
      WRITE(26,9001) IZ,JZ,IMODE,CHOICE,WDSTA((IMODE+5),ISTA),
     *               STANAME(SC2),
     *               TRIPS(ISTA)
      END IF   
C     IF(IC.LT.1.OR.IC.GT.2) CYCLE      
      STASUM4(DC,1)=STASUM4(DC,1)+TRIPS(ISTA)
      STASUM5(SC2,IC)=STASUM5(SC2,IC)+TRIPS(ISTA)
      ELSE
      SC=ASTA(IMODE,ISTA)-MAX_IZONES
      IC=STAIND(SC,JZ)
      IF((IC.LT.1.OR.IC.GT.2).AND.TRIPS(ISTA).GT.0) THEN
      CHOICE=1
C     WRITE(26,9001) IZ,JZ,IMODE,CHOICE,ASTA(IMODE,ISTA),STANAME(SC),
C    *               TRIPS(ISTA)
 9001 FORMAT(' EGRSUM 9001 (W) IZ=',I4,' JZ=',I4,' IMODE=',I2,
     *       ' CHOICE=',I2,
     *       ' STA=',I4,1X,A29,' TRIPS=',E12.5)
      END IF      
      IF(IC.LT.1.OR.IC.GT.2) CYCLE
      STASUM2(SC,IC)=STASUM2(SC,IC)+TRIPS(ISTA)
      END IF
      END DO
C...BUS ACCESS STATIONS
      DO ISTA=1,2
      IF(TRIPS(ISTA+2).LE.0.0) CYCLE
      IF(CHBIND(IMODE,ISTA)) THEN
      DC=OSTA(IMODE,(ISTA+2))-MAX_IZONES
      SC2=BDSTA((IMODE+5),ISTA)-MAX_IZONES
      IC=STAIND(SC2,JZ)
      IF((IC.LT.1.OR.IC.GT.2).AND.TRIPS(ISTA+2).GT.0) THEN
      CHOICE=21
      WRITE(26,9001) IZ,JZ,IMODE,CHOICE,BDSTA((IMODE+5),ISTA),
     *               STANAME(SC2),
     *               TRIPS(ISTA+2)
      END IF  
C     IF(IC.LT.1.OR.IC.GT.2) CYCLE 
      STASUM4(DC,2)=STASUM4(DC,2)+TRIPS(ISTA+2)
      STASUM5(SC2,IC)=STASUM5(SC2,IC)+TRIPS(ISTA+2)
      ELSE
      SC=ASTA(IMODE,(ISTA+2))-MAX_IZONES
      IC=STAIND(SC,JZ)
      IF((IC.LT.1.OR.IC.GT.2).AND.TRIPS(ISTA+2).GT.0) THEN
      CHOICE=2
C     WRITE(26,9001) IZ,JZ,IMODE,CHOICE,ASTA(IMODE,(ISTA+2)),
C    *               STANAME(SC),
C    *               TRIPS(ISTA+2)
      END IF      
      IF(IC.LT.1.OR.IC.GT.2) CYCLE
      STASUM2(SC,IC)=STASUM2(SC,IC)+TRIPS(ISTA+2)
      END IF
      END DO
C...PNR ACCESS STATIONS
      DO ISTA=1,4
      IF(TRIPS(ISTA+4).LE.0.0) CYCLE
      IF(CHPIND(IMODE,ISTA)) THEN
      DC=OSTA(IMODE,(ISTA+4))-MAX_IZONES
      SC2=PDSTA((IMODE+5),ISTA)-MAX_IZONES
      IC=STAIND(SC2,JZ)
      IF((IC.LT.1.OR.IC.GT.2).AND.TRIPS(ISTA+4).GT.0) THEN
      CHOICE=31
      WRITE(26,9001) IZ,JZ,IMODE,CHOICE,PDSTA((IMODE+5),ISTA),
     *               STANAME(SC2),
     *               TRIPS(ISTA+4)
      END IF  
C     IF(IC.LT.1.OR.IC.GT.2) CYCLE 
      STASUM4(DC,3)=STASUM4(DC,3)+TRIPS(ISTA+4)
      STASUM5(SC2,IC)=STASUM5(SC2,IC)+TRIPS(ISTA+4)
      ELSE
      SC=ASTA(IMODE,(ISTA+4))-MAX_IZONES
      IC=STAIND(SC,JZ)
      IF((IC.LT.1.OR.IC.GT.2).AND.TRIPS(ISTA+4).GT.0) THEN
      CHOICE=3
C     WRITE(26,9001) IZ,JZ,IMODE,CHOICE,ASTA(IMODE,(ISTA+4)),
C    *               STANAME(SC),
C    *               TRIPS(ISTA+4)
      END IF
      IF(IC.LT.1.OR.IC.GT.2) CYCLE
      STASUM2(SC,IC)=STASUM2(SC,IC)+TRIPS(ISTA+4)
      END IF
      END DO
C...KNR ACCESS STATIONS
      DO ISTA=1,4
      IF(TRIPS(ISTA+8).LE.0.0) CYCLE
      IF(CHKIND(IMODE,ISTA)) THEN
      DC=OSTA(IMODE,(ISTA+8))-MAX_IZONES
      SC2=KDSTA((IMODE+5),ISTA)-MAX_IZONES
      IC=STAIND(SC2,JZ)
      IF((IC.LT.1.OR.IC.GT.2).AND.TRIPS(ISTA+8).GT.0) THEN
      CHOICE=41
      WRITE(26,9001) IZ,JZ,IMODE,CHOICE,KDSTA((IMODE+5),ISTA),
     *               STANAME(SC2),
     *               TRIPS(ISTA+8)
      END IF  
C     IF(IC.LT.1.OR.IC.GT.2) CYCLE 
      STASUM4(DC,4)=STASUM4(DC,4)+TRIPS(ISTA+8)
      STASUM5(SC2,IC)=STASUM5(SC2,IC)+TRIPS(ISTA+8)
      ELSE
      SC=ASTA(IMODE,(ISTA+8))-MAX_IZONES
      IC=STAIND(SC,JZ)
      IF((IC.LT.1.OR.IC.GT.2).AND.TRIPS(ISTA+8).GT.0) THEN
      CHOICE=4
C     WRITE(26,9001) IZ,JZ,IMODE,CHOICE,ASTA(IMODE,(ISTA+8)),
C    *               STANAME(SC),
C    *               TRIPS(ISTA+8)
      END IF
      IF(IC.LT.1.OR.IC.GT.2) CYCLE
      STASUM2(SC,IC)=STASUM2(SC,IC)+TRIPS(ISTA+8)
      END IF
      END DO
C...BIKE ACCESS STATIONS
      DO ISTA=1,2
      IF(TRIPS(ISTA+12).LE.0.0) CYCLE
      IF(CHBKIND(IMODE,ISTA)) THEN
      DC=OSTA(IMODE,(ISTA+12))-MAX_IZONES
      SC2=BIKDSTA((IMODE+5),ISTA)-MAX_IZONES
      IC=STAIND(SC2,JZ)
C     IF(IC.LT.1.OR.IC.GT.2) CYCLE 
      STASUM4(DC,5)=STASUM4(DC,5)+TRIPS(ISTA+12)
      STASUM5(SC2,IC)=STASUM5(SC2,IC)+TRIPS(ISTA+12)
      ELSE
      SC=ASTA(IMODE,(ISTA+12))-MAX_IZONES
      IC=STAIND(SC,JZ)
      IF(IC.LT.1.OR.IC.GT.2) CYCLE
      STASUM2(SC,IC)=STASUM2(SC,IC)+TRIPS(ISTA+12)
      END IF
      END DO
C*****************************************************
      ELSE
C*****************************************************
C...WALK ACCESS STATIONS
      DO ISTA=1,2
      IF(TRIPS(ISTA).LE.0.0) CYCLE
      IF(CHWIND(IMODE,ISTA)) THEN
      DC=OSTA(IMODE,ISTA)-MAX_IZONES
      SC2=WDSTA((IMODE+5),ISTA)-MAX_IZONES 
      DMODE=STANUM(SC2)
      RTEMP=TRIPS(ISTA)*STAEGR(1,SC2,JZ)
      RTEMP3=TRIPS(ISTA)*STAEGR(4,SC2,JZ)
      RTEMP2=TRIPS(ISTA)*
     *   (1.0-STAEGR(1,SC2,JZ)-STAEGR(2,SC2,JZ)-STAEGR(4,SC2,JZ))
      STASUM4(DC,1)=STASUM4(DC,1)+TRIPS(ISTA)
      STASUM5(SC2,1)=STASUM5(SC2,1)+RTEMP
      STASUM5(SC2,3)=STASUM5(SC2,3)+RTEMP2
      STASUM5(SC2,4)=STASUM5(SC2,4)+RTEMP3
      STASUM5(SC2,2)=STASUM5(SC2,2)+TRIPS(ISTA)-RTEMP-RTEMP2-RTEMP3
      IF(DMODE.GT.0) THEN
      STAVOL(IMODE,DMODE)=STAVOL(IMODE,DMODE)+TRIPS(ISTA)
      CALL XFERSUM(2,1,DC,SC2,TRIPS(ISTA),RTEMP,RTEMP2,RTEMP3)
      END IF
      ELSE
      DC=OSTA(IMODE,ISTA)-MAX_IZONES
      SC=ASTA(IMODE,ISTA)-MAX_IZONES
      DMODE=STANUM(SC)
      RTEMP=TRIPS(ISTA)*STAEGR(1,SC,JZ)
      RTEMP3=TRIPS(ISTA)*STAEGR(4,SC,JZ)
      RTEMP2=TRIPS(ISTA)*
     *   (1.0-STAEGR(1,SC,JZ)-STAEGR(2,SC,JZ)-STAEGR(4,SC,JZ))
      STASUM2(SC,1)=STASUM2(SC,1)+RTEMP
      STASUM2(SC,3)=STASUM2(SC,3)+RTEMP2
      STASUM2(SC,4)=STASUM2(SC,4)+RTEMP3
      STASUM2(SC,2)=STASUM2(SC,2)+TRIPS(ISTA)-RTEMP-RTEMP2-RTEMP3
      IF(DMODE.GT.0) THEN 
      STAVOL(IMODE,DMODE)=STAVOL(IMODE,DMODE)+TRIPS(ISTA)
      CALL XFERSUM(1,1,DC,SC,TRIPS(ISTA),RTEMP,RTEMP2,RTEMP3)
      END IF
      END IF
      END DO     
C...BUS ACCESS STATIONS
      DO ISTA=1,2
      IF(TRIPS(ISTA+2).LE.0.0) CYCLE
      IF(CHBIND(IMODE,ISTA)) THEN
      DC=OSTA(IMODE,(ISTA+2))-MAX_IZONES
      SC2=BDSTA((IMODE+5),ISTA)-MAX_IZONES
      DMODE=STANUM(SC2)
      RTEMP=TRIPS(ISTA+2)*STAEGR(1,SC2,JZ)
      RTEMP3=TRIPS(ISTA+2)*STAEGR(4,SC2,JZ)
      RTEMP2=TRIPS(ISTA+2)*
     *   (1.0-STAEGR(1,SC2,JZ)-STAEGR(2,SC2,JZ)-STAEGR(4,SC2,JZ))
      STASUM4(DC,2)=STASUM4(DC,2)+TRIPS(ISTA+2)
      STASUM5(SC2,1)=STASUM5(SC2,1)+RTEMP
      STASUM5(SC2,3)=STASUM5(SC2,3)+RTEMP2
      STASUM5(SC2,4)=STASUM5(SC2,4)+RTEMP3
      STASUM5(SC2,2)=STASUM5(SC2,2)+TRIPS(ISTA+2)-RTEMP-RTEMP2-RTEMP3
      IF(DMODE.GT.0) THEN
      STAVOL(IMODE,DMODE)=STAVOL(IMODE,DMODE)+TRIPS(ISTA+2)
      CALL XFERSUM(2,2,DC,SC2,TRIPS(ISTA+2),RTEMP,RTEMP2,RTEMP3)
      END IF
      ELSE
      DC=OSTA(IMODE,(ISTA+2))-MAX_IZONES
      SC=ASTA(IMODE,(ISTA+2))-MAX_IZONES
      DMODE=STANUM(SC)
      RTEMP=TRIPS(ISTA+2)*STAEGR(1,SC,JZ)
      RTEMP3=TRIPS(ISTA+2)*STAEGR(4,SC,JZ)
      RTEMP2=TRIPS(ISTA+2)*
     *    (1.0-STAEGR(1,SC,JZ)-STAEGR(2,SC,JZ)-STAEGR(4,SC,JZ))
      STASUM2(SC,1)=STASUM2(SC,1)+RTEMP
      STASUM2(SC,3)=STASUM2(SC,3)+RTEMP2
      STASUM2(SC,4)=STASUM2(SC,4)+RTEMP3
      STASUM2(SC,2)=STASUM2(SC,2)+TRIPS(ISTA+2)-RTEMP-RTEMP2-RTEMP3
      IF(DMODE.GT.0) THEN 
      STAVOL(IMODE,DMODE)=STAVOL(IMODE,DMODE)+TRIPS(ISTA+2)
      CALL XFERSUM(1,2,DC,SC,TRIPS(ISTA+2),RTEMP,RTEMP2,RTEMP3)
      END IF
      END IF
      END DO      
C...PNR ACCESS STATIONS
      DO ISTA=1,4
      IF(TRIPS(ISTA+4).LE.0.0) CYCLE
      IF(CHPIND(IMODE,ISTA)) THEN
      DC=OSTA(IMODE,(ISTA+4))-MAX_IZONES
      SC2=PDSTA((IMODE+5),ISTA)-MAX_IZONES
      DMODE=STANUM(SC2)
      RTEMP=TRIPS(ISTA+4)*STAEGR(1,SC2,JZ)
      RTEMP3=TRIPS(ISTA+4)*STAEGR(4,SC2,JZ)
      RTEMP2=TRIPS(ISTA+4)*
     *    (1.0-STAEGR(1,SC2,JZ)-STAEGR(2,SC2,JZ)-STAEGR(4,SC2,JZ))
      STASUM4(DC,3)=STASUM4(DC,3)+TRIPS(ISTA+4)
      STASUM5(SC2,1)=STASUM5(SC2,1)+RTEMP
      STASUM5(SC2,3)=STASUM5(SC2,3)+RTEMP2
      STASUM5(SC2,4)=STASUM5(SC2,4)+RTEMP3
      STASUM5(SC2,2)=STASUM5(SC2,2)+TRIPS(ISTA+4)-RTEMP-RTEMP2-RTEMP3
      IF(DMODE.GT.0) THEN
      STAVOL(IMODE,DMODE)=STAVOL(IMODE,DMODE)+TRIPS(ISTA+4)
      CALL XFERSUM(2,3,DC,SC2,TRIPS(ISTA+4),RTEMP,RTEMP2,RTEMP3)
      END IF
      ELSE
      DC=OSTA(IMODE,(ISTA+4))-MAX_IZONES
      SC=ASTA(IMODE,(ISTA+4))-MAX_IZONES
      DMODE=STANUM(SC)
      RTEMP=TRIPS(ISTA+4)*STAEGR(1,SC,JZ)
      RTEMP3=TRIPS(ISTA+4)*STAEGR(4,SC,JZ)
      RTEMP2=TRIPS(ISTA+4)*
     *   (1.0-STAEGR(1,SC,JZ)-STAEGR(2,SC,JZ)-STAEGR(4,SC,JZ))
      STASUM2(SC,1)=STASUM2(SC,1)+RTEMP
      STASUM2(SC,3)=STASUM2(SC,3)+RTEMP2
      STASUM2(SC,4)=STASUM2(SC,4)+RTEMP3
      STASUM2(SC,2)=STASUM2(SC,2)+TRIPS(ISTA+4)-RTEMP-RTEMP2-RTEMP3   
      IF(DMODE.GT.0) THEN
      STAVOL(IMODE,DMODE)=STAVOL(IMODE,DMODE)+TRIPS(ISTA+4)
      CALL XFERSUM(1,3,DC,SC,TRIPS(ISTA+4),RTEMP,RTEMP2,RTEMP3)
      END IF
      END IF
      END DO   
C...KNR ACCESS STATIONS
      DO ISTA=1,4
      IF(TRIPS(ISTA+8).LE.0.0) CYCLE
      IF(CHKIND(IMODE,ISTA)) THEN
      DC=OSTA(IMODE,(ISTA+8))-MAX_IZONES
      SC2=KDSTA((IMODE+5),ISTA)-MAX_IZONES
      DMODE=STANUM(SC2)
      RTEMP=TRIPS(ISTA+8)*STAEGR(1,SC2,JZ)
      RTEMP3=TRIPS(ISTA+8)*STAEGR(4,SC2,JZ)
      RTEMP2=TRIPS(ISTA+8)*
     *   (1.0-STAEGR(1,SC2,JZ)-STAEGR(2,SC2,JZ)-STAEGR(4,SC2,JZ))
      STASUM4(DC,4)=STASUM4(DC,4)+TRIPS(ISTA+8)
      STASUM5(SC2,1)=STASUM5(SC2,1)+RTEMP
      STASUM5(SC2,3)=STASUM5(SC2,3)+RTEMP2
      STASUM5(SC2,4)=STASUM5(SC2,4)+RTEMP3
      STASUM5(SC2,2)=STASUM5(SC2,2)+TRIPS(ISTA+8)-RTEMP-RTEMP2-RTEMP3
      IF(DMODE.GT.0) THEN
      STAVOL(IMODE,DMODE)=STAVOL(IMODE,DMODE)+TRIPS(ISTA+8)
      CALL XFERSUM(2,4,DC,SC2,TRIPS(ISTA+8),RTEMP,RTEMP2,RTEMP3)
      END IF
      ELSE
      DC=OSTA(IMODE,(ISTA+8))-MAX_IZONES
      SC=ASTA(IMODE,(ISTA+8))-MAX_IZONES
      DMODE=STANUM(SC)
      RTEMP=TRIPS(ISTA+8)*STAEGR(1,SC,JZ)
      RTEMP3=TRIPS(ISTA+8)*STAEGR(4,SC,JZ)
      RTEMP2=TRIPS(ISTA+8)*
     *     (1.0-STAEGR(1,SC,JZ)-STAEGR(2,SC,JZ)-STAEGR(4,SC,JZ))
      STASUM2(SC,1)=STASUM2(SC,1)+RTEMP
      STASUM2(SC,3)=STASUM2(SC,3)+RTEMP2
      STASUM2(SC,4)=STASUM2(SC,4)+RTEMP3
      STASUM2(SC,2)=STASUM2(SC,2)+TRIPS(ISTA+8)-RTEMP-RTEMP2-RTEMP3
      IF(DMODE.GT.0) THEN
      STAVOL(IMODE,DMODE)=STAVOL(IMODE,DMODE)+TRIPS(ISTA+8)
      CALL XFERSUM(1,4,DC,SC,TRIPS(ISTA+8),RTEMP,RTEMP2,RTEMP3)
      END IF
      END IF
      END DO       
C...BIKE ACCESS STATIONS
      DO ISTA=1,2
      IF(TRIPS(ISTA+12).LE.0.0) CYCLE
      IF(CHBKIND(IMODE,ISTA)) THEN
      DC=OSTA(IMODE,(ISTA+12))-MAX_IZONES
      SC2=BIKDSTA((IMODE+5),ISTA)-MAX_IZONES
      DMODE=STANUM(SC2)
      RTEMP=TRIPS(ISTA+12)*STAEGR(1,SC2,JZ)
      RTEMP3=TRIPS(ISTA+12)*STAEGR(4,SC2,JZ)
      RTEMP2=TRIPS(ISTA+12)*
     *   (1.0-STAEGR(1,SC2,JZ)-STAEGR(2,SC2,JZ)-STAEGR(4,SC2,JZ))
      STASUM4(DC,5)=STASUM4(DC,5)+TRIPS(ISTA+12)
      STASUM5(SC2,1)=STASUM5(SC2,1)+RTEMP
      STASUM5(SC2,3)=STASUM5(SC2,3)+RTEMP2
      STASUM5(SC2,4)=STASUM5(SC2,4)+RTEMP3
      STASUM5(SC2,2)=STASUM5(SC2,2)+TRIPS(ISTA+12)-RTEMP-RTEMP2-RTEMP3
      IF(DMODE.GT.0) THEN
      STAVOL(IMODE,DMODE)=STAVOL(IMODE,DMODE)+TRIPS(ISTA+12)
      CALL XFERSUM(2,5,DC,SC2,TRIPS(ISTA+12),RTEMP,RTEMP2,RTEMP3)
      END IF
      ELSE
      DC=OSTA(IMODE,(ISTA+12))-MAX_IZONES
      SC=ASTA(IMODE,(ISTA+12))-MAX_IZONES
      DMODE=STANUM(SC)
      RTEMP=TRIPS(ISTA+12)*STAEGR(1,SC,JZ)
      RTEMP3=TRIPS(ISTA+12)*STAEGR(4,SC,JZ)
      RTEMP2=TRIPS(ISTA+12)*
     *     (1.0-STAEGR(1,SC,JZ)-STAEGR(2,SC,JZ)-STAEGR(4,SC,JZ))
      STASUM2(SC,1)=STASUM2(SC,1)+RTEMP
      STASUM2(SC,3)=STASUM2(SC,3)+RTEMP2
      STASUM2(SC,4)=STASUM2(SC,4)+RTEMP3
      STASUM2(SC,2)=STASUM2(SC,2)+TRIPS(ISTA+12)-RTEMP-RTEMP2-RTEMP3
      IF(DMODE.GT.0) THEN 
      STAVOL(IMODE,DMODE)=STAVOL(IMODE,DMODE)+TRIPS(ISTA+12)
      CALL XFERSUM(1,5,DC,SC,TRIPS(ISTA+12),RTEMP,RTEMP2,RTEMP3)
      END IF
      END IF
      END DO      
      END IF
      RETURN
      END
