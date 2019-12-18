C-------------------------------------------------------------------
C     BLENDED MODE ACCESS SUMMARY SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE ACCSUM(IZ,JZ,C,IMODE,TRIPS,
     *                  CHWIND,CHBKIND,CHBIND,CHPIND,CHKIND,
     *                  WDSTA,BIKDSTA,BDSTA,
     *                  PDSTA,KDSTA,XTESUM,XDISTRN,XTRIPS)
      INCLUDE 'stadat.com'
      INCLUDE 'param.com'
      INCLUDE 'mtamcpar.inc'
C
      INTEGER*2 IMODE,C,INDEX,DIZ,DJZ
      INTEGER*2 IZ,JZ,SC,ISTA,IC,DC,SC2
      INTEGER*2 WDSTA(10,5),BIKDSTA(10,5)
      INTEGER*2 BDSTA(10,2),PDSTA(10,10)
      INTEGER*2 KDSTA(10,10)
      REAL*8    TRIPS(14),XTESUM(36,6)
      REAL*8    XDISTRN(21,21,36)
      REAL*8    XTRIPS(74,MAX_ZONES)
      LOGICAL   CHWIND(5,2),CHBKIND(5,2),CHBIND(5,2)
      LOGICAL   CHPIND(5,4),CHKIND(5,4)
      IF(IMODE.EQ.2) INDEX=1
      IF(IMODE.EQ.5) INDEX=6
      DIZ=DISTEQ(IZ)
      DJZ=DISTEQ(JZ)
C
C...WALK ACCESS STATIONS
      DO ISTA=1,2
      IF(TRIPS(ISTA).LE.0.0) CYCLE
      IF(CHWIND(IMODE,ISTA)) THEN
      SC2=WDSTA((IMODE+5),ISTA)-MAX_IZONES 
      IF(IMODE.EQ.1) THEN
       IF(STANUM(SC2).EQ.2) THEN                                   !CR --> Urban Rail
       XTESUM(25,C)=XTESUM(25,C)+TRIPS(ISTA)
       XDISTRN(DIZ,DJZ,25)=XDISTRN(DIZ,DJZ,25)+TRIPS(ISTA)
       ELSE 
       XTESUM(31,C)=XTESUM(31,C)+TRIPS(ISTA)                       !CR --> BRT
       XDISTRN(DIZ,DJZ,31)=XDISTRN(DIZ,DJZ,31)+TRIPS(ISTA) 
       XTRIPS(72,JZ)=XTRIPS(72,JZ)+TRIPS(ISTA)                      
       END IF      
      CYCLE
      END IF
      IF(STANUM(SC2).EQ.1) THEN                                    !Urban Rail/BRT  --> Commuter Rail
      XTESUM(INDEX,C)=XTESUM(INDEX,C)+TRIPS(ISTA)
      XDISTRN(DIZ,DJZ,INDEX)=XDISTRN(DIZ,DJZ,INDEX)+TRIPS(ISTA)
      IF(IMODE.EQ.5) XTRIPS(71,JZ)=XTRIPS(71,JZ)+TRIPS(ISTA)
      ELSE
       IF(STANUM(SC2).EQ.2) THEN                                   !BRT --> Urban Rail
       XTESUM(11,C)=XTESUM(11,C)+TRIPS(ISTA)
       XDISTRN(DIZ,DJZ,11)=XDISTRN(DIZ,DJZ,11)+TRIPS(ISTA)
       XTRIPS(73,JZ)=XTRIPS(73,JZ)+TRIPS(ISTA)
       ELSE 
       XTESUM(19,C)=XTESUM(19,C)+TRIPS(ISTA)                       !Urban Rail --> BRT
       XDISTRN(DIZ,DJZ,19)=XDISTRN(DIZ,DJZ,19)+TRIPS(ISTA)   
       XTRIPS(74,JZ)=XTRIPS(74,JZ)+TRIPS(ISTA)                    
       END IF
      END IF
      END IF
      END DO     
C...BUS ACCESS STATIONS
      DO ISTA=1,2
      IF(TRIPS(ISTA+2).LE.0.0) CYCLE
      IF(CHBIND(IMODE,ISTA)) THEN
      SC2=BDSTA((IMODE+5),ISTA)-MAX_IZONES
      IF(IMODE.EQ.1) THEN
       IF(STANUM(SC2).EQ.2) THEN                                     !CR --> Urban Rail
       XTESUM(26,C)=XTESUM(26,C)+TRIPS(ISTA+2)
       XDISTRN(DIZ,DJZ,26)=XDISTRN(DIZ,DJZ,26)+TRIPS(ISTA+2)
       ELSE 
       XTESUM(32,C)=XTESUM(32,C)+TRIPS(ISTA+2)                       !CR --> BRT
       XDISTRN(DIZ,DJZ,32)=XDISTRN(DIZ,DJZ,32)+TRIPS(ISTA+2)  
       XTRIPS(72,JZ)=XTRIPS(72,JZ)+TRIPS(ISTA+2)                     
       END IF      
      CYCLE
      END IF
      IF(STANUM(SC2).EQ.1) THEN
      XTESUM((INDEX+1),C)=XTESUM((INDEX+1),C)+TRIPS(ISTA+2)
      XDISTRN(DIZ,DJZ,(INDEX+1))=XDISTRN(DIZ,DJZ,(INDEX+1))+
     *     TRIPS(ISTA+2)
      IF(IMODE.EQ.5) XTRIPS(71,JZ)=XTRIPS(71,JZ)+TRIPS(ISTA+2)
      ELSE
       IF(STANUM(SC2).EQ.2) THEN                                     !BRT --> Urban Rail
       XTESUM(12,C)=XTESUM(12,C)+TRIPS(ISTA+2)
       XDISTRN(DIZ,DJZ,12)=XDISTRN(DIZ,DJZ,12)+TRIPS(ISTA+2)
       XTRIPS(73,JZ)=XTRIPS(73,JZ)+TRIPS(ISTA+2)
       ELSE 
       XTESUM(20,C)=XTESUM(20,C)+TRIPS(ISTA+2)                       !Urban Rail --> BRT
       XDISTRN(DIZ,DJZ,20)=XDISTRN(DIZ,DJZ,20)+TRIPS(ISTA+2)
       XTRIPS(74,JZ)=XTRIPS(74,JZ)+TRIPS(ISTA+2)
       END IF
      END IF
      END IF
      END DO      
C...PNR ACCESS STATIONS
      DO ISTA=1,4
      IF(TRIPS(ISTA+4).LE.0.0) CYCLE
      IF(CHPIND(IMODE,ISTA)) THEN
      SC2=PDSTA((IMODE+5),ISTA)-MAX_IZONES
      IF(IMODE.EQ.1) THEN
       IF(STANUM(SC2).EQ.2) THEN                                     !CR --> Urban Rail
       XTESUM(27,C)=XTESUM(27,C)+TRIPS(ISTA+4)
       XDISTRN(DIZ,DJZ,27)=XDISTRN(DIZ,DJZ,27)+TRIPS(ISTA+4)
       ELSE 
       XTESUM(33,C)=XTESUM(33,C)+TRIPS(ISTA+4)                       !CR --> BRT
       XDISTRN(DIZ,DJZ,33)=XDISTRN(DIZ,DJZ,33)+TRIPS(ISTA+4)  
       XTRIPS(72,JZ)=XTRIPS(72,JZ)+TRIPS(ISTA+4)                     
       END IF      
      CYCLE
      END IF
      IF(STANUM(SC2).EQ.1) THEN
      XTESUM((INDEX+2),C)=XTESUM((INDEX+2),C)+TRIPS(ISTA+4)
      XDISTRN(DIZ,DJZ,(INDEX+2))=XDISTRN(DIZ,DJZ,(INDEX+2))+
     *     TRIPS(ISTA+4)
      IF(IMODE.EQ.5) XTRIPS(71,JZ)=XTRIPS(71,JZ)+TRIPS(ISTA+4)
      ELSE
       IF(STANUM(SC2).EQ.2) THEN                                     !BRT --> Urban Rail
       XTESUM(13,C)=XTESUM(13,C)+TRIPS(ISTA+4)
       XDISTRN(DIZ,DJZ,13)=XDISTRN(DIZ,DJZ,13)+TRIPS(ISTA+4)
       XTRIPS(73,JZ)=XTRIPS(73,JZ)+TRIPS(ISTA+4)
       ELSE  
       XTESUM(21,C)=XTESUM(21,C)+TRIPS(ISTA+4)                       !Urban Rail --> BRT
       XDISTRN(DIZ,DJZ,21)=XDISTRN(DIZ,DJZ,21)+TRIPS(ISTA+4)
       XTRIPS(74,JZ)=XTRIPS(74,JZ)+TRIPS(ISTA+4)
       END IF
      END IF
      END IF
      END DO   
C...KNR ACCESS STATIONS
      DO ISTA=1,4
      IF(TRIPS(ISTA+8).LE.0.0) CYCLE
      IF(CHKIND(IMODE,ISTA)) THEN
      SC2=KDSTA((IMODE+5),ISTA)-MAX_IZONES
      IF(IMODE.EQ.1) THEN
       IF(STANUM(SC2).EQ.2) THEN                                     !CR --> Urban Rail
       XTESUM(28,C)=XTESUM(28,C)+TRIPS(ISTA+8)
       XDISTRN(DIZ,DJZ,28)=XDISTRN(DIZ,DJZ,28)+TRIPS(ISTA+8)
       ELSE 
       XTESUM(34,C)=XTESUM(34,C)+TRIPS(ISTA+8)                       !CR --> BRT
       XDISTRN(DIZ,DJZ,34)=XDISTRN(DIZ,DJZ,34)+TRIPS(ISTA+8) 
       XTRIPS(72,JZ)=XTRIPS(72,JZ)+TRIPS(ISTA+8)                      
       END IF      
      CYCLE
      END IF
      IF(STANUM(SC2).EQ.1) THEN
      XTESUM((INDEX+3),C)=XTESUM((INDEX+3),C)+TRIPS(ISTA+8)
      XDISTRN(DIZ,DJZ,(INDEX+3))=XDISTRN(DIZ,DJZ,(INDEX+3))+
     *     TRIPS(ISTA+8)
      IF(IMODE.EQ.5) XTRIPS(71,JZ)=XTRIPS(71,JZ)+TRIPS(ISTA+8)
      ELSE
       IF(STANUM(SC2).EQ.2) THEN                                     !BRT --> Urban Rail
       XTESUM(14,C)=XTESUM(14,C)+TRIPS(ISTA+8)
       XDISTRN(DIZ,DJZ,14)=XDISTRN(DIZ,DJZ,14)+TRIPS(ISTA+8)
       XTRIPS(73,JZ)=XTRIPS(73,JZ)+TRIPS(ISTA+8)
       ELSE
       XTESUM(22,C)=XTESUM(22,C)+TRIPS(ISTA+8)                       !Urban Rail --> BRT
       XDISTRN(DIZ,DJZ,22)=XDISTRN(DIZ,DJZ,22)+TRIPS(ISTA+8)
       XTRIPS(74,JZ)=XTRIPS(74,JZ)+TRIPS(ISTA+8)
       END IF
      END IF
      END IF
      END DO       
C...BIKE ACCESS STATIONS
      DO ISTA=1,2
      IF(TRIPS(ISTA+12).LE.0.0) CYCLE
      IF(CHBKIND(IMODE,ISTA)) THEN
      SC2=BIKDSTA((IMODE+5),ISTA)-MAX_IZONES
      IF(IMODE.EQ.1) THEN
       IF(STANUM(SC2).EQ.2) THEN                                     !CR --> Urban Rail
       XTESUM(29,C)=XTESUM(29,C)+TRIPS(ISTA+12)
       XDISTRN(DIZ,DJZ,29)=XDISTRN(DIZ,DJZ,29)+TRIPS(ISTA+12)
       ELSE 
       XTESUM(35,C)=XTESUM(35,C)+TRIPS(ISTA+12)                       !CR --> BRT
       XDISTRN(DIZ,DJZ,35)=XDISTRN(DIZ,DJZ,35)+TRIPS(ISTA+12)  
       XTRIPS(72,JZ)=XTRIPS(72,JZ)+TRIPS(ISTA+12)                     
       END IF      
      CYCLE
      END IF
      IF(STANUM(SC2).EQ.1) THEN
      XTESUM((INDEX+4),C)=XTESUM((INDEX+4),C)+TRIPS(ISTA+12)
      XDISTRN(DIZ,DJZ,(INDEX+4))=XDISTRN(DIZ,DJZ,(INDEX+4))+
     *     TRIPS(ISTA+12)
      IF(IMODE.EQ.5) XTRIPS(71,JZ)=XTRIPS(71,JZ)+TRIPS(ISTA+12)
      ELSE
       IF(STANUM(SC2).EQ.2) THEN                                      !BRT --> Urban Rail
       XTESUM(15,C)=XTESUM(15,C)+TRIPS(ISTA+12)
       XDISTRN(DIZ,DJZ,15)=XDISTRN(DIZ,DJZ,15)+TRIPS(ISTA+12)
       XTRIPS(73,JZ)=XTRIPS(73,JZ)+TRIPS(ISTA+12)
       ELSE
       XTESUM(23,C)=XTESUM(23,C)+TRIPS(ISTA+12)                       !Urban Rail --> BRT
       XDISTRN(DIZ,DJZ,23)=XDISTRN(DIZ,DJZ,23)+TRIPS(ISTA+12)
       XTRIPS(74,JZ)=XTRIPS(74,JZ)+TRIPS(ISTA+12)
       END IF
      END IF
      END IF
      END DO      
      RETURN
      END
