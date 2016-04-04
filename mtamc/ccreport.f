      SUBROUTINE CCREPORT(SLUNREL,ELUNREL,ALUNREL,LBLUNREL,
     *                    RBLUNREL,EBLUNREL,TWLUNREL,
     *                    SNUNREL,ENUNREL,ANUNREL,LBNUNREL,
     *                    RBNUNREL,EBNUNREL,TWNUNREL,
     *                    SLCROWD,ELCROWD,ALCROWD,LBLCROWD,
     *                    RBLCROWD,EBLCROWD,TWLCROWD,
     *                    SNCAPAC,ENCAPAC,ANCAPAC,LBNCAPAC,
     *                    RBNCAPAC,EBNCAPAC,TWNCAPAC)
      INCLUDE 'param.com'
      INCLUDE 'mtamcpar.inc'
      INTEGER*2     T
      REAL*4        DIFF,REM,TEMP,AVG1,AVG2,AVG3,AVG4,TOTTRIP(3)
      REAL*4        SLUNREL(MAX_TLF,2,4),ELUNREL(MAX_TLF,5,10)
      REAL*4        ALUNREL(MAX_TLF,2,10),LBLUNREL(MAX_TLF,6)
      REAL*4        RBLUNREL(MAX_TLF,8),EBLUNREL(MAX_TLF,7)
      REAL*4        TWLUNREL(MAX_TLF,8)
      REAL*4        SNUNREL(MAX_WAIT,2,4),ENUNREL(MAX_WAIT,5,10)
      REAL*4        ANUNREL(MAX_WAIT,2,10),LBNUNREL(MAX_WAIT,6)
      REAL*4        RBNUNREL(MAX_WAIT,8),EBNUNREL(MAX_WAIT,7)
      REAL*4        TWNUNREL(MAX_WAIT,8)
      REAL*4        SLCROWD(MAX_TLF,2,4),SNCAPAC(MAX_WAIT,2,4)
      REAL*4        ELCROWD(MAX_TLF,5,10),ENCAPAC(MAX_WAIT,5,10)
      REAL*4        ALCROWD(MAX_TLF,2,10),ANCAPAC(MAX_WAIT,2,10)
      REAL*4        LBLCROWD(MAX_TLF,6),LBNCAPAC(MAX_WAIT,6)
      REAL*4        RBLCROWD(MAX_TLF,8),RBNCAPAC(MAX_WAIT,8)
      REAL*4        EBLCROWD(MAX_TLF,7),EBNCAPAC(MAX_WAIT,7)
      REAL*4        TWLCROWD(MAX_TLF,8),TWNCAPAC(MAX_WAIT,8)
C ----------------------------------------------------------------------
C   OUTPUT LINK RELIABILITY SUMMARIES
C ----------------------------------------------------------------------
      IF(LUNREL) THEN
      IF(MPRICE.GT.0) THEN
      WRITE(26,9410) MPRICE,MAX_TLF
 9410 FORMAT(95('-')/
     *       ' MTAMC 9410 (W) MAXIMUM IN-VEHICLE TIME OF ',F8.1,
     *       ' EXCEEDS MAXIMUM ALLOWABLE VALUE OF ',I5/
     *       ' PROGRAM SHOULD BE RE-COMPLILED WITH INCREASED',
     *        ' VALUE'/,95('-'))
      END IF
C.....STATION TO STATION
      WRITE(95,9401)
 9401 FORMAT('Station,To,Station'/
     *       'Commuter_Rail,,,,,,,,Urban_Rail'/
     *       'In_Vehicle_Time,Interchanges,Tot_Minutes,Average,',
     *       'Trips,WMinutes,WAverage,Percent,'
     *       'Interchanges,Tot_Minutes,Average,',
     *       'Trips,WMinutes,WAverage,Percent')
      TOTTRIP=0
      DO T=1,MAX_TLF
      DIFF=0.0
      AVG1=0.0
      AVG2=0.0
      REM=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+SLUNREL(T,1,3)
      TOTTRIP(2)=TOTTRIP(2)+SLUNREL(T,2,3)
      IF(SLUNREL(T,1,1).GT.0) DIFF=SLUNREL(T,1,2)/SLUNREL(T,1,1)
      IF(SLUNREL(T,1,3).GT.0) AVG1=SLUNREL(T,1,4)/SLUNREL(T,1,3)
      IF(T.GT.1) AVG2=(AVG1/FLOAT(T-1))*100.0
      IF(SLUNREL(T,2,1).GT.0) REM=SLUNREL(T,2,2)/SLUNREL(T,2,1)
      IF(SLUNREL(T,2,3).GT.0) AVG3=SLUNREL(T,2,4)/SLUNREL(T,2,3)
      IF(T.GT.1) AVG4=(AVG3/FLOAT(T-1))*100.0           
      IF((SLUNREL(T,1,3)+SLUNREL(T,2,3)).GT.0.01) 
     *    WRITE(95,9402) (T-1),SLUNREL(T,1,1),SLUNREL(T,1,2),DIFF,
     *                    SLUNREL(T,1,3),SLUNREL(T,1,4),AVG1,AVG2,
     *                    SLUNREL(T,2,1),SLUNREL(T,2,2),REM,
     *                    SLUNREL(T,2,3),SLUNREL(T,2,4),AVG3,AVG4
      END DO
      WRITE(95,9445) TOTTRIP(1),TOTTRIP(2)
 9445 FORMAT('Total,,,,',F8.2,',,,,,,,',F8.2)
C.....STATION TO ZONE - COMMUTER RAIL AND URBAN RAIL
      WRITE(95,9408)
 9408 FORMAT('Station,To,Zone'/
     *       'Commuter_Rail,,,,,,,,,,,,,,Urban_Rail'/
     *       'In_Vehicle_Time,Interchanges,Tot_Minutes,Average,Local,',
     *       'Rapid,Express,Transitway,BRT,UrbanRail,',
     *       'Trips,Tot_WMinutes,WAverage,Percent,',
     *       'Interchanges,Tot_Minutes,Average,Local,Rapid,Express,',
     *       'Transitway,BRT,UrbanRail,',
     *       'Trips,Tot_WMinutes,WAverage,Percent')
      TOTTRIP=0.0
      DO T=1,MAX_TLF
      DIFF=0.0
      REM=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+ELUNREL(T,1,9)
      TOTTRIP(2)=TOTTRIP(2)+ELUNREL(T,2,9)
      IF(ELUNREL(T,1,1).GT.0) DIFF=ELUNREL(T,1,2)/ELUNREL(T,1,1)
      IF(ELUNREL(T,1,9).GT.0) AVG1=ELUNREL(T,1,10)/ELUNREL(T,1,9)
      IF(T.GT.1) AVG2=(AVG1/FLOAT(T-1))*100.0      
      IF(ELUNREL(T,2,1).GT.0) REM=ELUNREL(T,2,2)/ELUNREL(T,2,1)   
      IF(ELUNREL(T,2,9).GT.0) AVG3=ELUNREL(T,2,10)/ELUNREL(T,2,9)
      IF(T.GT.1) AVG4=(AVG3/FLOAT(T-1))*100.0       
      IF((ELUNREL(T,1,9)+ELUNREL(T,2,9)).GT.0.01) 
     *    WRITE(95,9402) (T-1),ELUNREL(T,1,1),ELUNREL(T,1,2),DIFF,
     *                     ELUNREL(T,1,3),ELUNREL(T,1,4),
     *                     ELUNREL(T,1,5),ELUNREL(T,1,6),
     *                     ELUNREL(T,1,7),ELUNREL(T,1,8),
     *                     ELUNREL(T,1,9),ELUNREL(T,1,10),AVG1,AVG2,
     *                     ELUNREL(T,2,1),ELUNREL(T,2,2),REM,
     *                     ELUNREL(T,2,3),ELUNREL(T,2,4),
     *                     ELUNREL(T,2,5),ELUNREL(T,2,6),
     *                     ELUNREL(T,2,7),ELUNREL(T,2,8),
     *                     ELUNREL(T,2,9),ELUNREL(T,2,10),AVG3,AVG4
 9402 FORMAT(I3,50(',',F8.2))
      END DO
      WRITE(95,9446) TOTTRIP(1),TOTTRIP(2)
 9446 FORMAT('Total,,,,,,,,,,',F8.2,',,,,,,,,,,,,,',F8.2)
C.....STATION TO ZONE - EXPRESS BUS, TRANSITWAY AND BRT
      WRITE(95,9405)
 9405 FORMAT('Station,To,Zone'/
     *       'Express_Bus,,,,,,,,,,,,,,Transitway,,,,,,,,,,,,,BRT'/
     *       'In_Vehicle_Time,Interchanges,Tot_Minutes,Average,Local,',
     *       'Rapid,Express,Transitway,BRT,UrbanRail,',
     *       'Drv_Trips,Tot_WMinutes,WAverage,Percent,',
     *       'Interchanges,Tot_Minutes,Average,Local,Rapid,Express,',
     *       'Transitway,BRT,UrbanRail,',
     *       'Drv_Trips,Tot_WMinutes,WAverage,Percent,',
     *       'Interchanges,Tot_Minutes,Average,Local,Rapid,Express,',
     *       'Transitway,BRT,UrbanRail')
      TOTTRIP=0.0
      DO T=1,MAX_TLF
      DIFF=0.0
      REM=0.0
      TEMP=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+ELUNREL(T,3,9)
      TOTTRIP(2)=TOTTRIP(2)+ELUNREL(T,4,9)
      TOTTRIP(3)=TOTTRIP(3)+ELUNREL(T,5,9)
      IF(ELUNREL(T,3,1).GT.0) DIFF=ELUNREL(T,3,2)/ELUNREL(T,3,1)
      IF(ELUNREL(T,3,9).GT.0) AVG1=ELUNREL(T,3,10)/ELUNREL(T,3,9)
      IF(T.GT.1) AVG2=(AVG1/FLOAT(T-1))*100.0
      IF(ELUNREL(T,4,1).GT.0) REM=ELUNREL(T,4,2)/ELUNREL(T,4,1)
      IF(ELUNREL(T,4,9).GT.0) AVG3=ELUNREL(T,4,10)/ELUNREL(T,4,9)
      IF(T.GT.1) AVG4=(AVG3/FLOAT(T-1))*100.0      
      IF(ELUNREL(T,5,1).GT.0) TEMP=ELUNREL(T,5,2)/ELUNREL(T,5,1)    
      IF((ELUNREL(T,3,9)+ELUNREL(T,4,9)+ELUNREL(T,5,9)).GT.0.01) 
     *    WRITE(95,9402) (T-1),ELUNREL(T,3,1),ELUNREL(T,3,2),DIFF,
     *                     ELUNREL(T,3,3),ELUNREL(T,3,4),
     *                     ELUNREL(T,3,5),ELUNREL(T,3,6),
     *                     ELUNREL(T,3,7),ELUNREL(T,3,8),
     *                     ELUNREL(T,3,9),ELUNREL(T,3,10),AVG1,AVG2,
     *                    ELUNREL(T,4,1),ELUNREL(T,4,2),REM,
     *                     ELUNREL(T,4,3),ELUNREL(T,4,4),
     *                     ELUNREL(T,4,5),ELUNREL(T,4,6),
     *                     ELUNREL(T,4,7),ELUNREL(T,4,8),
     *                     ELUNREL(T,4,9),ELUNREL(T,4,10),AVG3,AVG4,
     *                    ELUNREL(T,5,1),ELUNREL(T,5,2),TEMP,
     *                     ELUNREL(T,5,3),ELUNREL(T,5,4),
     *                     ELUNREL(T,5,5),ELUNREL(T,5,6),
     *                     ELUNREL(T,5,7),ELUNREL(T,5,8)
      END DO
      WRITE(95,9447) TOTTRIP(1),TOTTRIP(2),TOTTRIP(3)
 9447 FORMAT('Total,,,,,,,,,,',F8.2,',,,,,,,,,,,,,',F8.2,
     *       ',,,,,,,,,,,,,',F8.2)
C.....ZONE TO STATION - COMMUTER RAIL AND URBAN RAIL
      WRITE(95,9409)
 9409 FORMAT('Zone,To,Station'/
     *       'Commuter_Rail,,,,,,,,,,,,,,Urban_Rail'/
     *       'In_Vehicle_Time,Interchanges,Tot_Minutes,Average,Local,',
     *       'Rapid,Express,Transitway,BRT,UrbanRail,',
     *       'Trips,Tot_WMinutes,WAverage,Percent,',
     *       'Interchanges,Tot_Minutes,Average,Local,Rapid,Express,',
     *       'Transitway,BRT,UrbanRail,',
     *       'Trips,Tot_WMinutes,WAverage,Percent')
      TOTTRIP=0.0
      DO T=1,MAX_TLF
      DIFF=0.0
      REM=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+ALUNREL(T,1,9)
      TOTTRIP(2)=TOTTRIP(2)+ALUNREL(T,2,9)
      IF(ALUNREL(T,1,1).GT.0) DIFF=ALUNREL(T,1,2)/ALUNREL(T,1,1)
      IF(ALUNREL(T,1,9).GT.0) AVG1=ALUNREL(T,1,10)/ALUNREL(T,1,9)
      IF(T.GT.1) AVG2=(AVG1/FLOAT(T-1))*100.0
      IF(ALUNREL(T,2,1).GT.0) REM=ALUNREL(T,2,2)/ALUNREL(T,2,1) 
      IF(ALUNREL(T,2,9).GT.0) AVG3=ALUNREL(T,2,10)/ALUNREL(T,2,9)
      IF(T.GT.1) AVG4=(AVG3/FLOAT(T-1))*100.0     
      IF((ALUNREL(T,1,9)+ALUNREL(T,2,9)).GT.0.01) 
     *    WRITE(95,9402) (T-1),ALUNREL(T,1,1),ALUNREL(T,1,2),DIFF,
     *                     ALUNREL(T,1,3),ALUNREL(T,1,4),
     *                     ALUNREL(T,1,5),ALUNREL(T,1,6),
     *                     ALUNREL(T,1,7),ALUNREL(T,1,8),
     *                     ALUNREL(T,1,9),ALUNREL(T,1,10),AVG1,AVG2,
     *                     ALUNREL(T,2,1),ALUNREL(T,2,2),REM,
     *                     ALUNREL(T,2,3),ALUNREL(T,2,4),
     *                     ALUNREL(T,2,5),ALUNREL(T,2,6),
     *                     ALUNREL(T,2,7),ALUNREL(T,2,8),
     *                     ALUNREL(T,2,9),ALUNREL(T,2,10),AVG3,AVG4     
      END DO
      WRITE(95,9446) TOTTRIP(1),TOTTRIP(2)
C.....LOCAL BUS
      WRITE(95,9403)
 9403 FORMAT('Local_Bus',/,'In-Vehicle_Time,',
     *       'Interchanges,Tot_Minutes,Average,',
     *       'Wlk_Trips,Tot_WMinutes,WAverage,',
     *       'Percent,Drv_Trips,Tot_WMinutes,WAverage,Percent')
      TOTTRIP=0.0
      DO T=1,MAX_TLF
      DIFF=0.0
      REM=0.0
      TEMP=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+LBLUNREL(T,3)
      TOTTRIP(2)=TOTTRIP(2)+LBLUNREL(T,5)
      IF(LBLUNREL(T,1).GT.0) DIFF=LBLUNREL(T,2)/LBLUNREL(T,1)
      IF(LBLUNREL(T,3).GT.0) REM=LBLUNREL(T,4)/LBLUNREL(T,3)
      IF(LBLUNREL(T,5).GT.0) TEMP=LBLUNREL(T,6)/LBLUNREL(T,5)
      IF(T.GT.1) AVG1=(REM/FLOAT(T-1))*100.0
      IF(T.GT.1) AVG2=(TEMP/FLOAT(T-1))*100.0      
      IF((LBLUNREL(T,3)+LBLUNREL(T,5)).GT.0.01) 
     *    WRITE(95,9402) (T-1),LBLUNREL(T,1),LBLUNREL(T,2),DIFF,
     *                   LBLUNREL(T,3),LBLUNREL(T,4),REM,AVG1,
     *                   LBLUNREL(T,5),LBLUNREL(T,6),TEMP,AVG2
      END DO
      WRITE(95,9448) TOTTRIP(1),TOTTRIP(2)
 9448 FORMAT('Total,,,,',F10.2,',,,,',F10.2)
C.....RAPID BUS
      WRITE(95,9404)
 9404 FORMAT('Rapid_Bus',/,'In-Vehicle_Time,',
     *       'Interchanges,Tot_Minutes,Average,Local,Rapid,',
     *       'Wlk_Trips,Tot_WMinutes,WAverage,',
     *       'Percent,Drv_Trips,Tot_WMinutes,WAverage,Percent')
      TOTTRIP=0.0
      DO T=1,MAX_TLF
      DIFF=0.0
      REM=0.0
      TEMP=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+RBLUNREL(T,5)
      TOTTRIP(2)=TOTTRIP(2)+RBLUNREL(T,7)
      IF(RBLUNREL(T,1).GT.0) DIFF=RBLUNREL(T,2)/RBLUNREL(T,1)
      IF(RBLUNREL(T,5).GT.0) REM=RBLUNREL(T,6)/RBLUNREL(T,5)
      IF(RBLUNREL(T,7).GT.0) TEMP=RBLUNREL(T,8)/RBLUNREL(T,7)
      IF(T.GT.1) AVG1=(REM/FLOAT(T-1))*100.0
      IF(T.GT.1) AVG2=(TEMP/FLOAT(T-1))*100.0         
      IF((RBLUNREL(T,5)+RBLUNREL(T,7)).GT.0.01) 
     *    WRITE(95,9402) (T-1),RBLUNREL(T,1),RBLUNREL(T,2),DIFF,
     *    RBLUNREL(T,3),RBLUNREL(T,4),
     *                   RBLUNREL(T,5),RBLUNREL(T,6),REM,AVG1,
     *                   RBLUNREL(T,7),RBLUNREL(T,8),TEMP,AVG2
      END DO
      WRITE(95,9449) TOTTRIP(1),TOTTRIP(2)
 9449 FORMAT('Total,,,,,',F10.2,',,,,',F10.2)
C.....WALK TO EXPRESS BUS
      WRITE(95,9406)
 9406 FORMAT('Walk_Express_Bus',/,'In-Vehicle_Time',
     *       ',Interchanges,Tot_Minutes,',
     *       'Average,',
     *       'Local,Rapid,Express,Trips,Tot_WMinutes,WAverage,Percent')
      TOTTRIP=0.0
      DO T=1,MAX_TLF
      DIFF=0.0
      REM=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+EBLUNREL(T,6)
      IF(EBLUNREL(T,1).GT.0) DIFF=EBLUNREL(T,2)/EBLUNREL(T,1)
      IF(EBLUNREL(T,6).GT.0) REM=EBLUNREL(T,7)/EBLUNREL(T,6) 
      IF(T.GT.1) AVG2=(REM/FLOAT(T-1))*100.0       
      IF(EBLUNREL(T,6).GT.0.01) 
     *    WRITE(95,9402) (T-1),EBLUNREL(T,1),EBLUNREL(T,2),DIFF,
     *    EBLUNREL(T,3),EBLUNREL(T,4),EBLUNREL(T,5),
     *    EBLUNREL(T,6),EBLUNREL(T,7),REM,AVG2
      END DO
      WRITE(95,9450) TOTTRIP(1)
 9450 FORMAT('Total,,,,,,,',F10.2)
C.....WALK TO TRANSITWAY
      WRITE(95,9407)
 9407 FORMAT('Walk_Transitway',/,'In-Vehicle_Time,',
     *       'Interchanges,Tot_Minutes,Average,',
     *       'Local,Rapid,Express,Transitway,Trips,Tot_WMinutes,',
     *       'WAverage,Percent')
      TOTTRIP=0.0
      DO T=1,MAX_TLF
      DIFF=0.0
      REM=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+TWLUNREL(T,7)
      IF(TWLUNREL(T,1).GT.0) DIFF=TWLUNREL(T,2)/TWLUNREL(T,1)
      IF(TWLUNREL(T,7).GT.0)  REM=TWLUNREL(T,8)/TWLUNREL(T,7) 
      IF(T.GT.1) AVG2=(REM/FLOAT(T-1))*100.0      
      IF(TWLUNREL(T,7).GT.0.01) 
     *    WRITE(95,9402) (T-1),TWLUNREL(T,1),TWLUNREL(T,2),DIFF,
     *    TWLUNREL(T,3),TWLUNREL(T,4),TWLUNREL(T,5),
     *    TWLUNREL(T,6),TWLUNREL(T,7),TWLUNREL(T,8),REM,AVG2
      END DO
      WRITE(95,9451) TOTTRIP(1)
 9451 FORMAT('Total,,,,,,,,',F10.2)      
      END IF
C ----------------------------------------------------------------
C   OUTPUT STOP RELIABILITY SUMMARIES
C ----------------------------------------------------------------
      IF(NUNREL) THEN
      IF(QPRICE.GT.0) THEN
      WRITE(26,9510) QPRICE,MAX_WAIT
 9510 FORMAT(95('-')/
     *       ' MTAMC 9510 (W) MAXIMUM WAIT TIME OF ',F8.1,
     *       ' EXCEEDS MAXIMUM ALLOWABLE VALUE OF ',I5/
     *       ' PROGRAM SHOULD BE RE-COMPLILED WITH INCREASED',
     *        ' VALUE'/,95('-'))
      END IF
C.....STATION TO STATION
      WRITE(96,9501)
 9501 FORMAT('Station,To,Station'/
     *       'Commuter_Rail,,,,,,,,Urban_Rail'/
     *       'Wait_Time,Interchanges,Tot_Minutes,Average,',
     *       'Trips,WMinutes,WAverage,Percent,'
     *       'Interchanges,Tot_Minutes,Average,',
     *       'Trips,WMinutes,WAverage,Percent')
      TOTTRIP=0.0
      DO T=1,MAX_WAIT
      DIFF=0.0
      AVG1=0.0
      AVG2=0.0
      REM=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+SNUNREL(T,1,3)
      TOTTRIP(2)=TOTTRIP(2)+SNUNREL(T,2,3)
      IF(SNUNREL(T,1,1).GT.0) DIFF=SNUNREL(T,1,2)/SNUNREL(T,1,1)
      IF(SNUNREL(T,1,3).GT.0) AVG1=SNUNREL(T,1,4)/SNUNREL(T,1,3)
      IF(T.GT.1) AVG2=(AVG1/FLOAT(T-1))*100.0
      IF(SNUNREL(T,2,1).GT.0) REM=SNUNREL(T,2,2)/SNUNREL(T,2,1)
      IF(SNUNREL(T,2,3).GT.0) AVG3=SNUNREL(T,2,4)/SNUNREL(T,2,3)
      IF(T.GT.1) AVG4=(AVG3/FLOAT(T-1))*100.0           
      IF((SNUNREL(T,1,3)+SNUNREL(T,2,3)).GT.0.01) 
     *    WRITE(96,9402) (T-1),SNUNREL(T,1,1),SNUNREL(T,1,2),DIFF,
     *                    SNUNREL(T,1,3),SNUNREL(T,1,4),AVG1,AVG2,
     *                    SNUNREL(T,2,1),SNUNREL(T,2,2),REM,
     *                    SNUNREL(T,2,3),SNUNREL(T,2,4),AVG3,AVG4
      END DO
      WRITE(96,9445) TOTTRIP(1),TOTTRIP(2)
C.....STATION TO ZONE - COMMUTER RAIL AND URBAN RAIL
      DIFF=0.0
      REM=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      WRITE(96,9508)
 9508 FORMAT('Station,To,Zone'/
     *       'Commuter_Rail,,,,,,,,,,,,,,Urban_Rail'/
     *       'Wait_Time,Interchanges,Tot_Minutes,Average,Local,',
     *       'Rapid,Express,Transitway,BRT,UrbanRail,',
     *       'Trips,Tot_WMinutes,WAverage,Percent,',
     *       'Interchanges,Tot_Minutes,Average,Local,Rapid,Express,',
     *       'Transitway,BRT,UrbanRail,',
     *       'Trips,Tot_WMinutes,WAverage,Percent,')
      TOTTRIP=0.0
      DO T=1,MAX_WAIT
      DIFF=0.0
      REM=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+ENUNREL(T,1,9)
      TOTTRIP(2)=TOTTRIP(2)+ENUNREL(T,2,9)
      IF(ENUNREL(T,1,1).GT.0) DIFF=ENUNREL(T,1,2)/ENUNREL(T,1,1)
      IF(ENUNREL(T,1,9).GT.0) AVG1=ENUNREL(T,1,10)/ENUNREL(T,1,9)
      IF(T.GT.1) AVG2=(AVG1/FLOAT(T-1))*100.0      
      IF(ENUNREL(T,2,1).GT.0) REM=ENUNREL(T,2,2)/ENUNREL(T,2,1)   
      IF(ENUNREL(T,2,9).GT.0) AVG3=ENUNREL(T,2,10)/ENUNREL(T,2,9)
      IF(T.GT.1) AVG4=(AVG3/FLOAT(T-1))*100.0       
      IF((ENUNREL(T,1,9)+ENUNREL(T,2,9)).GT.0.01) 
     *    WRITE(96,9402) (T-1),ENUNREL(T,1,1),ENUNREL(T,1,2),DIFF,
     *                     ENUNREL(T,1,3),ENUNREL(T,1,4),
     *                     ENUNREL(T,1,5),ENUNREL(T,1,6),
     *                     ENUNREL(T,1,7),ENUNREL(T,1,8),
     *                     ENUNREL(T,1,9),ENUNREL(T,1,10),AVG1,AVG2,
     *                     ENUNREL(T,2,1),ENUNREL(T,2,2),REM,
     *                     ENUNREL(T,2,3),ENUNREL(T,2,4),
     *                     ENUNREL(T,2,5),ENUNREL(T,2,6),
     *                     ENUNREL(T,2,7),ENUNREL(T,2,8),
     *                     ENUNREL(T,2,9),ENUNREL(T,2,10),AVG3,AVG4
      END DO
      WRITE(96,9446) TOTTRIP(1),TOTTRIP(2)
C.....STATION TO ZONE - EXPRESS BUS, TRANSITWAY AND BRT
      WRITE(96,9505)
 9505 FORMAT('Station,To,Zone'/
     *       'Express_Bus,,,,,,,,,,,,,,Transitway,,,,,,,,,,,,,BRT'/
     *       'Wait_Time,Interchanges,Tot_Minutes,Average,Local,',
     *       'Rapid,Express,Transitway,BRT,UrbanRail,',
     *       'Drv_Trips,Tot_WMinutes,WAverage,Percent,',
     *       'Interchanges,Tot_Minutes,Average,Local,Rapid,Express,',
     *       'Transitway,BRT,UrbanRail,',
     *       'Drv_Trips,Tot_WMinutes,WAverage,Percent,',
     *       'Interchanges,Tot_Minutes,Average,Local,Rapid,Express,',
     *       'Transitway,BRT,UrbanRail')
      TOTTRIP=0.0
      DO T=1,MAX_WAIT
      DIFF=0.0
      REM=0.0
      TEMP=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+ENUNREL(T,3,9)
      TOTTRIP(2)=TOTTRIP(2)+ENUNREL(T,4,9)
      TOTTRIP(3)=TOTTRIP(3)+ENUNREL(T,5,9)
      IF(ENUNREL(T,3,1).GT.0) DIFF=ENUNREL(T,3,2)/ENUNREL(T,3,1)
      IF(ENUNREL(T,3,9).GT.0) AVG1=ENUNREL(T,3,10)/ENUNREL(T,3,9)
      IF(T.GT.1) AVG2=(AVG1/FLOAT(T-1))*100.0
      IF(ENUNREL(T,4,1).GT.0) REM=ENUNREL(T,4,2)/ENUNREL(T,4,1)
      IF(ENUNREL(T,4,9).GT.0) AVG3=ENUNREL(T,4,10)/ENUNREL(T,4,9)
      IF(T.GT.1) AVG4=(AVG3/FLOAT(T-1))*100.0      
      IF(ENUNREL(T,5,1).GT.0) TEMP=ENUNREL(T,5,2)/ENUNREL(T,5,1)    
      IF((ENUNREL(T,3,9)+ENUNREL(T,4,9)+ENUNREL(T,5,9)).GT.0.01) 
     *    WRITE(96,9402) (T-1),ENUNREL(T,3,1),ENUNREL(T,3,2),DIFF,
     *                     ENUNREL(T,3,3),ENUNREL(T,3,4),
     *                     ENUNREL(T,3,5),ENUNREL(T,3,6),
     *                     ENUNREL(T,3,7),ENUNREL(T,3,8),
     *                     ENUNREL(T,3,9),ENUNREL(T,3,10),AVG1,AVG2,
     *                    ENUNREL(T,4,1),ENUNREL(T,4,2),REM,
     *                     ENUNREL(T,4,3),ENUNREL(T,4,4),
     *                     ENUNREL(T,4,5),ENUNREL(T,4,6),
     *                     ENUNREL(T,4,7),ENUNREL(T,4,8),
     *                     ENUNREL(T,4,9),ENUNREL(T,4,10),AVG3,AVG4,
     *                    ENUNREL(T,5,1),ENUNREL(T,5,2),TEMP,
     *                     ENUNREL(T,5,3),ENUNREL(T,5,4),
     *                     ENUNREL(T,5,5),ENUNREL(T,5,6),
     *                     ENUNREL(T,5,7),ENUNREL(T,5,8)
      END DO
      WRITE(96,9447) TOTTRIP
C.....ZONE TO STATION - COMMUTER RAIL AND URBAN RAIL
      WRITE(96,9411)
 9411 FORMAT('Zone,To,Station'/
     *       'Commuter_Rail,,,,,,,,,,,,,,Urban_Rail'/
     *       'Wait_Time,Interchanges,Tot_Minutes,Average,Local,',
     *       'Rapid,Express,Transitway,BRT,UrbanRail,',
     *       'Trips,Tot_WMinutes,WAverage,Percent,',
     *       'Interchanges,Tot_Minutes,Average,Local,Rapid,Express,',
     *       'Transitway,BRT,UrbanRail,',
     *       'Trips,Tot_WMinutes,WAverage,Percent')
      TOTTRIP=0.0
      DO T=1,MAX_WAIT
      DIFF=0.0
      REM=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+ANUNREL(T,1,9)
      TOTTRIP(2)=TOTTRIP(2)+ANUNREL(T,2,9)
      IF(ANUNREL(T,1,1).GT.0) DIFF=ANUNREL(T,1,2)/ANUNREL(T,1,1)
      IF(ANUNREL(T,1,9).GT.0) AVG1=ANUNREL(T,1,10)/ANUNREL(T,1,9)
      IF(T.GT.1) AVG2=(AVG1/FLOAT(T-1))*100.0
      IF(ANUNREL(T,2,1).GT.0) REM=ANUNREL(T,2,2)/ANUNREL(T,2,1) 
      IF(ANUNREL(T,2,9).GT.0) AVG3=ANUNREL(T,2,10)/ANUNREL(T,2,9)
      IF(T.GT.1) AVG4=(AVG3/FLOAT(T-1))*100.0     
      IF((ANUNREL(T,1,9)+ANUNREL(T,2,9)).GT.0.01) 
     *    WRITE(96,9402) (T-1),ANUNREL(T,1,1),ANUNREL(T,1,2),DIFF,
     *                     ANUNREL(T,1,3),ANUNREL(T,1,4),
     *                     ANUNREL(T,1,5),ANUNREL(T,1,6),
     *                     ANUNREL(T,1,7),ANUNREL(T,1,8),
     *                     ANUNREL(T,1,9),ANUNREL(T,1,10),AVG1,AVG2,
     *                     ANUNREL(T,2,1),ANUNREL(T,2,2),REM,
     *                     ANUNREL(T,2,3),ANUNREL(T,2,4),
     *                     ANUNREL(T,2,5),ANUNREL(T,2,6),
     *                     ANUNREL(T,2,7),ANUNREL(T,2,8),
     *                     ANUNREL(T,2,9),ANUNREL(T,2,10),AVG3,AVG4     
      END DO
      WRITE(96,9446) TOTTRIP(1),TOTTRIP(2)
C.....LOCAL BUS
      WRITE(96,9413)
 9413 FORMAT('Local_Bus',/,'Wait_Time,',
     *       'Interchanges,Tot_Minutes,Average,',
     *       'Wlk_Trips,Tot_WMinutes,WAverage,',
     *       'Percent,Drv_Trips,Tot_WMinutes,WAverage,Percent')
      TOTTRIP=0.0
      DO T=1,MAX_WAIT
      DIFF=0.0
      REM=0.0
      TEMP=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+LBNUNREL(T,3)
      TOTTRIP(2)=TOTTRIP(2)+LBNUNREL(T,5)
      IF(LBNUNREL(T,1).GT.0) DIFF=LBNUNREL(T,2)/LBNUNREL(T,1)
      IF(LBNUNREL(T,3).GT.0) REM=LBNUNREL(T,4)/LBNUNREL(T,3)
      IF(LBNUNREL(T,5).GT.0) TEMP=LBNUNREL(T,6)/LBNUNREL(T,5)
      IF(T.GT.1) AVG1=(REM/FLOAT(T-1))*100.0
      IF(T.GT.1) AVG2=(TEMP/FLOAT(T-1))*100.0      
      IF((LBNUNREL(T,3)+LBNUNREL(T,5)).GT.0.01) 
     *    WRITE(96,9402) (T-1),LBNUNREL(T,1),LBNUNREL(T,2),DIFF,
     *                   LBNUNREL(T,3),LBNUNREL(T,4),REM,AVG1,
     *                   LBNUNREL(T,5),LBNUNREL(T,6),TEMP,AVG2
      END DO
      WRITE(96,9448) TOTTRIP(1),TOTTRIP(2)
C.....RAPID BUS
      WRITE(96,9414)
 9414 FORMAT('Rapid_Bus',/,'Wait_Time',
     *       'Interchanges,Tot_Minutes,Average,Local,Rapid,',
     *       'Wlk_Trips,Tot_WMinutes,WAverage,',
     *       'Percent,Drv_Trips,Tot_WMinutes,WAverage,Percent')
      TOTTRIP=0.0
      DO T=1,MAX_WAIT
      DIFF=0.0
      REM=0.0
      TEMP=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+RBNUNREL(T,5)
      TOTTRIP(2)=TOTTRIP(2)+RBNUNREL(T,7)
      IF(RBNUNREL(T,1).GT.0) DIFF=RBNUNREL(T,2)/RBNUNREL(T,1)
      IF(RBNUNREL(T,5).GT.0) REM=RBNUNREL(T,6)/RBNUNREL(T,5)
      IF(RBNUNREL(T,7).GT.0) TEMP=RBNUNREL(T,8)/RBNUNREL(T,7)
      IF(T.GT.1) AVG1=(REM/FLOAT(T-1))*100.0
      IF(T.GT.1) AVG2=(TEMP/FLOAT(T-1))*100.0         
      IF((RBNUNREL(T,5)+RBNUNREL(T,7)).GT.0.01) 
     *    WRITE(96,9402) (T-1),RBNUNREL(T,1),RBNUNREL(T,2),DIFF,
     *    RBNUNREL(T,3),RBNUNREL(T,4),
     *                   RBNUNREL(T,5),RBNUNREL(T,6),REM,AVG1,
     *                   RBNUNREL(T,7),RBNUNREL(T,8),TEMP,AVG2
      END DO
      WRITE(96,9449) TOTTRIP(1),TOTTRIP(2)
C.....WALK TO EXPRESS BUS
      WRITE(96,9416)
 9416 FORMAT('Walk_Express_Bus',/,'Wait_Time',
     *       ',Interchanges,Tot_Minutes,',
     *       'Average,',
     *       'Local,Rapid,Express,Trips,Tot_WMinutes,WAverage,Percent')
      TOTTRIP=0.0
      DO T=1,MAX_WAIT     
      DIFF=0.0
      REM=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+EBnUNREL(T,6)
      IF(EBNUNREL(T,1).GT.0) DIFF=EBNUNREL(T,2)/EBNUNREL(T,1)
      IF(EBNUNREL(T,6).GT.0) REM=EBNUNREL(T,7)/EBNUNREL(T,6) 
      IF(T.GT.1) AVG2=(REM/FLOAT(T-1))*100.0       
      IF(EBNUNREL(T,6).GT.0.01) 
     *    WRITE(96,9402) (T-1),EBNUNREL(T,1),EBNUNREL(T,2),DIFF,
     *    EBNUNREL(T,3),EBNUNREL(T,4),EBNUNREL(T,5),
     *    EBNUNREL(T,6),EBNUNREL(T,7),REM,AVG2
      END DO
      WRITE(96,9450) TOTTRIP(1)
C.....WALK TO TRANSITWAY
      WRITE(96,9417)
 9417 FORMAT('Walk_Transitway',/,'Wait_Time,',
     *       'Interchanges,Tot_Minutes,Average,',
     *       'Local,Rapid,Express,Transitway,Trips,Tot_WMinutes,',
     *       'WAverage,Percent')
      TOTTRIP=0.0
      DO T=1,MAX_WAIT
      DIFF=0.0
      REM=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+TWNUNREL(T,7)
      IF(TWNUNREL(T,1).GT.0) DIFF=TWNUNREL(T,2)/TWNUNREL(T,1)
      IF(TWNUNREL(T,7).GT.0)  REM=TWNUNREL(T,8)/TWNUNREL(T,7) 
      IF(T.GT.1) AVG2=(REM/FLOAT(T-1))*100.0      
      IF(TWNUNREL(T,7).GT.0.01) 
     *    WRITE(96,9402) (T-1),TWNUNREL(T,1),TWNUNREL(T,2),DIFF,
     *    TWNUNREL(T,3),TWNUNREL(T,4),TWNUNREL(T,5),
     *    TWNUNREL(T,6),TWNUNREL(T,7),TWNUNREL(T,8),REM,AVG2
      END DO
      WRITE(96,9451) TOTTRIP(1)
      END IF
C --------------------------------------------------------------------
C   OUTPUT LINK CROWDING SUMMARIES
C --------------------------------------------------------------------
      IF(LCROWD) THEN
      IF(MPRICE.GT.0) THEN
      WRITE(26,9410) MPRICE,MAX_TLF
      END IF
C.....STATION TO STATION
      WRITE(97,9401)
      DO T=1,MAX_TLF
      IF(SLCROWD(T,1,1).GT.0) DIFF=SLCROWD(T,1,2)/SLCROWD(T,1,1)
      IF(SLCROWD(T,1,3).GT.0) AVG1=SLCROWD(T,1,4)/SLCROWD(T,1,3)
      IF(T.GT.1) AVG2=(AVG1/FLOAT(T-1))*100.0
      IF(SLCROWD(T,2,1).GT.0) REM=SLCROWD(T,2,2)/SLCROWD(T,2,1)
      IF(SLCROWD(T,2,3).GT.0) AVG3=SLCROWD(T,2,4)/SLCROWD(T,2,3)
      IF(T.GT.1) AVG4=(AVG3/FLOAT(T-1))*100.0           
      IF((SLCROWD(T,1,3)+SLCROWD(T,2,3)).GT.0.01) 
     *    WRITE(97,9402) (T-1),SLCROWD(T,1,1),SLCROWD(T,1,2),DIFF,
     *                    SLCROWD(T,1,3),SLCROWD(T,1,4),AVG1,AVG2,
     *                    SLCROWD(T,2,1),SLCROWD(T,2,2),REM,
     *                    SLCROWD(T,2,3),SLCROWD(T,2,4),AVG3,AVG4
      END DO
C.....STATION TO ZONE - COMMUTER RAIL AND URBAN RAIL
      WRITE(97,9408)
      DO T=1,MAX_TLF
      DIFF=0.0
      REM=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      IF(ELCROWD(T,1,1).GT.0) DIFF=ELCROWD(T,1,2)/ELCROWD(T,1,1)
      IF(ELCROWD(T,1,9).GT.0) AVG1=ELCROWD(T,1,10)/ELCROWD(T,1,9)
      IF(T.GT.1) AVG2=(AVG1/FLOAT(T-1))*100.0      
      IF(ELCROWD(T,2,1).GT.0) REM=ELCROWD(T,2,2)/ELCROWD(T,2,1)   
      IF(ELCROWD(T,2,9).GT.0) AVG3=ELCROWD(T,2,10)/ELCROWD(T,2,9)
      IF(T.GT.1) AVG4=(AVG3/FLOAT(T-1))*100.0       
      IF((ELCROWD(T,1,9)+ELCROWD(T,2,9)).GT.0.01) 
     *    WRITE(97,9402) (T-1),ELCROWD(T,1,1),ELCROWD(T,1,2),DIFF,
     *                     ELCROWD(T,1,3),ELCROWD(T,1,4),
     *                     ELCROWD(T,1,5),ELCROWD(T,1,6),
     *                     ELCROWD(T,1,7),ELCROWD(T,1,8),
     *                     ELCROWD(T,1,9),ELCROWD(T,1,10),AVG1,AVG2,
     *                     ELCROWD(T,2,1),ELCROWD(T,2,2),REM,
     *                     ELCROWD(T,2,3),ELCROWD(T,2,4),
     *                     ELCROWD(T,2,5),ELCROWD(T,2,6),
     *                     ELCROWD(T,2,7),ELCROWD(T,2,8),
     *                     ELCROWD(T,2,9),ELCROWD(T,2,10),AVG3,AVG4
      END DO
C.....STATION TO ZONE - EXPRESS BUS, TRANSITWAY AND BRT
      WRITE(97,9405)
      DO T=1,MAX_TLF
      DIFF=0.0
      REM=0.0
      TEMP=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      IF(ELCROWD(T,3,1).GT.0) DIFF=ELCROWD(T,3,2)/ELCROWD(T,3,1)
      IF(ELCROWD(T,3,9).GT.0) AVG1=ELCROWD(T,3,10)/ELCROWD(T,3,9)
      IF(T.GT.1) AVG2=(AVG1/FLOAT(T-1))*100.0
      IF(ELCROWD(T,4,1).GT.0) REM=ELCROWD(T,4,2)/ELCROWD(T,4,1)
      IF(ELCROWD(T,4,9).GT.0) AVG3=ELCROWD(T,4,10)/ELCROWD(T,4,9)
      IF(T.GT.1) AVG4=(AVG3/FLOAT(T-1))*100.0      
      IF(ELCROWD(T,5,1).GT.0) TEMP=ELCROWD(T,5,2)/ELCROWD(T,5,1)    
      IF((ELCROWD(T,3,9)+ELCROWD(T,4,9)+ELCROWD(T,5,9)).GT.0.01) 
     *    WRITE(97,9402) (T-1),ELCROWD(T,3,1),ELCROWD(T,3,2),DIFF,
     *                     ELCROWD(T,3,3),ELCROWD(T,3,4),
     *                     ELCROWD(T,3,5),ELCROWD(T,3,6),
     *                     ELCROWD(T,3,7),ELCROWD(T,3,8),
     *                     ELCROWD(T,3,9),ELCROWD(T,3,10),AVG1,AVG2,
     *                    ELCROWD(T,4,1),ELCROWD(T,4,2),REM,
     *                     ELCROWD(T,4,3),ELCROWD(T,4,4),
     *                     ELCROWD(T,4,5),ELCROWD(T,4,6),
     *                     ELCROWD(T,4,7),ELCROWD(T,4,8),
     *                     ELCROWD(T,4,9),ELCROWD(T,4,10),AVG3,AVG4,
     *                    ELCROWD(T,5,1),ELCROWD(T,5,2),TEMP,
     *                     ELCROWD(T,5,3),ELCROWD(T,5,4),
     *                     ELCROWD(T,5,5),ELCROWD(T,5,6),
     *                     ELCROWD(T,5,7),ELCROWD(T,5,8)
      END DO
C.....ZONE TO STATION - COMMUTER RAIL AND URBAN RAIL
      WRITE(97,9409)
      DO T=1,MAX_TLF
      DIFF=0.0
      REM=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      IF(ALCROWD(T,1,1).GT.0) DIFF=ALCROWD(T,1,2)/ALCROWD(T,1,1)
      IF(ALCROWD(T,1,9).GT.0) AVG1=ALCROWD(T,1,10)/ALCROWD(T,1,9)
      IF(T.GT.1) AVG2=(AVG1/FLOAT(T-1))*100.0
      IF(ALCROWD(T,2,1).GT.0) REM=ALCROWD(T,2,2)/ALCROWD(T,2,1) 
      IF(ALCROWD(T,2,9).GT.0) AVG3=ALCROWD(T,2,10)/ALCROWD(T,2,9)
      IF(T.GT.1) AVG4=(AVG3/FLOAT(T-1))*100.0     
      IF((ALCROWD(T,1,9)+ALCROWD(T,2,9)).GT.0.01) 
     *    WRITE(97,9402) (T-1),ALCROWD(T,1,1),ALCROWD(T,1,2),DIFF,
     *                     ALCROWD(T,1,3),ALCROWD(T,1,4),
     *                     ALCROWD(T,1,5),ALCROWD(T,1,6),
     *                     ALCROWD(T,1,7),ALCROWD(T,1,8),
     *                     ALCROWD(T,1,9),ALCROWD(T,1,10),AVG1,AVG2,
     *                     ALCROWD(T,2,1),ALCROWD(T,2,2),REM,
     *                     ALCROWD(T,2,3),ALCROWD(T,2,4),
     *                     ALCROWD(T,2,5),ALCROWD(T,2,6),
     *                     ALCROWD(T,2,7),ALCROWD(T,2,8),
     *                     ALCROWD(T,2,9),ALCROWD(T,2,10),AVG3,AVG4     
      END DO
C.....LOCAL BUS
      WRITE(97,9403)
      DO T=1,MAX_TLF
      DIFF=0.0
      REM=0.0
      TEMP=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      IF(LBLCROWD(T,1).GT.0) DIFF=LBLCROWD(T,2)/LBLCROWD(T,1)
      IF(LBLCROWD(T,3).GT.0) REM=LBLCROWD(T,4)/LBLCROWD(T,3)
      IF(LBLCROWD(T,5).GT.0) TEMP=LBLCROWD(T,6)/LBLCROWD(T,5)
      IF(T.GT.1) AVG1=(REM/FLOAT(T-1))*100.0
      IF(T.GT.1) AVG2=(TEMP/FLOAT(T-1))*100.0      
      IF((LBLCROWD(T,3)+LBLCROWD(T,5)).GT.0.01) 
     *    WRITE(97,9402) (T-1),LBLCROWD(T,1),LBLCROWD(T,2),DIFF,
     *                   LBLCROWD(T,3),LBLCROWD(T,4),REM,AVG1,
     *                   LBLCROWD(T,5),LBLCROWD(T,6),TEMP,AVG2
      END DO
C.....RAPID BUS
      WRITE(97,9404)
      DO T=1,MAX_TLF
      DIFF=0.0
      REM=0.0
      TEMP=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      IF(RBLCROWD(T,1).GT.0) DIFF=RBLCROWD(T,2)/RBLCROWD(T,1)
      IF(RBLCROWD(T,5).GT.0) REM=RBLCROWD(T,6)/RBLCROWD(T,5)
      IF(RBLCROWD(T,7).GT.0) TEMP=RBLCROWD(T,8)/RBLCROWD(T,7)
      IF(T.GT.1) AVG1=(REM/FLOAT(T-1))*100.0
      IF(T.GT.1) AVG2=(TEMP/FLOAT(T-1))*100.0         
      IF((RBLCROWD(T,5)+RBLCROWD(T,7)).GT.0.01) 
     *    WRITE(97,9402) (T-1),RBLCROWD(T,1),RBLCROWD(T,2),DIFF,
     *    RBLCROWD(T,3),RBLCROWD(T,4),
     *                   RBLCROWD(T,5),RBLCROWD(T,6),REM,AVG1,
     *                   RBLCROWD(T,7),RBLCROWD(T,8),TEMP,AVG2
      END DO
C.....WALK TO EXPRESS BUS
      WRITE(97,9406)
      TOTTRIP=0.0
      DO T=1,MAX_TLF
      DIFF=0.0
      REM=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+EBLCROWD(T,6)
      IF(EBLCROWD(T,1).GT.0) DIFF=EBLCROWD(T,2)/EBLCROWD(T,1)
      IF(EBLCROWD(T,6).GT.0) REM=EBLCROWD(T,7)/EBLCROWD(T,6) 
      IF(T.GT.1) AVG2=(REM/FLOAT(T-1))*100.0       
      IF(EBLCROWD(T,6).GT.0.01) 
     *    WRITE(97,9402) (T-1),EBLCROWD(T,1),EBLCROWD(T,2),DIFF,
     *    EBLCROWD(T,3),EBLCROWD(T,4),EBLCROWD(T,5),
     *    EBLCROWD(T,6),EBLCROWD(T,7),REM,AVG2
      END DO
      WRITE(97,9450) TOTTRIP(1)
C.....WALK TO TRANSITWAY
      WRITE(97,9407)
      TOTTRIP=0.0
      DO T=1,MAX_TLF
      DIFF=0.0
      REM=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+TWLCROWD(T,7)
      IF(TWLCROWD(T,1).GT.0) DIFF=TWLCROWD(T,2)/TWLCROWD(T,1)
      IF(TWLCROWD(T,7).GT.0)  REM=TWLCROWD(T,8)/TWLCROWD(T,7) 
      IF(T.GT.1) AVG2=(REM/FLOAT(T-1))*100.0      
      IF(TWLCROWD(T,7).GT.0.01) 
     *    WRITE(97,9402) (T-1),TWLCROWD(T,1),TWLCROWD(T,2),DIFF,
     *    TWLCROWD(T,3),TWLCROWD(T,4),TWLCROWD(T,5),
     *    TWLCROWD(T,6),TWLCROWD(T,7),TWLCROWD(T,8),REM,AVG2
      END DO
      WRITE(97,9451) TOTTRIP(1)      
      END IF
C -----------------------------------------------------------------
C   OUTPUT STOP CAPACITY SUMMARIES
C -----------------------------------------------------------------
      IF(NCAPAC) THEN
      IF(QPRICE.GT.0) THEN
      WRITE(26,9510) QPRICE,MAX_WAIT
      END IF
C.....STATION TO STATION
      WRITE(98,9501)
      DO T=1,MAX_WAIT
      DIFF=0.0
      AVG1=0.0
      AVG2=0.0
      REM=0.0
      AVG3=0.0
      AVG4=0.0
      IF(SNCAPAC(T,1,1).GT.0) DIFF=SNCAPAC(T,1,2)/SNCAPAC(T,1,1)
      IF(SNCAPAC(T,1,3).GT.0) AVG1=SNCAPAC(T,1,4)/SNCAPAC(T,1,3)
      IF(T.GT.1) AVG2=(AVG1/FLOAT(T-1))*100.0
      IF(SNCAPAC(T,2,1).GT.0) REM=SNCAPAC(T,2,2)/SNCAPAC(T,2,1)
      IF(SNCAPAC(T,2,3).GT.0) AVG3=SNCAPAC(T,2,4)/SNCAPAC(T,2,3)
      IF(T.GT.1) AVG4=(AVG3/FLOAT(T-1))*100.0           
      IF((SNCAPAC(T,1,3)+SNCAPAC(T,2,3)).GT.0.01) 
     *    WRITE(98,9402) (T-1),SNCAPAC(T,1,1),SNCAPAC(T,1,2),DIFF,
     *                    SNCAPAC(T,1,3),SNCAPAC(T,1,4),AVG1,AVG2,
     *                    SNCAPAC(T,2,1),SNCAPAC(T,2,2),REM,
     *                    SNCAPAC(T,2,3),SNCAPAC(T,2,4),AVG3,AVG4
      END DO
C.....STATION TO ZONE - COMMUTER RAIL AND URBAN RAIL
      WRITE(98,9508)
      DO T=1,MAX_WAIT
      DIFF=0.0
      REM=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      IF(ENCAPAC(T,1,1).GT.0) DIFF=ENCAPAC(T,1,2)/ENCAPAC(T,1,1)
      IF(ENCAPAC(T,1,9).GT.0) AVG1=ENCAPAC(T,1,10)/ENCAPAC(T,1,9)
      IF(T.GT.1) AVG2=(AVG1/FLOAT(T-1))*100.0      
      IF(ENCAPAC(T,2,1).GT.0) REM=ENCAPAC(T,2,2)/ENCAPAC(T,2,1)   
      IF(ENCAPAC(T,2,9).GT.0) AVG3=ENCAPAC(T,2,10)/ENCAPAC(T,2,9)
      IF(T.GT.1) AVG4=(AVG3/FLOAT(T-1))*100.0       
      IF((ENCAPAC(T,1,9)+ENCAPAC(T,2,9)).GT.0.01) 
     *    WRITE(98,9402) (T-1),ENCAPAC(T,1,1),ENCAPAC(T,1,2),DIFF,
     *                     ENCAPAC(T,1,3),ENCAPAC(T,1,4),
     *                     ENCAPAC(T,1,5),ENCAPAC(T,1,6),
     *                     ENCAPAC(T,1,7),ENCAPAC(T,1,8),
     *                     ENCAPAC(T,1,9),ENCAPAC(T,1,10),AVG1,AVG2,
     *                     ENCAPAC(T,2,1),ENCAPAC(T,2,2),REM,
     *                     ENCAPAC(T,2,3),ENCAPAC(T,2,4),
     *                     ENCAPAC(T,2,5),ENCAPAC(T,2,6),
     *                     ENCAPAC(T,2,7),ENCAPAC(T,2,8),
     *                     ENCAPAC(T,2,9),ENCAPAC(T,2,10),AVG3,AVG4
      END DO
C.....STATION TO ZONE - EXPRESS BUS, TRANSITWAY AND BRT
      WRITE(98,9505)
      DO T=1,MAX_WAIT
      DIFF=0.0
      REM=0.0
      TEMP=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      IF(ENCAPAC(T,3,1).GT.0) DIFF=ENCAPAC(T,3,2)/ENCAPAC(T,3,1)
      IF(ENCAPAC(T,3,9).GT.0) AVG1=ENCAPAC(T,3,10)/ENCAPAC(T,3,9)
      IF(T.GT.1) AVG2=(AVG1/FLOAT(T-1))*100.0
      IF(ENCAPAC(T,4,1).GT.0) REM=ENCAPAC(T,4,2)/ENCAPAC(T,4,1)
      IF(ENCAPAC(T,4,9).GT.0) AVG3=ENCAPAC(T,4,10)/ENCAPAC(T,4,9)
      IF(T.GT.1) AVG4=(AVG3/FLOAT(T-1))*100.0      
      IF(ENCAPAC(T,5,1).GT.0) TEMP=ENCAPAC(T,5,2)/ENCAPAC(T,5,1)    
      IF((ENCAPAC(T,3,9)+ENCAPAC(T,4,9)+ENCAPAC(T,5,9)).GT.0.01) 
     *    WRITE(98,9402) (T-1),ENCAPAC(T,3,1),ENCAPAC(T,3,2),DIFF,
     *                     ENCAPAC(T,3,3),ENCAPAC(T,3,4),
     *                     ENCAPAC(T,3,5),ENCAPAC(T,3,6),
     *                     ENCAPAC(T,3,7),ENCAPAC(T,3,8),
     *                     ENCAPAC(T,3,9),ENCAPAC(T,3,10),AVG1,AVG2,
     *                    ENCAPAC(T,4,1),ENCAPAC(T,4,2),REM,
     *                     ENCAPAC(T,4,3),ENCAPAC(T,4,4),
     *                     ENCAPAC(T,4,5),ENCAPAC(T,4,6),
     *                     ENCAPAC(T,4,7),ENCAPAC(T,4,8),
     *                     ENCAPAC(T,4,9),ENCAPAC(T,4,10),AVG3,AVG4,
     *                    ENCAPAC(T,5,1),ENCAPAC(T,5,2),TEMP,
     *                     ENCAPAC(T,5,3),ENCAPAC(T,5,4),
     *                     ENCAPAC(T,5,5),ENCAPAC(T,5,6),
     *                     ENCAPAC(T,5,7),ENCAPAC(T,5,8)
      END DO
C.....ZONE TO STATION - COMMUTER RAIL AND URBAN RAIL
      WRITE(98,9411)
      DO T=1,MAX_WAIT
      DIFF=0.0
      REM=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      IF(ANCAPAC(T,1,1).GT.0) DIFF=ANCAPAC(T,1,2)/ANCAPAC(T,1,1)
      IF(ANCAPAC(T,1,9).GT.0) AVG1=ANCAPAC(T,1,10)/ANCAPAC(T,1,9)
      IF(T.GT.1) AVG2=(AVG1/FLOAT(T-1))*100.0
      IF(ANCAPAC(T,2,1).GT.0) REM=ANCAPAC(T,2,2)/ANCAPAC(T,2,1) 
      IF(ANCAPAC(T,2,9).GT.0) AVG3=ANCAPAC(T,2,10)/ANCAPAC(T,2,9)
      IF(T.GT.1) AVG4=(AVG3/FLOAT(T-1))*100.0     
      IF((ANCAPAC(T,1,9)+ANCAPAC(T,2,9)).GT.0.01) 
     *    WRITE(98,9402) (T-1),ANCAPAC(T,1,1),ANCAPAC(T,1,2),DIFF,
     *                     ANCAPAC(T,1,3),ANCAPAC(T,1,4),
     *                     ANCAPAC(T,1,5),ANCAPAC(T,1,6),
     *                     ANCAPAC(T,1,7),ANCAPAC(T,1,8),
     *                     ANCAPAC(T,1,9),ANCAPAC(T,1,10),AVG1,AVG2,
     *                     ANCAPAC(T,2,1),ANCAPAC(T,2,2),REM,
     *                     ANCAPAC(T,2,3),ANCAPAC(T,2,4),
     *                     ANCAPAC(T,2,5),ANCAPAC(T,2,6),
     *                     ANCAPAC(T,2,7),ANCAPAC(T,2,8),
     *                     ANCAPAC(T,2,9),ANCAPAC(T,2,10),AVG3,AVG4     
      END DO
C.....LOCAL BUS
      WRITE(98,9403)
      DO T=1,MAX_WAIT
      DIFF=0.0
      REM=0.0
      TEMP=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      IF(LBNCAPAC(T,1).GT.0) DIFF=LBNCAPAC(T,2)/LBNCAPAC(T,1)
      IF(LBNCAPAC(T,3).GT.0) REM=LBNCAPAC(T,4)/LBNCAPAC(T,3)
      IF(LBNCAPAC(T,5).GT.0) TEMP=LBNCAPAC(T,6)/LBNCAPAC(T,5)
      IF(T.GT.1) AVG1=(REM/FLOAT(T-1))*100.0
      IF(T.GT.1) AVG2=(TEMP/FLOAT(T-1))*100.0      
      IF((LBNCAPAC(T,3)+LBNCAPAC(T,5)).GT.0.01) 
     *    WRITE(98,9402) (T-1),LBNCAPAC(T,1),LBNCAPAC(T,2),DIFF,
     *                   LBNCAPAC(T,3),LBNCAPAC(T,4),REM,AVG1,
     *                   LBNCAPAC(T,5),LBNCAPAC(T,6),TEMP,AVG2
      END DO
C.....RAPID BUS
      WRITE(98,9404)
      DO T=1,MAX_WAIT
      DIFF=0.0
      REM=0.0
      TEMP=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      IF(RBNCAPAC(T,1).GT.0) DIFF=RBNCAPAC(T,2)/RBNCAPAC(T,1)
      IF(RBNCAPAC(T,5).GT.0) REM=RBNCAPAC(T,6)/RBNCAPAC(T,5)
      IF(RBNCAPAC(T,7).GT.0) TEMP=RBNCAPAC(T,8)/RBNCAPAC(T,7)
      IF(T.GT.1) AVG1=(REM/FLOAT(T-1))*100.0
      IF(T.GT.1) AVG2=(TEMP/FLOAT(T-1))*100.0         
      IF((RBNCAPAC(T,5)+RBNCAPAC(T,7)).GT.0.01) 
     *    WRITE(98,9402) (T-1),RBNCAPAC(T,1),RBNCAPAC(T,2),DIFF,
     *    RBNCAPAC(T,3),RBNCAPAC(T,4),
     *                   RBNCAPAC(T,5),RBNCAPAC(T,6),REM,AVG1,
     *                   RBNCAPAC(T,7),RBNCAPAC(T,8),TEMP,AVG2
      END DO
C.....WALK TO EXPRESS BUS
      WRITE(98,9416)
      TOTTRIP=0.0
      DO T=1,MAX_WAIT     
      DIFF=0.0
      REM=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+EBNUNREL(T,6)
      IF(EBNCAPAC(T,1).GT.0) DIFF=EBNCAPAC(T,2)/EBNCAPAC(T,1)
      IF(EBNCAPAC(T,6).GT.0) REM=EBNCAPAC(T,7)/EBNCAPAC(T,6) 
      IF(T.GT.1) AVG2=(REM/FLOAT(T-1))*100.0       
      IF(EBNCAPAC(T,6).GT.0.01) 
     *    WRITE(98,9402) (T-1),EBNCAPAC(T,1),EBNCAPAC(T,2),DIFF,
     *    EBNCAPAC(T,3),EBNCAPAC(T,4),EBNCAPAC(T,5),
     *    EBNCAPAC(T,6),EBNCAPAC(T,7),REM,AVG2
      END DO
      WRITE(98,9450) TOTTRIP(1)
C.....WALK TO TRANSITWAY
      WRITE(98,9417)
      TOTTRIP=0.0
      DO T=1,MAX_WAIT
      DIFF=0.0
      REM=0.0
      AVG1=0.0
      AVG2=0.0
      AVG3=0.0
      AVG4=0.0
      TOTTRIP(1)=TOTTRIP(1)+TWNUNREL(T,7)
      IF(TWNCAPAC(T,1).GT.0) DIFF=TWNCAPAC(T,2)/TWNCAPAC(T,1)
      IF(TWNCAPAC(T,7).GT.0)  REM=TWNCAPAC(T,8)/TWNCAPAC(T,7) 
      IF(T.GT.1) AVG2=(REM/FLOAT(T-1))*100.0      
      IF(TWNCAPAC(T,7).GT.0.01) 
     *    WRITE(98,9402) (T-1),TWNCAPAC(T,1),TWNCAPAC(T,2),DIFF,
     *    TWNCAPAC(T,3),TWNCAPAC(T,4),TWNCAPAC(T,5),
     *    TWNCAPAC(T,6),TWNCAPAC(T,7),TWNCAPAC(T,8),REM,AVG2
      END DO
      WRITE(98,9451) TOTTRIP(1)
      END IF
      RETURN
      END
