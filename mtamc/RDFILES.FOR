C**********************************************************
C   SUBROUTINE RDFILES: OBTAIN DATA SET NAMES FOR INPUT   *
C                       FILES                             *
C**********************************************************
      SUBROUTINE RDFILES(CTLERR)
      INCLUDE 'param.com'
      LOGICAL      CTLERR
      INTEGER*4    IT,ST,TT,UT,VT,XT,LEN(88),RLEN
      CHARACTER*1  COMMA,BLANK
      CHARACTER*10  FILES(88)
      CHARACTER*90 NAMES(88)
      CHARACTER*90 STRING
      DATA         COMMA/','/,BLANK/' '/
      DATA         FILES/'XYCOORD=','ZFSTD=','STATBIKE=','WFNAME=',
     *                   'PKUTIN=','PKUTOUT=','BRTOUT=','SHADOUT=',
     *                   'HWYOUT=','CROUT=','UROUT=','BUSOUT=',
     *                   'NBUSOUT=','BACCEGR=','BRTURTCR=','LOGSUMS=',
     *                   'FUSERBN=','STATDATA=','PERTT=','BCRSK=',
     *                   'BURSK=','WBSSK=','WRBSK=','WEBSK=',
     *                   'WTWSK=','DASK=','P2SK=','P3SK=','FRPORT=',
     *                   'ERRRPT=','FACCEGR=','FDISTEQ=','BRTSK=',
     *                   'STNACC=','CTVINP=','XFERBLEND=','P4SK=',
     *                   'XFERURT=','XFERCMR=','RALIND=','XFERBRT=',
     *                   '       ','TLOGSUMS=','NHBSTA=','FACEG=',
     *                   'LAXPARK=','FLYAWAY=','RENTAL=','AIRCALIB=',
     *                   'FLOTTRN=','FLUNREL=','FNUNREL=','FCAPAC=',
     *                   'FCROWD=','FVENUE=','FSPEVENT=',
     *                   'BLENDOUT=','TXFERSUM=','FLOWRAIL=',
     *                   'FLOWINC=','FRNTTRN=','FITFTRN=','FAPMIND=',
     *                   'FNHBPKHBW=','FNHBPKHBO=','FNHBPKHBU=',
     *                   'FNHBOPHBW=','FNHBOPHBO=','FNHBOPHBU=',
     *                   'FWLKLSM=','FBYCLSM=','BBSSK=',
     *                   'BRBSK=','BEBSK=','BTWSK=','BBRTSK=',
     *                   'ZFBIKE=','INTRADIST=','FLUNRELBK=',
     *                   'FNUNRELBK=','FCAPACBK=','FCROWDBK=',
     *                   'FNHBPKHBS=','FNHBOPHBS=','FLYTRIP=',
     *                   'FAIRPROB=','FLOWDIST=','FSTSFARE='/
      DATA         NAMES/88*' '/
      DATA         LEN/7,5,8,6,6,7,6,7,6,5,5,6,
     *                 7,7,8,7,7,8,5,5,5,5,5,5,
     *                 5,4,4,4,6,6,7,7,5,6,6,9,
     *                 4,7,7,6,7,6,8,6,5,7,7,6,
     *                 8,7,7,7,6,6,6,8,8,8,8,7,
     *                 7,7,7,9,9,9,9,9,9,7,7,5,
     *                 5,5,5,6,6,9,9,9,8,8,9,9,
     *                 7,8,8,8/
      CTLERR=.FALSE.
C
C SEARCH FOR ONE FILE NAME AT A TIME
C
      DO 100 IT=1,88
      REWIND 10
    1 READ(10,101,END=100) STRING
  101 FORMAT(A90)
      RLEN=90-LEN(IT)
      DO 200 ST=1,RLEN
      TT=ST+LEN(IT)
      IF(STRING(ST:TT).EQ.FILES(IT)) THEN
C
C NOW SEARCH FOR COMMA
C
      DO 300 UT=TT,90
      IF((STRING(UT:UT).EQ.COMMA).OR.(STRING(UT:UT).EQ.BLANK)) THEN
      VT=TT+2
      XT=UT-2
      NAMES(IT)=STRING(VT:XT)
      GO TO 100
      END IF
  300 CONTINUE
      END IF
  200 CONTINUE
      GO TO 1
  100 CONTINUE
C
C SET FILE NAMES TO INPUT STRINGS RECEIVED
C
      XYCOORD=NAMES(1)
      ZFSTD=NAMES(2)
      STATBIKE=NAMES(3)
      WFNAME=NAMES(4)
      PKUTIN=NAMES(5)
      PKUTOUT=NAMES(6)
      BRTOUT=NAMES(7)
      SHADOUT=NAMES(8)
      HWYOUT=NAMES(9)
      CROUT=NAMES(10)
      UROUT=NAMES(11)
      BUSOUT=NAMES(12)
      NBUSOUT=NAMES(13)
      BACCEGR=NAMES(14)
      BRTURTCR=NAMES(15)
      LOGSUMS=NAMES(16)
      FUSERBN=NAMES(17)
      STATDATA=NAMES(18)
      PERTT=NAMES(19)  
      BCRSK=NAMES(20)  
      BURSK=NAMES(21)  
      WBSSK=NAMES(22)  
      WRBSK=NAMES(23)  
      WEBSK=NAMES(24)  
      WTWSK=NAMES(25)  
      DASK=NAMES(26)   
      P2SK=NAMES(27)   
      P3SK=NAMES(28)   
      FRPORT=NAMES(29) 
      ERRRPT=NAMES(30) 
      FACCEGR=NAMES(31)
      FDISTEQ=NAMES(32)
      BRTSK=NAMES(33)
      STNACC=NAMES(34)
      CTVINP=NAMES(35)
      XFERBLEND=NAMES(36)
      P4SK=NAMES(37) 
      XFERURT=NAMES(38)
      XFERCMR=NAMES(39)
      RALIND=NAMES(40)
      XFERBRT=NAMES(41)
      TLOGSUMS=NAMES(43)
      NHBSTA=NAMES(44)
C     FACEG=NAMES(45)
      LAXPARK=NAMES(46)
      FLYAWAY=NAMES(47)
      RENTAL=NAMES(48)
      AIRCALIB=NAMES(49)
      FLOTTRN=NAMES(50)
      FLUNREL=NAMES(51)
      FNUNREL=NAMES(52)
      FCAPAC=NAMES(53)
      FCROWD=NAMES(54)
      FVENUE=NAMES(55)
      FSPEVENT=NAMES(56)
      BLENDOUT=NAMES(57)
      TXFERSUM=NAMES(58)
      FLOWRAIL=NAMES(59)
      FLOWINC=NAMES(60)
      FRNTTRN=NAMES(61)
      FITFTRN=NAMES(62)
      FAPMIND=NAMES(63)
      FNHBPKHBW=NAMES(64)
      FNHBPKHBO=NAMES(65)
      FNHBPKHBU=NAMES(66)
      FNHBOPHBW=NAMES(67)
      FNHBOPHBO=NAMES(68)
      FNHBOPHBU=NAMES(69)
      FWLKLSM=NAMES(70)
      FBYCLSM=NAMES(71)
      BBSSK=NAMES(72)
      BRBSK=NAMES(73)
      BEBSK=NAMES(74)
      BTWSK=NAMES(75)
      ZFBIKE=NAMES(77)
      INTRADIST=NAMES(78)
      FLUNRELBK=NAMES(79)
      FNUNRELBK=NAMES(80)
      FCAPACBK=NAMES(81)
      FCROWDBK=NAMES(82)
      FNHBPKHBS=NAMES(83)
      FNHBOPHBS=NAMES(84)
      FLYTRIP=NAMES(85)
      FAIRPROB=NAMES(86)
      FLOWDIST=NAMES(87)
      FSTSFARE=NAMES(88)
C 
C SUMMARIZE FILE NAMES
C
      OPEN(26,FILE=FRPORT,STATUS='UNKNOWN',FORM='FORMATTED')
C
C WRITE PROGRAM BANNER, DATE & TIME
C
      CALL GETDAT(IYR,IMON,IDAY)
      CALL GETTIM(IHR,IMIN,ISEC,I100)
      IYR=IYR-2000
      WRITE(26,8001) IMON,IDAY,IYR,IHR,IMIN,ISEC
 8001 FORMAT(/20X,' PROGRAM MTAMC  (28May20)',/,
     *        20X,' -------------------------',/,
     *        20X,'      [Version 11.0]',//,
     *        20X,'     Date:',I2,'/',I2,'/',I2/
     *        20X,'     Time:',I2,':',I2,':',I2//)
      WRITE(*,8001) IMON,IDAY,IYR,IHR,IMIN,ISEC
C 
C PRINT FILE INPUT/OUTPUT SUMMARY
C
      WRITE(26,500) PERTT,ZFSTD,ZFBIKE,WFNAME,PKUTIN,STATBIKE,
     *              STATDATA,XYCOORD,DASK,P2SK,P3SK,P4SK,WBSSK,
     *              WRBSK,WEBSK,WTWSK,BRTSK,BURSK,BCRSK,BACCEGR,
     *              BRTURTCR,BBSSK,BRBSK,BEBSK,BTWSK,
     *              XFERBLEND,XFERURT,XFERCMR,XFERBRT,
     *              FWLKLSM,FBYCLSM,INTRADIST,FSTSFARE,BUSOUT,
     *              NBUSOUT,UROUT,CROUT,BRTOUT,BLENDOUT,
     *              HWYOUT,LOGSUMS,FUSERBN,FACCEGR,
     *              PKUTOUT,SHADOUT,FRPORT,ERRRPT,
     *              FDISTEQ,STNACC,CTVINP,
     *              RALIND,TLOGSUMS,NHBSTA,TXFERSUM,FLOWRAIL,
     *              FLOWINC,FLOWDIST,
     *              LAXPARK,FLYAWAY,FLYTRIP,
     *              RENTAL,FLOTTRN,
     *              FRNTTRN,FITFTRN,FAPMIND,FLUNREL,
     *              FNUNREL,FCAPAC,FCROWD,FVENUE,FSPEVENT,
     *              FLUNRELBK,FNUNRELBK,FCAPACBK,FCROWDBK,
     *              FNHBPKHBW,FNHBPKHBO,FNHBPKHBU,FNHBPKHBS,
     *              FNHBOPHBW,FNHBOPHBO,FNHBOPHBU,FNHBOPHBS
  500 FORMAT(1X,'SUMMARY OF FILE INPUTS & OUTPUTS'/
     *       1X,'--------------------------------------------------'/
     *       1X,'PERTT     ',a80,' PERSON TRIP MATRIX INPUT'/
     *       1X,'ZFSTD     ',a80,' STANDARD S/E FILE INPUT'/
     *       1X,'ZFBIKE    ',a80,' BIKE ZONAL ATTRIBUTE FILE INPUT'/
     *       1X,'WFNAME    ',a80,' PERCENT WALK FILE INPUT'/
     *       1X,'PKUTIN    ',a80,' PARKING UTILIZATION FILE INPUT'/
     *       1X,'STATBIKE  ',a80,' BIKE STATION ATTRIBUTE FILE INPUT'/
     *       1X,'STATDATA  ',a80,' STATION ATTRIBUTE FILE INPUT'/
     *       1X,'XYCOORD   ',a80,' X-Y COORDINATE FILE INPUT'//
     *       1X,'DASK      ',a80,' DRIVE ALONE SKIM MATRIX'/
     *       1X,'P2SK      ',a80,' 2 PERSON AUTO SKIM MATRIX'/
     *       1X,'P3SK      ',a80,' 3  PERSON AUTO SKIM MATRIX'/
     *       1X,'P4SK      ',a80,' 4+ PERSON AUTO SKIM MATRIX'/
     *       1X,'WBSSK     ',a80,' WALK TO LOCAL BUS SKIM MATRIX'/
     *       1X,'WRBSK     ',a80,' WALK TO RAPID BUS SKIM MATRIX'/
     *       1X,'WEBSK     ',a80,' WALK TO EXPRESS BUS SKIM MATRIX'/
     *       1X,'WTWSK     ',a80,' WALK TO TRANSITWAY SKIM MATRIX'/
     *       1X,'BRTSK     ',a80,' BUS RAPID TRANSIT SKIM MATRIX'/
     *       1X,'BURSK     ',a80,' URBAN RAIL SKIM MATRIX'/
     *       1X,'BCRSK     ',a80,' COMMUTER RAIL SKIM MATRIX'/
     *       1X,'BACCEGR   ',a80,' STATION ACCESS/EGRESS SKIM MATRIX'/
     *       1X,'BRTURTCR  ',a80,' BRT/URBAN RAIL SKIM MATRIX'/
     *       1X,'BBSSK     ',a80,' BIKE TO LOCAL BUS SKIM MATRIX'/
     *       1X,'BRBSK     ',a80,' BIKE TO RAPID BUS SKIM MATRIX'/
     *       1X,'BEBSK     ',a80,' BIKE TO EXPRESS BUS SKIM MATRIX'/
     *       1X,'BTWSK     ',a80,' BIKE TO TRANSITWAY SKIM MATRIX'/
     *       1X,'XFERBLEND ',a80,' TRANSFER NODES FOR BLENDED SKIM'/
     *       1X,'XFERURT   ',a80,' TRANSFER NODES FOR URBAN RAIL'/
     *       1X,'XFERCMR   ',a80,' TRANSFER NODES FOR COMMUTER RAIL'/
     *       1X,'XFERBRT   ',a80,' TRANSFER NODES FOR BRT'/
     *       1X,'FWLKLSM   ',a80,' WALK MODE LOGSUM MATRIX'/
     *       1X,'FBYCLSM   ',a80,' BICYCLE MODE LOGSUM MATRIX'/
     *       1X,'INTRADIST ',a80,' INTRAZONAL DISTANCE FILE'/
     *       1X,'FSTSFARE  ',a80,' FIXED GUIDEWAY STATION-TO-STATION',
     *                           ' FARE'//
     *       1X,'BUSOUT    ',a80,' GENERAL TRANSIT TRIP MATRIX OUTPUT'/
     *       1X,'NBUSOUT   ',a80,' EXPRESS BUS/TRANSITWAY/BRT TRIP',
     *                           ' MATRIX OUTPUT'/
     *       1X,'UROUT     ',a80,' URBAN RAIL TRIP TABLE OUTPUT'/
     *       1X,'CROUT     ',a80,' COMMUTER RAIL TRIP TABLE OUTPUT'/
     *       1X,'BRTOUT    ',a80,' BRT TRIP TABLE OUTPUT'/
     *       1X,'BLENDOUT  ',a80,' BLENDED MODE TRIP TABLE OUTPUT'/
     *       1X,'HWYOUT    ',a80,' HIGHWAY TRIP TABLE OUTPUT'/
     *       1X,'LOGSUMS   ',a80,' LOGSUM MATRIX OUTPUT'/
     *       1X,'FUSERBN   ',a80,' FTA USER BENEFIT FILE OUTPUT'/
     *       1X,'FACCEGR   ',a80,' MODE OF ACCESS/EGRESS FILE OUTPUT'/
     *       1X,'PKUTOUT   ',a80,' PARKING UTILIZATION OUTPUT FILE'/
     *       1X,'SHADOUT   ',a80,' PARKING SHADOW PRICE OUTPUT FILE'/
     *       1X,'FRPORT    ',a80,' REPORT OUTPUT FILE'/
     *       1X,'ERRRPT    ',a80,' ERROR REPORT FILE'/
     *       1X,'FDISTEQ   ',a80,' ZONE-DISTRICT EQUIVALENCE FILE'/
     *       1X,'STNACC    ',a80,' STATION ACCESS FOR BRT,TWY,EXP'//
     *       1X,'CTVINP    ',a80,' CALIBRATION TARGET VALUE INPUT'/
     *       1X,'RALIND    ',a80,' URBAN RAIL SELECTED LINK',
     *                           ' INDICATOR MATRIX'/,
     *       1X,'TLOGSUMS  ',a80,' TRANSIT LOGSUM FILE'/
     *       1X,'NHBSTA    ',a80,' STATION ALIGHTING FILE',
     *                           ' (URBAN RAIL)'/
     *       1X,'TXFERSUM  ',a80,' STATION TRANSFER SUMMARY'/
     *       1X,'FLOWRAIL  ',a80,' LOW INCOME STATION-TO-STATION',
     *                           ' MATRIX'/
     *       1X,'FLOWINC   ',a80,' LOW INCOME DISTRICT PERCENTAGES'/
     *       1X,'FLOWDIST  ',a80,' LOW INCOME DISTRICT DEFINITION'/
     *       1X,'LAXPARK   ',a80,' LAX PARKING SPACE FILE'/
     *       1X,'FLYAWAY   ',a80,' FLY-A-WAY PARKING FILE'/
     *       1X,'FLYTRIP   ',a80,' FLY-A-WAY OUTPUT TRIP MATRIX'/
     *       1X,'RENTAL    ',a80,' RENTAL CAR FACILITY FILE'/
     *       1X,'FLOTTRN   ',a80,' LAX PARKING LOT TRANSIT MATRICES'/
     *       1X,'FRNTTRN   ',a80,' RENTAL CAR FACILTITY TRANSIT',
     *                           ' MATRICES'/
     *       1X,'FITFTRN   ',a80,' ITF TRANSIT MATRICES'/
     *       1X,'FAPMIND   ',a80,' LAX APM INDICATOR MATRICES'/
     *       1X,'FLUNREL   ',a80,' LINK UNRELABILITY SKIM MATRIX'/
     *       1X,'FNUNREL   ',a80,' NODE UNRELABILITY SKIM MATRIX'/
     *       1X,'FCAPAC    ',a80,' CAPACITY PENALTY SKIM MATRIX'/
     *       1X,'FCROWD    ',a80,' CROWDING PENALTY SKIM MATRIX'/
     *       1X,'FVENUE    ',a80,' SPECIAL EVENT VENUE FILE'/
     *       1X,'FSPEVENT  ',a80,' SPECIAL EVENT PERSON TRIP TABLE'//
     *       1X,'FLUNRELBK ',a80,' BIKE LINK UNRELABILITY SKIM MATRIX'/
     *       1X,'FNUNRELBK ',a80,' BIKE NODE UNRELABILITY SKIM MATRIX'/
     *       1X,'FCAPACBK  ',a80,' BIKE CAPACITY PENALTY SKIM MATRIX'/ 
     *       1X,'FCROWDBK  ',a80,' BIKE CROWDING PENALTY SKIM MATRIX'/ 
     *       1X,'FNHBPKHBW ',a80,' PEAK HBW STATION ACCESS/EGRESS'/
     *       1X,'FNHBPKHBO ',a80,' PEAK HBO STATION ACCESS/EGRESS'/
     *       1X,'FNHBPKHBU ',a80,' PEAK HBU STATION ACCESS/EGRESS'/
     *       1X,'FNHBPKHBS ',a80,' PEAK HBS STATION ACCESS/EGRESS'/
     *       1X,'FNHBOPHBW ',a80,' OFPK HBW STATION ACCESS/EGRESS'/
     *       1X,'FNHBOPHBO ',a80,' OFPK HBO STATION ACCESS/EGRESS'/
     *       1X,'FNHBOPHBU ',a80,' OFPK HBU STATION ACCESS/EGRESS'/
     *       1X,'FNHBOPHBS ',a80,' OFPK HBS STATION ACCESS/EGRESS'/)
      IF(NAMES(34).NE.BLANK) THEN
      STNAC=.TRUE.
      ELSE
      WRITE(26,8004)
 8004 FORMAT(/,1X,'RDFILES(W) 8004 STATION OF ACCESS FOR EXPRESS BUS,',
     *            ' TRANSITWAY, AND BRT UNAVAIABLE'/)
      END IF
      IF(NAMES(37).NE.BLANK) THEN
      SKMP4=.TRUE.
      ELSE
      WRITE(26,8005)
 8005 FORMAT(/1X,'RDFILES(W) 8005 4+ PERSON AUTO SKIMS UNAVAILABLE'/)
      END IF
      IF(NAMES(40).NE.BLANK) INDRAL=.TRUE.     !Rail Indicator Matrix (RALIND)
      IF(NAMES(43).NE.BLANK) TRNUSER=.TRUE.    !Transit LogSums
      IF(NAMES(88).NE.BLANK) ISTSFARE=.TRUE.   !Station-to-Station Fare Indicator
      RETURN
      END
