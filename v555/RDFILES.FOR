C**********************************************************
C   SUBROUTINE RDFILES: OBTAIN DATA SET NAMES FOR INPUT   *
C                       FILES                             *
C**********************************************************
      SUBROUTINE RDFILES(CTLERR)
      INCLUDE 'param.com'
      LOGICAL      CTLERR
      INTEGER*4    IT,ST,TT,UT,VT,XT,LEN(49),RLEN
      CHARACTER*1  COMMA,BLANK
      CHARACTER*10  FILES(49)
      CHARACTER*90 NAMES(49)
      CHARACTER*90 STRING
      DATA         COMMA/','/,BLANK/' '/
      DATA         FILES/'XYCOORD=','ZFSTD=','ZFSUP=','WFNAME=',
     *                   'PKUTIN=','PKUTOUT=','SHADIN=','SHADOUT=',
     *                   'HWYOUT=','CROUT=','UROUT=','BUSOUT=',
     *                   'NBUSOUT=','INCOUT=','TXMOUT=','LOGSUMS=',
     *                   'FUSERBN=','STATDATA=','PERTT=','BCRSK=',
     *                   'BURSK=','WBSSK=','WRBSK=','WEBSK=',
     *                   'WTWSK=','DASK=','P2SK=','P3SK=','FRPORT=',
     *                   'PEROUT=','FCRPRB=','FDISTEQ=','WBRTSK=',
     *                   'STNACC=','CTVINP=','AVLPER=','P4SK=',
     *                   'OBSMTX=','DENOUT=','RALIND=','FBESTPATH=',
     *                   'REVENUE=','SELRAL=','FACEG=','SLINKTLF=',
     *                   'SLINKTRP=','SLINKDIST=','FDISTNAME=',
     *                   'SLINKSTA='/
      DATA         NAMES/49*' '/
      DATA         LEN/7,5,5,6,6,7,6,7,6,5,5,6,
     *                 7,6,6,7,7,8,5,5,5,5,5,5,
     *                 5,4,4,4,6,6,6,7,6,6,6,6,
     *                 4,6,6,6,9,7,6,5,8,8,9,9,
     *                 8/
      CTLERR=.FALSE.
C
C SEARCH FOR ONE FILE NAME AT A TIME
C
      DO 100 IT=1,49
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
      ZFSUP=NAMES(3)
      WFNAME=NAMES(4)
      PKUTIN=NAMES(5)
      PKUTOUT=NAMES(6)
      SHADIN=NAMES(7)
      SHADOUT=NAMES(8)
      HWYOUT=NAMES(9)
      CROUT=NAMES(10)
      UROUT=NAMES(11)
      BUSOUT=NAMES(12)
      NBUSOUT=NAMES(13)
      INCOUT=NAMES(14)
      TXMOUT=NAMES(15)
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
      PEROUT=NAMES(30) 
      FCRPRB=NAMES(31) 
      FDISTEQ=NAMES(32)
      WBRTSK=NAMES(33)
      STNACC=NAMES(34)
      CTVINP=NAMES(35)
      AVLPER=NAMES(36)
      P4SK=NAMES(37)
      OBSMTX=NAMES(38)
      DENOUT=NAMES(39)
      RALIND=NAMES(40)
      FBESTPATH=NAMES(41)
      REVENUE=NAMES(42)
      SELRAL=NAMES(43)
      FACEG=NAMES(44)
      SLINKTLF=NAMES(45)
      SLINKTRP=NAMES(46)
      SLINKDIST=NAMES(47)
      FDISTNAME=NAMES(48)
      SLINKSTA=NAMES(49)
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
C      CALL idate_(IARRAY)
C      CALL itime_(IARRAY2)
C      IARRAY(3)=IARRAY(3)-2000
C      WRITE(26,8001) IARRAY(2),IARRAY(1),IARRAY(3),IARRAY2
 8001 FORMAT(/20X,' PROGRAM MTAMC  (30Sep15)',/,
     *        20X,' -------------------------',/,
C    *        20X,'    [RIVERSIDE VERSION]'/
     *        20X,'      [3731 VERSION]'/
     *        20X,'     [Update 18Mar18]'/
     *        20X,'      [Version 5.55]',//,
     *        20X,'     Date:',I2,'/',I2,'/',I2/
     *        20X,'     Time:',I2,':',I2,':',I2//)
      WRITE(*,8001) IMON,IDAY,IYR,IHR,IMIN,ISEC
C     WRITE(*,8001) IARRAY(2),IARRAY(1),IARRAY(3),IARRAY2
C 
C PRINT FILE INPUT/OUTPUT SUMMARY
C
      WRITE(26,500) PERTT,ZFSTD,ZFSUP,WFNAME,PKUTIN,SHADIN,
     *              STATDATA,XYCOORD,DASK,P2SK,P3SK,P4SK,WBSSK,
     *              WRBSK,WEBSK,WTWSK,WBRTSK,BURSK,BCRSK,BUSOUT,
     *              NBUSOUT,UROUT,CROUT,HWYOUT,LOGSUMS,FUSERBN,
     *              INCOUT,PKUTOUT,SHADOUT,TXMOUT,FRPORT,
     *              FCRPRB,FDISTEQ,STNACC,CTVINP,AVLPER,PEROUT,
     *              OBSMTX,FBESTPATH,RALIND,REVENUE,SELRAL,FACEG,
     *              SLINKTLF,SLINKTRP,SLINKDIST,FDISTNAME,
     *              SLINKSTA
  500 FORMAT(1X,'SUMMARY OF FILE INPUTS & OUTPUTS'/
     *       1X,'--------------------------------------------------'/
     *       1X,'PERTT     ',a80,' PERSON TRIP MATRIX INPUT'/
     *       1X,'ZFSTD     ',a80,' STANDARD S/E FILE INPUT'/
     *       1X,'ZFSUP     ',a80,' SUPPLEMENTAL S/E FILE INPUT'/
     *       1X,'WFNAME    ',a80,' PERCENT WALK FILE INPUT'/
     *       1X,'PKUTIN    ',a80,' PARKING UTILIZATION FILE INPUT'/
     *       1X,'SHADIN    ',a80,' PARKING SHADOW PRICE INPUT FILE'/
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
     *       1X,'WBRTSK    ',a80,' WALK TO BUS RAPID TRANSIT SKIM',
     *                          ' MATRIX'/
     *       1X,'BURSK     ',a80,' BUS TO URBAN RAIL SKIM MATRIX'/
     *       1X,'BCRSK     ',a80,' BUS TO COMMUTER RAIL SKIM MATRIX'//
     *       1X,'BUSOUT    ',a80,' GENERAL TRANSIT TRIP MATRIX OUTPUT'/
     *       1X,'NBUSOUT   ',a80,' EXPRESS BUS/TRANSITWAY/BRT TRIP',
     *                          ' MATRIX OUTPUT'/
     *       1X,'UROUT     ',a80,' URBAN RAIL TRIP TABLE OUTPUT'/
     *       1X,'CROUT     ',a80,' COMMUTER RAIL TRIP TABLE OUTPUT'/
     *       1X,'HWYOUT    ',a80,' HIGHWAY TRIP TABLE OUTPUT'/
     *       1X,'LOGSUMS   ',a80,' LOGSUM MATRIX OUTPUT'/
     *       1X,'FUSERBN   ',a80,' FTA USER BENEFIT FILE OUTPUT'/
     *       1X,'INCOUT    ',a80,' LOW INCOME TRANSIT TRIP TABLE',
     *                          ' OUTPUT'/
     *       1X,'PKUTOUT   ',a80,' PARKING UTILIZATION OUTPUT FILE'/
     *       1X,'SHADOUT   ',a80,' PARKING SHADOW PRICE OUTPUT FILE'/
     *       1X,'TXMOUT    ',a80,' URBAN RAIL TRANSFER MATRIX OUTPUT'//
     *       1X,'FRPORT    ',a80,' REPORT OUTPUT FILE'/
     *       1X,'FCRPRB    ',a80,' COMMUTER RAIL PROBABILITY MATRIX'/
     *       1X,'FDISTEQ   ',a80,' ZONE-DISTRICT EQUIVALENCE FILE'/
     *       1X,'STNACC    ',a80,' STATION ACCESS FOR BRT,TWY,EXP'//
     *       1X,'CTVINP    ',a80,' CALIBRATION TARGET VALUE INPUT'/
     *       1X,'AVLPER    ',a80,' AVAILABLE PERSON TRIP MATRICES'/
     *       1X,'PEROUT    ',a80,' FACTORED PERSON TRIP MATRIX'/
     *       1X,'OBSMTX    ',a80,' STATION DATA/SHARES MATRIX'/
     *       1X,'FBESTPATH ',a80,' BESTPATH SKIM MATRIX'/
     *       1X,'RALIND    ',a80,' URBAN RAIL SELECED LINK',
     *                           ' INDICATOR MATRIX'/,
     *       1X,'REVENUE   ',a80,' REVENUE ESTIMATE MATRIX'/
     *       1X,'SELRAL    ',a80,' SELECT LINK RAIL INDICATOR MATRIX'/
     *       1X,'FACEG     ',a80,' URBAN RAIL STATIONS FOR ',
     *                           'COMMUTER RAIL'/
     *       1X,'SLINKTLF  ',a80,' SELECT LINK TLF'/
     *       1X,'SLINKTRP  ',a80,' SELECT LINK TRIP MATRICES'/
     *       1X,'SLINKDIST ',a80,' SELECT LINK DISTRICT SUMMARY'/
     *       1X,'FDISTNAME ',a80,' DISTRICT NAMES (OPTIONAL)'/
     *       1X,'SLINKSTA  ',a80,' SELECT LINK STATION MATRICES'/)
C
C CHECK FOR PRESENCE OF TRANSITWAY AND BRT SKIM INPUT ?
C
      IF(NAMES(25).NE.BLANK) THEN
      TWYSK=.TRUE.
      ELSE
      WRITE(26,8002)
 8002 FORMAT(/1X,'RDFILES(W) 8002 TRANSITWAY MODE UNAVAILABLE'/)
      END IF
      IF(NAMES(33).NE.BLANK) THEN
      BRTSK=.TRUE.
      ELSE
      WRITE(26,8003)
 8003 FORMAT(/,1X,'RDFILES(W) 8003 BUS RAPID TRANSIT MODE UNAVAIABLE'/)
      END IF
      IF(NAMES(34).NE.BLANK) THEN
      STNAC=.TRUE.
      ELSE
      WRITE(26,8004)
 8004 FORMAT(/,1X,'RDFILES(W) 8004 STATION OF ACCESS FOR EXPRESS BUS,',
     *            ' TRANSITWAY, AND BRT UNAVAIABLE'/)
      END IF
      IF(NAMES(36).NE.BLANK) PERAVL=.TRUE.
      IF(NAMES(37).NE.BLANK) THEN
      SKMP4=.TRUE.
      ELSE
      WRITE(26,8005)
 8005 FORMAT(/1X,'RDFILES(W) 8005 4+ PERSON AUTO SKIMS UNAVAILABLE'/)
      END IF
      IF(NAMES(38).NE.BLANK) MTXOBS=.TRUE.
      IF(NAMES(39).NE.BLANK) OUTDEN=.TRUE.
      IF(NAMES(40).NE.BLANK) INDRAL=.TRUE.
      IF(NAMES(42).NE.BLANK) REVEST=.TRUE.
      IF(MTXOBS.AND.REVEST) THEN
      WRITE(26,8006)
 8006 FORMAT(/1X,'RDFILE(F) 8006 OBSMTX & REVENUE',
     * ' CANNOT BE OUTPUT IN THE SAME RUN')
      STOP 8
      END IF
      RETURN
      END
