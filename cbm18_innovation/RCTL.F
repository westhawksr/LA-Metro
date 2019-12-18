C-------------------------------------------------------------------
C      READ CONTROL FILE SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE RCTL(PWALK,HOV2P,HOV3P,HOV4P,ZHHD,EQUIV,
     *                SXCOORD,SYCOORD)
      INCLUDE 'stadat.com'
      INCLUDE 'param.com'
      INCLUDE 'mtamcpar.inc'
C
C DECLARATIONS REQUIRED FOR SUBROUTINE
C
      
      INTEGER*2     IMON,IDAY,IYR,IHR,IMIN,ISEC,I100
C     INTEGER*4     IARRAY(3),IARRAY2(3),NCBDZ,NCTAZ
      integer*2     szone,sc,sc2,equv1,equv2,equv3,ERRN,K2,K1,E
      INTEGER*2     EQUV4,EQUV5,STAT1,LINDEX,JCOUNT
      integer*2     minsta,nmnsta,STAZONE,EQUIV(MAX_STATIONS)
      INTEGER*4     SNODE1,SNODE2,PCOSTZ(10)
      INTEGER*4     ZONE,STACOD,DIST(6),LDIST
      INTEGER*4     K,KK,NZONES,DIZ,INTEN(100)
      INTEGER*4     ALOW,AHIGH,ZINDEX,NODE                                     !Innovation
      REAL*4        RESID,AVGVAL,AVPROP(6,4)                                   !Innovation
      REAL*4        SPRICE,wlkb14,wlkb12,wlkr14,wlkr12,pcpi
      REAL*4        INCOME,INC1,INC2,INC3,TTIME,ZPCOST,COCODE
      REAL*4        PCOST,PKUSED,active,pcapc,HPCOST
      REAL*4        MCOEF(10),VCOEF(25),RVALUE(5)
      REAL*4        PERINC1,PERINC2,PERINC3,TOTHH
      REAL*4        PWALK(MAX_ZONES,2),ZHHD(34,MAX_IZONES)
      REAL*4        PKCOST(10),PDEN,ADEN,LSHARE
      REAL*4        AWALK,BWALK,PRWALK,PIVT,KWALK,KIVT
      REAL*4        POP,TOTEMP,AREA,DENSITY,CSPACES,RETAIL
      REAL*4        ADENP,BDENP,CEMPDEN,CDENP,DDENP,EDENP
      REAL*4        ADENA,BDENA,CDENA,DDENA,EDENA
      REAL*4        XCOOR,YCOOR,SXCOORD(MAX_IZONES),SYCOORD(MAX_IZONES)
      REAL*4        CLUNREL,CNUNREL,CNCAPAC,CLCROWD
      REAL*4        ATTEND,BIKEATT(8),ZEROTLF(22,20)
      REAL*4        XC,YC,BIKACC(5),BIKSHARE(2),BIKPER(5)
      character*1   mode,FF,PTYPE,PNR,KNR,ULINE,RED,BLUE,COMMA
      CHARACTER*1   GREEN,GOLD
      CHARACTER*2   EGTYPE,WALK,SHUTTLE,TRNONLY,ITF,ITF2
      CHARACTER*6   TPURP,TDAY
      CHARACTER*7   NAMERR(6)
      CHARACTER*10  DNAME(21)
      CHARACTER*60  ALTNAME,CONTROL
      CHARACTER*37  SNAME
      CHARACTER*200 SRECORD
      CHARACTER*300 NRECORD
      CHARACTER*80  HEADER
      LOGICAL       EXISTS,CTLERR(6),STACODE,LSUMERR,TXERR
      DATA          NAMERR/'FILES  ','PARAMS ','COEFFS ',
     *                     'OPTIONS','CONSTS ','OBS    '/
      DATA          PNR/'P'/,KNR/'K'/,RED/'R'/,BLUE/'B'/
     *              GREEN/'G'/,GOLD/'L'/,COMMA/','/
      DATA          WALK/'W'/,SHUTTLE/'ST'/,TRNONLY/'T'/
      DATA          ITF/'I'/,ITF2/'I2'/
      DATA  DNAME/'Riverside','SGV East','Gateway','SouthBay',
     *      'Westside',
     *      'Malibu','SBD','OC','Glendale','LACBD','LA Central',
     *      'LAnorth','Ventura','East LA','SFV','SGV West',
     *      'SantaMonic','CenturyBev','Brentwood','Arroyo',
     *      'Total'/
C
C     NAMELIST DEFINITION..........
C
      NAMELIST  /PARAMS/ TPURP,TDAY,ALTNAME,NITER,I,
     *                   J,OPCOST,HOV3P,HOVDIST,CTAZNE,
     *                   TOLDIST,TOLCST,WAITLT,RNTWAIT,LAXOCC,
     *                   PCPI,BPFACE,BPFACT,AUTOCC,
     *                   CBDZ,PERFACT,EGRDIST,EXPACCD,TWYACCD,LAXZ,
     *                   SCOORD,SWALK,LWALK,FCPI,BPFACR,
     *                   UNIONSTA,HOV4P,HOV2P,MINSAV,
     *                   PDRV0,PDRV2,PDRV4,NCATS,FIXIJTRIPS,
     *                   ITFZONE,ITFSAVFLY,ITFSAVONC,ITFSAVPRK,
     *                   ITFSAVAPM,INTRASPD,BSV,
     *                   WALKSPD,BIKESPD,HHFACT,TLINE3,TLINE4,
     *                   TLINE5,ITFZONE2,HTLZNE,AVPROP,
     *                   UBCOEF,TXCOEF,TAXIOBS,ADJFCT,TITER                    !Innovation
      NAMELIST /OPTIONS/ DEBUG,CALIB,LOGSUM,USERBEN,LDETAIL,
     *                   TRIPSOUT,EXPCAP,WALKTIME,HFACT,
     *                   CAPRES,NMOT,STACAR,BICYCLE,TRNEGR,
     *                   NEGSHP,NOTXFR,BUSTEXP,SDETAIL,STACODE,
     *                   AIRPASS,TXFSUM,WLKSTA,SPEVENT,HCALIB,
     *                   LAX,LDEBUG,EMPPRK,TRNLOT,ZEROCAR,PEAK,
     *                   LUNREL,NUNREL,NCAPAC,LCROWD,VDETAIL,
     *                   ITFONC,ITFFLY,ITFPRK,CONRAC,APMIND,NHBDIR,
     *                   NOTWYAE,ITFTXI,ITFLMO,ITFDRP,ITFHTL,
     *                   SAVPROB,LOWRAIL,TAXIMODE,UBERMODE,UBERTRN,            !Innovation
     *                   AVMDL,ALTPARK,AVSKIM,ESCOOTER,TCALIB,FDEBUG
      NAMELIST  /COEFFS/ MCOEF,MWALK1,MWALK2,MBIKE1,MBIKE2,MWALKT,
     *                   LSUM3CW,LSUM3CB,LSUM3CP,LSUM3CK,VCOEF,LSUMA,
     *                   LSUM3CBK,LSUM3UBK,BZCOEF,BSTCOEF,
     *                   LSUMS,LSUMT,CCRPNR,CCBD,CRDIST,DISTEXP,
     *                   CURPNR,LSUM3UW,LSUM3UB,LSUM3UP,LSUM3UK,
     *                   CDAR,URBDIST,URGDIST,COEFF,YINTER,YSLOPE,
     *                   LSUM2TW,LSUM2RB,ADENP,BDENP,CSPACES,
     *                   CEMPDEN,CDENP,DDENP,SAVCOEF,
     *                   EDENP,DSCT2P,DSCT3P,DSCT4P,ADENA,BDENA,CDENA,
     *                   DDENA,EDENA,LSUM2BR,MINDEN,ACOEF,LSUM1AUTO,
     *                   LSUM1TRN,LSUM1NM,LSUM2DA,ADJ1UR,ADJ1CR,
     *                   LSUM2SR,LSUM2UR,LSUM2CR,
     *                   LSUMWLK,LSUMBK,LSUM3DA,
     *                   LSUM3P2,LSUM3P3,LSUM3P4,
     *                   LSUM2LB,LSUM2EB,LSUMR,
     *                   CLUNREL,CNUNREL,CNCAPAC,CLCROWD,
     *                   NHBCOEF,NMLSUM,ITFPEN,
     *                   LSBRBIK,LSUM3BRW,LSUM3BRB,LSUM3BRP,
     *                   LSUM3BRK,LSUM3BRBK,MAX_SCOOTER,SCOTSPD
      NAMELIST  /CONSTS/ KKCR,KPCR,KBCR,KWCR,KKUR,KPUR,KBUR,KWUR,
     *                   KBKCR,KBKUR,KINTRAWK,KINTRABK,
     *                   KWWAY,KDWAY,KWEXP,KDEXP,KWBUS,KDBUS,KDTRN,
     *                   KBBUS,KBRPD,KBWAY,KBEXP,KBBRT,
     *                   KWBR,KBBR,KPBR,KKBR,KBKBR,
     *                   KPNR,KDA,K2P,K3P,K4P,KCR,KUR,KEBUS,KWAY,KLBUS,
     *                   KRBUS,KSR,KAUT,KTRN,KNMOT,KBIKE,KBRPDW,KBTWYW,
     *                   KBRPDB,KBTWYB,KBEXPB,KBBRTB,KEXPO,
     *                   KESFV,KWSAB,KSEPV,KIRCN,KVERM,
     *                   KBEXPW,KINFL,KINFR,KRED,KBLUE,KBEXPD,KBTWYD,
     *                   KGREEN,KGOLD,KTRNT,KBRT,KBRPDD,
     *                   KBBRTW,KBBRTD,CCBD,KTOLL,KDBRT,KTWYAE,KRPDAE,
     *                   KTOLL2,KTOLL3,KTOLL4,KHOV2,KHOV3,KHOV4,
     *                   KLOTRN,KFLYAWAY,KFLYKNR,KFLYPNR,ACNST,
     *                   KFLYWLK,KFLYTXI,KFLYTRN,KPUBPRK,AWUR,
     *                   PCONST,ACONST,PACONST,KSTAMODE,KBRTEGR,
     *                   KTAXI,KUBERMODE,KSCOT,KINTRASC                        !Innovation
      NAMELIST  /OBS/    NITER,CCODE,BIKRAPID,DRVRAPID,OBCBDTR,
     *                   WLKRAPID,BIKEXP,DRVEXP,WLKEXP,
     *                   BIKTWY,DRVTWY,WLKTWY,BIKBRT,DRVBRT,WLKBRT,
     *                   IAUTO,IWALK,IBIKE,WLTXFER,BLTXFER,
     *                   DLTXFER,WRTXFER,BRTXFER,DRTXFER,WETXFER,
     *                   BETXFER,DETXFER,WTTXFER,BTTXFER,DTTXFER,
     *                   WBTXFER,BBTXFER,DBTXFER,CBUSEGR,UBUSEGR,
     *                   ADJFCT,ADJFCT2,CLOSURE,MAXCALIT,MINVAL,
     *                   STAOBS,OBRTEGR,OBCBDCR
C--------------------------------------------------------------------
      FF=CHAR(12)
      NCATS=5
      PERFACT=1.0
      SCOORD=0.01
      HOV2P=1.2
      HOV3P=1.7
      HOV4P=2.0
      DSCT2P=1.0
      DSCT3P=1.0
      DSCT4P=1.0
      WAITLT=5.0
      LSUMA=0.75
      LSUMS=0.60
      LSUMT=0.60    
      LSUMR=0.40
      RNTWAIT=15.0
      LAXOCC=1.0
      MINVAL=-15.0
      SPEVENT=.FALSE.
      LAX=.FALSE.
      AIRPASS=.FALSE.
      MINSAV=0.0
      SAVCOEF=0.0
      NCBDZ=0
      KDBRT=0.0
      ADJFCT2=0.5
      PCONST=0.0
      ACONST=0.0
      PACONST=0.0
      TWYSK=.TRUE.
      SKM4P=.FALSE.
      BICYCLE=.FALSE.
      WALKTIME=.FALSE.
      TRNEGR=.FALSE.
      WALKSPD=3.0
      BIKESPD=10.0
      CCODE=.FALSE.
      BICYCLE=.FALSE.
      PEAK=.TRUE.
      HHFACT=0.0
      HFACT=.FALSE.
      HCALIB=.FALSE.
      STACODE=.FALSE.
      ZEROTLF=0.0
      STAOBS=0.0
      KBRTEGR=0.0
      COEFF=0.0
      TLINE3=0.0
      TLINE4=0.0
      TLINE5=0.0
      NOTWYAE=.FALSE.
      KRED=0.0
      KBLUE=0.0
      KGREEN=0.0
      KGOLD=0.0
      KEXPO=0.0
      KWSAB=0.0
      KESFV=0.0
      KSEPV=0.0
      KAIRCN=0.0
      KVERM=0.0
      LSUMERR=.FALSE.
      ITFTXI=.FALSE.
      ITFLMO=.FALSE.
      ITFDRP=.FALSE.
      TRIPSOUT=.FALSE.
      NCBDZ=0
      ITFPEN=0.0
      LDETAIL=.FALSE.
      LOWRAIL=.FALSE.
      TAXIMODE=.FALSE.                                                         !Innovation
      UBERMODE=.FALSE.                                                         !Innovation
      UBERTRN=.FALSE.                                                          !Innovation
      UBCOEF=0.0                                                               !Innovation
      TXCOEF=0.0                                                               !Innovation
      KTAXI=0.0                                                                !Innovation
      KUBERMODE=0.0                                                            !Innovation
      AVMDL=.FALSE.                                                            !Innovation
      ALTPARK=.FALSE.                                                          !Innovation
      AVSKIM=.FALSE.                                                           !Innovation
      ESCOOTER=.FALSE.                                                         !Innovation
      SCOTSPD=10.0                                                             !Innovation
      MAX_SCOOTER=3.0                                                          !Innovation
      AVPROP=0.0                                                               !Innovation
      TXERR=.FALSE.
      FDEBUG=.FALSE.
C
C     OPEN AND READ THE CONTROL FILE
C
      CALL GETARG(1,CONTROL,STAT1)
      IF(STAT1.LE.0) THEN
        WRITE(*,'('' RCTL (F) : CONTROL FILE NOT SPECIFIED'')')
        STOP " ENTER THE CONTROL FILE NAME ON THE COMMAND LINE."
      END IF
      OPEN(10,FILE=CONTROL,STATUS='OLD',FORM='FORMATTED',
     *     ERR=90)
      REWIND 10
      GO TO 91
   90 WRITE(*,9510)
 9510 FORMAT(/,1X,'RCTL 9510 (F) ERROR OPENING CONTROL FILE INPUT ',
     *       'OR DATA SET NOT FOUND')
      STOP 8
   91 CALL RDFILES(CTLERR(1))
      REWIND 10
      READ(10,PARAMS,ERR=5)
      REWIND 10
      GO TO 10
    5 CTLERR(2)=.TRUE.
   10 READ(10,COEFFS,ERR=15)
      REWIND 10
      GO TO 20
   15 CTLERR(3)=.TRUE.
   20 READ(10,OPTIONS,ERR=25)
      REWIND 10
      GO TO 30
   25 CTLERR(4)=.TRUE.
   30 READ(10,CONSTS,ERR=35)
      REWIND 10
      GO TO 40
   35 CTLERR(5)=.TRUE.
   40 IF(CALIB) READ(10,OBS,ERR=45)
      REWIND 10
      GO TO 50
   45 CTLERR(6)=.TRUE.
C
C     CLOSE THE CONTROL FILE AND OPEN THE STATION ERROR FILE;
C
   50 CLOSE(10,STATUS='KEEP')
      IF(DEBUG) THEN
      OPEN(42,FILE='MTAMC.UR.STACCC.RPT',STATUS='UNKNOWN',
     *        FORM='FORMATTED')
      OPEN(41,FILE='MTAMC.CR.STACCC.RPT',STATUS='UNKNOWN',
     *        FORM='FORMATTED')
      END IF
      OPEN(75,FILE=ERRRPT,STATUS='UNKNOWN',
     *        FORM='FORMATTED')
      OPEN(96,FILE='EXPBUS.ERR',STATUS='UNKNOWN',FORM='FORMATTED')
      OPEN(97,FILE='TWYBUS.ERR',STATUS='UNKNOWN',FORM='FORMATTED')
      OPEN(130,FILE=TXFERSUM,STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(130,77)
   77 FORMAT('STATION_FROM,NAME,STATION_TO,NAME,TRIPS')
C
C     CHECK FOR ERRORS IN THE READS; WRITE ANY MESSAGES TO REPORT FILE
C
      DO 75 E=1,6
        IF(CTLERR(E)) THEN
          WRITE(*,9002) NAMERR(E)
 9002     FORMAT(/' RCTL 9002 (F) ERROR(S) READING &',A7)
          ERRN=ERRN+1
        ENDIF
   75 CONTINUE
      IF(ERRN.GT.0) STOP 16
C 
C DETERMINE THE NUMBER OF CBD ZONES PROVIDED
C
      DO 76 E=1,50
      IF(CBDZ(E).GT.0) NCBDZ=E
      IF(E.GT.10) GO TO 76
      IF(CTAZNE(E).GT.0) NCTAZ=E
      IF(HTLZNE(E).GT.0) NHTLZ=E
   76 CONTINUE
C
C OPEN STATION-BASED BINARY FILES
C
      IF(DEBUG) THEN
      OPEN(31,FILE='CRUR.STA',STATUS='UNKNOWN',
     *          FORM='BINARY')
      OPEN(27,FILE='EPTW.STA',STATUS='UNKNOWN',
     *          FORM='BINARY')
      OPEN(33,FILE='CRUR.EGR',STATUS='UNKNOWN',
     *          FORM='BINARY')
      OPEN(34,FILE='EPTW.EGR',STATUS='UNKNOWN',
     *          FORM='BINARY')
      END IF
      IF(AIRPASS) LAX=.TRUE.
      IF(SPEVENT.AND.CALIB) THEN
      WRITE(26,8100) 
 8100 FORMAT(/' SELF CALIBRATION (CALIB) FUNCTION NOT AVAILABLE ',
     *        'FOR SPECIAL EVENT MODEL'/)
      WRITE(*,8100)
      STOP
      END IF
      IF((ITFONC.GT.0.OR.(ITFFLY.GT.0).OR.ITFPRK).AND.ITFZONE.LE.0) 
     *      THEN
      WRITE(*,8101)
      WRITE(26,8101)
 8101 FORMAT(/' ITF OPTION SET TO TRUE FOR ON-CALL, FLY-AWAY, OR ',
     *        'LAX PARKING LOT',
     *        '---ITF ZONE NOT PROVIDED')
      STOP 8101
      END IF
C
C     CHECK INTRAZONAL SPEED
C
      IF(INTRASPD.LE.0) THEN
      INTRASPD=25.0
      WRITE(26,8445)
 8445 FORMAT(/' RCTL 8445 (W) INTRAZONAL SPEED NOT PROVIDED,'
     *       ' SET SPEED TO 25 MPH'/)
      END IF
C
C     CHECK NHB DIRECT DEMAND MODEL COEFFICIENTS
C
      IF(NHBCOEF(1).EQ.0.0.AND.(NHBDIR)) THEN
      WRITE(26,8446)
      WRITE(*,8446) 
 8446 FORMAT(/' RCTL 8446 (F) NHB DIRECT DEMAND COEFFICIENT #1 IS ',
     *       ' ZERO'/)
      STOP 8446
      END IF
C
C     SET LOGSUM DEFAULT VALUES
C
      IF(LSUM1AUTO.EQ.0.0) LSUM1AUTO=LSUMA
      IF(LSUM2DA.EQ.0.0) LSUM2DA=LSUMS
      IF(LSUM3DA.EQ.0.0) LSUM3DA=LSUMT
      IF(LSUM2SR.EQ.0.0) LSUM2SR=LSUMS
      IF(LSUM3P2.EQ.0.0) LSUM3P2=LSUMT
      IF(LSUM3P3.EQ.0.0) LSUM3P3=LSUMT
      IF(LSUM3P4.EQ.0.0) LSUM3P4=LSUMT
      IF(LSUM1TRN.EQ.0.0) LSUM1TRN=LSUMA
      IF(ADJ1UR.EQ.0.0) ADJ1UR=1.0
      IF(ADJ1CR.EQ.0.0) ADJ1CR=1.0
      IF(LSUM2LB.EQ.0.0) LSUM2LB=LSUMS
      IF(LSUM2EB.EQ.0.0) LSUM2EB=LSUMS
      IF(LSUM2TW.EQ.0.0) LSUM2TW=LSUMS
      IF(LSUM2RB.EQ.0.0) LSUM2RB=LSUMS
      IF(LSUM2BR.EQ.0.0) LSUM2BR=LSUMS
      IF(LSUM2UR.EQ.0.0) LSUM2UR=LSUMS
      IF(LSUM2CR.EQ.0.0) LSUM2CR=LSUMS
      IF(LSUM1NM.EQ.0.0) LSUM1NM=LSUMA
C
C SUMMARIZE INPUT CONTROL FILE INFORMATION
C
      WRITE(26,7003) I,J
 7003 FORMAT(1X,'GLOBAL MODEL PARAMETERS'/
     *       1X,'-----------------------'//
     *       1X,'SELECTED ORIGIN ZONES (I)          =',10I5/
     *       1X,'SELECTED DESTINATION ZONES (J)     =',10I5/)
      IF(NCBDZ.GT.0) THEN
      WRITE(26,8024) (CBDZ(E),E=1,NCBDZ)
 8024 FORMAT(1X,'CBD ZONES (CBDZ)                   =',10I5/
     *       1X,'                                   =',10I5/
     *       1X,'                                   =',10I5/
     *       1X,'                                   =',10I5/
     *       1X,'                                   =',10I5/)
      END IF
      IF(NCTAZ.GT.0) THEN
      WRITE(26,8124) (CTAZNE(E),E=1,NCTAZ)
 8124 FORMAT(1X,'CTA ZONES (CTAZNE)                 =',10I5)
      END IF
      IF(NHTLZ.GT.0) THEN
      WRITE(26,8224) (HTLZNE(E),E=1,NCTAZ)
 8224 FORMAT(1X,'HOTEL ZONES (HTLZNE)               =',10I5)
      END IF
      WRITE(26,8023) NITER,NCATS,PCPI,FCPI,HHFACT,
     * BPFACE,BPFACT,BPFACR,AUTOCC,      
     * OPCOST,HOV2P,HOV3P,HOV4P,HOVDIST,
     * TOLDIST,TOLCST,YINTER,YSLOPE,
     * CCRPNR,CURPNR,
     * CRDIST,DISTEXP,EGRDIST,TWYACCD,EXPACCD,
     * CDAR,CCBD,PERFACT,SCOORD,SWALK,LWALK,
     * WAITLT,
     * MINDEN,ADENP,BDENP,
     * CDENP,DDENP,EDENP,ADENA,BDENA,CDENA,DDENA,EDENA,
     * CEMPDEN,DSCT2P,DSCT3P,DSCT4P,MINSAV,SAVCOEF,
     * WALKSPD,BIKESPD,UNIONSTA
 8023 FORMAT(/1X,'PARKING CAP RESTRAINT ITERATIONS   =',I8,' (NITER)'/
     *  1X,'NUMBER OF MARKET SEGMENTS          =',I8,  ' (NCATS)'/
     *  1X,'PARKING COST CPI FACTOR            =',F8.3,' (PCPI)'/
     *  1X,'TRANSIT FARE CPI FACTOR            =',F8.3,' (FCPI)'/
     *  1X,'HOUSEHOLD PERSON TRIP FACTORS      =',3F8.3,' (HHFACT)'/
     *  1X,'PNR PROPORTION FOR EXPRESS BUS     =',F8.3,' (BPFACE)'/
     *  1X,'PNR PROPORTION FOR TRANSITWAY      =',F8.3,' (BPFACT)'/
     *  1X,'PNR PROPORTION FOR BRT             =',F8.3,' (BPFACR)'/
     *  1X,'PARK-N-RIDE AUTO OCCUPANCY         =',F8.3,' (AUTOCC)'/    
     *  1X,'OPERATING COST PER MILE            =',F8.3,' (OPCOST)'/
     *  1X,'2  PERSON AVERAGE OCCUPANCY        =',F8.3,' (HOV2P)'/
     *  1X,'3+ PERSON AVERAGE OCCUPANCY        =',F8.3,' (HOV3P)'/
     *  1X,'4+ PERSON AVERAGE OCCUPANCY        =',F8.3,' (HOV4P)'/
     *  1X,'HOV MINIUMU DISTANCE THRESHOLD     =',F8.3,' (HOVDIST)'/
     *  1X,'TOLL MINIMUM DISTANCE THRESHOLD    =',F8.3,' (TOLDIST)'/
     *  1X,'TOLL COST PER MILE                 =',4F8.3,' (TOLCST)'/
     *  1X,'TOLL CHOICE Y-INTERCEPT VALUE      =',F8.3,' (YINTER)'/
     *  1X,'TOLL CHOICE DISTANCE SLOPE         =',F8.3,' (YSLOPE)'/
     *  1X,'COEFFICIENT ON CR DRIVE DST/TOT DST=',F8.3,' (CCRPNR)'/
     *  1X,'COEFFICIENT OF UR DRIVE DST/TOT DST=',F8.3,' (CURPNR)'/
     *  1X,'COEFFICIENT ON CR DISTANCE         =',F8.3,' (CRDIST)'/
     *  1X,'EXPONENT ON CR DISTANCE FUNCTION   =',F8.3,' (DISTEXP)'/
     *  1X,'MAXIMUM STATION EGRESS AUTO DIST   =',F8.3,' (EGRDIST)'/
     *  1X,'MAXIMUM TRANSITWAY ACCESS AUTO DIST=',F8.3,' (TWYACCD)'/
     *  1X,'MAXIMUM EXPRESSBUS ACCESS AUTO DIST=',F8.3,' (EXPACCD)'/
     *  1X,'COEFFICIENT ON DRIVE ACCESS RATIO  =',F8.3,' (CDAR)'/
     *  1X,'COEEFICIENT ON CBD DESTINATION     =',7F8.3,' (CCBD)'/
     *  1X,'SCALE ON INPUT PERSON TRIPS        =',F8.3,' (PERFACT)'/
     *  1X,'SCALE ON INPUT COORDINATES         =',F8.3,' (SCOORD)'/
     *  1X,'MAXIMUM WALK TIME - SHORT WALK     =',F8.3,' (SWALK)'/
     *  1X,'MAXIMUM WALK TIME -  LONG WALK     =',F8.3,' (LWALK)'/
     *  1X,'1ST WAIT TIME THRESHOLD VALUE      =',F8.3,' (WAITLT)'//
     *  1X,'MINIMUM DENSITY VALUE FOR FUNCTION =',F8.3,' (MINDEN)'/
     *  1X,'A     COEFFICIENT - DENSITY(PROD)  =',F8.5,' (ADEN)'/
     *  1X,'B     COEFFICIENT - DENSITY(PROD)  =',F8.5,' (BDEN)'/
     *  1X,'C     COEFFICIENT - DENSITY(PROD)  =',F8.5,' (CDEN)'/
     *  1X,'D     COEFFICIENT - DENSITY(PROD)  =',F8.5,' (DDEN)'/
     *  1X,'E     COEFFICIENT - DENSITY(PROD)  =',F8.5,' (EDEN)'/
     *  1X,'A     COEFFICIENT - DENSITY(ATTR)  =',F8.5,' (ADEN)'/
     *  1X,'B     COEFFICIENT - DENSITY(ATTR)  =',F8.5,' (BDEN)'/
     *  1X,'C     COEFFICIENT - DENSITY(ATTR)  =',F8.5,' (CDEN)'/
     *  1X,'D     COEFFICIENT - DENSITY(ATTR)  =',F8.5,' (DDEN)'/
     *  1X,'E     COEFFICIENT - DENSITY(ATTR)  =',F8.5,' (EDEN)'/
     *  1X,'EMPLOYMENT COEFFICIENT - DENSITY   =',F8.3,' (CEMPDEN)'//
     *  1X,'TOLL COST DISCOUNT FACTOR  - 2 PER =',F8.3,' (DSCT2P)'/
     *  1X,'TOLL COST DISCOUNT FACTOR  - 3 PER =',F8.3,' (DSCT3P)'/
     *  1X,'TOLL COST DISCOUNT FACTOR  - 4+PER =',F8.3,' (DSCT4P)'/
     *  1X,'TOLL PATH MINIMUM TIME SAVINGS     =',F8.3,' (MINSAV)'/
     *  1X,'TOLL PATH TIME SAVINGS COEFFICIENT =',F8.3,' (SAVCOEF)'/
     *  1X,'INTERCHANGE WALK SPEED             =',F8.3,' (WALKSPD)'/
     *  1X,'INTERCHANGE BIKE SPEED             =',F8.3,' (BIKESPD)'/
     *  1X,'UNION STATION NUMBER (METROLINK)   =',I8,' (UNIONSTA)'/)
      IF(TAXIMODE.OR.UBERMODE) THEN
      WRITE(26,8049) ((TXCOEF(K6,K5),K5=1,2),K6=1,6),
     *               ((UBCOEF(K6,K5),K5=1,2),K6=1,6)
 8049 FORMAT(/
     *  1X,'  TAXI AND TNC WAIT TIME DISTRIBUTION PARAMETERS'/
     *  1X,'-------------------------------------------------'/
     *               '  TXCOEF(1,1-2) = ', 2(1X,F12.5)/
     *               '  TXCOEF(2,1-2) = ', 2(1X,F12.5)/
     *               '  TXCOEF(3,1-2) = ', 2(1X,F12.5)/
     *               '  TXCOEF(4,1-2) = ', 2(1X,F12.5)/
     *               '  TXCOEF(5,1-2) = ', 2(1X,F12.5)/
     *               '  TXCOEF(6,1-2) = ', 2(1X,F12.5)/
     *               '  UBCOEF(1,1-2) = ', 2(1X,F12.5)/
     *               '  UBCOEF(2,1-2) = ', 2(1X,F12.5)/
     *               '  UBCOEF(3,1-2) = ', 2(1X,F12.5)/
     *               '  UBCOEF(4,1-2) = ', 2(1X,F12.5)/
     *               '  UBCOEF(5,1-2) = ', 2(1X,F12.5)/
     *               '  UBCOEF(6,1-2) = ', 2(1X,F12.5)/)
      END IF
      IF(NCAPAC.OR.LUNREL.OR.NUNREL.OR.LCROWD) THEN
      WRITE(26,8045) CLUNREL,CNUNREL,CNCAPAC,CLCROWD
 8045 FORMAT(/
     *  1X,'CAPACITY,CROWDING, RELIABILITY MODEL COEFFICIENTS'/
     *  1X,'-------------------------------------------------'//
     *  1X,'LINK UNRELIABILITY                 =',F8.5,' (CLUNREL)'/
     *  1X,'NODE UNRELIABILITY                 =',F8.5,' (CNUNREL)'/
     *  1X,'NODE CAPACITY                      =',F8.5,' (CNCAPAC)'/
     *  1X,'LINK CROWDING                      =',F8.5,' (CLCROWD)'/)
      END IF
      WRITE(26,8048) LSUM1TRN,LSUM2CR,LSUM3CW,LSUM3CBK,LSUM3CB,LSUM3CP,
     *               LSUM3CK,LSUM2UR,LSUM3UW,LSUM3UBK,LSUM3UB,
     *               LSUM3UP,LSUM3UK,LSUM2BR,LSUM3BRW,LSUM3BRBK,
     *               LSUM3BRB,LSUM3BRP,LSUM3BRK,LSUM2RB,LSUM2TW,
     *               LSUM2EB,LSUM2LB
 8048 FORMAT(/
     *  1X,'TRANSIT LOGSUM COEFFICIENTS'/
     *  1X,'---------------------------'//
     *  1X,'TRANSIT            LOGSUM          =',F8.5,' (LSUM1TRN)'/
     *  1X,'COMMUTER RAIL      LOGSUM          =',F8.5,' (LSUM2CR)'/
     *  1X,'COMMUTER RAIL WALK LOGSUM          =',F8.5,' (LSUM3CW)'/
     *  1X,'COMMUTER RAIL BIKE LOGSUM          =',F8.5,' (LSUM3CBK)'/
     *  1X,'COMMUTER RAIL BUS  LOGSUM          =',F8.5,' (LSUM3CB)'/
     *  1X,'COMMUTER RAIL PNR  LOGSUM          =',F8.5,' (LSUM3CP)'/
     *  1X,'COMMUTER RAIL KNR  LOGSUM          =',F8.5,' (LSUM3CK)'/
     *  1X,'URBAN    RAIL      LOBSUM          =',F8.5,' (LSUM2UR)'/
     *  1X,'URBAN    RAIL WALK LOGSUM          =',F8.5,' (LSUM3UW)'/
     *  1X,'URBAN    RAIL BIKE LOGSUM          =',F8.5,' (LSUM3UBK)'/
     *  1X,'URBAN    RAIL BUS  LOGSUM          =',F8.5,' (LSUM3UB)'/
     *  1X,'URBAN    RAIL PNR  LOGSUM          =',F8.5,' (LSUM3UP)'/
     *  1X,'URBAN    RAIL KNR  LOGSUM          =',F8.5,' (LSUM3UK)'/
     *  1X,'BUS RAPID TRN      LOGSUM          =',F8.5,' (LSUM2BR)'/
     *  1X,'BUS RAPID TRN WALK LOGSUM          =',F8.5,' (LSUM3BRW)'/
     *  1X,'BUS RAPID TRN BIKE LOGSUM          =',F8.5,' (LSUM3BRBK)'/
     *  1X,'BUS RAPID TRN BUS  LOGSUM          =',F8.5,' (LSUM3BRB)'/
     *  1X,'BUS RAPID TRN PNR  LOGSUM          =',F8.5,' (LSUM3BRP)'/
     *  1X,'BUS RAPID TRN KNR  LOGSUM          =',F8.5,' (LSUM3BRK)'/
     *  1X,'RAPID BUS          LOGSUM          =',F8.5,' (LSUM2RB)'/
     *  1X,'TRANSITWAY         LOGSUM          =',F8.5,' (LSUM2TW)'/
     *  1X,'EXPRESS BUS        LOGSUM          =',F8.5,' (LSUM2EB)'/
     *  1X,'LOCAL BUS          LOGSUM          =',F8.5,' (LSUM2LB)'/)
C
C CHECK LOGSUM VALUES
C
      IF(LSUM1TRN.LE.0.001) LSUMERR=.TRUE.
      IF(LSUM2CR.LE.0.001)  LSUMERR=.TRUE.
      IF(LSUM3CW.LE.0.001)  LSUMERR=.TRUE.
      IF(LSUM3CBK.LE.0.001) LSUMERR=.TRUE.
      IF(LSUM3CB.LE.0.001)  LSUMERR=.TRUE.
      IF(LSUM3CP.LE.0.001)  LSUMERR=.TRUE.
      IF(LSUM3CK.LE.0.001)  LSUMERR=.TRUE.
      IF(LSUM2UR.LE.0.001)  LSUMERR=.TRUE.
      IF(LSUM3UW.LE.0.001)  LSUMERR=.TRUE.
      IF(LSUM3UBK.LE.0.001) LSUMERR=.TRUE.
      IF(LSUM3UB.LE.0.001)  LSUMERR=.TRUE.
      IF(LSUM3UP.LE.0.001)  LSUMERR=.TRUE.
      IF(LSUM3UK.LE.0.001)  LSUMERR=.TRUE.
      IF(LSUM2BR.LE.0.001)  LSUMERR=.TRUE.
      IF(LSUM3BRW.LE.0.001) LSUMERR=.TRUE.
      IF(LSUM3BRBK.LE.0.001)LSUMERR=.TRUE.
      IF(LSUM3BRB.LE.0.001) LSUMERR=.TRUE.
      IF(LSUM3BRP.LE.0.001) LSUMERR=.TRUE.
      IF(LSUM3BRK.LE.0.001) LSUMERR=.TRUE.
      IF(LSUM2RB.LE.0.001)  LSUMERR=.TRUE.
      IF(LSUM2TW.LE.0.001)  LSUMERR=.TRUE.
      IF(LSUM2EB.LE.0.001)  LSUMERR=.TRUE.
      IF(LSUM2LB.LE.0.001)  LSUMERR=.TRUE.
      IF(LSUMERR) THEN
      WRITE(26,8149)
      WRITE(*,8149)
 8149 FORMAT(//' ONE OR MORE OF THE LOGSUM VALUES ARE ZERO'//)
      STOP 8149
      END IF
C
      WRITE(26,8148) KRED,KBLUE,KGREEN,KGOLD,KEXPO,
     *               KWSAB,KESFV,KSEPV,KAIRCN,KVERM
 8148 FORMAT(/1X,'URBAN RAIL LINE SPECIFIC CONSTANTS'/
     *       1X,'----------------------------------'/
     *       1X,'RED/PURPLE LINE               =',F8.5,' KRED'/
     *       1X,'BLUE LINE                     =',F8.5,' KBLUE'/
     *       1X,'GREEN LINE                    =',F8.5,' KGREEN'/
     *       1X,'GOLD LINE                     =',F8.5,' KGOLD'/
     *       1X,'EXPO LINE                     =',F8.5,' KEXPO'/
     *       1X,'WEST SANTA ANA BRANCH LINE    =',F8.5,' KWSAB'/
     *       1X,'EAST SAN FERNANDO VALLEY LINE =',F8.5,' KESFV'/
     *       1X,'SEPULVEDA LINE                =',F8.5,' KSEPV'/
     *       1X,'AIRPORT CONNECTOR LINE        =',F8.5,' KAIRCN'/
     *       1X,'VERMONT LINE                  =',F8.5,' KVERM'/)
      IF(AIRPASS) WRITE(26,8044) ACOEF,RNTWAIT,LAXOCC,ACNST,AWUR(2),
     *            ITFZONE,ITFZONE2,ITFONC,ITFHTL,ITFFLY,ITFPRK,
     *            ITFTXI,ITFLMO,ITFDRP,CONRAC,
     *            ITFSAVFLY,ITFSAVONC,ITFSAVPRK,
     *            ITFSAVAPM,(ITFPEN(1,K),K=1,3),
     *            (ITFPEN(2,K1),K1=1,3)
 8044 FORMAT(1X,'AIR PASSENGER MODEL COEFFICIENTS --'/
     *       1X,'--------------------------------'//
     *  1X,'IN-VEHICLE COEFFICIENT             =',F8.5,' ACOEF(1)'/
     *  1X,'OUT-OF-VEHICLE COEFFICIENT         =',F8.5,' ACOEF(2)'/
     *  1X,'COST COEFFICIENT                   =',F8.5,' ACOEF(3)'/
     *  1X,'RENTAL CAR AUTO OPERATING COST/MI  =',F8.5,' ACOEF(4)'/
     *  1X,'RENTAL CAR FACILITY WAIT TIME      =',F8.5,' RNTWAIT'/
     *  1X,'LAX PARKING LOT AUTO OCCUPANCY     =',F8.5,' LAXOCC'//
     *  1X,'TAXI CONSTANT                      =',F8.4,' ACNST(1)'/
     *  1X,'RENTAL CONSTANT                    =',F8.4,' ACNST(2)'/
     *  1X,'LIMO/TOWN CAR CONSTANT             =',F8.4,' ACNST(3)'/
     *  1X,'PRIVATE VEHICLE DROP-OFF CONSTANT  =',F8.4,' ACNST(4)'/
     *  1X,'PRIVATE VEHICLE PARKED CONSTANT    =',F8.4,' ACNST(5)'/
     *  1X,'ON-CALL SHUTTLE/VAN CONSTANT       =',F8.4,' ACNST(6)'/
     *  1X,'PUBLIC TRANSIT CONSTANT            =',F8.4,' ACNST(7)'/
     *  1X,'FLY-AWAY CONSTANT                  =',F8.4,' ACNST(8)'/
     *  1X,'PUBLIC SERVICES CONSTANT           =',F8.4,' ACNST(9)'/
     *  1X,'URBAN RAIL WALK ACCESS CONSTANT    =',F8.4,' AWUR'//
     *  1X,'ITF ZONE                           =',I8,' ITFZONE'/
     *  1X,'ITF ZONE 2                         =',I8,' ITFZONE2'/
     *  1X,'ITF RESTRICTION - ON CALL          =',I8,' ITFONC'/
     *  1X,'ITF LOCATION FOR HOTELS            =',I8,' ITFHTL'/
     *  1X,'ITF RESTRICTION - FLY AWAY         =',I8,' ITFFLY'/
     *  1X,'ITF RESTRICTION - PARKING          =',7X,L1,' ITFPRK'/
     *  1X,'ITF EGRESS OPTION - TAXI           =',7X,L1,' ITFTXI'/
     *  1X,'ITF EGRESS OPTION - LIMO           =',7X,L1,' ITFLMO'/
     *  1X,'ITF EGRESS OPTION - DROP OFF       =',7X,L1,' ITFDRP'/
     *  1X,'APM CONNECTION FOR CONRAC          =',7X,L1,' CONRAC'/
     *  1X,'ITF CHECKIN SAVINGS - FLY-AWAY     =',F8.2,' ITFSAVFLY'/
     *  1X,'ITF CHECKIN SAVINGS - ON-CALL      =',F8.2,' ITFSAVONC'/
     *  1X,'ITF CHECKIN SAVIGNS - PARKING LOT  =',F8.2,' ITFSAVPRK'/
     *  1X,'ITF CHECKIN SAVINGS - TRANSIT      =',F8.2,' ITFSAVAPM'/
     *  1X,'ITF 1 TAXI PENALTY VALUE           =',F8.2,' ITFPEN(1,1)'/
     *  1X,'ITF 1 LIMO PENALTY VALUE           =',F8.2,' ITFPEN(1,2)'/
     *  1X,'ITF 1 DROP OFF PENALTY VALUE       =',F8.2,' ITFPEN(1,3)'/
     *  1X,'ITF 2 TAXI PENALTY VALUE           =',F8.2,' ITFPEN(2,1)'/
     *  1X,'ITF 2 LIMO PENALTY VALUE           =',F8.2,' ITFPEN(2,2)'/
     *  1X,'ITF 2 DROP OFF PENALTY VALUE       =',F8.2,
     *     ' ITFPEN(2,3)'//)
      IF(NHBDIR) WRITE(26,8046) NHBCOEF
 8046 FORMAT(1X,'NON-HOME BASED DIRECT DEMAND MODEL COEFFICIENTS --'/
     *       1X,'--------------------------------------------------'/
     *       1X,'STATION-TO-STATION UTILITY COEFFICIENTS'//
     *  1X,'IN-VEHICLE COEFFICIENT             =',F8.5,' NHBCOEF(1)'/
     *  1X,'FARE                               =',F8.5,' NHBCOEF(2)'/
     *  1X,'FIRST WAIT - PART A                =',F8.5,' NHBCOEF(3)'/
     *  1X,'FIRST WAIT - PART B                =',F8.5,' NHBCOEF(4)'/
     *  1X,'TRANSFER WAIT TIME                 =',F8.5,' NHBCOEF(5)'/
     *  1X,'NUMBER OF TRANSFERS                =',F8.5,' NHBCOEF(6)'/
     *  1X,'WALK TIME                          =',F8.5,' NHBCOEF(7)'//)
      IF(BICYCLE) WRITE(26,8047) BZCOEF,BSTCOEF,BSV
 8047 FORMAT(1X,'ZONAL LEVEL BICYCLE MODEL COEFFICIENTS --'/
     *       1X,'-----------------------------------------'/
     *  1X,'LOGSUM                             =',F8.5,' BZCOEF(1)'/
     *  1X,'DISTANCE                           =',F8.5,' BZCOEF(2)'/
     *  1X,'LN INTENSITY - PRODUCTION          =',F8.5,' BZCOEF(3)'/
     *  1X,'URBAN FABRIC - PRODUCTION          =',F8.5,' BZCOEF(4)'/
     *  1X,'% LAND AREA EDUCATION - PRODUCTION =',F8.5,' BZCOEF(5)'/
     *  1X,'% 0-CAR HOUSEHOLDS - PRODUCTION    =',F8.5,' BZCOEF(6)'/
     *  1X,'LN INTENSITY - ATTRACTION          =',F8.5,' BZCOEF(7)'/
     *  1X,'URBAN FABRIC - ATTRACTION          =',F8.5,' BZCOEF(8)'/
     *  1X,'% LAND AREA EDUCATION - ATTRACTION =',F8.5,' BZCOEF(9)'/
     *  1X,'% BIKE PARKING - RACKS - ATTR.     =',F8.5,' BZCOEF(10)'/
     *  1X,'% BIKE PARKING - LOCKERS - ATTR.   =',F8.5,' BZCOEF(11)'/
     *  1X,'% BIKE PARKING - BICEBERG - ATTR.  =',F8.5,' BZCOEF(12)'/
     *  1X,'% BIKE PARKING - ROOMS - ATTR.     =',F8.5,' BZCOEF(13)'/
     *  1X,'% BIKE PARKING - SHOWERS - ATTR.   =',F8.5,' BZCOEF(14)'/
     *  1X,'% BIKE SHARE PROGRAM               =',F8.5,' BZCOEF(15)'//
     *  1X,'STATION LEVEL BICYCLE MODEL COEFFICIENTS --'/
     *       1X,'-------------------------------------------'/
     *  1X,'ACCESS LOGSUM                      =',F8.5,' BSTCOEF(1)'/
     *  1X,'EGRESS LOGSUM                      =',F8.5,' BSTCOEF(2)'/
     *  1X,'LN INTENSITY - PRODUCTION          =',F8.5,' BSTCOEF(3)'/
     *  1X,'URBAN FABRIC - PRODUCTION          =',F8.5,' BSTCOEF(4)'/
     *  1X,'% LAND AREA EDUCATION - PRODUCTION =',F8.5,' BSTCOEF(5)'/
     *  1X,'% 0-CAR HOUSEHOLDS - PRODUCTION    =',F8.5,' BSTCOEF(6)'/
     *  1X,'LN INTENSITY - ATTRACTION          =',F8.5,' BSTCOEF(7)'/
     *  1X,'URBAN FABRIC - ATTRACTION          =',F8.5,' BSTCOEF(8)'/
     *  1X,'% LAND AREA EDUCATION - ATTRACTION =',F8.5,' BSTCOEF(9)'/
     *  1X,'% BIKE PARKING - RACKS - ATTR.     =',F8.5,' BSTCOEF(10)'/
     *  1X,'% BIKE PARKING - LOCKERS - ATTR.   =',F8.5,' BSTCOEF(11)'/
     *  1X,'% BIKE PARKING - BICEBERG - ATTR.  =',F8.5,' BSTCOEF(12)'/
     *  1X,'% BIKE PARKING - ROOMS - ATTR.     =',F8.5,' BSTCOEF(13)'/
     *  1X,'% BIKE PARKING - SHOWERS - ATTR.   =',F8.5,' BSTCOEF(14)'/
     *  1X,'% BIKE PARKING - RACKS - ACCESS    =',F8.5,' BSTCOEF(15)'/
     *  1X,'% BIKE PARKING - LOCKERS - ACCESS  =',F8.5,' BSTCOEF(16)'/
     *  1X,'% BIKE PARKING - BICEBERG - ACCESS =',F8.5,' BSTCOEF(17)'/
     *  1X,'% BIKE PARKING - ROOMS - ACCESS    =',F8.5,' BSTCOEF(18)'/
     *  1X,'% BIKE PARKING - SHOWERS - ACCESS  =',F8.5,' BSTCOEF(19)'/
     *  1X,'% BIKE SHARE PROGRAM - ORGIN/ACC   =',F8.5,' BSTCOEF(20)'/
     *  1X,'% BIKE SHARE PROGRAM - DEST/EGR    =',F8.5,' BSTCOEF(21)'//
     *  1X,'REGIONAL BIKE SHARE PROGRAM VALUE  =',F8.5,' BSV'//)
C
      WRITE(26,8022) DEBUG,CALIB,CAPRES,EXPCAP,ZEROCAR,
     *               NEGSHP,NOTXFR,BUSTEXP,
     *               LOGSUM,USERBEN,TRIPSOUT,
     *               NMOT,STACAR,PEAK,
     *               AIRPASS,HFACT,STACODE,
     *               LAX,EMPPRK,TRNLOT,APMIND,LUNREL,NUNREL,NCAPAC,
     *               LCROWD,SPEVENT,NHBDIR,BICYCLE,WALKTIME,TRNEGR,
     *               NOTWYAE,LOWRAIL,TAXIMODE,UBERMODE,UBERTRN,AVMDL,
     *               ALTPARK,AVSKIM,ESCOOTER
 8022 FORMAT(1X,'BASIC PROGRAM OPTIONS'/
     *       1X,'------------------------'//
     *       1X,'DEBUG SWITCH                 =',7X,L1,' (DEBUG)'/
     *       1X,'CALIBRATION MODE             =',7X,L1,' (CALIB)'/
     *       1X,'CAPACITY RESTRAINT           =',7X,L1,' (CAPRES)'/
     *       1X,'CAPACITY RESTRAINT - EXP BUS =',7X,L1,' (EXPCAP)'/
     *       1X,'PROHIBIT DRIVE ALONE - 0-CAR =',7X,L1,' (ZEROCAR)'/
     *       1X,'ALLOW NEGATIVE SHADOW PRICES =',7X,L1,' (NEGSHP)'/
     *       1X,'NO BUS TRANSFERS ALLOWED     =',7X,L1,' (NOTXFR)'/
     *       1X,'EXAMINE TRANSFER TO EXP BUS  =',7X,L1,' (BUSTEXP)'/
     *       1X,'OUTPUT LOGSUM VALUES         =',7X,L1,' (LOGSUM)'/
     *       1X,'OUTPUT FTA USER BENEFIT FILE =',7X,L1,' (USERBEN)'/
     *       1X,'OUTPUT TRIP TABLES           =',7X,L1,' (TRIPSOUT)'/
     *       1X,'ALLOW NON-MOTORIZED TRAVEL   =',7X,L1,' (NMOT)'/
     *       1X,'CONSIDER CR AUTO EGRESS      =',7X,L1,' (STACAR)'/
     *       1X,'PEAK/OFF PEAK INDICATOR      =',7X,L1,' (PEAK)'/
     *       1X,'APPLY AIR PASSENGER MODEL    =',7X,L1,' (AIRPASS)'/
     *       1X,'APPLY HOUSEHOLD FACTORS      =',7X,L1,' (HFACT)'/
     *       1X,'USE AWALK AND BWALK          =',7X,L1,' (STACODE)'/
     *       1X,'APPLY PARKING LOT CHOICE     =',7X,L1,' (LAX)'/
     *       1X,'LAX EMPLOYEE PARKING ONLY    =',7X,L1,' (EMPPRK)'/
     *       1X,'LAX PARKING LOT TRANSIT ONLY =',7X,L1,' (TRNLOT)'/
     *       1X,'LAX APM INDICATOR MATRIX     =',7X,L1,' (APMIND)'/
     *       1X,'LINK UNRELIABILITY           =',7X,L1,' (LUNREL)'/
     *       1X,'NODE UNRELIABILITY           =',7X,L1,' (NUNREL)'/
     *       1X,'CAPACITY PENALTY             =',7X,L1,' (NCAPAC)'/
     *       1X,'CROWDING PENALTY             =',7X,L1,' (LCROWD)'/
     *       1X,'APPLY SPECIAL EVENT MODEL    =',7X,L1,' (SPEVENT)'/
     *       1X,'APPLY NNB DIRECT DEMAND MODEL=',7X,L1,' (NHBDIR)'/
     *       1X,'BICYCLE MODE                 =',7X,L1,' (BICYCLE)'/
     *       1X,'ZONE TO STATION WALK TIME    =',7X,L1,' (WALKTIME)'/
     *       1X,'TRANSIT EGRESS CHOICE        =',7X,L1,' (TRNEGR)'/
     *       1X,'NO TRANSITWAY ACCESS/EGRESS  =',7X,L1,' (NOTWYAE)'/
     *       1X,'OUTPUT LOW INCOME STS TRIPS  =',7X,L1,' (LOWRAIL)'/
     *       1X,'TAXI MODE IN MAAS            =',7X,L1,' (TAXIMODE)'/          !Innovation
     *       1X,'TNC  MODE IN MAAS            =',7X,L1,' (UBERMODE)'/          !Innovation
     *       1X,'TNC  MODE IN TRANSIT         =',7X,L1,' (UBERTRN)'/           !Innovation
     *       1X,'AUTOMATED VEHICLE MODEL      =',7X,L1,' (AVMDL)'/             !Innovation
     *       1X,'ALTERNATIVE PARKING MODEL    =',7X,L1,' (ALTPARK)'/           !Innovation
     *       1X,'READ AV HIGHWAY SKIMS        =',7X,L1,' (AVSKIM)'/            !Innovation
     *       1X,'E-SCOOTER MODE AVAILABILITY  =',7X,L1,' (ESCOOTER)'//)        !Innovation
      WRITE(26,8055) 	PDRV0,PDRV2,PDRV4
 8055 FORMAT(1X,'INTRAZONAL PROPORTIONS'/
     *       1X,'----------------------'/
     *       1X,'DRIVE ALONE   PROPORTION ( PDRV0)=',5(1X,F6.3),
     *             ' OF AUTO PERSON TRIPS'/
     *       1X,'2 PERSON AUTO PROPORTION ( PDRV2)=',5(1X,F6.3),
     *             ' OF SHARED RIDE TRIPS'/
     *       1X,'4 PERSON AUTO PROPORTION ( PDRV4)=',5(1X,F6.3),
     *             ' OF 3/4 PERSON AUTO TRIPS'//)
      IF(ZEROCAR.AND.PDRV0(1).GT.0.0) THEN
      WRITE(26,8056) PDRV0(1)
      WRITE(*,8056) PDRV0(1)
 8056 FORMAT(/' RCTL 8056 (F) PDRV0 FOR SEGMENT 1 (',F6.3,
     *       ') MUST BE ZERO'/)
      STOP 8056
      END IF
      IF (CALIB) THEN
      WRITE(26,8032) MAXCALIT,CLOSURE,MCOEF(5),COEFF(80)
 8032 FORMAT(1X,'CALIBRATION PARAMETERS'/
     *       1X,'----------------------'//
     *       1X,'MAXIMUM NUMBER OF ITERATIONS =',I8,' (MAXCALIT)'/
     *       1X,'CLOSURE CRITERIA             =',F8.5,' (CLOSURE)'/
     *       1X,'STATION LEVEL TRANSFER COEFF =',F8.5,' (MCOEF(5)'/
     *       1X,'URBAN RAIL TRANSFER COEFF    =',F8.5,' (COEFF(80)'/)
      WRITE(26,8033) CCODE
 8033 FORMAT(' CCODE( 1)=',L1,' KTRN,KSR,K3P,K4P,KNMOT,KBIKE',
     *                        ' (TRANSIT,SHARED RIDE,3-PERSON,',
     *                        '4-PERSON,NON-MOTORIZED,BIKE)'/
     *  ' CCODE( 2)=',L1,' KDTRN  (DRIVE ACCESS TO TRANSIT'/
     *  ' CCODE( 3)=',L1,' NOT USED'/
     *  ' CCODE( 4)=',L1,' KCR    (COMMUTER RAIL)'/
     *  ' CCODE( 5)=',L1,' KUR    (URBAN RAIL)'/
     *  ' CCODE( 6)=',L1,' KEBUS  (EXPRESS BUS)'/
     *  ' CCODE( 7)=',L1,' KWAY   (TRANSITWAY)'/
     *  ' CCODE( 8)=',L1,' KBKCR  (BIKE TO COMMUTER RAIL)'/
     *  ' CCODE( 9)=',L1,' KBKUR  (BIKE TO URBAN RAIL)'/
     *  ' CCODE(10)=',L1,' KBCR   (BUS TO COMMUTER RAIL)'/
     *  ' CCODE(11)=',L1,' KBUR   (BUS TO URBAN RAIL)'/
     *  ' CCODE(12)=',L1,' KBRPDW (BUS TRANSFER TO RAPID WALK)'/
     *  ' CCODE(13)=',L1,' KBTWYW (BUS TRANSFER TO WALK TRANSITWAY)'/
     *  ' CCODE(14)=',L1,' KBEXPW (BUS TRANSFER TO WALK EXPRESS)'/
     *  ' CCODE(15)=',L1,' KINFL  (INFORMAL DRIVE ACCESS TO LOCAL)'/
     *  ' CCODE(16)=',L1,' KINFR  (INFORMAL DRIVE ACCESS TO RAPID)'/
     *  ' CCODE(17)=',L1,' KTRNT  (TRANSIT CONSTANT BY DISTANCE)'/
     *  ' CCODE(18)=',L1,' KBRPDB (BUS TRANSFER TO BIKE RAPID)'/
     *  ' CCODE(19)=',L1,' KBRT   (BRT)'/
     *  ' CCODE(20)=',L1,' KRBUS  (RAPID BUS)'/
     *  ' CCODE(21)=',L1,' KBBRTW (BUS TRANSFER TO WALK BRT)'/
     *  ' CCODE(22)=',L1,' KBRPDD (BUS TRANSFER TO DRIVE RAPID)'/
     *  ' CCODE(23)=',L1,' KBBRTD (BUS TRANSFER TO DRIVE BRT)'/
     *  ' CCODE(24)=',L1,' CCBD   (CBD CONSTANT)'/
     *  ' CCODE(25)=',L1,' KBTWYD (BUS TRANSFER TO DRIVE TRANSITWAY)'/
     *  ' CCODE(26)=',L1,' KBEXPD (BUS TRANSFER TO DRIVE EXPRESS)'/
     *  ' CCODE(27)=',L1,' KBBUS  (BIKE TO LOCAL BUS)'/
     *  ' CCODE(28)=',L1,' KBRPD  (BIKE TO RAPID BUS)'/
     *  ' CCODE(29)=',L1,' KBWAY  (BIKE TO TRANSITWAY)'/
     *  ' CCODE(30)=',L1,' KBEXP  (BIKE TO EXPRESS)'/
     *  ' CCODE(31)=',L1,' KBBRT  (BIKE TO BRT)'/
     *  ' CCODE(32)=',L1,' KBTWYB (BUS TRANSFERS TO BIKE TRANSITWAY)'/
     *  ' CCODE(33)=',L1,' KBEXPB (BUS TRANSFERS TO BIKE EXPRESS)'/
     *  ' CCODE(34)=',L1,' KBBRTB (BUS TRANSFERS TO BRT)'/
     *  ' CCODE(35)=',L1,' KINTRAWK (INTRAZONAL WALK)'/
     *  ' CCODE(36)=',L1,' KINTRABK (INTRAZONAL BIKE)'/
     *  ' CCODE(37)=',L1,' KPUR (PNR TO URBAN RAIL)'/
     *  ' CCODE(38)=',L1,' KWCR (WALK TO COMMUTER RAIL)'/
     *  ' CCODE(39)=',L1,' KPNR (PNR TO COMMUTER & URBAN'/
     *  ' CCODE(40)=',L1,' KWUR (WALK TO URBAN RAIL)'/
     *  ' CCODE(41)=',L1,' KWWAY (WALK TO TRANSITWAY'/
     *  ' CCODE(42)=',L1,' DRIVE TO EXPRESS BUS (KDEXP)'/
     *  ' CCODE(43)=',L1,' KPCR (PNR TO COMMUTER RAIL)'/
     *  ' CCODE(44)=',L1,' KDA(5) (DRIVE ALONE SEGMENT 5)'/
     *  ' CCODE(45)=',L1,' KAUT(1) (AUTO CONSTANT SEGMENT 1)'/
     *  ' CCODE(46)=',L1,' COEFF(31) (WALK TO LOCAL BUS TRANSFERS)'/
     *  ' CCODE(47)=',L1,' COEFF(32) (BIKE TO LOCAL BUS TRANSFERS)'/
     *  ' CCODE(48)=',L1,' KWBUS (WALK TO LOCAL BUS)'/
     *  ' CCODE(49)=',L1,' COEFF(33) (DRIVE TO LOCAL BUS TRANSFERS)'/
     *  ' CCODE(50)=',L1,' COEFF(34) (WALK TO RAPID BUS TRANSFERS)'/
     *  ' CCODE(51)=',L1,' COEFF(35) (BIKE TO RAPID BUS TRANSFERS)'/
     *  ' CCODE(52)=',L1,' COEFF(36) (DRIVE TO RAPID BUS TRANSFERS)'/
     *  ' CCODE(53)=',L1,' COEFF(37) (WALK TO EXPRESS BUS TRANSFERS)'/
     *  ' CCODE(54)=',L1,' COEFF(38) (BIKE TO EXPRESS BUS TRANSFERS)'/
     *  ' CCODE(55)=',L1,' COEFF(39) (DRIVE TO EXPRESS BUS TRANSFERS)'/
     *  ' CCODE(56)=',L1,' COEFF(40) (WALK TO TWAY BUS TRANSFERS)'/
     *  ' CCODE(57)=',L1,' COEFF(41) (BIKE TO TWAY BUS TRANSFERS)'/
     *  ' CCODE(58)=',L1,' COEFF(42) (DRIVE TO TWAY BUS TRANSFERS)'/
     *  ' CCODE(59)=',L1,' COEFF(43) (WALK TO BRT BUS TRANSFERS)'/
     *  ' CCODE(60)=',L1,' COEFF(44) (BIKE TO BRT BUS TRANSFERS)'/
     *  ' CCODE(61)=',L1,' COEFF(45) (DRIVE TO BRT BUS TRANSFERS)'/
     *  ' CCODE(62)=',L1,' COEFF(46) (COMMUTER RAIL BUS EGRESS)'/
     *  ' CCODE(63)=',L1,' COEFF(47) (URBAN RAIL BUS EGRESS)'/
     *  ' CCODE(64)=',L1,' KWCR(1) (WALK TO COMMUTER RAIL)'/
     *  ' CCODE(65)=',L1,' KWUR(1) (WALK TO URBAN RAIL)'/
     *  ' CCODE(66)=',L1,' KWBR(1) (WALK TO BRT - SEG 1)'/
     *  ' CCODE(67)=',L1,' KSTAMODE (MODE-TO-MODE TRANSFER)'/
     *  ' CCODE(68)=',L1,' KRBUS(1) (RAPID BUS)'/
     *  ' CCODE(69)=',L1,' KCR(1)   (COMMUTER RAIL - SEG#1)'/
     *  ' CCODE(70)=',L1,' KUR(1)   (URBAN RAIL - SEG#1)'/
     *  ' CCODE(71)=',L1,' KBRTEGR  (BRT EGRESS BUS CONSTANT'/
     *  ' CCODE(72)=',L1,' KKBR     (KNR TO BRT CONSTANT)'/
     *  ' CCODE(73)=',L1,' KBBR     (BUS TO BRT)'/
     *  ' CCODE(74)=',L1,' CCBD(1)  (COMMUTER RAIL CBD)'/
     *  ' CCODE(75)=',L1,' KUR(2-3) (URBAN RAIL - SEG#2,3)'/)
      END IF 
C
C CHECK CAPACITY RESTRAINT ITERATION LOGIC
C
      IF((.NOT.CAPRES).AND.NITER.GT.0) NITER=0
C
C CHECK SHORT WALK/LONG WALK VALUE SETTINGS
C
      IF(SWALK.LE.0.OR.LWALK.LE.0) THEN
      WRITE(26,8039) SWALK,LWALK
 8039 FORMAT(1X,'RCTL 8039 (F) EITHER THE SHORT (',F6.2,
     * ') OR LONG (',F6.2,') WALK',
     * ' MAXIMUM TIME VALUES ARE ZERO')
      STOP 16
      END IF
C
C CHECK CPI FACTOR INPUTS
C
      IF(PCPI.LE.0) THEN
      WRITE(26,8040)
 8040 FORMAT(/1X,'RCTL 8040 (W) PARKING COST CPI FACTOR EQUAL TO 0,'
     *          ' RESET TO 1.0'/)
      PCPI=1.0
      END IF
      IF(FCPI.LE.0) THEN
      WRITE(26,8041)
 8041 FORMAT(/1X,'RCTL 8041 (W) TRANSIT FARE CPI FACTOR EQUAL TO 0,'
     *          ' RESET TO 1.0'/)
      FCPI=1.0
      END IF
C
C     CHECK ITF DEFINITION LOGIC
C
      IF(LAX) THEN
      IF(ITFONC.EQ.1.AND.ITFZONE.LE.0) THEN
      WRITE(26,8043) ITFONC,ITFZONE
      WRITE(*,8043) ITFONC,ITFZONE
 8043 FORMAT(/' ITFONC=',I1,' ITFZONE=',I4)
      STOP 8043
      END IF
      IF(ITFONC.EQ.2.AND.ITFZONE2.LE.0) THEN
      WRITE(26,8147) ITFONC,ITFZONE2
      WRITE(*,8147)  ITFONC,ITFZONE2
 8147 FORMAT(/' ITFONC=',I1,' ITFZONE2=',I4)
      STOP 8147
      END IF
      END IF
C
C STORE COEFFICIENT VALUES
C
      DO II=1,10
      COEFF(II)=MCOEF(II)
      END DO
      DO K=1,10
      KK=K+10
	    KL=K+20
      COEFF(KK)=VCOEF(K)
	    COEFF(KL)=VCOEF(K)
      END DO
      CCRPNR=CCRPNR/(LSUM1TRN*LSUM2CR*LSUM3CP)
      CURPNR=CURPNR/(LSUM1TRN*LSUM2UR*LSUM3UP)
      CRDIST=CRDIST/LSUM1TRN
      CDAR=CDAR*(LSUM1TRN*LSUM2UR)
      COEFF(98)=CSPACES/(LSUMT*LSUMA*LSUMS)
      COEFF(100)=VCOEF(1)
      IF(COEFF(61).EQ.0.0) COEFF(61)=COEFF(51)
      IF(COEFF(62).EQ.0.0) COEFF(62)=COEFF(52)
      IF(COEFF(63).EQ.0.0) COEFF(63)=COEFF(53)
      IF(COEFF(64).EQ.0.0) COEFF(64)=COEFF(54)
      IF(COEFF(65).EQ.0.0) COEFF(65)=COEFF(55)
C
C CAPACITY, CROWDING & RELIABILITY COEFFICIENTS
C
      COEFF(75)=CLUNREL
      COEFF(76)=CNUNREL
      COEFF(77)=CLCROWD
      COEFF(78)=CNCAPAC
C
C RESET WALK ACCESS COEFFICIENT IF NOT BICYCLE
C
      IF(WALKTIME) NMLSUM=COEFF(17)
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,8026)
 8026 FORMAT(/' COMPUTED COEFFICIENT VALUES'/
     *       ' ---------------------------'/)
      DO 21 K=1,100
      IF(COEFF(K).NE.0.0) WRITE(26,8027) K,COEFF(K)
 8027 FORMAT(1X,'COEFF(',I3,')=',E12.5E2)
   21 CONTINUE
      END IF
C.....................................................................
C
C OPEN CONTROL FILE, AND REPORT OUTPUT FILE
C
C
C OPEN STATION FILE
C
      INQUIRE (FILE=STATDATA,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9315) STATDATA
 9315 FORMAT(1X,'RCTL 9315 (F) STATION INPUT FILE STATDATA=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9315) STATDATA
      STOP 8
      ELSE
      OPEN(13,FILE=STATDATA,
     *        STATUS='OLD',FORM='FORMATTED')
      END IF
C
C OPEN ZONAL SE STANDARD DATA FILE
C
      INQUIRE (FILE=ZFSTD,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9015) ZFSUP
 9015 FORMAT(1X,'RCTL 9015 (F) HOUSEHOLD INPUT FILE ZFSTD=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9015) ZFSTD
      STOP 8
      ELSE
      OPEN(9,FILE=ZFSTD,STATUS='OLD',FORM='FORMATTED')
      END IF
C
C OPEN AREA TYPE INPUT FILE
C
      IF(TAXIMODE.OR.UBERMODE) THEN
      INQUIRE (FILE=AREATYPE,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9645) AREATYPE
 9645 FORMAT(1X,'RCTL 9645 (F) AREATYPE INPUT FILE AREATYPE=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9645) AREATYPE
      STOP 8
      ELSE
      OPEN(138,FILE=AREATYPE,STATUS='OLD',FORM='FORMATTED')
      END IF
      END IF
C
C CHECK TAXI WAIT TIME COEFFICIENTS
C
      IF(TAXIMODE) THEN
      DO K=1,6
      DO K1=1,2
      IF(TXCOEF(K,K1).EQ.0.0) THEN
      WRITE(26,9030) K,K1
      WRITE(*,9030) K,K1
 9030 FORMAT(' RCTL 9030 (F) MISSING COEFFICIENT VALUE FOR ',
     *       ' TXCOEF(',I1,',',I1,')')
      TXERR=.TRUE.
      END IF
      END DO
      END DO
      IF(TXERR) STOP 9030
      END IF
C
C CHECK UBER WAIT TIME COEFFICIENTS
C
      IF(UBERMODE.OR.UBERTRN) THEN
      DO K=1,6
      DO K1=1,2
      IF(UBCOEF(K,K1).EQ.0.0) THEN
      WRITE(26,9031) K,K1
      WRITE(*,9031) K,K1
 9031 FORMAT(' RCTL 9031 (F) MISSING COEFFICIENT VALUE FOR ',
     *       ' UBCOEF(',I1,',',I1,')')
      TXERR=.TRUE.
      END IF
      END DO
      END DO
      IF(TXERR) STOP 9031
      END IF
C
C  OPEN AND STORE DISTRICT-EQUIVALENCE DATA IF PROVIDED
C
      INQUIRE(FILE=FDISTEQ,EXIST=EXISTS)
      IF(EXISTS) THEN
      OPEN(3,FILE=FDISTEQ,STATUS='OLD',FORM='FORMATTED')
 9616 READ(3,*,END=9618) ZONE,DIST
      IF(ZONE.GT.20) THEN
      WRITE(26,9621)
      WRITE(*,9621)
 9621 FORMAT(//' MAXIMUM ALLOWABLE DISTRICT NUMBER IS 20')
      STOP
      END IF
      IF(ZONE.LE.0) GO TO 9618
      DISTEQ(DIST(1))=ZONE
      DO 9619 K=2,6
      IF(DIST(K).EQ.0) GO TO 9616
      IF(DIST(K).LT.0) THEN
      K1=DIST(K-1)
      K2=IABS(DIST(K))
      DO 9620 K3=K1,K2
      DISTEQ(K3)=ZONE
 9620 CONTINUE
      ELSE
      DISTEQ(DIST(K))=ZONE
      END IF
 9619 CONTINUE
      GO TO 9616
 9618 CONTINUE
      ELSE
      WRITE(26,9622) FDISTEQ
      WRITE(*,9622) FDISTEQ
 9622 FORMAT(//1X,'RCTL 9622 (F) ZONE-DISTRICT EQUIVALENCE FILE',
     *          ' FDISTEQ=',A40,' DOES NOT EXIST')
      STOP 16
      END IF
      CLOSE(3,STATUS='KEEP')
C
C OPEN ZONAL SE SUPPLEMENTAL DATA FILE
C
      INQUIRE (FILE=ZFSUP,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9416) ZFSUP
 9416 FORMAT(1X,'RCTL 9015 (F) HOUSEHOLD INPUT FILE ZFSUP=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9016) ZFSUP
      STOP 8
      ELSE
      OPEN(3,FILE=ZFSUP,STATUS='OLD',FORM='FORMATTED')
      END IF
C
C OPEN PERCENT WALK DATA FILE
C
      INQUIRE (FILE=WFNAME,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
        WRITE(26,9016) WFNAME
 9016   FORMAT(1X,'RCTL 9016 (F) PERCENT WALK FILE WFNAME=',A40,
     *         ' DOES NOT EXIST')
        WRITE(*,9016) WFNAME
        STOP 8
      ELSE
        OPEN(12,FILE=WFNAME,STATUS='OLD',FORM='FORMATTED')
      END IF      
C
C OPEN ZONAL BIKE ATTRIBUTE DATA FILE
C
      IF(BICYCLE) THEN
      INQUIRE (FILE=ZFBIKE,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9417) ZFBIKE
 9417 FORMAT(1X,'RCTL 9417 (F) BIKE ATTRIBUTE FILE ZFBIKE=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9417) ZFBIKE
      STOP 8
      ELSE
      OPEN(48,FILE=ZFBIKE,STATUS='OLD',FORM='FORMATTED')
      END IF
      ELSE
      INQUIRE (FILE=ZFHHD0,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9418) ZFHHD0
 9418 FORMAT(1X,'RCTL 9418 (F) ZERO CAR HOUSEHOLD FILE ZFHHD0=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9418) ZFHHD0
      STOP 8
      ELSE
      OPEN(115,FILE=ZFHHD0,STATUS='OLD',FORM='FORMATTED')
      END IF
      END IF
C-----------------------------------------------------------------
C  READ STATION FILE FOR CENTROID NUMBERS, EQUIVALENT STATIONS,
C  PARKING CAPCACITY & PARKING COST
C-----------------------------------------------------------------
      do 111,ik=1,MAX_STATIONS
        stanum(ik)=0
	do 111,il=1,25
  	stadata(ik,il)=0.0
 111  continue
C
C  READ & SKIP HEADER RECORD
C
      READ(13,9110,END=7,ERR=8) SRECORD
 9110 FORMAT(A200)
      INDEX=0
      WRITE(26,9106)
 9106 FORMAT(/20X,'STATION ATTRIBUTE INPUT FILE SUMMARY'//
     *    52X,'                 PLTF                       ',
     * '  PNR  PNR  KNR  KNR         STATION         ',
     * '-----------------------BICYCLE ATTRIBUTES-------',
     * '------------------------'/
     *       ' PSUEDO',2X,16X,'STATION',20X,       
     * ' UR   TRN   HWY  WALK  NOT PRK   PRK    PRK ',
     * ' WALK  IVT WALK  IVT ------EQUIVALENCE-------',
     * ' --------ACCESS ATTRIBUTES---------- EGRES -----',
     * ' % OF TRAVELERS --------'/
     *       '  ZONE ',2X,16X,'NAME',18X,'MODE',1X,
     * 'LINE  NODE  NODE TIME USED TYPE  CAPC   COST',
     * ' TIME TIME TIME TIME   1    2    3    4    5 ',
     * ' RACKS LCKRS BICBG ROOMS SHOWR SHARE SHARE RACKS',
     * ' LCKRS BICBG ROOMS SHOWR'/
     *       ' ------',2X,37('-'),1X,'----',1X,    
     * '---- ----- ----- ---- ---- ---- ------ -----',
     * ' ---- ---- ---- ---- ---- ---- ---- ---- ----',
     * ' ----- ----- ----- ----- ----- ----- ----- -----',
     * ' ----- ----- ----- -----')
C
C READ INDIVIDUAL STATION RECORDS
C
    6 CONTINUE
      READ(13,9110,END=7,ERR=8) SRECORD
      CALL REFORM(SRECORD,NRECORD)
C..PSUEDO ZONE
      K1=1
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(1:K2),9701) SZONE
 9701 FORMAT(I5)
C..STATION NAME
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9702) SNAME
 9702 FORMAT(A37)
C..STATION GROUP ID
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
C..USE FLAG
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) ACTIVE
C..STATION TYPE
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9703) MODE
 9703 FORMAT(A1)
C..URBAN RAIL INDICATOR CODE
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9703) ULINE
C..TRANSIT NODE
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9701) SNODE1
      IF(SNODE1.GT.25000) THEN
      WRITE(26,4444) SNODE1
 4444 FORMAT(' STATION TRANSIT NODE NUMBER ',I5,' EXCEEDS 25,000')
      STOP 4444
      END IF
      IF(SNODE1.GT.0) THEN
      SNODE(SNODE1)=SZONE
      ELSE
C      WRITE(26,4445) SZONE
 4445 FORMAT(' STATION ZONE=',I4,' HAS NO STATION TRANSIT NODE')
      END IF
C..HIGHWAY NODE
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9701) SNODE2
C..STATION WALK ACCESS TIME
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) AWALK
 9704 FORMAT(F5.0)
C..STATION BUS ACCESS TIME
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) BWALK
C..PARKING TYPE
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9703) PTYPE
C..PARKING CAPACITY
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) PCAPC
C..PARKING COST
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) PCOST
C..STATION PNR WALK
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) PRWALK
C..STATION PNR IVT
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) PIVT
C..STATION KNR WALK
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) KWALK
C..STATION KNR IVT
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) KIVT
C..STATION EQUIVALENCE NUMBERS
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9701) EQUV1
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9701) EQUV2
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9701) EQUV3
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9701) EQUV4
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9701) EQUV5
C...STATION COORDINATE VALUES
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9705) XC
 9705 FORMAT(F8.0)
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9705) YC
C..READ BICYCLE ACCESS ATTRIBUTES
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) BIKACC(1)
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) BIKACC(2)
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) BIKACC(3)
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) BIKACC(4)
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) BIKACC(5)
C..READ BICYCLE SHARE STATION (ACCESS & EGRESS)
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) BIKSHARE(1)
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) BIKSHARE(2)
C..READ PERCENT OF TRAVELERS WITH BICYCLE ATTRIBUTES
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) BIKPER(1)
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) BIKPER(2)
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) BIKPER(3)
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) BIKPER(4)
      K1=K2+2
      CALL STAREAD(K1,K2,NRECORD)
      READ(NRECORD(K1:K2),9704) BIKPER(5)
C.................................................................
      IF(ACTIVE.GT.0) THEN
      INDEX=INDEX+1
      WRITE(26,9206) SZONE,SNAME,MODE,ULINE,
     *  SNODE1,SNODE2,AWALK,BWALK,PTYPE,PCAPC,PCOST,
     *  PRWALK,PIVT,KWALK,KIVT,EQUV1,EQUV2,EQUV3,EQUV4,EQUV5,
     *  BIKACC,BIKSHARE,BIKPER
 9206 FORMAT(2X,I4,3X,A37,3X,A1,4X,A1,1X,2(1X,I5),2(1X,F4.2),
     *       3X,A1,2X,F6.0,1X,F5.2,4(1X,F4.2),5(1X,I4),
     *       7(2X,F2.0,2X),5(1X,F5.1))
      IF(INDEX.GT.59) THEN
      INDEX=0
      WRITE(26,9106)
      END IF
      END IF
C.................................................................
         SC=SZONE-MAX_IZONES
         IF(SC.LE.0) THEN
         WRITE(26,9011) SZONE
 9011    FORMAT(' STATION ZONE NUMBER (',I4,') IS OUT OF RANGE',
     *          ' ERROR IN STATION PARKING FILE')
         STOP 16
         END IF
         IF(SZONE.EQ.MAX_ZONES) THEN
         WRITE(26,9012) SZONE,MAX_ZONES
 9012    FORMAT(1X,'RCTL 9012 (F)--- THE LAST STATION ZONE ',I4,
     *             ' MUST BE LESS THAN',
     *             ' THE MAXIMUM NUMBER OF ZONES ',I4)
         STOP 16
         END IF
         STANAME(SC)=SNAME
         STANODE1(SC)=SNODE1
         STANODE2(SC)=SNODE2
	   IF(EQUV1.EQ.0) EQUV1=MAX_ZONES
	   IF(EQUV2.EQ.0) EQUV2=MAX_ZONES
	   IF(EQUV3.EQ.0) EQUV3=MAX_ZONES
	   IF(EQUV4.EQ.0) EQUV4=MAX_ZONES
	   IF(EQUV5.EQ.0) EQUV5=MAX_ZONES
	   IF(EQUV1.LT.MAX_IZONES) THEN
	   WRITE(26,9013) EQUV1,SZONE,MAX_IZONES
 9013      FORMAT(/1X,'RCTL 9013 (W) EQUIVALENT STATION VALUE=',I4,
     *            ' FOR STATION=',I4,
     *            ' IS LESS THAN THE NUMBER OF INTERNAL ZONES (',
     *            I4,') VALUE RESET')
           EQUV1=MAX_ZONES
           END IF
           IF(EQUV2.LT.MAX_IZONES) THEN
	   WRITE(26,9013) EQUV2,SZONE,MAX_IZONES
           EQUV2=MAX_ZONES
           END IF
           IF(EQUV3.LT.MAX_IZONES) THEN
	   WRITE(26,9013) EQUV3,SZONE,MAX_IZONES
           EQUV3=MAX_ZONES
           END IF
           IF(EQUV4.LT.MAX_IZONES) THEN
	   WRITE(26,9013) EQUV4,SZONE,MAX_IZONES
           EQUV4=MAX_ZONES
           END IF
           IF(EQUV5.LT.MAX_IZONES) THEN
	   WRITE(26,9013) EQUV5,SZONE,MAX_IZONES
           EQUV5=MAX_ZONES
           END IF
         MINSTA=MIN(SZONE,EQUV1,EQUV2,EQUV3,EQUV4,EQUV5)
         NMNSTA=MINSTA-MAX_IZONES
         IF(MODE.EQ.'B') STADATA(SC,2)=1.0
         IF(MODE.EQ.'C') STADATA(SC,2)=2.0
         IF(MODE.EQ.'U') STADATA(SC,2)=3.0
         IF(MODE.EQ.'T') STADATA(SC,2)=4.0
         IF(MODE.EQ.'R') STADATA(SC,2)=5.0
         IF(MODE.EQ.'B'.AND.ACTIVE.EQ.1.0) STANUM(SC)=3
         IF(MODE.EQ.'C'.AND.ACTIVE.EQ.1.0) STANUM(SC)=1
         IF(MODE.EQ.'U'.AND.ACTIVE.EQ.1.0) STANUM(SC)=2
         IF(MODE.EQ.'T'.AND.ACTIVE.EQ.1.0) STANUM(SC)=4
         IF(MODE.EQ.'R'.AND.ACTIVE.EQ.1.0) STANUM(SC)=5
         IF(MODE.EQ.'U'.AND.ULINE.EQ.'R') STADATA(SC,8)=1.0  !Red/Purple (KRED)
         IF(MODE.EQ.'U'.AND.ULINE.EQ.'B') STADATA(SC,8)=2.0  !Blue       (KBLUE)
         IF(MODE.EQ.'U'.AND.ULINE.EQ.'G') STADATA(SC,8)=3.0  !Green      (KGREEN)
         IF(MODE.EQ.'U'.AND.ULINE.EQ.'P') STADATA(SC,8)=4.0  !Gold       (KGOLD)
         IF(MODE.EQ.'U'.AND.ULINE.EQ.'E') STADATA(SC,8)=4.0  !Gold
         IF(MODE.EQ.'U'.AND.ULINE.EQ.'X') STADATA(SC,8)=5.0  !Expo       (KEXPO)
         IF(MODE.EQ.'U'.AND.ULINE.EQ.'W') STADATA(SC,8)=6.0  !WSAB       (KWSAB)
         IF(MODE.EQ.'U'.AND.ULINE.EQ.'F') STADATA(SC,8)=7.0  !ESFV       (KESFV)
         IF(MODE.EQ.'U'.AND.ULINE.EQ.'S') STADATA(SC,8)=8.0  !Sepulveda  (KSEPV)
         IF(MODE.EQ.'U'.AND.ULINE.EQ.'A') STADATA(SC,8)=9.0  !Airport Connector (KAIRCN)
         IF(MODE.EQ.'U'.AND.ULINE.EQ.'V') STADATA(SC,8)=10.0 !Vermont    (KVERM)
         EQUIV3(SC,1)=EQUV1
         EQUIV3(SC,2)=EQUV2
         EQUIV3(SC,3)=EQUV3
         EQUIV3(SC,4)=EQUV4
         EQUIV3(SC,5)=EQUV5
         EQUIV(SC)=NMNSTA
         STADATA(SC,1)=PCOST
         STADATA(SC,4)=PCAPC
         STADATA(SC,3)=PCAPC
         STADATA(SC,6)=ACTIVE
         IF(STACODE) THEN
         STADATA(SC,9)=AWALK
         STADATA(SC,10)=BWALK
         ELSE
         STADATA(SC,9)=0.0
         STADATA(SC,10)=0.0
         END IF
         STADATA(SC,11)=PRWALK
         STADATA(SC,12)=PIVT
         STADATA(SC,13)=KWALK
         STADATA(SC,14)=KIVT
         DO K=1,5
         STADATA(SC,(14+K))=BIKACC(K)*(BIKPER(K)/100.0)
         END DO
         STADATA(SC,20)=BIKSHARE(1)
         STADATA(SC,21)=BIKSHARE(2)
         STADATA(SC,23)=XC/scoord
         STADATA(SC,24)=YC/scoord
         IF(PTYPE.EQ.PNR) STADATA(SC,7)=1
         IF(PTYPE.EQ.KNR) STADATA(SC,7)=2
         IF(STADATA(SC,7).LE.0.AND.ACTIVE.EQ.1) THEN
         WRITE(75,9112) SZONE,SNAME
 9112    FORMAT(1X,'RCTL 9112 (WARNING) STATION (',I4,') ',A25,
     *          ' DOES NOT ALLOW FOR PNR OR KNR')
         END IF  
         IF(STADATA(SC,7).EQ.2.AND.PCAPC.GT.0) THEN
         WRITE(75,9212) SZONE,SNAME
 9212    FORMAT(1X,'RCTL 9312 (WARNING) STATION (',I4,') ',A25,
     *          ' IS DEFINED AS KNR, BUT CONTAINS SPACES')
         END IF  
         IF(MODE.EQ.'U'.AND.STADATA(SC,8).LT.1.0.AND.ACTIVE.EQ.1) THEN
         WRITE(75,9312) SZONE,SNAME
 9312    FORMAT(1X,'RCTL 9312 (WARNING) STATION (',I4,') ',A25,
     *          ' IS MISSING URBAN RAIL LINE CODE')
         END IF
      GO TO 6
    8 WRITE(26,9007) SZONE
 9007 FORMAT(/,1X,'RCTL 9007 (F) ENCOUNTERED ERROR READING DATA',
     *         ' RECORD FOR STATION ',I4)
      WRITE(*,9007) SZONE
      STOP 9007
    7 CONTINUE
C
C  EVALUATE REFERENCE VALUES
C
      DO SC=1,MAX_STATIONS
      IF(STADATA(SC,6).NE.1.0) CYCLE
      DO K=1,5
      IF(EQUIV3(SC,K).EQ.MAX_ZONES) CYCLE
      SZONE=EQUIV3(SC,K)-MAX_IZONES
      IF(STADATA(SZONE,6).NE.1.0) THEN
      WRITE(75,9232) (SC+MAX_IZONES),STANAME(SC),EQUIV3(SC,K),K
 9232 FORMAT(' STATION ',I4,1X,A37,' HAS STATION ',I4,
     * ' LISTED AS REFERENCE STATION ',I1,' NOT FOUND IN STATION FILE')
      END IF
      END DO
      END DO
C
C  READ ZONAL SE DATA FILES
C
  101 READ(9,8801,END=1001) ZONE,POP,RETAIL,TOTEMP,INCOME,
     *                      INC1,INC2,INC3,AREA
 8801 FORMAT(I8,24X,F8.0,16X,7F8.0)
        IF(ZONE.LE.0.OR.ZONE.GT.MAX_IZONES) THEN
        WRITE(26,8702) ZONE
        GO TO 101
        END IF 
        DIZ=DISTEQ(ZONE)
      ZHHD(12,ZONE)=POP
      RPOP=RPOP+POP
      ZHHD(13,ZONE)=TOTEMP
      REMP=REMP+TOTEMP
      ZHHD(14,ZONE)=AREA
      IF(AREA.GT.0) THEN
      DENSITY=(POP+CEMPDEN*TOTEMP)/(AREA*1000.0)
      ELSE
      DENSITY=0.0
      WRITE(26,8813) ZONE,POP,TOTEMP
 8813 FORMAT(1X,'RCTL 8813 (W) THE AREA OF ZONE ',I4,
     *           ' IS ZERO WITH POPULATION=',F7.0,' AND',
     *           ' TOTAL EMPLOYMENT=',F7.0)
      END IF
      ZHHD(15,ZONE)=DENSITY
      IF(DENSITY.GT.MINDEN) THEN
      ZHHD(16,ZONE)=ADENP+BDENP*(1-EXP(-CDENP*DENSITY))+
     *              DDENP*(1-EXP(-EDENP*DENSITY))
      END IF
      ZHHD(17,ZONE)=ADENA+BDENA*(1-EXP(-CDENA*DENSITY))+
     *              DDENA*(1-EXP(-EDENA*DENSITY))
      ZHHD(18,ZONE)=RETAIL
      IF(SPEVENT) THEN
      ZHHD(16,ZONE)=0.0
      ZHHD(17,ZONE)=0.0
      END IF
C
      IF(ZONE.LE.0.OR.ZONE.GT.MAX_IZONES) THEN
      WRITE(26,8702) ZONE
 8702 FORMAT(' ZONE NUMBER (',I4,') IS OUT OF RANGE',
     *          ' ERROR IN ZONAL SE DATA FILE')
      GO TO 101
      END IF 
      ZHHD(1,ZONE)=INCOME
      TOTHH=INC1+INC2+INC3
	    IF(TOTHH.GT.0.0) THEN
      PERINC1=(INC1/TOTHH)
      PERINC2=(INC2/TOTHH)
      PERINC3=(INC3/TOTHH)
	ELSEIF(TOTHH.LE.0.0) THEN
	PERINC1=0.0
	PERINC2=0.0
	PERINC3=0.0
	ENDIF
      ZHHD(2,ZONE)=PERINC1
      ZHHD(3,ZONE)=PERINC2
      ZHHD(4,ZONE)=PERINC3
      ZHHD(8,ZONE)=INC1
	ZHHD(9,ZONE)=INC2
	ZHHD(10,ZONE)=INC3
	    RHH3=RHH3+INC3
	ZHHD(11,ZONE)=TOTHH
      GOTO 101
 1001 CONTINUE
C
C  READ SUPPLEMENTAL ZONAL SE FILE (NOTE:  ZONAL PARKING COSTS IN $2011)
C
  103 READ(3,8003,END=1003) ZONE,TTIME,ZPCOST,cocode,HPCOST
 8003 FORMAT(I4,T9,f2.0,T11,f6.0,T28,f1.0,T41,f6.0)
      IF(ZONE.LE.0.OR.ZONE.GT.MAX_IZONES) THEN
      WRITE(26,8703) ZONE
 8703 FORMAT(' ZONE NUMBER (',I4,') IS OUT OF RANGE',
     *          ' ERROR IN SUPP ZONAL SE DATA FILE')
      GO TO 103
      END IF 
      IF(PEAK) THEN
      ZHHD(5,ZONE)=ZPCOST*Pcpi
      ELSE
      ZHHD(5,ZONE)=HPCOST*Pcpi      
      END IF
      ZHHD(6,ZONE)=COCODE
      ZHHD(7,ZONE)=TTIME
      GOTO 103
 1003 CONTINUE
      CLOSE(3,STATUS='KEEP')
      IF((.NOT.LAX).AND.(.NOT.SPEVENT)) THEN
      INDEX=0
      WRITE(26,1009)
 1009 FORMAT(//,' PARKING COST SUMMARY REPORT'/
     *          ' ---------------------------'/
     *          '  ZONE (DAILY COST 2011$)')
      DO 1010 ZONE=1,MAX_IZONES
      IF(ZHHD(5,ZONE).GT.0) THEN
      INDEX=INDEX+1
      PCOSTZ(INDEX)=ZONE
      PKCOST(INDEX)=ZHHD(5,ZONE)/100.0
       IF(INDEX.EQ.10) THEN
       WRITE(26,1011) ((PCOSTZ(K),PKCOST(K)),K=1,10)
 1011  FORMAT(2X,10(I4,'($',F6.2,')',1X))
       INDEX=0
       END IF
      END IF
 1010 CONTINUE
      IF(INDEX.GT.0) WRITE(26,1011) ((PCOSTZ(K),PKCOST(K)),K=1,INDEX)
      WRITE(26,8814)
 8814 FORMAT(//)
      END IF
C
C  READ AREA TYPE INPUT FILE
C
      IF(TAXIMODE.OR.UBERMODE) THEN
      READ(138,'(A80)') HEADER
 1400 READ(138,*,ERR=1401,END=1402) ZONE,AVGVAL,ALOW,AHIGH
      IF(ALOW.EQ.AHIGH) THEN
      ZHHD(29,ZONE)=ALOW
      GO TO 1400
      END IF
      RESID=AVGVAL-FLOAT(ALOW)
      IF(RESID.GT.0.49) THEN
      ZHHD(29,ZONE)=AHIGH
      ELSE
      ZHHD(29,ZONE)=ALOW
      END IF
      IF(RESID.GE.1.0) ZHHD(29,ZONE)=ALOW+1.0
      IF(ZHHD(29,ZONE).GT.6.0) ZHHD(29,ZONE)=6.0
      IF(ZHHD(29,ZONE).LT.1.0) ZHHD(29,ZONE)=6.0
      GO TO 1400
 1401 WRITE(26,1404) ZONE
 1404 FORMAT(' RCTL 1401 (F) ERROR READING AREA TYPE INPUT FILE',
     *       ' NEAR ZONE ',I4)
      STOP 1401
 1402 CONTINUE
      END IF
C 
C  READ BICYCLE ZONAL ATTRIBUTE FILE
C
      IF(BICYCLE) THEN
      READ(48,9626) HEADER
 1200 READ(48,*,END=1201,ERR=1202) ZONE,BIKEATT
      IF(ZONE.LE.0.OR.ZONE.GT.MAX_IZONES) THEN
      WRITE(26,1203) ZONE
 1203 FORMAT(/' RCTL 1203 (F) INPUT ZONE VALUE (',I5') IS OUT OF RANGE',
     *       ' FOR ZONAL BIKE ATTRIBUTE DATA')
      WRITE(*,1203) ZONE
      STOP 8
      END IF
      DO M=1,8
      IF(M.LE.7) THEN
      ZHHD((20+M),ZONE)=BIKEATT(M)/100.0
      ELSE
      ZHHD((20+M),ZONE)=BIKEATT(M)
      END IF
      IF(M.LE.7.AND.BIKEATT(M).GT.100.0) THEN
      WRITE(26,1205) ZONE,M,BIKEATT(M)
 1205 FORMAT(' RCTL 1205 (W) ZONE ',I4,' ATTRIBUTE NO=',I1,
     *       ' HAS A PERCENTAGE (',F6.1,') THAT EXCEEDS 100%')
      END IF
      END DO
      GO TO 1200
 1202 WRITE(26,1204) ZONE
 1204 FORMAT(/' RCTL 1204 (F) ERROR READING ZONAL BIKE DATA FOR',
     *        ' ZONE ',I4)
      WRITE(*,1204) ZONE
      STOP
 1201 CONTINUE
      ELSE
      READ(115,9626) HEADER
 1300 READ(115,*,END=1301,ERR=1302) ZONE,INC1
      IF(ZONE.LE.0.OR.ZONE.GT.MAX_IZONES) THEN
      WRITE(26,1303) ZONE
 1303 FORMAT(/' RCTL 1303 (F) INPUT ZONE VALUE (',I5') IS OUT OF RANGE',
     *       ' FOR ZERO CAR HOUSEHOLD DATA')
      WRITE(*,1303) ZONE
      STOP 8
      END IF
      ZHHD(22,ZONE)=INC1
      INDEX=IFIX(INC1/5.0)+1
      DIZ=DISTEQ(ZONE)
      ZEROTLF(INDEX,DIZ)=ZEROTLF(INDEX,DIZ)+1
      ZEROTLF(22,DIZ)=ZEROTLF(22,DIZ)+1
      GO TO 1300
 1302 WRITE(26,1304) ZONE
 1304 FORMAT(/' RCTL 1304 (F) ERROR READING ZERO CAR HOUSEHOLD DATA',
     *        ' FOR ZONE ',I4)
      WRITE(*,1304) ZONE
      STOP
 1301 CONTINUE      
      END IF
      IF(DEBUG) THEN
      OPEN(116,FILE='ZEROTLF_DIST.CSV',STATUS='UNKNOWN',
     *         FORM='FORMATTED')
      WRITE(116,1307) (DNAME(K),K=1,20)
 1307 FORMAT('PERCENT',20(',',A10))
      DO K=1,20
       DO K1=1,21
        IF(ZEROTLF(22,K).GT.0) THEN
        ZEROTLF(K1,K)=ZEROTLF(K1,K)/ZEROTLF(22,K)
        END IF
       END DO
      END DO
      DO K=1,21
      K1=(K-1)*5
      WRITE(116,1308) K1,(ZEROTLF(K,K2),K2=1,20) 
 1308 FORMAT(I3,20(',',F10.4))
      END DO
      END IF
C.................................................................
C
C INPUT AND STORE PERCENT WALK FILE DATA
C
   60 READ(12,61,END=66) ZONE,wlkb14,wlkb12,wlkr14,wlkr12
   61 FORMAT(T5,i4,t11,f6.0,t19,f6.0,t27,f6.0,t35,f6.0)
      IF(ZONE.LE.0.OR.ZONE.GT.MAX_IZONES) THEN
      WRITE(26,8704) ZONE
 8704 FORMAT(' ZONE NUMBER (',I4,') IS OUT OF RANGE',
     *          ' ERROR IN PERCENT WALK DATA FILE')
      GO TO 60
      END IF
      PWALK(ZONE,1)=wlkb14
      PWALK(ZONE,2)=wlkb12-wlkb14
          IF(PWALK(ZONE,2).LT.0) THEN
          WRITE(26,8705) ZONE,WLKB14,WLKB12
 8705     FORMAT(' ZONE NUMBER (',I4,') CONTAINS AN',
     *          ' ERROR IN PERCENT WALK DATA FILE --',
     *          ' SHORT WALK PERCENT=',F5.3,' LONG WALK',
     *          ' PERCENT=',F5.3)
          END IF
      GO TO 60
   66 CONTINUE
C
C     CONVENTIONAL/AUTOMATED VEHICLE ZONAL LEVEL DATA
C
      IF(AVMDL) THEN
      RESID=0.0
      DO K=1,6
      AVGVAL=0.0
      DO K1=1,4
      RESID=RESID+AVPROP(K,K1)
      AVGVAL=AVGVAL+AVPROP(K,K1)
      END DO
      IF(AVGVAL.GT.0.AND.(AVGVAL.LT.0.99.OR.AVGVAL.GT.1.01)) THEN
      WRITE(26,8709) K,AVGVAL
      WRITE(*,8709) K,AVGVAL
 8709 FORMAT(/1X,' RCTL 8709 (F) AVPROP VALUES FOR AREA TYPE ',
     *        I1,' NE 1.0 (',F5.2,')'/)
      STOP
      END IF
      END DO
      IF(RESID.LE.0.0) THEN
      WRITE(26,8706)
      WRITE(*,8706)
 8706 FORMAT(/1X,' RCTL 8706 (F) AVMDL=T, AVPROP VALUES ALL ZERO'/)
      STOP
      END IF
      DO IZ=1,MAX_IZONES
      ZINDEX=IFIX(ZHHD(29,IZ))
      IF(ZINDEX.EQ.0) ZINDEX=6
      IF(ZINDEX.GT.6) ZINDEX=6
      ZHHD(31,IZ)=AVPROP(ZINDEX,1)
      ZHHD(32,IZ)=AVPROP(ZINDEX,2)
      ZHHD(33,IZ)=AVPROP(ZINDEX,3)
      ZHHD(34,IZ)=AVPROP(ZINDEX,4)
      END DO
      ELSE
      DO IZ=1,MAX_IZONES
      ZHHD(31,IZ)=1.0
      ZHHD(32,IZ)=0.0
      ZHHD(33,IZ)=0.0
      ZHHD(34,IZ)=0.0
      END DO
      END IF
C
C  SET THE I-ZONE CONTROLS
C
C
      DO 65,IN=1,10
       IF(I(IN).GT.0) THEN
          NI=NI+1
          IF(I(IN).LE.MAX_IZONES) THEN
            IOI(I(IN))=.TRUE.
          ENDIF
       ELSEIF(I(IN).LT.0) THEN
          NI=NI+1
          IEND=I(IN)*(-1)
          IBEG=I(IN-1)+1
       DO 63,INF=IBEG,IEND
          IOI(INF)=.TRUE.
  63   CONTINUE
       ENDIF
  65   CONTINUE
C
C  SET THE J-ZONE CONTROLS
C
       DO 70,JN=1,10
       IF(J(JN).GT.0) THEN
         NJ=NJ+1
         IF(J(JN).LE.MAX_IZONES) THEN
            JOI(J(JN))=.TRUE.
            JCOUNT=JCOUNT+1
            IF(LAX) AEQUIV(JCOUNT)=J(JN)
         ENDIF
       ELSEIF(J(JN).LT.0) THEN
        NJ=NJ+1
        JEND=J(JN)*(-1)
        JBEG=J(JN-1)+1
        DO 68, INF=JBEG,JEND
        JOI(INF)=.TRUE.
        JCOUNT=JCOUNT+1
        IF(LAX) AEQUIV(JCOUNT)=INF
 68     CONTINUE
       ENDIF
 70    CONTINUE
       IF(LAX.AND.JCOUNT.GT.50) THEN
       WRITE(26,571) JCOUNT
  571  FORMAT(1X,'RCTL 71 (F) NUMBER OF SELECTED ATTRACTION ZONES (',
     *           I4,') EXCEEDS ALLOWABLE MAXIMUM OF 50')
       WRITE(*,571) JCOUNT
       STOP 71
       END IF
C
C  READ IN FILE CONTAINING PARKING UTILIZATION FROM PREVIOUS
C  RUNS AND SUBTRACT OUT USED SPACES FROM PARKING CAPACITY
C  IF PARKING RESTRAINT IN EFFECT
C
      INQUIRE (FILE=PKUTIN,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9515) PKUTIN
 9515 FORMAT(1X,'RCTL 9515 (F) PARKING UTILIZATION INPUT FILE PKUTIN=',
     *        A40,' DOES NOT EXIST')
      WRITE(*,9515) PKUTIN
      STOP 8
      ELSE
      open(15,file=PKUTIN,form='formatted',status='old')
      END IF
C
      IF(CAPRES) THEN
   71 read(15,8141,end=83) stazone,pkused
 8141 format(2x,i4,2x,F5.0)
      sc=stazone-MAX_IZONES
      IF(SC.LE.0) THEN
      WRITE(75,8707) STAZONE
 8707 FORMAT(' STATION ZONE NUMBER (',I4,') IS OUT OF RANGE',
     *          ' ERROR IN PARKING UTILIZATION FILE')
      GO TO 60
      END IF
      stadata(sc,3)=STADATA(sc,4) - PKUSED
      if(stadata(sc,3).lt.0.0) then
      write(75,8142) stazone,pkused,stadata(sc,4)
 8142 FORMAT(1X,'RCTL 8142 (W) STATION ',I4,' HAS ',F5.0,
     *       ' USED SPACES WHICH ',
     *       'EXCEEDS THE STATION CAPACITY OF ',F5.0)
      stadata(sc,3)=0.0
      end if
      goto 71
   83 continue
      ENDIF
C
C     READ & STORE LAX PARKING LOT FILE
C
      IF(LAX) THEN
      INQUIRE (FILE=LAXPARK,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9351) LAXPARK
 9351 FORMAT(1X,'RCTL 9351 (F) LAX PARKING LOT INPUT FILE=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9351) LAXPARK
      STOP 8
      ELSE
      OPEN(3,FILE=LAXPARK,
     *        STATUS='OLD',FORM='FORMATTED')
      END IF
      READ(3,*) HEADER,LCPI
      READ(3,*) HEADER,SHDROPP
      READ(3,*) HEADER,SHDROPE
      READ(3,*) HEADER
 9352 LINDEX=LINDEX+1
      IF(LINDEX.GT.50) THEN
      WRITE(26,9353)
      WRITE(*,9353) 
 9353 FORMAT(1X,'RCTL 9353 (F) NUMBER OF LAX PARKING LOTS EXCEED',
     *          'MAXIMUM ALLOWABLE VALUE (50)')
      STOP 9353
      END IF
 9355 READ(3,*,END=9356) ZONE,PRKDATA(LINDEX,1),EGTYPE,
     *                   (PRKDATA(LINDEX,K),K=3,18)
      IF(ZONE.LE.0.OR.ZONE.GT.4000) THEN
      WRITE(26,9358) ZONE
      WRITE(*,9358)  ZONE
 9358 FORMAT(1X,'RCTL 9358 (F) ENCOUNTERED INVALID ZONE NUMBER=',I4,
     *          ' IN PARKING LOT INPUT FILE')
      STOP 9358
      END IF 
      PEQUIV(LINDEX)=ZONE
      IF(EGTYPE.EQ.WALK) PRKDATA(LINDEX,2)=1.0
      IF(EGTYPE.EQ.SHUTTLE) PRKDATA(LINDEX,2)=2.0
      IF(EGTYPE.EQ.TRNONLY) PRKDATA(LINDEX,2)=3.0
      IF(EGTYPE.EQ.ITF) PRKDATA(LINDEX,2)=4.0
      IF(EGTYPE.EQ.ITF2) PRKDATA(LINDEX,2)=5.0
      IF(PRKDATA(LINDEX,2).LE.0.OR.PRKDATA(LINDEX,2).GT.5.0) THEN
      WRITE(26,9354) EGTYPE,ZONE
      WRITE(*,9354)  EGTYPE,ZONE
 9354 FORMAT(1X,'RCTL 9354 (F) INVALID EGRESS TYPE (',A2,') FOR',
     *          ' PARKING LOT ZONE=',I4) 
      STOP 9354
      END IF
      IF(PRKDATA(LINDEX,2).EQ.4.AND.(.NOT.ITFPRK)) THEN
      WRITE(26,9361) EGTYPE,ZONE
      WRITE(*,9361)  EGTYPE,ZONE
 9361 FORMAT(/1X,'RCTL 9361 (F) INVALID EGRESS TYPE (',A2,') FOR',
     *          ' PARKING LOT ZONE=',I4,' AND ITFPRK IS TRUE'/)
      STOP 9361
      END IF 
      IF(PRKDATA(LINDEX,2).EQ.5.AND.(.NOT.ITFPRK)) THEN
      WRITE(26,9362) EGTYPE,ZONE
      WRITE(*,9362)  EGTYPE,ZONE
 9362 FORMAT(/1X,'RCTL 9362 (F) INVALID EGRESS TYPE (',A2,') FOR',
     *          ' PARKING LOT ZONE=',I4,' AND ITFPRK IS TRUE'/)
      STOP 9362
      END IF 
      PRKDATA(LINDEX,5)=(PRKDATA(LINDEX,5)*100.0*LCPI)/2.0
      PRKDATA(LINDEX,6)=(PRKDATA(LINDEX,6)*100.0*LCPI)/44.0
      PRKDATA(LINDEX,10)=PRKDATA(LINDEX,10)*100.0*LCPI
      PRKDATA(LINDEX,11)=PRKDATA(LINDEX,11)*100.0*LCPI
      GO TO 9352
 9356 LINDEX=LINDEX-1
      CLOSE(3,STATUS='KEEP')
      WRITE(26,9357) LINDEX,LCPI,SHDROPP,SHDROPE
 9357 FORMAT(//,1X,'LAX PARKING LOT INPUT DATA'/
     *          1X,'--------------------------'/
     *          1X,'NUMBER OF LAX PARKING LOTS      =',I6/
     *          1X,'CPI CONVERSION FACTOR           =',F6.3/
     *          1X,'SHUTTLE DROP-OFF TIME - PUBLIC  =',F6.3/
     *          1X,'SHUTTLE DROP-OFF TIME - EMPLOYEE=',F6.3//
     *          1X,'                                                 ',
     *              'PUB   EMPL         PUB   EMPLY   PUB    EMPLY  ',
     *              'INTRA   PUB  EMPL  TURN  TURN'/
     *          1X,'       USE  EGRESS PUBLIC  EMPLOY  PUBLIC EMPLY  ',
     *              'SHLT  SHLT  SHLT   SHLT   SHLT   SHLT    SHLT   ',
     *              'TERM    SEARCH    OVER  OVER'/
     *          1X,' ZONE  TYPE  TYPE  SPACES  SPACES   COST   COST  ',
     *              'WAIT  WAIT  WALK   COST   COST  FACTOR  FACTOR  ',
     *              'WALK  TIME  TIME RATIO RATIO'/
     *          1X,' ----  ----  ----  ------  ------  -----  -----  ',
     *             '----  ----  ----  -----  -----  ------  ------  ',
     *             '----  ----  ----  ----  ----')
      DO 9359 K=1,LINDEX
      WRITE(26,9360) PEQUIV(K),(PRKDATA(K,K1),K1=1,18)
 9360 FORMAT(1X,I5,2(2X,F4.0),2(2X,F6.0),2(2X,F5.0),3(2X,F4.0),
     *             2(2X,F5.0),2(2X,F6.2),3(2X,F4.0),2(2X,F4.1))
 9359 CONTINUE
C
C...OBTAIN FLY-A-AWAY STATION FILE DATA
C
      INQUIRE(FILE=FLYAWAY,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9735) FLYAWAY
 9735 FORMAT(1X,'RCTL 9735 (F) FLY-A-AWAY INPUT FILE=',A40/
     *       ' DOES NOT EXIST')
      WRITE(*,9735) FLYAWAY
      STOP 9735
      ELSE
      OPEN(76,file=FLYAWAY,FORM='FORMATTED',STATUS='OLD')
      END IF
      LINDEX=0
      READ(76,*) HEADER,LCPI
      READ(76,*) HEADER,FLYAEGR
      READ(76,*) HEADER
 9736 LINDEX=LINDEX+1
      READ(76,*,END=9737) FLYNAME(LINDEX),(FLYADATA(LINDEX,K),K=1,6)
      FLYADATA(LINDEX,4)=FLYADATA(LINDEX,4)*0.50
      FLYADATA(LINDEX,5)=FLYADATA(LINDEX,5)*100.0*LCPI
      FLYADATA(LINDEX,6)=FLYADATA(LINDEX,6)*100.0*LCPI
      GO TO 9736
 9737 LINDEX=LINDEX-1
      WRITE(26,9757) LINDEX,LCPI,FLYAEGR
 9757 FORMAT(//,1X,'FLY-AWAY PARKING DATA'/
     *          1X,'---------------------'/
     *          1X,'NUMBER OF FLY-AWAY  LOTS        =',I6/
     *          1X,'CPI CONVERSION FACTOR           =',F6.3/
     *          1X,'EGRESS TIME                     =',F6.3//
     *  1X,'   STATION             PSEUDO  USE  WAIT            PRK'/ 
     *  1X,'     NAME         TAZ    NODE  TYP  TIME    FARE    COST'/
     *  1X,'---------------  -----  -----  ---  -----  ------  ------')
      DO NI=1,LINDEX
      WRITE(26,9758) FLYNAME(NI),(FLYADATA(NI,K),K=1,6)
 9758 FORMAT(1X,A15,2(2X,F5.0),2X,F3.0,2X,F5.1,2(2X,F6.1))
      END DO
C
C...OBTAIN RENTAL FACILITY FILE DATA
C
      IF(AIRPASS) THEN
      INQUIRE(FILE=RENTAL,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9745) RENTAL
 9745 FORMAT(1X,'RCTL 9745 (F) RENTAL FACILITY INPUT FILE=',A40/
     *       ' DOES NOT EXIST')
      WRITE(*,9745) RENTAL
      STOP 9745
      ELSE
      OPEN(77,file=RENTAL,FORM='FORMATTED',STATUS='OLD')
      END IF
      LINDEX=0
      READ(77,*) HEADER,RVALUE(1)
      READ(77,*) HEADER,RVALUE(2)
      READ(77,*) HEADER,RVALUE(3)
      READ(77,*) HEADER,RVALUE(4)
      READ(77,*) HEADER
 9746 LINDEX=LINDEX+1
      READ(77,*,END=9747) (RNTLDATA(LINDEX,K),K=1,2)
      RNTLDATA(LINDEX,3)=RVALUE(1)
      RNTLDATA(LINDEX,4)=RVALUE(2)*100.0
      RNTLDATA(LINDEX,5)=RVALUE(3)
      RNTLDATA(LINDEX,6)=RVALUE(4)
      GO TO 9746
 9747 LINDEX=LINDEX-1
      WRITE(26,9767) LINDEX
 9767 FORMAT(//,1X,'RENTAL FACILITY DATA'/
     *          1X,'--------------------'/
     *          1X,'NUMBER OF RENTAL FACILITIES     =',I6//
     *  1X,'                 DROP           SHUTTLE'/
     *  1X,'          MKT     OFF          WAIT   IVT'/
     *  1X,' TAZ     SHARE   TIME   COST   TIME   FCTR'/
     *  1X,'------  -------  -----  -----  -----  -----')
      DO NI=1,LINDEX
      WRITE(26,9768) (RNTLDATA(NI,K),K=1,6)
 9768 FORMAT(2X,F5.0,2X,F7.5,4(2X,F5.1))
      RVALUE(5)=RVALUE(5)+RNTLDATA(NI,2)
      END DO
      IF(RVALUE(5).LT.0.99.OR.RVALUE(5).GT.1.01) THEN
      WRITE(26,9769) RVALUE(5)
 9769 FORMAT(/,' MARKET SHARE PROPORTIONS (',F7.4,') DOES NOT'
     *         ' ADD TO 100%')
      WRITE(26,9769) RVALUE(5)
      STOP 9769
      END IF
      END IF
      END IF
C
C..OBTAIN X-Y COORDINATES FOR PRODUCTION ZONES
C
      INQUIRE (FILE=XYCOORD,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9715) XYCOORD
 9715 FORMAT(1X,'RCTL 9715 (F) X-Y COORD INPUT FILE XYCOORD=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9715) XYCOORD
      STOP 9715
      ELSE
      open(74,file=XYCOORD,form='formatted',status='old')
      END IF
 55   read(74,1111,end=67) node,xcoor,ycoor
 1111  format(i6,f10.0,f10.0)
       if(node.le.MAX_IZONES) then
       Sycoord(node)=ycoor/scoord
       Sxcoord(node)=xcoor/scoord
       endif
       goto 55
   67 continue
      CLOSE(74,STATUS='KEEP')
C
C     OPEN FTA USER BENEFIT FILE (IF REQUESTED)
C
      IF(USERBEN) THEN
       OPEN(29,FILE=FUSERBN,STATUS='UNKNOWN',FORM='BINARY')
       NZONES=MAX_IZONES
         IF(AIRPASS) THEN
         WRITE(29) NZONES,NCATS,ACOEF(1),ACOEF(1),TPURP,TDAY,ALTNAME
         ELSE
         WRITE(29) NZONES,NCATS,VCOEF(1),VCOEF(1),TPURP,TDAY,ALTNAME
         END IF
      END IF
C
C     CALIBRATION FILES
C
      IF(CALIB) THEN
C.....OPEN PERSON AND TRANSIT TRIP SUMMARY BY INCOME GROUP AND TRIP LENGTH
      OPEN(44,FILE='PERMKT.CSV',STATUS='UNKNOWN',
     *          FORM='FORMATTED')
C.....OPEN MODEL ESTIMATED VALUE FILE
      OPEN(67,FILE='MODEL_EST.CSV',STATUS='UNKNOWN',FORM='FORMATTED')
C.....CALIBRATION CONSTANT SUMMARY BY ITERATION
      OPEN(52,FILE='CONSTANT_ITERATION.CSV',STATUS='UNKNOWN',
     *        FORM='FORMATTED')
      WRITE(52,6002)
 6002 FORMAT('ITER,KTRN,KTRN,KTRN,KTRN,KTRN,KSR,KSR,KSR,KSR,KSR,',
     *       'K3P,K3P,K3P,K3P,K3P,K4P,K4P,K4P,K4P,K4P,',
     *       'KNMOT,KNMOT,KNMOT,KNMOT,KNMOT,',
     *       'KBIKE,KBIKE,KBIKE,KBIKE,KBIKE,',
     *       'KDTRN,KDTRN,KDTRN,KDTRN,KDTRN,KPNR,KPNR,KPNR,KPNR,KPNR,',
     *       'KCR,KUR,KEBUS,KWAY,KBKCR,KBKUR,KBCR,KBUR,',
     *       'KBRPDW,KBTWYW,KBEXPW,KINFL,KINFR,KBRPDB,KBRT,KRBUS,',
     *       'KBBRTW,KBRPDD,KBBRTD,CCBD,KBTWYD,KBEXPD,KBBUS,KBRPD,',
     *       'KBWAY,KBEXP,KBBRT,KBTWYB,KBEXPB,KBBRTB,KINTRAWK,',
     *       'KINTRAWK,KINTRAWK,KINTRAWK,KINTRAWK,KINTRABK,',
     *       'KINTRABK,KINTRABK,KINTRABK,KINTRABK,KPUR,KWCR'/
     *       '#,',8('1,2,3,4,5,'),'All',29(',All'),2(',1,2,3,4,5'),
     *       2(',ALL'))
      OPEN(53,FILE='TRNT_ITERATION.CSV',STATUS='UNKNOWN',
     *        FORM='FORMATTED')
      WRITE(53,6003)
 6003 FORMAT(',1,2,3,4,5')
      OPEN(40,FILE='RAIL_TLF.CSV',STATUS='UNKNOWN',FORM='FORMATTED')
      OPEN(45,FILE='NMOT_TLF.CSV',STATUS='UNKNOWN',FORM='FORMATTED')
      OPEN(111,FILE='TWAY_INVEH.CSV',STATUS='UNKNOWN',FORM='FORMATTED')
      OPEN(98,FILE='WALK_EXPRESS_RATIO.CSV',STATUS='UNKNOWN',
     *        FORM='FORMATTED')
      END IF
C
C      OPEN DETAILED REPORTING FILES
C
      IF(DEBUG.AND.SDETAIL) THEN
      OPEN(61,FILE='STA_TO_STA.RPT',STATUS='UNKNOWN',FORM='FORMATTED')
      OPEN(62,FILE='DSTA_TO_ZNE.RPT',STATUS='UNKNOWN',FORM='FORMATTED')
      OPEN(63,FILE='OSTA_TO_ZNE.RPT',STATUS='UNKNOWN',FORM='FORMATTED')
      OPEN(64,FILE='BRT_TO_UR.RPT',STATUS='UNKNOWN',FORM='FORMATTED')
      OPEN(104,FILE='STA_TO_STA_ALL.RPT',
     *         STATUS='UNKNOWN',FORM='FORMATTED')
      END IF
      IF(LDEBUG) 
     *    OPEN(77,FILE='LDEBUG.RPT',STATUS='UNKNOWN',FORM='FORMATTED')
C
C     READ & CHECK FOR LINK UNRELIABILITY MATRIX
C
      IF(LUNREL) THEN
      INQUIRE (FILE=FLUNREL,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9451) FLUNREL
 9451 FORMAT(1X,'RCTL 9451 (F) LINK UNRELIABILTY INPUT FILE=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9451) FLUNREL
      STOP 8
      ELSE
      CALL PREPIO(FLUNREL,90)
      END IF
      IF(CLUNREL.EQ.0) THEN
      WRITE(26,9452)
      WRITE(*,9452)
 9452 FORMAT(/1X,'RCTL 9452 (W) COEFFICIENT ON LINK UNRELIABILITY'
     *           ' IS ZERO')
      STOP
      END IF
      END IF
C
C     READ & CHECK FOR STOP UNRELIABILITY MATRIX
C
      IF(NUNREL) THEN
      INQUIRE (FILE=FNUNREL,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9453) FNUNREL
 9453 FORMAT(1X,'RCTL 9453 (F) NODE UNRELIABILTY INPUT FILE=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9453) FNUNREL
      STOP 8
      ELSE
      CALL PREPIO(FNUNREL,91)
      END IF
      IF(CNUNREL.EQ.0) THEN
      WRITE(26,9454)
      WRITE(*,9454)
 9454 FORMAT(/1X,'RCTL 9454 (W) COEFFICIENT ON NODE UNRELIABILITY'
     *           ' IS ZERO')
      STOP
      END IF
      END IF
C
C     READ & CHECK FOR LINK CROWDING MATRIX
C
      IF(LCROWD) THEN
      INQUIRE (FILE=FCROWD,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9455) FCROWD
 9455 FORMAT(1X,'RCTL 9455 (F) LINK CROWDING INPUT FILE=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9455) FCROWD
      STOP 8
      ELSE
      CALL PREPIO(FCROWD,92)
      END IF
      IF(CLCROWD.EQ.0) THEN
      WRITE(26,9456)
      WRITE(*,9456)
 9456 FORMAT(/1X,'RCTL 9456 (W) COEFFICIENT ON LINK CROWDING'
     *           ' IS ZERO')
      STOP 
      END IF
      END IF
C
C     READ & CHECK FOR STOP CAPACITY MATRIX
C
      IF(NCAPAC) THEN
      INQUIRE (FILE=FCAPAC,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9457) FCAPAC
 9457 FORMAT(1X,'RCTL 9457 (F) NODE CAPACITY INPUT FILE=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9457) FCAPAC
      STOP 8
      ELSE
      CALL PREPIO(FCAPAC,93)
      END IF
      IF(CNCAPAC.EQ.0) THEN
      WRITE(26,9458)
      WRITE(*,9458)
 9458 FORMAT(/1X,'RCTL 9458 (W) COEFFICIENT ON STOP CAPACITY'
     *           ' IS ZERO')
      STOP
      END IF
      END IF
C
C     READ & CHECK FOR BIKE LINK UNRELIABILITY MATRIX
C
      IF(LUNREL) THEN
      INQUIRE (FILE=FLUNRELBK,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9551) FLUNRELBK
 9551 FORMAT(1X,'RCTL 9551 (F) LINK UNRELIABILTY BIKE INPUT FILE=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9551) FLUNRELBK
      STOP 8
      ELSE
      CALL PREPIO(FLUNRELBK,100)
      END IF
      END IF
C
C     READ & CHECK FOR BIKE STOP UNRELIABILITY MATRIX
C
      IF(NUNREL) THEN
      INQUIRE (FILE=FNUNRELBK,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9553) FNUNRELBK
 9553 FORMAT(1X,'RCTL 9553 (F) NODE UNRELIABILTY BIKE INPUT FILE=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9553) FNUNRELBK
      STOP 8
      ELSE
      CALL PREPIO(FNUNRELBK,101)
      END IF
      END IF
C
C     READ & CHECK FOR BIKE LINK CROWDING MATRIX
C
      IF(LCROWD) THEN
      INQUIRE (FILE=FCROWDBK,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9555) FCROWDBK
 9555 FORMAT(1X,'RCTL 9555 (F) LINK CROWDING BIKE INPUT FILE=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9555) FCROWDBK
      STOP 8
      ELSE
      CALL PREPIO(FCROWDBK,102)
      END IF
      END IF
C
C     READ & CHECK FOR STOP CAPACITY MATRIX
C
      IF(NCAPAC) THEN
      INQUIRE (FILE=FCAPACBK,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9557) FCAPACBK
 9557 FORMAT(1X,'RCTL 9557 (F) NODE CAPACITY BIKE INPUT FILE=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9557) FCAPACBK
      STOP 8
      ELSE
      CALL PREPIO(FCAPACBK,103)
      END IF
      END IF
C
C     OPEN & CHECK FOR WALK AND BICYCLE LOGSUM MATRICES
C
      IF(.NOT.WALKTIME) THEN
      INQUIRE (FILE=FWLKLSM,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9459) FWLKLSM
 9459 FORMAT(1X,'RCTL 9459 (F) WALK MODE LOGSUM INPUT FILE=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9459) FWLKLSM
      STOP 8
      ELSE
      OPEN(84,FILE=FWLKLSM,STATUS='OLD',FORM='BINARY')
      END IF
      END IF
      IF(BICYCLE) THEN
      INQUIRE (FILE=FBYCLSM,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9460) FBYCLSM
 9460 FORMAT(1X,'RCTL 9460 (F) BICYCLE MODE LOGSUM INPUT FILE=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9460) FBYCLSM
      STOP 8
      ELSE
      OPEN(85,FILE=FBYCLSM,STATUS='OLD',FORM='BINARY')
      END IF
      END IF
C
C     READ SPECIAL EVENT VENUE FILE (IF REQUESTED)
C
      IF(SPEVENT) THEN
      INQUIRE (FILE=FVENUE,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9615) FVENUE
 9615 FORMAT(1X,'RCTL 9615 (F) SPECIAL EVENT VENUE FILE FVENUE=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9615) FVENUE
      STOP 8
      ELSE
      OPEN(13,FILE=FVENUE,
     *        STATUS='OLD',FORM='FORMATTED')
      END IF
      READ(13,9626,ERR=9617) HEADER
      WRITE(26,9635)
 9635 FORMAT(//' SPECIAL EVENT VENUE DATA'/
     *       ' ------------------------'//
     *       '        AVERAGE   PRK'/
     *       ' ZONE    ATTEND   COST'/
     *       ' ----  --------  -----')
      INDEX=0
 9626 FORMAT(A80)
 9638 READ(13,*,ERR=9617,END=9639) ZONE,ATTEND,PKCOST(1)
      WRITE(26,9636) ZONE,ATTEND,PKCOST(1)
 9636 FORMAT(I5,2X,F8.0,2X,F5.0)
      INDEX=INDEX+1
      IF(ZONE.LE.0.OR.ZONE.GT.MAX_IZONES) THEN
      WRITE(26,9630) ZONE
 9630 FORMAT(/1X,'RCTL 9630 (F) ENCOUNTERED ZONE NUMBER (',I5,
     *           ') OUT OF RANGE')
      STOP 16
      END IF
      ZHHD(19,ZONE)=ATTEND
      ZHHD(20,ZONE)=PKCOST(1)
      ZHHD(5,ZONE)=PKCOST(1)
      SPEQUIV(ZONE)=INDEX
      GO TO 9638
 9617 WRITE(26,9632) ZONE
 9632 FORMAT(/1X,'RCTL 9617 (F) ENCOUNTERED ERROR IN',
     *        ' READING FROM SPECIAL ',
     *        'EVENT VENUE FILE FOR ZONE ',I5)
      STOP 16
 9639 CONTINUE
      CLOSE(13,STATUS='KEEP')
      WRITE(26,9641) RPOP,REMP,RHH3
 9641 FORMAT(/' TOTAL REGIONAL POPULATION=',F10.0/
     *        ' TOTAL REGIONAL EMPLOYMENT=',F10.0/
     *        ' TOTAL HIGH INC HOUSEHOLDS=',F10.0/)
      END IF
C-------------------------------------------------------------
C     READ & STORE STATION ACCESS/EGRESS VALUES FOR
C     NON-HOME BASED DIRECT DEMAND MODEL
C
      IF(NHBDIR) CALL ACCEGR
C-------------------------------------------------------------
C     READ & STORE LOW INCOME PERCENTAGES AND DISTRICT EQUIVALENCE
C
      IF(LOWRAIL.AND.NCATS.EQ.1) THEN
      LDISTEQ=0
      LOWDIST=0.0
      INQUIRE (FILE=FLOWDIST,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9363) FLOWDIST
 9363 FORMAT(1X,'RCTL 9363 (F) LOW INCOME DISTRICT (FLOWDIST)=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9363) FLOWDIST
      STOP 8
      ELSE
      OPEN(133,FILE=FLOWDIST,
     *        STATUS='OLD',FORM='FORMATTED')
      END IF    
      READ(133,'(A80)') HEADER  
  200 READ(133,*,END=210,ERR=209) ZONE,LDIST
      LDISTEQ(ZONE)=LDIST
      GO TO 200
  209 WRITE(26,9364) ZONE
 9364 FORMAT(1X,' RCTL 9364 (F) ERROR READING LOW INCOME DISTRICT',
     *          ' FILE NEAR ZONE ',I4)
      STOP 8
  210 CONTINUE
C..
      INQUIRE (FILE=FLOWINC,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9365) FLOWINC
 9365 FORMAT(1X,'RCTL 9365 (F) LOW INCOME PERCENTAGES (FLOWINC)=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9365) FLOWINC
      STOP 8
      ELSE
      OPEN(132,FILE=FLOWINC,
     *        STATUS='OLD',FORM='FORMATTED')
      END IF    
      READ(132,'(A80)') HEADER  
  220 READ(132,*,END=230,ERR=229) ZONE,LSHARE
      LOWDIST(ZONE)=LSHARE
      GO TO 220
  229 WRITE(26,9366) ZONE
 9366 FORMAT(1X,' RCTL 9366 (F) ERROR READING LOW INCOME PERCENTAGE',
     *          ' FILE NEAR DISTRICT ',I4)
      STOP 8
  230 CONTINUE      
      END IF
C
C     ESCOOTER INDICATOR FILE
C
      IF(ESCOOTER) THEN
      INQUIRE (FILE=FESCOOTER,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9367) FESCOOTER
 9367 FORMAT(/1X,'RCTL 9367 (F) ESCOOTER INDICATOR FILE (ESCOOTER)=',
     *       A40,' DOES NOT EXIST'/)
      WRITE(*,9367) FESCOOTER
      STOP 8
      ELSE
      OPEN(145,FILE=FESCOOTER,
     *        STATUS='OLD',FORM='FORMATTED')
      END IF        
      READ(145,'(A80)') HEADER    
  235 READ(145,*,END=240,ERR=239) ZONE,XC,YC,LSHARE
      ZHHD(30,ZONE)=LSHARE
      GO TO 235
  239 WRITE(26,9368) ZONE
 9368 FORMAT(1X,' RCTL 9368 (F) ERROR READING ESCOOTER ',
     *          ' INDICATOR VALUE NEAR ZONE ',I4)
      STOP 8
  240 CONTINUE        
      END IF
C--------------------------------------------------------------------
      CLOSE(3,STATUS='KEEP')
      CLOSE(9,STATUS='KEEP')
      CLOSE(12,STATUS='KEEP')
      CLOSE(15,STATUS='KEEP')
      CLOSE(48,STATUS='KEEP')
      CLOSE(76,STATUS='KEEP')
      OPEN(46,FILE=FACCEGR,STATUS='UNKNOWN',FORM='FORMATTED')
      RETURN 
      END
C-------------------------------------------------------------------
C      READ AND RETURN VALUE FROM STATION RECORD STRING
C-------------------------------------------------------------------
      SUBROUTINE STAREAD(K1,K2,SRECORD)
      INTEGER*2     K1,K2,K3
      CHARACTER*300 SRECORD
      CHARACTER*1   COMMA
      DATA          COMMA/','/
      K2=0
      DO 9000 K=K1,300
      IF(SRECORD(K:K).EQ.COMMA) THEN
       K2=K-1
      RETURN
      END IF
 9000 CONTINUE
      K2=K1+20
      IF(K2.GT.300) K2=300
      RETURN
      END
C ----------------------------------------------------------------
C    INSERT BLANKS FOR MISSING FIELDS
C ---------------------------------------------------------------
      SUBROUTINE REFORM(SRECORD,TEMP)
      CHARACTER*200 SRECORD
      CHARACTER*300 TEMP
      CHARACTER*1   COMMA,BLANK
      INTEGER*2     K,K2
      DATA          COMMA/','/,BLANK/' '/
      K1=0
      DO K=1,200
      K1=K1+1
      IF(SRECORD(K:K).NE.COMMA) THEN
      TEMP(K1:K1)=SRECORD(K:K)
      ELSE
      TEMP(K1:K1)=SRECORD(K:K)
C     WRITE(26,9001) K,K1,SRECORD(K:K),TEMP(K1:K1)
       K2=K+1
       IF(SRECORD(K2:K2).EQ.COMMA) THEN
       K1=K1+1
       TEMP(K1:K1)=BLANK
       END IF
      END IF
C     WRITE(26,9001) K,K1,SRECORD(K:K),TEMP(K1:K1)
C9001 FORMAT(' K=',I3,' K1=',I3,' SRECORD(K)=',A1,
C    *       ' TEMP(K1)=',A1)
      END DO
C     WRITE(26,9002) TEMP
C9002 FORMAT(A300)    
      RETURN
      END 
