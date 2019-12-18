      PROGRAM MTAMC
C      
C      
C---------------------------------------------------------------------
C             NESTED LOGIT MODE CHOICE MODEL APPLICATION PROGRAM
C
C             HOME-BASED WORK, HOME-BASED NON-WORK 
C             AND NON-HOME BASED MODE CHOICE 
C                                                          
C             WITH STATION CHOICE FOR URBAN & COMMUTER RAIL
C
C
C            PARSONS BRINCKERHOFF QUADE & DOUGLAS, INC.
C                 SAN FRANCISCO, CALIFORNIA
C
C
C---------------------------------------------------------------------
C
C      WRITTEN FOR TRANPLAN I/O
C
      INCLUDE 'stadat.com'
      INCLUDE 'param.com'
      INCLUDE 'mtamcpar.inc'
      include 'tpcom.inc'
      include 'control.inc'
C
C DECLARATIONS REQUIRED FOR MAIN PROGRAM
C
C     INTEGER*2     IHR,IMIN,ISEC,I100
      INTEGER*4     IARRAY2(3),TBRTT
      INTEGER*2     IZ,JZ,C,M,ITEMP,ITEMP2,II,IN,IJ,JJ,MT,E,KT,MT2
      INTEGER*2     HOV3TOL,HOV3NTL,HOV2TOL,HOV2NTL,DIZ,DJZ
      INTEGER*2     HOV4TOL,HOV4NTL,ITEMP1
      INTEGER*2     IMODE,CSTAE,CDSTAE,EQUIV(MAX_STATIONS),BESTMODE(2)
      INTEGER*2     ZONESTA(MAX_IZONES),MODINC(18),CCSTAE,CCSTA,CCDSTA
      INTEGER*2     CBSTA,CBDSTA,CBSTAE,CWEXP,CWBRT,CWTWY,CCODE
C
      INTEGER*2     ZINDCR(MAX_STATIONS),ZINDUR(MAX_STATIONS),RALTXF(12)
      INTEGER*2     ORISTA,DESSTA,TSTA,TDSTA,SC1,SC2,SC3,SC4,SC5,SC
      INTEGER*2     WSTA(2,5),WDSTA(2,5),BSTA(2,2),BDSTA(2,2),
     *              DSTA(2,10),IUSED,OSTA(2,12),ASTA(2,12),ASTA2(2,12),
     *              BTXFER(2,2),IC,DC,KTX,KTY,BESTURI,BESTZONE(14,2)
      INTEGER*2     BUSMODE(2,5,2),BESTCRI,AIRPAS(2)
      INTEGER*2     STAINDC(MAX_STATIONS,MAX_STATIONS)
      INTEGER*2     STAINDC2(MAX_ZONES,MAX_STATIONS)
      INTEGER*2     STAINDC3(MAX_STATIONS,MAX_ZONES)
      INTEGER*2     USERBENC(12,3,MAX_ZONES),USERBENC2(12,3,MAX_ZONES)
      INTEGER*2     USERBENC3(3,MAX_ZONES),ISTA
      INTEGER*2     PSTA(2,10),KSTA(2,10),PDSTA(2,10),KDSTA(2,10)
      INTEGER*2     IX,T,KS,L,CITER,ITER,CSTAT,CDSTAT,NCATS
      INTEGER*2     CSTABRT,CDSTABRT,MODEINC(6,6,12,2)
      INTEGER*2     STAZNEI(MAX_STATIONS,MAX_IZONES,6,6)
      INTEGER*2     SELIND(MAX_STATIONS,MAX_STATIONS)
      INTEGER*2     ZSTAIND(MAX_ZONES,MAX_STATIONS)
      INTEGER*2     STAZIND(MAX_STATIONS,MAX_ZONES)
      INTEGER*2     ZSTASTA(MAX_ZONES,MAX_STATIONS,2)
      INTEGER*2     STAZSTA(MAX_STATIONS,MAX_ZONES,2)
      INTEGER*4     K,K1,K2,ITOT,WLKIND,DRVIND,PINDEX,TINDEX
      INTEGER*4     ROW(MAX_ZONES),ICO,LRTWLK(MAX_ZONES)
      INTEGER*4     LRTDRV(MAX_ZONES)
      INTEGER*4     STAZONE(MAX_STATIONS,MAX_IZONES,2)
      INTEGER*4     WKPATH(16,MAX_ZONES),DRPATH(16,MAX_ZONES)
      INTEGER*4     BUSTAT(3,MAX_ZONES),AVLPERT(54,MAX_ZONES)
      INTEGER*4     OBSVAL(52,MAX_ZONES)
C
      REAL*4        WUTIL(2,5),WDIST(2,5),BUTIL(2,2),BDIST(2,2),TUTIL
      REAL*4        PDIST(2,10),PUTIL(2,10),KDIST(2,10),KUTIL(2,10)
      REAL*4        URBSKIM(2,12),CRBSKIM(2,12)
      REAL*4        USED,WLKM1,WLKM2,WLKM,NHHD(3),TOTHH
      REAL*4        WLKBACC,WLKBEGR,WLKCACC,WLKCEGR,WLKWTACC
      REAL*4        WLKWBRTACC,WLKWBRTEGR
      REAL*4        WLKRACC,WLKREGR,WRUTL
      REAL*4        WLKWTEGR,rem,tnew,temp
      REAL*4        WBUTL,WCUTL,DCUTL,DCUTLCR,WTUTL
      REAL*4        WBRTUTL,DBRTUTL,DCUTLBR
      REAL*4        ZHHD(18,MAX_IZONES),KKUR(3)
      REAL*4        WALKACC(5,MAX_ZONES),FARE(5,MAX_ZONES)
      REAL*4        WALKEGR(5,MAX_ZONES),WALKTFR(5,MAX_ZONES)
      REAL*4        TIVT(5,MAX_ZONES),WAIT1(5,MAX_ZONES)
      REAL*4        WAIT2(5,MAX_ZONES),RIVT(5,MAX_ZONES)
      REAL*4        TXFER(5,MAX_ZONES),BIVT(5,MAX_ZONES)
      REAL*4        WIVT(5,MAX_ZONES),EIVT(5,MAX_ZONES)
      
      REAL*8        UTIL0NT,UTIL0T,WALKT,BIKET,UTILWK,UTILBK
      REAL*8        UTIL2NT,UTIL2T,UTIL3NT,UTIL3T,UTIL4T,UTIL4NT
      REAL*8        UTIL2NTH,UTIL2TH,UTIL3NTH,UTIL3TH,UTIL4NTH,UTIL4TH
      REAL*4        MWALKW(2,2),MWALKB(2,2),PNRRAT2
      REAL*4        NOWALK,TDIST,PNRRAT,CRPNR(10),CRDUM(10),CRPNR2(10)
      REAL*4        PERIN(3,MAX_ZONES),MWALK(7),TWALK(2),TSHAR(2)
      REAL*4        PERSON(MAX_ZONES),MAXWALK
      REAL*4        BWALK,RWALK,CWALK,DWALK,DBWLK,KDRIV,DRWLK
      REAL*4        BWALK1,BWALK2,RWALK1,RWALK2,CWALK1,CWALK2
      REAL*4        TWWALK1,TWWALK2,WALK1,WALK2,BIKE1,BIKE2
      REAL*4        TBRTWALK1,TBRTWALK2,TBRTWALK,TDBRTWALK
      REAL*4        CWPROB(3),CBPROB(3),CPPROB(5),CKPROB(5)
      REAL*4        UWPROB(3),UBPROB(3),UPPROB(5),UKPROB(5)
      REAL*4        BWPROB(3),NMPROB(2)
      REAL*4        DAPROB(2),P2PROB(4),P3PROB(4),BWYPROB(2)
      REAL*4        HHFACT(3),KCCBD(7),KLAX,P4PROB(4)
      REAL*8        USERBENT(12,3,MAX_ZONES),USERBENT2(12,3,MAX_ZONES)
      REAL*8        USERBENT3(3,MAX_ZONES)
C
      REAL*8        TRIPS(52,MAX_ZONES),LTRIPS(19,MAX_ZONES)
      REAL*8        OBSVALR(24,MAX_ZONES),FAREVAL(23)
      REAL*8        TOLSUM(14,4),CRPNRSUM(1000)
      REAL*8        PERTRP,TESUM(64,4),CALSUM(5,3,5),TXSUM(13,5)
      REAL*8        CRPNRRAT(1000),EXPCRT(4),LOCBUS(5,3,2)
      REAL*8        PTRIP(21,3),TTRIP(21,3),TTRIP2(21,3,7)
      REAL*8        STXFER(3,4,MAX_ZONES),XTRIP(MAX_ZONES)
      REAL*8        TWYEXP(101,3,2,3),TWYEXP2(100,3,3),MDETRP(12)
      REAL*8        LSCWLK,LSCBUS,LSCPR,LSCKR,LSCR
      REAL*8        LSUWLK,LSUBUS,LSUPR,LSUKR,LSURB
      REAL*8        LSSHR,TXTRIP(9,MAX_ZONES),STTRIP(4,MAX_ZONES)
      REAL*8        LSLOC,LSEXP,LSWAY,LSTRN,LSDA,LSMOT,LSRPD,LSBRT
      REAL*8        LSAUTO,STASUM(MAX_STATIONS,18),TTWLK,TTDRV,TTOTAL
      REAL*8        STASUM2(MAX_STATIONS,7),MODETRP(6,7,12,2),XPCT(2)
      REAL*8        LS3PER,LS2PER,LSNMOT,TTBUS,TTPNR,TTKNR,LS4PER
      REAL*4        BUSPROB(2),EXPPROB(2),RPDPROB(2),BRTPROB(2)
      REAL*4        SRPROB(3),TRNPROB(7)
      REAL*4        ATPROB(2),MOTOR(3)
      REAL*4        ADIST,PWALK(MAX_ZONES,2)
      REAL*4        STASTA(4,MAX_STATIONS,MAX_STATIONS)
      REAL*4        STAZNE(4,MAX_STATIONS,MAX_IZONES)
      REAL*4        EXPZNE(4,MAX_STATIONS,MAX_STATIONS)
      REAL*4        BRTZNE(4,MAX_STATIONS,MAX_STATIONS)
      REAL*4        STASTAD(3,9,BMAX_STATIONS,BMAX_STATIONS)
      REAL*4        STAZNED(5,15,BMAX_STATIONS,BMAX_IZONES)
      REAL*4        SSDIST(MAX_STATIONS,MAX_STATIONS)
      REAL*4        TAB1DA(MAX_ZONES),TAB2DA(MAX_ZONES)
      REAL*4        TAB3DA(MAX_ZONES)
      REAL*4        TAB4DA(MAX_ZONES),TAB5DA(MAX_ZONES)
      REAL*4        TAB6DA(MAX_ZONES)
      REAL*4        TAB8DA(MAX_ZONES)
      REAL*4        TAB12P(MAX_ZONES),TAB22P(MAX_ZONES)
      REAL*4        TAB32P(MAX_ZONES)
      REAL*4        TAB42P(MAX_ZONES),TAB52P(MAX_ZONES)
      REAL*4        TAB62P(MAX_ZONES)
      REAL*4        TAB72P(MAX_ZONES),TAB82P(MAX_ZONES)
      REAL*4        TAB13P(MAX_ZONES),TAB23P(MAX_ZONES)
      REAL*4        TAB33P(MAX_ZONES)
      REAL*4        TAB43P(MAX_ZONES),TAB53P(MAX_ZONES)
      REAL*4        TAB63P(MAX_ZONES)
      REAL*4        TAB73P(MAX_ZONES),TAB83P(MAX_ZONES)
      REAL*4        TAB14P(MAX_ZONES),TAB24P(MAX_ZONES)
      REAL*4        TAB34P(MAX_ZONES)
      REAL*4        TAB44P(MAX_ZONES),TAB54P(MAX_ZONES)
      REAL*4        TAB64P(MAX_ZONES)
      REAL*4        TAB74P(MAX_ZONES),TAB84P(MAX_ZONES)
C
      REAL*8        TTRAN,TAUTO,TDRV0,TDSHR
      REAL*8        TNMOT,TNMWK,TNMBK
      REAL*8        TCR,TEXP,TLOC,TUR,TWAY,TDRV0N,TDRV0T
      REAL*8        TRPD,TRPDW,TRPDD,TBRT,TBRTW,TBRTD
      REAL*8        TDRV2,TDRV3,TDRV2N,TDRV2T,TDRV3N,TDRV3T
      REAL*8        TLOCW,TLOCD,TEXPW,TEXPD
      REAL*8        TCRW,TCRB,TCRP,TCRK,TCRW1,TCRW2
      REAL*8        TCRB1,TCRB2,TCRP1,TCRP2,TCRP3,TCRP4
      REAL*8        TCRK1,TCRK2,TCRK3,TCRK4
      REAL*8        TURW,TURB,TURP,TURK,TURW1,TURW2
      REAL*8        TURB1,TURB2,TURP1,TURP2,TURP3,TURP4
      REAL*8        TURK1,TURK2,TURK3,TURK4
      REAL*8        TWAYW,TWAYD
      REAL*8        TDRV2NH,TDRV2NN,TDRV2TH
      REAL*8        TDRV2TN,TDRV3NH,TDRV3NN
      REAL*8        TDRV3TH,TDRV3TN,TDRV4TH,TDRV4TN
      REAL*8        TDRV4,TDRV4N,TDRV4T,TDRV4NH,TDRV4NN
C
      REAL*4        TOLSAV0,HOVSAV2,BCR(MAX_IZONES,MAX_STATIONS)
      REAL*4        BUR(MAX_IZONES,MAX_STATIONS),YTOLDST
      REAL*4        DTRAN(MAX_IZONES,MAX_STATIONS)
      REAL*4        CRSS(MAX_STATIONS,MAX_STATIONS)
      REAL*4        URSS(MAX_STATIONS,MAX_STATIONS)
      REAL*4        EBSTAZ(MAX_STATIONS,MAX_IZONES)
      REAL*4        TWSTAZ(MAX_STATIONS,MAX_IZONES)
      REAL*4        BRTSTAZ(MAX_STATIONS,MAX_IZONES)
      REAL*4        CRSTAZ(MAX_STATIONS,MAX_IZONES)
      REAL*4        URSTAZ(MAX_STATIONS,MAX_IZONES)
      REAL*4        LOGMIN(3,3),LOGMAX(3,3)
      REAL*4        ALOGSUM(6,MAX_ZONES),TLOGSUM(6,MAX_ZONES)
      REAL*4        ULOGSUM(6,MAX_ZONES),AUTEXP,CBDTRAN
      REAL*8        UTIL(89),EUTIL(89),DENOM
      REAL*8        WKBEST,DRBEST,BESTUR
      REAL*4        VALUES(15),VALUES2(15),VALUES3(15)
      REAL*8        CRPCT(MAX_ZONES,3,2)
      REAL*8        PNRTRP(8)
      REAL*4        STATRIPS(MAX_STATIONS,MAX_STATIONS)
      REAL*4        STATRIPS2(MAX_STATIONS,MAX_STATIONS)
      REAL*4        STATRIPS3(MAX_STATIONS,MAX_STATIONS)
      REAL*4        SELTRIPS(15,3)
      REAL*4        ZSTATRIPS(MAX_ZONES)
      REAL*4        ZSTATRIPS2(MAX_ZONES)
      REAL*4        ZSTATRIPS3(MAX_ZONES)
      REAL*4        SELDIST(21,21,3)
      INTEGER*4     CRPCTZ(MAX_ZONES)
      CHARACTER*1   FF,BLANK
      CHARACTER*12  COUNTY(5)
      LOGICAL       CSORT,EXT,TXOUT,STRHBW,EXISTS,UNCON,EXPCR,EBUSCR
      LOGICAL       BRTUR,STAMTH,EVENTSP,RAILSEL
      CHARACTER*13  NAME(2),BESTNAME(8)
      CHARACTER*20  DNAME(21),SNAME(3)
C
C GENERAL DATA DEFINITIONS
C
      DATA          CITER/0/,ITER/0/,BLANK/' '/
      DATA          LOGMIN/9*999.0/,LOGMAX/9*-999.0/
      DATA          NAME/'Commuter Rail',
     *                   'Urban Rail   '/
      DATA          BESTNAME/'Commuter Rail',
     *                   'Urban Rail   ',
     *                   'Local Bus    ',
     *                   'Express Bus  ',
     *                   'Transitway   ',
     *                   'Not Found    ',
     *                   'BRT          ',
     *                   'Metro Rapid  '/
      DATA        COUNTY/'LOS ANGELES ','ORANGE     ','RIVERSIDE  ',
     *                   'SAN BERNARD ','VENTURA    '/
      DATA        DNAME/'1','2','3','4','5','6','7','8','9','10',
     *                  '11','12','13','14','15','16','17','18',
     *                  '19','20','Total'/
      DATA        SNAME/'Station-to-Station','CR_Zone-to-Station',
     *                  'CR_Station-to-Zone'/
C----------------------------------------------------------------------
C
C     machin='IBMAIX'
      FF=CHAR(12)
      CSTABRT=0
      SELDIST=0.0
C               
C CONTROL FILE PARAMETERS
C
      CALL RCTL(PWALK,HOV2P,HOV3P,HOV4P,ZHHD,EQUIV,HHFACT,KKUR,EBUSCR,
     *          DNAME)
C
C   OPEN OUTPUT FILES
C
      IF(TRIPSOUT.OR.CALIB) THEN
      OPEN(21,FILE=HWYOUT,FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN(22,FILE=CROUT,FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN(23,FILE=UROUT,FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN(24,FILE=BUSOUT,FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN(25,FILE=NBUSOUT,FORM='UNFORMATTED',STATUS='UNKNOWN')
      ELSE
      IF(BESTPATH) THEN
       IF(FBESTPATH.NE.BLANK) THEN
       OPEN(21,FILE=FBESTPATH,FORM='UNFORMATTED',
     *             STATUS='UNKNOWN')
       ELSE
       WRITE(26,8045)
 8045  FORMAT(/' MTAMC 8045 (F) MISSING DATA SET NAME FOR ',
     *        ' BESTPATH OUTPUT MATRIX'/)
       WRITE(*,8045)
       STOP 16
       END IF
      END IF
      END IF
      IF(LOGSUM) 
     * OPEN(28,FILE=LOGSUMS,FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(INCLEV.GT.0)
     * OPEN(30,FILE=INCOUT,FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(LRTIND) 
     * OPEN(35,FILE='LRTIND.MTX',FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(CRTIND)
     * OPEN(35,FILE='CRTIND.MTX',FORM='UNFORMATTED',STATUS='UNKNOWN') 
      call prepio(bcrsk,10)
      call prepio(bursk,12)
      call prepio(wbssk,13)
      call prepio(websk,14)
      IF(TWYSK) call prepio(wtwsk,15)
      call prepio(wrbsk,16)
      IF(BRTSK) CALL PREPIO(WBRTSK,17)
      call prepio(dask,18) 
      call prepio(p2sk,19) 
      call prepio(p3sk,20) 
      IF(SKMP4) call prepio(p4sk,37)
      IF(STNAC) call prepio(stnacc,36)
      IF(CALIB.AND.PERAVL) 
     *  OPEN(45,FILE=AVLPER,FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(OUTPER) 
     *  OPEN(46,FILE=PEROUT,FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(MTXOBS)
     *  OPEN(66,FILE=OBSMTX,FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(REVEST)
     *  OPEN(68,FILE=REVENUE,FORM='UNFORMATTED',STATUS='UNKNOWN')
C
C  READ & STORE STATION TO ZONE TRAVEL TIME
C
      DO 12 IZ=MAX_IZONES+1,MAX_ZONES
      PURP=1
      UNCON=.FALSE.
      CALL INTAB(18,VAR,IZ,PURP,DUMMY,IO)
      DO 11,NI=1,MAX_IZONES
      SC=IZ-MAX_IZONES
      IF(SC.LE.0) STOP 16
      STAZONE(SC,NI,1)=VAR(NI)
      IF(STADATA(SC,6).EQ.1.AND.VAR(NI).LE.0) THEN
      IF(.NOT.UNCON) WRITE(26,314) IZ,NI
  314 FORMAT(/' MTAMC 314 (W) STATION ',I4, ' WAS NOT CONNECTED TO',
     *       ' INTERNAL ZONE ',I4)
      UNCON=.TRUE.
      END IF
   11 CONTINUE
      IF(UNCON) WRITE(26,13) IZ
   13 FORMAT(' MTAMC 13 (W) STATION ',I4,' HAS AT LEAST',
     *       ' ONE UNCONNECTED DESTINATION ZONE VIA DRIVE')
C
C  READ & STORE STATION-TO-STATION DISTANCE
C
      PURP=2
      CALL INTAB(18,VAR,IZ,PURP,DUMMY,IO)
      DO 14 JZ=MAX_IZONES+1,MAX_ZONES
      SC2=JZ-MAX_IZONES
      SSDIST(SC,SC2)=VAR(JZ)/100.0
   14 CONTINUE
C
C  STORE STATION TO ZONE TRAVEL DISTANCE
C
      DO 112,NI=1,MAX_IZONES
      SC=IZ-MAX_IZONES
      STAZONE(SC,NI,2)=VAR(NI)
  112 CONTINUE
   12 CONTINUE
      CLOSE(18,STATUS='KEEP')
      CALL PREPIO(DASK,18)
      call prepio(pertt,8)
C
C
C  CHECK NUMBER OF INPUT HBW PERSON TRIP MATRICES
C
      STRHBW=.FALSE.
      IF((HBW).AND.(NUMPUR.EQ.3)) STRHBW=.TRUE.
C
C  write headers for output trip tables
C
      ftype='VOLUME'
      IF(LRTIND.OR.CRTIND) THEN
      NUMPUR=2
      TABLES=2**2-1
      WRITE(35) HEAD1,HEAD2
      END IF
      IF(TRIPSOUT.OR.CALIB) THEN
	    numpur=14
      tables=2**14-1
	    write(21) HEAD1,HEAD2
      NUMPUR=3
	    tables=2**3-1
      WRITE(22) HEAD1,HEAD2
      WRITE(23) HEAD1,HEAD2
      NUMPUR=4
      WRITE(25) HEAD1,HEAD2
      NUMPUR=19
      TABLES=2**15-1
      WRITE(24) HEAD1,HEAD2
      IF(INCLEV.GT.0) WRITE(30) HEAD1,HEAD2
      DO 5 IT=1,20
      IF(SELSTA(IT).GT.0) THEN
      TXOUT=.TRUE.
      OPEN(30,FILE=TXMOUT,FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(.NOT.TRIPSOUT) THEN
      NUMPUR=9
      TABLES=2**9-1
      ELSE
      NUMPUR=4
      TABLES=2**4-1
      END IF
      SC=SELSTA(IT)-MAX_IZONES
      IF(STADATA(SC,6).NE.1.0) THEN
      WRITE(26,9339) SELSTA(IT)
 9339 FORMAT(/1X,'SELECT STATION (',I4,') IS NOT AN ACTIVE',
     *           ' STATION')
      WRITE(*,9339) SELSTA(IT)
      STOP 16
      END IF
      END IF
    5 CONTINUE
      IF((INCLEV.GT.0).AND.TXOUT) THEN
      WRITE(26,9440) 
 9440 FORMAT(/1X,'INCOME SPECIFIC AND TRANSFER TRIP',
     *           ' TABLES CANNOT BE OUTPUT ',
     *           ' IN THE SAME RUN')
      STOP 16
      END IF
      IF(TXOUT) WRITE(30) HEAD1,HEAD2
      END IF
      IF(LOGSUM) THEN
      NUMPUR=18
      TABLES=2**15-1
      WRITE(28) HEAD1,HEAD2
      END IF
      IF(BESTPATH) THEN
      IF(TRIPSOUT) THEN
      WRITE(26,9441) 
 9441 FORMAT(/1X,'TRIPSOUT AND BESTPATH CANNOT BE OUTPUT IN THE ',
     *           ' SAME RUN')
      STOP 16
      END IF
      NUMPUR=32
      TABLES=2**15-1
      WRITE(21) HEAD1,HEAD2
      END IF
      IF(CRPRB) THEN
      OPEN(60,FILE=FCRPRB,STATUS='UNKNOWN',
     *        FORM='UNFORMATTED')
      NUMPUR=6
      TABLES=2**6-1
      WRITE(60) HEAD1,HEAD2
      END IF
      IF(CALIB.AND.PERAVL) THEN
      NUMPUR=54
      TABLES=2**15-1
      WRITE(45) HEAD1,HEAD2
      END IF
      IF(OUTPER) THEN
      NUMPUR=3
      TABLES=2**3-1
      WRITE(46) HEAD1,HEAD2
      END IF
      IF(MTXOBS) THEN
      NUMPUR=76
      TABLES=2**15-1
      WRITE(66) HEAD1,HEAD2
      END IF
      IF(REVEST) THEN
      NUMPUR=28
      TABLES=2**15-1
      WRITE(68) HEAD1,HEAD2
      END IF
C
C COMPUTE STATION --> STATION UTILITY VALUES
C
      IF(INDRAL) CALL RAILINDC(STAINDC,STAINDC2,STAINDC3)
      IMODE=1
	CALL STATION(STASTA,IMODE,STASTAD)
      IMODE=2
        CALL STATION(STASTA,IMODE,STASTAD)
      CALL STASEL(SELIND,ZSTAIND,STAZIND,RAILSEL,ZSTASTA,STAZSTA)
C      IMODE=3
C	CALL STATEXP(STASTA,IMODE)
C      IMODE=4
C        IF(TWYSK) CALL STATEXP(STASTA,IMODE)
      WRITE(*,9900)
 9900 FORMAT(/)
C
      CLOSE (10,STATUS='KEEP')
      CLOSE (12,STATUS='KEEP')
C      CLOSE (14,STATUS='KEEP')
C      CLOSE (15,STATUS='KEEP')
      CLOSE (16,STATUS='KEEP')
      call prepio(bcrsk,10)
      call prepio(bursk,12)
C      call prepio(websk,14)
C      IF(TWYSK) call prepio(wtwsk,15)
      call prepio(wrbsk,16)
C       
C COMPUTE STATION --> DESTINATION ZONE UTILITY VALUES
C
      IMODE=1
        CALL EGRESS(STAZNE,IMODE,ZONESTA,STAZNEI,STAZNED)
      IMODE=2
        CALL EGRESS(STAZNE,IMODE,ZONESTA,STAZNEI,STAZNED)
      IMODE=3 
	CALL EGREXP(STAZNE,IMODE,EXPZNE,STAZNEI,BRTZNE,STAZNED,STASTAD)
      IMODE=4
	IF(TWYSK) CALL EGREXP(STAZNE,IMODE,EXPZNE,STAZNEI,BRTZNE,
     *                  STAZNED,STASTAD)
      IMODE=5
      IF(BRTSK) CALL EGREXP(STAZNE,IMODE,EXPZNE,STAZNEI,BRTZNE,
     *                        STAZNED,STASTAD)
C ------------------------------------------------------
      IF(CALIB) THEN
      OPEN(37,FILE='CLOSEST_DEST.STA',STATUS='UNKNOWN',
     *        FORM='FORMATTED')
      WRITE(37,98)
   98 FORMAT('        CLOSEST'/
     *       '          DEST'/
     *       ' ZONE   STATION'/
     *       ' ----   -------')
      DO 96 IZ=1,MAX_IZONES
      IF(ZONESTA(IZ).GT.0) THEN
      IC=ZONESTA(IZ)-MAX_IZONES
      WRITE(37,97) IZ,ZONESTA(IZ),STANAME(IC)
   97 FORMAT(I5,2X,I7,1X,A37)
      END IF
   96 CONTINUE
      END IF
C -------------------------------------------------------
C
      CLOSE (14,STATUS='KEEP')
      CLOSE (15,STATUS='KEEP')
      CLOSE (16,STATUS='KEEP')
      CLOSE (17,STATUS='KEEP')
      IF(DEBUG) THEN
      CLOSE (31,STATUS='KEEP')
      CLOSE (27,STATUS='KEEP')
      CLOSE (33,STATUS='KEEP')
      CLOSE (34,STATUS='KEEP')
      CLOSE (32,STATUS='KEEP')
      OPEN(31,FILE='CRUR.STA',STATUS='OLD',FORM='BINARY')
      OPEN(27,FILE='EPTW.STA',STATUS='OLD',FORM='BINARY')
      OPEN(33,FILE='CRUR.EGR',STATUS='OLD',FORM='BINARY')
      OPEN(34,FILE='EPTW.EGR',STATUS='OLD',FORM='BINARY')
      OPEN(32,FILE='BRTUR.STA',STATUS='OLD',FORM='BINARY')
      END IF
      call prepio(websk,14)
      IF(TWYSK) call prepio(wtwsk,15)
      call prepio(wrbsk,16)
      IF(BRTSK) CALL PREPIO(WBRTSK,17)
C
 8000   continue
C
C INITIALIZE SUMMARY ARRAYS
C
      CBDTRAN=0.0
      DO 2,IT=1,4
	DO 3,IU=1,64
	TESUM(IU,IT)=0.0
        IF(IU.LE.14) TOLSUM(IU,IT)=0.0
  3   CONTINUE
  2   CONTINUE
      DO 7,IT=1,5
	DO 7,IU=1,3
	DO 7,IW=1,5
	CALSUM(IT,IU,IW)=0.0
	IF(IT.LE.2) LOCBUS(IW,IU,IT)=0.0
  7   CONTINUE
      DO 8,IT=1,18
      DO 8,IV=1,MAX_STATIONS
      STASUM(IV,IT)=0.0
      IF(IT.LE.7) STASUM2(IV,IT)=0.0
  8   CONTINUE
      DO 9 IT=1,13
      DO 9 IU=1,5
      TXSUM(IT,IU)=0.0
  9   CONTINUE
      DO 111 IT=1,1000
      CRPNRSUM(IT)=0.0
      CRPNRRAT(IT)=0.0
 111  CONTINUE
        DO 1230 K1=1,6
        DO 1231 K2=1,7
        DO 1232 K3=1,12
        DO 1233 K4=1,2
        MODETRP(K1,K2,K3,K4)=0.0
 1233   CONTINUE
 1232   CONTINUE
 1231   CONTINUE
 1230   CONTINUE
        STATRIPS=0.0
        STATRIPS2=0.0
        STATRIPS3=0.0
C
      CLOSE (10,STATUS='KEEP')
      CLOSE (12,STATUS='KEEP')
      call prepio(bcrsk,10)
      call prepio(bursk,12)
C
      IF(CALIB.or.capres) THEN
      CLOSE(13,STATUS='KEEP')
      CLOSE(14,STATUS='KEEP')
      CLOSE(15,STATUS='KEEP')
      CLOSE(16,STATUS='KEEP')
      CLOSE(17,STATUS='KEEP')
      CLOSE(18,STATUS='KEEP')
      CLOSE(19,STATUS='KEEP')
      CLOSE(20,STATUS='KEEP')
      CLOSE(8,STATUS='KEEP')
      CLOSE(36,STATUS='KEEP')
      CLOSE(37,STATUS='KEEP')
      call prepio(wbssk,13)
      call prepio(websk,14)
      IF(TWYSK) call prepio(wtwsk,15)
      call prepio(wrbsk,16)
      IF(BRTSK) CALL PREPIO(WBRTSK,17)
      call prepio(dask,18) 
      call prepio(p2sk,19) 
      call prepio(p3sk,20) 
      IF(SKMP4) call prepio(p4sk,37)
      call prepio(pertt,8)
      IF(STNAC) call prepio(stnacc,36)
      ENDIF
C       
C
C*********************************************************************
C     ORIGIN ZONE LOOP                                               *
C*********************************************************************
      IF(DEBUG) THEN
      WRITE(*,8006)
 8006 FORMAT(/,1X,'Mode Choice Model Computations --- Debug Analysis'/
     *         1X,'-------------------------------------------------'/)
      ELSE
       IF(BESTPATH) THEN
       WRITE(*,8046)
 8046 FORMAT(/,1X,'Mode Choice Model Computations --- ',
     *            'BestPath Analysis'/
     *         1X,'-----------------------------------',
     *            '-----------------'/)
       ELSE
      WRITE(*,8003)
 8003 FORMAT(/,1X,'Mode Choice Model Computations'/
     *         1X,'------------------------------'/)
       END IF
      END IF
C
C    START OF ORIGIN ZONE LOOP
C
      DO 100 IZ=1,MAX_ZONES
      nk=IZ
      nk=mod(nk,100)
      if(nk.EQ.0.and.(.not.debug).AND.(.NOT.BESTPATH)) WRITE(*,8002) IZ
 8002 FORMAT(' Processing Origin Zone=',I5)
      NK=MOD(NK,10)  
      IF(BESTPATH.AND.NK.EQ.0) THEN
      CALL GETTIM(IHR,IMIN,ISEC,I100)
      WRITE(*,8044) IZ,IHR,IMIN,ISEC
 8044 FORMAT(' Processing Origin Zone=',I5,' Time= ',I2,':',I2,':',I2)
      END IF
C
C INITIALIZE OUTPUT TRIPS ARRAY
C
      DO 22,IT=1,52
      DO 22,IJT=1,MAX_ZONES
      IF(IT.LE.19) LTRIPS(IT,IJT)=0.0
      IF(IT.LE.9)  TXTRIP(IT,IJT)=0.0
      IF(IT.LE.4)  STTRIP(IT,IJT)=0.0
      TRIPS(IT,IJT)=0.0
      IF(IT.LE.16) THEN
      WKPATH(IT,IJT)=0
      DRPATH(IT,IJT)=0
      END IF
 22   CONTINUE
      DO 25 IT=1,3
      DO 25 IJT=1,MAX_ZONES
      CRPCT(IJT,IT,1)=0.0
      CRPCT(IJT,IT,2)=0.0
      CRPCTZ(IJT)=0
 25   CONTINUE
      ZSTATRIPS=0.0
      ZSTATRIPS2=0.0
      ZSTATRIPS3=0.0
C
      IF(.NOT.IOI(IZ)) GOTO 1121
      IF(IZ.GT.MAX_IZONES) GOTO 1121
C
C COMPUTE HOUSEHOLD PROPORTIONS
C
      NHHD(1)=ZHHD(8,IZ)*HHFACT(1)
	NHHD(2)=ZHHD(9,IZ)*HHFACT(2)
	NHHD(3)=ZHHD(10,IZ)*HHFACT(3)
	TOTHH=NHHD(1)+NHHD(2)+NHHD(3)
      if(tothh.gt.0.0) then
	ZHHD(2,IZ)=NHHD(1)/TOTHH
	ZHHD(3,IZ)=NHHD(2)/TOTHH
	ZHHD(4,IZ)=NHHD(3)/TOTHH
	endif 
C---------------------------------------------------------------------
C                 STATION CHOICE MODEL APPLICATION
C                 1:  COMMUTER RAIL
C                 2:  URBAN RAIL
C---------------------------------------------------------------------
C
C   READ HIGHWAY SKIMS FOR STADRV SUBROUTINE - 
C   use drive-alone non-toll time
C
      PURP=1
	CALL INTAB(18,VAR,IZ,PURP,DUMMY,IO)
      DO 7001,NI=1,MAX_ZONES
      TAB1DA(NI)=FLOAT(VAR(NI))/100.0
 7001 CONTINUE
C
C   drive-alone non-toll distance
C
      PURP=2
      CALL INTAB(18,VAR,IZ,PURP,DUMMY,IO)
      DO 7002,NI=1,MAX_ZONES
      TAB2DA(NI)=FLOAT(VAR(NI))/100.0
 7002 CONTINUE
C
C OBTAIN TWO WALK ACCESS STATIONS
C
      IF(STAXY) THEN
      DO 10,IMODE=1,2
      CALL STAWLK(IZ,WSTA,WDIST,imode)
      OSTA(IMODE,1)=WSTA(imode,1)
      OSTA(IMODE,2)=WSTA(imode,2)
   10 CONTINUE
      END IF
C
C...OBTAIN TWO FEEDER BUS ACCESS STATIONS (IF ANY)
C
      DO 115,IMODE=1,2
      IF(IMODE.EQ.1) THEN
      CALL STABUS(IZ,BSTA,BDIST,MWALKB,BTXFER,imode,CRBSKIM,BUSMODE,
     *            WDIST,WSTA)
      ELSE
      CALL STABUS(IZ,BSTA,BDIST,MWALKB,BTXFER,imode,URBSKIM,BUSMODE,
     *            WDIST,WSTA)
      END IF
       IF(.NOT.STAXY) THEN
       OSTA(IMODE,1)=WSTA(imode,1)
       OSTA(IMODE,2)=WSTA(imode,2)
       END IF
      OSTA(IMODE,3)=BSTA(imode,1)
      OSTA(IMODE,4)=BSTA(imode,2)
 115  continue
C
C...OBTAIN TOP TEN DRIVE ACCESS STATIONS
C
C
      imode=1
	CALL STADRV(IZ,ZINDCR,IMODE,TAB2DA)
c
      IMODE=2
	CALL STADRV(IZ,ZINDUR,IMODE,TAB2DA)
C
C  INPUT APPROPRIATE PERSON TRIP TABLE
C
      PURP=1
      CALL INTAB(8,VAR,IZ,PURP,DUMMY,IO)
      DO 7837,IT=1,MAX_ZONES
 7837 PERSON(it)=FLOAT(VAR(IT))*PERFACT
      IF(STRHBW) THEN
      PURP=2
      CALL INTAB(8,VAR,IZ,PURP,DUMMY,IO)
      DO 7838,IT=1,MAX_ZONES
      PERIN(1,IT)=PERSON(IT)
      PERIN(2,IT)=FLOAT(VAR(IT))*PERFACT
 7838 CONTINUE
      PURP=3
      CALL INTAB(8,VAR,IZ,PURP,DUMMY,IO)
      DO 7839,IT=1,MAX_ZONES
      PERIN(3,IT)=FLOAT(VAR(IT))*PERFACT
      PERSON(IT)=PERIN(1,IT)+PERIN(2,IT)+PERIN(3,IT)
 7839 CONTINUE
      HFACT=.TRUE.
      END IF
C
C  IF STATION DATA AND SHARES MATRIX OUTPUT REQUESTED
C  THEN SET ALL PERSON TRIP VALUES TO 1.0
C
      IF(MTXOBS.AND.CALIB) THEN
      DO 7840,IT=1,MAX_IZONES
      PERIN(1,IT)=1.0
      PERIN(2,IT)=1.0
      PERIN(3,IT)=1.0
      PERSON(IT)=PERIN(1,IT)+PERIN(2,IT)+PERIN(3,IT)
 7840 CONTINUE
      END IF
C ------------------------------------------------------------
C SUMMARIZE PERSON TRIPS BY INCOME GROUP FOR
C CALIBRATION OF HOUSEHOLD FACTORS
C
      IF(CALIB.AND.(.NOT.HFACT).AND.(.NOT.NHB).AND.(.NOT.STRHBW)) THEN
      DO 23 JZ=1,MAX_ZONES
C
C  PERSON TRIP MARKET SEGMENTATION
C  COMPUTE PERSON TRIPS BY INCOME LEVEL
C
      IF(.NOT.NHB) THEN
      DIZ=DISTEQ(IZ)
      DJZ=DISTEQ(JZ)
      TESUM(6,1)=TESUM(6,1)+PERSON(JZ)*ZHHD(2,IZ)
      TESUM(6,2)=TESUM(6,2)+PERSON(JZ)*ZHHD(3,IZ)
      TESUM(6,3)=TESUM(6,3)+PERSON(JZ)*ZHHD(4,IZ)
C
C ELIMINATE INCOME GROUP MARKET SEGMENTATION FOR NHB TRIPS
C
      ELSE
      TESUM(6,1)=TESUM(6,1)+PERSON(JZ)
      TESUM(6,2)=0.0
      TESUM(6,3)=0.0
      HFACT=.TRUE.
      END IF
   23 CONTINUE
      GO TO 100
      END IF
C
C     WALK TO LOCAL BUS - INDEX 1
C
c  1ST WAIT TIME
      PURP=1
      call intab(13,VAR,iz,PURP,dummy,io)
      do 505,ii=1,MAX_ZONES
  505 wait1(1,ii)=FLOAT(VAR(II))/100.0
c  TRANSFER WAIT TIME
      PURP=2
      call intab(13,VAR,iz,PURP,dummy,io)
      do 506,ii=1,MAX_ZONES
  506 wait2(1,ii)=FLOAT(VAR(II))/100.0
C  BWALK ACCESS WALK
      PURP=3
      call intab(13,VAR,iz,PURP,dummy,io)
      do 501,ii=1,MAX_ZONES
 501  walkacc(1,ii)=FLOAT(VAR(II))/100.0
c  TRANSFER
      PURP=4
      call intab(13,VAR,iz,PURP,dummy,io)
      do 504,ii=1,MAX_ZONES
  504 txfer(1,ii)=FLOAT(VAR(II))
c  BIVTT
      PURP=5
      call intab(13,VAR,iz,PURP,dummy,io)
      do 502,ii=1,MAX_ZONES
 502  Bivt(1,ii)=FLOAT(VAR(II))/100.0
c  FARE
      PURP=6
      call intab(13,VAR,iz,PURP,dummy,io)
      do 503,ii=1,MAX_ZONES
 503  fare(1,ii)=FLOAT(VAR(II))
C  WALK EGRESS TIME
      PURP=7
      CALL INTAB(13,VAR,IZ,PURP,DUMMY,IO)
      DO 507,II=1,MAX_ZONES
  507 WALKEGR(1,II)=FLOAT(VAR(II))/100.0
C  WALK TRANSFER TIME
      PURP=8
      CALL INTAB(13,VAR,IZ,PURP,DUMMY,IO)
      DO 508,II=1,MAX_ZONES
  508 WALKTFR(1,II)=FLOAT(VAR(II))/100.0
c
C     WALK TO EXPRESS BUS - INDEX 2
C
c  1ST WAIT TIME
      PURP=1
      call intab(14,VAR,iz,PURP,dummy,io)
      do 515,ii=1,MAX_ZONES
  515 wait1(2,ii)=FLOAT(VAR(II))/100.0
c  TRANSFER WAIT TIME
      PURP=2
      call intab(14,VAR,iz,PURP,dummy,io)
      do 516,ii=1,MAX_ZONES
  516 wait2(2,ii)=FLOAT(VAR(II))/100.0
C  WALK TIME ACCESS
      PURP=3
      call intab(14,VAR,iz,PURP,dummy,io)
      do 511,ii=1,MAX_ZONES
  511 walkACC(2,ii)=FLOAT(VAR(II))/100.0
c  TRANSFERS
      PURP=4
      call intab(14,VAR,iz,PURP,dummy,io)
      do 514,ii=1,MAX_ZONES
  514 txfer(2,ii)=FLOAT(VAR(II))
c  LOCAL BUS IVT
      PURP=5
      call intab(14,VAR,iz,PURP,dummy,io)
      do 518,ii=1,MAX_ZONES
  518 bivt(2,ii)=FLOAT(VAR(II))/100.0
c  FARE
       PURP=6
       call intab(14,VAR,iz,PURP,dummy,io)
       do 517,ii=1,MAX_ZONES
  517  fare(2,ii)=FLOAT(VAR(II))
c  RAPID BUS IVT
       PURP=7
       call intab(14,VAR,iz,PURP,dummy,io)
       do 597,ii=1,MAX_ZONES
  597 rivt(2,ii)=FLOAT(VAR(II))/100.0
c  EXPRESS BUS IVT
      PURP=8
      call intab(14,VAR,iz,PURP,dummy,io)
      do 513,ii=1,MAX_ZONES
  513 eivt(2,ii)=FLOAT(VAR(II))/100.0
c  TOTAL IN-VEHICLE TIME
      PURP=9
      call intab(14,VAR,iz,PURP,dummy,io)
      do 512,ii=1,MAX_ZONES
  512 tivt(2,ii)=FLOAT(VAR(II))/100.0
C  WALK EGRESS TIME
      PURP=10
      CALL INTAB(14,VAR,IZ,PURP,DUMMY,IO)
      DO 598,II=1,MAX_ZONES
  598 WALKEGR(2,II)=FLOAT(VAR(II))/100.0
C  WALK TRANSFER TIME
      PURP=11
      CALL INTAB(14,VAR,IZ,PURP,DUMMY,IO)
      DO 519,II=1,MAX_ZONES
  519 WALKTFR(2,II)=FLOAT(VAR(II))/100.0
C
C     WALK TO TRANSITWAY BUS - INDEX 3
C
      IF(TWYSK) THEN
c  1ST WAIT TIME
      PURP=1
      call intab(15,VAR,iz,PURP,dummy,io)
      do 535,ii=1,MAX_ZONES
 535  wait1(3,ii)=FLOAT(VAR(II))/100.0
c  TRANSFER WAIT TIME
      PURP=2
      call intab(15,VAR,iz,PURP,dummy,io)
      do 536,ii=1,MAX_ZONES
 536  wait2(3,ii)=FLOAT(VAR(II))/100.0
C  WALK TIME
      PURP=3
      call intab(15,VAR,iz,PURP,dummy,io)
      do 531,ii=1,MAX_ZONES
 531  walkACC(3,ii)=FLOAT(VAR(II))/100.0
c  TRANSFERS
      PURP=4
      call intab(15,VAR,iz,PURP,dummy,io)
      do 534,ii=1,MAX_ZONES
 534  txfer(3,ii)=FLOAT(VAR(II))
C  LOCAL BUS IN-VEHICLE TIME
       PURP=5
       call intab(15,VAR,iz,PURP,dummy,io)
       do 539,ii=1,MAX_ZONES
 539   bivt(3,ii)=FLOAT(VAR(II))/100.0
C  FARE
       PURP=6
       call intab(15,VAR,iz,PURP,dummy,io)
       do 538,ii=1,MAX_ZONES
 538   fare(3,ii)=FLOAT(VAR(II))
C  RAPID BUS IVT
       PURP=7
       call intab(15,VAR,iz,PURP,dummy,io)
       do 542,ii=1,MAX_ZONES
 542   rivt(3,ii)=FLOAT(VAR(II))/100.0
c  EXPRESS BUS IVT
      PURP=8
      call intab(15,VAR,iz,PURP,dummy,io)
      do 533,ii=1,MAX_ZONES
 533  eivt(3,ii)=FLOAT(VAR(II))/100.0
C  TRANSIT WAY BUS IN-VEHICLE TIME
      PURP=9
      call intab(15,VAR,iz,PURP,dummy,io)
      do 537,ii=1,MAX_ZONES
 537  wivt(3,ii)=FLOAT(VAR(II))/100.0
c  TOTAL IN-VEHICLE TIME
      PURP=10
      call intab(15,VAR,iz,PURP,dummy,io)
      do 532,ii=1,MAX_ZONES
 532  tivt(3,ii)=FLOAT(VAR(II))/100.0
C  WALK EGRESS TIME
      PURP=11
      CALL INTAB(15,VAR,IZ,PURP,DUMMY,IO)
      DO 540,II=1,MAX_ZONES
  540 WALKEGR(3,II)=FLOAT(VAR(II))/100.0
C  WALK TRANSFER TIME
      PURP=12
      CALL INTAB(15,VAR,IZ,PURP,DUMMY,IO)
      DO 541,II=1,MAX_ZONES
  541 WALKTFR(3,II)=FLOAT(VAR(II))/100.0
      END IF
C
C     WALK TO RAPID BUS - INDEX 4
C
c  1ST WAIT TIME
      PURP=1
      call intab(16,VAR,iz,PURP,dummy,io)
      do 635,ii=1,MAX_ZONES
 635  wait1(4,ii)=FLOAT(VAR(II))/100.0
c  TRANSFER WAIT TIME
      PURP=2
      call intab(16,VAR,iz,PURP,dummy,io)
      do 636,ii=1,MAX_ZONES
 636  wait2(4,ii)=FLOAT(VAR(II))/100.0
C  WALK TIME
      PURP=3
      call intab(16,VAR,iz,PURP,dummy,io)
      do 631,ii=1,MAX_ZONES
 631  walkACC(4,ii)=FLOAT(VAR(II))/100.0
c  TRANSFERS
      PURP=4
      call intab(16,VAR,iz,PURP,dummy,io)
      do 634,ii=1,MAX_ZONES
 634  txfer(4,ii)=FLOAT(VAR(II))
C  LOCAL BUS IN-VEHICLE TIME
       PURP=5
       call intab(16,VAR,iz,PURP,dummy,io)
       do 639,ii=1,MAX_ZONES
 639   bivt(4,ii)=FLOAT(VAR(II))/100.0
C  FARE
       PURP=6
       call intab(16,VAR,iz,PURP,dummy,io)
       do 638,ii=1,MAX_ZONES
 638   fare(4,ii)=FLOAT(VAR(II))
c  RAPID BUS IVT
       PURP=7
       call intab(16,VAR,iz,PURP,dummy,io)
       do 696,ii=1,MAX_ZONES
  696 rivt(4,ii)=FLOAT(VAR(II))/100.0
c  TOTAL IN-VEHICLE TIME
      PURP=8
      call intab(16,VAR,iz,PURP,dummy,io)
      do 632,ii=1,MAX_ZONES
 632  tivt(4,ii)=FLOAT(VAR(II))/100.0
C  WALK EGRESS TIME
      PURP=9
      CALL INTAB(16,VAR,IZ,PURP,DUMMY,IO)
      DO 640,II=1,MAX_ZONES
  640 WALKEGR(4,II)=FLOAT(VAR(II))/100.0
C  WALK TRANSFER TIME
      PURP=10
      CALL INTAB(16,VAR,IZ,PURP,DUMMY,IO)
      DO 641,II=1,MAX_ZONES
  641 WALKTFR(4,II)=FLOAT(VAR(II))/100.0
C
C     WALK TO BUS RAPID TRANSIT (BRT) - INDEX 5
C
      IF(BRTSK) THEN
c  1ST WAIT TIME
      PURP=1
      call intab(17,VAR,iz,PURP,dummy,io)
      do 735,ii=1,MAX_ZONES
 735  wait1(5,ii)=FLOAT(VAR(II))/100.0
c  TRANSFER WAIT TIME
      PURP=2
      call intab(17,VAR,iz,PURP,dummy,io)
      do 736,ii=1,MAX_ZONES
 736  wait2(5,ii)=FLOAT(VAR(II))/100.0
C  WALK TIME
      PURP=3
      call intab(17,VAR,iz,PURP,dummy,io)
      do 761,ii=1,MAX_ZONES
 761  walkACC(5,ii)=FLOAT(VAR(II))/100.0
c  TRANSFERS
      PURP=4
      call intab(17,VAR,iz,PURP,dummy,io)
      do 734,ii=1,MAX_ZONES
 734  txfer(5,ii)=FLOAT(VAR(II))
C  LOCAL BUS IN-VEHICLE TIME
       PURP=5
       call intab(17,VAR,iz,PURP,dummy,io)
       do 739,ii=1,MAX_ZONES
 739   bivt(5,ii)=FLOAT(VAR(II))/100.0
C  FARE
       PURP=6
       call intab(17,VAR,iz,PURP,dummy,io)
       do 738,ii=1,MAX_ZONES
 738   fare(5,ii)=FLOAT(VAR(II))
C  RAPID BUS IVT
       PURP=7
       call intab(17,VAR,iz,PURP,dummy,io)
       do 742,ii=1,MAX_ZONES
 742   rivt(5,ii)=FLOAT(VAR(II))/100.0
c  EXPRESS BUS IVT
      PURP=8
      call intab(17,VAR,iz,PURP,dummy,io)
      do 733,ii=1,MAX_ZONES
 733  eivt(5,ii)=FLOAT(VAR(II))/100.0
C  BUS RAPID TRANSIT IN-VEHICLE TIME
      PURP=9
      call intab(17,VAR,iz,PURP,dummy,io)
      do 737,ii=1,MAX_ZONES
 737  wivt(5,ii)=FLOAT(VAR(II))/100.0
c  TOTAL IN-VEHICLE TIME
      PURP=10
      call intab(17,VAR,iz,PURP,dummy,io)
      do 732,ii=1,MAX_ZONES
 732  tivt(5,ii)=FLOAT(VAR(II))/100.0
C  WALK EGRESS TIME
      PURP=11
      CALL intab(17,VAR,IZ,PURP,DUMMY,IO)
      DO 740,II=1,MAX_ZONES
  740 WALKEGR(5,II)=FLOAT(VAR(II))/100.0
C  WALK TRANSFER TIME
      PURP=12
      CALL intab(17,VAR,IZ,PURP,DUMMY,IO)
      DO 741,II=1,MAX_ZONES
  741 WALKTFR(5,II)=FLOAT(VAR(II))/100.0
      END IF
C
C   BRT, TRANSITWAY, & EXPRESS BUS STATION ACCESS
C
      IF(STNAC) THEN
      PURP=1
      CALL INTAB(36,VAR,IZ,PURP,DUMMY,IO)
      DO 840,II=1,MAX_ZONES
  840 BUSTAT(1,II)=VAR(II)
      PURP=2
      CALL INTAB(36,VAR,IZ,PURP,DUMMY,IO)
      DO 841,II=1,MAX_ZONES
  841 BUSTAT(2,II)=VAR(II)
      PURP=3
      CALL INTAB(36,VAR,IZ,PURP,DUMMY,IO)
      DO 842,II=1,MAX_ZONES
  842 BUSTAT(3,II)=VAR(II)
      END IF
C
C   DRIVE-ALONE AUTO SKIMS
C   NON-TOLL COST
      PURP=3
      call intab(18,VAR,iz,PURP,dummy,io)
      DO 7003,NI=1,MAX_ZONES
 7003 TAB3DA(NI)=FLOAT(VAR(NI))/100.0
C   TOLL TIME
      PURP=4
      call intab(18,VAR,iz,PURP,dummy,io)
      DO 7004,NI=1,MAX_ZONES
 7004 TAB4DA(NI)=FLOAT(VAR(NI))/100.0
C   TOLL DISTANCE
      PURP=5
      call intab(18,VAR,iz,PURP,dummy,io)
      DO 7005,NI=1,MAX_ZONES
 7005 TAB5DA(NI)=FLOAT(VAR(NI))/100.0
C   TOLL COST
      PURP=6
      call intab(18,VAR,iz,PURP,dummy,io)
      DO 7006,NI=1,MAX_ZONES
 7006 TAB6DA(NI)=FLOAT(VAR(NI))/100.0
C   TOLL TRAVEL DISTANCE (HOT LANES ONLY)
      PURP=7
      call intab(18,VAR,iz,PURP,dummy,io)
      DO 7007,NI=1,MAX_ZONES
 7007 TAB8DA(NI)=FLOAT(VAR(NI))/100.0
C
C  2-PERSON AUTO SKIMS
C      
C   NON-TOLL TIME
      PURP=1
      call intab(19,VAR,iz,PURP,dummy,io)
      DO 7008,NI=1,MAX_ZONES
 7008 TAB12P(NI)=FLOAT(VAR(NI))/100.0
C   NON-TOLL DISTANCE
      PURP=2
      call intab(19,VAR,iz,PURP,dummy,io)
      DO 7009,NI=1,MAX_ZONES
 7009 TAB22P(NI)=FLOAT(VAR(NI))/100.0
C   NON-TOLL COST
      PURP=3
      call intab(19,VAR,iz,PURP,dummy,io)
      DO 7010,NI=1,MAX_ZONES
 7010 TAB32P(NI)=FLOAT(VAR(NI))/100.0
C   NON-TOLL TRAVEL DISTANCE (HOV LANES ONLY)
      PURP=4
      call intab(19,VAR,iz,PURP,dummy,io)
      DO 7014,NI=1,MAX_ZONES
 7014 TAB72P(NI)=FLOAT(VAR(NI))/100.0
C   TOLL TIME
      PURP=5
      call intab(19,VAR,iz,PURP,dummy,io)
      DO 7011,NI=1,MAX_ZONES
 7011 TAB42P(NI)=FLOAT(VAR(NI))/100.0
C   TOLL DISTANCE
      PURP=6
      call intab(19,VAR,iz,PURP,dummy,io)
      DO 7012,NI=1,MAX_ZONES
 7012 TAB52P(NI)=FLOAT(VAR(NI))/100.0
C   TOLL COST
      PURP=7
      call intab(19,VAR,iz,PURP,dummy,io)
      DO 7013,NI=1,MAX_ZONES
 7013 TAB62P(NI)=FLOAT(VAR(NI))/100.0
C   TOLL TRAVEL DISTANCE (HOT LANES ONLY)
      PURP=8
      call intab(19,VAR,iz,PURP,dummy,io)
      DO 7015,NI=1,MAX_ZONES
 7015 TAB82P(NI)=FLOAT(VAR(NI))/100.0
C
C  3-PERSON AUTO SKIMS
C      
C   NON-TOLL TIME
      PURP=1
      call intab(20,VAR,iz,PURP,dummy,io)
      DO 7016,NI=1,MAX_ZONES
 7016 TAB13P(NI)=FLOAT(VAR(NI))/100.0
C   NON-TOLL DISTANCE
      PURP=2
      call intab(20,VAR,iz,PURP,dummy,io)
      DO 7017,NI=1,MAX_ZONES
 7017 TAB23P(NI)=FLOAT(VAR(NI))/100.0
C   NON-TOLL COST
      PURP=3
      call intab(20,VAR,iz,PURP,dummy,io)
      DO 7018,NI=1,MAX_ZONES
 7018 TAB33P(NI)=FLOAT(VAR(NI))/100.0
C   NON-TOLL TRAVEL DISTANCE (HOV LANES ONLY)
      PURP=4
      call intab(20,VAR,iz,PURP,dummy,io)
      DO 7022,NI=1,MAX_ZONES
 7022 TAB73P(NI)=FLOAT(VAR(NI))/100.0
C   TOLL TIME
      PURP=5
      call intab(20,VAR,iz,PURP,dummy,io)
      DO 7019,NI=1,MAX_ZONES
 7019 TAB43P(NI)=FLOAT(VAR(NI))/100.0
C   TOLL DISTANCE
      PURP=6
      call intab(20,VAR,iz,PURP,dummy,io)
      DO 7020,NI=1,MAX_ZONES
 7020 TAB53P(NI)=FLOAT(VAR(NI))/100.0
C   TOLL COST
      PURP=7
      call intab(20,VAR,iz,PURP,dummy,io)
      DO 7021,NI=1,MAX_ZONES
 7021 TAB63P(NI)=FLOAT(VAR(NI))/100.0
C   TOLL TRAVEL DISTANCE (HOT LANES ONLY)
      PURP=8
      call intab(20,VAR,iz,PURP,dummy,io)
      DO 7023,NI=1,MAX_ZONES
 7023 TAB83P(NI)=FLOAT(VAR(NI))/100.0
C
C  4-PERSON AUTO SKIMS
C  
      IF(SKMP4) THEN    
C   NON-TOLL TIME
      PURP=1
      call INTAB(37,VAR,iz,PURP,dummy,io)
      DO 7116,NI=1,MAX_ZONES
 7116 TAB14P(NI)=FLOAT(VAR(NI))/100.0
C   NON-TOLL DISTANCE
      PURP=2
      call INTAB(37,VAR,iz,PURP,dummy,io)
      DO 7117,NI=1,MAX_ZONES
 7117 TAB24P(NI)=FLOAT(VAR(NI))/100.0
C   NON-TOLL COST
      PURP=3
      call INTAB(37,VAR,iz,PURP,dummy,io)
      DO 7118,NI=1,MAX_ZONES
 7118 TAB34P(NI)=FLOAT(VAR(NI))/100.0
C   NON-TOLL TRAVEL DISTANCE (HOV LANES ONLY)
      PURP=4
      call INTAB(37,VAR,iz,PURP,dummy,io)
      DO 7122,NI=1,MAX_ZONES
 7122 TAB74P(NI)=FLOAT(VAR(NI))/100.0
C   TOLL TIME
      PURP=5
      call INTAB(37,VAR,iz,PURP,dummy,io)
      DO 7119,NI=1,MAX_ZONES
 7119 TAB44P(NI)=FLOAT(VAR(NI))/100.0
C   TOLL DISTANCE
      PURP=6
      call INTAB(37,VAR,iz,PURP,dummy,io)
      DO 7120,NI=1,MAX_ZONES
 7120 TAB54P(NI)=FLOAT(VAR(NI))/100.0
C   TOLL COST
      PURP=7
      call INTAB(37,VAR,iz,PURP,dummy,io)
      DO 7121,NI=1,MAX_ZONES
 7121 TAB64P(NI)=FLOAT(VAR(NI))/100.0
C   TOLL TRAVEL DISTANCE (HOT LANES ONLY)
      PURP=8
      call INTAB(37,VAR,iz,PURP,dummy,io)
      DO 7123,NI=1,MAX_ZONES
 7123 TAB84P(NI)=FLOAT(VAR(NI))/100.0
      ELSE
      DO 7125,NI=1,MAX_ZONES
      TAB14P(NI)=TAB13P(NI)
      TAB24P(NI)=TAB23P(NI)
      TAB34P(NI)=TAB33P(NI)
      TAB44P(NI)=TAB43P(NI)
      TAB54P(NI)=TAB53P(NI)
      TAB64P(NI)=TAB63P(NI)
      TAB74P(NI)=TAB73P(NI)
      TAB84P(NI)=TAB83P(NI)
 7125 CONTINUE
      END IF
C
C   INITIALIZE TRIPS MATRIX HERE
C
      DO 7024 JZ=1,MAX_ZONES
      LRTWLK(JZ)=0
      LRTDRV(JZ)=0
      DO 7025 C=1,6
      ALOGSUM(C,JZ)=0.0
      TLOGSUM(C,JZ)=0.0
      ULOGSUM(C,JZ)=0.0
 7025 CONTINUE
      DO 7026 C=1,12
      DO 7027 K=1,3
      USERBENT(C,K,JZ)=0.0
      USERBENC(C,K,JZ)=0
      USERBENC2(C,K,JZ)=0
      USERBENT2(C,K,JZ)=0.0
      USERBENC3(K,JZ)=0
      USERBENT3(K,JZ)=0.0
 7027 CONTINUE
 7026 CONTINUE
 7024 CONTINUE
C
C
C*********************************************************************
C     DESTINATION ZONE LOOP                                          *
C*********************************************************************
C
      DO 200 JZ=1,MAX_IZONES
      IF(.NOT.AIRPASS) THEN
      IF((PERSON(JZ).LE.0.0).AND.(.NOT.DEBUG)) GOTO 200
       IF(DEBUG.AND.PERSON(JZ).LE.0.0) THEN
       PERIN(1,JZ)=100.0
       PERIN(2,JZ)=100.0
       PERIN(3,JZ)=100.0
       PERSON(JZ)=300.0
       END IF
      END IF
	    IF(.NOT.JOI(JZ)) GOTO 200
C...................................................................
	IF(DEBUG) THEN
	WRITE(26,9344) IZ,JZ,PERSON(JZ)
	WRITE(*,9344) IZ,JZ,PERSON(JZ)
 9344 FORMAT(/,' ORIGIN ZONE =',I6,' DESTINATION ZONE=',I6,
     *        '  PERSON TRIPS =',F10.2/,1X,72('-'),/)
        ENDIF
C...................................................................
	IF(IZ.GT.MAX_ZONES.OR.JZ.GT.MAX_ZONES) GO TO 200
	KLAX=0.0
	IF(JZ.EQ.LAXZ) KLAX=KURLAX
        DO 1130 K1=1,6
        DO 1131 K2=1,6
        DO 1132 K3=1,12
        DO 1133 K4=1,2
        MODEINC(K1,K2,K3,K4)=0
 1133   CONTINUE
 1132   CONTINUE
 1131   CONTINUE
 1130   CONTINUE
        DO 1134 K5=1,2
        MODEINC(K5,1,3,1)=BUSMODE(K5,1,1)
        MODEINC(K5,2,3,1)=BUSMODE(K5,2,1)
        MODEINC(K5,3,3,1)=BUSMODE(K5,3,1)
        MODEINC(K5,4,3,1)=BUSMODE(K5,4,1)
        MODEINC(K5,5,3,1)=BUSMODE(K5,5,1)
        MODEINC(K5,1,4,1)=BUSMODE(K5,1,2)
        MODEINC(K5,2,4,1)=BUSMODE(K5,2,2)
        MODEINC(K5,3,4,1)=BUSMODE(K5,3,2)
        MODEINC(K5,4,4,1)=BUSMODE(K5,4,2)
        MODEINC(K5,5,4,1)=BUSMODE(K5,5,2)
 1134   CONTINUE
C	
C....WALK ACCESS STATION UTILITY COMPUTATION -- URBAN RAIL
      do 30,ista=1,5
	  IMODE=2
	CALL EGRSTA(JZ,WSTA(IMODE,ista),STASTA,STAZNE,
     *   WDSTA(imode,ista),IMODE,ZONESTA,STAZONE)
	CALL WUTL(ista,IZ,JZ,WSTA(IMODE,ista),WDSTA(imode,ista),
     *   WUTIL(imode,ista),STASTA,STAZNE,IMODE)
   30 CONTINUE
C...SORT & RETAIN BEST 2 ORIGIN STATIONS
   33 CSORT=.FALSE.
      DO 32 ISTA=2,5
      IF(WUTIL(IMODE,ISTA).EQ.0.0) GO TO 32
      IF(WUTIL(IMODE,ISTA).GT.WUTIL(IMODE,(ISTA-1))) THEN
      TSTA=WSTA(IMODE,ISTA)
      TDSTA=WDSTA(IMODE,ISTA)
      TUTIL=WUTIL(IMODE,ISTA)
      TDIST=WDIST(IMODE,ISTA)
      WSTA(IMODE,ISTA)=WSTA(IMODE,(ISTA-1))
      WDSTA(IMODE,ISTA)=WDSTA(IMODE,(ISTA-1))
      WUTIL(IMODE,ISTA)=WUTIL(IMODE,(ISTA-1))
      WDIST(IMODE,ISTA)=WDIST(IMODE,(ISTA-1))
      WSTA(IMODE,(ISTA-1))=TSTA
      WDSTA(IMODE,(ISTA-1))=TDSTA
      WUTIL(IMODE,(ISTA-1))=TUTIL
      WDIST(IMODE,(ISTA-1))=TDIST
      CSORT=.TRUE.
      END IF
   32 CONTINUE
      IF(CSORT) GO TO 33
      OSTA(IMODE,1)=WSTA(IMODE,1)
      OSTA(IMODE,2)=WSTA(IMODE,2)
      ASTA(IMODE,1)=WDSTA(IMODE,1)
      ASTA(IMODE,2)=WDSTA(IMODE,2)
C....WALK ACCESS STATION UTILITY COMPUTATION -- COMMUTER RAIL
      DO 31 ISTA=1,2   
	 IMODE=1
	CALL EGRSTA(JZ,WSTA(IMODE,ista),STASTA,STAZNE,
     *   WDSTA(imode,ista),IMODE,ZONESTA,STAZONE)
	CALL WUTL(ista,IZ,JZ,WSTA(IMODE,ista),WDSTA(imode,ista),
     *   WUTIL(imode,ista),STASTA,STAZNE,IMODE)
	ASTA(imode,ista)=WDSTA(imode,ista)
   31 continue
C...................................................................
      IF(DEBUG) THEN
      SC1=WSTA(2,1)-MAX_IZONES
      IF(SC1.LT.0) SC1=MAX_STATIONS 
      SC2=WSTA(2,2)-MAX_IZONES
      IF(SC2.LT.0) SC2=MAX_STATIONS 
      SC3=WSTA(2,3)-MAX_IZONES
      IF(SC3.LT.0) SC3=MAX_STATIONS
      SC4=WSTA(2,4)-MAX_IZONES
      IF(SC4.LT.0) SC4=MAX_STATIONS
      SC5=WSTA(2,5)-MAX_IZONES
      IF(SC5.LT.0) SC5=MAX_STATIONS
      WRITE(26,9177) IZ,WSTA(2,1),STANAME(SC1),
     *               WDIST(2,1),WUTIL(2,1),
     *               WSTA(2,2),STANAME(SC2),WDIST(2,2),WUTIL(2,2),
     *               WSTA(2,3),STANAME(SC3),WDIST(2,3),WUTIL(2,3),
     *               WSTA(2,4),STANAME(SC4),WDIST(2,4),WUTIL(2,4),
     *               WSTA(2,5),STANAME(SC5),WDIST(2,5),WUTIL(2,5)
 9177 FORMAT(//1X,'Sorted Station Selection (Walk Access) --'
     *          ' Urban Rail'/
     *       1X,'-------------------------------------------'/
     *       1X,'ORIGIN   ZONE   NUMBER  =',I9/  
     *       1X,'CLOSEST STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'                UTILITY =',F9.4/
     *       1X,'SECOND  STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'                UTILITY =',F9.4/
     *       1X,'THIRD   STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'                UTILITY =',F9.4/
     *       1X,'FOURTH  STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'                UTILITY =',F9.4/
     *       1X,'FIFTH   STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'                UTILITY =',F9.4/)
      ENDIF
C...................................................................   
C   
C
C....BUS ACCESS STATION UTILITY COMPUTATION
       do 40,ista=1,2
       IMODE=1
       IF(DEBUG) WRITE(26,41) ISTA,NAME(IMODE)
   41  FORMAT(/' BUS ACCESS #',I1,' ---> ',a13/
     *         ' =================================')
	CALL EGRSTA(JZ,BSTA(imode,ista),STASTA,STAZNE,
     *   BDSTA(imode,ista),IMODE,ZONESTA,STAZONE)
      CALL BUTL(ista,IZ,JZ,BDIST(imode,ista),BSTA(imode,ista),
     *  BDSTA(imode,ista),BUTIL(imode,ista),STASTA,STAZNE,IMODE)
       ASTA(IMODE,(ISTA+2))=BDSTA(IMODE,ISTA)
	 IMODE=2
	 IF(DEBUG) WRITE(26,41) ISTA,NAME(IMODE)
	CALL EGRSTA(JZ,BSTA(imode,ista),STASTA,STAZNE,
     *   BDSTA(imode,ista),IMODE,ZONESTA,STAZONE)
       CALL BUTL(ista,IZ,JZ,BDIST(imode,ista),BSTA(imode,ista),
     *  BDSTA(imode,ista),BUTIL(imode,ista),STASTA,STAZNE,IMODE)
       ASTA(IMODE,(ISTA+2))=BDSTA(IMODE,ISTA)
  40  continue
C
C....DRIVE ACCESS STATION UTILITY COMPUTATION
      DO 300 IX=1,10
C..COMMUTER RAIL
      IMODE=1
      IF(DEBUG) WRITE(26,341) IX,NAME(IMODE)
  341  FORMAT(/' DRIVE ACCESS #',I2,' ---> ',a13/
     *         ' =================================')
	 ITEMP=ZINDCR(IX)+MAX_IZONES
       CALL EGRSTA(JZ,ITEMP,STASTA,STAZNE,ITEMP2,
     *          IMODE,ZONESTA,STAZONE)
      DSTA(IMODE,IX)=ITEMP2
C      WRITE(26,*) IX,IZ,JZ,PDIST(IMODE,IX),ZINDCR(IX),
C     *             DSTA(IMODE,IX),PUTIL(IMODE,IX)
      CALL PUTL(IX,IZ,JZ,PDIST(imode,IX),ZINDCR(IX),
     *  DSTA(imode,IX),PUTIL(imode,IX),STASTA,STAZNE,TAB1DA,TAB2DA,
     *  IMODE,PNRRAT,PNRRAT2)
      CRPNR(IX)=PNRRAT
      CRPNR2(IX)=PNRRAT2
      CALL KUTL(IX,IZ,JZ,KDIST(imode,IX),ZINDCR(IX),
     *  DSTA(imode,IX),KUTIL(imode,IX),STASTA,STAZNE,TAB1DA,TAB2DA,
     *  IMODE)
C...URBAN RAIL
      IMODE=2
      IF(DEBUG) WRITE(26,341) IX,NAME(IMODE)
	ITEMP=ZINDUR(IX)+MAX_IZONES
      CALL EGRSTA(JZ,ITEMP,STASTA,STAZNE,ITEMP2,
     *          IMODE,ZONESTA,STAZONE)
	 DSTA(IMODE,IX)=ITEMP2
      CALL PUTL(IX,IZ,JZ,PDIST(imode,IX),ZINDUR(IX),
     *  DSTA(imode,IX),PUTIL(imode,IX),STASTA,STAZNE,TAB1DA,TAB2DA,
     *  IMODE,PNRRAT,PNRRAT2)
      CALL KUTL(IX,IZ,JZ,KDIST(imode,IX),ZINDUR(IX),
     *  DSTA(imode,IX),KUTIL(imode,IX),STASTA,STAZNE,TAB1DA,TAB2DA,
     *  IMODE)
  300 CONTINUE
C.....SORT DRIVE ACCESS STATIONS - COMMUTER RAIL
C      DO 24,IND=1,MAX_STATIONS
C       ZINDCR(IND)=NINDEX(1,IND)
C       ZINDUR(IND)=NINDEX(2,IND)
C  24  CONTINUE
      IMODE=1
      CALL USORT(PSTA,PDIST,ZINDCR,DSTA,PDSTA,PUTIL,CRPNR,CRPNR2,IMODE)
      OSTA(imode,5)=PSTA(imode,1)
      OSTA(imode,6)=PSTA(imode,2)
      OSTA(imode,7)=PSTA(imode,3)
      OSTA(imode,8)=PSTA(imode,4)
      ASTA(imode,5)=PDSTA(imode,1)
      ASTA(imode,6)=PDSTA(imode,2)
      ASTA(imode,7)=PDSTA(imode,3)
      ASTA(imode,8)=PDSTA(imode,4)
      CALL USORT(KSTA,KDIST,ZINDCR,DSTA,KDSTA,KUTIL,CRDUM,CRDUM,IMODE)
      OSTA(imode,9)=KSTA(imode,1)
      OSTA(imode,10)=KSTA(imode,2)
      OSTA(imode,11)=KSTA(imode,3)
      OSTA(imode,12)=KSTA(imode,4)
      ASTA(imode,9)=KDSTA(imode,1)
      ASTA(imode,10)=KDSTA(imode,2)
      ASTA(imode,11)=KDSTA(imode,3)
      ASTA(imode,12)=KDSTA(imode,4)
C.....SORT DRIVE ACCESS STATIONS - URBAN RAIL
      IMODE=2
      CALL USORT(PSTA,PDIST,ZINDUR,DSTA,PDSTA,PUTIL,CRDUM,CRDUM,IMODE)
      OSTA(imode,5)=PSTA(imode,1)
      OSTA(imode,6)=PSTA(imode,2)
      OSTA(imode,7)=PSTA(imode,3)
      OSTA(imode,8)=PSTA(imode,4)
      ASTA(imode,5)=PDSTA(imode,1)
      ASTA(imode,6)=PDSTA(imode,2)
      ASTA(imode,7)=PDSTA(imode,3)
      ASTA(imode,8)=PDSTA(imode,4)
      AIRPAS(1)=OSTA(IMODE,9)
      AIRPAS(2)=ASTA(IMODE,9)
      CALL USORT(KSTA,KDIST,ZINDUR,DSTA,KDSTA,KUTIL,CRDUM,CRDUM,IMODE)
      OSTA(imode,9)=KSTA(imode,1)
      OSTA(imode,10)=KSTA(imode,2)
      OSTA(imode,11)=KSTA(imode,3)
      OSTA(imode,12)=KSTA(imode,4)
      ASTA(imode,9)=KDSTA(imode,1)
      ASTA(imode,10)=KDSTA(imode,2)
      ASTA(imode,11)=KDSTA(imode,3)
      ASTA(imode,12)=KDSTA(imode,4)
  50  continue
C...STORE INDICATOR VALUES - URBAN RAIL
      DO 51 K1=1,12
      SC1=ASTA(2,K1)-MAX_IZONES
      IF(SC1.LT.0) SC1=MAX_STATIONS
      DO 52 K2=1,5 
      IF(STAZNEI(SC1,JZ,2,K2).GT.0) MODEINC(2,K2,K1,2)=1
   52 CONTINUE
   51 CONTINUE
C
C
C---------------------------------------------------------------------
C            WALK --> LOCAL BUS UTILITY COMPUTATION                   |
C---------------------------------------------------------------------
C   COMPUTE MODE CHOICE MODEL UTILITY VALUE FOR WALK TO LOCAL BUS
C
      IF(BIVT(1,JZ).GT.0.0) THEN
      WAIT1A=AMIN1(WAIT1(1,JZ),WAITLT)
      WAIT1B=DIM(WAIT1(1,JZ),WAITLT)
      WBUTL=COEFF(11)*BIVT(1,JZ) + COEFF(12)*WAIT1A +
     *        COEFF(13)*WAIT1B + COEFF(15)*WAIT2(1,JZ) +
     *        COEFF(17)*WALKTFR(1,JZ)
      WLKBACC=WALKACC(1,JZ)
      WLKBEGR=WALKEGR(1,JZ)
      ELSE
      WBUTL=0.0
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9043) BIVT(1,JZ),FARE(1,JZ),TXFER(1,JZ),WAIT1(1,JZ),
     *               WAIT1A,WAIT1B,WAIT2(1,JZ),WALKTFR(1,JZ),
     *               WALKACC(1,JZ),WALKEGR(1,JZ),WBUTL
 9043 FORMAT(//1X,'Walk --> Bus Level-Of-Service Data'/
     *       1X,  '----------------------------------'//
     *       1X,'TOTAL IVTT           =',F8.2/
     *       1X,'BUS   FARE           =',F8.2/
     *       1X,'BUS   TRANSFERS      =',F8.2/
     *       1X,'BUS   1ST WAIT       =',F8.2/
     *       1X,'BUS   1ST WAIT (<5)  =',F8.2/
     *       1X,'BUS   1ST WAIT (>5)  =',F8.2/
     *       1X,'BUS   2ND WAIT       =',F8.2/
     *       1X,'BUS   TRANSFER WALK  =',F8.2/
     *       1X,'BUS   ACCESS   WALK  =',F8.2/
     *       1X,'BUS   EGRESS   WALK  =',F8.2/
     *       1X,'MODE CHOICE MODEL UTILITY=',F10.5/)
      END IF
C
C---------------------------------------------------------------------
C            WALK --> RAPID BUS UTILITY COMPUTATION                   |
C---------------------------------------------------------------------
C   COMPUTE MODE CHOICE MODEL UTILITY VALUE FOR WALK TO RAPID BUS
C
      IF(RIVT(4,JZ).GT.0.0) THEN
      WAIT1A=AMIN1(WAIT1(4,JZ),WAITLT)
      WAIT1B=DIM(WAIT1(4,JZ),WAITLT)
      WRUTL=COEFF(11)*TIVT(4,JZ) + COEFF(12)*WAIT1A +
     *        COEFF(13)*WAIT1B + COEFF(15)*WAIT2(4,JZ) +
     *        COEFF(17)*WALKTFR(4,JZ)
      WLKRACC=WALKACC(4,JZ)
      WLKREGR=WALKEGR(4,JZ)
        IF(BIVT(4,JZ).GT.0.0) THEN
        MODEINC(6,1,1,1)=1
        MODEINC(6,1,1,2)=1 
        END IF
      ELSE
      WRUTL=0.0
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9093) TIVT(4,JZ),BIVT(4,JZ),RIVT(4,JZ),
     *               FARE(4,JZ),TXFER(4,JZ),WAIT1(4,JZ),
     *               WAIT1A,WAIT1B,WAIT2(4,JZ),WALKTFR(4,JZ),
     *               WALKACC(4,JZ),WALKEGR(4,JZ),WRUTL
 9093 FORMAT(//1X,'Walk --> Rapid Bus Level-Of-Service Data'/
     *       1X,  '----------------------------------------'//
     *       1X,'TOTAL IVTT           =',F8.2/
     *       1X,'LOCAL IVTT           =',F8.2/
     *       1X,'RAPID IVTT           =',F8.2/
     *       1X,'RAPID FARE           =',F8.2/
     *       1X,'RAPID TRANSFERS      =',F8.2/
     *       1X,'RAPID 1ST WAIT       =',F8.2/
     *       1X,'RAPID 1ST WAIT (<5)  =',F8.2/
     *       1X,'RAPID 1ST WAIT (>5)  =',F8.2/
     *       1X,'RAPID 2ND WAIT       =',F8.2/
     *       1X,'RAPID TRANSFER WALK  =',F8.2/
     *       1X,'RAPID ACCESS   WALK  =',F8.2/
     *       1X,'RAPID EGRESS   WALK  =',F8.2/
     *       1X,'MODE CHOICE MODEL UTILITY=',F10.5/)
      END IF
C---------------------------------------------------------------------
C            WALK --> TRANSITWAY BUS UTILITY COMPUTATION              
C---------------------------------------------------------------------
C   COMPUTE MODE CHOICE MODEL UTILITY VALUE FOR WALK TO TRANSITWAY BUS
C
      if(wivt(3,jz).gt.0.0) then
	WAIT1A=AMIN1(WAIT1(3,JZ),WAITLT)
      WAIT1B=DIM(WAIT1(3,JZ),WAITLT)
      WTUTL=COEFF(11)*TIVT(3,JZ) + COEFF(12)*WAIT1A +
     *        COEFF(13)*WAIT1B + COEFF(15)*WAIT2(3,JZ) +
     *        COEFF(17)*WALKTFR(3,JZ)
      WLKWTACC=WALKACC(3,JZ)
      WLKWTEGR=WALKEGR(3,JZ)
C...OBTAIN STATION NUMBER
      CWTWY=BUSTAT(2,JZ)
      IF(CWTWY.GT.0) THEN
      DO 5740 K=1,MAX_STATIONS
        IF(STANODE1(K).EQ.CWTWY) THEN
        L=K+MAX_IZONES
        IF(STANUM(K).EQ.4) THEN
        CWTWY=L
        GO TO 5741
        END IF
        END IF
        IF(STANODE2(K).EQ.CWTWY) THEN
        L=K+MAX_IZONES
        IF(STANUM(K).EQ.4) THEN
        CWTWY=L
        GO TO 5741
        END IF
        END IF 
 5740 CONTINUE
      WRITE(26,5742) CWTWY
 5742 FORMAT(' WARNING - TRANSITWAY STATION NOT FOUND',
     *      ' FOR NODE ',I6)
      END IF
 5741 CONTINUE
        IF(CWTWY.GT.0) THEN
        WTUTL=WTUTL + COEFF(17)*STADATA((CWTWY-MAX_IZONES),9)
        END IF
        IF(BIVT(3,JZ).GT.0.0) THEN
        WTUTL=WTUTL+KBTWYW
        END IF
	elseif(wivt(3,jz).le.0.0) then
	CWTWY=0
	wtutl=0.0
	endif
C
C....................................................................
      IF(DEBUG) THEN
      X=0.0
      IF(CWTWY.GT.0) X=STADATA((CWTWY-MAX_IZONES),9)
      WRITE(26,9045) TIVT(3,JZ),BIVT(3,JZ),EIVT(3,JZ),WIVT(3,JZ),
     *               RIVT(3,JZ),
     *               FARE(3,JZ),TXFER(3,JZ),WAIT1(3,JZ),WAIT1A,WAIT1B,
     *               WAIT2(3,JZ),WALKTFR(3,JZ),WALKACC(3,JZ),
     *               WALKEGR(3,JZ),KBTWYW,CWTWY,
     *               X,WTUTL
 9045 FORMAT(//1X,'Walk --> Transitway Bus Level-Of-Service Data'/
     *       1X,  '----------------------------------'//
     *       1X,'TOTAL IVTT                =',F8.2/
     *       1X,'LOCAL IVTT                =',F8.2/
     *       1X,'EXPRESS IVTT              =',F8.2/
     *       1X,'TRANSITWAY IVTT           =',F8.2/
     *       1X,'RAPID BUS IVTT            =',F8.2/
     *       1X,'TRANSITWAY FARE           =',F8.2/
     *       1X,'TRANSITWAY TRANSFERS      =',F8.2/
     *       1X,'TRANSITWAY 1ST WAIT       =',F8.2/
     *       1X,'TRANSITWAY 1ST WAIT (<5)  =',F8.2/
     *       1X,'TRANSITWAY 1ST WAIT (>5)  =',F8.2/
     *       1X,'TRANSITWAY 2ND WAIT       =',F8.2/
     *       1X,'TRANSITWAY WALK TRANSFER  =',F8.2/
     *       1X,'TRANSITWAY WALK ACCESS    =',F8.2/
     *       1X,'TRANSITWAY WALK EGRESS    =',F8.2/
     *       1X,'TRANSITWAY BUS TXFER CNST =',F8.2/
     *       1X,'TRANSITWAY ACCESS STATION =',I8/
     *       1X,'TRANSITWAY ACCESS STA WALK=',F8.2/
     *       1X,'MODE CHOICE MODEL UTILITY=',F10.5/)
      END IF
C -----------------------------------------------------------------
C---------------------------------------------------------------------
C            DRIVE --> TRANSITWAY BUS UTILITY COMPUTATION            |
C---------------------------------------------------------------------
      CSTAT=MAX_ZONES
      IF(TWYSK) THEN
      IMODE=4
	    CALL DRVNEW(JZ,DTUTL,TAB2DA,TAB1DA,CSTAT,CDSTAT,IMODE,
     *            STAZNE,EXPZNE,EXPCR,TEMP,ITEMP1,ITEMP2)
	    IT=CDSTAT-MAX_IZONES
	    WALKDT=STAZNE(3,IT,JZ)
	    IS=CSTAT-MAX_IZONES
	    IF((STAZNEI(IS,JZ,4,1).GT.0).OR.(STAZNEI(IS,JZ,4,2).GT.0).OR.
     *   (STAZNEI(IS,JZ,4,3).GT.0)) THEN
      DTUTL=DTUTL+KBTWYD
      END IF
      END IF
C---------------------------------------------------------------------
C            WALK --> EXPRESS BUS UTILITY COMPUTATION                |
C---------------------------------------------------------------------
C   COMPUTE MODE CHOICE MODEL UTILITY VALUE FOR WALK TO EXPRESS BUS
C
      if(eivt(2,jz).gt.0.0) then
	WAIT1A=AMIN1(WAIT1(2,JZ),WAITLT)
      WAIT1B=DIM(WAIT1(2,JZ),WAITLT)
      WCUTL=COEFF(11)*TIVT(2,JZ) + COEFF(12)*WAIT1A +
     *        COEFF(13)*WAIT1B + COEFF(15)*WAIT2(2,JZ) +
     *        COEFF(17)*WALKTFR(2,JZ)
      WLKCACC=WALKACC(2,JZ)
      WLKCEGR=WALKEGR(2,JZ)
C...OBTAIN STATION NUMBER
      CWEXP=BUSTAT(3,JZ)
            IF(CWEXP.GT.0) THEN
      DO 5720 K=1,MAX_STATIONS
        IF(STANODE1(K).EQ.CWEXP) THEN
        L=K+MAX_IZONES
        IF(STANUM(K).EQ.3) THEN
        CWEXP=L
        GO TO 5721
        END IF
        END IF
        IF(STANODE2(K).EQ.CWEXP) THEN
        L=K+MAX_IZONES
        IF(STANUM(K).EQ.3) THEN
        CWEXP=L
        GO TO 5721
        END IF
        END IF 
 5720 CONTINUE
      WRITE(26,5722) CWEXP
 5722 FORMAT(' WARNING - EXPRESS BUS STATION NOT FOUND FOR NODE ',I6)
      END IF
 5721 CONTINUE
        IF(CWEXP.GT.0) THEN
        WCUTL=WCUTL + COEFF(17)*STADATA((CWEXP-MAX_IZONES),9)
        END IF
        IF(BIVT(2,JZ).GT.0.0) THEN
        WCUTL=WCUTL+KBEXPW
        END IF
	elseif(eivt(2,jz).le.0.0) then
	CWEXP=0
	wcutl=0.0
	endif
	if(eivt(2,jz).gt.0) denom=bivt(2,jz)/eivt(2,jz)
	if(bustexp.and.(denom.gt.0.5)) wcutl=0.0
	if(bustexp.and.(txfer(2,jz).gt.1)) wcutl=0.0
C
C....................................................................
      IF(DEBUG) THEN
      X=0.0
      IF(CWEXP.GT.0) X=STADATA((CWEXP-MAX_IZONES),9)
      WRITE(26,9044) TIVT(2,JZ),BIVT(2,JZ),EIVT(2,JZ),RIVT(2,JZ),
     *               FARE(2,JZ),TXFER(2,JZ),WAIT1(2,JZ),WAIT1A,WAIT1B,
     *               WAIT2(2,JZ),WALKTFR(2,JZ),WALKACC(2,JZ),
     *               WALKEGR(2,JZ),KBEXPW,CWEXP,X,WCUTL
 9044 FORMAT(//1X,'Walk --> Express Bus Level-Of-Service Data'/
     *       1X,  '----------------------------------'//
     *       1X,'TOTAL IVTT           =',F8.2/
     *       1X,'LOCAL IVTT           =',F8.2/
     *       1X,'EXPRESS IVTT         =',F8.2/
     *       1X,'RAPID BUS IVTT       =',F8.2/
     *       1X,'EXP   FARE           =',F8.2/
     *       1X,'EXP   TRANSFERS      =',F8.2/
     *       1X,'EXP   1ST WAIT       =',F8.2/
     *       1X,'EXP   1ST WAIT (<5)  =',F8.2/
     *       1X,'EXP   1ST WAIT (>5)  =',F8.2/
     *       1X,'EXP   2ND WAIT       =',F8.2/
     *       1X,'EXP   WALK TRANSFER  =',F8.2/
     *       1X,'EXP   WALK ACCESS    =',F8.2/
     *       1X,'EXP   WALK EGRESS    =',F8.2/
     *       1X,'EXP   BUS TXFER CNST =',F8.2/
     *       1X,'EXP   ACCESS STATION =',I8/
     *       1X,'EXP   ACCESS STA WALK=',F8.2/
     *       1X,'MODE CHOICE MODEL UTILITY=',F10.5/)
      END IF
C---------------------------------------------------------------------
C            DRIVE --> EXPRESS BUS UTILITY COMPUTATION                
C---------------------------------------------------------------------
      IMODE=3
	CALL DRVNEW(JZ,DCUTL,TAB2DA,TAB1DA,CSTAE,CDSTAE,IMODE,
     *            STAZNE,EXPZNE,EXPCR,DCUTLCR,CCSTA,CCDSTA)
	IT=CDSTAE-MAX_IZONES
	WALKD=STAZNE(3,IT,JZ)
	IS=CSTAE-MAX_IZONES
        IF((STAZNEI(IS,JZ,3,1).GT.0).OR.(STAZNEI(IS,JZ,3,2).GT.0)) THEN
        DCUTL=DCUTL + KBEXPD
        END IF
	IF(EXPCR) THEN
	IF(DEBUG) WRITE(26,9049) CCSTA,CCDSTA
 9049   FORMAT(' DRIVE ACCESS TO EXPRESS BUS TO COMMUTER RAIL'/
     *         ' --------------------------------------------'/
     *         ' EXPRESS BUS STATION =',I8/
     *         ' CR ACCESS   STATION =',I8/)
	CALL EGRSTA(JZ,CCDSTA,STASTA,STAZNE,
     *   CCSTAE,1,ZONESTA,STAZONE)
        K1=CCDSTA-MAX_IZONES
        K2=CCSTAE-MAX_IZONES
        IF(CCDSTA.LE.0.OR.CCSTAE.LE.0.0) THEN
        DCUTLCR=0.0
        ELSE
        DCUTLCR=DCUTLCR+STASTA(2,K1,K2) + STAZNE(2,K2,JZ)
        END IF
C....................................................................
        IF(DEBUG) THEN
        WRITE(26,9046) CCSTA,CCDSTA,CCSTAE,DCUTLCR
 9046   FORMAT(/
     *       1X,'EXPRESS BUS STATION  =',I8/
     *       1X,'CR ORIGIN STATION    =',I8/
     *       1X,'CR DEST STATION      =',I8/
     *       1X,'MODE CHOICE MODEL UTILITY=',F10.5/)
        END IF  
      IF(DCUTLCR.NE.0.0.AND.(DCUTLCR.GT.DCUTL)) THEN
      CSTAE=CCSTA
      DCUTL=DCUTLCR
      CDSTAE=CCDSTA
      EXPCR=.TRUE.
      IF(DEBUG) THEN
      WRITE(26,9048)
      WRITE(26,9046) CSTAE,CDSTAE,CCSTAE,DCUTL
 9048 FORMAT(' EXPRESS BUS TO COMMUTER RAIL SELECTED'/
     *       ' -------------------------------------'/)
      END IF
      ELSE
      EXPCR=.FALSE.
      IF(DEBUG) WRITE(26,9047)
 9047 FORMAT(' EXPRESS BUS TO COMMUTER RAIL NOT SELECTED'/
     *       ' -----------------------------------------'/)
      END IF
        END IF
C---------------------------------------------------------------------
C            WALK --> BUS RAPID TRANSIT (BRT) UTILITY COMPUTATION              
C---------------------------------------------------------------------
C   COMPUTE MODE CHOICE MODEL UTILITY VALUE FOR WALK TO BRT
C
      WAIT1A=0.0
      WAIT1B=0.0
      if(wivt(5,jz).gt.0.0) then
      WAIT1A=AMIN1(WAIT1(5,JZ),WAITLT)
      WAIT1B=DIM(WAIT1(5,JZ),WAITLT)
      WBRTUTL=COEFF(11)*TIVT(5,JZ) + COEFF(12)*WAIT1A +
     *        COEFF(13)*WAIT1B + COEFF(15)*WAIT2(5,JZ) +
     *        COEFF(17)*WALKTFR(5,JZ)
      WLKWBRTACC=WALKACC(5,JZ)
      WLKWBRTEGR=WALKEGR(5,JZ)
C..OBTAIN STATION NUMBER
      CWBRT=BUSTAT(1,JZ)
      IF(CWBRT.GT.0) THEN
      DO 5730 K=1,MAX_STATIONS
        IF(STANODE1(K).EQ.CWBRT) THEN
        L=K+MAX_IZONES
        IF(STANUM(K).EQ.5) THEN
        CWBRT=L
        GO TO 5731
        END IF
        END IF
        IF(STANODE2(K).EQ.CWBRT) THEN
        L=K+MAX_IZONES
        IF(STANUM(K).EQ.5) THEN
        CWBRT=L
        GO TO 5731
        END IF
        END IF 
 5730 CONTINUE
      WRITE(26,5732) CWBRT
 5732 FORMAT(' WARNING - BUS RAPID TRANSIT STATION NOT FOUND',
     *      ' FOR NODE ',I6)
      END IF
 5731 CONTINUE
        IF(CWBRT.GT.0) THEN
        WBRTUTL=WBRTUTL + COEFF(17)*STADATA((CWBRT-MAX_IZONES),9)
        END IF
        IF(RIVT(5,JZ).GT.0.0) MODEINC(5,2,1,1)=1
        IF(EIVT(5,JZ).GT.0.0) MODEINC(5,3,1,1)=1
        IF(BIVT(5,JZ).GT.0.0.OR.RIVT(5,JZ).GT.0.0) THEN
        WBRTUTL=WBRTUTL+KBBRTW
        MODEINC(5,1,1,1)=1
        END IF
	elseif(wivt(5,jz).le.0.0) then
	CWBRT=0
	WBRTUTL=0.0
	endif
C
C....................................................................
      IF(DEBUG) THEN
      X=0.0
      IF(CWBRT.GT.0) X=STADATA((CWBRT-MAX_IZONES),9)
      WRITE(26,9145) TIVT(5,JZ),BIVT(5,JZ),EIVT(5,JZ),WIVT(5,JZ),
     *               RIVT(5,JZ),
     *               FARE(5,JZ),TXFER(5,JZ),WAIT1(5,JZ),WAIT1A,WAIT1B,
     *               WAIT2(5,JZ),WALKTFR(5,JZ),WALKACC(5,JZ),
     *               WALKEGR(5,JZ),KBBRTW,CWBRT,X,WBRTUTL
 9145 FORMAT(//1X,'Walk --> Bus Rapid Transit (BRT',
     *            ' Level-Of-Service Data)'/
     *       1X,  '----------------------------------'//
     *       1X,'TOTAL IVTT                       =',F8.2/
     *       1X,'LOCAL IVTT                       =',F8.2/
     *       1X,'EXPRESS IVTT                     =',F8.2/
     *       1X,'BUS RAPID TRANIT IVTT            =',F8.2/
     *       1X,'RAPID BUS IVTT                   =',F8.2/
     *       1X,'BUS RAPID TRANSIT FARE           =',F8.2/
     *       1X,'BUS RAPID TRANSIT TRANSFERS      =',F8.2/
     *       1X,'BUS RAPID TRANSIT 1ST WAIT       =',F8.2/
     *       1X,'BUS RAPID TRANSIT 1ST WAIT (<5)  =',F8.2/
     *       1X,'BUS RAPID TRANSIT 1ST WAIT (>5)  =',F8.2/
     *       1X,'BUS RAPID TRANSIT 2ND WAIT       =',F8.2/
     *       1X,'BUS RAPID TRANSIT WALK TRANSFER  =',F8.2/
     *       1X,'BUS RAPID TRANSIT WALK ACCESS    =',F8.2/
     *       1X,'BUS RAPID TRANSIT WALK EGRESS    =',F8.2/
     *       1X,'BUS RAPID TRANSIT BUS TXFER CNST =',F8.2/
     *       1X,'BUS RAPID ACCESS STATION         =',I8/
     *       1X,'BUS RAPID ACCESS STATION WALK    =',F8.2/
     *       1X,'MODE CHOICE MODEL UTILITY        =',F10.5/)
      END IF
C -----------------------------------------------------------------
C---------------------------------------------------------------------
C            DRIVE --> BUS RAPID TRANSIT UTILITY COMPUTATION         |
C---------------------------------------------------------------------
      IF(BRTSK) THEN
      IMODE=5
	    CALL DRVNEW(JZ,DBRTUTL,TAB2DA,TAB1DA,CSTABRT,CDSTABRT,IMODE,
     *            STAZNE,BRTZNE,BRTUR,DCUTLBR,CBSTA,CBDSTA)
	    IT=CDSTABRT-MAX_IZONES
	    WALKDBRT=STAZNE(3,IT,JZ)
	    IF(BRTUR) THEN
	    IF(DEBUG) WRITE(26,9149) CBSTA,CBDSTA
 9149   FORMAT(' DRIVE ACCESS TO BRT TO URBAN RAIL'/
     *         ' --------------------------------------------'/
     *         ' BRT ACCESS  STATION =',I8/
     *         ' UR ACCESS   STATION =',I8/)
	    CALL EGRSTA(JZ,CBDSTA,STASTA,STAZNE,
     *   CBSTAE,2,ZONESTA,STAZONE)
        K1=CBDSTA-MAX_IZONES
        K2=CBSTAE-MAX_IZONES
        IF(CBDSTA.LE.0.OR.CBSTAE.LE.0.0) THEN
        DCUTLBR=0.0
        ELSE
        DCUTLBR=DCUTLBR+LSUMSUP*(STASTA(2,K1,K2) + STAZNE(2,K2,JZ))
        END IF
C....................................................................
        IF(DEBUG) THEN
        WRITE(26,9146) CBSTA,CBDSTA,CBSTAE,DCUTLBR
 9146   FORMAT(/
     *       1X,'BRT ACCESS  STATION  =',I8/
     *       1X,'UR ORIGIN STATION    =',I8/
     *       1X,'UR DEST STATION      =',I8/
     *       1X,'MODE CHOICE MODEL UTILITY=',F10.5/)
        END IF  
      IF(DCUTLBR.NE.0.0.AND.(DCUTLBR.GT.DCUTL)) THEN
      CSTABRT=CBSTA
      DBRTUTL=DCUTLBR
      CDSTABRT=CBDSTA
      IT=CDSTABRT-MAX_IZONES
      WALKDBRT=STAZNE(3,IT,JZ)
      BRTUR=.TRUE.
      IF(DEBUG) THEN
      WRITE(26,9148)
 9148 FORMAT(' BRT TO URBAN RAIL SELECTED'/
     *       ' --------------------------'/)
      END IF
      ELSE
      BRTUR=.FALSE.
      IF(STAZNEI((CSTABRT-MAX_IZONES),JZ,5,1).GT.0) THEN
      MODEINC(5,1,3,2)=1
      DBRTUTL=DBRTUTL+KBBRTD
      END IF
      IF(STAZNEI((CSTABRT-MAX_IZONES),JZ,5,2).GT.0) MODEINC(5,2,3,2)=1
      IF(STAZNEI((CSTABRT-MAX_IZONES),JZ,5,3).GT.0) MODEINC(5,3,3,2)=1
      IF(DEBUG) WRITE(26,9147)
 9147 FORMAT(' BRT TO URBAN RAIL NOT SELECTED'/
     *       ' ------------------------------'/)
      END IF
        END IF
      END IF
C ----------------------------------------------------------------------------
C
C       COMPUTE THE COMMON EXPONENTIATED UTILITY FOR WALK
C
      IF(NMOT) THEN
          WALKT=TAB2DA(JZ)
          WALK1=DMIN1(WALKT,MWALKT)
          WALK2=DDIM(WALKT,MWALKT)
          UTILWK= MWALK1*(60.0/3.0)*WALK1
     *            + MWALK2*(60.0/3.0)*WALK2
C
C       COMPUTE THE COMMON EXPONENTIATED UTILITY FOR BICYCLE
C
          BIKET=TAB2DA(JZ)
          BIKE1=DMIN1(BIKET,MWALKT)
          BIKE2=DDIM(BIKET,MWALKT)
          UTILBK=MBIKE1*(60.0/10.0)*BIKE1
     *            +MBIKE2*(60.0/10.0)*BIKE2
      END IF
C -------------------------------------------------------------------------------
C
C
C.................................................................
      IF(DEBUG) THEN
      WRITE(26,9026) IZ,JZ,
     *               WALKT,WALK1,WALK2,BIKET,BIKE1,BIKE2,
     *               UTILWK,UTILBK
 9026 FORMAT(/1X,'NON-MOTORIZED MODE UTILITY COMPUTATIONS'/
     *       1X,'----------------------------------------'/
     *       1X,'ORIGIN ZONE                =',I10/
     *       1X,'DESTINATION ZONE           =',I10/
     *       1X,'TOTAL  WALK MODE DIST      =',F10.2/
     *       1X,'FIRST  WALK MODE DIST      =',F10.2/
     *       1X,'SECOND WALK MODE DIST      =',F10.2/
     *       1X,'TOTAL  BIKE MODE DIST      =',F10.2/
     *       1X,'FIRST  BIKE MODE DIST      =',F10.2/
     *       1X,'SECOND BIKE MODE DIST      =',F10.2/
     *       1X,'WALK   MODE UTILITY        =',F10.5/
     *       1X,'BIKE   MODE UTILITY        =',F10.5//)
      END IF
C.....................................................................
C----------------------------------------------------------------------
C ADJUST PARK&RIDE UTILITY VALUES
C TO INCLUDE SHADOW PRICE
C------------------------------------------------------------
C
      IF(CAPRES) THEN
C..................................................................
      IF(DEBUG) THEN
      WRITE(26,9002)
 9002 FORMAT(/1X,'REVISED PARK&RIDE UTILITY VALUES'/
     *        1X,'--------------------------------------'//)
      END IF
C..................................................................
      DO 15,IMODE=1,2
      DO 15,T=1,4
      KT=T+4
      ORISTA=OSTA(IMODE,KT)-MAX_IZONES
      IF(ASTA(IMODE,KT).GT.0.AND.PUTIL(IMODE,T).NE.0.0) THEN
      PUTIL(IMODE,T)=PUTIL(IMODE,T)+COEFF(6)*
     *               STADATA((OSTA(IMODE,KT)-MAX_IZONES),5)
C....................................................................
      IF(DEBUG) THEN
      IC=OSTA(IMODE,KT)-MAX_IZONES
      WRITE(26,9001) OSTA(IMODE,KT),STANAME(IC),
     *               STADATA((OSTA(IMODE,KT)-MAX_IZONES),5),
     *               PUTIL(IMODE,T)
 9001 FORMAT(1X,'P&R STATION=',I4,5X,A37,' SHADOW PRICE=',F5.0,
     *          ' REVISED UTILITY=',F10.3)
      END IF
C.....................................................................
      END IF
   15 CONTINUE
C
C  ADJUST UTILITIES FOR DRIVE TO EXPRESS BUS AND
C  DRIVE TO TRANSITWAY BUS AND DRIVE TO BRT
C
      IF(CSTAT.GT.0.AND.DTUTL.NE.0.0) THEN
      DTUTL=DTUTL + COEFF(6)*STADATA((CSTAT)-MAX_IZONES,5)
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9001) CSTAT,STANAME(CSTAT-MAX_IZONES),
     *               STADATA((CSTAT-MAX_IZONES),5),DTUTL
      END IF
C.....................................................................  
      END IF
      IF(CSTAE.GT.0.AND.DCUTL.NE.0.0) THEN
      DCUTL=DCUTL + COEFF(6)*STADATA((CSTAE)-MAX_IZONES,5)
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9001) CSTAE,STANAME(CSTAE-MAX_IZONES),
     *               STADATA((CSTAE-MAX_IZONES),5),DCUTL
      END IF
C.....................................................................  
      END IF
      IF(CSTABRT.GT.MAX_IZONES.AND.DBRTUTL.NE.0.0) THEN
      DBRTUTL=DBRTUTL + COEFF(6)*STADATA((CSTABRT-MAX_IZONES),5)
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9001) CSTABRT,STANAME(CSTABRT-MAX_IZONES),
     *               STADATA((CSTABRT-MAX_IZONES),5),DBRTUTL
      END IF
C.....................................................................  
      END IF
	END IF
C
C
C---------------------------------------------------------------------
C        UPPER LEVEL UTILITIES & PROBABLILITY COMPUTATIONS           |
C---------------------------------------------------------------------
C
C
C
C  CONVERT WALK DISTANCE --> TIME
C
C WALK DISTANCE FOR WALK ACCESS TO COMMUTER/URBAN RAIL
C
      MWALKW(1,1)=WDIST(1,1)*20.0
      MWALKW(1,2)=WDIST(1,2)*20.0
      MWALKW(2,1)=WDIST(2,1)*20.0
      MWALKW(2,2)=WDIST(2,2)*20.0
      
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9000) IZ,JZ,WUTIL(1,1),WUTIL(1,2),
     *               BUTIL(1,1),BUTIL(1,2),(PUTIL(1,K1),K1=1,4),
     *               (KUTIL(1,K2),K2=1,4),WUTIL(2,1),WUTIL(2,2),
     *               BUTIL(2,1),BUTIL(2,2),(PUTIL(2,K3),K3=1,4),
     *               (KUTIL(2,K4),K4=1,4),
     *               WBUTL,WRUTL,WCUTL,DCUTL,WTUTL,DTUTL,
     *               WBRTUTL,DBRTUTL,UTILWK,UTILBK
 9000 FORMAT(/1X,'SUMMARY OF LOWER LEVEL UTILITY VALUES'/
     *       1X,'---------------------------------------'/
     *       1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10//
     *       1X,'COMMUTER RAIL  WALK-->STA #1 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  WALK-->STA #2 UTILITY=',F10.5//
     *       1X,'COMMUTER RAIL  BUS -->STA #1 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  BUS -->STA #2 UTILITY=',F10.5//
     *       1X,'COMMUTER RAIL  P&R -->STA #1 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  P&R -->STA #2 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  P&R -->STA #3 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  P&R -->STA #4 UTILITY=',F10.5//
     *       1X,'COMMUTER RAIL  K&R -->STA #1 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  K&R -->STA #2 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  K&R -->STA #3 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  K&R -->STA #4 UTILITY=',F10.5///
     *       1X,'URBAN    RAIL  WALK-->STA #1 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  WALK-->STA #2 UTILITY=',F10.5//
     *       1X,'URBAN    RAIL  BUS -->STA #1 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  BUS -->STA #2 UTILITY=',F10.5//
     *       1X,'URBAN    RAIL  P&R -->STA #1 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  P&R -->STA #2 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  P&R -->STA #3 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  P&R -->STA #4 UTILITY=',F10.5//
     *       1X,'URBAN    RAIL  K&R -->STA #1 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  K&R -->STA #2 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  K&R -->STA #3 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  K&R -->STA #4 UTILITY=',F10.5///
     *       1X,'WALK     -> LOCAL   BUS      UTILITY=',F10.5/
     *       1X,'WALK     -> RAPID   BUS      UTILITY=',F10.5/
     *       1X,'WALK     -> EXPRESS BUS      UTILITY=',F10.5/
     *       1X,'DRIVE    -> EXPRESS BUS      UTILITY=',F10.5//
     *       1X,'WALK     -> TRANSITWAY       UTILITY=',F10.5/
     *       1X,'DRIVE    -> TRANSITWAY       UTILITY=',F10.5//
     *       1X,'WALK     -> BUS RAPID        UTILITY=',F10.5/
     *       1X,'DRIVE    -> BUS RAPID        UTILITY=',F10.5//
     *       1X,'WALK MODE                    UTILITY=',F10.5/
     *       1X,'BIKE MODE                    UTILITY=',F10.5/)
C
      WRITE(26,9112) MWALKW(1,1),MWALKW(1,2),MWALKB(1,1),
     *               MWALKB(1,2),MWALKW(2,1),MWALKW(2,2),
     *               MWALKB(2,1),MWALKB(2,2),WLKBACC,WLKRACC,
     *               WLKCACC,WALKD,
     *               WLKWTACC,WALKDT,WLKWBRTACC,WALKDBRT
 9112 FORMAT(//1X,'ORIGIN WALK TIMES'/
     *       1X,'-----------------'/
     *       1X,'COMMUTER RAIL WALK-->STA #1 WALK TIME=',F10.2/
     *       1X,'COMMUTER RAIL WALK-->STA #2 WALK TIME=',F10.2//
     *       1X,'COMMUTER RAIL BUS -->STA #1 WALK TIME=',F10.2/
     *       1X,'COMMUTER RAIL BUS -->STA #2 WALK TIME=',F10.2//
     *       1X,'URBAN    RAIL WALK-->STA #1 WALK TIME=',F10.2/
     *       1X,'URBAN    RAIL WALK-->STA #2 WALK TIME=',F10.2//
     *       1X,'URBAN    RAIL BUS -->STA #1 WALK TIME=',F10.2/
     *       1X,'RAIL     RAIL BUS -->STA #2 WALK TIME=',F10.2//
     *       1X,'WALK     -> LOCAL BUS       WALK TIME=',F10.2/
     *       1X,'WALK     -> RAPID BUS       WALK TIME=',F10.2//
     *       1X,'WALK     -> EXPRESS BUS     WALK TIME=',F10.2/
     *       1X,'DRIVE    -> EXPRESS BUS     WALK TIME=',F10.2//
     *       1X,'WALK     -> TRANSITWAY BUS  WALK TIME=',F10.2/
     *       1X,'DRIVE    -> TRANSITWAY BUS  WALK TIME=',F10.2/
     *       1X,'WALK     -> BUS RAPID TRN   WALK TIME=',F10.2/
     *       1X,'DRIVE    -> BUS RAPID TRN   WALK TIME=',F10.2)
C
      WRITE(26,9003) (OSTA(1,K),ASTA(1,K),K=1,12)
 9003 FORMAT(//1X,'ORIGIN --> DESTINATION STATION SUMMARY'/
     *       1X,'--------------------------------------'/
     *       1X,'COMMUTER RAIL  WALK-->STA #1 ',I4,'-->',I4/
     *       1X,'COMMUTER RAIL  WALK-->STA #2 ',I4,'-->',I4//
     *       1X,'COMMUTER RAIL  BUS -->STA #1 ',I4,'-->',I4/
     *       1X,'COMMUTER RAIL  BUS -->STA #2 ',I4,'-->',I4//
     *       1X,'COMMUTER RAIL  P&R -->STA #1 ',I4,'-->',I4/
     *       1X,'COMMUTER RAIL  P&R -->STA #2 ',I4,'-->',I4/
     *       1X,'COMMUTER RAIL  P&R -->STA #3 ',I4,'-->',I4/
     *       1X,'COMMUTER RAIL  P&R -->STA #4 ',I4,'-->',I4//
     *       1X,'COMMUTER RAIL  K&R -->STA #1 ',I4,'-->',I4/
     *       1X,'COMMUTER RAIL  K&R -->STA #2 ',I4,'-->',I4/
     *       1X,'COMMUTER RAIL  K&R -->STA #3 ',I4,'-->',I4/
     *       1X,'COMMUTER RAIL  K&R -->STA #4 ',I4,'-->',I4)
C
      WRITE(26,9004) (OSTA(2,K),ASTA(2,K),K=1,12)
 9004 FORMAT(//1X,'ORIGIN --> DESTINATION STATION SUMMARY'/
     *       1X,'--------------------------------------'/
     *       1X,'URBAN RAIL     WALK-->STA #1 ',I4,'-->',I4/
     *       1X,'URBAN RAIL     WALK-->STA #2 ',I4,'-->',I4//
     *       1X,'URBAN RAIL     BUS -->STA #1 ',I4,'-->',I4/
     *       1X,'URBAN RAIL     BUS -->STA #2 ',I4,'-->',I4//
     *       1X,'URBAN RAIL     P&R -->STA #1 ',I4,'-->',I4/
     *       1X,'URBAN RAIL     P&R -->STA #2 ',I4,'-->',I4/
     *       1X,'URBAN RAIL     P&R -->STA #3 ',I4,'-->',I4/
     *       1X,'URBAN RAIL     P&R -->STA #4 ',I4,'-->',I4//
     *       1X,'URBAN RAIL     K&R -->STA #1 ',I4,'-->',I4/
     *       1X,'URBAN RAIL     K&R -->STA #2 ',I4,'-->',I4/
     *       1X,'URBAN RAIL     K&R -->STA #3 ',I4,'-->',I4/
     *       1X,'URBAN RAIL     K&R -->STA #4 ',I4,'-->',I4)
C
      WRITE(26,9115) (MODEINC(2,K1,3,1),K1=1,6),
     *               (MODEINC(2,K4,4,1),K4=1,6),
     *               ((MODEINC(2,K2,K3,2),K2=1,6),K3=1,12)
 9115 FORMAT(//,1X,' URBAN RAIL SUPPORT MODE INDICATORS'/
     *          1X,' ACCES LOCAL  RAPID  EXPRESS  TWY  BRT  UR'/
     *          1X,' ----- -----  -----  -------  ---  ---  --'/
     *          1X,' BUS 1',3X,I1,6X,I1,7X,I1,6X,I1,4X,I1,4X,I1/
     *          1X,' BUS 1',3X,I1,6X,I1,7X,I1,6X,I1,4X,I1,4X,I1//
     *          1X,' EGRES LOCAL  RAPID  EXPRESS  TWY  BRT  UR'/
     *          1X,' ----- -----  -----  -------  ---  ---  --'/
     *          1X,' WALK1',3X,I1,6X,I1,7X,I1,6X,I1,4X,I1,4X,I1/
     *          1X,' WALK2',3X,I1,6X,I1,7X,I1,6X,I1,4X,I1,4X,I1/
     *          1X,' BUS 1',3X,I1,6X,I1,7X,I1,6X,I1,4X,I1,4X,I1/
     *          1X,' BUS 1',3X,I1,6X,I1,7X,I1,6X,I1,4X,I1,4X,I1/
     *          1X,' PNR 1',3X,I1,6X,I1,7X,I1,6X,I1,4X,I1,4X,I1/
     *          1X,' PNR 2',3X,I1,6X,I1,7X,I1,6X,I1,4X,I1,4X,I1/
     *          1X,' PNR 3',3X,I1,6X,I1,7X,I1,6X,I1,4X,I1,4X,I1/
     *          1X,' PNR 4',3X,I1,6X,I1,7X,I1,6X,I1,4X,I1,4X,I1/
     *          1X,' KNR 1',3X,I1,6X,I1,7X,I1,6X,I1,4X,I1,4X,I1/
     *          1X,' KNR 2',3X,I1,6X,I1,7X,I1,6X,I1,4X,I1,4X,I1/
     *          1X,' KNR 3',3X,I1,6X,I1,7X,I1,6X,I1,4X,I1,4X,I1/
     *          1X,' KNR 4',3X,I1,6X,I1,7X,I1,6X,I1,4X,I1,4X,I1)
      WRITE(26,9114) (MODEINC(5,K1,1,1),K1=1,6),
     *               (MODEINC(5,K2,3,2),K2=1,6)
 9114 FORMAT(//,1X,' BRT SUPPORT MODE INDICATORS'/
     *          1X,' ACCES LOCAL  RAPID  EXPRESS  TWY  BRT  UR'/
     *          1X,' ----- -----  -----  -------  ---  ---  --'/
     *          1X,' WALK',4X,I1,6X,I1,7X,I1,6X,I1,4X,I1,4X,I1//
     *          1X,' EGRES LOCAL  RAPID  EXPRESS  TWY  BRT  UR'/
     *          1X,' ----- -----  -----  -------  ---  ---  --'/
     *          1X,'  DRV',4X,I1,6X,I1,7X,I1,6X,I1,4X,I1,4X,I1/)
      WRITE(26,9116) (MODEINC(6,K1,1,1),K1=1,6),
     *               (MODEINC(6,K2,1,2),K2=1,6)
 9116 FORMAT(//,1X,' BUS METRO RAPID SUPPORT MODE INDICATORS'/
     *          1X,' ACCES LOCAL  RAPID  EXPRESS  TWY  BRT  UR'/
     *          1X,' ----- -----  -----  -------  ---  ---  --'/
     *          1X,' WALK',4X,I1,6X,I1,7X,I1,6X,I1,4X,I1,4X,I1//
     *          1X,' EGRES LOCAL  RAPID  EXPRESS  TWY  BRT  UR'/
     *          1X,' ----- -----  -----  -------  ---  ---  --'/
     *          1X,'  DRV',4X,I1,6X,I1,7X,I1,6X,I1,4X,I1,4X,I1/)
C
       END IF
C.....................................................................
C
C  CHECK STATION NUMBER VALUES
C
      DO 18 K=1,12
      DO 18 L=1,2
      IF(OSTA(L,K).LE.0) OSTA(L,K)=MAX_ZONES
      IF(ASTA(L,K).LE.0) ASTA(L,K)=MAX_ZONES
      DC=ASTA(L,K)-MAX_IZONES
      IC=STAIND(DC,JZ)
      IF(IC.LT.1) IC=1
      ASTA2(L,K)=IC+(L-1)*4
C
C  DETERMINE NUMBER OF TRANSFERS
C  FOR EACH STATION PAIR - URBAN RAIL ONLY
C
      IF(L.EQ.2) THEN
      RALTXF(K)=-1
      DO 19 M=1,20
      IF(OSTA(L,K).EQ.SELSTA(M)) THEN
      IC=OSTA(L,K)-MAX_IZONES
      DC=ASTA(L,K)-MAX_IZONES
      RALTXF(K)=TXFERS1(IC,DC)+TXFERS2(DC,JZ)
      IF(K.EQ.3) RALTXF(K)=RALTXF(K)+BTXFER(L,1)
      IF(K.EQ.4) RALTXF(K)=RALTXF(K)+BTXFER(L,2)
      END IF
   19 CONTINUE
      END IF
   18 CONTINUE
C.............................................................................
      IF(DEBUG) THEN
      WRITE(26,9034) (OSTA(2,K),ASTA(2,K),RALTXF(K),K=1,12)
 9034 FORMAT(//1X,'ORIGIN --> DESTINATION STATION SUMMARY'/
     *    1X,'--------------------------------------'/
     *    1X,'URBAN RAIL     WALK-->STA #1 ',I4,'-->',I4,' XFERS=',I2/
     *    1X,'URBAN RAIL     WALK-->STA #2 ',I4,'-->',I4,' XFERS=',I2//
     *    1X,'URBAN RAIL     BUS -->STA #1 ',I4,'-->',I4,' XFERS=',I2/
     *    1X,'URBAN RAIL     BUS -->STA #2 ',I4,'-->',I4,' XFERS=',I2//
     *    1X,'URBAN RAIL     P&R -->STA #1 ',I4,'-->',I4,' XFERS=',I2/
     *    1X,'URBAN RAIL     P&R -->STA #2 ',I4,'-->',I4,' XFERS=',I2/
     *    1X,'URBAN RAIL     P&R -->STA #3 ',I4,'-->',I4,' XFERS=',I2/
     *    1X,'URBAN RAIL     P&R -->STA #4 ',I4,'-->',I4,' XFERS=',I2//
     *    1X,'URBAN RAIL     K&R -->STA #1 ',I4,'-->',I4,' XFERS=',I2/
     *    1X,'URBAN RAIL     K&R -->STA #2 ',I4,'-->',I4,' XFERS=',I2/
     *    1X,'URBAN RAIL     K&R -->STA #3 ',I4,'-->',I4,' XFERS=',I2/
     *    1X,'URBAN RAIL     K&R -->STA #4 ',I4,'-->',I4,' XFERS=',I2)
      WRITE(26,9035) CWEXP,CWTWY,CWBRT
 9035 FORMAT(//1X,'WALK ACCESS STATION NUMBERS'/
     *         1X,'---------------------------'/
     *         1X,'EXPRESS BUS=',I5/
     *         1X,'TRANSITWAY =',I5/
     *         1X,'BRT        =',I5/)
      END IF
C.................................................................................
C
C
C  PERSON TRIP MARKET SEGMENTATION
C  COMPUTE PERSON TRIPS BY INCOME LEVEL
C
      IF(.NOT.STRHBW) THEN
      PERIN(1,JZ)=PERSON(JZ)*ZHHD(2,IZ)
      PERIN(2,JZ)=PERSON(JZ)*ZHHD(3,IZ)
      PERIN(3,JZ)=PERSON(JZ)*ZHHD(4,IZ)
      END IF
C
C ELIMINATE INCOME GROUP MARKET SEGMENTATION FOR NHB TRIPS
C
      IF(NHB) THEN
      PERIN(1,JZ)=PERSON(JZ)
      PERIN(2,JZ)=0.0
      PERIN(3,JZ)=0.0
      END IF
C
C FACTOR PERSON TRIP INPUTS (IF DESIRED)
C
      DIZ=DISTEQ(IZ)
      DJZ=DISTEQ(JZ)
      PERIN(1,JZ)=PERIN(1,JZ)*FACTPER(DIZ,DJZ,1)  
      PERIN(2,JZ)=PERIN(2,JZ)*FACTPER(DIZ,DJZ,2) 
      PERIN(3,JZ)=PERIN(3,JZ)*FACTPER(DIZ,DJZ,3) 
C
C SUMMARIZE PERSON TRIPS BY INCOME AND TRIP LENGTH
C
      PINDEX=IFIX(TAB2DA(JZ)/5.0)+1
      IF(PINDEX.GT.21) PINDEX=21
      PTRIP(PINDEX,1)=PTRIP(PINDEX,1)+PERIN(1,JZ)
      PTRIP(PINDEX,2)=PTRIP(PINDEX,2)+PERIN(2,JZ)
      PTRIP(PINDEX,3)=PTRIP(PINDEX,3)+PERIN(3,JZ)   
C
C  SET PERSON TRIPS FOR DEBUG FUNCTION
C
      IF(DEBUG.AND.FIXIJTRIPS.GT.0) THEN
      PERIN(1,JZ)=FIXIJTRIPS
      PERIN(2,JZ)=FIXIJTRIPS
      PERIN(3,JZ)=FIXIJTRIPS
      PERSON(JZ)=PERIN(1,JZ)+PERIN(2,JZ)+PERIN(3,JZ)
      WRITE(26,9345) IZ,JZ,PERSON(JZ)
	    WRITE(*,9345) IZ,JZ,PERSON(JZ)
 9345 FORMAT(/,' DEBUG PERSON TRIPS OVERRIDDEN WITH FIXIJTRIPS'/
     *         ' ---------------------------------------------'/
     *        ' ORIGIN ZONE =',I6,' DESTINATION ZONE=',I6,
     *        '  PERSON TRIPS =',F10.2/,1X,72('-'),/)
      END IF
C
C
C  WALK ACCESS MARKET SEGMENTATION COMPUTATIONS
C
      MWALK(1)=PWALK(IZ,1)*PWALK(JZ,1)
      MWALK(2)=PWALK(IZ,1)*PWALK(JZ,2)
      MWALK(3)=PWALK(IZ,2)*PWALK(JZ,1)
      MWALK(4)=PWALK(IZ,2)*PWALK(JZ,2)
      NOWALK=1.0-PWALK(IZ,2)-PWALK(IZ,1)
      IF(NOWALK.LT.0.0) NOWALK=0.0
      MWALK(5)=NOWALK*PWALK(JZ,1)
      MWALK(6)=NOWALK*PWALK(JZ,2)
      MWALK(7)=1.0-MWALK(1)-MWALK(2)-MWALK(3)-MWALK(4)-MWALK(5)
     *          -MWALK(6)
C
C  COMPUTE FTA-RELATED ACCESS PROPORTIONS
C
      TWALK(1)=MWALK(1)+MWALK(2)+MWALK(3)+MWALK(4)
      TWALK(2)=MWALK(5)+MWALK(6)
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9021) PERSON(jz),PERIN(1,JZ),PERIN(2,JZ),PERIN(3,JZ),
     *               PWALK(IZ,1),PWALK(IZ,2),
     *               PWALK(JZ,1),PWALK(JZ,2),MWALK(1),
     *               MWALK(2),MWALK(3),MWALK(4),
     *               MWALK(5),MWALK(6),MWALK(7),
     *               TWALK(1),TWALK(2)
 9021 FORMAT(/1X,'MARKET SEGMENTATION COMPUTATIONS'/
     *       1X,'----------------------------------'/
     *       1X,'PERSON TRIPS      TOTAL=',F10.4/
     *       1X,'PERSON TRIPS INC. GRP 1=',F10.4/
     *       1X,'PERSON TRIPS INC. GRP 2=',F10.4/
     *       1X,'PERSON TRIPS INC. GRP 3=',F10.4//
     *       1X,'SHORT WALK      ORIGIN =',F10.4/
     *       1X,'LONG  WALK      ORIGIN =',F10.4/
     *       1X,'SHORT WALK      DESTIN =',F10.4/
     *       1X,'LONG  WALK      DESTIN =',F10.4//
     *       1X,'WALK  SEGMENT    1     =',F10.4/
     *       1X,'WALK  SEGMENT    2     =',F10.4/
     *       1X,'WALK  SEGMENT    3     =',F10.4/
     *       1X,'WALK  SEGMENT    4     =',F10.4/
     *       1X,'DRIVE SEGMENT    5     =',F10.4/
     *       1X,'DRIVE SEGMENT    6     =',F10.4/
     *       1X,'NO    TRANSIT    7     =',F10.4//
     *       1X,'WALK  SEGMENT   FTA    =',F10.4/
     *       1X,'DRIVE SEGMENT   FTA    =',F10.4/)
      END IF
C....................................................................
C
C  CALCULATE CONDITIONAL MODE PROBABILITIES
C  ....... 3 INCOME GROUPS  INDEX C=INCOME GROUP
C
      NCATS=3
      IF(NHB) NCATS=1
      DO 1000 C=1,NCATS
      TSHAR(1)=0.0
      TSHAR(2)=0.0
C---------------------------------------------------------------------
C       HIGHWAY MODE UTILITY VALUE COMPUTATION                  |
C---------------------------------------------------------------------
C
C  INITIALIZE UTILITIES
C
 150  UTIL0NT=0.0
      UTIL0T=0.0
      UTIL2NT=0.0
      UTIL2NTH=0.0
      UTIL2T=0.0
      UTIL2TH=0.0
      UTIL3NT=0.0
      UTIL3NTH=0.0
      UTIL3T=0.0
      UTIL3TH=0.0
      UTIL4NT=0.0
      UTIL4NTH=0.0
      UTIL4T=0.0
      UTIL4TH=0.0
      HOV3TOL=0
      HOV3NTL=0
      HOV2TOL=0
      HOV2NTL=0
      HOV4TOL=0
      HOV4NTL=0
C
C       COMPUTE THE COMMON EXPONENTIATED UTILITY FOR DRIVE-ALONE AUTO
C
C....NON-TOLL 
      UTIL0NT=COEFF(21)* TAB1DA(JZ)
     *        +COEFF(27)*(ZHHD(7,IZ)+ZHHD(7,JZ) )
     *        +COEFF(70+C)* ZHHD(5,JZ)/2.0
     *        +COEFF(70+C)* opcost*(TAB2DA(JZ))
     *        +COEFF(70+C)* TAB3DA(JZ)
C....TOLL 
      IF((TAB4DA(JZ).GT.0).AND.(TAB8DA(JZ).GT.TOLDIST)) THEN
      YTOLDS0=YINTER+YSLOPE*TAB5DA(JZ)
	    YTOLDS0=MIN(YTOLDS0,0.0)
      UTIL0T = COEFF(21)* TAB4DA(JZ)
     *        +COEFF(27)*( ZHHD(7,IZ)+ZHHD(7,JZ) )
     *        +COEFF(70+C)* ZHHD(5,JZ)/2.0
     *        +COEFF(70+C)* opcost* TAB5DA(JZ)
     *        +COEFF(70+C)* TAB6DA(JZ)
     *        +COEFF(70+C)* TOLCST(1)*TAB8DA(JZ) + YTOLDS0
     *        +KTOLL/(LSUMA*LSUMS*LSUMT)
      END IF
C
C       COMPUTE THE COMMON EXPONENTIATED UTILITY FOR 2-PERSON AUTO
C
	    YTOLDS2=YINTER+YSLOPE*TAB5DA(JZ)
	    YTOLDS2=MIN(YTOLDS2,0.0)
C....NON-TOLL/NON-HOV
      UTIL2NT =COEFF(21)* TAB1DA(JZ)
     *        +COEFF(27)*(ZHHD(7,IZ)+ZHHD(7,JZ))
     *        +COEFF(70+C)* ZHHD(5,JZ)/(HOV2P*2.0)
     *        +COEFF(70+C)* opcost* TAB2DA(JZ)/HOV2P
     *        +COEFF(70+C)* TAB3DA(JZ)/HOV2P
C....HOV UTILITY FOR NON-TOLL CHOICE
	    IF(TAB72P(JZ).GT.HOVDIST) THEN
      UTIL2NTH =COEFF(21)* TAB12P(JZ)
     *        +COEFF(27)*(ZHHD(7,IZ)+ZHHD(7,JZ))
     *        +COEFF(70+C)* ZHHD(5,JZ)/(HOV2P*2.0)
     *        +COEFF(70+C)* opcost* TAB22P(JZ)/HOV2P
     *        +COEFF(70+C)* TAB32P(JZ)/HOV2P + YTOLDS2
     *        +KHOV2/(LSUMA*LSUMS*LSUMT)
      HOV2NTL=1
      END IF
C....TOLL/NON-HOV
      IF((TAB4DA(JZ).GT.0).AND.(TAB8DA(JZ).GT.TOLDIST)) THEN
	    UTIL2T = COEFF(21)* TAB4DA(JZ)
     *        +COEFF(27)*( ZHHD(7,IZ)+ZHHD(7,JZ) )
     *        +COEFF(70+C)* ZHHD(5,JZ)/(HOV2P*2.0)
     *        +COEFF(70+C)* OPCOST* TAB5DA(JZ)/HOV2P
     *        +COEFF(70+C)* (DSCT2P*TAB6DA(JZ))/HOV2P
     *        +COEFF(70+C)* TOLCST(2)*TAB8DA(JZ) + YTOLDS2
     *        +KTOLL2/(LSUMA*LSUMS*LSUMT)
      END IF
C....HOV UTILITY FOR TOLL CHOICE
	    IF((TAB72P(JZ).GT.HOVDIST).AND.(TAB82P(JZ).GT.TOLDIST)) THEN
      UTIL2TH= COEFF(21)* TAB42P(JZ)
     *        +COEFF(27)*( ZHHD(7,IZ)+ZHHD(7,JZ) )
     *        +COEFF(70+C)* ZHHD(5,JZ)/(HOV2P*2.0)
     *        +COEFF(70+C)* OPCOST* TAB52P(JZ)/HOV2P
     *        +COEFF(70+C)* (DSCT2P*TAB62P(JZ))/HOV2P
     *        +COEFF(70+C)* TOLCST(2)*TAB82P(JZ) + YTOLDS2
     *        +KTOLL2/(LSUMA*LSUMS*LSUMT)
     *        +KHOV2/(LSUMA*LSUMS*LSUMT)
      HOV2TOL=1
      END IF
C
C       COMPUTE THE COMMON EXPONENTIATED UTILITY FOR 3 PERSON AUTO
C
	    YTOLDS3=YINTER+YSLOPE*TAB5DA(JZ)
	    YTOLDS3=MIN(YTOLDS3,0.0)
C....NON-TOLL/NON-HOV
      UTIL3NT =COEFF(21)* TAB1DA(JZ)
     *        +COEFF(27)*( ZHHD(7,IZ)+ZHHD(7,JZ) )
     *        +COEFF(70+C)* (ZHHD(5,JZ)/(HOV3P*2.0))
     *        +COEFF(70+C)* opcost* (TAB2DA(JZ)/hov3p)
     *        +COEFF(70+C)* TAB3DA(JZ)/hov3p + YTOLDS3
C....HOV UTILITY FOR NON-TOLL CHOICE
	    IF(TAB73P(JZ).GT.HOVDIST) THEN
      UTIL3NTH=COEFF(21)* TAB13P(JZ)
     *        +COEFF(27)*( ZHHD(7,IZ)+ZHHD(7,JZ) )
     *        +COEFF(70+C)* (ZHHD(5,JZ)/(HOV3P*2.0))
     *        +COEFF(70+C)* opcost* (TAB23P(JZ)/hov3p)
     *        +COEFF(70+C)* TAB33P(JZ)/hov3p + YTOLDS3
     *        +KHOV3/(LSUMA*LSUMS*LSUMT)  
      HOV3NTL=1
      END IF
C....TOLL/NON-HOV
      IF((TAB4DA(JZ).GT.0).AND.(TAB8DA(JZ).GT.TOLDIST)) THEN
	    UTIL3T = COEFF(21)* TAB4DA(JZ)
     *        +COEFF(27)*(ZHHD(7,IZ)+ZHHD(7,JZ))
     *        +COEFF(70+C)* (ZHHD(5,JZ)/(HOV3P*2.0))
     *        +COEFF(70+C)* opcost* (TAB5DA(JZ)/hov3p)
     *        +COEFF(70+C)* (DSCT3P*TAB6DA(JZ))/hov3p
     *        +COEFF(70+C)* TOLCST(3)*TAB8DA(JZ) + YTOLDS3
     *        +KTOLL3/(LSUMA*LSUMS*LSUMT)
      END IF
C....HOV UTILITY FOR TOLL CHOICE
	    IF((TAB73P(JZ).GT.HOVDIST).AND.(TAB83P(JZ).GT.TOLDIST)) THEN
	    UTIL3TH= COEFF(21)* TAB43P(JZ)
     *        +COEFF(27)*(ZHHD(7,IZ)+ZHHD(7,JZ))
     *        +COEFF(70+C)* (ZHHD(5,JZ)/(HOV3P*2.0))
     *        +COEFF(70+C)* opcost* (TAB53P(JZ)/hov3p)
     *        +COEFF(70+C)* (DSCT3P*TAB63P(JZ))/hov3p
     *        +COEFF(70+C)* TOLCST(3)*TAB83P(JZ) + YTOLDS3
     *        +KTOLL3/(LSUMA*LSUMS*LSUMT)   
     *        +KHOV3/(LSUMA*LSUMS*LSUMT)  
       HOV3TOL=1
       END IF
C
C       COMPUTE THE COMMON EXPONENTIATED UTILITY FOR 4+PERSON AUTO
C
	    YTOLDS4=YINTER+YSLOPE*TAB5DA(JZ)
	    YTOLDS4=MIN(YTOLDS4,0.0)
C....NON-TOLL/NON-HOV
      UTIL4NT =COEFF(21)* TAB1DA(JZ)
     *        +COEFF(27)*( ZHHD(7,IZ)+ZHHD(7,JZ) )
     *        +COEFF(70+C)* (ZHHD(5,JZ)/(HOV4P*2.0))
     *        +COEFF(70+C)* opcost* (TAB2DA(JZ)/HOV4P)
     *        +COEFF(70+C)* TAB3DA(JZ)/hov4p
C...HOV UTILITY FOR NON-TOLL CHOICE
	    IF(TAB74P(JZ).GT.HOVDIST) THEN
      UTIL4NTH=COEFF(21)* TAB14P(JZ)
     *        +COEFF(27)*( ZHHD(7,IZ)+ZHHD(7,JZ) )
     *        +COEFF(70+C)* (ZHHD(5,JZ)/(HOV4P*2.0))
     *        +COEFF(70+C)* opcost* (TAB24P(JZ)/HOV4P)
     *        +COEFF(70+C)* TAB34P(JZ)/hov4p + YTOLDS4
     *        +KHOV4/(LSUMA*LSUMS*LSUMT)
      HOV4NTL=1
      END IF
C....TOLL/NON-HOV
      IF((TAB4DA(JZ).GT.0).AND.(TAB8DA(JZ).GT.TOLDIST)) THEN
	    UTIL4T = COEFF(21)* TAB4DA(JZ)
     *        +COEFF(27)*(ZHHD(7,IZ)+ZHHD(7,JZ))
     *        +COEFF(70+C)* (ZHHD(5,JZ)/(HOV4P*2.0))
     *        +COEFF(70+C)* opcost* (TAB5DA(JZ)/HOV4P)
     *        +COEFF(70+C)* (DSCT4P*TAB64P(JZ))/hov4p
     *        +COEFF(70+C)* TOLCST(4)*TAB8DA(JZ)+ YTOLDS4
     *        +KTOLL4/(LSUMA*LSUMS*LSUMT)
      END IF
C....HOV UTILITY FOR TOLL CHOICE
	    IF((TAB74P(JZ).GT.HOVDIST).AND.(TAB84P(JZ).GT.TOLDIST)) THEN
	    UTIL4TH= COEFF(21)* TAB44P(JZ)
     *        +COEFF(27)*(ZHHD(7,IZ)+ZHHD(7,JZ))
     *        +COEFF(70+C)* (ZHHD(5,JZ)/(HOV4P*2.0))
     *        +COEFF(70+C)* opcost* (TAB54P(JZ)/HOV4P)
     *        +COEFF(70+C)* (DSCT4P*TAB64P(JZ))/hov4p
     *        +COEFF(70+C)* TOLCST(4)*TAB84P(JZ)+ YTOLDS4
     *        +KTOLL4/(LSUMA*LSUMS*LSUMT)
     *        +KHOV4/(LSUMA*LSUMS*LSUMT)
       HOV4TOL=1
       END IF
C.................................................................
      IF(DEBUG) THEN
      WRITE(26,9025) IZ,JZ,TAB1DA(JZ),TAB2DA(JZ),TAB3DA(JZ),
     *               TAB4DA(JZ),TAB5DA(JZ),TAB6DA(JZ),TAB8DA(JZ),
     *               YTOLDS0,
     *               TAB12P(JZ),TAB22P(JZ),TAB32P(JZ),
     *               TAB42P(JZ),TAB52P(JZ),TAB62P(JZ),DSCT2P,
     *               TAB72P(JZ),TAB82P(JZ),YTOLDS2,
     *               TAB13P(JZ),TAB23P(JZ),TAB33P(JZ),
     *               TAB43P(JZ),TAB53P(JZ),TAB63P(JZ),DSCT3P,
     *               TAB73P(JZ),TAB83P(JZ),YTOLDS3,
     *               TAB14P(JZ),TAB24P(JZ),TAB34P(JZ),
     *               TAB44P(JZ),TAB54P(JZ),TAB64P(JZ),DSCT4P,
     *               TAB74P(JZ),TAB84P(JZ),YTOLDS4,
     *               ZHHD(5,JZ),ZHHD(7,IZ),ZHHD(7,JZ),
     *               UTIL0NT,UTIL0T,UTIL2NT,UTIL2NTH,UTIL2T,UTIL2TH,
     *               UTIL3NT,UTIL3NTH,UTIL3T,UTIL3TH,
     *               UTIL4NT,UTIL4NTH,UTIL4T,UTIL4TH
 9025 FORMAT(/1X,'HIGHWAY MODE UTILITY COMPUTATIONS'/
     *       1X,'---------------------------------------'/
     *       1X,'ORIGIN ZONE                =',I10/
     *       1X,'DESTINATION ZONE           =',I10/
     *       1X,'DA AUTO TIME-    NON TOLL  =',F10.2/
     *       1X,'DA AUTO DISTANCE-NON TOLL  =',F10.2/
     *       1X,'DA AUTO COST-    NON TOLL  =',F10.2/
     *       1X,'DA AUTO TIME-        TOLL  =',F10.2/
     *       1X,'DA AUTO DISTANCE-    TOLL  =',F10.2/
     *       1X,'DA AUTO COST-        TOLL  =',F10.2/
     *       1X,'DA TOLL DISTANCE -   TOLL  =',F10.2/
     *       1X,'DA TOLL DISTANCE CONSTANT  =',F10.2//
     *       1X,'2P AUTO TIME-    NON TOLL  =',F10.2/
     *       1X,'2P AUTO DISTANCE-NON TOLL  =',F10.2/
     *       1X,'2P AUTO COST-    NON TOLL  =',F10.2/
     *       1X,'2P AUTO TIME-        TOLL  =',F10.2/
     *       1X,'2P AUTO DISTANCE-    TOLL  =',F10.2/
     *       1X,'2P AUTO COST-        TOLL  =',F10.2/
     *       1X,'2P TOLL DISCOUNT   FACTOR  =',F10.4/
     *       1X,'2P HOV  LANE DISTANCE      =',F10.2/
     *       1X,'2P TOLL DISTANCE -   TOLL  =',F10.2/
     *       1X,'2P TOLL DISTANCE CONSTANT  =',F10.2//
     *       1X,'3P AUTO TIME-    NON TOLL  =',F10.2/
     *       1X,'3P AUTO DISTANCE-NON TOLL  =',F10.2/
     *       1X,'3P AUTO COST-    NON TOLL  =',F10.2/
     *       1X,'3P AUTO TIME-        TOLL  =',F10.2/
     *       1X,'3P AUTO DISTANCE-    TOLL  =',F10.2/
     *       1X,'3P AUTO COST-        TOLL  =',F10.2/
     *       1X,'3P TOLL DISCOUNT   FACTOR  =',F10.4/     
     *       1X,'3P HOV  LANE DISTANCE      =',F10.2/
     *       1X,'3P TOLL DISTANCE -   TOLL  =',F10.2/
     *       1X,'3P TOLL DISTANCE CONSTANT  =',F10.2//
     *       1X,'4P AUTO TIME-    NON TOLL  =',F10.2/
     *       1X,'4P AUTO DISTANCE-NON TOLL  =',F10.2/
     *       1X,'4P AUTO COST-    NON TOLL  =',F10.2/
     *       1X,'4P AUTO TIME-        TOLL  =',F10.2/
     *       1X,'4P AUTO DISTANCE-    TOLL  =',F10.2/
     *       1X,'4P AUTO COST-        TOLL  =',F10.2/
     *       1X,'4P TOLL DISCOUNT   FACTOR  =',F10.4/
     *       1X,'4P HOV  LANE DISTANCE      =',F10.2/
     *       1X,'4P TOLL DISTANCE -   TOLL  =',F10.2/
     *       1X,'4P TOLL DISTANCE CONSTANT  =',F10.2//
     *       1X,'PARKING COST               =',F10.2/
     *       1X,'ORIGIN TERMINAL TIME       =',F10.2/
     *       1X,'DEST.  TERMINAL TIME       =',F10.2//
     *       1X,'DRIVE ALONE NON-TOLL UTIL  =',F10.5/
     *       1X,'DRIVE ALONE TOLL UTILITY   =',F10.5/
     *       1X,'2 PERSON NON-TOLL UTILITY  =',F10.5/
     *       1X,'2 PERSON NON-TOLL/HOV UTIL =',F10.5/
     *       1X,'2 PERSON TOLL UTILITY      =',F10.5/
     *       1X,'2 PERSON TOLL/HOV UTILITY  =',F10.5/
     *       1X,'3 PERSON NON-TOLL UTILITY  =',F10.5/
     *       1X,'3 PERSON NON-TOLL/HOV UTIL =',F10.5/
     *       1X,'3 PERSON TOLL UTILITY      =',F10.5/
     *       1X,'3 PERSON TOLL/HOV UTILITY  =',F10.5/
     *       1X,'4+ PERSON NON-TOLL UTILITY =',F10.5/
     *       1X,'4+ PERSON NON-TOLL/HOV UTIL=',F10.5/
     *       1X,'4+ PERSON TOLL UTILITY     =',F10.5/
     *       1X,'4+ PERSON TOLL/HOV UTIL    =',F10.5//)
      END IF
C....................................................................
C
C  WALK ACCESS MARKET SEGMENTATION LOOP  INDEX M=WALK SEGMENTATION
C
      DO 2000 M=1,7
C
C  INITIALIZE TRIP VALUES
C
      TTRAN=0.0
      TNMOT=0.0
      TAUTO=0.0
      TNMWK=0.0
      TNMBK=0.0
      TDRV0=0.0
      TDSHR=0.0
      TCR=0.0
      TUR=0.0
      TWAY=0.0
      TEXP =0.0
      TLOC =0.0
      TRPD =0.0
      TBRT=0.0
      TDRV2=0.0
      TDRV3=0.0
      TDRV4=0.0
      TLOCW=0.0
      TLOCD=0.0
      TRPDW=0.0
      TRPDD=0.0
      TEXPW=0.0
      TEXPD=0.0
      TCRW=0.0
      TCRB=0.0
      TCRP=0.0
      TCRK=0.0
      TCRW1=0.0
      TCRW2=0.0
      TCRB1=0.0
      TCRB2=0.0
      TCRP1=0.0
      TCRP2=0.0
      TCRP3=0.0
      TCRP4=0.0
      TCRK1=0.0
      TCRK2=0.0
      TCRK3=0.0
      TCRK4=0.0
      TURW=0.0
      TURB=0.0
      TURP=0.0
      TURK=0.0
      TURW1=0.0
      TURW2=0.0
      TURB1=0.0
      TURB2=0.0
      TURP1=0.0
      TURP2=0.0
      TURP3=0.0
      TURP4=0.0
      TURK1=0.0
      TURK2=0.0
      TURK3=0.0
      TURK4=0.0
      TWAYW=0.0
      TWAYD=0.0
      TBRTW=0.0
      TBRTD=0.0
      TBRTDUR=0.0
      TDRV0N=0.0
      TDRV0T=0.0
      TDRV2NH=0.0
      TDRV2NN=0.0
      TDRV2TH=0.0
      TDRV2TN=0.0
      TDRV3NH=0.0
      TDRV3NN=0.0
      TDRV3TH=0.0
      TDRV3TN=0.0
      TDRV4NH=0.0
      TDRV4NN=0.0
      TDRV4TH=0.0
      TDRV4TN=0.0
C
C  PERSON TRIPS BY WALK ACCESS SEGMENT
C
      PERTRP=PERIN(C,JZ)*MWALK(M)
      if(pertrp.lt.0.001.AND.(.NOT.DEBUG).AND.(.NOT.AIRPASS)) GOTO 2000
      DO 201 K=1,89
      UTIL(K)=0.0
      EUTIL(K)=0.0
  201 CONTINUE
      IF(M.EQ.7) GO TO 225
      BWALK=0.0
      DBWLK=0.0
      DRWLK=0.0
      KDRIV=0.0
      CWALK=0.0
      DWALK=0.0
      TWWALK=0.0
      TDWALK=0.0
C
C CONSTRAIN PRODUCTION & ATTRACTION WALK TIME
C
C            1    2   3   4   5   6
      GO TO (211,212,213,214,215,216),M
 211  BWALK1=AMIN1(WLKBACC,SWALK)
      BWALK2=AMIN1(WLKBEGR,SWALK)
      BWALK=BWALK1+BWALK2
      RWALK1=AMIN1(WLKRACC,SWALK)
      RWALK2=AMIN1(WLKREGR,SWALK)
      RWALK=RWALK1+RWALK2
      DBWLK=AMIN1(WLKBEGR,SWALK)
      DRWLK=AMIN1(WLKREGR,SWALK)
      KDRIV=WLKBACC/10.0
      CWALK1=AMIN1(WLKCACC,SWALK)
      CWALK2=AMIN1(WLKCEGR,SWALK)
      CWALK=CWALK1+CWALK2
      DWALK =AMIN1(WALKD,SWALK)
      TWWALK1=AMIN1(WLKWTACC,SWALK)
      TWWALK2=AMIN1(WLKWTEGR,SWALK)
      TWWALK=TWWALK1+TWWALK2
      TDWALK =AMIN1(WALKDT,SWALK)
      TBRTWALK1=AMIN1(WLKWBRTACC,SWALK)
      TBRTWALK2=AMIN1(WLKWBRTEGR,SWALK)
      TBRTWALK=TBRTWALK1+TBRTWALK2
      TDBRTWALK =AMIN1(WALKDBRT,SWALK)
      GO TO 210
  212 BWALK1=AMIN1(WLKBACC,SWALK)
      BWALK2=AMIN1(WLKBEGR,LWALK)
      BWALK=BWALK1+BWALK2
      RWALK1=AMIN1(WLKRACC,SWALK)
      RWALK2=AMIN1(WLKREGR,LWALK)
      RWALK=RWALK1+RWALK2
      DBWLK=AMIN1(WLKBEGR,LWALK)
      DRWLK=AMIN1(WLKREGR,LWALK)
      KDRIV=WLKBACC/10.0
      CWALK1=AMIN1(WLKCACC,SWALK)
      CWALK2=AMIN1(WLKCEGR,LWALK)
      CWALK=CWALK1+CWALK2
      DWALK =AMIN1(WALKD,LWALK)
      TWWALK1=AMIN1(WLKWTACC,SWALK)
      TWWALK2=AMIN1(WLKWTEGR,LWALK)
      TWWALK=TWWALK1+TWWALK2
      TDWALK =AMIN1(WALKDT,LWALK)
      TBRTWALK1=AMIN1(WLKWBRTACC,SWALK)
      TBRTWALK2=AMIN1(WLKWBRTEGR,LWALK)
      TBRTWALK=TBRTWALK1+TBRTWALK2
      TDBRTWALK =AMIN1(WALKDBRT,LWALK)
      GO TO 210
  213 BWALK1=AMIN1(WLKBACC,LWALK)
      BWALK2=AMIN1(WLKBEGR,SWALK)
      BWALK=BWALK1+BWALK2
      RWALK1=AMIN1(WLKRACC,LWALK)
      RWALK2=AMIN1(WLKREGR,SWALK)
      RWALK=RWALK1+RWALK2
      DBWLK=AMIN1(WLKBEGR,SWALK)
      DRWLK=AMIN1(WLKREGR,SWALK)
      KDRIV=WLKBACC/10.0
      CWALK1=AMIN1(WLKCACC,LWALK)
      CWALK2=AMIN1(WLKCEGR,SWALK)
      CWALK=CWALK1+CWALK2
      DWALK =AMIN1(WALKD,SWALK)
      TWWALK1=AMIN1(WLKWTACC,LWALK)
      TWWALK2=AMIN1(WLKWTEGR,SWALK)
      TWWALK=TWWALK1+TWWALK2
      TDWALK =AMIN1(WALKDT,SWALK)
      TBRTWALK1=AMIN1(WLKWBRTACC,LWALK)
      TBRTWALK2=AMIN1(WLKWBRTEGR,SWALK)
      TBRTWALK=TBRTWALK1+TBRTWALK2
      TDBRTWALK =AMIN1(WALKDBRT,SWALK)
      GO TO 210
  214 BWALK1=AMIN1(WLKBACC,LWALK)
      BWALK2=AMIN1(WLKBEGR,LWALK)
      BWALK=BWALK1+BWALK2
      RWALK1=AMIN1(WLKRACC,LWALK)
      RWALK2=AMIN1(WLKREGR,LWALK)
      RWALK=RWALK1+RWALK2
      DBWLK=AMIN1(WLKBEGR,LWALK)
      DRWLK=AMIN1(WLKREGR,LWALK)
      KDRIV=WLKBACC/10.0
      CWALK1=AMIN1(WLKCACC,LWALK)
      CWALK2=AMIN1(WLKCEGR,LWALK)
      CWALK=CWALK1+CWALK2
      DWALK =AMIN1(WALKD,LWALK)
      TWWALK1=AMIN1(WLKWTACC,LWALK)
      TWWALK2=AMIN1(WLKWTEGR,LWALK)
      TWWALK=TWWALK1+TWWALK2
      TDWALK =AMIN1(WALKDT,LWALK)
      TBRTWALK1=AMIN1(WLKWBRTACC,LWALK)
      TBRTWALK2=AMIN1(WLKWBRTEGR,LWALK)
      TBRTWALK=TBRTWALK1+TBRTWALK2
      TDBRTWALK =AMIN1(WALKDBRT,LWALK)
      GO TO 210
  215 BWALK=AMIN1(WLKBEGR,SWALK)
      RWALK=AMIN1(WLKREGR,SWALK)
      DBWLK=AMIN1(WLKBEGR,SWALK)
      DRWLK=AMIN1(WLKREGR,SWALK)
      KDRIV=WLKBACC/10.0
      CWALK=AMIN1(WLKCEGR,SWALK)
      DWALK=AMIN1(WALKD,SWALK)
      TWWALK=AMIN1(WLKWTEGR,SWALK)
      TDWALK=AMIN1(WALKDT,SWALK)
      TBRTWALK=AMIN1(WLKWBRTEGR,SWALK)
      TDBRTWALK =AMIN1(WALKDBRT,SWALK)
      GOTO 210
  216 BWALK=AMIN1(WLKBEGR,LWALK)
      RWALK=AMIN1(WLKREGR,LWALK)
      DBWLK=AMIN1(WLKBEGR,LWALK)
      DRWLK=AMIN1(WLKREGR,LWALK)
      KDRIV=WLKBACC/10.0
      CWALK=AMIN1(WLKCEGR,LWALK)
      DWALK=AMIN1(WALKD,LWALK)
      TWWALK=AMIN1(WLKWTEGR,LWALK)
      TDWALK=AMIN1(WALKDT,LWALK)
      TBRTWALK=AMIN1(WLKWBRTEGR,LWALK)
      TDBRTWALK =AMIN1(WALKDBRT,LWALK)
      
  210 CONTINUE
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9006) FF,C,M,PERTRP,BWALK,DBWLK,RWALK,DRWLK,CWALK,DWALK,
     *                TWWALK,TDWALK,TBRTWALK,TDBRTWALK
 9006 FORMAT(A1,1X,'INCOME GROUP=',I2,' MSEG=',I2,/
     *       1X,'---------------------------'//
     *       1X,' PERTRP=',F12.5//
     *       1X,'SUMMARY OF CONSTRAINED WALK TIMES'/
     *       1X,'-----------------------------------'/
     *       1X,'WALK   --> LOCAL BUS =',F8.2/
     *       1X,'DRIVE  --> LOCAL BUS =',F8.2/
     *       1X,'WALK   --> RAPID BUS =',F8.2/
     *       1X,'DRIVE  --> RAPID BUS =',F8.2/
     *       1X,'WALK   --> EXP BUS   =',F8.2/
     *       1X,'DRIVE  --> EXP BUS   =',F8.2/
     *       1X,'WALK   --> TRANSITWAY=',F8.2/
     *       1X,'DRIVE  --> TRANSITWAY=',F8.2/
     *       1X,'WALK   --> BUS RAPID =',F8.2/
     *       1X,'DRIVE  --> BUS RAPID =',F8.2)
      END IF
C....................................................................
C
C  COMPUTE STATION LEVEL UTILITIES FOR COMMUTER RAIL & URBAN RAIL
C
C  COMMUTER RAIL
C
      DO 301,ICH=1,2
C..WALK STATION #1/2
      IST=ICH
      IF((WUTIL(1,ICH).NE.0.0).AND.(M.LT.5)) THEN
C            1    2   3   4
      GO TO (371,372,373,374),M
 371  WLKM1=AMIN1(MWALKW(1,IST),SWALKCR)
      WLKM2=AMIN1(STAZNE(3,(ASTA(1,ICH)-MAX_IZONES),JZ),SWALKCR)
      WLKM=WLKM1+WLKM2
      GOTO 375
 372  WLKM1=AMIN1(MWALKW(1,IST),SWALKCR)
      WLKM2=AMIN1(STAZNE(3,(ASTA(1,ICH)-MAX_IZONES),JZ),LWALKCR)
      WLKM=WLKM1+WLKM2
      GOTO 375
 373  WLKM1=AMIN1(MWALKW(1,IST),LWALKCR)
      WLKM2=AMIN1(STAZNE(3,(ASTA(1,ICH)-MAX_IZONES),JZ),SWALKCR)
      WLKM=WLKM1+WLKM2
      GOTO 375
 374  WLKM1=AMIN1(MWALKW(1,IST),LWALKCR)
      WLKM2=AMIN1(STAZNE(3,(ASTA(1,ICH)-MAX_IZONES),JZ),LWALKCR)
      WLKM=WLKM1+WLKM2
 375  CONTINUE
C
       SC=OSTA(1,ICH)-MAX_IZONES
       SC2=ASTA(1,ICH)-MAX_IZONES
       IF(SC.LT.0) SC=MAX_STATIONS
       IF(SC2.LT.0) SC2=MAX_STATIONS
       UTIL(ICH)=WUTIL(1,ICH) + COEFF(7)*WLKM + 
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI
C
       IF(UTIL(ICH).LE.(-100.0)) THEN
       EUTIL(ICH)=0.0
       ELSE
       EUTIL(ICH)=EXP(UTIL(ICH))
       END IF
      END IF
 301  CONTINUE
C
C
C..BUS STATION #1/2
      DO 303,ICH=3,4
      IST=ICH-2
      IF((BUTIL(1,IST).NE.0.0).AND.(M.LT.5)) THEN
C            1    2   3   4
      GO TO (381,382,383,384),M
 381  WLKM1=AMIN1(MWALKB(1,IST),SWALKCR)
      WLKM2=AMIN1(STAZNE(3,(ASTA(1,ICH)-MAX_IZONES),JZ),SWALKCR)
      WLKM=WLKM1+WLKM2
      GOTO 385
 382  WLKM1=AMIN1(MWALKB(1,IST),SWALKCR)
      WLKM2=AMIN1(STAZNE(3,(ASTA(1,ICH)-MAX_IZONES),JZ),LWALKCR)
      WLKM=WLKM1+WLKM2
      GOTO 385
 383  WLKM1=AMIN1(MWALKB(1,IST),LWALKCR)
      WLKM2=AMIN1(STAZNE(3,(ASTA(1,ICH)-MAX_IZONES),JZ),SWALKCR)
      WLKM=WLKM1+WLKM2
      GOTO 385
 384  WLKM1=AMIN1(MWALKB(1,IST),LWALKCR)
      WLKM2=AMIN1(STAZNE(3,(ASTA(1,ICH)-MAX_IZONES),JZ),LWALKCR)
      WLKM=WLKM1+WLKM2
 385  CONTINUE
C
      SC=OSTA(1,ICH)-MAX_IZONES
      SC2=ASTA(1,ICH)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      UTIL(ICH)=BUTIL(1,IST) + COEFF(7)*WLKM +
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ)+
     *                CRBSKIM(IST,6))*FCPI
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
 303  CONTINUE
C
C..P&R STATION #1-4
C
      DO 305,ICH=5,8
      IST=ICH-4
      IF(M.EQ.1.OR.M.EQ.3.OR.M.EQ.5) THEN
      WLKM=AMIN1(STAZNE(3,(ASTA(1,ICH)-MAX_IZONES),JZ),5.0)
      ELSEIF(M.EQ.2.OR.M.EQ.4.OR.M.EQ.6) THEN
      WLKM=AMIN1(STAZNE(3,(ASTA(1,ICH)-MAX_IZONES),JZ),10.0)
      ENDIF
      IF(PUTIL(1,IST).NE.0.0) THEN
      SC=OSTA(1,ICH)-MAX_IZONES
      SC2=ASTA(1,ICH)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      UTIL(ICH)=PUTIL(1,IST) + COEFF(7)*WLKM+
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI+
     *          (KDTRN(C)+KPNR(C))/(LSUMA*LSUMS)
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
 305  CONTINUE
C
C..K&R STATION #1-4
      DO 307,ICH=9,12
      IST=ICH-8
      IF(M.EQ.1.OR.M.EQ.3.OR.M.EQ.5) THEN
      WLKM=AMIN1(STAZNE(3,(ASTA(1,ICH)-MAX_IZONES),JZ),5.0)
      ELSEIF(M.EQ.2.OR.M.EQ.4.OR.M.EQ.6)THEN
      WLKM=AMIN1(STAZNE(3,(ASTA(1,ICH)-MAX_IZONES),JZ),10.0)
      ENDIF
      IF(KUTIL(1,IST).NE.0.0) THEN
      SC=OSTA(1,ICH)-MAX_IZONES
      SC2=ASTA(1,ICH)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      UTIL(ICH)=KUTIL(1,IST) + COEFF(7)*WLKM+KDTRN(C)/(LSUMA*LSUMS)+
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
 307  CONTINUE
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9005) (UTIL(K),EUTIL(K),K=1,12)
 9005 FORMAT(/1X,'SUMMARY OF COMMUTER RAIL UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',11X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',8X,'----------',5X,
     *          '----------'/
     *       1X,'COMMUTER RAIL  WALK-->STA #1 ',F10.5,3X,E12.5/
     *       1X,'COMMUTER RAIL  WALK-->STA #2 ',F10.5,3X,E12.5//
     *       1X,'COMMUTER RAIL  BUS -->STA #1 ',F10.5,3X,E12.5/
     *       1X,'COMMUTER RAIL  BUS -->STA #2 ',F10.5,3X,E12.5//
     *       1X,'COMMUTER RAIL  P&R -->STA #1 ',F10.5,3X,E12.5/
     *       1X,'COMMUTER RAIL  P&R -->STA #2 ',F10.5,3X,E12.5/
     *       1X,'COMMUTER RAIL  P&R -->STA #3 ',F10.5,3X,E12.5/
     *       1X,'COMMUTER RAIL  P&R -->STA #4 ',F10.5,3X,E12.5//
     *       1X,'COMMUTER RAIL  K&R -->STA #1 ',F10.5,3X,E12.5/
     *       1X,'COMMUTER RAIL  K&R -->STA #2 ',F10.5,3X,E12.5/
     *       1X,'COMMUTER RAIL  K&R -->STA #3 ',F10.5,3X,E12.5/
     *       1X,'COMMUTER RAIL  K&R -->STA #4 ',F10.5,3X,E12.5)
      END IF
C
C  URBAN RAIL
C
      DO 311,ICH=13,14
C..WALK STATION #1/2
      IST=ICH-12
      IF((WUTIL(2,IST).NE.0.0).AND.(M.LT.5)) THEN
C
C            1    2   3   4
      GO TO (571,572,573,574),M
 571  WLKM1=AMIN1(MWALKW(2,IST),SWALKUR)
      WLKM2=AMIN1(STAZNE(3,(ASTA(2,IST)-MAX_IZONES),JZ),SWALKUR)
      WLKM=WLKM1+WLKM2
      GOTO 575
 572  WLKM1=AMIN1(MWALKW(2,IST),SWALKUR)
      WLKM2=AMIN1(STAZNE(3,(ASTA(2,IST)-MAX_IZONES),JZ),LWALKUR)
      WLKM=WLKM1+WLKM2
      GOTO 575
 573  WLKM1=AMIN1(MWALKW(2,IST),LWALKUR)
      WLKM2=AMIN1(STAZNE(3,(ASTA(2,IST)-MAX_IZONES),JZ),SWALKUR)
      WLKM=WLKM1+WLKM2
      GOTO 575
 574  WLKM1=AMIN1(MWALKW(2,IST),LWALKUR)
      WLKM2=AMIN1(STAZNE(3,(ASTA(2,IST)-MAX_IZONES),JZ),LWALKUR)
      WLKM=WLKM1+WLKM2
 575  CONTINUE
      SC=OSTA(2,IST)-MAX_IZONES
      SC2=ASTA(2,IST)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      UTIL(ICH)=WUTIL(2,IST) + COEFF(7)*WLKM+
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI
       IF(UTIL(ICH).LE.(-100.0)) THEN
       EUTIL(ICH)=0.0
       ELSE
       EUTIL(ICH)=EXP(UTIL(ICH))
       END IF
      END IF
 311  CONTINUE
C
C
C..BUS STATION #1/2
      DO 313,ICH=15,16
      IST=ICH-14
      INT=ICH-12
      IF((BUTIL(2,IST).NE.0.0).AND.(M.LT.5)) THEN
C
C            1    2   3   4
      GO TO (581,582,583,584),M
 581  WLKM1=AMIN1(MWALKB(2,IST),SWALKUR)
      WLKM2=AMIN1(STAZNE(3,(ASTA(2,INT)-MAX_IZONES),JZ),SWALKUR)
      WLKM=WLKM1+WLKM2
      GOTO 585
 582  WLKM1=AMIN1(MWALKB(2,IST),SWALKUR)
      WLKM2=AMIN1(STAZNE(3,(ASTA(2,INT)-MAX_IZONES),JZ),LWALKUR)
      WLKM=WLKM1+WLKM2
      GOTO 585
 583  WLKM1=AMIN1(MWALKB(2,IST),LWALKUR)
      WLKM2=AMIN1(STAZNE(3,(ASTA(2,INT)-MAX_IZONES),JZ),SWALKUR)
      WLKM=WLKM1+WLKM2
      GOTO 585
 584  WLKM1=AMIN1(MWALKB(2,IST),LWALKUR)
      WLKM2=AMIN1(STAZNE(3,(ASTA(2,INT)-MAX_IZONES),JZ),LWALKUR)
      WLKM=WLKM1+WLKM2
 585  CONTINUE
      SC=OSTA(2,INT)-MAX_IZONES
      SC2=ASTA(2,INT)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      UTIL(ICH)=BUTIL(2,IST) + COEFF(7)*WLKM+
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ)+
     *                URBSKIM(IST,6))*FCPI
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
 313  CONTINUE
C
C..P&R STATION #1-4
C
      DO 315,ICH=17,20
      IST=ICH-16
      INT=ICH-12
      IF(M.EQ.1.OR.M.EQ.3.OR.M.EQ.5) THEN
      WLKM=AMIN1(STAZNE(3,(ASTA(2,INT)-MAX_IZONES),JZ),5.0)
      ELSEIF(M.EQ.2.OR.M.EQ.4.OR.M.EQ.6) THEN
      WLKM=AMIN1(STAZNE(3,(ASTA(2,INT)-MAX_IZONES),JZ),10.0)
      ENDIF
      IF(PUTIL(2,IST).NE.0.0) THEN
      SC=OSTA(2,INT)-MAX_IZONES
      SC2=ASTA(2,INT)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      UTIL(ICH)=PUTIL(2,IST) + COEFF(7)*WLKM+
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI+
     *          (KDTRN(C)+KPNR(C))/(LSUMA*LSUMS)
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
 315  CONTINUE
C
C..K&R STATION #1-4
      DO 317,ICH=21,24
      IST=ICH-20
      INT=ICH-12
      IF(M.EQ.1.OR.M.EQ.3.OR.M.EQ.5) THEN
      WLKM=AMIN1(STAZNE(3,(ASTA(2,INT)-MAX_IZONES),JZ),5.0)
      ELSEIF(M.EQ.2.OR.M.EQ.4.OR.M.EQ.6) THEN
      WLKM=AMIN1(STAZNE(3,(ASTA(2,INT)-MAX_IZONES),JZ),10.0)
      ENDIF
      IF(KUTIL(2,IST).NE.0.0) THEN
      SC=OSTA(2,INT)-MAX_IZONES
      SC2=ASTA(2,INT)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      UTIL(ICH)=KUTIL(2,IST) + COEFF(7)*WLKM+KDTRN(C)/(LSUMA*LSUMS)+
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
 317  CONTINUE
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9007) (UTIL(K),EUTIL(K),K=13,24)
 9007 FORMAT(/1X,'SUMMARY OF URBAN RAIL UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',8X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',5X,'----------',5X,
     *          '----------'/
     *       1X,'URBAN RAIL  WALK-->STA #1 ',F10.5,3X,E12.5/
     *       1X,'URBAN RAIL  WALK-->STA #2 ',F10.5,3X,E12.5//
     *       1X,'URBAN RAIL  BUS -->STA #1 ',F10.5,3X,E12.5/
     *       1X,'URBAN RAIL  BUS -->STA #2 ',F10.5,3X,E12.5//
     *       1X,'URBAN RAIL  P&R -->STA #1 ',F10.5,3X,E12.5/
     *       1X,'URBAN RAIL  P&R -->STA #2 ',F10.5,3X,E12.5/
     *       1X,'URBAN RAIL  P&R -->STA #3 ',F10.5,3X,E12.5/
     *       1X,'URBAN RAIL  P&R -->STA #4 ',F10.5,3X,E12.5//
     *       1X,'URBAN RAIL  K&R -->STA #1 ',F10.5,3X,E12.5/
     *       1X,'URBAN RAIL  K&R -->STA #2 ',F10.5,3X,E12.5/
     *       1X,'URBAN RAIL  K&R -->STA #3 ',F10.5,3X,E12.5/
     *       1X,'URBAN RAIL  K&R -->STA #4 ',F10.5,3X,E12.5)
      END IF
C
C
C....................................................................
C
C   TRANSIT MODE UTILITIES
C
C..WALK ACCESS TO LOCAL BUS
      IF(M.LT.5) THEN
      IF(WBUTL.NE.0.0) THEN
      MODINC(1)=1
      FAREVAL(1)=FARE(1,JZ)*FCPI
      UTIL(44)=WBUTL + COEFF(17)*BWALK  + 
     *         COEFF(60+C)*FARE(1,JZ)*FCPI   +
     *         KWBUS(C)/(LSUMS*LSUMA)
      EUTIL(44)=EXP(UTIL(44))
      END IF
      END IF
C..DRIVE ACCESS TO LOCAL BUS
      IF(WBUTL.NE.0.0) THEN
      MODINC(2)=1
      FAREVAL(2)=FARE(1,JZ)*FCPI
      UTIL(45)=WBUTL + COEFF(17)*DBWLK + COEFF(18)*KDRIV +
     *         (KDBUS(C)+KDTRN(C))/(LSUMA*LSUMS)+
     *         KINFL(C)/(LSUMA*LSUMS) + COEFF(60+C)*FARE(1,JZ)*FCPI
      EUTIL(45)=EXP(UTIL(45))
      END IF
C..WALK ACCESS TO RAPID BUS
      IF(M.LT.5) THEN
      IF(WRUTL.NE.0.0) THEN
      MODINC(15)=1
      FAREVAL(3)=FARE(4,JZ)*FCPI
      UTIL(72)=WRUTL + COEFF(17)*RWALK  +
     *         COEFF(60+C)*FARE(4,JZ)*FCPI   +
     *         KWRPD(C)/(LSUMS*LSUMA)
      IF(BIVT(4,JZ).GT.0.0) THEN
       UTIL(72)=UTIL(72)+KBRPDW
      END IF
      EUTIL(72)=EXP(UTIL(72))
      END IF
      END IF
C..DRIVE ACCESS TO RAPID BUS
      IF(WRUTL.NE.0.0) THEN
      MODINC(16)=1
      FAREVAL(4)=FARE(4,JZ)*FCPI
      UTIL(73)=WRUTL + COEFF(17)*DRWLK + COEFF(18)*(WLKRACC/10.0) +
     *         (KDRPD(C)+KDTRN(C))/(LSUMA*LSUMS) +
     *          COEFF(60+C)*FARE(4,JZ)*FCPI+
     *          KINFR/(LSUMA*LSUMS)
      IF(BIVT(4,JZ).GT.0.0) THEN
       UTIL(73)=UTIL(73)+KBRPDD  
      END IF
      EUTIL(73)=EXP(UTIL(73))
      END IF      
C..WALK ACCESS TO EXPRESS BUS
      IF(M.LT.5) THEN
      IF(WCUTL.NE.0.0) THEN
      MODINC(3)=1
      FAREVAL(5)=FARE(2,JZ)*FCPI
      UTIL(46)=WCUTL +  COEFF(17)*CWALK +
     *         COEFF(60+C)*FARE(2,JZ)*FCPI   +
     *         KWEXP(C)/(LSUMA*LSUMS)
      EUTIL(46)=EXP(UTIL(46))
      END IF
      END IF
C..DRIVE ACCESS TO EXPRESS BUS
      IF(DCUTL.NE.0.0) THEN
      MODINC(4)=1
      IT=CSTAE-MAX_IZONES
      FAREVAL(6)=STAZNE(4,IT,JZ)*FCPI
      UTIL(47)=DCUTL + COEFF(17)*DWALK +
     *          COEFF(60+C)*STAZNE(4,IT,JZ)*FCPI +
     *         (KDEXP(C)+KDTRN(C))/(LSUMA*LSUMS)
      EUTIL(47)=EXP(UTIL(47))
      END IF
C..WALK ACCESS TO TRANSITWAY BUS
      IF(M.LT.5) THEN
      IF(WTUTL.NE.0.0) THEN
      MODINC(13)=1
      UTIL(56)=WTUTL +  COEFF(99)*COEFF(17)*TWWALK +
     *         COEFF(60+C)*FARE(3,JZ)*FCPI  +
     *         KWWAY(C)/(LSUMA*LSUMS)
      EUTIL(56)=EXP(UTIL(56))
      END IF
      END IF
C..DRIVE ACCESS TO TRANSITWAY BUS
      IF(DTUTL.NE.0.0) THEN
      MODINC(14)=1
      IT=CSTAT-MAX_IZONES
      UTIL(57)=DTUTL + COEFF(17)*TDWALK +
     *          COEFF(60+C)*STAZNE(4,IT,JZ)*FCPI +
     *         (KDWAY(C)+KDTRN(C))/(LSUMA*LSUMS)
      EUTIL(57)=EXP(UTIL(57))
      END IF
C..WALK ACCESS TO BUS RAPID (BRT)
      IF(M.LT.5) THEN
      IF(WBRTUTL.NE.0.0) THEN
      MODINC(17)=1
      UTIL(78)=WBRTUTL +  COEFF(17)*TBRTWALK +
     *         COEFF(60+C)*FARE(5,JZ)*FCPI  +
     *         KWBRT(C)/(LSUMA*LSUMS)
      EUTIL(78)=EXP(UTIL(78))
      END IF
      END IF
C..DRIVE ACCESS TO BUS RAPID (BRT)
      IF(DBRTUTL.NE.0.0.AND.CSTABRT.GT.0) THEN
      MODINC(18)=1
      IT=CSTABRT-MAX_IZONES
      UTIL(79)=DBRTUTL + COEFF(17)*TDBRTWALK +
     *          COEFF(60+C)*STAZNE(4,IT,JZ)*FCPI +
     *         (KDBRT(C)+KDTRN(C))/(LSUMA*LSUMS)
      EUTIL(79)=EXP(UTIL(79))
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9306) (UTIL(K),EUTIL(K),K=44,47),UTIL(56),EUTIL(56),
     *                UTIL(57),EUTIL(57),UTIL(72),EUTIL(72),
     *                UTIL(73),EUTIL(73),UTIL(78),EUTIL(78),
     *                UTIL(79),EUTIL(79)
 9306 FORMAT(/1X,'SUMMARY OF TRANSIT MODE UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',12X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',10X,'----------',5X,
     *          '----------'/
     *       1X,'WALK  ACCESS TO LOCAL      BUS=',F10.5,3X,E12.5/
     *       1X,'DRIVE ACCESS TO LOCAL      BUS=',F10.5,3X,E12.5//
     *       1X,'WALK  ACCESS TO EXPRESS    BUS=',F10.5,3X,E12.5/
     *       1X,'DRIVE ACCESS TO EXPRESS    BUS=',F10.5,3X,E12.5/
     *       1X,'WALK  ACCESS TO TRANSITWAY BUS=',F10.5,3X,E12.5/
     *       1X,'DRIVE ACCESS TO TRANSITWAY BUS=',F10.5,3X,E12.5/
     *       1X,'WALK  ACCESS TO RAPID      BUS=',F10.5,3X,E12.5/
     *       1X,'DRIVE ACCESS TO RAPID      BUS=',F10.5,3X,E12.5/
     *       1X,'WALK  ACCESS TO BUS RAPID  BRT=',F10.5,3X,E12.5/
     *       1X,'DRIVE ACCESS TO BUS RAPID  BRT=',F10.5,3X,E12.5)
      END IF
C....................................................................
C
C  GENERAL MODE UTILITIES & PROBABILITIES
C
C
C..DRIVE ALONE UTILITY-NON-TOLL
C
 225  IF(UTIL0NT.NE.0.0) THEN
      UTIL(38)=UTIL0NT
      EUTIL(38)=EXP(UTIL(38))
	    ENDIF
C
C..DRIVE ALONE UTILITY-TOLL
C
      IF(UTIL0T.NE.0.0) THEN
	    UTIL(39)=UTIL0T
      EUTIL(39)=EXP(UTIL(39))
	    ENDIF
C
C..2 PERSON AUTO UTILITY - NON-TOLL/NON-HOV
C
      IF(UTIL2NT.NE.0.0) THEN
	    UTIL(40)=UTIL2NT
      EUTIL(40)=EXP(UTIL(40))
	    ENDIF
C
C..2 PERSON AUTO UTILITY - NON-TOLL/HOV
C
      IF(UTIL2NTH.NE.0.0) THEN
	    UTIL(84)=UTIL2NTH
      EUTIL(84)=EXP(UTIL(84))
	    ENDIF
C
C..2 PERSON AUTO UTILITY - TOLL/NON-HOV
C
      IF(UTIL2T.NE.0.0) THEN
	    UTIL(41)=UTIL2T
      EUTIL(41)=EXP(UTIL(41))
	    ENDIF
C
C..2 PERSON AUTO UTILITY - TOLL/HOV
C
      IF(UTIL2TH.NE.0.0) THEN
	    UTIL(85)=UTIL2TH
      EUTIL(85)=EXP(UTIL(85))
	    ENDIF
C
C..3+ PERSON AUTO UTILITY - NON-TOLL/NON-HOV
      IF(UTIL3NT.NE.0.0) THEN
	    UTIL(42)=UTIL3NT
      EUTIL(42)=EXP(UTIL(42))
	    ENDIF
C
C..3+ PERSON AUTO UTILITY - NON-TOLL/HOV
      IF(UTIL3NTH.NE.0.0) THEN
	    UTIL(86)=UTIL3NTH
      EUTIL(86)=EXP(UTIL(86))
	    ENDIF
C
C..3+ PERSON AUTO UTILITY - TOLL
      IF(UTIL3T.NE.0.0) THEN
	    UTIL(43)=UTIL3T
      EUTIL(43)=EXP(UTIL(43))
	    ENDIF
C
C..3+ PERSON AUTO UTILITY - TOLL/HOV
      IF(UTIL3TH.NE.0.0) THEN
	    UTIL(87)=UTIL3TH
      EUTIL(87)=EXP(UTIL(87))
	    ENDIF
C
C..4+ PERSON AUTO UTILITY - NON-TOLL
      IF(UTIL4NT.NE.0.0) THEN
	    UTIL(81)=UTIL4NT
      EUTIL(81)=EXP(UTIL(81))
	    ENDIF
C
C..4+ PERSON AUTO UTILITY - NON-TOLL/HOV
      IF(UTIL4NTH.NE.0.0) THEN
	    UTIL(88)=UTIL4NTH
      EUTIL(88)=EXP(UTIL(88))
	    ENDIF
C
C..4+ PERSON AUTO UTILITY - TOLL
      IF(UTIL4T.NE.0.0) THEN
	    UTIL(82)=UTIL4T
      EUTIL(82)=EXP(UTIL(82))
	    ENDIF
C
C..4+ PERSON AUTO UTILITY - TOLL/HOV
      IF(UTIL4TH.NE.0.0) THEN
	    UTIL(89)=UTIL4TH
      EUTIL(89)=EXP(UTIL(89))
	ENDIF
C
C..DRIVE-ALONE PROBABILITIES
      DAPROB(1)=1.0
      DAPROB(2)=0.0
      LSDA=0.0
      DENOM=EUTIL(38) + EUTIL(39)
      IF(DENOM.GT.0.0) THEN
      LSDA=DLOG(DENOM)
      DAPROB(1)=EUTIL(38)/DENOM
      DAPROB(2)=1.0-DAPROB(1)
      ENDIF
C..2-PERSON DRIVE
      P2PROB(1)=1.0
      P2PROB(2)=0.0
      P2PROB(3)=0.0
      P2PROB(4)=0.0
      LS2PER=0.0
      DENOM=EUTIL(40)+EUTIL(41)+EUTIL(84)+EUTIL(85)
      IF(DENOM.GT.0.0) THEN
      LS2PER=DLOG(DENOM)
      P2PROB(2)=EUTIL(84)/DENOM
      P2PROB(3)=EUTIL(41)/DENOM
      P2PROB(4)=EUTIL(85)/DENOM
      P2PROB(1)=1.0 - P2PROB(2) - P2PROB(3) - P2PROB(4)
      ENDIF
C..3+PERSON DRIVE
      P3PROB(1)=1.0
      P3PROB(2)=0.0
      P3PROB(3)=0.0
      P3PROB(4)=0.0
      LS3PER=0.0
      DENOM=EUTIL(42) + EUTIL(43)+EUTIL(86)+EUTIL(87)
      IF(DENOM.GT.0.0) THEN
      LS3PER=DLOG(DENOM)
      P3PROB(2)=EUTIL(86)/DENOM
      P3PROB(3)=EUTIL(43)/DENOM
      P3PROB(4)=EUTIL(87)/DENOM
      P3PROB(1)=1.0-P3PROB(2)-P3PROB(3)-P3PROB(4)
      ENDIF
C..4+PERSON DRIVE
      P4PROB(1)=1.0
      P4PROB(2)=0.0
      P4PROB(3)=0.0
      P4PROB(4)=0.0
      LS4PER=0.0
      DENOM=EUTIL(81) + EUTIL(82)+EUTIL(88)+EUTIL(89)
      IF(DENOM.GT.0.0) THEN
      LS4PER=DLOG(DENOM)
      P4PROB(2)=EUTIL(88)/DENOM
      P4PROB(3)=EUTIL(82)/DENOM
      P4PROB(4)=EUTIL(89)/DENOM
      P4PROB(1)=1.0-P4PROB(2)-P4PROB(3)-P4PROB(4)
      ENDIF
C..NON-MOTORIZED
      NMPROB(1)=1.0
      NMPROB(2)=0.0
      LSNMOT=0.0
      EUTIL(76)=EXP(UTILBK)*EXP(KBIKE(C)/LSUMA)
      EUTIL(75)=EXP(UTILWK)
      DENOM=EUTIL(75)+EUTIL(76)
      IF(DENOM.GT.0.0) THEN
      LSNMOT=DLOG(DENOM)
      NMPROB(1)=EUTIL(75)/DENOM
      NMPROB(2)=1.0-NMPROB(1)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9016) C,M,
     *               UTIL(38),EUTIL(38),DAPROB(1),
     *               UTIL(39),EUTIL(39),DAPROB(2),
     *               UTIL(40),EUTIL(40),P2PROB(1),
     *               UTIL(84),EUTIL(84),P2PROB(2),
     *               UTIL(41),EUTIL(41),P2PROB(3),
     *               UTIL(85),EUTIL(85),P2PROB(4),
     *               UTIL(42),EUTIL(42),P3PROB(1),
     *               UTIL(86),EUTIL(86),P3PROB(2),
     *               UTIL(43),EUTIL(43),P3PROB(3),
     *               UTIL(87),EUTIL(87),P3PROB(4),
     *               UTIL(81),EUTIL(81),P4PROB(1),
     *               UTIL(88),EUTIL(88),P4PROB(2),
     *               UTIL(82),EUTIL(82),P4PROB(3),
     *               UTIL(87),EUTIL(87),P4PROB(4),
     *               UTIL(75),EUTIL(75),NMPROB(1),
     *               UTIL(76),EUTIL(76),NMPROB(2)
 9016 FORMAT(/1X,'INCOME GROUP=',I2,' MSEG=',I2//
     *       1X,'SUMMARY OF GENERAL MODE UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',7X,'UTIL',10X,'EUTIL',3X,
     *       3X,' PROB'/
     *       1X,'                     ',4X,'----------',5X,
     *          '----------',3X,'------'/
     *       1X,'DRIVE ALONE NON-TOLL     ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'DRIVE ALONE TOLL         ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'2  PERSON   NON-TOLL     ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'2  PERSON   NON-TOLL/HOV ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'2  PERSON   TOLL         ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'2  PERSON   TOLL/HOV     ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'3  PERSON   NON-TOLL     ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'3  PERSON   NON-TOLL/HOV ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'3  PERSON   TOLL         ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'3  PERSON   TOLL/HOV     ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'4+ PERSON   NON-TOLL     ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'4+ PERSON   NON-TOLL/HOV ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'4+ PERSON   TOLL         ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'4+ PERSON   TOLL/HOV     ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'WALK MODE                ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'BIKE MODE                ',F10.5,3X,E12.5,3X,F6.4/)
      END IF
C
C....................................................................
C
C   CALCULATE STATION LEVEL PROBABILITIES
C    BY MODE
C
C
C..WALK ACCESS - COMMUTER RAIL
      CWPROB(1)=1.0
      CWPROB(2)=0.0
      LSCWLK=0.0
      DENOM=EUTIL(1)+EUTIL(2)
      IF(DENOM.GT.0.0) THEN
      MODINC(5)=1
      LSCWLK=DLOG(DENOM)
      CWPROB(1)=EUTIL(1)/DENOM
      CWPROB(2)=1.0-CWPROB(1)
      END IF
C..BUS ACCESS - COMMUTER RAIL
      CBPROB(1)=1.0
      CBPROB(2)=0.0
      LSCBUS=0.0
      DENOM=EUTIL(3)+EUTIL(4)
      IF(DENOM.GT.0.0) THEN
      MODINC(6)=1
      LSCBUS=DLOG(DENOM)
      CBPROB(1)=EUTIL(3)/DENOM
      CBPROB(2)=1.0-CBPROB(1)
      END IF
C..P&R ACCESS - COMMUTER RAIL
      CPPROB(1)=1.0
      CPPROB(2)=0.0
      CPPROB(3)=0.0
      CPPROB(4)=0.0
      LSCPR=0.0
      DENOM=EUTIL(5)+EUTIL(6)+EUTIL(7)+EUTIL(8)
      IF(DENOM.GT.0.0) THEN
      MODINC(7)=1
      LSCPR=DLOG(DENOM)
      CPPROB(1)=EUTIL(5)/DENOM
      CPPROB(2)=EUTIL(6)/DENOM
      CPPROB(3)=EUTIL(7)/DENOM
      CPPROB(4)=1.0-CPPROB(1)-CPPROB(2)-CPPROB(3)
      END IF
C..K&R ACCESS - COMMUTER RAIL
      CKPROB(1)=1.0
      CKPROB(2)=0.0
      CKPROB(3)=0.0
      CKPROB(4)=0.0
      LSCKR=0.0
      DENOM=EUTIL(9)+EUTIL(10)+EUTIL(11)+EUTIL(12)
      IF(DENOM.GT.0.0) THEN
      MODINC(8)=1
      LSCKR=DLOG(DENOM)
      CKPROB(1)=EUTIL(9)/DENOM
      CKPROB(2)=EUTIL(10)/DENOM
      CKPROB(3)=EUTIL(11)/DENOM
      CKPROB(4)=1.0-CKPROB(1)-CKPROB(2)-CKPROB(3)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9008) CWPROB(1),CWPROB(2),CBPROB(1),CBPROB(2),
     *               CPPROB(1),CPPROB(2),CPPROB(3),CPPROB(4),
     *               CKPROB(1),CKPROB(2),CKPROB(3),CKPROB(4),
     *               LSCWLK,LSCBUS,LSCPR,LSCKR
 9008 FORMAT(/1X,'SUMMARY OF COMMUTER RAIL STATION CHOICE PROB: '/
     *       1X,'---------------------------------------------'/
     *       1X,'WALK  ACCESS STATION #1=',F8.5/ 
     *       1X,'WALK  ACCESS STATION #2=',F8.5//
     *       1X,'BUS   ACCESS STATION #1=',F8.5/
     *       1X,'BUS   ACCESS STATION #2=',F8.5//
     *       1X,'P&R   ACCESS STATION #1=',F8.5/
     *       1X,'P&R   ACCESS STATION #2=',F8.5/
     *       1X,'P&R   ACCESS STATION #3=',F8.5/
     *       1X,'P&R   ACCESS STATION #4=',F8.5//
     *       1X,'K&R   ACCESS STATION #1=',F8.5/
     *       1X,'K&R   ACCESS STATION #2=',F8.5/
     *       1X,'K&R   ACCESS STATION #3=',F8.5/
     *       1X,'K&R   ACCESS STATION #4=',F8.5//
     *       1X,'WALK  ACCESS LOGSUM    =',F8.3/
     *       1X,'BUS   ACCESS LOGSUM    =',F8.3/
     *       1X,'P&R   ACCESS LOGSUM    =',F8.3/
     *       1X,'K&R   ACCESS LOGSUM    =',F8.3)
      END IF
C....................................................................
C
C  COMMUTER RAIL ACCESS UTILITIES & PROBABILITIES
C
C...WALK ACCESS
      IF(LSCWLK.NE.0.0) THEN
      UTIL(48)=LSUMSW*LSCWLK +
     *         KWCR(C)/(LSUMA*LSUMS)
      EUTIL(48)=EXP(UTIL(48))
      END IF
C...BUS ACCESS
      IF(LSCBUS.NE.0.0) THEN
      UTIL(49)=LSUMSB*LSCBUS  +
     *         KBCR(C)/(LSUMA*LSUMS)
      EUTIL(49)=EXP(UTIL(49))
      END IF
C...PARK&RIDE ACCESS
      IF(LSCPR.NE.0.0) THEN
      UTIL(50)=LSUMSP*LSCPR   +
     *         KPCR(C)/(LSUMA*LSUMS)
      EUTIL(50)=EXP(UTIL(50))
      END IF
C...KISS&RIDE ACCESS
      IF(LSCKR.NE.0.0) THEN
      UTIL(51)=LSUMSK*LSCKR   +
     *         KKCR(C)/(LSUMA*LSUMS)
      EUTIL(51)=EXP(UTIL(51))
      END IF
C...PROBABILITIES
      CWPROB(3)=0.0
      CBPROB(3)=0.0
      CPPROB(5)=0.0
      CKPROB(5)=0.0
      LSCR=0.0
      DENOM=EUTIL(48)+EUTIL(49)+EUTIL(50)+EUTIL(51)
      IF(DENOM.GT.0.0) THEN
      CWPROB(3)=EUTIL(48)/DENOM
      CBPROB(3)=EUTIL(49)/DENOM
      CPPROB(5)=EUTIL(50)/DENOM
      CKPROB(5)=1.0-CWPROB(3)-CBPROB(3)-CPPROB(5)
      CKPROB(5)=AMAX1(CKPROB(5),0.0)
      LSCR=DLOG(DENOM)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9009) (UTIL(K),EUTIL(K),K=48,51)
 9009 FORMAT(/1X,'SUMMARY OF COMMUTER RAIL ACCESS UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',11X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',8X,'----------',5X,
     *          '----------'/
     *       1X,'WALK ACCESS TO COMMUTER RAIL ',F10.5,3X,E12.5/
     *       1X,'BUS  ACCESS TO COMMUTER RAIL ',F10.5,3X,E12.5//
     *       1X,'P&R  ACCESS TO COMMUTER RAIL ',F10.5,3X,E12.5/
     *       1X,'K&R  ACCESS TO COMMUTER RAIL ',F10.5,3X,E12.5)
      WRITE(26,9010) CWPROB(3),CBPROB(3),CPPROB(5),CKPROB(5),
     *               LSCR
 9010 FORMAT(/1X,'SUMMARY OF COMMUTER RAIL ACCESS CHOICE PROB:'/
     *       1X,'---------------------------------------------'/
     *       1X,'WALK  ACCESS=',F8.5/ 
     *       1X,'BUS   ACCESS=',F8.5/
     *       1X,'P&R   ACCESS=',F8.5/
     *       1X,'K&R   ACCESS=',F8.5//
     *       1X,'COMMUTER RAIL LOGSUM=',F10.5)
      END IF
C
C  URBAN RAIL ACCESS UTILITIES & PROBABILITIES
C
C..WALK ACCESS - URBAN RAIL
      UWPROB(1)=1.0
      UWPROB(2)=0.0
      LSUWLK=0.0
      DENOM=EUTIL(13)+EUTIL(14)
      IF(DENOM.GT.0.0) THEN
      MODINC(9)=1
      LSUWLK=DLOG(DENOM)
      UWPROB(1)=EUTIL(13)/DENOM
      UWPROB(2)=1.0-UWPROB(1)
      END IF
C..BUS ACCESS - URBAN RAIL
      UBPROB(1)=1.0
      UBPROB(2)=0.0
      LSUBUS=0.0
      DENOM=EUTIL(15)+EUTIL(16)
      IF(DENOM.GT.0.0) THEN
      MODINC(10)=1
      LSUBUS=DLOG(DENOM)
      UBPROB(1)=EUTIL(15)/DENOM
      UBPROB(2)=1.0-UBPROB(1)
      END IF
C..P&R ACCESS - URBAN RAIL
      UPPROB(1)=1.0
      UPPROB(2)=0.0
      UPPROB(3)=0.0
      UPPROB(4)=0.0
      LSUPR=0.0
      DENOM=EUTIL(17)+EUTIL(18)+EUTIL(19)+EUTIL(20)
      IF(DENOM.GT.0.0) THEN
      MODINC(11)=1
      LSUPR=DLOG(DENOM)
      UPPROB(1)=EUTIL(17)/DENOM
      UPPROB(2)=EUTIL(18)/DENOM
      UPPROB(3)=EUTIL(19)/DENOM
      UPPROB(4)=1.0-UPPROB(1)-UPPROB(2)-UPPROB(3)
      END IF
C..K&R ACCESS - URBAN RAIL
      UKPROB(1)=1.0
      UKPROB(2)=0.0
      UKPROB(3)=0.0
      UKPROB(4)=0.0
      LSUKR=0.0
      DENOM=EUTIL(21)+EUTIL(22)+EUTIL(23)+EUTIL(24)
      IF(DENOM.GT.0.0) THEN
      MODINC(12)=1
      LSUKR=DLOG(DENOM)
      UKPROB(1)=EUTIL(21)/DENOM
      UKPROB(2)=EUTIL(22)/DENOM
      UKPROB(3)=EUTIL(23)/DENOM
      UKPROB(4)=1.0-UKPROB(1)-UKPROB(2)-UKPROB(3)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9108) UWPROB(1),UWPROB(2),UBPROB(1),UBPROB(2),
     *               UPPROB(1),UPPROB(2),UPPROB(3),UPPROB(4),
     *               UKPROB(1),UKPROB(2),UKPROB(3),UKPROB(4),
     *               LSUWLK,LSUBUS,LSUPR,LSUKR
 9108 FORMAT(/1X,'SUMMARY OF URBAN RAIL STATION CHOICE PROB: '/
     *       1X,'---------------------------------------------'/
     *       1X,'WALK  ACCESS STATION #1=',F8.5/ 
     *       1X,'WALK  ACCESS STATION #2=',F8.5//
     *       1X,'BUS   ACCESS STATION #1=',F8.5/
     *       1X,'BUS   ACCESS STATION #2=',F8.5//
     *       1X,'P&R   ACCESS STATION #1=',F8.5/
     *       1X,'P&R   ACCESS STATION #2=',F8.5/
     *       1X,'P&R   ACCESS STATION #3=',F8.5/
     *       1X,'P&R   ACCESS STATION #4=',F8.5//
     *       1X,'K&R   ACCESS STATION #1=',F8.5/
     *       1X,'K&R   ACCESS STATION #2=',F8.5/
     *       1X,'K&R   ACCESS STATION #3=',F8.5/
     *       1X,'K&R   ACCESS STATION #4=',F8.5//
     *       1X,'WALK  ACCESS LOGSUM    =',F10.5/
     *       1X,'BUS   ACCESS LOGSUM    =',F10.5/
     *       1X,'P&R   ACCESS LOGSUM    =',F10.5/
     *       1X,'K&R   ACCESS LOGSUM    =',F10.5)
      END IF
C....................................................................
C
C  URBAN RAIL ACCESS UTILITIES & PROBABILITIES
C
C...WALK ACCESS
      IF(LSUWLK.NE.0.0) THEN
      UTIL(52)=LSUMSUW*LSUWLK  +
     *         KWUR(C)/(LSUMA*LSUMS)
      EUTIL(52)=EXP(UTIL(52))
      END IF
C...BUS ACCESS
      IF(LSUBUS.NE.0.0) THEN
      UTIL(53)=LSUMSUB*LSUBUS  +
     *         KBUR(C)/(LSUMA*LSUMS)
      EUTIL(53)=EXP(UTIL(53))
      END IF
C...PARK&RIDE ACCESS
      IF(LSUPR.NE.0.0) THEN
      UTIL(54)=LSUMSUP*LSUPR   +
     *         KPUR(C)/(LSUMA*LSUMS)
      EUTIL(54)=EXP(UTIL(54))
      END IF
C...KISS&RIDE ACCESS
      IF(LSUKR.NE.0.0) THEN
      UTIL(55)=LSUMSUK*LSUKR   +
     *         KKUR(C)/(LSUMA*LSUMS)
      EUTIL(55)=EXP(UTIL(55))
      END IF
C...PROBABILITIES
      UWPROB(3)=0.0
      UBPROB(3)=0.0
      UPPROB(5)=0.0
      UKPROB(5)=0.0
      LSURB=0.0
      DENOM=EUTIL(52)+EUTIL(53)+EUTIL(54)+EUTIL(55)
      IF(DENOM.GT.0.0) THEN
      UWPROB(3)=EUTIL(52)/DENOM
      UBPROB(3)=EUTIL(53)/DENOM
      UPPROB(5)=EUTIL(54)/DENOM
      UKPROB(5)=1.0-UWPROB(3)-UBPROB(3)-UPPROB(5)
      UKPROB(5)=AMAX1(UKPROB(5),0.0)
      LSURB=DLOG(DENOM)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9109) (UTIL(K),EUTIL(K),K=52,55)
 9109 FORMAT(/1X,'SUMMARY OF URBAN RAIL ACCESS UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',6X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',5X,'----------',5X,
     *          '----------'/
     *       1X,'WALK ACCESS TO URBAN RAIL ',F10.5,3X,E12.5/
     *       1X,'BUS  ACCESS TO URBAN RAIL ',F10.5,3X,E12.5//
     *       1X,'P&R  ACCESS TO URBAN RAIL ',F10.5,3X,E12.5/
     *       1X,'K&R  ACCESS TO URBAN RAIL ',F10.5,3X,E12.5)
      WRITE(26,9110) UWPROB(3),UBPROB(3),UPPROB(5),UKPROB(5),
     *               LSURB
 9110 FORMAT(/1X,'SUMMARY OF URBAN RAIL ACCESS CHOICE PROB:'/
     *       1X,'---------------------------------------------'/
     *       1X,'WALK  ACCESS=',F8.5/ 
     *       1X,'BUS   ACCESS=',F8.5/
     *       1X,'P&R   ACCESS=',F8.5/
     *       1X,'K&R   ACCESS=',F8.5//
     *       1X,'URBAN RAIL LOGSUM=',F10.5)
      END IF
C
C
C...............................................................
C...............................................................
C
C  LOCAL & EXPRESS BUS ACCESS UTILITIES AND PROBABILITIES
C
C..LOCAL BUS
      BUSPROB(1)=0.0
      BUSPROB(2)=0.0
      LSLOC=0.0
      DENOM=EUTIL(44)+EUTIL(45)
      IF(DENOM.GT.0.0) THEN
      LSLOC=DLOG(DENOM)
      BUSPROB(1)=EUTIL(44)/DENOM
      BUSPROB(2)=1.0-BUSPROB(1)
      END IF
C..RAPID BUS
      RPDPROB(1)=0.0
      RPDPROB(2)=0.0
      LSRPD=0.0
      DENOM=EUTIL(72)+EUTIL(73)
      IF(DENOM.GT.0.0) THEN
      LSRPD=DLOG(DENOM)
      RPDPROB(1)=EUTIL(72)/DENOM
      RPDPROB(2)=1.0-RPDPROB(1)
      END IF      
C..EXPRESS BUS
      EXPPROB(1)=0.0
      EXPPROB(2)=0.0
      LSEXP=0.0
      DENOM=EUTIL(46)+EUTIL(47)
      IF(DENOM.GT.0.0) THEN
      LSEXP=DLOG(DENOM)
      EXPPROB(1)=EUTIL(46)/DENOM
      EXPPROB(2)=1.0-EXPPROB(1)
      END IF
C..TRANSITWAY BUS
      BWYPROB(1)=0.0
      BWYPROB(2)=0.0
      LSWAY=0.0
      DENOM=EUTIL(56)+EUTIL(57)
      IF(DENOM.GT.0.0) THEN
      LSWAY=DLOG(DENOM)
      BWPROB(1)=EUTIL(56)/DENOM
      BWPROB(2)=1.0-BWPROB(1)
      END IF
C..BUS RAPID TRANSIT
      BRTPROB(1)=0.0
      BRTPROB(2)=0.0
      LSBRT=0.0
      DENOM=EUTIL(78)+EUTIL(79)
      IF(DENOM.GT.0.0) THEN
      LSBRT=DLOG(DENOM)
      BRTPROB(1)=EUTIL(78)/DENOM
      BRTPROB(2)=1.0-BRTPROB(1)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9011) BUSPROB(1),BUSPROB(2),LSLOC,
     *               RPDPROB(1),RPDPROB(2),LSRPD,
     *               EXPPROB(1),EXPPROB(2),
     *               LSEXP,BWPROB(1),BWPROB(2),LSWAY,
     *               BRTPROB(1),BRTPROB(2),LSBRT
 9011 FORMAT(/1X,'SUMMARY OF LOCAL & EXPRESS BUS CHOICE PROBABILITIES'/
     *       1X,'---------------------------------------------'/
     *       1X,'WALK  ACCESS --> LOCAL       =',F8.5/
     *       1X,'DRIVE ACCESS --> LOCAL       =',F8.5/
     *       1X,'LOCAL BUS LOGSUM             =',F8.2//
     *       1X,'WALK  ACCESS --> RAPID       =',F8.5/
     *       1X,'DRIVE ACCESS --> RAPID       =',F8.5/
     *       1X,'RAPID BUS LOGSUM             =',F8.2//
     *       1X,'WALK  ACCESS --> EXPRESS     =',F8.5/
     *       1X,'DRIVE ACCESS --> EXPRESS     =',F8.5/
     *       1X,'EXPRESS BUS LOGSUM           =',F8.2//
     *       1X,'WALK  ACCESS --> TRANSITWAY  =',F8.5/
     *       1X,'DRIVE ACCESS --> TRANSITWAY  =',F8.5/
     *       1X,'TRANSITWAY BUS LOGSUM        =',F8.2//
     *       1X,'WALK  ACCESS --> BRT         =',F8.5/
     *       1X,'DRIVE ACCESS --> BRT         =',F8.5/
     *       1X,'BUS RAPID      LOGSUM        =',F8.2)
	  END IF
C....................................................................
C
C  SHARED RIDE UTILITIES AND PROBABILITIES
C
      ICO=IFIX(ZHHD(6,JZ))
      ICO=MAX0(ICO,1)
C 
      IF(LS2PER.NE.0.0) THEN
	    UTIL(66)=LSUMT*LS2PER + K2P(C,ICO)/(LSUMA*LSUMS)
      EUTIL(66)=EXP(UTIL(66))
	    ENDIF
C
      IF(LS3PER.NE.0.0) THEN
	    UTIL(67)=LSUMT*LS3PER + K3P(C,ICO)/(LSUMA*LSUMS)
      EUTIL(67)=EXP(UTIL(67))
	    ENDIF
C
      IF(LS4PER.NE.0.0) THEN
	    UTIL(83)=LSUMT*LS4PER + K4P(C,ICO)/(LSUMA*LSUMS)
      EUTIL(83)=EXP(UTIL(83))
	    ENDIF
C
      SRPROB(1)=0.0
      SRPROB(2)=0.0
      SRPROB(3)=0.0
      LSSHR=0.0
C
      DENOM=EUTIL(66)+EUTIL(67)+EUTIL(83)
C
      IF(DENOM.GT.0.0) THEN
      LSSHR=DLOG(DENOM)
      SRPROB(1)=EUTIL(66)/DENOM
      SRPROB(3)=EUTIL(83)/DENOM
      SRPROB(2)=1.0-SRPROB(1)-SRPROB(3)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9012) C,ICO,LS2PER,LS3PER,LS4PER,
     *               K2P(C,ICO),K3P(C,ICO),K4P(C,ICO),
     *               UTIL(66),UTIL(67),UTIL(83),
     *               SRPROB(1),SRPROB(2),SRPROB(3),LSSHR
 9012 FORMAT(/1X,'SUMMARY OF SHARED RIDE CHOICE PROBABILITIES'/
     *       1X,'---------------------------------------------'/
     *       1X,'INCOME GROUP     =',I9/
     *       1X,'COUNTY OF ORIGIN =',I9/
     *       1X,'2  PERSON LOGSUM =',F9.3/
     *       1X,'3+ PERSON LOGSUM =',F9.3/
     *       1X,'4+ PERSON LOGSUM =',F9.3/
     *       1X,'K2P      CONSTANT=',F9.4/
     *       1X,'K3P      CONSTANT=',F9.4/
     *       1X,'K4P      CONSTANT=',F9.4/
     *       1X,'2  PERSON UTILITY=',F9.3/
     *       1X,'3+ PERSON UTILITY=',F9.3/
     *       1X,'4+ PERSON UTILITY=',F9.3/
     *       1X,'2  PERSON AUTO   =',F9.4/ 
     *       1X,'3+ PERSON AUTO   =',F9.4/
     *       1X,'4+ PERSON AUTO   =',F9.4//
     *       1X,'SHARED RIDE LOGSUM=',F10.5)
	  END IF
C....................................................................
C
C  CALCULATE PRIMARY TRANSIT MODE PROBABILITIES
C
C  
C  SET CBD CONSTANT, IF APPLICABLE
C
      KCCBD(1)=0
      KCCBD(2)=0
      KCCBD(3)=0
      KCCBD(4)=0
      KCCBD(5)=0
      KCCBD(6)=0
      KCCBD(7)=0
      IF(HBW) THEN
      IF(DIZ.EQ.7.AND.DJZ.EQ.7) KCCBD(1)=-0.65
      IF(DIZ.EQ.1.AND.DJZ.EQ.8) KCCBD(1)=0.80
      IF(DIZ.EQ.8.AND.DJZ.EQ.8) KCCBD(1)=-0.50
      END IF
      DO 320 E=1,50
      IF(JZ.EQ.CBDZ(E)) THEN
      KCCBD(1)=CCBD(1)
      IF(HBW) KCCBD(1)=CCBD(1)+1.25
      KCCBD(2)=CCBD(2)
      KCCBD(3)=CCBD(3)
      KCCBD(4)=CCBD(4)
      KCCBD(5)=CCBD(5)
      KCCBD(6)=CCBD(6)
      KCCBD(7)=CCBD(7)
      END IF
  320 CONTINUE
C..COMMUTER RAIL
      IF(LSCR.NE.0.0) THEN
      UTIL(60)=LSUMS*LSCR + KCR(C)/(LSUMA) + KCCBD(1)/LSUMA
     *        +CRDIST*((1.0/TAB2DA(JZ))**DISTEXP)
      IF((HBW).AND.(DIZ.EQ.1.OR.DIZ.EQ.2.OR.DIZ.EQ.7.OR.DIZ.EQ.8)) 
     * GO TO 1101
      UTIL(60)=UTIL(60) + (ZHHD(16,IZ) + ZHHD(17,JZ))/LSUMA
 1101 EUTIL(60)=EXP(UTIL(60))
      END IF
C
C..URBAN RAIL
      IF(LSURB.NE.0.0) THEN
      UTIL(61)=LSUMS*LSURB + KUR(C)/(LSUMA)+ KCCBD(2)/LSUMA +
     *                       KLAX/LSUMA 
     *                     + (ZHHD(16,IZ) + ZHHD(17,JZ))/LSUMA
      ALOGSUM(C,JZ)=ALOGSUM(C,JZ)+(MWALK(M)*UTIL(61)*LSUMA)/COEFF(100)
      EUTIL(61)=EXP(UTIL(61))
      END IF
C..TRANSITWAY
      IF(LSWAY.NE.0.0) THEN
      UTIL(62)=LSUMTW*LSWAY + KWAY(C)/(LSUMA)+ KCCBD(3)/LSUMA
     *                     + (ZHHD(16,IZ) + ZHHD(17,JZ))/LSUMA
      EUTIL(62)=EXP(UTIL(62))
      END IF
C..EXPRESS BUS
      IF(LSEXP.NE.0.0) THEN
      UTIL(63)=LSUMS*LSEXP + KEBUS(C)/(LSUMA)+ KCCBD(4)/LSUMA
     *                     + (ZHHD(16,IZ) + ZHHD(17,JZ))/LSUMA
      EUTIL(63)=EXP(UTIL(63))
      END IF
C..RAPID BUS
      IF(LSRPD.NE.0.0) THEN
      UTIL(74)=LSUMRB*LSRPD + KRBUS(C)/(LSUMA)+ KCCBD(5)/LSUMA
     *                     + (ZHHD(16,IZ) + ZHHD(17,JZ))/LSUMA
      EUTIL(74)=EXP(UTIL(74))
      END IF
C..LOCAL BUS
      IF(LSLOC.NE.0.0) THEN
      UTIL(64)=LSUMS*LSLOC + KLBUS(C)/(LSUMA)+ KCCBD(6)/LSUMA
     *                     + (ZHHD(16,IZ) + ZHHD(17,JZ))/LSUMA
      EUTIL(64)=EXP(UTIL(64))
      END IF
C..BUS RAPID TRANSIT (BRT)
      IF(LSBRT.NE.0.0) THEN
      UTIL(80)=LSUMBR*LSBRT + KBRT(C)/(LSUMA)+ KCCBD(7)/LSUMA
     *                     + (ZHHD(16,IZ) + ZHHD(17,JZ))/LSUMA
      EUTIL(80)=EXP(UTIL(80))
      END IF
C..TRANSIT PROBABILITIES
      TRNPROB(1)=0.0
      TRNPROB(2)=0.0
      TRNPROB(3)=0.0
      TRNPROB(4)=0.0
      TRNPROB(5)=0.0
      TRNPROB(6)=0.0
      TRNPROB(7)=0.0
      LSTRN=0.0
      DENOM=EUTIL(60)+EUTIL(61)+EUTIL(62)+EUTIL(63)+EUTIL(64)+
     *      EUTIL(74)+EUTIL(80)
      IF(DENOM.GT.0.0) THEN
      LSTRN=DLOG(DENOM)
      TRNPROB(1)=EUTIL(60)/DENOM
      TRNPROB(2)=EUTIL(61)/DENOM
      TRNPROB(3)=EUTIL(62)/DENOM
      TRNPROB(4)=EUTIL(63)/DENOM
      TRNPROB(6)=EUTIL(74)/DENOM
      TRNPROB(7)=EUTIL(80)/DENOM
      TRNPROB(5)=EUTIL(64)/DENOM
       IF(TRNPROB(5).GT.0.0) THEN
       TRNPROB(5)=1.0-TRNPROB(1)-TRNPROB(2)-TRNPROB(3)-
     *               TRNPROB(4)-TRNPROB(6)-TRNPROB(7)
       ELSE
       TRNPROB(5)=0.0
       END IF
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9013) (UTIL(K),EUTIL(K),K=60,64),UTIL(74),EUTIL(74),
     *               UTIL(80),EUTIL(80)
 9013 FORMAT(/1X,'SUMMARY OF PRIMARY TRANSIT UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',6X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',3X,'----------',5X,
     *          '----------'/
     *       1X,'COMMUTER RAIL           ',F10.5,3X,E12.5/
     *       1X,'URBAN RAIL              ',F10.5,3X,E12.5/
     *       1X,'TRANSITWAY BUS          ',F10.5,3X,E12.5/
     *       1X,'EXPRESS BUS             ',F10.5,3X,E12.5/
     *       1X,'LOCAL BUS               ',F10.5,3X,E12.5/
     *       1X,'RAPID BUS               ',F10.5,3X,E12.5/
     *       1X,'BUS RAPID TRANSIT       ',F10.5,3X,E12.5/)
      WRITE(26,9014) TRNPROB(1),TRNPROB(2),TRNPROB(3),
     *               TRNPROB(4),TRNPROB(5),TRNPROB(6),
     *               TRNPROB(7),LSTRN
 9014 FORMAT(/1X,'SUMMARY OF TRANSIT CHOICE PROBABILITIES'/
     *       1X,'---------------------------------------------'/
     *       1X,'COMMUTER RAIL  =',F8.5/
     *       1X,'URBAN RAIL     =',F8.5/
     *       1X,'TRANSITWAY BUS =',F8.5/
     *       1X,'EXPRESS BUS    =',F8.5/
     *       1X,'LOCAL BUS      =',F8.5/
     *       1X,'RAPID BUS      =',F8.5/
     *       1X,'BUS RAPID (BRT)=',F8.5/
     *       1X,'LOGSUM         =',F10.5/)
      END IF       
C
C  DRIVE ALONE & SHARED RIDE PROBABILITIES
C
C ..DRIVE-ALONE UTILITY
      IF(LSDA.NE.0.0) THEN
	    UTIL(65)=LSUMS*LSUMT*LSDA + KDA(C,ICO)/LSUMA
      EUTIL(65)=EXP(UTIL(65))
	    ENDIF
C..SHARED RIDE UTILITY
      IF(LSSHR.NE.0.0) THEN
	    UTIL(68)=LSUMS*LSSHR + KSR(C,ICO)/LSUMA
      EUTIL(68)=EXP(UTIL(68))
	    ENDIF
C..AUTO SUBMODE PROBABILITIES
      ATPROB(1)=0.0
      ATPROB(2)=0.0
      LSAUTO=0.0
      DENOM=EUTIL(65)+EUTIL(68)
      IF(DENOM.GT.0.0) THEN
      LSAUTO=DLOG(DENOM)
      ATPROB(1)=EUTIL(65)/DENOM
      ATPROB(2)=1.0-ATPROB(1)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9015) UTIL(65),EUTIL(65),UTIL(68),EUTIL(68)
 9015 FORMAT(/1X,'SUMMARY OF AUTO SUBMODE UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',6X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',3X,'----------',5X,
     *          '----------'/
     *       1X,'DRIVE ALONE             ',F10.5,3X,F12.5,/
     *       1X,'SHARED RIDE             ',F10.5,3X,F12.5) 
      WRITE(26,9416) ATPROB(1),ATPROB(2),LSAUTO
 9416 FORMAT(/1X,'SUMMARY OF AUTO SUBMODE CHOICE PROBABILITIES'/
     *       1X,'---------------------------------------------'/
     *       1X,'DRIVE ALONE   =',F8.5/ 
     *       1X,'SHARED RIDE   =',F8.5//
     *       1X,'AUTO LOGSUM   =',F10.5)
      END IF
C....................................................................
C
C  CALCULATE MOTORIZED UTILITIES AND PROBABILITIES
C
C..AUTO
      IF(LSAUTO.NE.0.0) THEN
      UTIL(69)=LSUMA*LSAUTO + KAUT(C,ICO)
      EUTIL(69)=EXP(UTIL(69))
      ELSE
      AUTEXP=0.0
      ENDIF
C..TRANSIT
      IF(LSTRN.NE.0.0) THEN
      UTIL(70)=LSUMA*LSTRN + KTRN(C) + KTRNT(PINDEX,C) + TRNPROD(DIZ,C)
      UTIL(70)=UTIL(70) 
      EUTIL(70)=EXP(UTIL(70))
      TLOGSUM(C,JZ)=TLOGSUM(C,JZ)+(MWALK(M)*UTIL(70))/COEFF(100)
      END IF
C..NON-MOTORIZED
      IF(NMOT.AND.(LSNMOT.NE.0.0)) THEN
      UTIL(77)=LSUMA*LSNMOT+KNMOT(C)
      EUTIL(77)=EXP(UTIL(77))
      END IF
C..NON-TRANSIT EXPOENTIATED UTILITY FOR FTA USERBEN FILE
      AUTEXP=EUTIL(69)+EUTIL(77)
C..PROBABILITIES
      MOTOR(1)=0.0
      MOTOR(2)=0.0
      MOTOR(3)=0.0
      LSMOT=0.0
      DENOM=EUTIL(69)+EUTIL(70)+EUTIL(77)
      IF(DENOM.GT.0.0) THEN
      LSMOT=DLOG(DENOM)
      MOTOR(1)=EUTIL(69)/DENOM
      MOTOR(3)=EUTIL(77)/DENOM
      MOTOR(2)=1.0-MOTOR(1)-MOTOR(3)
      MOTOR(2)=AMAX1(MOTOR(2),0.0)
      ULOGSUM(C,JZ)=ULOGSUM(C,JZ)+(MWALK(M)*LSMOT)/COEFF(100)
      END IF
C
C  COMPUTE FTA RELATED USER BENEFIT INFORMATION
C
      IF((M.LE.4).AND.(TWALK(1).GT.0.0)) THEN
          TSHAR(1)=TSHAR(1)+(MWALK(M)/TWALK(1))*MOTOR(2)
          CRPCT(JZ,C,1)=CRPCT(JZ,C,1)+(MWALK(M)/TWALK(1))*TRNPROB(1)
      END IF
      IF((M.EQ.5.OR.M.EQ.6).AND.(TWALK(2).GT.0.0)) THEN
          TSHAR(2)=TSHAR(2)+(MWALK(M)/TWALK(2))*MOTOR(2)
          CRPCT(JZ,C,2)=CRPCT(JZ,C,2)+(MWALK(M)/TWALK(2))*TRNPROB(1)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9017) UTIL(69),EUTIL(69),
     *               UTIL(70),EUTIL(70),ZHHD(16,JZ),
     *               UTIL(77),EUTIL(77)
 9017 FORMAT(/1X,'SUMMARY OF BASIC UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',6X,'UTIL',10X,'EUTIL',
     *                                  6X,'DENSITY CONSTANT'/
     *       1X,'                     ',3X,'----------',5X,
     *          '----------',3X,'----------------'/
     *       1X,'AUTO                    ',F10.5,3X,E13.6/
     *       1X,'TRANSIT                 ',F10.5,3X,E13.6,3X,F8.3/
     *       1X,'NON-MOTORIZED           ',F10.5,3X,E13.6/)
      WRITE(26,9018) MOTOR(1),MOTOR(2),MOTOR(3),LSMOT,C,
     *               ALOGSUM(C,JZ),TLOGSUM(C,JZ),ULOGSUM(C,JZ),
     *               TSHAR(1),TSHAR(2)
 9018 FORMAT(/1X,'SUMMARY OF BASIC CHOICE PROBABILITIES'/
     *       1X,'---------------------------------------------'/
     *       1X,'AUTO          =',F8.5/ 
     *       1X,'TRANSIT       =',F8.5/
     *       1X,'NON-MOTORIZED =',F8.5//
     *       1X,'MOTORIZED LOGSUM=',F10.5//,
     *       1X,'SUMMARY OF WEIGHTED LOGSUM VALUES'/
     *       1X,'---------------------------------'/
     *       1X,'INCOME   GROUP=',I8/
     *       1X,'URB RL  LOGSUM=',E12.5/
     *       1X,'TRANSIT LOGSUM=',E12.5/
     *       1X,'TOTAL   LOGSUM=',E12.5//
     *       1X,'SUMMARY OF TRANSIT MODE SHARES  '/
     *       1X,'--------------------------------'/
     *       1X,' WALK ACCESS  =',F8.5/
     *       1X,'DRIVE ACCESS  =',F8.5/) 
      WRITE(26,9019) TSHAR(1),CRPCT(JZ,C,1),MWALK(M),TWALK(1),MOTOR(2),
     *               TRNPROB(1),TSHAR(2),CRPCT(JZ,C,2),TWALK(2)
 9019 FORMAT(/1X,'SUMMARY OF SUMMIT COMPUTATIONS'/
     *        1X,'------------------------------'/
     *        1X,'WALK/DRIVE TRANSIT SHARE=',F8.5/
     *        1X,'           CR      SHARE=',F8.5/
     *        1X,'           MARKET  SHARE=',F8.5/
     *        1X,'CURRENT    MARKET  SHARE=',F8.5/
     *        1X,'TRANSIT            PROB =',F8.5/
     *        1X,'COMMUTER RAIL      PROB =',F8.5/
     *        1X,'DRIVE ONLY TRANSIT SHARE=',F8.5/
     *        1X,'           CR      SHARE=',F8.5/
     *        1X,'           TOTAL   SHARE=',F8.5/)
      END IF
C....................................................................
C
C  COMPUTE TRIP MATRIX VALUES
C
      TTRAN=PERTRP*MOTOR(2)
      TNMOT=PERTRP*MOTOR(3)
      TAUTO=PERTRP-TTRAN-TNMOT
C
      TDRV0=TAUTO*ATPROB(1)
      TDSHR=TAUTO-TDRV0
c
      TCR=TTRAN*TRNPROB(1)
      TUR=TTRAN*TRNPROB(2)
      TWAY=TTRAN*TRNPROB(3)
      TEXP =TTRAN*TRNPROB(4)
      TRPD=TTRAN*TRNPROB(6)
      TBRT=TTRAN*TRNPROB(7)
      IF(TRNPROB(5).GT.0) THEN
      TLOC=TTRAN-(TCR+TUR+TWAY+TEXP+TRPD+TBRT)
      ELSE
      TLOC=0.0
      END IF
      IF(JZ.EQ.LAXZ) URLAX=URLAX+TUR
C
      TNMWK=TNMOT*NMPROB(1)
      TNMBK=TNMOT*NMPROB(2)
c
      TDRV2=TDSHR*SRPROB(1)
      TDRV4=TDSHR*SRPROB(3)
      TDRV3=TDSHR-TDRV2-TDRV4
C
      TLOCW=TLOC*BUSPROB(1)
      TLOCD=TLOC-TLOCW
C
      TRPDW=TRPD*RPDPROB(1)
      TRPDD=TRPD-TRPDW
C
      TEXPW=TEXP*EXPPROB(1)
      TEXPD=TEXP-TEXPW
C
      TBRTW=TBRT*BRTPROB(1)
      TBRTD=TBRT-TBRTW
C
      TCRW=TCR*CWPROB(3)
      TCRB=TCR*CBPROB(3)
      TCRP=TCR*CPPROB(5)
      TCRK=TCR-(TCRW+TCRB+TCRP)
      TCRK=DMAX1(TCRK,0.0)
C
      TURW=TUR*UWPROB(3)
      TURB=TUR*UBPROB(3)
      TURP=TUR*UPPROB(5)
      TURK=TUR-(TURW+TURB+TURP)
      TURK=DMAX1(TURK,0.0)
C
      TWAYW=TWAY*BWPROB(1)
      TWAYD=TWAY-TWAYW
C
      TCRW1=TCRW*CWPROB(1)
      TCRW2=TCRW-TCRW1
C
      TCRB1=TCRB*CBPROB(1)
      TCRB2=TCRB-TCRB1
C
      TCRP1=TCRP*CPPROB(1)
      TCRP2=TCRP*CPPROB(2)
      TCRP3=TCRP*CPPROB(3)
      TCRP4=TCRP-(TCRP1+TCRP2+TCRP3)
      TCRP4=DMAX1(TCRP4,0.0)
C
      TCRK1=TCRK*CKPROB(1)
      TCRK2=TCRK*CKPROB(2)
      TCRK3=TCRK*CKPROB(3)
      TCRK4=TCRK-(TCRK1+TCRK2+TCRK3)
      TCRK4=DMAX1(TCRK4,0.0)
C
      TURW1=TURW*UWPROB(1)
      TURW2=TURW-TURW1
C
      TURB1=TURB*UBPROB(1)
      TURB2=TURB-TURB1
C
      TURP1=TURP*UPPROB(1)
      TURP2=TURP*UPPROB(2)
      TURP3=TURP*UPPROB(3)
      TURP4=TURP-(TURP1+TURP2+TURP3)
      TURP4=DMAX1(TURP4,0.0)
C
      TURK1=TURK*UKPROB(1)
      TURK2=TURK*UKPROB(2)
      TURK3=TURK*UKPROB(3)
      TURK4=TURK-(TURK1+TURK2+TURK3)
      TURK4=DMAX1(TURK4,0.0)
C
C..DRIVE ALONE TOLL AND NON-TOLL
      TDRV0N=TDRV0*DAPROB(1)
      TDRV0T=TDRV0-TDRV0N
C
C DETERMINE HOV TRIP TABLES
C
      TDRV2NH=TDRV2*P2PROB(2)
      TDRV2NN=TDRV2*P2PROB(1)
      TDRV2TH=TDRV2*P2PROB(4)
      TDRV2TN=TDRV2*P2PROB(3)
      TDRV2N=TDRV2NH+TDRV2NN
      TDRV2T=TDRV2TH+TDRV2TN
C
      TDRV3NH=TDRV3*P3PROB(2)
      TDRV3NN=TDRV3*P3PROB(1)
      TDRV3TH=TDRV3*P3PROB(4)
      TDRV3TN=TDRV3*P3PROB(3)
      TDRV3N=TDRV3NH+TDRV3NN
      TDRV3T=TDRV3TH+TDRV3TN
C
      TDRV4NH=TDRV4*P4PROB(2)
      TDRV4NN=TDRV4*P4PROB(1)
      TDRV4TH=TDRV4*P4PROB(4)
      TDRV4TN=TDRV4*P4PROB(3)
      TDRV4N=TDRV4NH+TDRV4NN
      TDRV4T=TDRV4TH+TDRV4TN
C
C CREATE LEVEL-OF-SERVICE DATA FOR AIR PASSENGER MODEL ESTIMATION
C
      IF(AIRPASS) THEN
      IF(C.EQ.1.AND.M.EQ.1) THEN
C....LOCAL BUS
      CALL RESETV(VALUES)
      VALUES(1)=WAIT1(1,JZ)
      VALUES(2)=WAIT2(1,JZ)
      VALUES(3)=TXFER(1,JZ)
      VALUES(4)=WALKACC(1,JZ)
      VALUES(5)=0.0
      VALUES(6)=WALKEGR(1,JZ)
      VALUES(7)=WALKTFR(1,JZ)
      VALUES(8)=BIVT(1,JZ)
      VALUES(9)=0.0
      VALUES(10)=0.0
      VALUES(11)=0.0
      VALUES(12)=0.0
      VALUES(13)=FARE(1,JZ)
      VALUES(14)=0.0
      VALUES(15)=0.0
      WRITE(65,9309) IZ,JZ,BESTNAME(3),VALUES
 9309 FORMAT(I4,',',I4,',Walk,',A13,15(',',F10.2))
C....RAPID BUS
      CALL RESETV(VALUES)
      VALUES(1)=WAIT1(4,JZ)
      VALUES(2)=WAIT2(4,JZ)
      VALUES(3)=TXFER(4,JZ)
      VALUES(4)=WALKACC(4,JZ)
      VALUES(5)=0.0
      VALUES(6)=WALKEGR(4,JZ)
      VALUES(7)=WALKTFR(4,JZ)
      VALUES(8)=BIVT(4,JZ)
      VALUES(9)=0.0
      VALUES(10)=0.0
      VALUES(11)=0.0
      VALUES(12)=0.0
      VALUES(13)=FARE(4,JZ)
      VALUES(14)=RIVT(4,JZ)
      VALUES(15)=0.0
      WRITE(65,9309) IZ,JZ,BESTNAME(8),VALUES
C....EXPRESS BUS
      CALL RESETV(VALUES)
      VALUES(1)=WAIT1(2,JZ)
      VALUES(2)=WAIT2(2,JZ)
      VALUES(3)=TXFER(2,JZ)
      VALUES(4)=WALKACC(2,JZ)
      VALUES(5)=0.0
      VALUES(6)=WALKEGR(2,JZ)
      VALUES(7)=WALKTFR(2,JZ)
      VALUES(8)=BIVT(2,JZ)
      VALUES(9)=EIVT(2,JZ)
      VALUES(10)=0.0
      VALUES(11)=0.0
      VALUES(12)=0.0
      VALUES(13)=FARE(2,JZ)
      VALUES(14)=RIVT(2,JZ)
      VALUES(15)=0.0
      WRITE(65,9309) IZ,JZ,BESTNAME(4),VALUES
C....TRANSITWAY
      CALL RESETV(VALUES)
      VALUES(1)=WAIT1(3,JZ)
      VALUES(2)=WAIT2(3,JZ)
      VALUES(3)=TXFER(3,JZ)
      VALUES(4)=WALKACC(3,JZ)
      VALUES(5)=0.0
      VALUES(6)=WALKEGR(3,JZ)
      VALUES(7)=WALKTFR(3,JZ)
      VALUES(8)=BIVT(3,JZ)
      VALUES(9)=EIVT(3,JZ)
      VALUES(10)=WIVT(3,JZ)
      VALUES(11)=0.0
      VALUES(12)=0.0
      VALUES(13)=FARE(3,JZ)
      VALUES(14)=RIVT(3,JZ)
      VALUES(15)=0.0
      WRITE(65,9309) IZ,JZ,BESTNAME(5),VALUES 
C....URBAN RAIL
      CALL RESETV(VALUES)
       BESTURI=0
       EVENTSP=.TRUE.
 5559  IF(OSTA(2,3).GT.0.AND.OSTA(2,3).LT.MAX_ZONES) BESTURI=3
       IF(OSTA(2,1).GT.0.AND.OSTA(2,1).LT.MAX_ZONES) BESTURI=1
       IF((SPEVENT).AND.(EVENTSP).AND.(BESTURI.GT.1)) GO TO 5551
       IF((SPEVENT).AND.(.NOT.EVENTSP)) BESTURI=3
        IF(BESTURI.EQ.0) GO TO 5551
        IMODE=2
        CALL RAILPATH(JZ,OSTA(IMODE,BESTURI),
     *                ASTA(IMODE,BESTURI),IMODE,VALUES,
     *                STAZONE,CDSTABRT,STASTAD,STAZNED)
        IF(BESTURI.LE.2) 
     *  VALUES(4)=VALUES(4)+MWALKW(IMODE,BESTURI)
        IF(BESTURI.GE.3.AND.BESTURI.LE.4) THEN
        VALUES(2)=VALUES(1)+VALUES(2)+
     *        URBSKIM((BESTURI-2),4)
        VALUES(1)=URBSKIM((BESTURI-2),3) 
        VALUES(3)=VALUES(3)+BTXFER(IMODE,(BESTURI-2))+1
        VALUES(4)=VALUES(4)+MWALKB(IMODE,(BESTURI-2))
        END IF
        VALUES(5)=0.0
        IF(BESTURI.GE.3.AND.BESTURI.LE.4) THEN
        VALUES(7)=VALUES(7)+URBSKIM((BESTURI-2),5)
        VALUES(8)=VALUES(8)+URBSKIM((BESTURI-2),2)
        VALUES(9)=VALUES(9)+URBSKIM((BESTURI-2),10)
        VALUES(10)=VALUES(10)+URBSKIM((BESTURI-2),11)
        VALUES(14)=VALUES(14)+URBSKIM((BESTURI-2),9)
        VALUES(15)=VALUES(15)+URBSKIM((BESTURI-2),12)
        VALUES(13)=VALUES(13)+URBSKIM((BESTURI-2),6)
	      END IF
 5551   WRITE(65,9399) IZ,JZ,BESTNAME(2),VALUES,OSTA(IMODE,BESTURI),
     *                 ASTA(IMODE,BESTURI)  
 9399   FORMAT(I4,',',I4,',Walk,',A13,15(',',F10.2),2(',',I4))
        IF((SPEVENT).AND.(EVENTSP)) THEN
        EVENTSP=.FALSE.
        CALL RESETV(VALUES)
        GO TO 5559
        END IF
C....COMMUTER RAIL RAIL
       CALL RESETV(VALUES)
       BESTCRI=0
       IF(OSTA(1,3).GT.0.AND.OSTA(1,3).LT.MAX_ZONES) BESTCRI=3
       IF(OSTA(1,1).GT.0.AND.OSTA(1,1).LT.MAX_ZONES) BESTCRI=1
        IF(BESTCRI.EQ.0) GO TO 5552
        IMODE=1
        CALL RAILPATH(JZ,OSTA(IMODE,BESTCRI),
     *                ASTA(IMODE,BESTCRI),IMODE,VALUES,STAZONE,
     *                CDSTABRT,STASTAD,STAZNED)
        IF(BESTCRI.LE.2) 
     *  VALUES(4)=VALUES(4)+MWALKW(IMODE,BESTCRI)
        IF(BESTCRI.GE.3.AND.BESTCRI.LE.4) THEN
        VALUES(2)=VALUES(1)+VALUES(2)+
     *        CRBSKIM((BESTCRI-2),4)
        VALUES(1)=CRBSKIM((BESTCRI-2),3) 
        VALUES(3)=VALUES(3)+BTXFER(IMODE,(BESTCRI-2))+1
        VALUES(4)=VALUES(4)+MWALKB(IMODE,(BESTCRI-2))
        END IF
        VALUES(5)=0.0
        IF(BESTCRI.GE.3.AND.BESTCRI.LE.4) THEN
        VALUES(7)=VALUES(7)+CRBSKIM((BESTCRI-2),5)
        VALUES(8)=VALUES(8)+CRBSKIM((BESTCRI-2),2)
        VALUES(9)=VALUES(9)+CRBSKIM((BESTCRI-2),10)
        VALUES(10)=VALUES(10)+CRBSKIM((BESTCRI-2),11)
        VALUES(14)=VALUES(14)+CRBSKIM((BESTCRI-2),9)
        VALUES(15)=VALUES(15)+CRBSKIM((BESTCRI-2),12)
        VALUES(13)=VALUES(13)+CRBSKIM((BESTCRI-2),6)
	      END IF
 5552   WRITE(65,9399) IZ,JZ,BESTNAME(1),VALUES,OSTA(IMODE,BESTCRI),
     *                 ASTA(IMODE,BESTCRI)
C....BRT
      CALL RESETV(VALUES)
      VALUES(1)=WAIT1(5,JZ)
      VALUES(2)=WAIT2(5,JZ)
      VALUES(3)=TXFER(5,JZ)
      VALUES(4)=WALKACC(5,JZ)
      VALUES(5)=0.0
      VALUES(6)=WALKEGR(5,JZ)
      VALUES(7)=WALKTFR(5,JZ)
      VALUES(8)=BIVT(5,JZ)
      VALUES(9)=EIVT(5,JZ)
      VALUES(10)=0.0
      VALUES(11)=0.0
      VALUES(12)=0.0
      VALUES(13)=FARE(5,JZ)
      VALUES(14)=RIVT(5,JZ)
      VALUES(15)=WIVT(5,JZ)
      WRITE(65,9309) IZ,JZ,BESTNAME(7),VALUES 
C.....DRIVE TO EXPRESS BUS
      CALL RESETV(VALUES)
      IMODE=3
      CALL DRVBUS(JZ,CSTAE,CDSTAE,IMODE,VALUES)
      VALUES(5)=TAB1DA(CSTAE)
      VALUES(6)=DWALK+VALUES(6)
      WRITE(65,9388) IZ,JZ,BESTNAME(4),VALUES,CSTAE
C.....DRIVE TO TRANSITWAY BUS
      CALL RESETV(VALUES)
      IMODE=4
      CALL DRVBUS(JZ,CSTAT,CDSTAT,IMODE,VALUES)
      VALUES(5)=TAB1DA(CSTAT)
      VALUES(6)=TDWALK+VALUES(6)
      WRITE(65,9388) IZ,JZ,BESTNAME(5),VALUES,CSTAT
C....DRIVE TO URBAN RAIL
       CALL RESETV(VALUES)
       BESTURI=0
       DO 4001 K=5,12
       IF(STADATA((OSTA(2,K)-MAX_IZONES),7).EQ.0) THEN
       OSTA(2,K)=MAX_ZONES
       END IF
       IF(K.LT.9.AND.STADATA((OSTA(2,K)-MAX_IZONES),7).NE.1) THEN
       OSTA(2,K)=MAX_ZONES
       END IF
 4001  CONTINUE
       IF(OSTA(2,12).GT.0.AND.OSTA(2,12).LT.MAX_ZONES) BESTURI=12
       IF(OSTA(2,11).GT.0.AND.OSTA(2,11).LT.MAX_ZONES) BESTURI=11
       IF(OSTA(2,10).GT.0.AND.OSTA(2,10).LT.MAX_ZONES) BESTURI=10
       IF(OSTA(2,9).GT.0.AND.OSTA(2,9).LT.MAX_ZONES) BESTURI=9
       IF(OSTA(2,8).GT.0.AND.OSTA(2,8).LT.MAX_ZONES) BESTURI=8
       IF(OSTA(2,7).GT.0.AND.OSTA(2,7).LT.MAX_ZONES) BESTURI=7
       IF(OSTA(2,6).GT.0.AND.OSTA(2,6).LT.MAX_ZONES) BESTURI=6
       IF(OSTA(2,5).GT.0.AND.OSTA(2,5).LT.MAX_ZONES) BESTURI=5
        IF(BESTURI.EQ.0) GO TO 5553
        IMODE=2
        CALL RAILPATH(JZ,OSTA(IMODE,BESTURI),
     *                ASTA(IMODE,BESTURI),IMODE,VALUES,STAZONE,
     *                CDSTABRT,STASTAD,STAZNED)
        VALUES(5)=TAB1DA(OSTA(IMODE,BESTURI))
 5553   WRITE(65,9388) IZ,JZ,BESTNAME(2),VALUES,
     *                 OSTA(IMODE,BESTURI),ASTA(IMODE,BESTURI)
 9388   FORMAT(I4,',',I4,',Drive,',A13,15(',',F10.2),2(',',I4))
C....DRIVE TO COMMUTER RAIL
       CALL RESETV(VALUES)
       BESTCRI=0
       DO 4002 K=5,12
       IF(STADATA((OSTA(1,K)-MAX_IZONES),7).EQ.0) THEN
       OSTA(1,K)=MAX_ZONES
       END IF
       IF(K.LT.9.AND.STADATA((OSTA(1,K)-MAX_IZONES),7).NE.1) THEN
       OSTA(1,K)=MAX_ZONES
       END IF
 4002  CONTINUE
       IF(OSTA(1,12).GT.0.AND.OSTA(1,12).LT.MAX_ZONES) BESTCRI=12
       IF(OSTA(1,11).GT.0.AND.OSTA(1,11).LT.MAX_ZONES) BESTCRI=11
       IF(OSTA(1,10).GT.0.AND.OSTA(1,10).LT.MAX_ZONES) BESTCRI=10
       IF(OSTA(1,9).GT.0.AND.OSTA(1,9).LT.MAX_ZONES) BESTCRI=9
       IF(OSTA(1,8).GT.0.AND.OSTA(1,8).LT.MAX_ZONES) BESTCRI=8
       IF(OSTA(1,7).GT.0.AND.OSTA(1,7).LT.MAX_ZONES) BESTCRI=7
       IF(OSTA(1,6).GT.0.AND.OSTA(1,6).LT.MAX_ZONES) BESTCRI=6
       IF(OSTA(1,5).GT.0.AND.OSTA(1,5).LT.MAX_ZONES) BESTCRI=5
        IF(BESTCRI.EQ.0) GO TO 5554
        IMODE=1
        CALL RAILPATH(JZ,OSTA(IMODE,BESTCRI),
     *                ASTA(IMODE,BESTCRI),IMODE,VALUES,STAZONE,
     *                CDSTABRT,STASTAD,STAZNED)
        VALUES(5)=TAB1DA(OSTA(IMODE,BESTCRI))
 5554   WRITE(65,9388) IZ,JZ,BESTNAME(1),VALUES,OSTA(IMODE,BESTCRI),
     *                 ASTA(IMODE,BESTCRI)
C.....DRIVE TO BUS RAPID TRANSIT
      CALL RESETV(VALUES)
      CALL RESETV(VALUES2)
      CALL RESETV(VALUES3)
      IMODE=5
      IF(BRTUR) THEN
      CALL RAILPATH(JZ,CSTABRT,CBDSTA,IMODE,VALUES2,STAZONE,
     *              CDSTABRT,STASTAD,STAZNED)
      IMODE=2
      CALL RAILPATH(JZ,CBDSTA,CBSTAE,IMODE,VALUES3,STAZONE,
     *              CDSTABRT,STASTAD,STAZNED)
      VALUES(1)=VALUES2(1)
      VALUES(2)=VALUES2(2)+VALUES3(1)+VALUES3(2)
      VALUES(3)=VALUES2(3)+VALUES3(3)+1
      VALUES(4)=VALUES2(4)
      VALUES(6)=VALUES3(6)
      VALUES(7)=VALUES3(7)+VALUES2(6)
      VALUES(8)=VALUES2(8)+VALUES3(8)
      VALUES(9)=VALUES2(9)+VALUES3(9)
      VALUES(10)=VALUES2(10)+VALUES3(10)
      VALUES(11)=VALUES2(11)+VALUES3(11)
      VALUES(12)=VALUES2(12)+VALUES3(12)
      VALUES(13)=VALUES2(13)+VALUES3(13)
      VALUES(14)=VALUES2(14)+VALUES3(14)
      VALUES(15)=VALUES2(15)+VALUES3(15)
      ELSE
      CALL DRVBUS(JZ,CSTABRT,CDSTABRT,IMODE,VALUES)
      VALUES(6)=TDBRTWALK+VALUES(6)
      END IF
      IF(CSTABRT.LE.0) CSTABRT=MAX_ZONES 
      VALUES(5)=TAB1DA(CSTABRT)
      WRITE(65,9388) IZ,JZ,BESTNAME(7),VALUES,CSTABRT
      END IF
      END IF
C.................................................................... 
C
C  BEST WALK AND DRIVE PATH ANALYSIS
C
C
      IF(BESTPATH.AND.(IZ.NE.JZ)) THEN
       WLKIND=0
       IF(MWALK(1).GT.0) THEN
       WLKIND=1
       ELSE
        IF(MWALK(2).GT.0) WLKIND=2
       END IF
       IF(WLKIND.EQ.0.AND.MWALK(3).GT.0) THEN
       WLKIND=3
       ELSE
        IF(WLKIND.EQ.0.AND.MWALK(4).GT.0) WLKIND=4
       END IF
       DRVIND=0
       IF(MWALK(5).GT.0) THEN
       DRVIND=5
       ELSE
       IF(DRVIND.EQ.0.AND.MWALK(6).GT.0) DRVIND=6
       END IF
       IF(DRVIND.EQ.0.AND.WLKIND.GT.0) DRVIND=WLKIND
C
C..FIND BEST WALK PATH
C
      WKBEST=DMAX1(TCRW1,TCRW2,TCRB1,TCRB2,TURW1,TURW2,TURB1,TURB2,
     *             TEXPW,TWAYW,TRPDW,TBRTW)
       BESTMODE(1)=6
      IF(M.EQ.WLKIND.AND.WKBEST.GT.0.0) THEN
C..    IF LOCAL BUS
       IF(TLOCW.EQ.WKBEST) THEN
        WKPATH(1,JZ)=IFIX(WAIT1(1,JZ)*100.0)
        WKPATH(2,JZ)=IFIX(WAIT2(1,JZ)*100.0)
        WKPATH(3,JZ)=TXFER(1,JZ)
        WKPATH(4,JZ)=IFIX(WALKACC(1,JZ)*100.0)
        WKPATH(6,JZ)=IFIX(WALKEGR(1,JZ)*100.0)
        WKPATH(7,JZ)=IFIX(WALKTFR(1,JZ)*100.0)
        WKPATH(8,JZ)=IFIX(BIVT(1,JZ)*100.0)
        WKPATH(13,JZ)=FARE(1,JZ)
        BESTMODE(1)=3
        BESTZONE(3,1)=IZ
        BESTZONE(3,2)=JZ
       END IF
C..    IF RAPID BUS
       IF(TRPDW.EQ.WKBEST) THEN
        WKPATH(1,JZ)=IFIX(WAIT1(4,JZ)*100.0)
        WKPATH(2,JZ)=IFIX(WAIT2(4,JZ)*100.0)
        WKPATH(3,JZ)=TXFER(4,JZ)
        WKPATH(4,JZ)=IFIX(WALKACC(4,JZ)*100.0)
        WKPATH(6,JZ)=IFIX(WALKEGR(4,JZ)*100.0)
        WKPATH(7,JZ)=IFIX(WALKTFR(4,JZ)*100.0)
        WKPATH(8,JZ)=IFIX(BIVT(4,JZ)*100.0)
        WKPATH(13,JZ)=FARE(4,JZ)
        WKPATH(14,JZ)=IFIX(RIVT(4,JZ)*100.0)
        BESTMODE(1)=8
        BESTZONE(8,1)=IZ
        BESTZONE(8,2)=JZ
       END IF
C..    IF EXPRESS BUS
       IF(TEXPW.EQ.WKBEST) THEN
        WKPATH(1,JZ)=IFIX(WAIT1(2,JZ)*100.0)
        WKPATH(2,JZ)=IFIX(WAIT2(2,JZ)*100.0)
        WKPATH(3,JZ)=TXFER(2,JZ)
        WKPATH(4,JZ)=IFIX(WALKACC(2,JZ)*100.0)
        WKPATH(6,JZ)=IFIX(WALKEGR(2,JZ)*100.0)
        WKPATH(7,JZ)=IFIX(WALKTFR(2,JZ)*100.0)
        WKPATH(8,JZ)=IFIX(BIVT(2,JZ)*100.0)
        WKPATH(9,JZ)=IFIX(EIVT(2,JZ)*100.0)
        WKPATH(13,JZ)=FARE(2,JZ)
        WKPATH(14,JZ)=IFIX(RIVT(2,JZ)*100.0)
        BESTMODE(1)=4
        BESTZONE(4,1)=IZ
        BESTZONE(4,2)=JZ
       END IF
C..    IF TRANSITWAY BUS
       IF(TWAYW.EQ.WKBEST) THEN
        WKPATH(1,JZ)=IFIX(WAIT1(3,JZ)*100.0)
        WKPATH(2,JZ)=IFIX(WAIT2(3,JZ)*100.0)
        WKPATH(3,JZ)=TXFER(3,JZ)
        WKPATH(4,JZ)=IFIX(WALKACC(3,JZ)*100.0)
        WKPATH(6,JZ)=IFIX(WALKEGR(3,JZ)*100.0)
        WKPATH(7,JZ)=IFIX(WALKTFR(3,JZ)*100.0)
        WKPATH(8,JZ)=IFIX(BIVT(3,JZ)*100.0)
        WKPATH(9,JZ)=IFIX(EIVT(3,JZ)*100.0)
        WKPATH(10,JZ)=IFIX(WIVT(3,JZ)*100.0)
        WKPATH(13,JZ)=FARE(3,JZ)
        WKPATH(14,JZ)=IFIX(RIVT(3,JZ)*100.0)
        BESTMODE(1)=5
        BESTZONE(5,1)=IZ
        BESTZONE(5,2)=JZ
       END IF   
C..    IF URBAN RAIL
       IF(TURW1.EQ.WKBEST.OR.TURW2.EQ.WKBEST.OR.
     *    TURB1.EQ.WKBEST.OR.TURB2.EQ.WKBEST) THEN
        BESTUR=0.0
        BESTURI=0
        BESTMODE(1)=2
        BESTZONE(2,1)=IZ
        BESTZONE(2,2)=JZ
        DO 700 K=13,16
        IF(EUTIL(K).GT.BESTUR) THEN
        BESTUR=EUTIL(K)
        BESTURI=K
        END IF
  700   CONTINUE
        IF(BESTUR.EQ.0.0.OR.BESTURI.EQ.0) THEN
        WRITE(26,701) IZ,JZ
  701   FORMAT(2X,'NO URBAN RAIL BEST WALK PATH FOR IZ=',I4,' JZ=',I4)
C        STOP 16
        END IF
        IMODE=2
        BESTURI=BESTURI-12
        CALL RESETV(VALUES)
        CALL RAILPATH(JZ,OSTA(IMODE,BESTURI),
     *                ASTA(IMODE,BESTURI),IMODE,VALUES,STAZONE,
     *                CDSTABRT,STASTAD,STAZNED)
        WKPATH(1,JZ)=IFIX(VALUES(1)*100.0)
        WKPATH(2,JZ)=IFIX(VALUES(2)*100.0)
        WKPATH(3,JZ)=IFIX(VALUES(3))
        WKPATH(4,JZ)=IFIX(VALUES(4)*100.0)
        IF(BESTURI.LE.2) 
     *  WKPATH(4,JZ)=WKPATH(4,JZ)+MWALKW(IMODE,BESTURI)*100.0
        IF(BESTURI.GE.3.AND.BESTURI.LE.4) THEN
        WKPATH(2,JZ)=WKPATH(1,JZ)+WKPATH(2,JZ)+
     *        IFIX(URBSKIM((BESTURI-2),4)*100.0)
        WKPATH(1,JZ)=IFIX(URBSKIM((BESTURI-2),3)*100.0) 
        WKPATH(3,JZ)=WKPATH(3,JZ)+BTXFER(IMODE,(BESTURI-2))
        WKPATH(4,JZ)=WKPATH(4,JZ)+IFIX(MWALKB(2,(BESTURI-2))*100.0)
        END IF
        WKPATH(5,JZ)=0.0
        IF(BESTURI.LE.4) THEN
        WLKM=STAZNE(3,(ASTA(IMODE,BESTURI)-MAX_IZONES),JZ)
        ELSE
        WLKM=0.0
        END IF
        WKPATH(6,JZ)=IFIX((WLKM+VALUES(6))*100.0)
        WKPATH(7,JZ)=IFIX(VALUES(7)*100.0)
        WKPATH(8,JZ)=IFIX(VALUES(8)*100.0)
        WKPATH(9,JZ)=IFIX(VALUES(9)*100.0)
        WKPATH(10,JZ)=IFIX(VALUES(10)*100.0)
        WKPATH(11,JZ)=IFIX(VALUES(11)*100.0)
        WKPATH(12,JZ)=IFIX(VALUES(12)*100.0)
        WKPATH(13,JZ)=IFIX(VALUES(13))
        WKPATH(14,JZ)=IFIX(VALUES(14)*100.0)
        WKPATH(15,JZ)=IFIX(VALUES(15)*100.0)
        IF(BESTURI.GE.3.AND.BESTURI.LE.4) THEN
        WKPATH(3,JZ)=WKPATH(3,JZ)+1
        WKPATH(7,JZ)=WKPATH(7,JZ)+IFIX(URBSKIM((BESTURI-2),5)*100.0)
        WKPATH(8,JZ)=WKPATH(8,JZ)+IFIX(URBSKIM((BESTURI-2),1)*100.0)
        WKPATH(13,JZ)=WKPATH(13,JZ)+IFIX(URBSKIM((BESTURI-2),6))
        END IF
       END IF 
C..    IF COMMUTER RAIL
       IF(TCRW1.EQ.WKBEST.OR.TCRW2.EQ.WKBEST.OR.
     *    TCRB1.EQ.WKBEST.OR.TCRB2.EQ.WKBEST) THEN
        BESTUR=0.0
        BESTURI=0
        BESTMODE(1)=1
        BESTZONE(1,1)=IZ
        BESTZONE(1,2)=JZ
        DO 710 K=1,4
        IF(EUTIL(K).GT.BESTUR) THEN
        BESTUR=EUTIL(K)
        BESTURI=K
        END IF
  710   CONTINUE
        IF(BESTUR.EQ.0.0.OR.BESTURI.EQ.0) THEN
        WRITE(26,711) IZ,JZ
  711   FORMAT(2X,'NO COMMUTER RAIL BEST PATH FOR IZ=',I4,' JZ=',I4)
        STOP 16
        END IF
        IMODE=1
        CALL RESETV(VALUES)
        CALL RAILPATH(JZ,OSTA(IMODE,BESTURI),
     *               ASTA(IMODE,BESTURI),IMODE,VALUES,STAZONE,
     *               CDSTABRT,STASTAD,STAZNED)
        WKPATH(1,JZ)=IFIX(VALUES(1)*100.0)
        WKPATH(2,JZ)=IFIX(VALUES(2)*100.0)
        WKPATH(3,JZ)=IFIX(VALUES(3))
        WKPATH(4,JZ)=IFIX(VALUES(4)*100.0)
        IF(BESTURI.LE.2) 
     *  WKPATH(4,JZ)=WKPATH(4,JZ)+MWALKW(IMODE,BESTURI)*100.0
        IF(BESTURI.GE.3.AND.BESTURI.LE.4) THEN
        WKPATH(2,JZ)=WKPATH(1,JZ)+WKPATH(2,JZ)+
     *        IFIX(CRBSKIM((BESTURI-2),4)*100.0)
        WKPATH(1,JZ)=IFIX(CRBSKIM((BESTURI-2),3)*100.0) 
        WKPATH(3,JZ)=WKPATH(3,JZ)+BTXFER(IMODE,(BESTURI-2))
        WKPATH(4,JZ)=WKPATH(4,JZ)+IFIX(MWALKB(2,(BESTURI-2))*100.0)
        END IF
        WKPATH(5,JZ)=0.0
        IF(BESTURI.LE.4) THEN
        WLKM=STAZNE(3,(ASTA(IMODE,BESTURI)-MAX_IZONES),JZ)
        ELSE
        WLKM=0.0
        END IF
        WKPATH(6,JZ)=IFIX((WLKM+VALUES(6))*100.0)
        WKPATH(7,JZ)=IFIX(VALUES(7)*100.0)
        WKPATH(8,JZ)=IFIX(VALUES(8)*100.0)
        WKPATH(9,JZ)=IFIX(VALUES(9)*100.0)
        WKPATH(10,JZ)=IFIX(VALUES(10)*100.0)
        WKPATH(11,JZ)=IFIX(VALUES(11)*100.0)
        WKPATH(12,JZ)=IFIX(VALUES(12)*100.0)
        WKPATH(13,JZ)=IFIX(VALUES(13))
        WKPATH(14,JZ)=IFIX(VALUES(14)*100.0)
        WKPATH(15,JZ)=IFIX(VALUES(15)*100.0)
        IF(BESTURI.GE.3.AND.BESTURI.LE.4) THEN
        WKPATH(3,JZ)=WKPATH(3,JZ)+1
        WKPATH(7,JZ)=WKPATH(7,JZ)+IFIX(CRBSKIM((BESTURI-2),5)*100.0)
        WKPATH(8,JZ)=WKPATH(8,JZ)+IFIX(CRBSKIM((BESTURI-2),1)*100.0)
        WKPATH(13,JZ)=WKPATH(13,JZ)+IFIX(CRBSKIM((BESTURI-2),6))
        END IF
        END IF
C..    IF BUS RAPID TRANSIT (BRT)
       IF(TBRTW.EQ.WKBEST) THEN
        WKPATH(1,JZ)=IFIX(WAIT1(5,JZ)*100.0)
        WKPATH(2,JZ)=IFIX(WAIT2(5,JZ)*100.0)
        WKPATH(3,JZ)=TXFER(5,JZ)
        WKPATH(4,JZ)=IFIX(WALKACC(5,JZ)*100.0)
        WKPATH(6,JZ)=IFIX(WALKEGR(5,JZ)*100.0)
        WKPATH(7,JZ)=IFIX(WALKTFR(5,JZ)*100.0)
        WKPATH(8,JZ)=IFIX(BIVT(5,JZ)*100.0)
        WKPATH(9,JZ)=IFIX(EIVT(5,JZ)*100.0)
        WKPATH(10,JZ)=IFIX(WIVT(5,JZ)*100.0)
        WKPATH(13,JZ)=FARE(5,JZ)
        WKPATH(14,JZ)=IFIX(RIVT(5,JZ)*100.0)
        WKPATH(15,JZ)=IFIX(WIVT(5,JZ)*100.0)
        BESTMODE(1)=7
        BESTZONE(7,1)=IZ
        BESTZONE(7,2)=JZ
       END IF  
       WKPATH(16,JZ)=BESTMODE(1)
       END IF
C
C..FIND BEST DRIVE PATH
C
      DRBEST=DMAX1(TCRP1,TCRP2,TCRP3,TCRP4,TCRK1,TCRK2,TCRK3,TCRK4,
     *             TURP1,TURP2,TURP3,TURP4,TURK1,TURK2,TURK3,TURK4,
     *             TEXPD,TWAYD,TBRTD)
      BESTMODE(2)=6
      BESTZONE(9,1)=9999
      BESTZONE(9,2)=9999
      IF(M.EQ.DRVIND.AND.DRBEST.GT.0.0) THEN
C..    IF EXPRESS BUS
       IF(TEXPD.EQ.DRBEST) THEN
        IMODE=3
        CALL DRVBUS(JZ,CSTAE,CDSTAE,IMODE,VALUES)
        DRPATH(1,JZ)=IFIX(VALUES(1)*100.0)
        DRPATH(2,JZ)=IFIX(VALUES(2)*100.0)
        DRPATH(3,JZ)=IFIX(VALUES(3))
        DRPATH(4,JZ)=IFIX((CWALK1+VALUES(4))*100.0)
        DRPATH(5,JZ)=IFIX(TAB1DA(CSTAE)*100.0)
        DRPATH(6,JZ)=IFIX((CWALK2+VALUES(6))*100.0)
        DRPATH(7,JZ)=IFIX(VALUES(7)*100.0)
        DRPATH(8,JZ)=IFIX(VALUES(8)*100.0)
        DRPATH(9,JZ)=IFIX(VALUES(9)*100.0)
        DRPATH(13,JZ)=IFIX(VALUES(13))
        DRPATH(14,JZ)=IFIX(VALUES(14)*100.0)
        DRPATH(15,JZ)=IFIX(VALUES(15)*100.0)
        BESTMODE(2)=4
        BESTZONE(9,1)=IZ
        BESTZONE(9,2)=JZ
       END IF
C..    IF TRANSITWAY BUS
       IF(TWAYD.EQ.DRBEST) THEN
        IMODE=4
        CALL DRVBUS(JZ,CSTAT,CDSTAT,IMODE,VALUES)
        DRPATH(1,JZ)=IFIX(VALUES(1)*100.0)
        DRPATH(2,JZ)=IFIX(VALUES(2)*100.0)
        DRPATH(3,JZ)=IFIX(VALUES(3))
        DRPATH(4,JZ)=IFIX((TWWALK1+VALUES(4))*100.0)
        DRPATH(5,JZ)=IFIX(TAB1DA(CSTAT)*100.0)
        DRPATH(6,JZ)=IFIX((TWWALK2+VALUES(6))*100.0)
        DRPATH(7,JZ)=IFIX(VALUES(7)*100.0)
        DRPATH(8,JZ)=IFIX(VALUES(8)*100.0)
        DRPATH(9,JZ)=IFIX(VALUES(9)*100.0)
        DRPATH(10,JZ)=IFIX(VALUES(10)*100.0)
        DRPATH(13,JZ)=IFIX(VALUES(13))
        DRPATH(14,JZ)=IFIX(VALUES(14)*100.0)
        DRPATH(15,JZ)=IFIX(VALUES(15)*100.0)
        BESTMODE(2)=5
        BESTZONE(10,1)=IZ
        BESTZONE(10,2)=JZ
       END IF   
C..    IF URBAN RAIL
       IF(TURP1.EQ.DRBEST.OR.TURP2.EQ.DRBEST.OR.
     *    TURP3.EQ.DRBEST.OR.TURP4.EQ.DRBEST.OR.
     *    TURK1.EQ.DRBEST.OR.TURK2.EQ.DRBEST.OR.
     *    TURK3.EQ.DRBEST.OR.TURK4.EQ.DRBEST) THEN
        BESTUR=0.0
        BESTURI=0
        BESTMODE(2)=2
        BESTZONE(11,1)=IZ
        BESTZONE(11,2)=JZ
        DO 720 K=17,24
        IF(EUTIL(K).GT.BESTUR) THEN
        BESTUR=EUTIL(K)
        BESTURI=K
        END IF
  720   CONTINUE
        IF(BESTUR.EQ.0.0.OR.BESTURI.EQ.0) THEN
        WRITE(26,721) IZ,JZ
  721   FORMAT(2X,'NO URBAN RAIL BEST DRIVE PATH FOR IZ=',
     *         I4,' JZ=',I4)
C        STOP 16
        END IF
        IMODE=2
        BESTURI=BESTURI-12
        CALL RAILPATH(JZ,OSTA(IMODE,BESTURI),
     *                ASTA(IMODE,BESTURI),IMODE,VALUES,STAZONE,
     *                CDSTABRT,STASTAD,STAZNED)
        DRPATH(1,JZ)=IFIX(VALUES(1)*100.0)
        DRPATH(2,JZ)=IFIX(VALUES(2)*100.0)
        DRPATH(3,JZ)=IFIX(VALUES(3))
        DRPATH(4,JZ)=IFIX(VALUES(4)*100.0)
        IF(BESTURI.LE.2) 
     *  DRPATH(4,JZ)=DRPATH(4,JZ)+MWALKW(IMODE,BESTURI)*100.0
        IF(BESTURI.GE.3.AND.BESTURI.LE.4) THEN
        DRPATH(2,JZ)=DRPATH(1,JZ)+DRPATH(2,JZ)+
     *        IFIX(URBSKIM((BESTURI-2),4)*100.0)
        DRPATH(1,JZ)=IFIX(URBSKIM((BESTURI-2),3)*100.0) 
        DRPATH(3,JZ)=DRPATH(3,JZ)+BTXFER(IMODE,(BESTURI-2))
        DRPATH(4,JZ)=DRPATH(4,JZ)+IFIX(MWALKB(2,(BESTURI-2))*100.0)
        END IF
        DRPATH(5,JZ)=0.0
        IF(BESTURI.GT.4) 
     *  DRPATH(5,JZ)=IFIX(TAB1DA(OSTA(IMODE,BESTURI))*100.0)
        IF(BESTURI.LE.4) THEN
        WLKM=STAZNE(3,(ASTA(IMODE,BESTURI)-MAX_IZONES),JZ)
        ELSE
        WLKM=0.0
        END IF
        DRPATH(6,JZ)=IFIX((WLKM+VALUES(6))*100.0)
        DRPATH(7,JZ)=IFIX(VALUES(7)*100.0)
        DRPATH(8,JZ)=IFIX(VALUES(8)*100.0)
        DRPATH(9,JZ)=IFIX(VALUES(9)*100.0)
        DRPATH(10,JZ)=IFIX(VALUES(10)*100.0)
        DRPATH(11,JZ)=IFIX(VALUES(11)*100.0)
        DRPATH(12,JZ)=IFIX(VALUES(12)*100.0)
        DRPATH(13,JZ)=IFIX(VALUES(13))
        DRPATH(14,JZ)=IFIX(VALUES(14)*100.0)
        DRPATH(15,JZ)=IFIX(VALUES(15)*100.0)
        IF(BESTURI.GE.3.AND.BESTURI.LE.4) THEN
        DRPATH(3,JZ)=DRPATH(3,JZ)+1
        DRPATH(7,JZ)=DRPATH(7,JZ)+IFIX(URBSKIM((BESTURI-2),5)*100.0)
        DRPATH(8,JZ)=DRPATH(8,JZ)+IFIX(URBSKIM((BESTURI-2),1)*100.0)
        DRPATH(13,JZ)=DRPATH(13,JZ)+IFIX(URBSKIM((BESTURI-2),6))
        END IF
       END IF 
C..    IF COMMUTER RAIL
       IF(TCRP1.EQ.DRBEST.OR.TCRP2.EQ.DRBEST.OR.
     *    TCRP3.EQ.DRBEST.OR.TCRP4.EQ.DRBEST.OR.
     *    TCRK1.EQ.DRBEST.OR.TCRK2.EQ.DRBEST.OR.
     *    TCRK3.EQ.DRBEST.OR.TCRK4.EQ.DRBEST) THEN
        BESTUR=0.0
        BESTURI=0
        BESTMODE(2)=1
        BESTZONE(12,1)=IZ
        BESTZONE(12,2)=JZ
        DO 730 K=5,12
        IF(EUTIL(K).GT.BESTUR) THEN
        BESTUR=EUTIL(K)
        BESTURI=K
        END IF
  730   CONTINUE
        IF(BESTUR.EQ.0.0.OR.BESTURI.EQ.0) THEN
        WRITE(26,731) IZ,JZ
  731   FORMAT(2X,'NO COMMUTER RAIL BEST DRIVE PATH FOR IZ=',
     *         I4,' JZ=',I4)
        STOP 16
        END IF
        IMODE=1
        CALL RAILPATH(JZ,OSTA(IMODE,BESTURI),
     *                ASTA(IMODE,BESTURI),IMODE,VALUES,STAZONE,
     *                CDSTABRT,STASTAD,STAZNED)
        DRPATH(1,JZ)=IFIX(VALUES(1)*100.0)
        DRPATH(2,JZ)=IFIX(VALUES(2)*100.0)
        DRPATH(3,JZ)=IFIX(VALUES(3))
        DRPATH(4,JZ)=IFIX(VALUES(4)*100.0)
        IF(BESTURI.LE.2) 
     *  DRPATH(4,JZ)=DRPATH(4,JZ)+MWALKW(IMODE,BESTURI)*100.0
        IF(BESTURI.GE.3.AND.BESTURI.LE.4) THEN
        DRPATH(2,JZ)=DRPATH(1,JZ)+DRPATH(2,JZ)+
     *        IFIX(CRBSKIM((BESTURI-2),4)*100.0)
        DRPATH(1,JZ)=IFIX(CRBSKIM((BESTURI-2),3)*100.0) 
        DRPATH(3,JZ)=DRPATH(3,JZ)+BTXFER(IMODE,(BESTURI-2))
        DRPATH(4,JZ)=DRPATH(4,JZ)+IFIX(MWALKB(1,(BESTURI-2))*100.0)
        END IF
        DRPATH(5,JZ)=0.0
        IF(BESTURI.GT.4) 
     *  DRPATH(5,JZ)=IFIX(TAB1DA(OSTA(IMODE,BESTURI))*100.0)
        IF(BESTURI.LE.4) THEN
        WLKM=STAZNE(3,(ASTA(IMODE,BESTURI)-MAX_IZONES),JZ)
        ELSE
        WLKM=0.0
        END IF
        DRPATH(6,JZ)=IFIX((WLKM+VALUES(6))*100.0)
        DRPATH(7,JZ)=IFIX(VALUES(7)*100.0)
        DRPATH(8,JZ)=IFIX(VALUES(8)*100.0)
        DRPATH(9,JZ)=IFIX(VALUES(9)*100.0)
        DRPATH(10,JZ)=IFIX(VALUES(10)*100.0)
        DRPATH(11,JZ)=IFIX(VALUES(11)*100.0)
        DRPATH(12,JZ)=IFIX(VALUES(12)*100.0)
        DRPATH(13,JZ)=IFIX(VALUES(13))
        DRPATH(14,JZ)=IFIX(VALUES(14)*100.0)
        DRPATH(15,JZ)=IFIX(VALUES(15)*100.0)
        IF(BESTURI.GE.3.AND.BESTURI.LE.4) THEN
        DRPATH(3,JZ)=DRPATH(3,JZ)+1
        DRPATH(7,JZ)=DRPATH(7,JZ)+IFIX(CRBSKIM((BESTURI-2),5)*100.0)
        DRPATH(8,JZ)=DRPATH(8,JZ)+IFIX(CRBSKIM((BESTURI-2),1)*100.0)
        DRPATH(13,JZ)=DRPATH(13,JZ)+IFIX(CRBSKIM((BESTURI-2),6))
        END IF
       END IF
C..    IF BRT
        IF(TBRTD.EQ.DRBEST) THEN
        IMODE=5
        IF(BRTUR) THEN
        CALL RAILPATH(JZ,CSTABRT,CBSTAE,IMODE,VALUES,STAZONE,
     *                CDSTABRT,STASTAD,STAZNED)
        DRPATH(1,JZ)=IFIX(VALUES(1)*100.0)
        DRPATH(2,JZ)=IFIX(VALUES(2)*100.0)
        DRPATH(3,JZ)=IFIX(VALUES(3))
        DRPATH(4,JZ)=IFIX(VALUES(4)*100.0)
        DRPATH(5,JZ)=IFIX(TAB1DA(CSTABRT)*100.0)
        DRPATH(6,JZ)=IFIX(VALUES(6)*100.0)
        DRPATH(7,JZ)=IFIX(VALUES(7)*100.0)
        DRPATH(8,JZ)=IFIX(VALUES(8)*100.0)
        DRPATH(9,JZ)=IFIX(VALUES(9)*100.0)
        DRPATH(10,JZ)=IFIX(VALUES(10)*100.0)
        DRPATH(11,JZ)=IFIX(VALUES(11)*100.0)
        DRPATH(12,JZ)=IFIX(VALUES(12)*100.0)
        DRPATH(13,JZ)=IFIX(VALUES(13))
        DRPATH(14,JZ)=IFIX(VALUES(14)*100.0)
        DRPATH(15,JZ)=IFIX(VALUES(15)*100.0)
        ELSE
        CALL DRVBUS(JZ,CSTABRT,CDSTABRT,IMODE,VALUES)
        DRPATH(1,JZ)=IFIX(VALUES(1)*100.0)
        DRPATH(2,JZ)=IFIX(VALUES(2)*100.0)
        DRPATH(3,JZ)=IFIX(VALUES(3))
        DRPATH(4,JZ)=IFIX((TWWALK1+VALUES(4))*100.0)
        DRPATH(5,JZ)=IFIX(TAB1DA(CSTABRT)*100.0)
        DRPATH(6,JZ)=IFIX((TWWALK2+VALUES(6))*100.0)
        DRPATH(7,JZ)=IFIX(VALUES(7)*100.0)
        DRPATH(8,JZ)=IFIX(VALUES(8)*100.0)
        DRPATH(9,JZ)=IFIX(VALUES(9)*100.0)
        DRPATH(10,JZ)=IFIX(VALUES(10)*100.0)
        DRPATH(13,JZ)=IFIX(VALUES(13))
        DRPATH(14,JZ)=IFIX(VALUES(14)*100.0)
        DRPATH(15,JZ)=IFIX(VALUES(15)*100.0)
        END IF
        BESTMODE(2)=7
        BESTZONE(13,1)=IZ
        BESTZONE(13,2)=JZ
        END IF 
        DRPATH(16,JZ)=BESTMODE(2)   
       END IF
C....................................................................
      IF(DEBUG.AND.(M.EQ.WLKIND.OR.M.EQ.DRVIND)) THEN
      WRITE(26,9250) C,M,WLKIND,DRVIND,WKBEST,BESTNAME(BESTMODE(1)),
     *               DRBEST,BESTNAME(BESTMODE(2)),
     *              ((WKPATH(K,JZ),DRPATH(K,JZ)),K=1,15),BESTURI,BESTUR,
     *               WKPATH(16,JZ),DRPATH(16,JZ)
 9250 FORMAT(/1X,'BEST WALK/DRIVE PATH ANALYSIS'/
     *        1X,'-----------------------------'/
     *        1X,'INCOME GROUP        =',I12/
     *        1X,'MARKET SEGMENT      =',I12/
     *        1X,'WALK  INDICATOR     =',I12/
     *        1X,'DRIVE INDICATOR     =',I12/
     *        1X,'BEST  WALK TRIP     =',E12.5,3X,A13/
     *        1X,'BEST DRIVE TRIP     =',E12.5,3X,A13//
     *        1X,'                     ',' WALK/DRIVE ',
     *               2X,'    DRIVE'/
     *        1X,'---------------------','------------',
     *               2X,'------------'/
     *        1X,'FIRST WAIT          =',I12,2X,I12/
     *        1X,'TRANSFER WAIT       =',I12,2X,I12/
     *        1X,'TRANSFERS           =',I12,2X,I12/
     *        1X,'WALK ACCESS         =',I12,2X,I12/
     *        1X,'DRIVE ACCESS        =',I12,2X,I12/
     *        1X,'WALK EGRESS         =',I12,2X,I12/
     *        1X,'TRANSFER WALK       =',I12,2X,I12/
     *        1X,'LOCAL BUS IVT       =',I12,2X,I12/
     *        1X,'EXPRESS BUS IVT     =',I12,2X,I12/
     *        1X,'TRANSITWAY IVT      =',I12,2X,I12/
     *        1X,'URBAN RAIL IVT      =',I12,2X,I12/
     *        1X,'COMMUTER RAIL IVT   =',I12,2X,I12/
     *        1X,'FARE                =',I12,2X,I12/
     *        1X,'RAPID BUS IVT       =',I12,2X,I12/
     *        1X,'BRT IVT             =',I12,2X,I12//
     *        1X,'BESTURI             =',I12/
     *        1X,'BESTUR              =',E12.5/
     *        1X,'BESTMODE            =',I12,2X,I12/)
      END IF
C....................................................................
      END IF 
C
C  STORE STATION DATA AND SHARE VALUES FOR
C  USE IN ON-BOARD SURVEY ASSIGNMENT ANALYSIS
C
      IF(MTXOBS) THEN
      OBSVAL(1,JZ)=CSTAE
      OBSVAL(2,JZ)=CSTAT
       IF(BRTUR) THEN
       OBSVAL(3,JZ)=MAX_ZONES
       OBSVAL(4,JZ)=CBSTA
       ELSE
       OBSVAL(3,JZ)=CSTABRT
       OBSVAL(4,JZ)=MAX_ZONES
       END IF
      K3=4
      DO 771 K1=1,2
      DO 772 K2=1,12
      K3=K3+1
      OBSVAL(K3,JZ)=OSTA(K1,K2)
      K3=K3+1
      OBSVAL(K3,JZ)=ASTA(K1,K2)
  772 CONTINUE
  771 CONTINUE
      OBSVALR(1,JZ)=OBSVALR(1,JZ)+TCRW1
      OBSVALR(2,JZ)=OBSVALR(2,JZ)+TCRW2
      OBSVALR(3,JZ)=OBSVALR(3,JZ)+TCRB1
      OBSVALR(4,JZ)=OBSVALR(4,JZ)+TCRB2
      OBSVALR(5,JZ)=OBSVALR(5,JZ)+TCRP1
      OBSVALR(6,JZ)=OBSVALR(6,JZ)+TCRP2
      OBSVALR(7,JZ)=OBSVALR(7,JZ)+TCRP3
      OBSVALR(8,JZ)=OBSVALR(8,JZ)+TCRP4
      OBSVALR(9,JZ)=OBSVALR(9,JZ)+TCRK1
      OBSVALR(10,JZ)=OBSVALR(10,JZ)+TCRK2
      OBSVALR(11,JZ)=OBSVALR(11,JZ)+TCRK3
      OBSVALR(12,JZ)=OBSVALR(12,JZ)+TCRK4     
      OBSVALR(13,JZ)=OBSVALR(13,JZ)+TURW1
      OBSVALR(14,JZ)=OBSVALR(14,JZ)+TURW2
      OBSVALR(15,JZ)=OBSVALR(15,JZ)+TURB1
      OBSVALR(16,JZ)=OBSVALR(16,JZ)+TURB2
      OBSVALR(17,JZ)=OBSVALR(17,JZ)+TURP1
      OBSVALR(18,JZ)=OBSVALR(18,JZ)+TURP2
      OBSVALR(19,JZ)=OBSVALR(19,JZ)+TURP3
      OBSVALR(20,JZ)=OBSVALR(20,JZ)+TURP4
      OBSVALR(21,JZ)=OBSVALR(21,JZ)+TURK1
      OBSVALR(22,JZ)=OBSVALR(22,JZ)+TURK2
      OBSVALR(23,JZ)=OBSVALR(23,JZ)+TURK3
      OBSVALR(24,JZ)=OBSVALR(24,JZ)+TURK4
C ----------------------------------------------------
      IF(DEBUG) THEN
      DO 774 K1=1,52
      WRITE(26,773) K1,OBSVAL(K1,JZ)
  773 FORMAT(' OBSVAL(',I2,')=',I8)
  774 CONTINUE
      DO 775 K1=1,24
      K2=K1+52
      WRITE(26,776) K1,K2,OBSVALR(K1,JZ)
  776 FORMAT(' OBSVALR(',I2,'=',I2,')=',E12.5)
  775 CONTINUE
      END IF
C ----------------------------------------------------
      END IF
C
C  INTRAZONAL COMPUTATIONS
C
      IF(IZ.EQ.JZ) THEN
      TTRAN=0.0
      TNMOT=0.0
      TAUTO=0.0
      TNMWK=0.0
      TNMBK=0.0
      TDRV0=0.0
      TDSHR=0.0
      TCR=0.0
      TUR=0.0
      TWAY=0.0
      TBRT=0.0
      TEXP =0.0
      TLOC =0.0
      TRPD =0.0
      TDRV2=0.0
      TDRV3=0.0
      TLOCW=0.0
      TLOCD=0.0
      TRPDW=0.0
      TRPDD=0.0
      TEXPW=0.0
      TEXPD=0.0
      TBRTW=0.0
      TBRTD=0.0
      TCRW=0.0
      TCRB=0.0
      TCRP=0.0
      TCRK=0.0
      TCRW1=0.0
      TCRW2=0.0
      TCRB1=0.0
      TCRB2=0.0
      TCRP1=0.0
      TCRP2=0.0
      TCRP3=0.0
      TCRP4=0.0
      TCRK1=0.0
      TCRK2=0.0
      TCRK3=0.0
      TCRK4=0.0
      TURW=0.0
      TURB=0.0
      TURP=0.0
      TURK=0.0
      TURW1=0.0
      TURW2=0.0
      TURB1=0.0
      TURB2=0.0
      TURP1=0.0
      TURP2=0.0
      TURP3=0.0
      TURP4=0.0
      TURK1=0.0
      TURK2=0.0
      TURK3=0.0
      TURK4=0.0
      TWAYW=0.0
      TWAYD=0.0
      TDRV0N=0.0
      TDRV0T=0.0
      TDRV2NH=0.0
      TDRV2NN=0.0
      TDRV2TH=0.0
      TDRV2TN=0.0
      TDRV3NH=0.0
      TDRV3NN=0.0
      TDRV3TH=0.0
      TDRV3TN=0.0
      TDRV4NH=0.0
      TDRV4NN=0.0
      TDRV4TH=0.0
      TDRV4TN=0.0
      IF(NMOT) TNMOT=PNMOT*PERTRP
      TAUTO=PERTRP-TNMOT
      TNMWK=PNWALK*TNMOT
      TNMBK=TNMOT-TNMWK
      TDRV0=PDRV0*TAUTO
      TDSHR=TAUTO-TDRV0
      TDRV2=PDRV2*TDSHR
      TDRV4=PDRV4*(TDSHR-TDRV2)
      TDRV3=TDSHR-TDRV2-TDRV4
      TDRV0N=TDRV0
      TDRV2NN=TDRV2
      TDRV3NN=TDRV3
      TDRV4NN=TDRV4
      LRTWLK(JZ)=0
      LRTDRV(JZ)=0
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9421) C,M,TTRAN,TAUTO,TNMOT,TDRV0,
     *               TDSHR,TCR,TUR,TWAY,TEXP,TLOC,TRPD,TDRV2,TDRV3,
     *               TDRV4,TNMWK,TNMBK
      WRITE(26,9022) TLOCW,TLOCD,TRPDW,TRPDD,
     *               TCRW,TCRB,TCRP,TCRK,TCRW1,TCRW2,
     *               TCRB1,TCRB2
      WRITE(26,9024) TCRP1,TCRP2,TCRP3,TCRP4,
     *               TCRK1,TCRK2,TCRK3,TCRK4
      WRITE(26,9023) TURW,TURB,TURP,TURK,TURW1,TURW2,
     *               TURB1,TURB2
      WRITE(26,9425) TURP1,TURP2,TURP3,TURP4,
     *               TURK1,TURK2,TURK3,TURK4
      WRITE(26,9027) TWAYW,TWAYD,TEXPW,TEXPD,TBRTW,TBRTD
 9421 FORMAT(/1X,'SUMMARY OF MODAL TRIP VALUES'/
     *       1X,'-----------------------------------'/
     *       1X,'INCOME GROUP=',I2,' MARKET SEGMENT=',I2//
     *       1X,'TRANSIT                        =',E12.5/
     *       1X,'AUTO                           =',E12.5/
     *       1X,'NON-MOTORIZED                  =',E12.5//
     *       1X,'DRIVE ALONE                    =',E12.5/
     *       1X,'SHARED RIDE                    =',E12.5//
     *       1X,'COMMUTER RAIL                  =',E12.5/
     *       1X,'URBAN RAIL                     =',E12.5/
     *       1X,'TRANSITWAY BUS                 =',E12.5/
     *       1X,'EXPRESS BUS                    =',E12.5/
     *       1X,'LOCAL BUS                      =',E12.5/
     *       1X,'RAPID BUS                      =',E12.5//
     *       1X,'2  PERSON AUTO                 =',E12.5/
     *       1X,'3+ PERSON AUTO                 =',E12.5/
     *       1X,'4+ PERSON AUTO                 =',E12.5/
     *       1X,'NON-MOTORIZED WALK             =',E12.5/
     *       1X,'NON-MOTORIZED BIKE             =',E12.5/)
 9022 FORMAT(1X,'WALK  --> LOCAL BUS            =',E12.5/
     *       1X,'DRIVE --> LOCAL BUS            =',E12.5/
     *       1X,'WALK  --> RAPID BUS            =',E12.5/
     *       1X,'DRIVE --> RAPID BUS            =',E12.5/
     *       1X,'WALK  --> COMMUTER RAIL        =',E12.5/
     *       1X,'BUS   --> COMMUTER RAIL        =',E12.5/
     *       1X,'P&R   --> COMMUTER RAIL        =',E12.5/
     *       1X,'K&R   --> COMMUTER RAIL        =',E12.5//
     *       1X,'COMMUTER RAIL WALK  -STATION #1=',E12.5/
     *       1X,'COMMUTER RAIL WALK  -STATION #2=',E12.5/
     *       1X,'COMMUTER RAIL BUS   -STATION #1=',E12.5/
     *       1X,'COMMUTER RAIL BUS   -STATION #2=',E12.5)
 9024 FORMAT(1X,'COMMUTER RAIL PNR   -STATION #1=',E12.5/
     *       1X,'COMMUTER RAIL PNR   -STATION #2=',E12.5/
     *       1X,'COMMUTER RAIL PNR   -STATION #3=',E12.5/
     *       1X,'COMMUTER RAIL PNR   -STATION #4=',E12.5/
     *       1X,'COMMUTER RAIL PNR   -STATION #1=',E12.5/
     *       1X,'COMMUTER RAIL PNR   -STATION #2=',E12.5/
     *       1X,'COMMUTER RAIL KNR   -STATION #3=',E12.5/
     *       1X,'COMMUTER RAIL KNR   -STATION #4=',E12.5/)
 9023 FORMAT(1X,'WALK  --> URBAN RAIL           =',E12.5/
     *       1X,'BUS   --> URBAN RAIL           =',E12.5/
     *       1X,'P&R   --> URBAN RAIL           =',E12.5/
     *       1X,'K&R   --> URBAN RAIL           =',E12.5//
     *       1X,'URBAN RAIL WALK     -STATION #1=',E12.5/
     *       1X,'URBAN RAIL WALK     -STATION #2=',E12.5/
     *       1X,'URBAN RAIL BUS      -STATION #1=',E12.5/
     *       1X,'URBAN RAIL BUS      -STATION #2=',E12.5/)
 9425 FORMAT(1X,'URBAN RAIL PNR      -STATION #1=',E12.5/
     *       1X,'URBAN RAIL PNR      -STATION #2=',E12.5/
     *       1X,'URBAN RAIL PNR      -STATION #3=',E12.5/
     *       1X,'URBAN RAIL PNR      -STATION #4=',E12.5/
     *       1X,'URBAN RAIL KNR      -STATION #1=',E12.5/
     *       1X,'URBAN RAIL KNR      -STATION #2=',E12.5/
     *       1X,'URBAN RAIL KNR      -STATION #3=',E12.5/
     *       1X,'URBAN RAIL KNR      -STATION #4=',E12.5/)
 9027 FORMAT(1X,'WALK  --> TRANSITWAY BUS       =',E12.5/
     *       1X,'DRIVE --> TRANSITWAY BUS       =',E12.5/
     *       1X,'WALK  --> EXPRESS BUS          =',E12.5/
     *       1X,'DRIVE --> EXPRESS BUS          =',E12.5/
     *       1X,'WALK  --> BUS RAPID TRANSIT    =',E12.5/
     *       1X,'DRIVE --> BUS RAPID TRANSIT    =',E12.5)
      END IF
C....................................................................
C
C  STORE SUMMARY STATISTICS
C
C..LOGSUM DATA
      ALOGSUM((C+3),JZ)=ALOGSUM((C+3),JZ)+ TURW+TURB+TURP+TURK
      TLOGSUM((C+3),JZ)=TLOGSUM((C+3),JZ)+ TTRAN
      ULOGSUM((C+3),JZ)=ULOGSUM((C+3),JZ)+ PERTRP
C..UPPER LEVEL
      TESUM(1,C)=TESUM(1,C) + TDRV0
      TESUM(2,C)=TESUM(2,C) + TDRV2
      TESUM(3,C)=TESUM(3,C) + TDRV3
      TESUM(64,C)=TESUM(64,C) + TDRV4
      TESUM(5,C)=TESUM(5,C) + TTRAN
      TESUM(6,C)=TESUM(6,C) + PERTRP
      TESUM(50,C)=TESUM(50,C) + TNMOT
      TESUM(51,C)=TESUM(51,C) + TNMWK
      TESUM(52,C)=TESUM(52,C) + TNMBK
      LZ=IFIX(ZHHD(6,IZ))
      LZ=MAX0(LZ,1)
      IF(LZ.GT.0.AND.LZ.LE.5) THEN
      CALSUM(1,C,LZ)=CALSUM(1,C,LZ)+TDRV0
      CALSUM(2,C,LZ)=CALSUM(2,C,LZ)+TDRV2
      CALSUM(3,C,LZ)=CALSUM(3,C,LZ)+TDRV3
      CALSUM(4,C,LZ)=CALSUM(4,C,LZ)+PERTRP
      CALSUM(5,C,LZ)=CALSUM(5,C,LZ)+TDRV4
      END IF
C..TRANSIT PRIMARY/SUBMODE LEVEL
      TESUM(7,C)=TESUM(7,C) + TLOCW
      TESUM(8,C)=TESUM(8,C) + TLOCD
      TESUM(9,C)=TESUM(9,C) + TEXPW
      TESUM(10,C)=TESUM(10,C) + TEXPD
      TESUM(11,C)=TESUM(11,C) + TCRW
      TESUM(12,C)=TESUM(12,C) + TCRB
      TESUM(13,C)=TESUM(13,C) + TCRP
      TESUM(14,C)=TESUM(14,C) + TCRK
      TESUM(15,C)=TESUM(15,C) + TURW
      TESUM(16,C)=TESUM(16,C) + TURB
      TESUM(17,C)=TESUM(17,C) + TURP
      TESUM(18,C)=TESUM(18,C) + TURK
      TESUM(19,C)=TESUM(19,C) + TWAYW
      TESUM(20,C)=TESUM(20,C) + TWAYD
      TESUM(21,C)=TESUM(21,C) + TTRAN
      TESUM(48,C)=TESUM(48,C) + TRPDW
      TESUM(49,C)=TESUM(49,C) + TRPDD
      TESUM(56,C)=TESUM(56,C) + TBRTW
C..RAPID TRANSFERS
      IF(BIVT(4,JZ).GT.0) THEN
       TESUM(53,C)=TESUM(53,C) + TRPDW
       TESUM(60,C)=TESUM(60,C) + TRPDD
      END IF
C..TRANSITWAY TRANSFERS
      IF(BIVT(3,JZ).GT.0)
     * TESUM(54,C)=TESUM(54,C) + TWAYW
      IT=CSTAT-MAX_IZONES
      IF((STAZNEI(IT,JZ,4,1).GT.0).OR.(STAZNEI(IT,JZ,4,2).GT.0).OR.
     *   (STAZNEI(IT,JZ,4,3).GT.0)) THEN
      TESUM(62,C)=TESUM(62,C)+ TWAYD
      END IF
C..EXPRESS TRANSFERS
      IF(BIVT(2,JZ).GT.0)
     * TESUM(55,C)=TESUM(55,C) + TEXPW
      IT=CSTAE-MAX_IZONES
      IF((STAZNEI(IT,JZ,3,1).GT.0).OR.(STAZNEI(IT,JZ,3,2).GT.0)) THEN
      TESUM(63,C)=TESUM(63,C)+ TEXPD
      END IF
C..BRT TRANSFERS 
      IF(BIVT(5,JZ).GT.0.OR.RIVT(5,JZ).GT.0.0)
     * TESUM(58,C)=TESUM(58,C) + TBRTW
      IF(BRTUR) THEN
      TESUM(59,C)=TESUM(59,C) + TBRTD
      ELSE
      TESUM(57,C)=TESUM(57,C) + TBRTD
       IF(MODEINC(5,1,3,2).GT.0.OR.MODEINC(5,2,3,2).GT.0) THEN
       TESUM(61,C)=TESUM(61,C) + TBRTD
       END IF
      END IF
C
C..TOTAL TRANSIT TRIPS TO THE LA CBD
C
      DO 1320 E=1,50
      IF(JZ.EQ.CBDZ(E)) THEN
      CBDTRAN=CBDTRAN+TTRAN
      END IF
 1320 CONTINUE
C
C..LOCAL BUS BY COUNTY OF ORIGIN
C
      CCODE=IFIX(ZHHD(6,IZ))
      CCODE=IMAX0(CCODE,1)
      IF(CCODE.GT.0.AND.CCODE.LE.5) THEN
      LOCBUS(CCODE,C,1)=LOCBUS(CCODE,C,1)+TLOCW
      LOCBUS(CCODE,C,2)=LOCBUS(CCODE,C,2)+TLOCD
      END IF
C..COMMUTER RAIL STATION LEVEL
      TESUM(22,C)=TESUM(22,C) + TCRW1
      TESUM(23,C)=TESUM(23,C) + TCRW2
      TESUM(24,C)=TESUM(24,C) + TCRB1
      TESUM(25,C)=TESUM(25,C) + TCRB2
      TESUM(26,C)=TESUM(26,C) + TCRP1
      TESUM(27,C)=TESUM(27,C) + TCRP2
      TESUM(28,C)=TESUM(28,C) + TCRP3
      TESUM(29,C)=TESUM(29,C) + TCRP4
      TESUM(30,C)=TESUM(30,C) + TCRK1
      TESUM(31,C)=TESUM(31,C) + TCRK2
      TESUM(32,C)=TESUM(32,C) + TCRK3
      TESUM(33,C)=TESUM(33,C) + TCRK4
      TESUM(34,C)=TESUM(34,C) + TCR
C..URBAN RAIL STATION LEVEL
      TESUM(35,C)=TESUM(35,C) + TURW1
      TESUM(36,C)=TESUM(36,C) + TURW2
      TESUM(37,C)=TESUM(37,C) + TURB1
      TESUM(38,C)=TESUM(38,C) + TURB2
      TESUM(39,C)=TESUM(39,C) + TURP1
      TESUM(40,C)=TESUM(40,C) + TURP2
      TESUM(41,C)=TESUM(41,C) + TURP3
      TESUM(42,C)=TESUM(42,C) + TURP4
      TESUM(43,C)=TESUM(43,C) + TURK1
      TESUM(44,C)=TESUM(44,C) + TURK2
      TESUM(45,C)=TESUM(45,C) + TURK3
      TESUM(46,C)=TESUM(46,C) + TURK4
      TESUM(47,C)=TESUM(47,C) + TUR
C...COMMUTER RAIL AND URBAN RAIL PNR TRIPS BY POSITION
      PNRTRP(1)=PNRTRP(1)+TCRP1
      PNRTRP(2)=PNRTRP(2)+TCRP2
      PNRTRP(3)=PNRTRP(3)+TCRP3
      PNRTRP(4)=PNRTRP(4)+TCRP4
      PNRTRP(5)=PNRTRP(5)+TURP1
      PNRTRP(6)=PNRTRP(6)+TURP2
      PNRTRP(7)=PNRTRP(7)+TURP3
      PNRTRP(8)=PNRTRP(8)+TURP4
C ----------------------------------------
C..MODE TRANSFER ANALYSIS SUMMARY
C ----------------------------------------
C..URBAN RAIL
      MDETRP(1)=TURW1
      MDETRP(2)=TURW2
      MDETRP(3)=TURB1
      MDETRP(4)=TURB2
      MDETRP(5)=TURP1
      MDETRP(6)=TURP2
      MDETRP(7)=TURP3
      MDETRP(8)=TURP4
      MDETRP(9)=TURK1
      MDETRP(10)=TURK2
      MDETRP(11)=TURK3
      MDETRP(12)=TURK4
C..EGRESS
      DO 53 K1=1,12
      II=0
      DO 54 K2=1,5
      MODETRP(2,K2,K1,2)=MODETRP(2,K2,K1,2)+
     *                   MDETRP(K1)*FLOAT(MODEINC(2,K2,K1,2))
      II=II+MODEINC(2,K2,K1,2)
   54 CONTINUE
      IF(II.GT.0) MODETRP(2,7,K1,2)=MODETRP(2,7,K1,2)+MDETRP(K1)
   53 CONTINUE
C..ACCESS
      MODETRP(2,1,3,1)=MODETRP(2,1,3,1)+TURB1*FLOAT(MODEINC(2,1,3,1))
      MODETRP(2,2,3,1)=MODETRP(2,2,3,1)+TURB1*FLOAT(MODEINC(2,2,3,1))
      MODETRP(2,3,3,1)=MODETRP(2,3,3,1)+TURB1*FLOAT(MODEINC(2,3,3,1))
      MODETRP(2,4,3,1)=MODETRP(2,4,3,1)+TURB1*FLOAT(MODEINC(2,4,3,1))
      MODETRP(2,5,3,1)=MODETRP(2,5,3,1)+TURB1*FLOAT(MODEINC(2,5,3,1))
      II=MODEINC(2,1,3,1)+MODEINC(2,2,3,1)+MODEINC(2,3,3,1)+
     *   MODEINC(2,4,3,1)+MODEINC(2,5,3,1)
      IF(II.GT.0) MODETRP(2,7,3,1)=MODETRP(2,7,3,1)+TURB1
      MODETRP(2,1,4,1)=MODETRP(2,1,4,1)+TURB2*FLOAT(MODEINC(2,1,4,1))
      MODETRP(2,2,4,1)=MODETRP(2,2,4,1)+TURB2*FLOAT(MODEINC(2,2,4,1))
      MODETRP(2,3,4,1)=MODETRP(2,3,4,1)+TURB2*FLOAT(MODEINC(2,3,4,1))
      MODETRP(2,4,4,1)=MODETRP(2,4,4,1)+TURB2*FLOAT(MODEINC(2,4,4,1))
      MODETRP(2,5,4,1)=MODETRP(2,5,4,1)+TURB2*FLOAT(MODEINC(2,5,4,1))
      II=MODEINC(2,1,4,1)+MODEINC(2,2,4,1)+MODEINC(2,3,4,1)+
     *   MODEINC(2,4,4,1)+MODEINC(2,5,4,1)
      IF(II.GT.0) MODETRP(2,7,4,1)=MODETRP(2,7,4,1)+TURB2
C..BRT
      MODETRP(5,1,1,1)=MODETRP(5,1,1,1)+TBRTW*FLOAT(MODEINC(5,1,1,1))
      MODETRP(5,2,1,1)=MODETRP(5,2,1,1)+TBRTW*FLOAT(MODEINC(5,2,1,1))
      MODETRP(5,3,1,1)=MODETRP(5,3,1,1)+TBRTW*FLOAT(MODEINC(5,3,1,1))
      II=MODEINC(5,1,1,1)+MODEINC(5,2,1,1)+MODEINC(5,3,1,1)
      IF(II.GT.0) MODETRP(5,7,1,1)=MODETRP(5,7,1,1)+TBRTW
      MODETRP(5,1,3,2)=MODETRP(5,1,3,2)+TBRTD*FLOAT(MODEINC(5,1,3,2))
      MODETRP(5,2,3,2)=MODETRP(5,2,3,2)+TBRTD*FLOAT(MODEINC(5,2,3,2))
      MODETRP(5,3,3,2)=MODETRP(5,3,3,2)+TBRTD*FLOAT(MODEINC(5,3,3,2))
      II=MODEINC(5,1,3,2)+MODEINC(5,2,3,2)+MODEINC(5,3,3,2)
      IF(II.GT.0) MODETRP(5,7,3,2)=MODETRP(5,7,3,2)+TBRTD
C..METRO RAPID TRANSIT
      MODETRP(6,1,1,1)=MODETRP(6,1,1,1)+TRPDW*FLOAT(MODEINC(6,1,1,1))
      MODETRP(6,1,1,2)=MODETRP(6,1,1,2)+TRPDD*FLOAT(MODEINC(6,1,1,2))
C..COMMUTER RAIL STATION SUMMARY MATRIX
      STASUM((OSTA(1,1)-MAX_IZONES),1)=STASUM((OSTA(1,1)-MAX_IZONES),1)+
     *                                 TCRW1
      STASUM((OSTA(1,2)-MAX_IZONES),1)=STASUM((OSTA(1,2)-MAX_IZONES),1)+
     *                                 TCRW2
      STASUM((OSTA(1,3)-MAX_IZONES),2)=STASUM((OSTA(1,3)-MAX_IZONES),2)+
     *                                 TCRB1
      STASUM((OSTA(1,4)-MAX_IZONES),2)=STASUM((OSTA(1,4)-MAX_IZONES),2)+
     *                                 TCRB2
      STASUM((OSTA(1,5)-MAX_IZONES),3)=STASUM((OSTA(1,5)-MAX_IZONES),3)+
     *                                 TCRP1
      STASUM((OSTA(1,6)-MAX_IZONES),3)=STASUM((OSTA(1,6)-MAX_IZONES),3)+
     *                                 TCRP2
      STASUM((OSTA(1,7)-MAX_IZONES),3)=STASUM((OSTA(1,7)-MAX_IZONES),3)+
     *                                 TCRP3
      STASUM((OSTA(1,8)-MAX_IZONES),3)=STASUM((OSTA(1,8)-MAX_IZONES),3)+
     *                                 TCRP4
      STASUM((OSTA(1,9)-MAX_IZONES),4)=STASUM((OSTA(1,9)-MAX_IZONES),4)+
     *                                 TCRK1
      STASUM((OSTA(1,10)-MAX_IZONES),4)=
     *                    STASUM((OSTA(1,10)-MAX_IZONES),4)+TCRK2
      STASUM((OSTA(1,11)-MAX_IZONES),4)=
     *                    STASUM((OSTA(1,11)-MAX_IZONES),4)+TCRK3
      STASUM((OSTA(1,12)-MAX_IZONES),4)=
     *                    STASUM((OSTA(1,12)-MAX_IZONES),4)+TCRK4
      IF(EXPCR) THEN
      IC=CDSTAE-MAX_IZONES
      IF(IC.GT.0) STASUM(IC,2)=STASUM(IC,2)+TEXPD
      EXPCRT(C)=EXPCRT(C)+TEXPD
      END IF
C..COMMUTER RAIL STATION EGRESS SUMMARY MATRIX
      STASUM2((ASTA(1,1)-MAX_IZONES),ASTA2(1,1))=
     *   STASUM2((ASTA(1,1)-MAX_IZONES),ASTA2(1,1))+TCRW1
      STASUM2((ASTA(1,2)-MAX_IZONES),ASTA2(1,2))=
     *   STASUM2((ASTA(1,2)-MAX_IZONES),ASTA2(1,2))+TCRW2
      STASUM2((ASTA(1,3)-MAX_IZONES),ASTA2(1,3))=
     *   STASUM2((ASTA(1,3)-MAX_IZONES),ASTA2(1,3))+TCRB1
      STASUM2((ASTA(1,4)-MAX_IZONES),ASTA2(1,4))=
     *   STASUM2((ASTA(1,4)-MAX_IZONES),ASTA2(1,4))+TCRB2
      STASUM2((ASTA(1,5)-MAX_IZONES),ASTA2(1,5))=
     *   STASUM2((ASTA(1,5)-MAX_IZONES),ASTA2(1,5))+TCRP1
      STASUM2((ASTA(1,6)-MAX_IZONES),ASTA2(1,6))=
     *   STASUM2((ASTA(1,6)-MAX_IZONES),ASTA2(1,6))+TCRP2
      STASUM2((ASTA(1,7)-MAX_IZONES),ASTA2(1,7))=
     *   STASUM2((ASTA(1,7)-MAX_IZONES),ASTA2(1,7))+TCRP3
      STASUM2((ASTA(1,8)-MAX_IZONES),ASTA2(1,8))=
     *   STASUM2((ASTA(1,8)-MAX_IZONES),ASTA2(1,8))+TCRP4
      STASUM2((ASTA(1,9)-MAX_IZONES),ASTA2(1,9))=
     *   STASUM2((ASTA(1,9)-MAX_IZONES),ASTA2(1,9))+TCRK1
      STASUM2((ASTA(1,10)-MAX_IZONES),ASTA2(1,10))=
     *   STASUM2((ASTA(1,10)-MAX_IZONES),ASTA2(1,10))+TCRK2
      STASUM2((ASTA(1,11)-MAX_IZONES),ASTA2(1,11))=
     *   STASUM2((ASTA(1,11)-MAX_IZONES),ASTA2(1,11))+TCRK3
      STASUM2((ASTA(1,12)-MAX_IZONES),ASTA2(1,12))=
     *   STASUM2((ASTA(1,12)-MAX_IZONES),ASTA2(1,12))+TCRK4
      IF(EXPCR) THEN
      IC=CCSTAE-MAX_IZONES
      IF(IC.GT.0) IC=STAIND(IC,JZ)
      IC2=CCSTAE-MAX_IZONES
      IF(IC2.GT.0) STASUM(IC2,IC)=STASUM(IC2,IC)+TEXPD
      END IF
C..USER BENEFIT VALUES -- URBAN RAIL
      DO 7029 K=1,12
      USERBENC(K,1,JZ)=OSTA(2,K)
      USERBENC(K,2,JZ)=ASTA(2,K)
      USERBENC(K,3,JZ)=
     *    STAINDC((OSTA(2,K)-MAX_IZONES),(ASTA(2,K)-MAX_IZONES))
 7029 CONTINUE
      USERBENT(1,C,JZ)=USERBENT(1,C,JZ)+TURW1
      USERBENT(2,C,JZ)=USERBENT(2,C,JZ)+TURW2
      USERBENT(3,C,JZ)=USERBENT(3,C,JZ)+TURB1
      USERBENT(4,C,JZ)=USERBENT(4,C,JZ)+TURB2
      USERBENT(5,C,JZ)=USERBENT(5,C,JZ)+TURP1
      USERBENT(6,C,JZ)=USERBENT(6,C,JZ)+TURP2
      USERBENT(7,C,JZ)=USERBENT(7,C,JZ)+TURP3
      USERBENT(8,C,JZ)=USERBENT(8,C,JZ)+TURP4
      USERBENT(9,C,JZ)=USERBENT(9,C,JZ)+TURK1
      USERBENT(10,C,JZ)=USERBENT(10,C,JZ)+TURK2
      USERBENT(11,C,JZ)=USERBENT(11,C,JZ)+TURK3
      USERBENT(12,C,JZ)=USERBENT(12,C,JZ)+TURK4
C..USER BENEFIT VALUES -- COMMUTER RAIL
      DO 7129 K=1,12
      USERBENC2(K,1,JZ)=OSTA(1,K)
      USERBENC2(K,2,JZ)=ASTA(1,K)
      USERBENC2(K,3,JZ)=
     *    STAINDC2(IZ,(OSTA(1,K)-MAX_IZONES)) +
     *    STAINDC3((ASTA(1,K)-MAX_IZONES),JZ)
 7129 CONTINUE
      USERBENT2(1,C,JZ)=USERBENT2(1,C,JZ)+TCRW1
      USERBENT2(2,C,JZ)=USERBENT2(2,C,JZ)+TCRW2
      USERBENT2(3,C,JZ)=USERBENT2(3,C,JZ)+TCRB1
      USERBENT2(4,C,JZ)=USERBENT2(4,C,JZ)+TCRB2
      USERBENT2(5,C,JZ)=USERBENT2(5,C,JZ)+TCRP1
      USERBENT2(6,C,JZ)=USERBENT2(6,C,JZ)+TCRP2
      USERBENT2(7,C,JZ)=USERBENT2(7,C,JZ)+TCRP3
      USERBENT2(8,C,JZ)=USERBENT2(8,C,JZ)+TCRP4
      USERBENT2(9,C,JZ)=USERBENT2(9,C,JZ)+TCRK1
      USERBENT2(10,C,JZ)=USERBENT2(10,C,JZ)+TCRK2
      USERBENT2(11,C,JZ)=USERBENT2(11,C,JZ)+TCRK3
      USERBENT2(12,C,JZ)=USERBENT2(12,C,JZ)+TCRK4
C..USER BENEFIT VALUES -- DRIVE TO BRT TO URBAN RAIL
      IF(BRTUR) THEN
      USERBENC3(1,JZ)=CDSTABRT
      USERBENC3(2,JZ)=CBSTAE
      USERBENC3(3,JZ)=
     *    STAINDC((CDSTABRT-MAX_IZONES),(CDSTAE-MAX_IZONES))
      USERBENT3(C,JZ)=USERBENT3(C,JZ)+TBRTD 
      END IF     
C..URBAN RAIL STATION SUMMARY MATRIX
      STASUM((OSTA(2,1)-MAX_IZONES),6)=
     *                    STASUM((OSTA(2,1)-MAX_IZONES),6)+TURW1
      STASUM((OSTA(2,2)-MAX_IZONES),6)=
     *                    STASUM((OSTA(2,2)-MAX_IZONES),6)+TURW2
      STASUM((OSTA(2,3)-MAX_IZONES),7)=
     *                    STASUM((OSTA(2,3)-MAX_IZONES),7)+TURB1
      STASUM((OSTA(2,4)-MAX_IZONES),7)=
     *                    STASUM((OSTA(2,4)-MAX_IZONES),7)+TURB2
      STASUM((OSTA(2,5)-MAX_IZONES),8)=
     *                    STASUM((OSTA(2,5)-MAX_IZONES),8)+TURP1
      STASUM((OSTA(2,6)-MAX_IZONES),8)=
     *                    STASUM((OSTA(2,6)-MAX_IZONES),8)+TURP2
      STASUM((OSTA(2,7)-MAX_IZONES),8)=
     *                    STASUM((OSTA(2,7)-MAX_IZONES),8)+TURP3
      STASUM((OSTA(2,8)-MAX_IZONES),8)=
     *                    STASUM((OSTA(2,8)-MAX_IZONES),8)+TURP4
      STASUM((OSTA(2,9)-MAX_IZONES),9)=
     *                    STASUM((OSTA(2,9)-MAX_IZONES),9)+TURK1
      STASUM((OSTA(2,10)-MAX_IZONES),9)=
     *                    STASUM((OSTA(2,10)-MAX_IZONES),9)+TURK2
      STASUM((OSTA(2,11)-MAX_IZONES),9)=
     *                    STASUM((OSTA(2,11)-MAX_IZONES),9)+TURK3
      STASUM((OSTA(2,12)-MAX_IZONES),9)=
     *                    STASUM((OSTA(2,12)-MAX_IZONES),9)+TURK4
C..URBAN RAIL STATION EGRESS SUMMARY MATRIX
      STASUM2((ASTA(2,1)-MAX_IZONES),ASTA2(2,1))=
     *   STASUM2((ASTA(2,1)-MAX_IZONES),ASTA2(2,1))+TURW1
      STASUM2((ASTA(2,2)-MAX_IZONES),ASTA2(2,2))=
     *   STASUM2((ASTA(2,2)-MAX_IZONES),ASTA2(2,2))+TURW2
      STASUM2((ASTA(2,3)-MAX_IZONES),ASTA2(2,3))=
     *   STASUM2((ASTA(2,3)-MAX_IZONES),ASTA2(2,3))+TURB1
      STASUM2((ASTA(2,4)-MAX_IZONES),ASTA2(2,4))=
     *   STASUM2((ASTA(2,4)-MAX_IZONES),ASTA2(2,4))+TURB2
      STASUM2((ASTA(2,5)-MAX_IZONES),ASTA2(2,5))=
     *   STASUM2((ASTA(2,5)-MAX_IZONES),ASTA2(2,5))+TURP1
      STASUM2((ASTA(2,6)-MAX_IZONES),ASTA2(2,6))=
     *   STASUM2((ASTA(2,6)-MAX_IZONES),ASTA2(2,6))+TURP2
      STASUM2((ASTA(2,7)-MAX_IZONES),ASTA2(2,7))=
     *   STASUM2((ASTA(2,7)-MAX_IZONES),ASTA2(2,7))+TURP3
      STASUM2((ASTA(2,8)-MAX_IZONES),ASTA2(2,8))=
     *   STASUM2((ASTA(2,8)-MAX_IZONES),ASTA2(2,8))+TURP4
      STASUM2((ASTA(2,9)-MAX_IZONES),ASTA2(2,9))=
     *   STASUM2((ASTA(2,9)-MAX_IZONES),ASTA2(2,9))+TURK1
      STASUM2((ASTA(2,10)-MAX_IZONES),ASTA2(2,10))=
     *   STASUM2((ASTA(2,10)-MAX_IZONES),ASTA2(2,10))+TURK2
      STASUM2((ASTA(2,11)-MAX_IZONES),ASTA2(2,11))=
     *   STASUM2((ASTA(2,11)-MAX_IZONES),ASTA2(2,11))+TURK3
      STASUM2((ASTA(2,12)-MAX_IZONES),ASTA2(2,12))=
     *   STASUM2((ASTA(2,12)-MAX_IZONES),ASTA2(2,12))+TURK4
C..TRANSITWAY STATION SUMMARY
      IF(CWTWY.GT.0.AND.CWTWY.LT.MAX_ZONES) THEN
      STASUM((CWTWY-MAX_IZONES),11)=
     *                    STASUM((CWTWY-MAX_IZONES),11) + TWAYW
      END IF
      IF(CSTAT.GT.0.AND.CSTAT.LT.MAX_ZONES) THEN
      STASUM((CSTAT-MAX_IZONES),12)=
     *                    STASUM((CSTAT-MAX_IZONES),12) + TWAYD
      END IF
C..EXPRESS BUS STATION SUMMARY
      IF(CWEXP.GT.0.AND.CWEXP.LT.MAX_ZONES) THEN
      STASUM((CWEXP-MAX_IZONES),13)=
     *                    STASUM((CWEXP-MAX_IZONES),13) + TEXPW
      END IF
      IF(CSTAE.GT.0.AND.CSTAE.LT.MAX_ZONES) THEN
      STASUM((CSTAE-MAX_IZONES),14)=
     *                    STASUM((CSTAE-MAX_IZONES),14) + TEXPD
      END IF
C..BUS RAPID TRANSIT (BRT) SUMMARY
      IF(CWBRT.GT.0.AND.CWBRT.LT.MAX_ZONES) THEN
      STASUM((CWBRT-MAX_IZONES),17)=
     *                    STASUM((CWBRT-MAX_IZONES),17) + TBRTW
      ELSE
      TBRTT=IDINT(TBRTW*100000.0)
      IF(TBRTW.GT.0) WRITE(43,9999) IZ,JZ,M,TBRTT
 9999 FORMAT(2I5,I3,I7)
      END IF
      IF(CSTABRT.GT.0.AND.CSTABRT.LT.MAX_ZONES) THEN
      STASUM((CSTABRT-MAX_IZONES),18)=
     *                    STASUM((CSTABRT-MAX_IZONES),18) + TBRTD
      END IF
C-----------------------------------------------------------------------
C..SAVE WALK ACCESS FREQUENCY DISTRIBUTION DATA - COMMUTER RAIL
      K=IFIX(WDIST(1,1)*10.0)
      K=MIN0(K,20)
      K=MAX0(K,1)
      IF(TCRW1.GT.0) THEN
      WLKPER(K,1)=WLKPER(K,1)+TCRW1
      WLKPER(K,2)=WLKPER(K,2)+PERTRP
      END IF
      K=IFIX(WDIST(1,2)*10.0)
      K=MIN0(K,20)
      K=MAX0(K,1)
      IF(TCRW2.GT.0) THEN
      WLKPER(K,3)=WLKPER(K,3)+TCRW2
      WLKPER(K,4)=WLKPER(K,4)+PERTRP
      END IF
C
C..SAVE WALK ACCESS FREQUENCY DISTRIBUTION DATA - URBAN RAIL
      K=IFIX(WDIST(2,1)*10.0)
      K=MIN0(K,20)
      K=MAX0(K,1)
      IF(TURW1.GT.0) THEN
      WLKPER(K,5)=WLKPER(K,5)+TURW1
      WLKPER(K,6)=WLKPER(K,6)+PERTRP
      END IF
      K=IFIX(WDIST(2,2)*10.0)
      K=MIN0(K,20)
      K=MAX0(K,1)
      IF(TURW2.GT.0) THEN
      WLKPER(K,7)=WLKPER(K,7)+TURW2
      WLKPER(K,8)=WLKPER(K,8)+PERTRP
      END IF
C
C
C..SAVE TRIP LENGTH (DISTANCE) DISTRIBUTION DATA - COMMUTER RAIL TRIPS
C
C  CURRENTLY USING MAXIMUM DISTANCE OF 100 MILES - MAY BE ABLE
C  TO REDUCE THIS
C
C   USE AUTO DISTANCE
C
      K=IFIX(TAB2DA(JZ))
      K=MAX0(K,1)
      K=MIN0(K,50)
      ZNEDIST(1,K)=ZNEDIST(1,K) + TCRW1 + TCRW2
      ZNEDIST(2,K)=ZNEDIST(2,K) + TCRB1 + TCRB2
      ZNEDIST(3,K)=ZNEDIST(3,K) + TCRP1+TCRP2+TCRP3+TCRP4
      ZNEDIST(4,K)=ZNEDIST(4,K) + TCRK1+TCRK2+TCRK3+TCRK4
C URBAN RAIL TRIPS
      ZNEDIST(5,K)=ZNEDIST(5,K) + TURW1 + TURW2
      ZNEDIST(6,K)=ZNEDIST(6,K) + TURB1 + TURB2
      ZNEDIST(7,K)=ZNEDIST(7,K) + TURP1+TURP2+TURP3+TURP4
      ZNEDIST(8,K)=ZNEDIST(8,K) + TURK1+TURK2+TURK3+TURK4
C
C SUMMARIZE TRANSIT TRIPS BY INCOME AND TRIP LENGTH
C
      TINDEX=IFIX(TAB2DA(JZ)/5.0)+1
      IF(TINDEX.GT.21) TINDEX=21
      TTRIP(TINDEX,C)=TTRIP(TINDEX,C)+TTRAN
      TTRIP2(TINDEX,C,1)=TTRIP2(TINDEX,C,1)+TCR
      TTRIP2(TINDEX,C,2)=TTRIP2(TINDEX,C,2)+TUR
      TTRIP2(TINDEX,C,3)=TTRIP2(TINDEX,C,3)+TWAY 
      TTRIP2(TINDEX,C,4)=TTRIP2(TINDEX,C,4)+TEXP
      TTRIP2(TINDEX,C,5)=TTRIP2(TINDEX,C,5)+TLOC 
      TTRIP2(TINDEX,C,6)=TTRIP2(TINDEX,C,6)+TRPD
      TTRIP2(TINDEX,C,7)=TTRIP2(TINDEX,C,7)+TBRT
C
C..SAVE STATION-TO-STATION TRIP LENGTH (DISTANCE) DISTRIBUTION DATA
C
C..COMMUTER RAIL
      ADIST=SSDIST((ASTA(1,1)-MAX_IZONES),(OSTA(1,1)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(1,K)=STADIST(1,K) + TCRW1
      ADIST=SSDIST((ASTA(1,2)-MAX_IZONES),(OSTA(1,2)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(1,K)=STADIST(1,K) + TCRW2
      ADIST=SSDIST((ASTA(1,3)-MAX_IZONES),(OSTA(1,3)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(2,K)=STADIST(2,K) + TCRB1
      ADIST=SSDIST((ASTA(1,4)-MAX_IZONES),(OSTA(1,4)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(2,K)=STADIST(2,K) + TCRB2
      ADIST=SSDIST((ASTA(1,5)-MAX_IZONES),(OSTA(1,5)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(3,K)=STADIST(3,K) + TCRP1
      ADIST=SSDIST((ASTA(1,6)-MAX_IZONES),(OSTA(1,6)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(3,K)=STADIST(3,K) + TCRP2
      ADIST=SSDIST((ASTA(1,7)-MAX_IZONES),(OSTA(1,7)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(3,K)=STADIST(3,K) + TCRP3
      ADIST=SSDIST((ASTA(1,8)-MAX_IZONES),(OSTA(1,8)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(3,K)=STADIST(3,K) + TCRP4
      ADIST=SSDIST((ASTA(1,9)-MAX_IZONES),(OSTA(1,9)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(4,K)=STADIST(4,K) + TCRK1
      ADIST=SSDIST((ASTA(1,10)-MAX_IZONES),(OSTA(1,10)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(4,K)=STADIST(4,K) + TCRK2
      ADIST=SSDIST((ASTA(1,11)-MAX_IZONES),(OSTA(1,11)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(4,K)=STADIST(4,K) + TCRK3
      ADIST=SSDIST((ASTA(1,12)-MAX_IZONES),(OSTA(1,12)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(4,K)=STADIST(4,K) + TCRK4
C..URBAN RAIL
      ADIST=SSDIST((ASTA(2,1)-MAX_IZONES),(OSTA(2,1)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(5,K)=STADIST(5,K) + TURW1
      ADIST=SSDIST((ASTA(2,2)-MAX_IZONES),(OSTA(2,2)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(5,K)=STADIST(5,K) + TURW2
      ADIST=SSDIST((ASTA(2,3)-MAX_IZONES),(OSTA(2,3)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(6,K)=STADIST(6,K) + TURB1
      ADIST=SSDIST((ASTA(2,4)-MAX_IZONES),(OSTA(2,4)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(6,K)=STADIST(6,K) + TURB2
      ADIST=SSDIST((ASTA(2,5)-MAX_IZONES),(OSTA(2,5)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(7,K)=STADIST(7,K) + TURP1
      ADIST=SSDIST((ASTA(2,6)-MAX_IZONES),(OSTA(2,6)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(7,K)=STADIST(7,K) + TURP2
      ADIST=SSDIST((ASTA(2,7)-MAX_IZONES),(OSTA(2,7)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(7,K)=STADIST(7,K) + TURP3
      ADIST=SSDIST((ASTA(2,8)-MAX_IZONES),(OSTA(2,8)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(7,K)=STADIST(7,K) + TURP4
      ADIST=SSDIST((ASTA(2,9)-MAX_IZONES),(OSTA(2,9)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(8,K)=STADIST(8,K) + TURK1
      ADIST=SSDIST((ASTA(2,10)-MAX_IZONES),(OSTA(2,10)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(8,K)=STADIST(8,K) + TURK2
      ADIST=SSDIST((ASTA(2,11)-MAX_IZONES),(OSTA(2,11)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(8,K)=STADIST(8,K) + TURK3
      ADIST=SSDIST((ASTA(2,12)-MAX_IZONES),(OSTA(2,12)-MAX_IZONES))
      K=IFIX(ADIST)
      K=MAX0(K,1)
      K=MIN0(K,50)
      STADIST(8,K)=STADIST(8,K) + TURK4
C
C..SUMMARIZE COMMUTER RAIL PNR TRIPS BY
C  RATIO OF DRIVE ACCESS IVT/CR IVT
C
      KL=IFIX(CRPNR(1)*10.0)
      IF(KL.GT.0.AND.KL.LT.1000) 
     * CRPNRSUM(KL)=CRPNRSUM(KL)+TCRP1
      KL=IFIX(CRPNR(2)*10.0)
      IF(KL.GT.0.AND.KL.LT.1000) 
     * CRPNRSUM(KL)=CRPNRSUM(KL)+TCRP2
      KL=IFIX(CRPNR(3)*10.0)
      IF(KL.GT.0.AND.KL.LT.1000) 
     * CRPNRSUM(KL)=CRPNRSUM(KL)+TCRP3
      KL=IFIX(CRPNR(4)*10.0)
      IF(KL.GT.0.AND.KL.LT.1000) 
     * CRPNRSUM(KL)=CRPNRSUM(KL)+TCRP4
C
C..SUMMARIZE COMMUTER RAIL PNR TRIPS BY
C  RATIO OF DRIVE ACCESS DISTANCE/TOTAL DISTANCE
C
      KL=IFIX(CRPNR2(1)*10.0)
      IF(KL.GT.0.AND.KL.LT.1000) 
     * CRPNRRAT(KL)=CRPNRRAT(KL)+TCRP1
      KL=IFIX(CRPNR2(2)*10.0)
      IF(KL.GT.0.AND.KL.LT.1000) 
     * CRPNRRAT(KL)=CRPNRRAT(KL)+TCRP2
      KL=IFIX(CRPNR2(3)*10.0)
      IF(KL.GT.0.AND.KL.LT.1000) 
     * CRPNRRAT(KL)=CRPNRRAT(KL)+TCRP3
      KL=IFIX(CRPNR2(4)*10.0)
      IF(KL.GT.0.AND.KL.LT.1000) 
     * CRPNRRAT(KL)=CRPNRRAT(KL)+TCRP4
C
C..SUMMARIZE DRIVE TO EXPRESS BUS, TRANSITWAY & BRT 
C  ZONE TO STATION DISTANCE & TIME
C
      IF(CSTAE.GT.0) THEN
      K=IFIX(TAB2DA(CSTAE))+1
      K=MAX0(K,1)
      K=MIN0(K,100)
      TWYEXP(K,C,1,1)=TWYEXP(K,C,1,1)+TEXPD
      K=IFIX(TAB1DA(CSTAE))+1
      K=MAX0(K,1)
      K=MIN0(K,100)
      TWYEXP(K,C,2,1)=TWYEXP(K,C,2,1)+TEXPD
      END IF
      IF(CSTAT.GT.0) THEN
      K=IFIX(TAB2DA(CSTAT))+1
      K=MAX0(K,1)
      K=MIN0(K,100)
      TWYEXP(K,C,1,2)=TWYEXP(K,C,1,2)+TWAYD
      K=IFIX(TAB1DA(CSTAT))+1
      K=MAX0(K,1)
      K=MIN0(K,100)
      TWYEXP(K,C,2,2)=TWYEXP(K,C,2,2)+TWAYD
      END IF
      IF(CSTABRT.GT.0) THEN
      K=IFIX(TAB2DA(CSTABRT))+1
      K=MAX0(K,1)
      K=MIN0(K,100)
      TWYEXP(K,C,1,3)=TWYEXP(K,C,1,3)+TBRTD
      K=IFIX(TAB1DA(CSTABRT))+1
      K=MAX0(K,1)
      K=MIN0(K,100)
      TWYEXP(K,C,2,3)=TWYEXP(K,C,2,3)+TBRTD
      END IF
C
C..SUMMARIZE DRIVE TO EXPRESS BUS, TRANSIWAY & BRT
C  DRIVE ACCESS RATIO
C
      IF(CSTAE.GT.0.AND.TAB2DA(JZ).GT.0) THEN
      K=IFIX((TAB2DA(CSTAE)/TAB2DA(JZ))*10.0)
      K=MAX0(K,1)
      K=MIN0(K,100)
      TWYEXP2(K,C,1)=TWYEXP2(K,C,1)+TEXPD
      END IF
      IF(CSTAT.GT.0.AND.TAB2DA(JZ).GT.0) THEN
      K=IFIX((TAB2DA(CSTAT)/TAB2DA(JZ))*10.0)
      K=MAX0(K,1)
      K=MIN0(K,100)
      TWYEXP2(K,C,2)=TWYEXP2(K,C,2)+TWAYD
      END IF
      IF(CSTABRT.GT.0.AND.TAB2DA(JZ).GT.0) THEN
      K=IFIX((TAB2DA(CSTABRT)/TAB2DA(JZ))*10.0)
      K=MAX0(K,1)
      K=MIN0(K,100)
      TWYEXP2(K,C,3)=TWYEXP2(K,C,3)+TBRTD
      END IF
C...................................................
      IF(DEBUG) THEN
      WRITE(26,9555) CRPNR(1),TCRP1,CRPNR(2),TCRP2,
     *               CRPNR(3),TCRP3,CRPNR(4),TCRP4,
     *               CRPNR2(1),TCRP1,CRPNR2(2),TCRP2,
     *               CRPNR2(3),TCRP3,CRPNR2(4),TCRP4
 9555 FORMAT(/,1X,'SUMMARY OF CR DRIVE IVT/CR IVT RESULTS'/
     *         1X,'--------------------------------------'/
     *         3X,'CRPNR(1)=',F7.5,' TCRP1=',F8.5/
     *         3X,'CRPNR(2)=',F7.5,' TCRP2=',F8.5/
     *         3X,'CRPNR(3)=',F7.5,' TCRP3=',F8.5/
     *         3X,'CRPNR(4)=',F7.5,' TCRP4=',F8.5/
     *         1X,'SUMMARY OF CR DRIVE DST/TOTAL DIST RESULTS'/
     *         1X,'--------------------------------------'/
     *         3X,'CRPNR2(1)=',F7.5,' TCRP1=',F8.5/
     *         3X,'CRPNR2(2)=',F7.5,' TCRP2=',F8.5/
     *         3X,'CRPNR2(3)=',F7.5,' TCRP3=',F8.5/
     *         3X,'CRPNR2(4)=',F7.5,' TCRP4=',F8.5/)
      END IF
C...........................................................
C
C
C..FILL OUTPUT TRIPS MATRIX
      IF(INCLEV.GT.0.AND.INCLEV.NE.C) GO TO 255
c...origin zone to origin station - FOR DRIVE CAN COMBINE MODES
c...bus to commuter rail
      orista=osta(1,3)-MAX_IZONES
      bcr(iz,orista)=bcr(iz,orista) + sngl(tcrb1)
      if(railsel) call tselect(2,iz,jz,orista,dessta,tcrb1,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      orista=osta(1,4)-MAX_IZONES
      bcr(iz,orista)=bcr(iz,orista) + sngl(tcrb2)
      if(railsel) call tselect(2,iz,jz,orista,dessta,tcrb2,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
c...bus to urban rail
      orista=osta(2,3)-MAX_IZONES
      bur(iz,orista)=bur(iz,orista) + sngl(turb1)
      orista=osta(2,4)-MAX_IZONES
      bur(iz,orista)=bur(iz,orista) + sngl(turb2)
c... drive to commuter rail
      orista=osta(1,5)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrp1)
      orista=osta(1,6)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrp2)
      orista=osta(1,7)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrp3)
      orista=osta(1,8)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrp4)
      orista=osta(1,9)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrk1)
      orista=osta(1,10)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrk2)
      orista=osta(1,11)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrk3)
      orista=osta(1,12)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrk4)
c....drive to urban rail
      orista=osta(2,5)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turp1)
      orista=osta(2,6)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turp2)
      orista=osta(2,7)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turp3)
      orista=osta(2,8)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turp4)
      orista=osta(2,9)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turk1)
      orista=osta(2,10)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turk2)
      orista=osta(2,11)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turk3)
      orista=osta(2,12)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turk4)
c...drive to express bus
      orista=cstae-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(texpd)
c...drive to transitway  bus
      orista=cstat-MAX_IZONES
      if(orista.gt.0) then
      dtran(iz,orista)=dtran(iz,orista) + sngl(twayd)
      end if
c...drive to bus rapid transit (brt)
      orista=cstabrt-MAX_IZONES
      if(orista.gt.0) then
      dtran(iz,orista)=dtran(iz,orista) + sngl(tbrtd)
      end if
c
c...Station-to-Station tables
c
c...commuter rail
      orista=osta(1,1)-MAX_IZONES
      dessta=asta(1,1)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrw1)
      orista=osta(1,2)-MAX_IZONES
      dessta=asta(1,2)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrw2)
      orista=osta(1,3)-MAX_IZONES
      dessta=asta(1,3)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrb1)
      orista=osta(1,4)-MAX_IZONES
      dessta=asta(1,4)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrb2)
      orista=osta(1,5)-MAX_IZONES
      dessta=asta(1,5)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrp1)
      orista=osta(1,6)-MAX_IZONES
      dessta=asta(1,6)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrp2)
      orista=osta(1,7)-MAX_IZONES
      dessta=asta(1,7)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrp3)
      orista=osta(1,8)-MAX_IZONES
      dessta=asta(1,8)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrp4)
      orista=osta(1,9)-MAX_IZONES
      dessta=asta(1,9)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrk1)
      orista=osta(1,10)-MAX_IZONES
      dessta=asta(1,10)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrk2)
      orista=osta(1,11)-MAX_IZONES
      dessta=asta(1,11)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrk3)
      orista=osta(1,12)-MAX_IZONES
      dessta=asta(1,12)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrk4)
      if(expcr) then
      orista=cdstae-MAX_IZONES
      dessta=ccstae-MAX_IZONES
      if(orista.gt.0.and.dessta.gt.0) then
      crss(orista,dessta)=crss(orista,dessta) + sngl(TEXPD)
      end if
      end if
c...urban rail
      orista=osta(2,1)-MAX_IZONES
      dessta=asta(2,1)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turw1)
      if(railsel) call tselect(1,iz,jz,orista,dessta,turw1,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      orista=osta(2,2)-MAX_IZONES
      dessta=asta(2,2)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turw2)
      if(railsel) call tselect(1,iz,jz,orista,dessta,turw2,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      orista=osta(2,3)-MAX_IZONES
      dessta=asta(2,3)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turb1)
      if(railsel) call tselect(1,iz,jz,orista,dessta,turb1,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      orista=osta(2,4)-MAX_IZONES
      dessta=asta(2,4)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turb2)
      if(railsel) call tselect(1,iz,jz,orista,dessta,turb2,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      orista=osta(2,5)-MAX_IZONES
      dessta=asta(2,5)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turp1)
      if(railsel) call tselect(1,iz,jz,orista,dessta,turp1,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      orista=osta(2,6)-MAX_IZONES
      dessta=asta(2,6)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turp2)
      if(railsel) call tselect(1,iz,jz,orista,dessta,turp2,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      orista=osta(2,7)-MAX_IZONES
      dessta=asta(2,7)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turp3)
      if(railsel) call tselect(1,iz,jz,orista,dessta,turp3,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      orista=osta(2,8)-MAX_IZONES
      dessta=asta(2,8)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turp4)
      if(railsel) call tselect(1,iz,jz,orista,dessta,turp4,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      orista=osta(2,9)-MAX_IZONES
      dessta=asta(2,9)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turk1)
      if(railsel) call tselect(1,iz,jz,orista,dessta,turk1,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      orista=osta(2,10)-MAX_IZONES
      dessta=asta(2,10)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turk2)
      if(railsel) call tselect(1,iz,jz,orista,dessta,turk2,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      orista=osta(2,11)-MAX_IZONES
      dessta=asta(2,11)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turk3)
      if(railsel) call tselect(1,iz,jz,orista,dessta,turk3,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      orista=osta(2,12)-MAX_IZONES
      dessta=asta(2,12)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turk4)
      if(railsel) call tselect(1,iz,jz,orista,dessta,turk4,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
c
c...brt to urban rail	
      if(brtur) then
      orista=cdstabrt-MAX_IZONES
      dessta=cbstae-MAX_IZONES
       if(orista.le.0.or.dessta.le.0) then
       write(26,9655) iz,jz,cdstabrt,cbstae
 9655  format('  BRT to Urban Rail Station Error'/
     *        '  iz=',i4,' jz=',i4,' Urban Rail Access Station=',i4,
     *        ' Urban Rail Egress Station=',i4)
       stop 8
       end if
      urss(orista,dessta)=urss(orista,dessta) + sngl(tbrtd)
      end if
c
c...Express Bus & Transitway & BRT drive access station to dest. zone
c
      if(.not.expcr) then
      orista=cstae-MAX_IZONES
      ebstaz(orista,jz)=ebstaz(orista,jz) + sngl(texpd)
      end if
      orista=cstat-MAX_IZONES
      if(orista.gt.0) then
      twstaz(orista,jz)=twstaz(orista,jz) + sngl(twayd)
      end if
      if(brtur) then
      orista=cstabrt-MAX_IZONES
      dessta=cdstabrt-MAX_IZONES
      if(orista.le.0.or.dessta.le.0) then
       write(26,9656) iz,jz,cstabrt,cdstabrt
 9656  format('  BRT to Urban Rail Station Error'/
     *        '  iz=',i4,' jz=',i4,' BRT Access Station=',i4,
     *        ' Urban Rail Access Station=',i4)
       stop 8
       end if
       brtstaz(orista,dessta)=brtstaz(orista,dessta) + sngl(tbrtd)
      else
      orista=cstabrt-MAX_IZONES
      if(orista.gt.0) then
      brtstaz(orista,jz)=brtstaz(orista,jz) + sngl(tbrtd)
      end if
      end if
c
c...Destination Station to Destination Zone
c...commuter rail
      dessta=asta(1,1)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrw1)
      if(railsel) call tselect(3,iz,jz,orista,dessta,tcrw1,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      dessta=asta(1,2)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrw2)
      if(railsel) call tselect(3,iz,jz,orista,dessta,tcrw2,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      dessta=asta(1,3)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrb1)
      if(railsel) call tselect(3,iz,jz,orista,dessta,tcrb1,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      dessta=asta(1,4)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrb2)
      if(railsel) call tselect(3,iz,jz,orista,dessta,tcrb2,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      dessta=asta(1,5)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrp1)
      if(railsel) call tselect(3,iz,jz,orista,dessta,tcrp1,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      dessta=asta(1,6)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrp2)
      if(railsel) call tselect(3,iz,jz,orista,dessta,tcrp2,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      dessta=asta(1,7)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrp3)
      if(railsel) call tselect(3,iz,jz,orista,dessta,tcrp3,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      dessta=asta(1,8)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrp4)
      if(railsel) call tselect(3,iz,jz,orista,dessta,tcrp4,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      dessta=asta(1,9)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrk1)
      if(railsel) call tselect(3,iz,jz,orista,dessta,tcrk1,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      dessta=asta(1,10)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrk2)
      if(railsel) call tselect(3,iz,jz,orista,dessta,tcrk2,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      dessta=asta(1,11)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrk3)
      if(railsel) call tselect(3,iz,jz,orista,dessta,tcrk3,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      dessta=asta(1,12)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrk4)
      if(railsel) call tselect(3,iz,jz,orista,dessta,tcrk4,
     *            selind,zstaind,stazind,zstasta,stazsta,
     *            statrips,statrips2,statrips3,seltrips,
     *            zstatrips,zstatrips2,zstatrips3,seldist)
      if(expcr) then
      dessta=ccstae-MAX_IZONES
      if(dessta.gt.0) then
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(TEXPD)
      end if
      end if
c...urban rail
      dessta=asta(2,1)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turw1)
      dessta=asta(2,2)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turw2)
      dessta=asta(2,3)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turb1)
      dessta=asta(2,4)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turb2)
      dessta=asta(2,5)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turp1)
      dessta=asta(2,6)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turp2)
      dessta=asta(2,7)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turp3)
      dessta=asta(2,8)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turp4)
      dessta=asta(2,9)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turk1)
      dessta=asta(2,10)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turk2)
      dessta=asta(2,11)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turk3)
      dessta=asta(2,12)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turk4)
c...brt to urban rail
      if(brtur) then
      dessta=cbstae-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(tbrtd)
      end if
C
      TRIPS( 1,JZ)=TRIPS( 1,JZ)+TDRV0N
      TRIPS( 2,JZ)=TRIPS( 2,JZ)+TDRV0T
      TRIPS( 3,JZ)=TRIPS( 3,JZ)+TDRV2NH
      TRIPS( 4,JZ)=TRIPS( 4,JZ)+TDRV2NN
      TRIPS( 5,JZ)=TRIPS( 5,JZ)+TDRV2TH
      TRIPS( 6,JZ)=TRIPS( 6,JZ)+TDRV2TN
      TRIPS( 7,JZ)=TRIPS( 7,JZ)+TDRV3NH
      TRIPS( 8,JZ)=TRIPS( 8,JZ)+TDRV3NN
      TRIPS( 9,JZ)=TRIPS( 9,JZ)+TDRV3TH
      TRIPS(10,JZ)=TRIPS(10,JZ)+TDRV3TN
      TRIPS(49,JZ)=TRIPS(49,JZ)+TDRV4NH
      TRIPS(50,JZ)=TRIPS(50,JZ)+TDRV4NN
      TRIPS(51,JZ)=TRIPS(51,JZ)+TDRV4TH
      TRIPS(52,JZ)=TRIPS(52,JZ)+TDRV4TN
  255 CONTINUE
      TRIPS(11,JZ)=TRIPS(11,JZ)+TLOCW
      TRIPS(12,JZ)=TRIPS(12,JZ)+TLOCD
      TRIPS(13,JZ)=TRIPS(13,JZ)+TEXPW
      TRIPS(14,JZ)=TRIPS(14,JZ)+TEXPD
      TRIPS(15,JZ)=TRIPS(15,JZ)+TCRW1
      TRIPS(16,JZ)=TRIPS(16,JZ)+TCRW2
      TRIPS(17,JZ)=TRIPS(17,JZ)+TCRB1
      TRIPS(18,JZ)=TRIPS(18,JZ)+TCRB2
      TRIPS(19,JZ)=TRIPS(19,JZ)+TCRP1
      TRIPS(20,JZ)=TRIPS(20,JZ)+TCRP2
      TRIPS(21,JZ)=TRIPS(21,JZ)+TCRP3
      TRIPS(22,JZ)=TRIPS(22,JZ)+TCRP4
      TRIPS(23,JZ)=TRIPS(23,JZ)+TCRK1
      TRIPS(24,JZ)=TRIPS(24,JZ)+TCRK2
      TRIPS(25,JZ)=TRIPS(25,JZ)+TCRK3
      TRIPS(26,JZ)=TRIPS(26,JZ)+TCRK4
      TRIPS(27,JZ)=TRIPS(27,JZ)+TURW1
      TRIPS(28,JZ)=TRIPS(28,JZ)+TURW2
      TRIPS(29,JZ)=TRIPS(29,JZ)+TURB1
      TRIPS(30,JZ)=TRIPS(30,JZ)+TURB2
      TRIPS(31,JZ)=TRIPS(31,JZ)+TURP1
      TRIPS(32,JZ)=TRIPS(32,JZ)+TURP2
      TRIPS(33,JZ)=TRIPS(33,JZ)+TURP3
      TRIPS(34,JZ)=TRIPS(34,JZ)+TURP4
      TRIPS(35,JZ)=TRIPS(35,JZ)+TURK1
      TRIPS(36,JZ)=TRIPS(36,JZ)+TURK2
      TRIPS(37,JZ)=TRIPS(37,JZ)+TURK3
      TRIPS(38,JZ)=TRIPS(38,JZ)+TURK4
      TRIPS(39,JZ)=TRIPS(39,JZ)+TWAYW
      TRIPS(40,JZ)=TRIPS(40,JZ)+TWAYD
      TRIPS(41,JZ)=TRIPS(41,JZ)+TCR
      TRIPS(42,JZ)=TRIPS(42,JZ)+TUR
      TRIPS(43,JZ)=TRIPS(43,JZ)+TWAY
      TRIPS(44,JZ)=TRIPS(44,JZ)+TRPDW
      TRIPS(45,JZ)=TRIPS(45,JZ)+TRPDD
      TRIPS(46,JZ)=TRIPS(46,JZ)+TBRTW
      IF(BRTUR) THEN
      TRIPS(48,JZ)=TRIPS(48,JZ)+TBRTD
      ELSE
      TRIPS(47,JZ)=TRIPS(47,JZ)+TBRTD
      END IF
C
C     SAVE URBAN RAIL TRIPS BY INCOME AND NUMBER OF TRANSFERS
C
      IF(RALTXF(1).LT.0) GO TO 5001
      IF(RALTXF(1).EQ.0) KTX=1
      IF(RALTXF(1).EQ.1) KTX=2
      IF(RALTXF(1).GT.1) KTX=3
      KTY=KTX+9
      KTX=(C-1)*3+KTX
      TXTRIP(KTX,JZ)=TXTRIP(KTX,JZ)+TURW1
      TXSUM(KTX,1)=TXSUM(KTX,1)+TURW1
      TXSUM(KTY,1)=TXSUM(KTY,1)+TURW1
      TXSUM(KTX,5)=TXSUM(KTX,5)+TURW1
      TXSUM(KTY,5)=TXSUM(KTY,5)+TURW1
      TXSUM(13,1)=TXSUM(13,1)+TURW1
      TXSUM(13,5)=TXSUM(13,5)+TURW1
 5001 IF(RALTXF(2).LT.0) GO TO 5002
      IF(RALTXF(2).EQ.0) KTX=1
      IF(RALTXF(2).EQ.1) KTX=2
      IF(RALTXF(2).GT.1) KTX=3
      KTY=KTX+9
      KTX=(C-1)*3+KTX
      TXTRIP(KTX,JZ)=TXTRIP(KTX,JZ)+TURW2
      TXSUM(KTX,1)=TXSUM(KTX,1)+TURW2
      TXSUM(KTY,1)=TXSUM(KTY,1)+TURW2
      TXSUM(KTX,5)=TXSUM(KTX,5)+TURW2
      TXSUM(KTY,5)=TXSUM(KTY,5)+TURW2
      TXSUM(13,1)=TXSUM(13,1)+TURW2
      TXSUM(13,5)=TXSUM(13,5)+TURW2
 5002 IF(RALTXF(3).LT.0) GO TO 5003
      IF(RALTXF(3).EQ.0) KTX=1
      IF(RALTXF(3).EQ.1) KTX=2
      IF(RALTXF(3).GT.1) KTX=3
      KTY=KTX+9
      KTX=(C-1)*3+KTX
      TXTRIP(KTX,JZ)=TXTRIP(KTX,JZ)+TURB1
      TXSUM(KTX,2)=TXSUM(KTX,2)+TURB1
      TXSUM(KTY,2)=TXSUM(KTY,2)+TURB1
      TXSUM(KTX,5)=TXSUM(KTX,5)+TURB1
      TXSUM(KTY,5)=TXSUM(KTY,5)+TURB1
      TXSUM(13,2)=TXSUM(13,2)+TURB1
      TXSUM(13,5)=TXSUM(13,5)+TURB1
 5003 IF(RALTXF(4).LT.0) GO TO 5004
      IF(RALTXF(4).EQ.0) KTX=1
      IF(RALTXF(4).EQ.1) KTX=2
      IF(RALTXF(4).GT.1) KTX=3
      KTY=KTX+9
      KTX=(C-1)*3+KTX
      TXTRIP(KTX,JZ)=TXTRIP(KTX,JZ)+TURB2
      TXSUM(KTX,2)=TXSUM(KTX,2)+TURB2
      TXSUM(KTY,2)=TXSUM(KTY,2)+TURB2
      TXSUM(KTX,5)=TXSUM(KTX,5)+TURB2
      TXSUM(KTY,5)=TXSUM(KTY,5)+TURB2
      TXSUM(13,2)=TXSUM(13,2)+TURB2
      TXSUM(13,5)=TXSUM(13,5)+TURB2
 5004 IF(RALTXF(5).LT.0) GO TO 5005
      IF(RALTXF(5).EQ.0) KTX=1
      IF(RALTXF(5).EQ.1) KTX=2
      IF(RALTXF(5).GT.1) KTX=3
      KTY=KTX+9
      KTX=(C-1)*3+KTX
      TXTRIP(KTX,JZ)=TXTRIP(KTX,JZ)+TURP1
      TXSUM(KTX,3)=TXSUM(KTX,3)+TURP1
      TXSUM(KTY,3)=TXSUM(KTY,3)+TURP1
      TXSUM(KTX,5)=TXSUM(KTX,5)+TURP1
      TXSUM(KTY,5)=TXSUM(KTY,5)+TURP1
      TXSUM(13,3)=TXSUM(13,3)+TURP1
      TXSUM(13,5)=TXSUM(13,5)+TURP1
 5005 IF(RALTXF(6).LT.0) GO TO 5006
      IF(RALTXF(6).EQ.0) KTX=1
      IF(RALTXF(6).EQ.1) KTX=2
      IF(RALTXF(6).GT.1) KTX=3
      KTY=KTX+9
      KTX=(C-1)*3+KTX
      TXTRIP(KTX,JZ)=TXTRIP(KTX,JZ)+TURP2
      TXSUM(KTX,3)=TXSUM(KTX,3)+TURP2
      TXSUM(KTY,3)=TXSUM(KTY,3)+TURP2
      TXSUM(KTX,5)=TXSUM(KTX,5)+TURP2
      TXSUM(KTY,5)=TXSUM(KTY,5)+TURP2
      TXSUM(13,3)=TXSUM(13,3)+TURP2
      TXSUM(13,5)=TXSUM(13,5)+TURP2
 5006 IF(RALTXF(7).LT.0) GO TO 5007
      IF(RALTXF(7).EQ.0) KTX=1
      IF(RALTXF(7).EQ.1) KTX=2
      IF(RALTXF(7).GT.1) KTX=3
      KTY=KTX+9
      KTX=(C-1)*3+KTX
      TXTRIP(KTX,JZ)=TXTRIP(KTX,JZ)+TURP3
      TXSUM(KTX,3)=TXSUM(KTX,3)+TURP3
      TXSUM(KTY,3)=TXSUM(KTY,3)+TURP3
      TXSUM(KTX,5)=TXSUM(KTX,5)+TURP3
      TXSUM(KTY,5)=TXSUM(KTY,5)+TURP3
      TXSUM(13,3)=TXSUM(13,3)+TURP3
      TXSUM(13,5)=TXSUM(13,5)+TURP3
 5007 IF(RALTXF(8).LT.0) GO TO 5008
      IF(RALTXF(8).EQ.0) KTX=1
      IF(RALTXF(8).EQ.1) KTX=2
      IF(RALTXF(8).GT.1) KTX=3
      KTY=KTX+9
      KTX=(C-1)*3+KTX
      TXTRIP(KTX,JZ)=TXTRIP(KTX,JZ)+TURP4
      TXSUM(KTX,3)=TXSUM(KTX,3)+TURP4
      TXSUM(KTY,3)=TXSUM(KTY,3)+TURP4
      TXSUM(KTX,5)=TXSUM(KTX,5)+TURP4
      TXSUM(KTY,5)=TXSUM(KTY,5)+TURP4
      TXSUM(13,3)=TXSUM(13,3)+TURP4
      TXSUM(13,5)=TXSUM(13,5)+TURP4
 5008 IF(RALTXF(9).LT.0) GO TO 5009
      IF(RALTXF(9).EQ.0) KTX=1
      IF(RALTXF(9).EQ.1) KTX=2
      IF(RALTXF(9).GT.1) KTX=3
      KTY=KTX+9
      KTX=(C-1)*3+KTX
      TXTRIP(KTX,JZ)=TXTRIP(KTX,JZ)+TURK1
      TXSUM(KTX,4)=TXSUM(KTX,4)+TURK1
      TXSUM(KTY,4)=TXSUM(KTY,4)+TURK1
      TXSUM(KTX,5)=TXSUM(KTX,5)+TURK1
      TXSUM(KTY,5)=TXSUM(KTY,5)+TURK1
      TXSUM(13,4)=TXSUM(13,4)+TURK1
      TXSUM(13,5)=TXSUM(13,5)+TURK1
 5009 IF(RALTXF(10).LT.0) GO TO 5010
      IF(RALTXF(10).EQ.0) KTX=1
      IF(RALTXF(10).EQ.1) KTX=2
      IF(RALTXF(10).GT.1) KTX=3
      KTY=KTX+9
      KTX=(C-1)*3+KTX
      TXTRIP(KTX,JZ)=TXTRIP(KTX,JZ)+TURK2
      TXSUM(KTX,4)=TXSUM(KTX,4)+TURK2
      TXSUM(KTY,4)=TXSUM(KTY,4)+TURK2
      TXSUM(KTX,5)=TXSUM(KTX,5)+TURK2
      TXSUM(KTY,5)=TXSUM(KTY,5)+TURK2
      TXSUM(13,4)=TXSUM(13,4)+TURK2
      TXSUM(13,5)=TXSUM(13,5)+TURK2
 5010 IF(RALTXF(11).LT.0) GO TO 5011
      IF(RALTXF(11).EQ.0) KTX=1
      IF(RALTXF(11).EQ.1) KTX=2
      IF(RALTXF(11).GT.1) KTX=3
      KTY=KTX+9
      KTX=(C-1)*3+KTX
      TXTRIP(KTX,JZ)=TXTRIP(KTX,JZ)+TURK3
      TXSUM(KTX,4)=TXSUM(KTX,4)+TURK3
      TXSUM(KTY,4)=TXSUM(KTY,4)+TURK3
      TXSUM(KTX,5)=TXSUM(KTX,5)+TURK3
      TXSUM(KTY,5)=TXSUM(KTY,5)+TURK3
      TXSUM(13,4)=TXSUM(13,4)+TURK3
      TXSUM(13,5)=TXSUM(13,5)+TURK3
 5011 IF(RALTXF(12).LT.0) GO TO 5012
      IF(RALTXF(12).EQ.0) KTX=1
      IF(RALTXF(12).EQ.1) KTX=2
      IF(RALTXF(12).GT.1) KTX=3
      KTY=KTX+9
      KTX=(C-1)*3+KTX
      TXTRIP(KTX,JZ)=TXTRIP(KTX,JZ)+TURK4
      TXSUM(KTX,4)=TXSUM(KTX,4)+TURK4
      TXSUM(KTY,4)=TXSUM(KTY,4)+TURK4
      TXSUM(KTX,5)=TXSUM(KTX,5)+TURK4
      TXSUM(KTY,5)=TXSUM(KTY,5)+TURK4
      TXSUM(13,4)=TXSUM(13,4)+TURK4
      TXSUM(13,5)=TXSUM(13,5)+TURK4
 5012 CONTINUE
C
C     SAVE LOW INCOME TRANSIT TRIPS
C
      IF(INCLEV.EQ.C) THEN
      LTRIPS(1,JZ)=LTRIPS(1,JZ)+TLOCW
      LTRIPS(2,JZ)=LTRIPS(2,JZ)+TLOCD
      LTRIPS(3,JZ)=LTRIPS(3,JZ)+TEXPW
      LTRIPS(4,JZ)=LTRIPS(4,JZ)+TEXPD
      LTRIPS(5,JZ)=LTRIPS(5,JZ)+TCRW1+TCRW2
      LTRIPS(6,JZ)=LTRIPS(6,JZ)+TCRB1+TCRB2
      LTRIPS(7,JZ)=LTRIPS(7,JZ)+TCRP1+TCRP2+TCRP3+TCRP4
      LTRIPS(8,JZ)=LTRIPS(8,JZ)+TCRK1+TCRK2+TCRK3+TCRK4
      LTRIPS(9,JZ)=LTRIPS(9,JZ)+TURW1+TURW2
      LTRIPS(10,JZ)=LTRIPS(10,JZ)+TURB1+TURB2
      LTRIPS(11,JZ)=LTRIPS(11,JZ)+TURP1+TURP2+TURP3+TURP4
      LTRIPS(12,JZ)=LTRIPS(12,JZ)+TURK1+TURK2+TURK3+TURK4
      LTRIPS(13,JZ)=LTRIPS(13,JZ)+TWAYW
      LTRIPS(14,JZ)=LTRIPS(14,JZ)+TWAYD
      LTRIPS(15,JZ)=LTRIPS(15,JZ)+TRPDW
      LTRIPS(16,JZ)=LTRIPS(16,JZ)+TRPDD
      LTRIPS(17,JZ)=LTRIPS(17,JZ)+TBRTW
      IF(BRTUR) THEN
      LTRIPS(19,JZ)=LTRIPS(19,JZ)+TBRTD
      ELSE
      LTRIPS(18,JZ)=LTRIPS(18,JZ)+TBRTD
      END IF
      END IF
C 
C SAVE SELECTED STATION TRIPS BY ACCESS MODE
C
      IF(TXOUT) THEN
      DO 5150 L=1,2
      DO 5151 K=1,12
      STAMTH=.FALSE.
      DO 5152 MT=1,20
      IF(OSTA(L,K).EQ.SELSTA(MT)) THEN
        STAMTH=.TRUE.
        IF(L.EQ.1) THEN
        IF(K.EQ.1) STTRIP(1,JZ)=STTRIP(1,JZ)+TCRW1
        IF(K.EQ.2) STTRIP(1,JZ)=STTRIP(1,JZ)+TCRW2
        IF(K.EQ.3) STTRIP(2,JZ)=STTRIP(2,JZ)+TCRB1
        IF(K.EQ.4) STTRIP(2,JZ)=STTRIP(2,JZ)+TCRB2
        IF(K.EQ.5) STTRIP(3,JZ)=STTRIP(3,JZ)+TCRP1
        IF(K.EQ.6) STTRIP(3,JZ)=STTRIP(3,JZ)+TCRP2
        IF(K.EQ.7) STTRIP(3,JZ)=STTRIP(3,JZ)+TCRP3
        IF(K.EQ.8) STTRIP(3,JZ)=STTRIP(3,JZ)+TCRP4
        IF(K.EQ.9) STTRIP(4,JZ)=STTRIP(4,JZ)+TCRK1
        IF(K.EQ.10) STTRIP(4,JZ)=STTRIP(4,JZ)+TCRK2
        IF(K.EQ.11) STTRIP(4,JZ)=STTRIP(4,JZ)+TCRK3
        IF(K.EQ.12) STTRIP(4,JZ)=STTRIP(4,JZ)+TCRK4
        ELSE
        IF(K.EQ.1) STTRIP(1,JZ)=STTRIP(1,JZ)+TURW1
        IF(K.EQ.2) STTRIP(1,JZ)=STTRIP(1,JZ)+TURW2
        IF(K.EQ.3) STTRIP(2,JZ)=STTRIP(2,JZ)+TURB1
        IF(K.EQ.4) STTRIP(2,JZ)=STTRIP(2,JZ)+TURB2
        IF(K.EQ.5) STTRIP(3,JZ)=STTRIP(3,JZ)+TURP1
        IF(K.EQ.6) STTRIP(3,JZ)=STTRIP(3,JZ)+TURP2
        IF(K.EQ.7) STTRIP(3,JZ)=STTRIP(3,JZ)+TURP3
        IF(K.EQ.8) STTRIP(3,JZ)=STTRIP(3,JZ)+TURP4
        IF(K.EQ.9) STTRIP(4,JZ)=STTRIP(4,JZ)+TURK1
        IF(K.EQ.10) STTRIP(4,JZ)=STTRIP(4,JZ)+TURK2
        IF(K.EQ.11) STTRIP(4,JZ)=STTRIP(4,JZ)+TURK3
        IF(K.EQ.12) STTRIP(4,JZ)=STTRIP(4,JZ)+TURK4
        END IF
      END IF
 5152 CONTINUE
C
C SAVE SELECTED STATION TRIPS BY EGRESS MODE
C
      IF(.NOT.STAMTH) THEN
       DO 5153 MT2=1,20
       IF(ASTA(L,K).EQ.SELSTA(MT2)) THEN
         IF(L.EQ.1) THEN                             
         IF(K.EQ.1) STTRIP(1,JZ)=STTRIP(1,JZ)+TCRW1  
         IF(K.EQ.2) STTRIP(1,JZ)=STTRIP(1,JZ)+TCRW2  
         IF(K.EQ.3) STTRIP(2,JZ)=STTRIP(2,JZ)+TCRB1  
         IF(K.EQ.4) STTRIP(2,JZ)=STTRIP(2,JZ)+TCRB2  
         IF(K.EQ.5) STTRIP(3,JZ)=STTRIP(3,JZ)+TCRP1  
         IF(K.EQ.6) STTRIP(3,JZ)=STTRIP(3,JZ)+TCRP2  
         IF(K.EQ.7) STTRIP(3,JZ)=STTRIP(3,JZ)+TCRP3  
         IF(K.EQ.8) STTRIP(3,JZ)=STTRIP(3,JZ)+TCRP4  
         IF(K.EQ.9) STTRIP(4,JZ)=STTRIP(4,JZ)+TCRK1  
         IF(K.EQ.10) STTRIP(4,JZ)=STTRIP(4,JZ)+TCRK2 
         IF(K.EQ.11) STTRIP(4,JZ)=STTRIP(4,JZ)+TCRK3 
         IF(K.EQ.12) STTRIP(4,JZ)=STTRIP(4,JZ)+TCRK4 
         ELSE                                        
         IF(K.EQ.1) STTRIP(1,JZ)=STTRIP(1,JZ)+TURW1  
         IF(K.EQ.2) STTRIP(1,JZ)=STTRIP(1,JZ)+TURW2  
         IF(K.EQ.3) STTRIP(2,JZ)=STTRIP(2,JZ)+TURB1  
         IF(K.EQ.4) STTRIP(2,JZ)=STTRIP(2,JZ)+TURB2  
         IF(K.EQ.5) STTRIP(3,JZ)=STTRIP(3,JZ)+TURP1  
         IF(K.EQ.6) STTRIP(3,JZ)=STTRIP(3,JZ)+TURP2  
         IF(K.EQ.7) STTRIP(3,JZ)=STTRIP(3,JZ)+TURP3  
         IF(K.EQ.8) STTRIP(3,JZ)=STTRIP(3,JZ)+TURP4  
         IF(K.EQ.9) STTRIP(4,JZ)=STTRIP(4,JZ)+TURK1  
         IF(K.EQ.10) STTRIP(4,JZ)=STTRIP(4,JZ)+TURK2 
         IF(K.EQ.11) STTRIP(4,JZ)=STTRIP(4,JZ)+TURK3 
         IF(K.EQ.12) STTRIP(4,JZ)=STTRIP(4,JZ)+TURK4 
         END IF  
       END IF 
 5153  CONTINUE
      END IF
 5151 CONTINUE
 5150 CONTINUE
      END IF
C 
C SAVE PERSON TRIP AVAILABILITY
C
      IF(CALIB) THEN
      DO 5013 MT=1,18
      IF(MODINC(MT).GT.0) THEN
      KT=(C-1)*18+MT
      AVLPERT(KT,JZ)=AVLPERT(KT,JZ)+IDINT(PERTRP*1000.0)
      END IF
      MODINC(MT)=0
 5013 CONTINUE
      END IF
C
C SUMMARIZE AUTO PERSON TRIPS BY TOLL/NON TOLL & HOV
C
      TOLSUM( 1,C)=TOLSUM( 1,C)+TDRV0N
      TOLSUM( 2,C)=TOLSUM( 2,C)+TDRV0T
      TOLSUM( 3,C)=TOLSUM( 3,C)+TDRV2NH
      TOLSUM( 4,C)=TOLSUM( 4,C)+TDRV2NN
      TOLSUM( 5,C)=TOLSUM( 5,C)+TDRV2TH
      TOLSUM( 6,C)=TOLSUM( 6,C)+TDRV2TN
      TOLSUM( 7,C)=TOLSUM( 7,C)+TDRV3NH
      TOLSUM( 8,C)=TOLSUM( 8,C)+TDRV3NN
      TOLSUM( 9,C)=TOLSUM( 9,C)+TDRV3TH
      TOLSUM(10,C)=TOLSUM(10,C)+TDRV3TN
      TOLSUM(11,C)=TOLSUM(11,C)+TDRV4NH
      TOLSUM(12,C)=TOLSUM(12,C)+TDRV4NN
      TOLSUM(13,C)=TOLSUM(13,C)+TDRV4TH
      TOLSUM(14,C)=TOLSUM(14,C)+TDRV4TN
C
C STORE URBAN RAIL INDICATOR MATRICES (CAN WALK/MUST DRIVE)
C
      IF(LRTIND) THEN
      IF(M.LE.4.AND.TUR.GT.0) LRTWLK(JZ)=1
      IF(M.GT.4.AND.M.LE.6.AND.(TURP.GT.0.OR.TURK.GT.0)) LRTDRV(JZ)=1
      END IF
C
C STORE COMMUTER RAIL INDICATOR MATRICES (CAN WALK/MUST DRIVE)
C
      IF(CRTIND) THEN
      IF(M.LE.4.AND.TCR.GT.0) LRTWLK(JZ)=1
      IF(M.GT.4.AND.M.LE.6.AND.(TCRP.GT.0.OR.TCRK.GT.0)) LRTDRV(JZ)=1
      END IF
C....................................................................
      IF(DEBUG) THEN
      IF(LRTIND) THEN
      WRITE(26,9427) M,LRTWLK(JZ),LRTDRV(JZ)
 9427 FORMAT(/1X,'SUMMARY OF LIGHT RAIL INDICATOR VALUES   '/
     *        1X,'-----------------------------------------'/
     *        1X,'MARKET SEGMENT        =',I10/
     *        1X,'LRT  WALK INDICATOR   =',I10/
     *        1X,'LRT DRIVE INDICATOR   =',I10)
      END IF
      IF(CRTIND) THEN
      WRITE(26,9428) M,LRTWLK(JZ),LRTDRV(JZ)
 9428 FORMAT(/1X,'SUMMARY OF COMMUTER RAIL INDICATOR VALUES'/
     *        1X,'-----------------------------------------'/
     *        1X,'MARKET SEGMENT        =',I10/
     *        1X,'CRT  WALK INDICATOR   =',I10/
     *        1X,'CRT DRIVE INDICATOR   =',I10)
      END IF
      END IF
C.....................................................................
C
C END OF M-INDEX WALK LOOP
C
C
C CHECK FOR COMPLETE SET OF EXAMPLE BEST ZONES
C
C      DO 5111 K=1,10
C      IF(BESTZONE(K,1).EQ.0) GO TO 2000
C 5111 CONTINUE
C      WRITE(26,5112) IZ,((BESTZONE(K1,K2),K2=1,2),K1=1,10)
C 5112 FORMAT(' Origin Zone=',I4/10(1X,I4,2X,I4/))
C      STOP 8
 2000 CONTINUE
C
C END OF C-INDEX INCOME GROUP LOOP
C
C
C OUTPUT FTA USER BENEFIT RECORD
C
      IF(USERBEN) THEN
      IF(CAPRES.AND.(ITER.LT.(NITER-1))) GO TO 1001
       IF(IZ.EQ.JZ) THEN
       AUTEXP=0.99
       TWALK(1)=1.0
       TWALK(2)=0.0
       TSHAR(1)=0.0
       TSHAR(2)=0.0
       END IF
       IF(USERASC) THEN
       WRITE(29,6002) IZ,JZ,C,PERIN(C,JZ),PERIN(C,JZ),AUTEXP,
     *          TWALK(1),TSHAR(1),TWALK(2),TSHAR(2)
 6002  FORMAT(2I5,I2,2F10.4,2x,E12.5,4F10.5)
       ELSE
       WRITE(29) IZ,JZ,C,PERIN(C,JZ),PERIN(C,JZ),AUTEXP,
     *          TWALK(1),TSHAR(1),TWALK(2),TSHAR(2)
       END IF
      END IF
 1001 CONTINUE
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9426) C,PERIN(C,JZ),PERIN(C,JZ),AUTEXP,
     *               TWALK(1),TSHAR(1),TWALK(2),TSHAR(2),
     *               TESUM(5,C)
 9426 FORMAT(/1X,'SUMMARY OF FTA USER BENEFIT DATA   '/
     *        1X,'-----------------------------------'/
     *        1X,'INCOME GROUP                 =',I10/
     *        1X,'PERSON TRIPS IN MARKET       =',F10.4/
     *        1X,'MOTORIZED TRIPS IN MARKET    =',F10.4/
     *        1X,'NON TRANSIT EXP UTILITY      =',E12.5/
     *        1X,'% PERSON TRIPS -  WALK MARKET=',F10.5/
     *        1X,' TRANSIT SHARE -  WALK MARKET=',F10.5/
     *        1X,'% PERSON TRIPS - DRIVE MARKET=',F10.5/
     *        1X,' TRANSIT SHARE - DRIVE MARKET=',F10.5/
     *        1X,'TOTAL TRANST TRIPS           =',F10.5/)
      END IF
C....................................................................
C
C SUMMARIZE MAXIMUM AND MINIMUM LOGSUM VALUES
C
      IF(ALOGSUM(C,JZ).NE.0.0) THEN
      LOGMIN(1,C)=AMIN1(LOGMIN(1,C),ALOGSUM(C,JZ))
      LOGMAX(1,C)=AMAX1(LOGMAX(1,C),ALOGSUM(C,JZ))
      END IF
      IF(TLOGSUM(C,JZ).NE.0.0) THEN
      LOGMIN(2,C)=AMIN1(LOGMIN(2,C),TLOGSUM(C,JZ))
      LOGMAX(2,C)=AMAX1(LOGMAX(2,C),TLOGSUM(C,JZ))
      END IF
      IF(ULOGSUM(C,JZ).NE.0.0) THEN
      LOGMIN(3,C)=AMIN1(LOGMIN(3,C),ULOGSUM(C,JZ))
      LOGMAX(3,C)=AMAX1(LOGMAX(3,C),ULOGSUM(C,JZ))
      END IF
 1000 CONTINUE
C
C  END OF THE J-ZONE LOOP
C
  200 CONTINUE
C
C     OUTPUT TRIPS MATRIX FOR JZ HERE
C
 1121 CONTINUE
C
C OUTPUT AVAILABLE PERSON TRIPS (IF CALIB ONLY)
C
      IF(CALIB.AND.PERAVL.AND.(CITER.EQ.0)) THEN
      DO 1992 IT=1,54
      DO 1993 JZ=1,MAX_ZONES
      ROW(JZ)=AVLPERT(IT,JZ)
      AVLPERT(IT,JZ)=0
 1993 CONTINUE
      PURP=IT
      CALL OUTAB(45,ROW,IZ,PURP,DUMMY,IO)
 1992 CONTINUE
      END IF
C
C OUTPUT STATION LEVEL DATA & SHARES
C FOR ON-BOARD SURVEY ASSIGNMENT ANALYSIS
C
      IF(MTXOBS) THEN
      DO 3992 IT=1,52
      DO 3993 JZ=1,MAX_ZONES
      ROW(JZ)=OBSVAL(IT,JZ)
      OBSVAL(IT,JZ)=0
 3993 CONTINUE
      PURP=IT
      CALL OUTAB(66,ROW,IZ,PURP,DUMMY,IO)
 3992 CONTINUE
      IF(CALIB) THEN
      CALL SHAREVAL(OBSVALR,1,2)
      CALL SHAREVAL(OBSVALR,3,4)
      CALL SHAREVAL(OBSVALR,5,8)
      CALL SHAREVAL(OBSVALR,9,12)
      CALL SHAREVAL(OBSVALR,13,14)
      CALL SHAREVAL(OBSVALR,15,16)
      CALL SHAREVAL(OBSVALR,17,20)
      CALL SHAREVAL(OBSVALR,21,24)
      END IF
      DO 3994 IT=1,24
      DO 3995 JZ=1,MAX_ZONES
      ROW(JZ)=IDINT(OBSVALR(IT,JZ)*10000.0)
      OBSVALR(IT,JZ)=0.0
 3995 CONTINUE
      PURP=IT+52
      CALL OUTAB(66,ROW,IZ,PURP,DUMMY,IO)
 3994 CONTINUE
      END IF
C
C OUTPUT FACTORED PERSON TRIP MATRIX (IF REQUESTED)
C
      IF(OUTPER.AND.(CITER.EQ.0)) THEN
      DO 1994 IT=1,3
      DO 1995 JZ=1,MAX_ZONES
      ROW(JZ)=IFIX(PERIN(IT,JZ)*100.0)
 1995 CONTINUE
      PURP=IT
      CALL OUTAB(46,ROW,IZ,PURP,DUMMY,IO)
 1994 CONTINUE
      END IF
C
C OUTPUT COMMUTER RAIL PERCENTAGES FOR SUMMIT
C
      IF(CRPRB) THEN
      DO 1892 IT=1,2
      DO 1893 K=1,3
      DO 1894 JZ=1,MAX_ZONES
      CRPCTZ(JZ)=IDINT(CRPCT(JZ,K,IT)*10000.0)
 1894 CONTINUE
      PURP=(IT-1)*3+K
      CALL OUTAB(60,CRPCTZ,IZ,PURP,DUMMY,IO)
 1893 CONTINUE
 1892 CONTINUE
      END IF
C
C   BEST PATH MATRICES, IF REQUESED
C
      IF(BESTPATH) THEN
      DO 3101 II=1,32
      DO 3102 JJ=1,MAX_ZONES
      IF(II.LE.16) THEN
      ROW(JJ)=WKPATH(II,JJ)
      ELSE
      ROW(JJ)=DRPATH((II-16),JJ)
	END IF
 3102 CONTINUE
      PURP=II
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
 3101 CONTINUE
      END IF
C
C  OUTPUT USERBENC RELATED DATA, IF REQUESTED
C
      IF(INDRAL) THEN
C..URBAN RAIL
      DO 3390 II=1,12
      DO 3391 K=1,3
      DO 3392 JJ=1,MAX_ZONES
      ROW(JJ)=USERBENC(II,K,JJ)
 3392 CONTINUE
      PURP=(II-1)*3+K
      CALL OUTAB(11,ROW,IZ,PURP,DUMMY,IO)
 3391 CONTINUE
 3390 CONTINUE
      DO 3394 II=1,12
      DO 3395 K=1,3
      DO 3393 JJ=1,MAX_ZONES
      ROW(JJ)=IDINT(USERBENT(II,K,JJ)*1000.0)
 3393 CONTINUE
      PURP=(II-1)*3+K+36
      CALL OUTAB(11,ROW,IZ,PURP,DUMMY,IO)
 3395 CONTINUE
 3394 CONTINUE
C....BRT TO URBAN RAIL
      DO 3396 K=1,3
      PURP=72+K
      DO 3397 JJ=1,MAX_ZONES
      ROW(JJ)=USERBENC3(K,JJ)
 3397 CONTINUE
      CALL OUTAB(11,ROW,IZ,PURP,DUMMY,IO)
 3396 CONTINUE
      DO 3398 K=1,3
      PURP=75+K
      DO 3399 JJ=1,MAX_ZONES
      ROW(JJ)=IDINT(USERBENT3(K,JJ)*1000.0)
 3399 CONTINUE
      CALL OUTAB(11,ROW,IZ,PURP,DUMMY,IO)
 3398 CONTINUE
C...COMMUTER RAIL
      DO 3490 II=1,12
      DO 3491 K=1,3
      DO 3492 JJ=1,MAX_ZONES
      ROW(JJ)=USERBENC2(II,K,JJ)
 3492 CONTINUE
      PURP=(II-1)*3+K
      CALL OUTAB(70,ROW,IZ,PURP,DUMMY,IO)
 3491 CONTINUE
 3490 CONTINUE
      DO 3494 II=1,12
      DO 3495 K=1,3
      DO 3493 JJ=1,MAX_ZONES
      ROW(JJ)=IDINT(USERBENT2(II,K,JJ)*1000.0)
 3493 CONTINUE
      PURP=(II-1)*3+K+36
      CALL OUTAB(70,ROW,IZ,PURP,DUMMY,IO)
 3495 CONTINUE
 3494 CONTINUE
      END IF
C
C   OUTPUT SELECTED LINK TRIP MATRICES
C
      IF(RAILSEL) THEN
c.....station-to-station
      purp=1
      rem122=0.0
      do jj=1,max_zones
      if(zstatrips(jj).gt.0.0) then
      temp=(zstatrips(jj)*100.0)+rem122
      row(jj)=ifix(temp)
      rem122=temp-row(jj)
      else
      row(jj)=0
      end if
      end do
      call outab(122,row,iz,purp,dummy,io)
c.....zone-to-station
      purp=2
      rem123=0.0
      do jj=1,max_zones
      if(zstatrips2(jj).gt.0.0) then
      temp=(zstatrips2(jj)*100.0)+rem123
      row(jj)=ifix(temp)
      rem123=temp-row(jj)
      else
      row(jj)=0
      end if
      end do
      call outab(122,row,iz,purp,dummy,io)
c.....zone-to-station
      purp=3
      rem124=0.0
      do jj=1,max_zones
      if(zstatrips3(jj).gt.0.0) then
      temp=(zstatrips3(jj)*100.0)+rem124
      row(jj)=ifix(temp)
      rem124=temp-row(jj)
      else
      row(jj)=0
      end if
      end do
      call outab(122,row,iz,purp,dummy,io)
      END IF
C
C   OUTPUT HIGHWAY TRIP MATRICES
C
C...DA NON-TOLL
      if(CALIB.AND.(CITER.LT.MAXCALIT)) goto 100
      if(CAPRES.AND.(ITER.LT.(NITER-1))) goto 100
      IF(.NOT.TRIPSOUT) GO TO 99
      PURP=1
      tnew=0.0
      itot=0
      rem21=0.0
      DO 3301,JJ=1,MAX_ZONES
      if(trips(1,jj).gt.0.0) then
      temp=(trips(1,jj)*100.0) + rem21
      ROW(JJ)=IFIX(temp)
      rem21=temp-row(jj)
      else
      row(jj)=0
      endif
      tnew=tnew+trips(1,jj)
      itot=itot+row(jj)
 3301 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...DA TOLL
      PURP=2
      tnew=0.0
      itot=0
      rem22=0.0
      DO 3302,JJ=1,MAX_ZONES
      if(trips(2,jj).gt.0.0) then
      temp=(trips(2,jj)*100.0) + rem22
      ROW(JJ)=IFIX(temp)
      rem22=temp-row(jj)
      else
      row(jj)=0
      endif
      tnew=tnew+trips(2,jj)
      itot=itot+row(jj)
 3302 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...2-PERSON NON-TOLL HOV
      PURP=3
      tnew=0.0
      itot=0
      rem23=0.0
      DO 3303,JJ=1,MAX_ZONES
      if(trips(3,jj).gt.0.0) then
      temp=(trips(3,jj)*100.0) + rem23
      ROW(JJ)=IFIX(temp)
      rem23=temp-row(jj)
      else
      row(jj)=0
      endif
      tnew=tnew+trips(3,jj)
      itot=itot+row(jj)
 3303 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...2-PERSON NON-TOLL NON-HOV
      PURP=4
      tnew=0.0
      itot=0
      rem24=0.0
      DO 3304,JJ=1,MAX_ZONES
      if(trips(4,jj).gt.0.0) then
      temp=(trips(4,jj)*100.0) + rem24
      ROW(JJ)=IFIX(temp)
      rem24=temp-row(jj)
      else
      row(jj)=0
      endif
      tnew=tnew+trips(4,jj)
      itot=itot+row(jj)
 3304 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...2-PERSON TOLL HOV
      PURP=5
      tnew=0.0
      itot=0
      rem25=0.0
      DO 3305,JJ=1,MAX_ZONES
      if(trips(5,jj).gt.0.0) then
      temp=(trips(5,jj)*100.0) + rem25
      ROW(JJ)=IFIX(temp)
      rem25=temp-row(jj)
      else
      row(jj)=0
      endif
      tnew=tnew+trips(5,jj)
      itot=itot+row(jj)
 3305 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...2-PERSON TOLL NON-HOV
      PURP=6
      tnew=0.0
      itot=0
      rem26=0.0
      DO 3306,JJ=1,MAX_ZONES
      if(trips(6,jj).gt.0.0) then
      temp=(trips(6,jj)*100.0) + rem26
      ROW(JJ)=IFIX(temp)
      rem26=temp-row(jj)
      else
      row(jj)=0
      endif
      tnew=tnew+trips(6,jj)
      itot=itot+row(jj)
 3306 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...3-PERSON NON-TOLL HOV
      PURP=7
      tnew=0.0
      itot=0
      rem27=0.0
      DO 3307,JJ=1,MAX_ZONES
      if(trips(7,jj).gt.0.0) then
      temp=(trips(7,jj)*100.0) + rem27
      ROW(JJ)=IFIX(temp)
      rem27=temp-row(jj)
      else
      row(jj)=0
      endif
      tnew=tnew+trips(7,jj)
      itot=itot+row(jj)
 3307 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...3-PERSON NON-TOLL NON-HOV
      PURP=8
      tnew=0.0
      itot=0
      rem28=0.0
      DO 3308,JJ=1,MAX_ZONES
      if(trips(8,jj).gt.0.0) then
      temp=(trips(8,jj)*100.0) + rem28
      ROW(JJ)=IFIX(temp)
      rem28=temp-row(jj)
      else
      row(jj)=0
      endif
      tnew=tnew+trips(8,jj)
      itot=itot+row(jj)
 3308 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...3-PERSON TOLL HOV
      PURP=9
      tnew=0.0
      itot=0
      rem29=0.0
      DO 3309,JJ=1,MAX_ZONES
      if(trips(9,jj).gt.0.0) then
      temp=(trips(9,jj)*100.0) + rem29
      ROW(JJ)=IFIX(temp)
      rem29=temp-row(jj)
      else
      row(jj)=0
      endif
      tnew=tnew+trips(9,jj)
      itot=itot+row(jj)
 3309 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...3-PERSON TOLL NON-HOV
      PURP=10
      tnew=0.0
      itot=0
      rem20=0.0
      DO 3310,JJ=1,MAX_ZONES
      if(trips(10,jj).gt.0.0) then
      temp=(trips(10,jj)*100.0) + rem20
      ROW(JJ)=IFIX(temp)
      rem20=temp-row(jj)
      else
      row(jj)=0
      endif
      tnew=tnew+trips(10,jj)
      itot=itot+row(jj)
 3310 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...4-PERSON NON-TOLL HOV
      PURP=11
      tnew=0.0
      itot=0
      rem35=0.0
      DO 3347,JJ=1,MAX_ZONES
      if(trips(49,jj).gt.0.0) then
      temp=(trips(49,jj)*100.0) + rem35
      ROW(JJ)=IFIX(temp)
      rem35=temp-row(jj)
      else
      row(jj)=0
      endif
      tnew=tnew+trips(49,jj)
      itot=itot+row(jj)
 3347 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...4-PERSON NON-TOLL NON-HOV
      PURP=12
      tnew=0.0
      itot=0
      rem36=0.0
      DO 3348,JJ=1,MAX_ZONES
      if(trips(50,jj).gt.0.0) then
      temp=(trips(50,jj)*100.0) + rem36
      ROW(JJ)=IFIX(temp)
      rem36=temp-row(jj)
      else
      row(jj)=0
      endif
      tnew=tnew+trips(50,jj)
      itot=itot+row(jj)
 3348 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...4-PERSON TOLL HOV
      PURP=13
      tnew=0.0
      itot=0
      rem37=0.0
      DO 3349,JJ=1,MAX_ZONES
      if(trips(51,jj).gt.0.0) then
      temp=(trips(51,jj)*100.0) + rem37
      ROW(JJ)=IFIX(temp)
      rem37=temp-row(jj)
      else
      row(jj)=0
      endif
      tnew=tnew+trips(51,jj)
      itot=itot+row(jj)
 3349 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...4-PERSON TOLL NON-HOV
      PURP=14
      tnew=0.0
      itot=0
      rem38=0.0
      DO 3342,JJ=1,MAX_ZONES
      if(trips(52,jj).gt.0.0) then
      temp=(trips(52,jj)*100.0) + rem38
      ROW(JJ)=IFIX(temp)
      rem38=temp-row(jj)
      else
      row(jj)=0
      endif
      tnew=tnew+trips(52,jj)
      itot=itot+row(jj)
 3342 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C OUTPUT LOGSUM VALUES
C
      IF(LOGSUM) THEN
C..AUTO LOGSUM
      DO 3511 C=1,6
      rem30=0.0
      DO 3510 JJ=1,MAX_ZONES
      temp=ALOGSUM(C,JJ)*100.0+rem30
      ROW(JJ)=IFIX(temp)
      if(c.gt.3) then
       if(ALOGSUM(c,jj).gt.0.0) then
       rem30=temp-row(jj)
       else
       row(jj)=0
       end if
      end if
 3510 CONTINUE
      PURP=C
      CALL OUTAB(28,ROW,IZ,PURP,DUMMY,IO)
 3511 CONTINUE
C..TRANSIT LOGSUM
      DO 3512 C=1,6
      rem31=0.0
      DO 3513 JJ=1,MAX_ZONES
      temp=TLOGSUM(C,JJ)*100.0+rem31
      ROW(JJ)=IFIX(temp)
      if(c.gt.3) then
       if(TLOGSUM(c,jj).gt.0.0) then
       rem31=temp-row(jj)
       else
       row(jj)=0
       end if
      end if
 3513 CONTINUE
      PURP=C+6
      CALL OUTAB(28,ROW,IZ,PURP,DUMMY,IO)
 3512 CONTINUE
C..TOTAL LOGSUM
      DO 3514 C=1,6
      rem32=0.0
      DO 3515 JJ=1,MAX_ZONES
      temp=ULOGSUM(C,JJ)*100.0+rem32
      ROW(JJ)=IFIX(temp)
      if(c.gt.3) then
       if(ULOGSUM(c,jj).gt.0.0) then
       rem32=temp-row(jj)
       else
       row(jj)=0
       end if
      end if
 3515 CONTINUE
      PURP=C+12
      CALL OUTAB(28,ROW,IZ,PURP,DUMMY,IO)
 3514 CONTINUE
      END IF
C ----------------------------------------------------------------  
C OUTPUT TRANSIT TRIP TABLES
C ZONE TO ZONE BASIS
C ----------------------------------------------------------------
C...WALK TO LOCAL BUS
      PURP=1
      tnew=0.0
      itot=0
      rem1=0.0
      DO 3311,JJ=1,MAX_ZONES
      if(trips(11,jj).gt.0.0) then
      temp=(trips(11,jj)*100.0)+ rem1
      ROW(JJ)=IFIX(temp)
      rem1=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(11,jj)
      itot=itot+row(jj)
 3311 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...DRIVE TO LOCAL BUS
      PURP=2
      tnew=0.0
      itot=0
      rem2=0.0
      DO 3312,JJ=1,MAX_ZONES
      if(trips(12,jj).gt.0.0) then
      temp=(trips(12,jj)*100.0)+ rem2
      ROW(JJ)=IFIX(temp)
      rem2=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(12,jj)
      itot=itot+row(jj)
 3312 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...WALK TO EXPRESS BUS
      PURP=3
      tnew=0.0
      itot=0
      rem3=0.0
      DO 3313,JJ=1,MAX_ZONES
      if(trips(13,jj).gt.0.0) then
      temp=(trips(13,jj)*100.)+ rem3
      ROW(JJ)=IFIX(temp)
      rem3=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(13,jj)
      itot=itot+row(jj)
 3313 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...DRIVE TO EXPRESS BUS
      PURP=4
      tnew=0.0
      itot=0
      rem4=0.0
      DO 3314,JJ=1,MAX_ZONES
      if(trips(14,jj).gt.0.0) then
      temp=(trips(14,jj)*100.0)+ rem4
      ROW(JJ)=IFIX(temp)
      rem4=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(14,jj)
      itot=itot+row(jj)
 3314 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...WALK TO COMMUTER RAIL
      PURP=5
      tnew=0.0
      itot=0
      rem5=0.0
      DO 3315,JJ=1,MAX_ZONES
      if((trips(15,jj)+trips(16,jj)).gt.0.0) then
      temp=((trips(15,jj)+ trips(16,jj))*100.0) + rem5
      ROW(JJ)=IFIX(temp)
      rem5=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(15,jj)+trips(16,jj)
      itot=itot+row(jj)
 3315 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...BUS TO COMMUTER RAIL
      PURP=6
      tnew=0.0
      itot=0
      rem6=0.0
      DO 3316,JJ=1,MAX_ZONES
      if((trips(17,jj)+trips(18,jj)).gt.0.0) then
      temp=((trips(17,jj)+ trips(18,jj))*100.0) + rem6
      ROW(JJ)=IFIX(temp)
      rem6=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(17,jj)+trips(18,jj)
      itot=itot+row(jj)
 3316 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...PARK-N-RIDE TO COMMUTER RAIL
      PURP=7
      tnew=0.0
      itot=0
      rem7=0.0
      DO 3325,JJ=1,MAX_ZONES
      if((trips(19,jj)+trips(20,jj)+trips(21,jj)+trips(22,jj)).gt.0.0)
     *    then
      temp=((trips(19,jj)+trips(20,jj)+trips(21,jj)+trips(22,jj))
     *       *100.0) + rem7
      ROW(JJ)=IFIX(temp)
      rem7=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(19,jj)+trips(20,jj)+trips(21,jj)+trips(22,jj)
      itot=itot+row(jj)
 3325 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...KISS-N-RIDE TO COMMUTER RAIL
      PURP=8
      tnew=0.0
      itot=0
      rem8=0.0
      DO 3326,JJ=1,MAX_ZONES
      if((trips(23,jj)+trips(24,jj)+trips(25,jj)+trips(26,jj)).gt.0.0)
     *    then
      temp=((trips(23,jj)+trips(24,jj)+trips(25,jj)+trips(26,jj))
     *       *100.0) + rem8
      ROW(JJ)=IFIX(temp)
      rem8=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(23,jj)+trips(24,jj)+trips(25,jj)+trips(26,jj)
      itot=itot+row(jj)
 3326 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...WALK TO URBAN RAIL
      PURP=9
      tnew=0.0
      itot=0
      rem9=0.0
      DO 3327,JJ=1,MAX_ZONES
      if((trips(27,jj)+trips(28,jj)).gt.0.0) then
      temp=((trips(27,jj)+ trips(28,jj))*100.0) + rem9
      ROW(JJ)=IFIX(temp)
      rem9=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(27,jj)+trips(28,jj)
      itot=itot+row(jj)
 3327 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...BUS TO URBAN RAIL
      PURP=10
      tnew=0.0
      itot=0
      rem10=0.0
      DO 3320,JJ=1,MAX_ZONES
      if((trips(29,jj)+trips(30,jj)).gt.0.0) then
      temp=((trips(29,jj)+ trips(30,jj))*100.0) + rem10
      ROW(JJ)=IFIX(temp)
      rem10=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(29,jj)+trips(30,jj)
      itot=itot+row(jj)
 3320 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...PARK-N-RIDE TO URBAN RAIL
      PURP=11
      tnew=0.0
      itot=0
      rem11=0.0
      DO 3321,JJ=1,MAX_ZONES
      if((trips(31,jj)+trips(32,jj)+trips(33,jj)+trips(34,jj)).gt.0.0)
     *    then
      temp=((trips(31,jj)+trips(32,jj)+trips(33,jj)+trips(34,jj))
     *      *100.0) + rem11
      ROW(JJ)=IFIX(temp)
      rem11=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(31,jj)+trips(32,jj)+trips(33,jj)+trips(34,jj)
      itot=itot+row(jj)
 3321 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...KISS-N-RIDE TO URBAN RAIL
      PURP=12
      tnew=0.0
      itot=0
      rem12=0.0
      DO 3322,JJ=1,MAX_ZONES
      if((trips(35,jj)+trips(36,jj)+trips(37,jj)+trips(38,jj)).gt.0.0)
     *    then
      temp=((trips(35,jj)+trips(36,jj)+trips(37,jj)+trips(38,jj))
     *       *100.0) + rem12
      ROW(JJ)=IFIX(temp)
      rem12=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(35,jj)+trips(36,jj)+trips(37,jj)+trips(38,jj)
      itot=itot+row(jj)
 3322 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...WALK TO TRANSITWAY
      PURP=13
      tnew=0.0
      itot=0
      rem13=0.0
      DO 3323,JJ=1,MAX_ZONES
      if(trips(39,jj).gt.0.0) then
      temp=(trips(39,jj)*100.0) + rem13
      ROW(JJ)=IFIX(temp)
      rem13=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(39,jj)
      itot=itot+row(jj)
 3323 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...DRIVE TO TRANSITWAY 
      PURP=14
      tnew=0.0
      itot=0
      rem14=0.0
      DO 3324,JJ=1,MAX_ZONES
      if(trips(40,jj).gt.0.0) then
      temp=(trips(40,jj)*100.0) + rem14
      ROW(JJ)=IFIX(temp)
      rem14=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(40,jj)
      itot=itot+row(jj)
 3324 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...WALK TO RAPID BUS
      PURP=15
      tnew=0.0
      itot=0
      rem15=0.0
      DO 3333,JJ=1,MAX_ZONES
      if(trips(44,jj).gt.0.0) then
      temp=(trips(44,jj)*100.0) + rem15
      ROW(JJ)=IFIX(temp)
      rem15=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(44,jj)
      itot=itot+row(jj)
 3333 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...DRIVE TO RAPID BUS 
      PURP=16
      tnew=0.0
      itot=0
      rem16=0.0
      DO 3334,JJ=1,MAX_ZONES
      if(trips(45,jj).gt.0.0) then
      temp=(trips(45,jj)*100.0) + rem16
      ROW(JJ)=IFIX(temp)
      rem16=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(45,jj)
      itot=itot+row(jj)
 3334 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...WALK TO BUS RAPID TRANSIT
      PURP=17
      tnew=0.0
      itot=0
      rem17=0.0
      DO 3335,JJ=1,MAX_ZONES
      if(trips(46,jj).gt.0.0) then
      temp=(trips(46,jj)*100.0) + rem17
      ROW(JJ)=IFIX(temp)
      rem17=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(46,jj)
      itot=itot+row(jj)
 3335 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...DRIVE TO BUS RAPID TRANSIT
      PURP=18
      tnew=0.0
      itot=0
      rem18=0.0
      DO 3336,JJ=1,MAX_ZONES
      if(trips(47,jj).gt.0.0) then
      temp=(trips(47,jj)*100.0) + rem18
      ROW(JJ)=IFIX(temp)
      rem18=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(47,jj)
      itot=itot+row(jj)
 3336 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...DRIVE TO BUS RAPID TRANSIT & URBAN RAIL
      PURP=19
      tnew=0.0
      itot=0
      rem19=0.0
      DO 3337,JJ=1,MAX_ZONES
      if(trips(48,jj).gt.0.0) then
      temp=(trips(48,jj)*100.0) + rem19
      ROW(JJ)=IFIX(temp)
      rem19=temp-row(jj)
      else
      row(jj)=0
      end if
      tnew=tnew+trips(48,jj)
      itot=itot+row(jj)
 3337 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C  
C  OUTPUT URBAN RAIL TRIPS BY INCOME AND NUMBER OF TRANSFERS (IF REQUESTED)
C
      IF(TXOUT.AND.(.NOT.TRIPSOUT)) THEN
      DO 3901 T=1,9
      DO 3902 JJ=1,MAX_ZONES
      XTRIP(JJ)=TXTRIP(T,JJ)*10.0
 3902 CONTINUE
      CALL TABOUT(30,IZ,T,XTRIP)
 3901 CONTINUE
      END IF
C  
C  OUTPUT RAIL TRIPS BY ACCESS MODE FOR SELECTED STATION (IF REQUESTED)
C
      IF(TXOUT.AND.TRIPSOUT) THEN
      DO 3911 T=1,4
      DO 3912 JJ=1,MAX_ZONES
      XTRIP(JJ)=STTRIP(T,JJ)*10.0
 3912 CONTINUE
      CALL TABOUT(30,IZ,T,XTRIP)
 3911 CONTINUE
      END IF
C
C  OUTPUT URBAN RAIL INDICATOR MATRIX
C
   99 IF(LRTIND.OR.CRTIND) THEN
      PURP=1
      CALL OUTAB(35,LRTWLK,IZ,PURP,DUMMY,IO)
      PURP=2
      CALL OUTAB(35,LRTDRV,IZ,PURP,DUMMY,IO)
      END IF
C  
C  OUTPUT INCOME SPECIFIC TRANSIT TRIP MATRICES (IF REQUESTED)
C
      IF(INCLEV.GT.0) THEN
      IF(CALIB.AND.(CITER.LT.MAXCALIT)) GO TO 3503
      DO 3501 T=1,19
      DO 3502 JJ=1,MAX_ZONES
      XTRIP(JJ)=LTRIPS(T,JJ)
 3502 CONTINUE
      CALL TABOUT(30,IZ,T,XTRIP)
 3501 CONTINUE
 3503 CONTINUE
      END IF
C
C  END OF THE I-ZONE LOOP
C  
  100 CONTINUE
C
C  SUMMARIZE MINIMUM AND MAXIMUM LOGSUM VALUES
C
C
C     WRITE(26,9070) FF,((LOGMIN(IZ,JZ),JZ=1,3),IZ=1,3),
C    *               ((LOGMAX(IZ,JZ),JZ=1,3),IZ=1,3)
C9070 FORMAT(A1,1X,'           MINIMUM LOGSUM VALUES'/
C    *       1X,'           ---------------------'//
C    *       1X,'                 INCOME GROUP'/
C    *       1X,'         ----------------------------'/
C    *       1X,'             1         2        3'/
C    *       1X,' URB RL  ',3F8.2/
C    *       1X,' TRANSIT ',3F8.2/
C    *       1X,' TOTAL   ',3F8.2//
C    *       1X,'           MAXIMUM LOGSUM VALUES'/
C    *       1X,'           ---------------------'//
C    *       1X,'                 INCOME GROUP'/
C    *       1X,'         ----------------------------'/
C    *       1X,'             1         2        3'/
C    *       1X,' URB RL  ',3F8.2/
C    *       1X,' TRANSIT ',3F8.2/
C    *       1X,' TOTAL   ',3F8.2)
C
C OUTPUT SELECTED LINK STATION-TO-STATION MATRICES
C
      IF(RAILSEL) THEN
      rem125=0.0
      rem126=0.0
      rem127=0.0
      do ii=1,max_zones
c.....urban rail station-to-station
      row=0.0
      purp=1
      if(ii.gt.max_izones) then
      do ij=1,max_stations
	    jj=ij+max_izones
	    in=ii-max_izones
	    if(statrips(in,ij).gt.0.0) then
      temp=(statrips(in,ij)*100.0)+rem125
      row(jj)=ifix(temp)
      rem125=temp-row(jj)
      else
      row(jj)=0
      end if
      end do
      end if
      CALL OUTAB(124,ROW,II,PURP,DUMMY,IO)   
c.....commuter rail zone-to-station   
      row=0.0
      purp=2
      if(ii.gt.max_izones) then
      do ij=1,max_stations
	    jj=ij+max_izones
	    in=ii-max_izones
	    if(statrips2(in,ij).gt.0.0) then
      temp=(statrips2(in,ij)*100.0)+rem126
      row(jj)=ifix(temp)
      rem126=temp-row(jj)
      else
      row(jj)=0
      end if
      end do
      end if
      CALL OUTAB(124,ROW,II,PURP,DUMMY,IO)   
c.....commuter rail station-to-zone
      row=0.0
      purp=3
      if(ii.gt.max_izones) then
      do ij=1,max_stations
	    jj=ij+max_izones
	    in=ii-max_izones
	    if(statrips3(in,ij).gt.0.0) then
      temp=(statrips3(in,ij)*100.0)+rem127
      row(jj)=ifix(temp)
      rem127=temp-row(jj)
      else
      row(jj)=0
      end if
      end do
      end if
      CALL OUTAB(124,ROW,II,PURP,DUMMY,IO)        
      end do
      END IF
C
C  OUTPUT REST OF TRIP TABLES
C
C
      IF(CALIB.AND.(CITER.LT.MAXCALIT)) GOTO 9200
      IF(CAPRES.AND.(ITER.LT.(NITER-1))) GO TO 9200
      IF(.NOT.TRIPSOUT) GO TO 9200
C
C...BUS TO COMMUTER RAIL ORIGIN ZONE TO ORIGIN STATION
C
      DO 3318,II=1,MAX_ZONES
	DO 3319,IT=1,MAX_ZONES
 3319   ROW(IT)=0
	PURP=1
        tnew=0.0
        itot=0
	IF(II.LE.MAX_IZONES) THEN
	DO 3317,IJ=1,MAX_STATIONS
	IF(STANUM(IJ).NE.1) GOTO 3317
	JJ=IJ+MAX_IZONES
        if(bcr(ii,ij).gt.0.0) then
	TEMP=(BCR(II,IJ)*100.0)+rem31
	ROW(JJ)=IFIX(TEMP)
	rem31=TEMP-row(jj)
        else
        row(jj)=0
        end if
        tnew=tnew+bcr(ii,ij)
        itot=itot+row(jj)
 3317 CONTINUE
       ENDIF
       CALL OUTAB(22,ROW,II,PURP,DUMMY,IO)
C
C...COMMUTER RAIL STATION-TO-STATION
C
	DO 3410,IT=1,MAX_ZONES
 3410   ROW(IT)=0
	PURP=2
        tnew=0.0
        itot=0
	IF(II.GT.MAX_IZONES) THEN
	 IF(STANUM((II-MAX_IZONES)).EQ.1) THEN
	DO 3414,IJ=1,MAX_STATIONS
	IF(STANUM(IJ).NE.1) GOTO 3414
	JJ=IJ+MAX_IZONES
        IN=II-MAX_IZONES
        if(CRSS(iN,ij).gt.0.0) then
	TEMP=(CRSS(IN,IJ)*100.0)+rem32
	ROW(JJ)=IFIX(TEMP)
	rem32=TEMP-row(jj)
        else
        row(jj)=0
        end if
        tnew=tnew+CRSS(iN,ij)
        itot=itot+row(jj)
 3414 CONTINUE
        ENDIF
       ENDIF
       CALL OUTAB(22,ROW,II,PURP,DUMMY,IO)
c
C...COMMUTER RAIL STATION TO DESTINATION ZONE
C
	DO 3420,IT=1,MAX_ZONES
 3420   ROW(IT)=0
	PURP=3
        tnew=0.0
        itot=0
        IF(II.GT.MAX_IZONES) THEN
	IF(STANUM((II-MAX_IZONES)).EQ.1) THEN
	DO 3424,IJ=1,MAX_IZONES
        IN=II-MAX_IZONES
        if(CRSTAZ(in,ij).gt.0.0) then
	TEMP=(CRSTAZ(In,IJ)*100.0)+rem33
	ROW(ij)=IFIX(TEMP)
	rem33=TEMP-row(ij)
        else
        row(ij)=0
        end if
        tnew=tnew+CRSTAZ(in,ij)
        itot=itot+row(ij)
 3424 CONTINUE
       ENDIF
       ENDIF
       CALL OUTAB(22,ROW,II,PURP,DUMMY,IO)
 3318 CONTINUE
c
c
C...BUS TO URBAN RAIL ORIGIN ZONE TO ORIGIN STATION
C
C 
C
      DO 3518,II=1,MAX_ZONES
       DO 3519,IT=1,MAX_ZONES
 3519   ROW(IT)=0
	PURP=1
        tnew=0.0
        itot=0
	IF(II.LE.MAX_IZONES) THEN
	DO 3517,IJ=1,MAX_STATIONS
	IF(STANUM(IJ).NE.2) GOTO 3517
	JJ=IJ+MAX_IZONES
        if(BUR(ii,ij).gt.0.0) then
	TEMP=(BUR(II,IJ)*100.0)+rem41
	ROW(JJ)=IFIX(TEMP)
	rem41=TEMP-row(jj)
        else
        row(jj)=0
        end if
        tnew=tnew+BUR(ii,ij)
        itot=itot+row(jj)
 3517 CONTINUE
       ENDIF
       CALL OUTAB(23,ROW,II,PURP,DUMMY,IO)
C
C...URBAN RAIL STATION-TO-STATION
C
	DO 3610,IT=1,MAX_ZONES
 3610   ROW(IT)=0
	PURP=2
        tnew=0.0
        itot=0
        IF(II.GT.MAX_IZONES) THEN
	IF(STANUM((II-MAX_IZONES)).EQ.2) THEN
	DO 3614,IJ=1,MAX_STATIONS
	IF(STANUM(IJ).NE.2) GOTO 3614
	JJ=IJ+MAX_IZONES
        IN=II-MAX_IZONES
        if(URSS(iN,ij).gt.0.0) then
	TEMP=(URSS(IN,IJ)*100.0)+rem42
	ROW(JJ)=IFIX(TEMP)
	rem42=TEMP-row(jj)
        else
        row(jj)=0
        end if
        tnew=tnew+URSS(iN,ij)
        itot=itot+row(jj)
 3614 CONTINUE
       ENDIF
       ENDIF
       CALL OUTAB(23,ROW,II,PURP,DUMMY,IO)
c
C...URBAN RAIL STATION TO DESTINATION ZONE
C
	DO 3620,IT=1,MAX_ZONES
 3620   ROW(IT)=0
	PURP=3
        tnew=0.0
        itot=0
        IF(II.GT.MAX_IZONES) THEN
	IF(STANUM((II-MAX_IZONES)).EQ.2) THEN
	DO 3624,IJ=1,MAX_IZONES
        IN=II-MAX_IZONES
        if(URSTAZ(in,ij).gt.0.0) then
	TEMP=(URSTAZ(In,IJ)*100.0)+rem43
	ROW(ij)=IFIX(TEMP)
	rem43=TEMP-row(ij)
        else
        row(ij)=0
        end if
        tnew=tnew+URSTAZ(in,ij)
        itot=itot+row(ij)
 3624 CONTINUE
       ENDIF
       ENDIF
       CALL OUTAB(23,ROW,II,PURP,DUMMY,IO)
 3518 CONTINUE
C
C
C...EXPRESS BUS STATION TO DESTINATION ZONE
      DO 3722,II=1,MAX_ZONES 
	DO 3720,IT=1,MAX_ZONES
 3720   ROW(IT)=0
	PURP=1
        tnew=0.0
        itot=0
        IF(II.GT.MAX_IZONES) THEN
	IF(STANUM((II-MAX_IZONES)).EQ.3) THEN
	DO 3724,IJ=1,MAX_IZONES
        IN=II-MAX_IZONES
        if(EBSTAZ(in,ij).gt.0.0) then
	TEMP=(EBSTAZ(In,IJ)*100.0)+rem51
	ROW(ij)=IFIX(TEMP)
	rem51=TEMP-row(ij)
        else
        row(ij)=0
        end if
        tnew=tnew+EBSTAZ(in,ij)
        itot=itot+row(ij)
 3724 CONTINUE
       ENDIF
       ENDIF
       CALL OUTAB(25,ROW,II,PURP,DUMMY,IO)
C
C
C...TRANSITWAY STATION TO DESTINATION ZONE
	DO 3730,IT=1,MAX_ZONES
 3730   ROW(IT)=0
	PURP=2
        tnew=0.0
        itot=0
        IF(II.GT.MAX_IZONES) THEN
	IF(STANUM((II-MAX_IZONES)).EQ.4) THEN
	DO 3734,IJ=1,MAX_IZONES
        IN=II-MAX_IZONES
        if(TWSTAZ(in,ij).gt.0.0) then
	TEMP=(TWSTAZ(In,IJ)*100.0)+rem52
	ROW(ij)=IFIX(TEMP)
	rem52=TEMP-row(ij)
        else
        row(ij)=0
        end if
        tnew=tnew+TWSTAZ(in,ij)
        itot=itot+row(ij)
 3734 CONTINUE
       ENDIF
       ENDIF
       CALL OUTAB(25,ROW,II,PURP,DUMMY,IO)
C
C
C...BRT STATION TO DESTINATION ZONE
	DO 3736,IT=1,MAX_ZONES
 3736   ROW(IT)=0
	PURP=3
        tnew=0.0
        itot=0
        IF(II.GT.MAX_IZONES) THEN
	IF(STANUM((II-MAX_IZONES)).EQ.5) THEN
	DO 3737,IJ=1,MAX_IZONES
        IN=II-MAX_IZONES
        if(BRTSTAZ(in,ij).gt.0.0) then
	TEMP=(BRTSTAZ(In,IJ)*100.0)+rem54
	ROW(ij)=IFIX(TEMP)
	rem54=TEMP-row(ij)
        else
        row(ij)=0
        end if
        tnew=tnew+BRTSTAZ(in,ij)
        itot=itot+row(ij)
 3737 CONTINUE
       ENDIF
       ENDIF
       CALL OUTAB(25,ROW,II,PURP,DUMMY,IO)
c
c
C...    ALL DRIVE TO TRANSIT
C
	DO 3819,IT=1,MAX_ZONES
 3819   ROW(IT)=0
	PURP=4
        tnew=0.0
        itot=0
	IF(II.LE.MAX_IZONES) THEN
	DO 3817,IJ=1,MAX_STATIONS  
	JJ=IJ+MAX_IZONES
        if(DTRAN(ii,ij).gt.0.0) then
	TEMP=(DTRAN(II,IJ)*100.0)+rem53
	ROW(JJ)=IFIX(TEMP)
	rem53=TEMP-row(jj)
        else
        row(jj)=0
        end if
        tnew=tnew+DTRAN(ii,ij)
        itot=itot+row(jj)
 3817 CONTINUE
       ENDIF
       CALL OUTAB(25,ROW,II,PURP,DUMMY,IO)
 3722 CONTINUE
c
C
C SUMMARIZE MODAL TRIP VALUES FOR REGION
C
 9200 IF(AIRPASS) THEN
      WRITE(26,9299)
 9299 FORMAT(//1X,' CREATING AIR PASSENGER SKIM DATA OUTPUT'/
     *         1X,' ---------------------------------------'//)
      WRITE(*,9299)
      GO TO 9515
      END IF
      IF(CALIB.AND.(.NOT.HFACT)) GO TO 1505
      WRITE(26,9201)
 9201 FORMAT(//,30X,'R E P O R T   1',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE AND INCOME  LEVEL',//,
     *       1X,'  INCOME   ','         ','    2    ','      3    ',
     *          '   4+   ','         ',
     *          '         ',
     *          '         ','         '/
     *       1X,'  GROUP   ','  DRIVE  ','  PERSON ','    PERSON ',
     *          ' PERSON ','          ','   NON   '/
     *       1X,'  LEVEL  ','   ALONE ','    AUTO ','      AUTO ',
     *          '   AUTO ','  TRANSIT ',' MOTORIZED ',
     *          '    TOTAL      '/
     *       1X,'---------','---------','---------','----------',
     *          '----------','----------',
     *          '----------',
     *          '-----------','----------')
      DO 400 C=1,3
      WRITE(26,9202) C,(TESUM(T,C),T=1,3),TESUM(64,C),TESUM(5,C),
     *                  TESUM(50,C),TESUM(6,C)
 9202 FORMAT(5X,I1,2X,7F10.0)
      DO 401 K=1,64
      TESUM(K,4)=TESUM(K,4) + TESUM(K,C)
      IF(K.LE.14) TOLSUM(K,4)=TOLSUM(K,4) + TOLSUM(K,C)
  401 CONTINUE
  400 CONTINUE
      WRITE(26,9203) (TESUM(T,4),T=1,3),TESUM(64,4),TESUM(5,4),
     *                TESUM(50,4),TESUM(6,4)
 9203 FORMAT(/,2X,'TOTAL',1X,7F10.0)
      IF(NMOT) THEN
      WRITE(26,9901)
 9901 FORMAT(//,30X,'R E P O R T   1A',/,
     *          20X,
     *          'SUMMARIZE NON-MOTORIZED TRIPS BY INCOME  LEVEL',//,
     *       1X,'  INCOME   '/
     *       1X,'  GROUP   ',/
     *       1X,'  LEVEL  ','   WALK  ','   BIKE  ',/
     *       1X,'---------','---------','---------'/)
      DO 9400 C=1,3
      WRITE(26,9902) C,TESUM(51,C),TESUM(52,C)
 9902 FORMAT(5X,I1,2X,2F10.0)
 9400 CONTINUE
      WRITE(26,9903) TESUM(51,4),TESUM(52,4)
 9903 FORMAT(/,2X,'TOTAL',1X,2F10.0)
      END IF
C
C SUMMARIZE AUTO TRIP VALUES FOR REGION
C
      WRITE(26,9225)
 9225 FORMAT(//,30X,'R E P O R T   1B',/,
     *          20X,
     *          'SUMMARIZE AUTO PERSON TRIPS BY INCOME LEVEL',//,
     *       1X,'  INCOME ',' DRIVE   ','   DRIVE ',' 2 PERSON',
     *          ' 2 PERSON',' 2 PERSON',' 2 PERSON',' 3 PERSON',
     *          ' 3 PERSON',' 3 PERSON',' 3 PERSON',' 4 PERSON',
     *          ' 4 PERSON',' 4 PERSON',' 4 PERSON'/
     *       1X,'  GROUP  ',' ALONE   ','   ALONE ',' NON TOLL',
     *          ' NON TOLL','   TOLL  ','   TOLL  ',' NON TOLL',
     *          ' NON TOLL','   TOLL  ','   TOLL  ',' NON TOLL',
     *          ' NON TOLL','   TOLL  ','   TOLL  '/
     *       1X,'  LEVEL  ','NON TOLL ','    TOLL ','    HOV  ',
     *          ' NON HOV ','   HOV   ',' NON HOV ','    HOV  ',
     *          ' NON HOV ','   HOV   ',' NON HOV ','    HOV  ',
     *          ' NON HOV ','   HOV   ',' NON HOV '/
     *       1X,'---------','---------','---------','---------',
     *          '---------','---------','---------','---------',
     *          '---------','---------','---------','---------',
     *          '---------','---------','---------')
      DO 430 C=1,3
      WRITE(26,9302) C,(TOLSUM(T,C),T=1,14)
 9302 FORMAT(5X,I1,3X,14F9.0)
  430 CONTINUE
      WRITE(26,9303) (TOLSUM(T,4),T=1,14)
 9303 FORMAT(/,2X,'TOTAL',2X,14F9.0)
C
C SUMMARIZE TRANSIT TRIP VALUES FOR REGION
C
      WRITE(26,9204)
 9204 FORMAT(//,30X,'R E P O R T   2A',/,
     *          20X,
     *          'SUMMARIZE TRANSIT TRIPS BY INCOME GROUP',//,
     *       1X,' INCOME  ','   LOCAL  ','  LOCAL   ',' EXPRESS  ',
     *                      '  EXPRESS ','TRANSITWAY','TRANSITWAY',
     *                                   '  RAPID   ','  RAPID   ',
     *                                   '   BRT    ','   BRT    ',
     *                                   '   BRT    '/
     *       1X,'  GROUP  ','    BUS   ','   BUS    ','   BUS    ',
     *                      '    BUS   ','   BUS    ','   BUS    ',
     *                                   '   BUS    ','   BUS    ',
     *                                   '          ','          ',
     *                                   '  DRIVE   '/
     *       1X,'  LEVEL  ','    WALK  ','  DRIVE   ','   WALK   ',
     *                      '   DRIVE  ','   WALK   ','  DRIVE   ',
     *                                   '   WALK   ','  DRIVE   ',
     *                                   '   WALK   ','  DRIVE   ',
     *                                   'URBAN RAIL'/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------','----------',
     *                                   '----------','----------',
     *                                   '----------','----------',
     *                                   '----------')
      DO 410 C=1,3
      WRITE(26,9205) C,(TESUM(T,C),T=7,10),(TESUM(L,C),L=19,20),
     *               TESUM(48,C),TESUM(49,C),TESUM(56,C),TESUM(57,C),
     *               TESUM(59,C)
 9205 FORMAT(5X,I1,2X,11F10.0)
      IF(CALIB) THEN
      IF(C.EQ.1) WRITE(67,5210) CITER
 5210 FORMAT(' Calibration Iteration=',i2/
     *       'MODE,LCLWLK,LCLDRV,EXPWLK,EXPDRV,TWYWLK,TWYDRV,RPDWLK,',
     *       'RPDDRV,BRTWLK,BRTDRV,BRTURB')
      WRITE(67,5211) C,(TESUM(T,C),T=7,10),(TESUM(L,C),L=19,20),
     *               TESUM(48,C),TESUM(49,C),TESUM(56,C),TESUM(57,C),
     *               TESUM(59,C)
 5211 FORMAT(I1,11(',',F10.0))
      END IF
  410 CONTINUE
      WRITE(26,9206) (TESUM(T,4),T=7,10),(TESUM(L,4),L=19,20),
     *                TESUM(48,4),TESUM(49,4),TESUM(56,4),
     *                TESUM(57,4),TESUM(59,4)
 9206 FORMAT(/,2X,'TOTAL',1X,11F10.0)
C
C SUMMARIZE AVAILABLE PERSON TRIPS BY TRANSIT MODE AND INCOME
C
      IF(CALIB) THEN
      WRITE(67,5212) (TESUM(T,4),T=7,10),(TESUM(L,4),L=19,20),
     *                TESUM(48,4),TESUM(49,4),TESUM(56,4),
     *                TESUM(57,4),TESUM(59,4)
 5212 FORMAT('TOTAL',11(',',F10.0))
C
C SUMMARIZE TRIPS WITH LOCAL BUS TRANSFER
C
      WRITE(26,6801)
 6801 FORMAT(//,30X,'R E P O R T   2A',/,
     *          20X,
     *        'SUMMARIZE WALK ACCESS TRANSIT TRIPS WITH BUS TRANSFER',
     *        ' BY INCOME GROUP',//,
     *       1X,' INCOME  ','  RAPID   ','TRANSITWAY',' EXPRESS  ',
     *                      '   BRT    '/
     *       1X,'  GROUP  ','   BUS    ','   BUS    ','   BUS    ',
     *                      '   BUS    '/
     *       1X,'  LEVEL  ',' TRANSFERS',' TRANSFERS',' TRANSFERS',
     *                      ' TRANSFERS'/
     *       1X,'---------','----------','----------','----------',
     *          '----------')
      DO 6802 C=1,3
      WRITE(26,9205) C,TESUM(53,C),TESUM(54,C),TESUM(55,C),TESUM(58,C)
 6802 CONTINUE
      WRITE(26,9206) TESUM(53,4),TESUM(54,4),TESUM(55,4),TESUM(58,4)
      WRITE(26,6803)
 6803 FORMAT(//,30X,'R E P O R T   2A',/,
     *          20X,
     *        'SUMMARIZE DRIVE ACCESS TRANSIT TRIPS WITH BUS TRANSFER',
     *        ' BY INCOME GROUP',//,
     *       1X,' INCOME  ','  RAPID   ','TRANSITWAY',' EXPRESS  ',
     *                      '   BRT    '/
     *       1X,'  GROUP  ','   BUS    ','   BUS    ','   BUS    ',
     *                      '   BUS    '/
     *       1X,'  LEVEL  ',' TRANSFERS',' TRANSFERS',' TRANSFERS',
     *                      ' TRANSFERS'/
     *       1X,'---------','----------','----------','----------',
     *          '----------')
      DO 6804 C=1,3
      WRITE(26,9205) C,TESUM(60,C),TESUM(62,C),TESUM(63,C),TESUM(61,C)
 6804 CONTINUE
      WRITE(26,9206) TESUM(60,4),TESUM(62,4),TESUM(63,4),TESUM(61,4)
      END IF
C
C COMMUTER RAIL AND THEN URBAN RAIL
C
      WRITE(26,9207)
 9207 FORMAT(//,30X,'R E P O R T   2B',/,
     *          20X,
     *          'SUMMARIZE COMMUTER RAIL BY INCOME GROUP',//,
     *       1X,' INCOME  ','          ','          ','   PARK   ',
     *                      '   KISS   ','          '/
     *       1X,'  GROUP  ','          ','          ','   AND    ',
     *                      '    AND   ','          '/
     *       1X,'  LEVEL  ','    WALK  ','   BUS    ','   RIDE   ',
     *                      '    RIDE  ','  TOTAL   '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------','----------')
      DO 420 C=1,3
      WRITE(26,9208) C,(TESUM(T,C),T=11,14),TESUM(34,C)
 9208 FORMAT(5X,I1,2X,5F10.0)
      IF(CALIB) THEN
      IF(C.EQ.1) WRITE(67,'(A20)') 'COMMUTER RAIL'
      WRITE(67,5208) C,(TESUM(T,C),T=11,14),TESUM(34,C)
 5208 FORMAT(I1,5(',',F10.0))
      END IF
  420 CONTINUE
      WRITE(26,9209) (TESUM(T,4),T=11,14),TESUM(34,4)
 9209 FORMAT(/,2X,'TOTAL',1X,5F10.0)
      IF(CALIB) THEN
      WRITE(67,5209) (TESUM(T,4),T=11,14),TESUM(34,4)
 5209 FORMAT('TOTAL',5(',',F10.0))
      END IF
C
      WRITE(26,9326)
 9326 FORMAT(//,30X,'R E P O R T   2C',/,
     *          20X,
     *          'SUMMARIZE    URBAN RAIL BY INCOME GROUP',//,
     *       1X,' INCOME  ','          ','          ','   PARK   ',
     *                      '   KISS   ','          '/
     *       1X,'  GROUP  ','          ','          ','   AND    ',
     *                      '    AND   ','          '/
     *       1X,'  LEVEL  ','    WALK  ','   BUS    ','   RIDE   ',
     *                      '    RIDE  ','  TOTAL   '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------','----------')
      DO 421 C=1,3
      WRITE(26,9208) C,(TESUM(T,C),T=15,18),TESUM(47,C)
      IF(CALIB) THEN
      IF(C.EQ.1) WRITE(67,'(A20)') 'URBAN RAIL'
      WRITE(67,5208) C,(TESUM(T,C),T=15,18),TESUM(47,C)
      END IF
  421 CONTINUE
      WRITE(26,9209) (TESUM(T,4),T=15,18),TESUM(47,4)
      IF(CALIB) THEN
      WRITE(67,5209) (TESUM(T,4),T=15,18),TESUM(47,4)
      END IF
      IF(EBUSCR) THEN 
      WRITE(26,6806)
 6806 FORMAT(//,30X,'R E P O R T   2D',/,
     *          20X,
     *          'SUMMARIZE EXPRES BUS TO COMMUTER RAIL BY',
     *          ' INCOME GROUP',//,
     *       1X,' INCOME  '/
     *       1X,'  GROUP  '/
     *       1X,'  LEVEL  ','   TRIPS  '/
     *       1X,'---------','----------')
      DO 6807 C=1,3
      WRITE(26,6810) C,EXPCRT(C)
 6810 FORMAT(5X,I1,2X,5F10.0)
      EXPCRT(4)=EXPCRT(4)+EXPCRT(C)
 6807 CONTINUE
      WRITE(26,6809) EXPCRT(4)
 6809 FORMAT(1X,' TOTAL ',F10.0)
      DO 6808 C=1,4
      EXPCRT(C)=0.0
 6808 CONTINUE
      END IF
      IF(CALIB.OR.TXFSUM) THEN
C...BRT
      XPCT(1)=(MODETRP(5,7,1,1)/TESUM(56,4))*100.0
      XPCT(2)=(MODETRP(5,7,3,2)/TESUM(57,4))*100.0
      WRITE(26,6811) (MODETRP(5,K1,1,1),K1=1,7),XPCT(1),
     *               (MODETRP(5,K2,3,2),K2=1,7),XPCT(2)
 6811 FORMAT(//,30X,'R E P O R T   4A',/,
     *          20X,' BRT -- SUMMARY OF TRIPS USING FEEDER MODES'//
     *          1X,' MODE    LOCAL    RAPID   EXPRESS  TRANSWY',
     *             '    BRT     U-RAIL    ANY  PERCENT'/
     *          1X,' -----  -------  -------  -------  -------',
     *             '  -------  -------  -------  -------'/
     *          1X,' WALK ',7(2X,F7.0),F7.2/
     *          1X,' DRIV ',7(2X,F7.0),F7.2)
C...METRO RAPID
      XPCT(1)=(MODETRP(6,1,1,1)/TESUM(48,4))*100.0
      XPCT(2)=(MODETRP(6,1,1,2)/TESUM(49,4))*100.0
      WRITE(26,6813) (MODETRP(6,K1,1,1),K1=1,6),XPCT(1),
     *               (MODETRP(6,K2,1,2),K2=1,6),XPCT(2)
 6813 FORMAT(//,30X,'R E P O R T   4B',/,
     *          20X,' METRO RAPID -- SUMMARY OF TRIPS USING FEEDER',
     *              ' MODES'//
     *          1X,' MODE    LOCAL    RAPID   EXPRESS  TRANSWY',
     *             '    BRT     U-RAIL  PERCENT'/
     *          1X,' -----  -------  -------  -------  -------',
     *             '  -------  -------  -------'/
     *          1X,' WALK ',6(2X,F7.0),F7.2/
     *          1X,' DRIV ',6(2X,F7.0),F7.2)
C...URBAN RAIL
      WRITE(26,6812) ((MODETRP(2,K1,K4,1),K1=1,7),K4=3,4),
     *               ((MODETRP(2,K2,K3,2),K2=1,7),K3=1,12)
 6812 FORMAT(//,30X,'R E P O R T   4C',/,
     *      20X,' URBAN RAIL -- SUMMARY OF TRIPS USING FEEDER MODES'//
     *          1X,'ACCESS'/
     *          1X,' MODE    LOCAL    RAPID   EXPRESS  TRANSWY',
     *             '    BRT     U-RAIL    ANY'/
     *          1X,' -----  -------  -------  -------  -------',
     *             '  -------  -------  -------'/
     *          1X,' BUS1 ',7(2X,F7.0)/
     *          1X,' BUS2 ',7(2X,F7.0)//
     *          1X,'EGRESS'/
     *          1X,' MODE    LOCAL    RAPID   EXPRESS  TRANSWY',
     *             '    BRT     U-RAIL    ANY'/
     *          1X,' -----  -------  -------  -------  -------',
     *             '  -------  -------  -------'/
     *          1X,' WLK1 ',7(2X,F7.0)/
     *          1X,' WLK2 ',7(2X,F7.0)/
     *          1X,' BUS1 ',7(2X,F7.0)/
     *          1X,' BUS2 ',7(2X,F7.0)/
     *          1X,' PNR1 ',7(2X,F7.0)/
     *          1X,' PNR2 ',7(2X,F7.0)/
     *          1X,' PNR3 ',7(2X,F7.0)/
     *          1X,' PNR4 ',7(2X,F7.0)/
     *          1X,' KNR1 ',7(2X,F7.0)/
     *          1X,' KNR2 ',7(2X,F7.0)/
     *          1X,' KNR3 ',7(2X,F7.0)/
     *          1X,' KNR4 ',7(2X,F7.0))
       END IF
      IF(CALIB) THEN
      DO 9910 K1=1,5
      WRITE(26,9912) COUNTY(K1)
 9912 FORMAT(//,30X,'R E P O R T   5',/,
     *          20X,
     *          'SUMMARIZE LOCAL BUS TRIPS BY INCOME  LEVEL',/
     *          20X,A12,' COUNTY'//
     *       1X,'  INCOME   '/
     *       1X,'  GROUP   ',/
     *       1X,'  LEVEL  ','   WALK  ','  DRIVE  ',/
     *       1X,'---------','---------','---------'/)
      DO 9911 C=1,3
      WRITE(26,9902) C,LOCBUS(K1,C,1),LOCBUS(K1,C,2)
 9911 CONTINUE
 9910 CONTINUE
      END IF
C---------------------------------------------------------------
      IF(DEBUG) THEN
      WRITE(26,9216) (TESUM(T,4),T=1,49)
 9216 FORMAT(/1X,'TOTAL INTERCHANGE SUMMARY VALUES'/
     *        1X,'--------------------------------'//
     *        1X,'TESUM=',10F9.0/
     *        1X,'      ',10F9.0/
     *        1X,'      ',10F9.0/
     *        1X,'      ',10F9.0/
     *        1X,'      ', 9F9.0)
      END IF
C---------------------------------------------------------------
C
C  SUMMARIZE STATION MODE OF ACCESS DATA
C
      WRITE(26,9210)
 9210 FORMAT(//,30X,'R E P O R T   3A',/,
     *          20X,
     *          'SUMMARIZE COMMUTER RAIL STATION ACCESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','   BUS  ','  PARK  ','  KISS  ',
     *          '        ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' ACCESS ',' ACCESS ','  RIDE  ','  RIDE  ',
     *          '  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','---------',
     *          '------  ')
        TTWLK=0.0
	TTBUS=0.0
	TTPNR=0.0
	TTKNR=0.0
	TTOTAL=0.0
      DO 500 K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      STASUM(K,5)=STASUM(K,1)+STASUM(K,2)+STASUM(K,3)+STASUM(K,4)
        TTWLK=TTWLK+STASUM(K,1)
	TTBUS=TTBUS+STASUM(K,2)
	TTPNR=TTPNR+STASUM(K,3)
	TTKNR=TTKNR+STASUM(K,4)
	TTOTAL=TTOTAL+STASUM(K,5)
      IF(STASUM(K,5).GT.0.1) THEN
	 WRITE(26,9211) KS,STANAME(K),
     *                  (STASUM(K,L),L=1,5)
 9211    FORMAT(2X,I4,3X,A29,1X,5F8.0)
      END IF
  500 CONTINUE
        WRITE(26,9337) TTWLK,TTBUS,TTPNR,TTKNR,TTOTAL
 9337   FORMAT(/3X,'TOTAL',31X,5F8.0)
      WRITE(26,9212)
 9212 FORMAT(//,30X,'R E P O R T   3A',/,
     *          20X,
     *          'SUMMARIZE COMMUTER RAIL STATION EGRESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','   BUS  ',' DRIVE  ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' EGRESS ',' EGRESS ',' EGRESS ','  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','------  ')
      DO 1501 K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      STASUM2(K,4)=STASUM2(K,1)+STASUM2(K,2)+STASUM2(K,3)
      IF(STASUM2(K,4).GT.0.1) THEN
	 WRITE(26,9213) KS,STANAME(K),
     *                  (STASUM2(K,L),L=1,4)
 9213  FORMAT(2X,I4,3X,A29,1X,4F8.0)
      END IF
 1501 CONTINUE
C
      WRITE(26,9310)
 9310 FORMAT(//,30X,'R E P O R T   3B',/,
     *          20X,
     *          'SUMMARIZE URBAN RAIL STATION ACCESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','   BUS  ','  PARK  ','  KISS  ',
     *          '        ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' ACCESS ',' ACCESS ','  RIDE  ','  RIDE  ',
     *          '  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','---------',
     *          '------  ')
C
        TTWLK=0.0
	TTBUS=0.0
	TTPNR=0.0
	TTKNR=0.0
	TTOTAL=0.0
      DO 1502 K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      STASUM(K,10)=STASUM(K,6)+STASUM(K,7)+STASUM(K,8)+STASUM(K,9)
        TTWLK=TTWLK+STASUM(K,6)
	TTBUS=TTBUS+STASUM(K,7)
	TTPNR=TTPNR+STASUM(K,8)
	TTKNR=TTKNR+STASUM(K,9)
	TTOTAL=TTOTAL+STASUM(K,10)
      IF(STASUM(K,10).GT.0.1) THEN
	 WRITE(26,9211) KS,STANAME(K),(STASUM(K,L),L=6,10)
      END IF
 1502 CONTINUE
      WRITE(26,9337) TTWLK,TTBUS,TTPNR,TTKNR,TTOTAL
      WRITE(26,9311)
 9311 FORMAT(//,30X,'R E P O R T   3B',/,
     *          20X,
     *          'SUMMARIZE URBAN RAIL STATION EGRESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','   BUS  ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' EGRESS ',' EGRESS ','  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','------  ')
      DO 1506 K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      STASUM2(K,7)=STASUM2(K,5)+STASUM2(K,6)
      IF(STASUM2(K,7).GT.0.1) THEN
	 WRITE(26,9313) KS,STANAME(K),
     *                  (STASUM2(K,L),L=5,7)
 9313  FORMAT(2X,I4,3X,A29,1X,3F8.0)
      END IF
 1506 CONTINUE
C
C
      IF(TWYSK) THEN
      WRITE(26,9231)
 9231 FORMAT(//,30X,'R E P O R T  3C',/,
     *          20X,
     *         'SUMMARIZE TRANSITWAY BUS STATION ACCESS VOLUMES',//,
     *      1X,'STATION','                                      ',
     *         '  WALK   ','   DRIVE    ','    TOTAL    ',/,
     *      1X,'------------------------------------------------',
     *         '---------','------------','--------------')
        TTWLK=0.0
        TTDRV=0.0
        TTOTAL=0.0
	DO 1503,K=1,MAX_STATIONS
	KS=K+MAX_IZONES
	TEMP=STASUM(K,12)+STASUM(K,11)
	IF(TEMP.GT.0.1) THEN
	TTWLK=TTWLK+STASUM(K,11)
	TTDRV=TTDRV+STASUM(K,12)
	TTOTAL=TTOTAL+TEMP
	WRITE(26,9233) KS,STANAME(K),STASUM(K,11),STASUM(K,12),TEMP
 9233 FORMAT(2X,I4,3X,A29,1X,3(4X,F8.0))
      ENDIF
 1503 CONTINUE
      WRITE(26,9336) TTWLK,TTDRV,TTOTAL
      END IF
C
      WRITE(26,9331)
 9331 FORMAT(//,30X,'R E P O R T  3D',/,
     *          20X,
     *         'SUMMARIZE EXPRESS BUS STATION ACCESS VOLUMES',//,
     *      1X,'STATION','                                      ',
     *         '  WALK   ','   DRIVE    ','    TOTAL    ',/,
     *      1X,'------------------------------------------------',
     *         '---------','------------','--------------')
        TTWLK=0.0
        TTDRV=0.0
        TTOTAL=0.0
	DO 1504,K=1,MAX_STATIONS
	KS=K+MAX_IZONES
	TEMP=STASUM(K,14)+STASUM(K,13)
	IF(TEMP.GT.0.1) THEN
	TTWLK=TTWLK+STASUM(K,13)
	TTDRV=TTDRV+STASUM(K,14)
	TTOTAL=TTOTAL+TEMP
	WRITE(26,9333) KS,STANAME(K),STASUM(K,13),STASUM(K,14),TEMP
 9333 FORMAT(2X,I4,3X,A29,1X,3(4X,F8.0))
      ENDIF
 1504 CONTINUE
      WRITE(26,9336) TTWLK,TTDRV,TTOTAL
C
      IF(BRTSK) THEN
      WRITE(26,9334)
 9334 FORMAT(//,30X,'R E P O R T  3E',/,
     *          20X,
     *         'SUMMARIZE BRT STATION ACCESS VOLUMES',//,
     *      1X,'STATION','                                      ',
     *         '  WALK   ','   DRIVE    ','    TOTAL    ',/,
     *      1X,'------------------------------------------------',
     *         '---------','------------','--------------')
        TTWLK=0.0
        TTDRV=0.0
        TTOTAL=0.0
	DO 1507,K=1,MAX_STATIONS
	KS=K+MAX_IZONES
	TEMP=STASUM(K,17)+STASUM(K,18)
	IF(TEMP.GT.0.1) THEN
	TTWLK=TTWLK+STASUM(K,17)
	TTDRV=TTDRV+STASUM(K,18)
	TTOTAL=TTOTAL+TEMP
	WRITE(26,9335) KS,STANAME(K),STASUM(K,17),STASUM(K,18),TEMP
 9335 FORMAT(2X,I4,3X,A29,1X,3(4X,F8.0))
      ENDIF
 1507 CONTINUE
        WRITE(26,9336) TTWLK,TTDRV,TTOTAL
 9336   FORMAT(/2X,'TOTAL',32X,3(4X,F8.0))
      END IF
 1505 CONTINUE
C
C...URBAN RAIL TRIPS BY INCOME AND NUMBER OF TRANSFERS
C
      IF(TXOUT.OR.DEBUG) THEN
      WRITE(26,9551) ((TXSUM(K1,K2),K2=1,5),K1=1,13)
 9551 FORMAT(//,25X,'REPORT 9'/
     *     1X,'  URBAN RAIL TRIP SUMMARY - INCOME & TRANSFERS'/
     *     1X,'  --------------------------------------------'//
     *     1X,'INCOME TXFERS   WALK       BUS       PNR       KNR',
     *        '      TOTAL'/
     *     1X,'------ ------ --------- --------- --------- ---------',
     *        ' ---------'/
     *     1X,'   1     0    ',5(F9.1,1X)/
     *     1X,'         1    ',5(F9.1,1X)/
     *     1X,'         2+   ',5(F9.1,1X)/
     *     1X,'   2     0    ',5(F9.1,1X)/
     *     1X,'         1    ',5(F9.1,1X)/
     *     1X,'         2+   ',5(F9.1,1X)/
     *     1X,'   3     0    ',5(F9.1,1X)/
     *     1X,'         1    ',5(F9.1,1X)/
     *     1X,'         2+   ',5(F9.1,1X)//
     *     1X,'  TOT    0    ',5(F9.1,1X)/
     *     1X,'         1    ',5(F9.1,1X)/
     *     1X,'         2+   ',5(F9.1,1X)/
     *     1X,'         TOT  ',5(F9.1,1X)/)
      END IF
C
C OUTPUT PERSON TRIPS BY INCOME GROUP
C
      IF(CALIB) THEN
      WRITE(44,9602) CITER
 9602 FORMAT(/' Calibration Iteration=',I4,/,7X,
     *        12X,'PERSON TRIPS',23X,'TRANSIT TRIPS',23X,
     *        'COMMUTER RAIL',24X,' URBAN RAIL ',24X,
     *        ' TRANSITWAY ',24X,'EXPRESS BUS ',27X,
     *        'LOCAL BUS',27X,'RAPID BUS',18X,'BUS RAPID TRANSIT'/
     *        ' Fr  To', 
     *        9(2X,'  INC 1   ',2X,'  INC 2   ',2X,'  INC 3   ')/
     *        '-------',27(2X,'----------'))
      DO 9600 K=1,21
      K1=(K-1)*5
      K2=K*5
      WRITE(44,9601) K1,K2,PTRIP(K,1),PTRIP(K,2),PTRIP(K,3),
     *                     TTRIP(K,1),TTRIP(K,2),TTRIP(K,3),
     *                     TTRIP2(K,1,1),TTRIP2(K,2,1),TTRIP2(K,3,1),
     *                     TTRIP2(K,1,2),TTRIP2(K,2,2),TTRIP2(K,3,2),
     *                     TTRIP2(K,1,3),TTRIP2(K,2,3),TTRIP2(K,3,3),
     *                     TTRIP2(K,1,4),TTRIP2(K,2,4),TTRIP2(K,3,4),
     *                     TTRIP2(K,1,5),TTRIP2(K,2,5),TTRIP2(K,3,5),
     *                     TTRIP2(K,1,6),TTRIP2(K,2,6),TTRIP2(K,3,6),
     *                     TTRIP2(K,1,7),TTRIP2(K,2,7),TTRIP2(K,3,7)
 9601 FORMAT(I3,'-',I3,6(2X,F10.1),21(2X,F10.1))
 9600 CONTINUE
      END IF 
C 
C SUMMARIZE COMMUTER RAIL PNR DRIVE ACCESS RATIO FREQ. DISTR.
C
      OPEN(40,FILE='RAIL.TLF.RPT',STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(40,9558) CITER
 9558 FORMAT(//'CALIBRATION ITERATION =',I4//
     *         ' COMMUTER RAIL RATIO OF PNR DRIVE',
     *         ' ACCESS TO IN-VEHICLE TIME AND DRIVE ACCESS',
     *         ' DISTANCE TO TOTAL DISTANCE'/
     *         ' ------------------------------------------',
     *         '---------------------------'/
     *         ' RATIO     PNR TRIPS     PNR TRIPS'/
     *         ' -----     ---------    ----------')
      DO 9552 K=1,1000
      TEMP=FLOAT(K)/10.0
      IF(CRPNRSUM(K).GT.0.001.OR.CRPNRRAT(K).GT.0.001) 
     *      WRITE(40,9553) TEMP,CRPNRSUM(K),CRPNRRAT(K)
 9553 FORMAT(1X,F5.1,4X,F10.3,4X,F10.3)
 9552 CONTINUE
      WRITE(40,9556)
 9556 FORMAT(//15X,' ZONE TO ZONE DISTANCE FREQUENCY DISTRIBUTION',/,
     *       15X,' --------------------------------------------'/
     *       '             COMMUTER RAIL        ',
     *       '               URBAN RAIL'/     
     *       ' DIST    WALK   BUS    PNR    KNR ',4X,
     *       ' WALK     BUS    PNR    KNR ',/,
     *       ' ----   -----  -----  -----  -----',4X,
     *       ' -----   -----  -----  -----')
      DO 9554 K=1,50
      WRITE(40,9557) K,(ZNEDIST(K1,K),K1=1,8)
 9557 FORMAT(2X,I2,3X,4(2X,F5.0),3X,4(2X,F5.0))
 9554 CONTINUE
      WRITE(40,9559)
 9559 FORMAT(//
     *      15X,' STATION TO STATION DISTANCE FREQUENCY DISTRIBUTION'/
     *      15X,' --------------------------------------------------'/
     *       '             COMMUTER RAIL        ',
     *       '               URBAN RAIL'/     
     *       ' DIST    WALK   BUS    PNR    KNR ',4X,
     *       ' WALK     BUS    PNR    KNR ',/,
     *       ' ----   -----  -----  -----  -----',4X,
     *       ' -----   -----  -----  -----')
      DO 9560 K=1,50
      WRITE(40,9561) K,(STADIST(K1,K),K1=1,8)
 9561 FORMAT(2X,I2,3X,4(2X,F5.0),3X,4(2X,F5.0))
 9560 CONTINUE
C
C SUMMARIZE DRIVE TO EXPRESS BUS AND TRANSITWAY
C DISTANCE AND TIME FROM ZONE TO STATION
C
      OPEN(38,FILE='TWYEXP.CSV',STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(38,9562)
 9562 FORMAT(',,',6(',EXPBUS'),6(',TWAY'),6(',BRT')/
     *       ',,',6(',INC1,INC2,INC3')/
     *       'FROM,-,TO',3(',DIST'),3(',TIME'),3(',DIST'),3(',TIME'),
     *                   3(',DIST'),3(',TIME'))
      DO 9563 K=1,100
      WRITE(38,9564) (K-1),K,
     *               TWYEXP(K,1,1,1),TWYEXP(K,2,1,1),TWYEXP(K,3,1,1),
     *               TWYEXP(K,1,2,1),TWYEXP(K,2,2,1),TWYEXP(K,3,2,1),
     *               TWYEXP(K,1,1,2),TWYEXP(K,2,1,2),TWYEXP(K,3,1,2),
     *               TWYEXP(K,1,2,2),TWYEXP(K,2,2,2),TWYEXP(K,3,2,2),
     *               TWYEXP(K,1,1,3),TWYEXP(K,2,1,3),TWYEXP(K,3,1,3),
     *               TWYEXP(K,1,2,3),TWYEXP(K,2,2,3),TWYEXP(K,3,2,3)
 9564 FORMAT(I3,',-,',I3,18(',',F9.3))
      DO K1=1,3
      DO K2=1,2
      DO K3=1,3
      TWYEXP(K,K1,K2,K3)=0.0
      ENDDO
      ENDDO
      ENDDO
 9563 CONTINUE
C
C SUMMARIZE DRIVE TO EXPRESS BUS AND TRANSITWAY
C DRIVE ACCESS RATIO
C
      OPEN(39,FILE='TWYEXP2.CSV',STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(39,9565)
 9565 FORMAT(',',3(',EXPBUS'),3(',TWAY'),3(',BRT')/
     *       'RATIO',3(',INC1,INC2,INC3'))
      DO 9566 K=1,100
      TEMP=FLOAT(K)/10.0
      WRITE(39,9567) TEMP,
     *               TWYEXP2(K,1,1),TWYEXP2(K,2,1),TWYEXP2(K,3,1),
     *               TWYEXP2(K,1,2),TWYEXP2(K,2,2),TWYEXP2(K,3,2),
     *               TWYEXP2(K,1,3),TWYEXP2(K,2,3),TWYEXP2(K,3,3)
 9567 FORMAT(F4.1,9(',',F9.3))
      DO K1=1,3
      DO K2=1,3
      TWYEXP2(K,K1,K2)=0.0
      ENDDO
      ENDDO
 9566 CONTINUE
C     OPEN(70,FILE='PNRSUM.CSV',STATUS='UNKNOWN',
C    *        FORM='FORMATTED')
C     WRITE(70,7070) PNRTRP
C7070 FORMAT(2X,F7.1,7(',',F7.1))
C
C
C SELF-CALIBRATION BIAS CONSTANT CALIBRATION
C
      IF(CALIB) THEN
	 CITER=CITER+1
          IF (CITER.EQ.1) THEN
          OPEN(7,FILE='CONSTANT.OUT',STATUS='UNKNOWN',FORM='FORMATTED')
          OPEN(77,FILE='TRN_CONSTANT.CSV',STATUS='UNKNOWN')
          END IF
	 CALL SCALIB(CITER,TESUM,CALSUM,stasum,
     *               HHFACT,KKUR,STRHBW,PTRIP,TTRIP,CBDTRAN)
      IF (CITER.GT.MAXCALIT) GO TO 9515
      IF (CITER.EQ.MAXCALIT) TRIPSOUT=.TRUE.
      GO TO 8001
      END IF
C
C ITERATION COUNTER FOR STATION PARKING CAPACITY RESTRAINT
C
      IF(CAPRES) THEN
      ITER=ITER+1
      IF(ITER.LE.NITER) THEN
      WRITE(*,8005) ITER
      WRITE(26,8005) ITER
 8005 FORMAT(/1X,'Station Capacity Restraint Iteration=',I2)
      CALL STACAP(ITER,stasum,EQUIV)
      ENDIF
      ENDIF
C
C  WRITE OUT FINAL SHADOW PRICE & NUMBER OF DRIVE TRIPS PER STATION
C  FOR INPUT IN SUBSEQUENT RUNS
C
      IF(ITER.EQ.NITER) THEN
       OPEN(11,FILE=SHADOUT,FORM='FORMATTED',STATUS='UNKNOWN')
       OPEN(9,FILE=PKUTOUT,FORM='FORMATTED',STATUS='UNKNOWN')
C..SHADOW PRICE
       DO 411,NS=1,MAX_STATIONS
       NSTA=NS+MAX_IZONES
       IF(STADATA(NS,6).gt.0.0) then
       WRITE(11,8333) NSTA,STADATA(NS,5)
 8333  FORMAT(2x,i4,2x,f6.0)
C..SPACES USED
       USED=(STASUM(NS,15)/2.0)+(STADATA(NS,4)-STADATA(NS,3))
       IUSED=IFIX(USED)
       WRITE(9,8335) NSTA,IUSED
 8335  FORMAT(2X,I4,2X,I5)
       ENDIF
  411  CONTINUE
       close(11,status='keep')
       close(9,status='keep')
      ENDIF
C
C OUTPUT SELECT LINK FREQUENCY DISTRIBUTION
C DISTRICT-TO-DISTRICT SUMMARY
C
      IF(RAILSEL) THEN
C....LINK FREQUENCY SUMMARY
      OPEN(121,FILE=SLINKTLF,STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(121,8856) 
 8856 FORMAT('LINKS,STA_STA,ZONE_STA,STA_ZONE')
      DO K=1,15
      WRITE(121,8857) K,(SELTRIPS(K,K1),K1=1,3)
 8857 FORMAT(I2,3(',',F8.2))
      END DO
C.....DISTRICT SUMMARY
      OPEN(123,FILE=SLINKDIST,STATUS='UNKNOWN',FORM='FORMATTED')
      DO C=1,3
      DO K=1,MAXDIST
        DO K1=1,MAXDIST
        SELDIST(21,K1,C)=SELDIST(21,K1,C)+SELDIST(K,K1,C)
        SELDIST(K,21,C)=SELDIST(K,21,C)+SELDIST(K,K1,C)
        SELDIST(21,21,C)=SELDIST(21,21,C)+SELDIST(K,K1,C)
        END DO
      END DO
      WRITE(123,8858) SNAME(C),(DNAME(K),K=1,MAXDIST),DNAME(21)
 8858 FORMAT(A20,/,'DISTRICT',21(',',A20))
      DO K=1,MAXDIST
      WRITE(123,8859) DNAME(K),(SELDIST(K,K1,C),K1=1,MAXDIST),
     *                SELDIST(K,21,C)
 8859 FORMAT(A20,21(',',F8.1))
      END DO
      WRITE(123,8859) DNAME(21),(SELDIST(21,K1,C),K1=1,MAXDIST),
     *                SELDIST(21,21,C)
      END DO
      END IF
C
C RESET ARRAYS FOR NEXT ITERATION
C
 8001  CONTINUE
       DO 350 C=1,64
       DO 351 K=1,4
       TESUM(C,K)=0.0
  351  CONTINUE
  350  CONTINUE
       DO 352 C=1,MAX_STATIONS
       DO 353 K=1,18
       STASUM(C,K)=0.0
  353  CONTINUE
  352  CONTINUE
       DO 355 C=1,8
       DO 355 K=1,50
       STADIST(C,K)=0.0
       ZNEDIST(C,K)=0.0
  355  CONTINUE
       DO 356,C=1,20
	 DO 356,K=1,8
	 WLKPER(C,K)=0.0
  356  CONTINUE
       DO 357 C=1,MAX_IZONES
         DO 358 K=1,MAX_STATIONS
         BCR(C,K)=0.0
         BUR(C,K)=0.0
         DTRAN(C,K)=0.0
         CRSTAZ(K,C)=0.0
         URSTAZ(K,C)=0.0
         EBSTAZ(K,C)=0.0
         TWSTAZ(K,C)=0.0
         BRTSTAZ(K,C)=0.0
        IF(C.GT.MAX_STATIONS) GO TO 358
         CRSS(C,K)=0.0
         URSS(C,K)=0.0
  358    CONTINUE
  357 CONTINUE
       DO 359 C=1,3
       DO 360 K=1,21
       PTRIP(K,C)=0.0
       TTRIP(K,C)=0.0
       DO 361 K1=1,7
       TTRIP2(K,C,K1)=0.0
  361  CONTINUE
  360  CONTINUE
  359  CONTINUE
       DO 362 C=1,100
       DO 362 K=1,2
       DO 362 K1=1,3
	 DO 362 K2=1,3
       TWYEXP(C,K2,K,K1)=0.0
  362  CONTINUE
        DO 1330 K1=1,6
        DO 1331 K2=1,7
        DO 1332 K3=1,12
        DO 1333 K4=1,2
        MODETRP(K1,K2,K3,K4)=0.0
 1333   CONTINUE
 1332   CONTINUE
 1331   CONTINUE
 1330   CONTINUE
        STATRIPS=0.0
        STATRIPS2=0.0
        STATRIPS3=0.0
        SELDIST=0.0
C
C     DELETE DEBUG WORK FILES
C
      IF(DEBUG) THEN
      CLOSE(31,STATUS='DELETE')
      CLOSE(27,STATUS='DELETE')
      CLOSE(33,STATUS='DELETE')
      CLOSE(34,STATUS='DELETE')
      CLOSE(32,STATUS='DELETE')
      END IF
C
C PROGRAM COMPLETION
C
 9515 CONTINUE
C     CALL itime_(IARRAY2)
C     WRITE(26,8004) IARRAY2
C     WRITE(*,8004) IARRAY2
       CALL GETTIM(IHR,IMIN,ISEC,I100)
       WRITE(26,8004) IHR,IMIN,ISEC
       WRITE(*,8004) IHR,IMIN,ISEC
 8004  FORMAT(/' Program Completed: ',I2,':',I2,':',I2)
      END
