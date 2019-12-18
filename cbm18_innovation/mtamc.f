      PROGRAM MTAMC
C      
C      
C---------------------------------------------------------------------
C             NESTED LOGIT MODE CHOICE MODEL APPLICATION PROGRAM                                                       
C             WITH STATION CHOICE FOR URBAN & COMMUTER RAIL & BRT
C
C            PARSONS BRINCKERHOFF QUADE & DOUGLAS, INC.
C                 SAN FRANCISCO, CALIFORNIA
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
      INTEGER*4     IARRAY2(3),TIZ
      INTEGER*2     IZ,JZ,C,M,ITEMP,ITEMP2,II,IN,IJ,JJ,MT,E,KT,MT2,ISTA
      INTEGER*2     ITEMP1,URIND(14),JJZ
      INTEGER*2     HOV3TOL,HOV3NTL,HOV2TOL,HOV2NTL,DIZ,DJZ,PJZ,CJZ,AJZ
      INTEGER*2     HOV4TOL,HOV4NTL,HTLIND
      INTEGER*2     IMODE,CSTAE,CDSTAE,EQUIV(MAX_STATIONS)
      INTEGER*2     MODINC(23),CCSTAE,CCSTA,CCDSTA
      INTEGER*2     CBSTA,CBDSTA,CBSTAE,CWEXP,CWTWY,CBTWY,CBEXP
      INTEGER*2     LAXSTAP(50,50,34),LAXSTAA(50,50,25),PARKIND,JZIND
      INTEGER*2     LAXRNTP(10,50,34),LAXRNTA(10,50,25)
      INTEGER*2     LAXITFP(50,34),LAXITFA(50,34)
      INTEGER*2     LAXITF2P(50,34),LAXITF2A(50,34)
      INTEGER*2     FLYAIND,RNTLIND,LBUSIND
      INTEGER*2     LAXSTAFP(MAX_IZONES,10,34)
      INTEGER*2     LAXSTAFA(MAX_IZONES,10,25)
      INTEGER*2     INDAPM(MAX_ZONES,MAX_ZONES)
      INTEGER*2     YINDEX,ZINDEX,AV,AV4,AVCV,APRKZONE(4000)                   !Innovation
      INTEGER*2     CNTYORG,CNTYDST,CNTYINDX,VMTCODE(2,2),CNTYPRK              !Innovation
C
      INTEGER*2     ZINDCR(MAX_STATIONS),ZINDUR(MAX_STATIONS)
      INTEGER*2     ZINDBR(MAX_STATIONS)
      INTEGER*2     ORISTA,DESSTA,TSTA,TDSTA,SC1,SC2,SC3,SC4,SC5,SC
      INTEGER*2     WSTA(5,5),WDSTA(10,5),BSTA(5,2),BDSTA(10,2),
     *              BIKSTA(5,5),BIKDSTA(10,5),
     *              DSTA(5,10),IUSED,OSTA(5,14),ASTA(5,14),ASTA2(5,14),
     *              BTXFER(5,2),IC,DC,KTX,KTY,LDIST
      INTEGER*2     BUSMODE(5,4,2),BESTCRI,AIRPAS(2)
      INTEGER*2     PSTA(5,10),KSTA(5,10),PDSTA(10,10),KDSTA(10,10)
      INTEGER*2     IX,T,KS,L,CITER,ITER,CSTAT,CDSTAT
      INTEGER*2     CSTABRT,CDSTABRT,MODEINC(6,6,14,2)
      INTEGER*2     STAZNEI(MAX_STATIONS,MAX_IZONES,6,6)
      INTEGER*2     STAACC,ACCSTA,FRMODE(6),TOMODE(6)
      INTEGER*2     CLOSEZONE(MAX_STATIONS)
      INTEGER*2     TMESAV(10),STAEXP(50),STATWY(50)
C
      INTEGER*4     K,K1,K2,PINDEX,TINDEX,QINDEX,INDEX,OINDEX,DINDEX
      INTEGER*4     ROW(MAX_ZONES),LRTWLK(MAX_ZONES)
      INTEGER*4     LRTDRV(MAX_ZONES),ALLOC_VAL,MXZONES
      INTEGER*4     STAZONE(MAX_STATIONS,MAX_IZONES,2)
      INTEGER*4     ZNELOT(MAX_IZONES,50,2),FLYZNE(10,12)
      INTEGER*4     BUSTAT(4,MAX_ZONES)
      INTEGER*4     ZNERNT(MAX_IZONES,10,2)
      INTEGER*4     TAB8DA(MAX_ZONES)
      INTEGER*4     TAB102P(MAX_ZONES),TAB152P(MAX_ZONES)
      INTEGER*4     TAB103P(MAX_ZONES),TAB153P(MAX_ZONES)
      INTEGER*4     TAB104P(MAX_ZONES),TAB154P(MAX_ZONES)
      INTEGER*4     TAB162P(MAX_ZONES),TAB163P(MAX_ZONES)
      INTEGER*4     TAB164P(MAX_ZONES)
      INTEGER*4     TXFER(10,MAX_ZONES)
C
      REAL*4        WUTIL(10,5),WDIST(5,5),BUTIL(10,2),BDIST(5,2),TUTIL
      REAL*4        BIKDIST(5,5),BIKUTIL(10,5),LNINTENP,LNINTENA
      REAL*4        PDIST(5,10),PUTIL(10,10),KDIST(5,10),KUTIL(10,10)
      REAL*4        URBSKIM(2,16),CRBSKIM(2,16),BRTSKIM(2,16)
      REAL*4        USED,WLKM1,WLKM2,WLKM,NHHD(3),TOTHH
      REAL*4        WLKBACC,WLKBEGR,WLKCACC,WLKCEGR,WLKWTACC
      REAL*4        BIKBACC,BIKBEGR,BIKRACC,BIKREGR,BIKWTACC,BIKWTEGR
      REAL*4        WLKWBRTACC,WLKWBRTEGR,BIKBRTACC,BIKBRTEGR
      REAL*4        WLKRACC,WLKREGR,WRUTL,EGRWALK
      REAL*4        WLKWTEGR,diff,rem,temp,AVG1,AVG2,AVG3,AVG4
      REAL*4        WBUTL,WCUTL,DCUTL,DCUTLCR,WTUTL,WTRAT(11)
      REAL*4        DBRTUTL,ZSTATIME(2)
      REAL*4        BBUTL,BRUTL,BTUTL,BCUTL,BBRTUTL
      REAL*4        ZHHD(34,MAX_IZONES)
      REAL*4        WALKACC(10,MAX_ZONES),FARE(10,MAX_ZONES)
      REAL*4        WALKEGR(10,MAX_ZONES),WALKTFR(10,MAX_ZONES)
      REAL*4        TIVT(10,MAX_ZONES),WAIT1(10,MAX_ZONES)
      REAL*4        WAIT2(10,MAX_ZONES),RIVT(10,MAX_ZONES)
      REAL*4        BIVT(10,MAX_ZONES)
      REAL*4        WIVT(10,MAX_ZONES),EIVT(10,MAX_ZONES)
      REAL*4        LUNRELM(MAX_ZONES,28),NUNRELM(MAX_ZONES,28)
      REAL*4        LCROWDMBK(MAX_ZONES,15),NCAPACMBK(MAX_ZONES,15)
      REAL*4        LUNRELMBK(MAX_ZONES,15),NUNRELMBK(MAX_ZONES,15)
      REAL*4        LCROWDM(MAX_ZONES,28),NCAPACM(MAX_ZONES,28)
      REAL*4        K_PUBPRK,NHBTRP(MAX_STATIONS,4)
      REAL*4        INTRADST(MAX_ZONES),INTRAUTL(4),INTRATRP(8,6)
      REAL*4        INTRAEXP(4),INTRAPRB(4)
      
      REAL*8        UTIL0NT,UTIL0T,UTILWK,UTILBK,UTLZBIK,UTILSC
      REAL*8        UTIL2NT,UTIL2T,UTIL3NT,UTIL3T,UTIL4T,UTIL4NT
      REAL*8        UTIL2NTH,UTIL2TH,UTIL3NTH,UTIL3TH,UTIL4NTH,UTIL4TH
      REAL*8        SAV0T,SAVAL0,SUTIL0,SAV2T,SAVAL2,SUTIL2
      REAL*8        SAV3T,SAVAL3,SUTIL3,SAV4T,SAVAL4,SUTIL4
      REAL*8        UTILOT(50,2),ULOTWLK(50),ULOTSHL(50),ULOTTRN(50)
      REAL*8        LOTA,LOTB,LOTC,UTLSBIK
      REAL*8        URNTSHL(10),URNTTRN(10),EUTLRNT(10,2),PROBRNT(10,2)
      REAL*8        PROBLOT(50),EUTILOT(50,4),LOTRIPS(50,4)
      REAL*8        EPROBLOT(50,3),LSLOTE,LSLOT
      REAL*8        PRKCST,PSPACES,SHCOST,SFACTR,FLYTRIPS(10,6),TFLY
      REAL*8        FLYATRIPS(10,8),VCRATIO,SHDPRICE
      REAL*4        MWALKW(5,2),MWALKB(5,2),PNRRAT2,MWALKBIK(5,2)
      REAL*4        NOWALK,TDIST,PNRRAT,CRPNR(10),CRDUM(10),CRPNR2(10)
      REAL*4        PERIN(5,MAX_ZONES),MWALK(7),TWALK(2),TSHAR(2)
      REAL*4        PERSON(MAX_ZONES),MAXWALK,LNWALK(6)
      REAL*4        BWALK,RWALK,CWALK,DWALK,DBWLK,KDRIV,DRWLK
      REAL*4        BWALK1,BWALK2,RWALK1,RWALK2,CWALK1,CWALK2
      REAL*4        WALKT,BIKET
      REAL*4        TWWALK1,TWWALK2,WALK1,WALK2,BIKE1,BIKE2
      REAL*4        CWPROB(3),CBPROB(3),CPPROB(5),CKPROB(5)
      REAL*4        UWPROB(3),UBPROB(3),UPPROB(5),UKPROB(5)
      REAL*4        CUPROB(6),LSCUR,UUPROB(6),LSUUR,BRUPROB(6),LSUBR
      REAL*4        CBKPROB(3),UBKPROB(3)
      REAL*4        BRWPROB(3),BRBPROB(3),BRPPROB(5),BRKPROB(5)
      REAL*4        BRBKPROB(3),LSBRWLK,LSBRBUS,LSBRPR,LSBRKR,LSBRBIK
      REAL*4        BWPROB(3),NMPROB(3)
      REAL*4        DAPROB(2),P2PROB(4),P3PROB(4)
      REAL*4        KCCBD(7),P4PROB(4)
      REAL*8        TRNLSUM(MAX_ZONES,7,7,5)
      REAL*8        ETRNLSUM(7)
C
      REAL*8        TRIPS(89,MAX_ZONES),LSHARE,AVTRIPS(15,MAX_ZONES)
      REAL*8        FAREVAL(23),MISTRP(6),TBRTEGR
      REAL*8        TOLSUM(28,6),CRPNRSUM(1001),SAVFLY(10,13)
      REAL*8        PERTRP,TESUM(90,6),CALSUM(4,6),TAXISUM(6)
      REAL*8        CRPNRRAT(1001),XTESUM(42,6),RTEMP,POBS,PEST,NCON
      REAL*8        PTRIP(21,5),TTRIP(21,5),TTRIP2(21,5,7)
      REAL*8        STXFER(3,4,MAX_ZONES),XTRIP(MAX_ZONES)
      REAL*8        TWYEXP(101,5,2,2),TWYEXP2(100,5,2),MDETRP(12)
      REAL*8        LSCWLK,LSCBUS,LSCPR,LSCKR,LSCR,LSCBIK
      REAL*8        LSUWLK,LSUBUS,LSUPR,LSUKR,LSURB,LSUBIK,LSSHR
      REAL*8        LSLOC,LSEXP,LSWAY,LSTRN,LSDA,LSMOT,LSRPD,LSBRT
      REAL*8        LSAUTO,STASUM(MAX_STATIONS,27),TTWLK,TTDRV,TTOTAL
      REAL*8        MODETRP(6,7,12,2),XPCT(2),TTBIK
      REAL*8        STASUM2(MAX_STATIONS,5),STASUM3(MAX_STATIONS,7,5)
      REAL*8        STASUM4(MAX_STATIONS,6),STASUM5(MAX_STATIONS,5)
      REAL*8        LS3PER,LS2PER,LSNMOT,TTBUS,TTPNR,TTKNR,LS4PER
      REAL*4        BUSPROB(3),EXPPROB(3),RPDPROB(3),BRTPROB(3)
      REAL*4        SRPROB(3),TRNPROB(8)
      REAL*4        ATPROB(2),MOTOR(4)                                         !Innovation
      REAL*4        HHLDAV(4),UBERAV(4)                                        !Innovation
      REAL*4        ADIST,PWALK(MAX_ZONES,2)
      REAL*4        BRTEGR(MAX_STATIONS,MAX_IZONES)
      REAL*4        STASTA(5,MAX_STATIONS,MAX_STATIONS)
      REAL*4        STASTA2(3,MAX_STATIONS,MAX_STATIONS)
      REAL*4        STAZNE(4,MAX_STATIONS,MAX_IZONES)
      REAL*4        STAEGR(4,MAX_STATIONS,MAX_IZONES)
      REAL*4        NHBACC(MAX_STATIONS,MAX_STATIONS)
      REAL*4        NHBUTL(MAX_STATIONS,MAX_STATIONS)
      REAL*4        SSDIST(MAX_STATIONS,MAX_STATIONS)
      REAL*4        TAB1DA(MAX_ZONES),TAB2DA(MAX_ZONES)
      REAL*4        TAB3DA(MAX_ZONES)
      REAL*4        TAB4DA(MAX_ZONES),TAB5DA(MAX_ZONES)
      REAL*4        TAB6DA(MAX_ZONES),TAB7DA(MAX_ZONES)
      REAL*4        TAB12P(MAX_ZONES),TAB22P(MAX_ZONES)
      REAL*4        TAB32P(MAX_ZONES),TAB42P(MAX_ZONES)
      REAL*4        TAB52P(MAX_ZONES),TAB62P(MAX_ZONES)
      REAL*4        TAB72P(MAX_ZONES),TAB82P(MAX_ZONES)
      REAL*4        TAB92P(MAX_ZONES),TAB112P(MAX_ZONES)
      REAL*4        TAB122P(MAX_ZONES),TAB132P(MAX_ZONES)
      REAL*4        TAB142P(MAX_ZONES)
      REAL*4        TAB13P(MAX_ZONES),TAB23P(MAX_ZONES)
      REAL*4        TAB33P(MAX_ZONES),TAB43P(MAX_ZONES)
      REAL*4        TAB53P(MAX_ZONES),TAB63P(MAX_ZONES)
      REAL*4        TAB73P(MAX_ZONES),TAB83P(MAX_ZONES)
      REAL*4        TAB93P(MAX_ZONES),TAB113P(MAX_ZONES)
      REAL*4        TAB123P(MAX_ZONES),TAB133P(MAX_ZONES)
      REAL*4        TAB143P(MAX_ZONES)
      REAL*4        TAB14P(MAX_ZONES),TAB24P(MAX_ZONES)
      REAL*4        TAB34P(MAX_ZONES),TAB44P(MAX_ZONES)
      REAL*4        TAB54P(MAX_ZONES),TAB64P(MAX_ZONES)
      REAL*4        TAB74P(MAX_ZONES),TAB84P(MAX_ZONES)
      REAL*4        TAB94P(MAX_ZONES),TAB114P(MAX_ZONES)
      REAL*4        TAB124P(MAX_ZONES),TAB134P(MAX_ZONES)
      REAL*4        TAB144P(MAX_ZONES)
      REAL*4        AVHTIME(MAX_ZONES),AVTTIME(MAX_ZONES)
      REAL*4        AVH2TIME(MAX_ZONES),AVT2TIME(MAX_ZONES)
      REAL*4        AVH3TIME(MAX_ZONES),AVT3TIME(MAX_ZONES)
	    REAL*4, ALLOCATABLE : : EMATX(:)
	    REAL*4        FROW(MAX_ZONES),MFVAL
C
      REAL*8        TTRAN,TAUTO,TDRV0,TDSHR
      REAL*8        TNMOT,TNMWK,TNMBK,TNMSC
      REAL*8        TCR,TEXP,TLOC,TUR,TWAY,TDRV0N,TDRV0T
      REAL*8        TRPD,TRPDW,TRPDD,TRPDB,TBRT,TBRTW
      REAL*8        TCRU,TURU,TBRTU
      REAL*8        TDRV2,TDRV3,TDRV2N,TDRV2T,TDRV3N,TDRV3T
      REAL*8        TLOCW,TLOCD,TEXPW,TEXPD,TLOCB,TEXPB,TBRTB
      REAL*8        TCRW,TCRB,TCRP,TCRK,TCRW1,TCRW2,TCRBK1,TCRBK2
      REAL*8        TCRB1,TCRB2,TCRP1,TCRP2,TCRP3,TCRP4,TCRBK
      REAL*8        TCRK1,TCRK2,TCRK3,TCRK4,TCRT(18),TURT(18)
      REAL*8        TCRU1,TCRU2,TCRU3,TCRU4
      REAL*8        TURW,TURB,TURP,TURK,TURW1,TURW2,TURBK1,TURBK2
      REAL*8        TURB1,TURB2,TURP1,TURP2,TURP3,TURP4,TURBK
      REAL*8        TURK1,TURK2,TURK3,TURK4
      REAL*8        TURU1,TURU2,TURU3,TURU4
      REAL*8        TBRTU1,TBRTU2,TBRTU3,TBRTU4
      REAL*8        TWAYW,TWAYD,TWAYB
      REAL*8        TBRTBK,TBRTP,TBRTK,TBRTW1,TBRTW2
      REAL*8        TBRTB1,TBRTB2,TBRTP1,TBRTP2,TBRTP3,TBRTP4
      REAL*8        TBRTK1,TBRTK2,TBRTK3,TBRTK4,TBRTT(18)
      REAL*8        TDRV2NH,TDRV2NN,TDRV2TH
      REAL*8        TDRV2TN,TDRV3NH,TDRV3NN
      REAL*8        TDRV3TH,TDRV3TN,TDRV4TH,TDRV4TN
      REAL*8        TDRV4,TDRV4N,TDRV4T,TDRV4NH,TDRV4NN
C
      REAL*4        TOLSAV0,HOVSAV2,BCR(MAX_IZONES,MAX_STATIONS)
      REAL*4        BUR(MAX_IZONES,MAX_STATIONS),YTOLDST
      REAL*4        CRWLK(MAX_STATIONS,MAX_IZONES)
      REAL*4        URWLK(MAX_STATIONS,MAX_IZONES)
      REAL*4        BRTWLK(MAX_STATIONS,MAX_IZONES)
      REAL*4        ALLWLK(MAX_STATIONS,MAX_IZONES)
      REAL*4        CRTNC(MAX_STATIONS,MAX_IZONES)
      REAL*4        URTNC(MAX_STATIONS,MAX_IZONES)
      REAL*4        BRTTNC(MAX_STATIONS,MAX_IZONES)
      REAL*4        ALLTNC(MAX_STATIONS,MAX_IZONES)
      REAL*4        WLKCR(MAX_IZONES,MAX_STATIONS)
      REAL*4        BIKCR(MAX_IZONES,MAX_STATIONS)
      REAL*4        WLKUR(MAX_IZONES,MAX_STATIONS)
      REAL*4        BIKUR(MAX_IZONES,MAX_STATIONS)
      REAL*4        WLKBRT(MAX_IZONES,MAX_STATIONS)
      REAL*4        BBRT(MAX_IZONES,MAX_STATIONS)
      REAL*4        BIKBRT(MAX_IZONES,MAX_STATIONS)
      REAL*4        DTRAN(MAX_IZONES,MAX_STATIONS)
      REAL*4        CRSS(MAX_STATIONS,MAX_STATIONS)
      REAL*4        URSS(MAX_STATIONS,MAX_STATIONS)
      REAL*4        BRTSS(MAX_STATIONS,MAX_STATIONS)
      REAL*4        CRURBRTSS(MAX_STATIONS,MAX_STATIONS)
      REAL*4        CRSSL(MAX_STATIONS,MAX_STATIONS)
      REAL*4        URSSL(MAX_STATIONS,MAX_STATIONS)
      REAL*4        BRTSSL(MAX_STATIONS,MAX_STATIONS)
      REAL*4        CRURBRTSSL(MAX_STATIONS,MAX_STATIONS)
      REAL*4        EBSTAZ(MAX_STATIONS,MAX_IZONES)
      REAL*4        TWSTAZ(MAX_STATIONS,MAX_IZONES)
      REAL*4        BRTSTAZ(MAX_STATIONS,MAX_IZONES)
      REAL*4        CRSTAZ(MAX_STATIONS,MAX_IZONES)
      REAL*4        URSTAZ(MAX_STATIONS,MAX_IZONES)
      REAL*4        ALLSTAZ(MAX_STATIONS,MAX_IZONES)
      REAL*4        WCR(50,MAX_STATIONS),LCR
      REAL*4        WUR(50,MAX_STATIONS),LUR
      REAL*4        WBRT(50,MAX_STATIONS),LBRT
      REAL*4        LCRSS(MAX_STATIONS,MAX_STATIONS)
      REAL*4        LURSS(MAX_STATIONS,MAX_STATIONS)
      REAL*4        LBRTSS(MAX_STATIONS,MAX_STATIONS)
      REAL*4        LCRSTAZ(MAX_STATIONS,50)
      REAL*4        LURSTAZ(MAX_STATIONS,50)
      REAL*4        LBRTSTAZ(MAX_STATIONS,50)
      REAL*4        LOTTRN(50,50,7)
      REAL*4        WCRR(10,MAX_STATIONS),LCRR
      REAL*4        WURR(10,MAX_STATIONS),LURR
      REAL*4        WBRTR(10,MAX_STATIONS),LBRTR
      REAL*4        LCRSSR(MAX_STATIONS,MAX_STATIONS)
      REAL*4        LURSSR(MAX_STATIONS,MAX_STATIONS)
      REAL*4        LBRTSSR(MAX_STATIONS,MAX_STATIONS)
      REAL*4        LCRRSTAZ(MAX_STATIONS,50)
      REAL*4        LURRSTAZ(MAX_STATIONS,50)
      REAL*4        LBRTRSTAZ(MAX_STATIONS,50)
      REAL*4        RNTTRN(10,50,7)
      REAL*4        WCRI(MAX_STATIONS),LCRI
      REAL*4        WURI(MAX_STATIONS),LURI
      REAL*4        WBRTI(MAX_STATIONS),LBRTI
      REAL*4        LCRSSI(MAX_STATIONS,MAX_STATIONS)
      REAL*4        LURSSI(MAX_STATIONS,MAX_STATIONS)
      REAL*4        LBRTSSI(MAX_STATIONS,MAX_STATIONS)
      REAL*4        LCRISTAZ(MAX_STATIONS,50)
      REAL*4        LURISTAZ(MAX_STATIONS,50)
      REAL*4        LBRTISTAZ(MAX_STATIONS,50)
      REAL*4        ITFTRN(50,7),ITFTRN2(50,7)
      REAL*4        ALOGSUM(10,MAX_ZONES),TLOGSUM(10,MAX_ZONES)
      REAL*4        ULOGSUM(10,MAX_ZONES),AUTEXP,CBDTRAN,CBDTRCR
      REAL*4        LUNRELV(5),NUNRELV(5),TWAIT
      REAL*4        LCROWDV(5),NCAPACV(5)
      REAL*4        CLOSEDIST(MAX_STATIONS),ZCARP,ZCARA
      REAL*4        EGRUTIL1,EGRUTIL2,UTIL1,UTIL2
      REAL*4        TAXICOST,TAXITOLL,UTILTAXI,RANVAL,KWAIT,TXWAIT             !Innovation
      REAL*4        COSTUBER,UTILUBER,MAASPROB(2),LSMAAS,ACCUBER               !Innovation
      REAL*4        TMASS,TUBER,COSTPARK,APARKTRP(4,2)                         !Innovation
      REAL*4        HOMECOST,ALTWRK,ALTOTH,EXPWRK,EXPOTH,PROBWRK               !Innovation
      REAL*4        UBERUTIL(5,10),UBERCOST(5,10),LSUM4CU,LSCKR2               !Innovation
      REAL*4        LSUM4UU,LSUKR2,LSUM4BRU,LSBRK2                             !Innovation
      REAL*4        APRKCST(4000),TAXIMAAS                                     !Innovation
C
      REAL*8        WLKLSM(MAX_ZONES),BYCLSM(MAX_ZONES)
      REAL*8        UTIL(125),EUTIL(125),DENOM,DENOME,VEHTRP                   !Innovation
      REAL*4        VALUES(15),VALUES2(15),VALUES3(15)
      REAL*4        SXCOORD(MAX_IZONES),SYCOORD(MAX_IZONES)
      REAL*8        PNRTRP(12),UTILTRN,UTILPNR,UTILKNR,UTILWLK
      REAL*8        UTILTXI,FLYAEUIL(10,7),FLYAPROB(10,6)
      REAL*8        UTILFLY,LSFLY,EUTILFLY
      REAL*8        LAXTPROB(50,50,42),LAXFPROB(MAX_IZONES,10,42)
      REAL*8        LAXRPROB(10,50,42),R8TRIP,LAXIPROB(50,42)
      REAL*8        LAXI2PROB(50,42)
      REAL*8        MOTLSUM(MAX_IZONES,25),TEVENT(MAX_IZONES,25)
      REAL*8        UTLATXI,UTLALMO,UTLADRP,UTLAONC,EUTLAIR(10)
      REAL*8        UTLATXI1,UTLATXI2,UTLALMO1,UTLALMO2
      REAL*8        UTLADRP1,UTLADRP2,PROBTXI(3),PROBLMO(3),PROBDRP(3)
      REAL*8        UTLARNT,PROBAIR(10),LSPUB,LSPRV
      REAL*8        TDROP,TLIMO,TRNTL,TTAXI,TONCL,AESUM(12)
      REAL*8        TTAXI1,TTAXI2,TLIMO1,TLIMO2,TDROP1,TDROP2
      REAL*8        TTAXI0,TLIMO0,TDROP0
      REAL*8        TONCLI,TFLYI,TPRKSHL,TPRKSHL2
      REAL*8        ITFSUM(3,4)
      REAL*8        STAVOL(5,5),XDISTRN(21,21,36)
      REAL*8        TOLLSUM(9,10),NMOTLF(150,6,2),TOLLTLF(102,9,13)
      REAL*8        TOLLDIST(21,21),TOLLSAV(31,9,10),TRNDIST(21,21,29)
      REAL*8        WEXPTLF(12),YDISTRN(21,21,3)
      REAL*8        VMT(48,4)
      INTEGER*4     TLSUMZ(MAX_ZONES)
      CHARACTER*1   FF,BLANK
      LOGICAL       CSORT,EXT,TXOUT,EXISTS,UNCON,EBUSCR,HOMEAV
      LOGICAL       BRTUR,EVENTSP,LAXTRN,CNVRGE,EVENTLI,EVENTPT
      LOGICAL       RECYC,CHWIND(5,2),CHBKIND(5,2),CHBIND(5,2)
      LOGICAL       CHPIND(5,4),CHKIND(5,4)
      CHARACTER*10  DNAME(21)
      CHARACTER*12  COUNTY(6)
      CHARACTER*13  NAME(5),TRNAME(7)
      CHARACTER*18  TLNAME(9),TNAME(29),XTNAME(36),YTNAME(3)
C
C GENERAL DATA DEFINITIONS
C
      DATA          CITER/0/,ITER/0/,BLANK/' '/
      DATA          CLOSEDIST/MAX_STATIONS*-999.9/
      DATA          URIND/2,2,3,3,4,4,4,4,5,5,5,5,6,6/
      DATA          NAME/'Commuter Rail',
     *                   'Urban Rail   ',
     *                   'Express Bus  ',
     *                   'Transitway   ',
     *                   'BRT          '/
      DATA        TRNAME/'Commuter Rail',
     *                   'Urban Rail   ',
     *                   'Transitway   ',
     *                   'Express Bus  ',
     *                   'Rapid Bus    ',
     *                   'Local Bus    ',
     *                   'BRT          '/
      DATA        TLNAME/'I-110 HARBOR      ',
     *                   'I-10  EL MONTE    ',
     *                   'I-110 & I-10      ',
     *                   'SR-91             ',
     *                   'I-110 & SR-91     ',
     *                   'I-10  & SR-91     ',
     *                   'I-110 I-10 SR-91  ',
     *                   'OTHER             ',
     *                   'TOTAL             '/
      DATA  DNAME/'Riverside','SGV East','Gateway','SouthBay',
     *      'Westside',
     *      'Malibu','SBD','OC','Glendale','LACBD','LA Central',
     *      'LAnorth','Ventura','East LA','SFV','SGV West',
     *      'SantaMonic','CenturyBev','Brentwood','Arroyo',
     *      'Total'/
      DATA  TNAME/'Walk_Local','Drive_Local','Walk_Rapid',
     *            'Drive_Rapid','Walk_Express','Drive_Express',
     *            'Walk_Transitway','Drive_Transitway',
     *            'Walk_BRT','Bus_BRT',
     *            'Walk_Commuter','Bus_Commuter','PNR_Commuter',
     *            'KNR_Commuter','Walk_Urban','Bus_Urban',
     *            'PNR_Urban','KNR_Urban','Local_Bus','Rapid_Bus',
     *            'Express_Bus','Transitway','BRT','Commuter_Rail',
     *            'Urban_Rail','Total','PNR_BRT','KNR_BRT','Person'/
      DATA XTNAME/'WALK_URCR','BUS_URCR','PNR_URCR',
     *            'KNR_URCR','BIKE_URCR','WALK_BRCR',
     *            'BUS_BRCR','PNR_BRCR','KNR_BRCR','BIKE_BRCR',
     *            'WALK_BRUR','BUS_BRUR','PNR_BRUR',
     *            'KNR_BRUR','BIKE_BRUR','TOT_URCR','TOT_BRCR',
     *            'TOT_BRUR','WALK_URBR','BUS_URBR','PNR_URBR',
     *            'KNR_URBR','BIKE_URBR','TOT_URBR',
     *            'WALK_CRUR','BUS_CRUR','PNR_CRUR',
     *            'KNR_CRUR','BIKE_CRUR','TOT_CRUR',
     *            'WALK_CRBR','BUS_CRBR','PNR_CRBR',
     *            'KNR_CRBR','BIKE_CRBR','TOT_CRBR'/
      DATA YTNAME/'Commuter_Rail','Urban_Rail','BRT'/
      DATA FRMODE/1,1,2,2,5,5/,TOMODE/2,5,1,5,1,2/
      DATA         HHLDAV/0,0,1,1/,UBERAV/0,1,0,1/
      DATA        COUNTY/'LOS ANGELES ','ORANGE     ','RIVERSIDE  ',
     *                   'SAN BERNARD ','VENTURA    ','TOTAL      '/
C----------------------------------------------------------------------
C
      FF=CHAR(12)
C               
C CONTROL FILE PARAMETERS
C
      CALL RCTL(PWALK,HOV2P,HOV3P,HOV4P,ZHHD,EQUIV,
     *          SXCOORD,SYCOORD) 
      LAXTRN=.FALSE.
      CNVRGE=.FALSE.
      RECYC=.FALSE.
      SPEVENT=.FALSE.
      EVENTLI=.FALSE.
      EVENTPT=.FALSE.
      EBUSCR=.FALSE.
      EUTILFLY=0.0
      MAXVALUE=0.0
      MAXIZ=0
      MAXJZ=0
      BUSTXFER=0.0
      STATXF=0.0
      RALEGR=0.0
      TRNDIST=0.0
      XDISTRN=0.0
      YDISTRN=0.0
      WTRAT=0.0
      IF(LAX) LAXTRN=.TRUE.
      IF(LAX) CNVRGE=.TRUE.
      CHWIND=.FALSE.
      CHBKIND=.FALSE.
      CHBIND=.FALSE.
      CHPIND=.FALSE.
      CHKIND=.FALSE.
      STAEGR=0.0
      XTESUM=0.0
      STAVOL=0.0
      STAEXP=0
      STATWY=0
      WEXPTLF=0.0
      TESUM=0.0
      XFERMATX=0.0
      XFERSTA=0
      ITFSUM=0.0
      SAVFLY=0.0
      CRWLK=0.0
      URWLK=0.0
      BRTWLK=0.0
      ALLWLK=0.0
      WBRTI=0.0
      LBRTSSI=0.0
      LBRTISTAZ=0.0
      WBRTR=0.0
      LBRTSSR=0.0
      LBRTRSTAZ=0.0
      WBRT=0.0
      LBRTSS=0.0
      LBRTSTAZ=0.0
      AV4=1
      AVHTIME=0.0
      AVTTIME=0.0
      AVH2TIME=0.0
      AVT2TIME=0.0
      AVH3TIME=0.0
      AVT3TIME=0.0
      MXZONES=4000
      TAXISUM=0.0
      VMT=0.0
      VMTCODE(1,1)=1
      VMTCODE(1,2)=2
      VMTCODE(2,1)=3
      VMTCODE(2,2)=4
      IF(LAX) THEN
      INQUIRE (FILE=FAIRPROB,EXIST=EXISTS)
      IF(EXISTS) THEN
      CALL RDAIRPROB(LAXTPROB,LAXSTAP,LAXSTAA,
     *             LAXRPROB,LAXRNTP,LAXRNTA,
     *             LAXIPROB,LAXITFP,LAXITFA,
     *             LAXI2PROB,LAXITF2P,LAXITF2A,
     *             LAXFPROB,LAXSTAFP,LAXSTAFA)
      LAXTRN=.FALSE.
      OPEN(132,FILE='AIRPROB.CSV',STATUS='UNKNOWN',FORM='FORMATTED')
      END IF 
      END IF
C
      CALL STANEAR(SXCOORD,SYCOORD)
      CALL FREQDIST                                                            !Innovations
      CALL TAXIDIST                                                            !Innovations
      IF(AVMDL) AV4=4                                                          !Innovations
      CALL PARKCOST(ZHHD,APRKZONE,APRKCST)                                     !Innovations
C
C   OPEN OUTPUT FILES
C
      IF(HFACT.AND.HCALIB) THEN
      CALL PREPIO(PERTT,8)
      GO TO 8301
      END IF
      IF(TRIPSOUT.OR.CALIB) THEN
      OPEN(21,FILE=HWYOUT,FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN(22,FILE=CROUT,FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN(23,FILE=UROUT,FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN(24,FILE=BUSOUT,FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN(25,FILE=NBUSOUT,FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN(126,FILE=BRTOUT,FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN(127,FILE=BLENDOUT,FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(LOWRAIL) 
     * OPEN(128,FILE=FLOWRAIL,FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(AVMDL)
     * OPEN(144,FILE=FAVTRIPS,FORM='UNFORMATTED',STATUS='UNKNOWN')
      END IF
      IF(AVMDL.AND.ALTPARK.AND.TRIPSOUT) THEN
      write(*,9727)
 9727 format(' Building Temporary Alternative Parking Files'/)
      allocate(ematx(16000000)) 
      ematx=0.0
      open(251,file='mf251.emx',
     *       status='unknown',form='binary')
      open(252,file='mf252.emx',
     *       status='unknown',form='binary')
      open(253,file='mf253.emx',
     *       status='unknown',form='binary')
      open(254,file='mf254.emx',
     *       status='unknown',form='binary')
      open(255,file='mf255.emx',
     *       status='unknown',form='binary')
      open(256,file='mf256.emx',
     *       status='unknown',form='binary')
      open(257,file='mf257.emx',
     *       status='unknown',form='binary')
      open(258,file='mf258.emx',
     *       status='unknown',form='binary')
      do it=1,8
      write(250+it) (ematx(k),k=1,16000000)
      close((250+it),status='keep')
      end do
      deallocate(ematx,stat=alloc_val)
      open(251,file='mf251.emx',recl=4,access='direct',
     *       status='unknown',form='binary')
      open(252,file='mf252.emx',recl=4,access='direct',
     *       status='unknown',form='binary')
      open(253,file='mf253.emx',recl=4,access='direct',
     *       status='unknown',form='binary')
      open(254,file='mf254.emx',recl=4,access='direct',
     *       status='unknown',form='binary')
      open(255,file='mf255.emx',recl=4,access='direct',
     *       status='unknown',form='binary')
      open(256,file='mf256.emx',recl=4,access='direct',
     *       status='unknown',form='binary')
      open(257,file='mf257.emx',recl=4,access='direct',
     *       status='unknown',form='binary')
      open(258,file='mf258.emx',recl=4,access='direct',
     *       status='unknown',form='binary')
      OPEN(143,FILE=FALTPARK,FORM='UNFORMATTED',STATUS='UNKNOWN')
      END IF
      IF(LOGSUM) 
     * OPEN(28,FILE=LOGSUMS,FORM='UNFORMATTED',STATUS='UNKNOWN')
      call prepio(bcrsk,10)
      call prepio(bursk,12)
      call prepio(wbssk,13)
      call prepio(websk,14)
      call prepio(wtwsk,15)
      call prepio(wrbsk,16)
      call prepio(brtsk,17)
      call prepio(xferurt,112)
      call prepio(xfercmr,113)
      call prepio(xferbrt,114)
      call prepio(dask,18) 
      call prepio(p2sk,19) 
      call prepio(p3sk,20)
      if(bicycle) then
      call prepio(bbssk,86)
      call prepio(brbsk,87)
      call prepio(bebsk,88)
      call prepio(btwsk,89)
      end if
      IF(SKMP4) call prepio(p4sk,37)
      IF(STNAC) call prepio(stnacc,36)
      INQUIRE (FILE=INTRADIST,EXIST=EXISTS)
      IF(EXISTS) THEN
      OPEN(49,FILE=INTRADIST,STATUS='OLD',FORM='FORMATTED')
      ELSE
      WRITE(26,5514) INTRADIST
 5514 FORMAT(/' MTAMC 5514 (F) INTRAZONAL DISTANCE FILE ',A40,
     *       ' NOT FOUND'/)
      WRITE(*,5514) INTRADIST
      STOP 5514
      END IF
      IF(TRNUSER) 
     *  OPEN(69,FILE=TLOGSUMS,FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(LAX.AND.TRIPSOUT) THEN
      OPEN(79,FILE=FLOTTRN,FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN(80,FILE=FRNTTRN,FORM='UNFORMATTED',STATUS='UNKNOWN')
      END IF
      IF(LAX.AND.TRIPSOUT.AND.ITFZONE.GT.0)
     *  OPEN(94,FILE=FITFTRN,FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(AIRPASS.AND.APMIND) CALL PREPIO(FAPMIND,99)
      IF(LAX.AND.TRIPSOUT)
     *  OPEN(95,FILE=FLYTRIP,FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(AVMDL) THEN
       IF(AVSKIM) THEN
       CALL PREPIO(FAVSKIM,142)
       ELSE
       WRITE(26,5518)
       WRITE(*,5518)
 5518  FORMAT(/' MTAMC 5518 (W) AV HIGHWAY SKIMS',
     *         ' NOT PROVIDED, AVMDL=T'/)
       END IF
      END IF
       
C 
C CHECK ON AVAILABILITY OF SPECIAL EVENT PERSON TRIP TABLE
C
      EVENTSP=.FALSE.
      IF(SPEVENT) THEN
      INQUIRE (FILE=FSPEVENT,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
	     OPEN(82,FILE=FSPEVENT,STATUS='UNKNOWN',
     *         FORM='UNFORMATTED')
	     NUMPUR=1
	     TABLES=2**1-1
	     WRITE(82) HEAD1,HEAD2
      EVENTSP=.TRUE.
      ELSE
      OPEN(82,FILE=FSPEVENT,STATUS='OLD',
     *        FORM='UNFORMATTED')
      READ(82) HEAD1,HEAD2
      EVENTLI=.TRUE.
      EVENTPT=.TRUE.
      END IF
      END IF
C
C     READ & STORE INTRAZONAL DISTANCE VALUES
C
  550 READ(49,5515,END=551,ERR=552) IZ,TOTHH
 5515 FORMAT(10X,I5,25X,F10.0)
      INTRADST(IZ)=TOTHH
      GO TO 550
  552 WRITE(26,553) IZ
      WRITE(*,553) IZ
  553 FORMAT(/' ERROR READING INTRAZONAL DISTANCE FILE NEAR ',
     *       ' ZONE ',I4/)
      STOP 552
  551 CONTINUE
      CLOSE(49,STATUS='KEEP')
C
C  READ & STORE HIGHWAY TRAVEL TIME & DISTANCE VALUES
C
C     DO 12 IZ=MAX_IZONES+1,MAX_ZONES
      DO 12 IZ=1,MAX_ZONES
      PURP=1
      UNCON=.FALSE.
      CALL INTAB(18,VAR,IZ,PURP,DUMMY,IO)
C..READ & STORE ZONE TO LAX PARKING LOT TIME
      IF(LAX.AND.LAXTRN.AND.(IZ.LE.MAX_IZONES)) THEN
      DO 17,NI=1,50
      IF(PEQUIV(NI).LE.0) GO TO 17
      K=PEQUIV(NI)
      ZNELOT(IZ,NI,1)=VAR(K)
   17 CONTINUE
      DO 5517 NI=1,10
      K=IDINT(RNTLDATA(NI,1))
      IF(K.LE.0) GO TO 5517
      ZNERNT(IZ,NI,1)=VAR(K)
 5517 CONTINUE
      END IF
C..READ & STORE STATION TO ZONE TRAVEL TIME
      IF(IZ.GT.MAX_IZONES) THEN
      DO 11,NI=1,MAX_IZONES
      SC=IZ-MAX_IZONES
      IF(SC.LE.0) STOP 16
      STAZONE(SC,NI,1)=VAR(NI)
      IF(STADATA(SC,6).EQ.1.AND.VAR(NI).LE.0) THEN
      IF(.NOT.UNCON) WRITE(75,314) IZ,NI
  314 FORMAT(' MTAMC 314 (W) STATION ',I4, ' WAS NOT CONNECTED TO',
     *       ' INTERNAL ZONE ',I4)
      UNCON=.TRUE.
      END IF
   11 CONTINUE
      END IF
C..READ & STORE FLY-AWAY TO CTA ZONE TRAVEL TIME
C  FROM 3+ CARPOOL SKIMS
      IF(LAX.AND.LAXTRN.AND.(IZ.LE.MAX_IZONES)) THEN
      CALL INTAB(20,VAR,IZ,PURP,DUMMY,IO)
      K1=0
      DO 217 NI=1,10
      KJZ=IDINT(FLYADATA(NI,1))
      IF(IZ.EQ.KJZ) K1=NI
  217 CONTINUE
      DO 218 JZ=1,MAX_IZONES
      DO 219 NI=1,10
      IF(JZ.EQ.CTAZNE(NI).AND.K1.GT.0) THEN
      FLYZNE(K1,NI)=VAR(JZ)
      END IF
  219 CONTINUE
      IF(K1.LE.0) GO TO 218
      IF(JZ.EQ.ITFZONE) FLYZNE(K1,11)=VAR(JZ)
      IF(JZ.EQ.ITFZONE2) FLYZNE(K1,12)=VAR(JZ)
  218 CONTINUE
      END IF 
C
      PURP=2
      CALL INTAB(18,VAR,IZ,PURP,DUMMY,IO)
C..READ & STORE ZONE TO LAX PARKING LOT DISTANCE
      IF(LAX.AND.LAXTRN.AND.(IZ.LE.MAX_IZONES)) THEN
      DO 16,NI=1,50
      IF(PEQUIV(NI).LE.0) GO TO 16
      K=PEQUIV(NI)
      ZNELOT(IZ,NI,2)=VAR(K)
   16 CONTINUE
      DO 5516 NI=1,10
      K=IDINT(RNTLDATA(NI,1))
      IF(K.LE.0) GO TO 5516
      ZNERNT(IZ,NI,2)=VAR(K)
 5516 CONTINUE
      END IF
C
C  READ & STORE STATION-TO-STATION DISTANCE
C
      IF(IZ.GT.MAX_IZONES) THEN
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
      END IF
   12 CONTINUE
      CLOSE(18,STATUS='KEEP')
      CALL PREPIO(DASK,18)
      CLOSE(20,STATUS='KEEP')
      CALL PREPIO(P3SK,20)
      IF(.NOT.SPEVENT) CALL PREPIO(PERTT,8)
C
C  write headers for output trip tables
C
      ftype='VOLUME'
      IF(TRIPSOUT.OR.CALIB) THEN
	    numpur=16
      tables=2**15-1
	    write(21) HEAD1,HEAD2
	    IF(AVMDL) THEN
	    NUMPUR=15
	    TABLES=2**15-1
	    WRITE(144) HEAD1,HEAD2
	    END IF
	    NUMPUR=4
	    TABLES=2**4-1
	    WRITE(127) HEAD1,HEAD2
	    IF(LOWRAIL) THEN
	    NUMPUR=4
	    TABLES=2**4-1
	    WRITE(128) HEAD1,HEAD2
	    END IF
	    NUMPUR=11
	    tables=2**11-1
      WRITE(126) HEAD1,HEAD2	    
      NUMPUR=11
	    tables=2**11-1
      IF(NHBDIR) THEN
      NUMPUR=3
      TABLES=2**3-1
      END IF
      WRITE(23) HEAD1,HEAD2
      NUMPUR=11
      TABLES=2**10-1
      WRITE(22) HEAD1,HEAD2
      NUMPUR=3
      tables=2**3-1
      WRITE(25) HEAD1,HEAD2
      NUMPUR=37
      TABLES=2**15-1
      WRITE(24) HEAD1,HEAD2
      END IF
      IF(LOGSUM) THEN
      NUMPUR=18
      TABLES=2**15-1
      WRITE(28) HEAD1,HEAD2
      END IF
      IF(TRNUSER) THEN
      NUMPUR=35
      TABLES=2**15-1
      WRITE(69) HEAD1,HEAD2
      END IF
      IF(LAX.AND.TRIPSOUT) THEN
      NUMPUR=16
      TABLES=2**15-1
      WRITE(79) HEAD1,HEAD2
      WRITE(80) HEAD1,HEAD2
      IF(ITFZONE.GT.0) WRITE(94) HEAD1,HEAD2
      NUMPUR=13
      TABLES=2**13-1
      WRITE(95) HEAD1,HEAD2
      END IF
      IF(AVMDL.AND.ALTPARK.AND.TRIPSOUT) THEN
      NUMPUR=8
      TABLES=2**8-1
      WRITE(143) HEAD1,HEAD2
      END IF
C
C COMPUTE STATION --> STATION UTILITY VALUES
C
      IMODE=1
      IF(.NOT.NHBDIR)
     *  CALL STATION(STASTA,IMODE,INDAPM,NHBACC,NHBUTL)
      IMODE=2
      CALL STATION(STASTA,IMODE,INDAPM,NHBACC,NHBUTL)
      IMODE=5
      IF(.NOT.NHBDIR) 
     *  CALL STATION(STASTA,IMODE,INDAPM,NHBACC,NHBUTL)
      IF(.NOT.NHBDIR) CALL STATION2(STASTA,STASTA2)
      WRITE(*,9900)
 9900 FORMAT(/)
      IF(NHBDIR) THEN
      CALL NHBGEN(NHBACC,SSDIST,ZHHD,NHBTRP)
      CALL NHBDST(SSDIST,NHBTRP,NHBUTL)
      GO TO 9515
      END IF
C
      CLOSE (10,STATUS='KEEP')
      CLOSE (12,STATUS='KEEP')
C      CLOSE (16,STATUS='KEEP')
      CLOSE (17,STATUS='KEEP')
C      call prepio(bcrsk,10)
C      call prepio(bursk,12)
C      call prepio(wrbsk,16)
C      call prepio(brtsk,17)
      call prepio(baccegr,66)
C       
C COMPUTE STATION --> DESTINATION ZONE UTILITY VALUES
C
      IF(TRNEGR) THEN
      IMODE=1
      CALL EGRPROB(STAZNE,IMODE,STAZNEI,INDAPM,STAEGR,STAZONE,ZHHD)            !Innovation
      IMODE=2
      CALL EGRPROB(STAZNE,IMODE,STAZNEI,INDAPM,STAEGR,STAZONE,ZHHD)            !Innovation
      IMODE=3 
	    CALL EGREXP(STAZNE,IMODE,STAZNEI)
      IMODE=4
	    CALL EGREXP(STAZNE,IMODE,STAZNEI)
	    IMODE=5
	    CALL EGRPROB(STAZNE,IMODE,STAZNEI,INDAPM,STAEGR,STAZONE,ZHHD)            !Innovation
      ELSE
      IMODE=1
      CALL EGRESS(STAZNE,IMODE,STAZNEI,INDAPM)
      IMODE=2
      CALL EGRESS(STAZNE,IMODE,STAZNEI,INDAPM)
      IMODE=3 
	    CALL EGREXP(STAZNE,IMODE,STAZNEI)
      IMODE=4
	    CALL EGREXP(STAZNE,IMODE,STAZNEI)
	    IMODE=5
	    CALL EGRESS(STAZNE,IMODE,STAZNEI,INDAPM)
	    END IF
C
      CLOSE (14,STATUS='KEEP')
      CLOSE (15,STATUS='KEEP')
      CLOSE (16,STATUS='KEEP')
      CLOSE (17,STATUS='KEEP')
      CLOSE (66,STATUS='KEEP')
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
      END IF
      call prepio(websk,14)
      call prepio(wtwsk,15)
      call prepio(wrbsk,16)
C
 8000   continue
C
C INITIALIZE SUMMARY ARRAYS
C
      CBDTRAN=0.0
      CBDTRCR=0.0
	    TESUM=0.0
	    XTESUM=0.0
      TOLSUM=0.0
      AESUM=0.0
	    CALSUM=0.0
      STASUM=0.0
      STASUM2=0.0
      STASUM3=0.0
      STASUM4=0.0
      STASUM5=0.0
      STASUM6=0.0
      STASUM7=0.0
      STAVOL=0.0
      CRPNRSUM=0.0
      CRPNRRAT=0.0
      MODETRP=0.0
      URSS=0.0
      CRSS=0.0
      BRTSS=0.0
      CRURBRTSS=0.0
      URSSL=0.0
      CRSSL=0.0
      BRTSSL=0.0
      CRURBRTSSL=0.0
      CRURSS=0.0
      URCRSS=0.0
      URBRTSS=0.0
      BRTURSS=0.0
      CR2URSS=0.0
      UR2CRSS=0.0
      CRBRTSS=0.0
      BRTCRSS=0.0
C
      CLOSE (10,STATUS='KEEP')
      CLOSE (12,STATUS='KEEP')
      call prepio(bcrsk,10)
      call prepio(bursk,12)
      CLOSE (66,STATUS='KEEP')
      call prepio(baccegr,66)
C
      IF(CALIB.OR.CAPRES.OR.TCALIB) THEN
      CLOSE(13,STATUS='KEEP')
      CLOSE(14,STATUS='KEEP')
      CLOSE(15,STATUS='KEEP')
      CLOSE(16,STATUS='KEEP')
      CLOSE(17,STATUS='KEEP')
      CLOSE(18,STATUS='KEEP')
      CLOSE(19,STATUS='KEEP')
      CLOSE(20,STATUS='KEEP')
      IF(AVSKIM) CLOSE(142,STATUS='KEEP')
      CLOSE(8,STATUS='KEEP')
      CLOSE(36,STATUS='KEEP')
      CLOSE(37,STATUS='KEEP')
      CLOSE(86,STATUS='KEEP')
      CLOSE(87,STATUS='KEEP')
      CLOSE(88,STATUS='KEEP')
      CLOSE(89,STATUS='KEEP')
      CLOSE(9,STATUS='KEEP')
      CLOSE(112,STATUS='KEEP')
      CLOSE(113,STATUS='KEEP')
      CLOSE(114,STATUS='KEEP')
      call prepio(wbssk,13)
      if(bicycle) then
      call prepio(bbssk,86)
      call prepio(brbsk,87)
      call prepio(bebsk,88)
      call prepio(btwsk,89)
      end if
      call prepio(websk,14)
      call prepio(wtwsk,15)
      call prepio(wrbsk,16)
      CALL PREPIO(BRTSK,17)
      call prepio(xferurt,112)
      call prepio(xfercmr,113)
      call prepio(xferbrt,114)
      call prepio(dask,18) 
      call prepio(p2sk,19) 
      call prepio(p3sk,20)
      if(avskim) call prepio(favskim,142)
      IF(SKMP4) call prepio(p4sk,37)
      IF(.NOT.SPEVENT) call prepio(pertt,8)
      IF(STNAC) call prepio(stnacc,36)
      IF(LUNREL)THEN
      CLOSE(90,STATUS='KEEP')
      CALL PREPIO(FLUNREL,90)
      CLOSE(100,STATUS='KEEP')
      CALL PREPIO(FLUNRELBK,100)
      END IF
      IF(NUNREL) THEN
      CLOSE(91,STATUS='KEEP')
      CALL PREPIO(FNUNREL,91)
      CLOSE(101,STATUS='KEEP')
      CALL PREPIO(FNUNRELBK,101)
      END IF
      IF(LCROWD) THEN
      CLOSE(92,STATUS='KEEP')
      CALL PREPIO(FCROWD,92)
      CLOSE(102,STATUS='KEEP')
      CALL PREPIO(FCROWDBK,102)
      END IF
      IF(NCAPAC) THEN
      CLOSE(93,STATUS='KEEP')
      CALL PREPIO(FCAPAC,93)
      CLOSE(103,STATUS='KEEP')
      CALL PREPIO(FCAPACBK,103)
      END IF
      IF(.NOT.WALKTIME) THEN
      CLOSE(84,STATUS='KEEP')
      OPEN(84,FILE=FWLKLSM,STATUS='OLD',FORM='BINARY')
      END IF
      IF(BICYCLE) THEN
      CLOSE(85,STATUS='KEEP')
      OPEN(85,FILE=FBYCLSM,STATUS='OLD',FORM='BINARY')
      END IF
      ENDIF
      CLOSE(71,STATUS='KEEP')
C
C*********************************************************************
C     ORIGIN ZONE LOOP                                               *
C*********************************************************************
 8301 IF(DEBUG) THEN
      WRITE(*,8006)
 8006 FORMAT(/,1X,'Mode Choice Model Computations --- Debug Analysis'/
     *         1X,'-------------------------------------------------'/)
      ELSE
        IF(LAXTRN) THEN
        WRITE(*,8334)
 8334   FORMAT(/,1X,'Preliminary Transit Computations'/
     *           1X,'--------------------------------'/)
        ELSE
      IF(.NOT.SPEVENT) WRITE(*,8003)
 8003 FORMAT(/,1X,'Mode Choice Model Computations'/
     *         1X,'------------------------------'/)
        IF(RECYC) WRITE(*,8777)
 8777   FORMAT(1X,'Re-Run Last Iteration'/
     *         1X,'---------------------'/)
        END IF
      END IF
      IF(EVENTSP) THEN
      WRITE(*,8336)
 8336 FORMAT(/1X,'Special Event Model LogSum Computation'/
     *       1X,'--------------------------------------'/)
      ELSE
       IF(EVENTLI) THEN
       WRITE(*,8338) 
 8338  FORMAT(/1X,'Special Event Model Application'/
     *        1X,'-------------------------------'/)
       ELSE
       IF(SPEVENT) WRITE(*,8341)
 8341  FORMAT(/' Special Event Destination',
     *        ' Choice Model Application'/
     *        ' -------------------------',
     *        '-------------------------'/)
       END IF     
      END IF
C
C    START OF ORIGIN ZONE LOOP
C
      DO 100 IZ=1,MAX_ZONES
      nk=IZ
      nk=mod(nk,100)
      if(nk.EQ.0.and.(.not.debug)) WRITE(*,8002) IZ
 8002 FORMAT(' Processing Origin Zone=',I5)
C
C LAX PARKING LOT OR RENTAL FACILITY ZONE?
C
      IF(LAX.AND.LAXTRN) THEN
      PARKIND=0
      RNTLIND=0
      DO 8041 K=1,50
      IF(PEQUIV(K).EQ.IZ) PARKIND=K
      IF(K.LE.10) THEN
      K1=IDINT(RNTLDATA(K,1))
      IF(K1.EQ.IZ) RNTLIND=K
      END IF
 8041 CONTINUE
      END IF
C
C LAX HOTEL LOCATION?
C
      IF(AIRPASS) THEN
      HTLIND=0
      DO K=1,NHTLZ
      IF(IZ.EQ.HTLZNE(K)) HTLIND=1
      END DO
      END IF
C
C INITIALIZE OUTPUT TRIPS ARRAY
C
      TRIPS=0.0
      AVTRIPS=0.0
      TLSUMZ=0
C
      IF(.NOT.IOI(IZ).AND.(.NOT.LAXTRN)) GOTO 1121
      IF(IZ.GT.MAX_IZONES) GOTO 1121
C
C COMPUTE HOUSEHOLD PROPORTIONS
C
      IF(HFACT) THEN
      NHHD(1)=ZHHD(8,IZ)*HHFACT(1)
	    NHHD(2)=ZHHD(9,IZ)*HHFACT(2)
	    NHHD(3)=ZHHD(10,IZ)*HHFACT(3)
	    TOTHH=NHHD(1)+NHHD(2)+NHHD(3)
      IF(TOTHH.GT.0.0) THEN
	    ZHHD(2,IZ)=NHHD(1)/TOTHH
	    ZHHD(3,IZ)=NHHD(2)/TOTHH
	    ZHHD(4,IZ)=NHHD(3)/TOTHH
	    ELSE
	    ZHHD(2,IZ)=0.2988*HHFACT(1)
	    ZHHD(3,IZ)=0.2782*HHFACT(2)
	    ZHHD(4,IZ)=0.4230*HHFACT(3)
	    END IF 
	    END IF
	    IF(HFACT.AND.HCALIB) GO TO 8302
C
C   READ HIGHWAY SKIMS FOR STADRV SUBROUTINE - 
C   USE DRIVE-ALONE NON-TOLL TIME
C
      PURP=1
	    CALL INTAB(18,VAR,IZ,PURP,DUMMY,IO)
      DO 7001,NI=1,MAX_ZONES
      TAB1DA(NI)=FLOAT(VAR(NI))/100.0
 7001 CONTINUE
C
C   DRIVE-ALONE NON-TOLL DISTANCE
C
      PURP=2
      CALL INTAB(18,VAR,IZ,PURP,DUMMY,IO)
      DO 7002,NI=1,MAX_ZONES
      TAB2DA(NI)=FLOAT(VAR(NI))/100.0
 7002 CONTINUE
C
C   LINK UNRELIABILITY
C
      IF(LUNREL) THEN
      DO T=1,28
      CALL INTAB(90,VAR,IZ,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      LUNRELM(II,T)=FLOAT(VAR(II))/100.0
      END DO
      END DO
      DO T=1,15
      CALL INTAB(100,VAR,IZ,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      LUNRELMBK(II,T)=FLOAT(VAR(II))/100.0
      END DO
      END DO
      END IF
C
C   STOP UNRELIABILITY
C
      IF(NUNREL) THEN
      DO T=1,28
      CALL INTAB(91,VAR,IZ,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      NUNRELM(II,T)=FLOAT(VAR(II))/100.0
      END DO
      END DO
      DO T=1,15
      CALL INTAB(101,VAR,IZ,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      NUNRELMBK(II,T)=FLOAT(VAR(II))/100.0
      END DO
      END DO
      END IF
C
C   LINK CROWDING
C
      IF(LCROWD) THEN
      DO T=1,28
      CALL INTAB(92,VAR,IZ,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      LCROWDM(II,T)=FLOAT(VAR(II))/100.0
      END DO
      END DO
      DO T=1,15
      CALL INTAB(102,VAR,IZ,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      LCROWDMBK(II,T)=FLOAT(VAR(II))/100.0
      END DO
      END DO
      END IF
C
C   STOP CAPACITY
C
      IF(NCAPAC) THEN
      DO T=1,28
      CALL INTAB(93,VAR,IZ,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      NCAPACM(II,T)=FLOAT(VAR(II))/100.0
      END DO
      END DO
      DO T=1,15
      CALL INTAB(103,VAR,IZ,T,DUMMY,IO)
      DO II=1,MAX_ZONES
      NCAPACMBK(II,T)=FLOAT(VAR(II))/100.0
      END DO
      END DO
      END IF
C
C   WALK & BICYCLE LOGSUM VALUES
C
  119 CONTINUE
      IF(.NOT.WALKTIME) READ(84) IIZ,TIZ,(WLKLSM(K),K=1,MAX_ZONES)
      IF(BICYCLE) READ(85) IIZ,TIZ,(BYCLSM(K),K=1,MAX_ZONES)
      IF(WALKTIME.AND.(.NOT.BICYCLE)) GO TO 120
      IF(IIZ.LT.IZ) GO TO 119
      IF(IIZ.GT.IZ) THEN
      WRITE(26,118) IIZ,IZ
  118 FORMAT(' LOGSUM FILE ZONE=',I4,' GREATER THAN',
     *       ' CURRENT ZONE=',I4)
      STOP 8
      END IF
  120 CONTINUE
C---------------------------------------------------------------------
C                 STATION CHOICE MODEL APPLICATION
C                 1:  COMMUTER RAIL
C                 2:  URBAN RAIL
C---------------------------------------------------------------------
C
C...OBTAIN TWO WALK, BIKE & BUS ACCESS STATIONS (IF ANY)
C
      DO 115,IMODE=1,5
      IF(IMODE.EQ.3.OR.IMODE.EQ.4) GO TO 115
      IF(IMODE.EQ.1) THEN
      CALL STABUS(IZ,BSTA,BDIST,MWALKB,BTXFER,imode,CRBSKIM,BUSMODE,
     *            WDIST,WSTA,LUNRELM,NUNRELM,
     *            LCROWDM,NCAPACM,WLKLSM,BYCLSM,
     *            BIKDIST,BIKSTA)
      ELSEIF(IMODE.EQ.2) THEN
      CALL STABUS(IZ,BSTA,BDIST,MWALKB,BTXFER,imode,URBSKIM,BUSMODE,
     *            WDIST,WSTA,LUNRELM,NUNRELM,
     *            LCROWDM,NCAPACM,WLKLSM,BYCLSM,
     *            BIKDIST,BIKSTA)
C
C...DETERMINE CLOSEST ZONE TO EACH URBAN RAIL STATION
C
      DO KT=1,5
      IF(WSTA(2,KT).GT.0) THEN
       SC=WSTA(2,KT)-MAX_IZONES
       IF(WDIST(2,KT).GT.CLOSEDIST(SC)) THEN
       CLOSEZONE(SC)=IZ
       CLOSEDIST(SC)=WDIST(2,KT)
       END IF
      END IF
      END DO
C...BRT
      ELSEIF(IMODE.EQ.5) THEN
      CALL STABUS(IZ,BSTA,BDIST,MWALKB,BTXFER,imode,BRTSKIM,BUSMODE,
     *            WDIST,WSTA,LUNRELM,NUNRELM,
     *            LCROWDM,NCAPACM,WLKLSM,BYCLSM,
     *            BIKDIST,BIKSTA)      
      END IF
      OSTA(IMODE,1)=WSTA(imode,1)
      OSTA(IMODE,2)=WSTA(imode,2)
      OSTA(IMODE,13)=BIKSTA(imode,1)
      OSTA(IMODE,14)=BIKSTA(imode,2)
      OSTA(IMODE,3)=BSTA(imode,1)
      OSTA(IMODE,4)=BSTA(imode,2)
      CLOSE(66,STATUS='KEEP')
      CALL PREPIO(BACCEGR,66)
 115  continue
C
C...OBTAIN TOP TEN DRIVE ACCESS STATIONS
C
C
      imode=1
	    CALL STADRV(IZ,ZINDCR,IMODE,TAB2DA)
      IMODE=2
	    CALL STADRV(IZ,ZINDUR,IMODE,TAB2DA)
	    IMODE=5
	    CALL STADRV(IZ,ZINDBR,IMODE,TAB2DA)
 8302 CONTINUE
C
C  SPECIAL EVENT DESTINATION CHOICE MODEL APPLICATION
C
      IF(EVENTSP.OR.EVENTLI) GO TO 7841
      IF(SPEVENT.AND.(.NOT.EVENTSP)) THEN
       NI=SPEQUIV(IZ)
       IF(NI.GT.0) THEN
       DENOM=0.0
C..GET DESTINATION CHOICE DENOMINATOR
       DO IIZ=1,MAX_IZONES
       DENOME=ZHHD(12,IIZ)+ZHHD(13,IIZ)+ZHHD(10,IIZ)
       IF(DENOME.GT.0) THEN
       NHHD(1)=ZHHD(12,IIZ)/RPOP
       NHHD(2)=ZHHD(13,IIZ)/REMP
       NHHD(3)=ZHHD(10,IIZ)/RHH3
       DENOME=0.6*MOTLSUM(IIZ,NI)+
     *        ALOG(0.4456*NHHD(1)+0.5*NHHD(2)+0.4635*NHHD(3))
       ELSE
       DENOME=-9999.9
       END IF
       DENOM=DENOM+DEXP(DENOME)
       IF(VDETAIL) WRITE(26,8339) IZ,IIZ,NI,MOTLSUM(IIZ,NI),
     *                NHHD,DENOME,DENOM
 8339  FORMAT(' IZ=',I4,' IIZ=',I4,' NI=',I2,' MOTLSUM=',F10.5,
     *        ' POP SHARE=',F8.4,' EMP SHARE=',F8.4,
     *        ' HIGH INC SHARE=',F8.4,
     *        ' UTILITY=',F12.5,' DENOM=',F8.5)
       END DO
C..DISTRIBUTE TRIPS FOR EACH VENUE
       DO IIZ=1,MAX_IZONES
       DENOME=ZHHD(12,IIZ)+ZHHD(13,IIZ)+ZHHD(10,IIZ)
       IF(DENOME.GT.0) THEN
       NHHD(1)=ZHHD(12,IIZ)/RPOP
       NHHD(2)=ZHHD(13,IIZ)/REMP
       NHHD(3)=ZHHD(10,IIZ)/RHH3
       DENOME=0.6*MOTLSUM(IIZ,NI)+
     *        ALOG(0.4456*NHHD(1)+0.5*NHHD(2)+0.4635*NHHD(3))
       ELSE
       DENOME=-9999.9
       END IF      
       TEVENT(IIZ,NI)=(DEXP(DENOME)/DENOM)*ZHHD(19,IZ)
       R8TRIP=R8TRIP+TEVENT(IIZ,NI)
       IF(VDETAIL)WRITE(26,8440) IZ,IIZ,DENOME,TEVENT(IIZ,NI)
 8440  FORMAT(' IZ=',I4,' IIZ=',I4,' UTILITY=',F12.5,
     *        ' TEVENT=',F10.5)
       END DO
       IF(VDETAIL) WRITE(26,8441) R8TRIP
 8441  FORMAT(' R8TRIP=',F10.3)
       GO TO 100
       END IF
      ELSE
C
C  INPUT APPROPRIATE PERSON TRIP TABLE
C
      PERIN=0.0
      PURP=1
      CALL INTAB(8,VAR,IZ,PURP,DUMMY,IO)
      DO IT=1,MAX_ZONES
      PERIN(1,IT)=FLOAT(VAR(IT))*PERFACT
      END DO
      IF(NCATS.GE.3.AND.(.NOT.HFACT).AND.(.NOT.AIRPASS)) THEN
      PURP=2
      CALL INTAB(8,VAR,IZ,PURP,DUMMY,IO)
      DO IT=1,MAX_ZONES
      PERIN(2,IT)=FLOAT(VAR(IT))*PERFACT
      END DO
      PURP=3
      CALL INTAB(8,VAR,IZ,PURP,DUMMY,IO)
      DO IT=1,MAX_ZONES
      PERIN(3,IT)=FLOAT(VAR(IT))*PERFACT
      END DO
      END IF
      IF(NCATS.EQ.5) THEN
      PURP=4
      CALL INTAB(8,VAR,IZ,PURP,DUMMY,IO)
      DO IT=1,MAX_ZONES
      PERIN(4,IT)=FLOAT(VAR(IT))*PERFACT
      END DO      
      PURP=5
      CALL INTAB(8,VAR,IZ,PURP,DUMMY,IO)
      DO IT=1,MAX_ZONES
      PERIN(5,IT)=FLOAT(VAR(IT))*PERFACT
      END DO
      END IF
      DO IT=1,MAX_ZONES      
      PERSON(IT)=PERIN(1,IT)+PERIN(2,IT)+PERIN(3,IT)+
     *           PERIN(4,IT)+PERIN(5,IT)
      END DO
      END IF
C
C  PERSON TRIP MARKET SEGMENTATION
C  COMPUTE PERSON TRIPS BY INCOME LEVEL
C
      IF(HFACT) THEN
      DO JZ=1,MAX_ZONES
      PERIN(1,JZ)=PERSON(JZ)*ZHHD(2,IZ)
      PERIN(2,JZ)=PERSON(JZ)*ZHHD(3,IZ)
      PERIN(3,JZ)=PERSON(JZ)*ZHHD(4,IZ)
      IF(HCALIB) THEN
      TESUM(6,1)=TESUM(6,1)+PERSON(JZ)*ZHHD(2,IZ)
      TESUM(6,2)=TESUM(6,2)+PERSON(JZ)*ZHHD(3,IZ)
      TESUM(6,3)=TESUM(6,3)+PERSON(JZ)*ZHHD(4,IZ)
      END IF     
      END DO
      IF(HCALIB) GO TO 100
      END IF
C
C  IF AIR PASSENGER MODEL PLACE PERSON TRIPS IN MARKET SEGMENT 2
C
      IF(AIRPASS) THEN
      PERIN=0.0
      DO IT=1,MAX_ZONES
      PERIN(1,IT)=0.0
      PERIN(2,IT)=PERSON(IT)
      PERIN(3,IT)=0.0
      END DO
      END IF
 7841 CONTINUE
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
  504 txfer(1,ii)=VAR(II)
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
  514 txfer(2,ii)=VAR(II)
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
 534  txfer(3,ii)=VAR(II)
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
 634  txfer(4,ii)=VAR(II)
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
      IF(BICYCLE) THEN
C
C     BIKE TO LOCAL BUS - INDEX 6
C
c  1ST WAIT TIME
      PURP=1
      call INTAB(86,VAR,iz,PURP,dummy,io)
      do 2505,ii=1,MAX_ZONES
 2505 wait1(6,II)=FLOAT(VAR(II))/100.0
c  TRANSFER WAIT TIME
      PURP=2
      call INTAB(86,VAR,iz,PURP,dummy,io)
      do 2506,ii=1,MAX_ZONES
 2506 wait2(6,II)=FLOAT(VAR(II))/100.0
C  BWALK ACCESS WALK
       PURP=3
       call INTAB(86,VAR,iz,PURP,dummy,io)
       do 2501,ii=1,MAX_ZONES
 2501  walkacc(6,II)=FLOAT(VAR(II))/100.0
c  TRANSFER
      PURP=4
      call INTAB(86,VAR,iz,PURP,dummy,io)
      do 2504,ii=1,MAX_ZONES
 2504 txfer(6,II)=VAR(II)
c  BIVTT
      PURP=5
      call INTAB(86,VAR,iz,PURP,dummy,io)
      do 2502,ii=1,MAX_ZONES
 2502 Bivt(6,II)=FLOAT(VAR(II))/100.0
c  FARE
      PURP=6
      call INTAB(86,VAR,iz,PURP,dummy,io)
      do 2503,ii=1,MAX_ZONES
 2503 fare(6,II)=FLOAT(VAR(II))
C  WALK EGRESS TIME
      PURP=7
      CALL INTAB(86,VAR,IZ,PURP,DUMMY,IO)
      DO 2507,II=1,MAX_ZONES
 2507 WALKEGR(6,II)=FLOAT(VAR(II))/100.0
C  WALK TRANSFER TIME
      PURP=8
      CALL INTAB(86,VAR,IZ,PURP,DUMMY,IO)
      DO 2508,II=1,MAX_ZONES
 2508 WALKTFR(6,II)=FLOAT(VAR(II))/100.0
C
C     BIKE TO RAPID BUS - INDEX 7
C
c  1ST WAIT TIME
      PURP=1
      call INTAB(87,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      wait1(7,II)=FLOAT(VAR(II))/100.0
      end do
c  TRANSFER WAIT TIME
      PURP=2
      call INTAB(87,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      wait2(7,II)=FLOAT(VAR(II))/100.0
      end do
C  WALK TIME
      PURP=3
      call INTAB(87,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      walkACC(7,II)=FLOAT(VAR(II))/100.0
      end do
c  TRANSFERS
      PURP=4
      call INTAB(87,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      txfer(7,II)=VAR(II)
      end do
C  LOCAL BUS IN-VEHICLE TIME
       PURP=5
       call INTAB(87,VAR,iz,PURP,dummy,io)
       do ii=1,MAX_ZONES
       bivt(7,II)=FLOAT(VAR(II))/100.0
       end do
C  FARE
       PURP=6
       call INTAB(87,VAR,iz,PURP,dummy,io)
       do ii=1,MAX_ZONES
       fare(7,II)=FLOAT(VAR(II))
       end do
c  RAPID BUS IVT
       PURP=7
       call INTAB(87,VAR,iz,PURP,dummy,io)
       do ii=1,MAX_ZONES
       rivt(7,II)=FLOAT(VAR(II))/100.0
       end do
c  TOTAL IN-VEHICLE TIME
      PURP=8
      call INTAB(87,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      tivt(7,II)=FLOAT(VAR(II))/100.0
      end do
C  WALK EGRESS TIME
      PURP=9
      CALL INTAB(87,VAR,IZ,PURP,DUMMY,IO)
      DO II=1,MAX_ZONES
      WALKEGR(7,II)=FLOAT(VAR(II))/100.0
      end do
C  WALK TRANSFER TIME
      PURP=10
      CALL INTAB(87,VAR,IZ,PURP,DUMMY,IO)
      DO II=1,MAX_ZONES
      WALKTFR(7,II)=FLOAT(VAR(II))/100.0
      end do
c
C     BIKE TO EXPRESS BUS - INDEX 8
C
c  1ST WAIT TIME
      PURP=1
      call INTAB(88,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      wait1(8,II)=FLOAT(VAR(II))/100.0
      end do
c  TRANSFER WAIT TIME
      PURP=2
      call INTAB(88,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      wait2(8,II)=FLOAT(VAR(II))/100.0
      end do
C  WALK TIME ACCESS
      PURP=3
      call INTAB(88,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      walkACC(8,II)=FLOAT(VAR(II))/100.0
      end do
c  TRANSFERS
      PURP=4
      call INTAB(88,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      txfer(8,II)=VAR(II)
      end do
c  LOCAL BUS IVT
      PURP=5
      call INTAB(88,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      bivt(8,II)=FLOAT(VAR(II))/100.0
      end do
c  FARE
       PURP=6
       call INTAB(88,VAR,iz,PURP,dummy,io)
       do ii=1,MAX_ZONES
       fare(8,II)=FLOAT(VAR(II))
       end do
c  RAPID BUS IVT
       PURP=7
       call INTAB(88,VAR,iz,PURP,dummy,io)
       do ii=1,MAX_ZONES
       rivt(8,II)=FLOAT(VAR(II))/100.0
       end do
c  EXPRESS BUS IVT
      PURP=8
      call INTAB(88,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      eivt(8,II)=FLOAT(VAR(II))/100.0
      end do
c  TOTAL IN-VEHICLE TIME
      PURP=9
      call INTAB(88,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      tivt(8,II)=FLOAT(VAR(II))/100.0
      end do
C  WALK EGRESS TIME
      PURP=10
      CALL INTAB(88,VAR,IZ,PURP,DUMMY,IO)
      DO II=1,MAX_ZONES
      WALKEGR(8,II)=FLOAT(VAR(II))/100.0
      end do
C  WALK TRANSFER TIME
      PURP=11
      CALL INTAB(88,VAR,IZ,PURP,DUMMY,IO)
      DO II=1,MAX_ZONES
      WALKTFR(8,II)=FLOAT(VAR(II))/100.0
      end do
C
C     BIKE TO TRANSITWAY BUS - INDEX 9
C
      IF(TWYSK) THEN
c  1ST WAIT TIME
      PURP=1
      call INTAB(89,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      wait1(9,II)=FLOAT(VAR(II))/100.0
      end do
c  TRANSFER WAIT TIME
      PURP=2
      call INTAB(89,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      wait2(9,II)=FLOAT(VAR(II))/100.0
      end do
C  WALK TIME
      PURP=3
      call INTAB(89,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      walkACC(9,II)=FLOAT(VAR(II))/100.0
      end do
c  TRANSFERS
      PURP=4
      call INTAB(89,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      txfer(9,II)=VAR(II)
      end do
C  LOCAL BUS IN-VEHICLE TIME
       PURP=5
       call INTAB(89,VAR,iz,PURP,dummy,io)
       do ii=1,MAX_ZONES
       bivt(9,II)=FLOAT(VAR(II))/100.0
       end do
C  FARE
       PURP=6
       call INTAB(89,VAR,iz,PURP,dummy,io)
       do ii=1,MAX_ZONES
       fare(9,II)=FLOAT(VAR(II))
       end do
C  RAPID BUS IVT
       PURP=7
       call INTAB(89,VAR,iz,PURP,dummy,io)
       do ii=1,MAX_ZONES
       rivt(9,II)=FLOAT(VAR(II))/100.0
       end do
c  EXPRESS BUS IVT
      PURP=8
      call INTAB(89,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      eivt(9,II)=FLOAT(VAR(II))/100.0
      end do
C  TRANSIT WAY BUS IN-VEHICLE TIME
      PURP=9
      call INTAB(89,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      wivt(9,II)=FLOAT(VAR(II))/100.0
      end do
c  TOTAL IN-VEHICLE TIME
      PURP=10
      call INTAB(89,VAR,iz,PURP,dummy,io)
      do ii=1,MAX_ZONES
      tivt(9,II)=FLOAT(VAR(II))/100.0
      end do
C  WALK EGRESS TIME
      PURP=11
      CALL INTAB(89,VAR,IZ,PURP,DUMMY,IO)
      DO II=1,MAX_ZONES
      WALKEGR(9,II)=FLOAT(VAR(II))/100.0
      end do
C  WALK TRANSFER TIME
      PURP=12
      CALL INTAB(89,VAR,IZ,PURP,DUMMY,IO)
      DO II=1,MAX_ZONES
      WALKTFR(9,II)=FLOAT(VAR(II))/100.0
      end do
      END IF
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
C.....BIKE MATRICES
      IF(BICYCLE) THEN
      PURP=3
      CALL INTAB(36,VAR,IZ,PURP,DUMMY,IO)
      DO 842,II=1,MAX_ZONES
  842 BUSTAT(3,II)=VAR(II)
      PURP=4
      CALL INTAB(36,VAR,IZ,PURP,DUMMY,IO)
      DO 843,II=1,MAX_ZONES
  843 BUSTAT(4,II)=VAR(II)
      END IF
      END IF
C
C   DRIVE-ALONE AUTO SKIMS
C
C   NON-TOLL COST
      PURP=3
      call intab(18,VAR,iz,PURP,dummy,io)
      DO 7003,NI=1,MAX_ZONES
 7003 TAB3DA(NI)=FLOAT(VAR(NI))
C
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
 7006 TAB6DA(NI)=FLOAT(VAR(NI))
C   TOLL TRAVEL DISTANCE (TOLL/HOT LANES ONLY)
      PURP=7
      call intab(18,VAR,iz,PURP,dummy,io)
      DO 7007,NI=1,MAX_ZONES
 7007 TAB7DA(NI)=FLOAT(VAR(NI))/100.0
C   HOT FACILITY INDICATOR
      PURP=8
      call intab(18,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      TAB8DA(NI)=VAR(NI)
      END DO
C
C  2-PERSON AUTO SKIMS
C
C   FREE PATH WITH HOV LANE      
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
 7010 TAB32P(NI)=FLOAT(VAR(NI))
C   NON-TOLL TRAVEL DISTANCE (HOV LANES ONLY)
      PURP=4
      call intab(19,VAR,iz,PURP,dummy,io)
      DO 7014,NI=1,MAX_ZONES
 7014 TAB72P(NI)=FLOAT(VAR(NI))/100.0
C   TOLL PATH WITH HOV LANE
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
 7013 TAB62P(NI)=FLOAT(VAR(NI))
C   TOLL TRAVEL DISTANCE (HOT LANES ONLY)
      PURP=8
      call intab(19,VAR,iz,PURP,dummy,io)
      DO 7015,NI=1,MAX_ZONES
 7015 TAB82P(NI)=FLOAT(VAR(NI))/100.0
C   DISTANCE ON HOV LANES
      PURP=9
      CALL INTAB(19,VAR,IZ,PURP,DUMMY,IO)
      DO NI=1,MAX_ZONES
      TAB92P(NI)=FLOAT(VAR(NI))/100.0
      END DO
C   HOT FACILITY INDICATOR
      PURP=10
      CALL INTAB(19,VAR,IZ,PURP,DUMMY,IO)
      DO NI=1,MAX_ZONES
      TAB102P(NI)=VAR(NI)
      END DO
C   TOLL PATH WITHOUT HOV LANE
C   TOLL TIME
      PURP=11
      call intab(19,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      TAB112P(NI)=FLOAT(VAR(NI))/100.0
      END DO
C   TOLL DISTANCE
      PURP=12
      call intab(19,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      TAB122P(NI)=FLOAT(VAR(NI))/100.0
      END DO
C   TOLL COST
      PURP=13
      call intab(19,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      TAB132P(NI)=FLOAT(VAR(NI))
      END DO
C   TOLL TRAVEL DISTANCE (HOT LANES ONLY)
      PURP=14
      call intab(19,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      TAB142P(NI)=FLOAT(VAR(NI))/100.0
      END DO
C   HOT FACILITY INDICATOR
      PURP=15
      CALL INTAB(19,VAR,IZ,PURP,DUMMY,IO)
      DO NI=1,MAX_ZONES
      TAB152P(NI)=VAR(NI)
      END DO
C   HOV LANE FACILITY INDICATOR
      PURP=16
      CALL INTAB(19,VAR,IZ,PURP,DUMMY,IO)
      DO NI=1,MAX_ZONES
      TAB162P(NI)=VAR(NI)
      END DO
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
 7018 TAB33P(NI)=FLOAT(VAR(NI))
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
 7021 TAB63P(NI)=FLOAT(VAR(NI))
C   TOLL TRAVEL DISTANCE (HOT LANES ONLY)
      PURP=8
      call intab(20,VAR,iz,PURP,dummy,io)
      DO 7023,NI=1,MAX_ZONES
 7023 TAB83P(NI)=FLOAT(VAR(NI))/100.0
C   DISTANCE ON HOV LANES
      PURP=9
      CALL INTAB(20,VAR,IZ,PURP,DUMMY,IO)
      DO NI=1,MAX_ZONES
      TAB93P(NI)=FLOAT(VAR(NI))/100.0
      END DO
C   HOT FACILITY INDICATOR
      PURP=10
      CALL INTAB(20,VAR,IZ,PURP,DUMMY,IO)
      DO NI=1,MAX_ZONES
      TAB103P(NI)=VAR(NI)
      END DO
C   TOLL PATH WITHOUT HOV LANE
C   TOLL TIME
      PURP=11
      call INTAB(20,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      TAB113P(NI)=FLOAT(VAR(NI))/100.0
      END DO
C   TOLL DISTANCE
      PURP=12
      call INTAB(20,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      TAB123P(NI)=FLOAT(VAR(NI))/100.0
      END DO
C   TOLL COST
      PURP=13
      call INTAB(20,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      TAB133P(NI)=FLOAT(VAR(NI))
      END DO
C   TOLL TRAVEL DISTANCE (HOT LANES ONLY)
      PURP=14
      call INTAB(20,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      TAB143P(NI)=FLOAT(VAR(NI))/100.0
      END DO
C   HOT FACILITY INDICATOR
      PURP=15
      CALL INTAB(20,VAR,IZ,PURP,DUMMY,IO)
      DO NI=1,MAX_ZONES
      TAB153P(NI)=VAR(NI)
      END DO
C   HOV LANE FACILITY INDICATOR
      PURP=16
      CALL INTAB(20,VAR,IZ,PURP,DUMMY,IO)
      DO NI=1,MAX_ZONES
      TAB163P(NI)=VAR(NI)
      END DO
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
 7118 TAB34P(NI)=FLOAT(VAR(NI))
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
 7121 TAB64P(NI)=FLOAT(VAR(NI))
C   TOLL TRAVEL DISTANCE (HOT LANES ONLY)
      PURP=8
      call INTAB(37,VAR,iz,PURP,dummy,io)
      DO 7123,NI=1,MAX_ZONES
 7123 TAB84P(NI)=FLOAT(VAR(NI))/100.0
C   DISTANCE ON HOV LANES
      PURP=9
      CALL INTAB(37,VAR,IZ,PURP,DUMMY,IO)
      DO NI=1,MAX_ZONES
      TAB94P(NI)=FLOAT(VAR(NI))/100.0
      END DO
C   HOT FACILITY INDICATOR
      PURP=10
      CALL INTAB(37,VAR,IZ,PURP,DUMMY,IO)
      DO NI=1,MAX_ZONES
      TAB104P(NI)=VAR(NI)
      END DO
C   TOLL PATH WITHOUT HOV LANE
C   TOLL TIME
      PURP=11
      call INTAB(37,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      TAB114P(NI)=FLOAT(VAR(NI))/100.0
      END DO
C   TOLL DISTANCE
      PURP=12
      call INTAB(37,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      TAB124P(NI)=FLOAT(VAR(NI))/100.0
      END DO
C   TOLL COST
      PURP=13
      call INTAB(37,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      TAB134P(NI)=FLOAT(VAR(NI))
      END DO
C   TOLL TRAVEL DISTANCE (HOT LANES ONLY)
      PURP=14
      call INTAB(37,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      TAB144P(NI)=FLOAT(VAR(NI))/100.0
      END DO
C   HOT FACILITY INDICATOR
      PURP=15
      CALL INTAB(37,VAR,IZ,PURP,DUMMY,IO)
      DO NI=1,MAX_ZONES
      TAB154P(NI)=VAR(NI)
      END DO
C   HOV LANE FACILITY INDICATOR
      PURP=16
      CALL INTAB(37,VAR,IZ,PURP,DUMMY,IO)
      DO NI=1,MAX_ZONES
      TAB164P(NI)=VAR(NI)
      END DO
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
      TAB94P(NI)=TAB93P(NI)
      TAB104P(NI)=TAB103P(NI)
      TAB114P(NI)=TAB113P(NI)
      TAB124P(NI)=TAB123P(NI)
      TAB134P(NI)=TAB133P(NI)
      TAB144P(NI)=TAB143P(NI)
      TAB154P(NI)=TAB153P(NI)
      TAB164P(NI)=TAB163P(NI)
 7125 CONTINUE
      END IF
C
      IF(AVMDL.AND.AVSKIM) THEN
C
C  AUTOMATED VEHICLE SKIMS
C      
C   DRIVE ALONE AV NON-TOLL HIGHWAY TIME
      PURP=1
      call intab(142,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      AVHTIME(NI)=FLOAT(VAR(NI))/100.0
      END DO
C   DRIVE ALONE AV TOLL HIGHWAY TIME
      PURP=2
      call intab(142,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      AVTTIME(NI)=FLOAT(VAR(NI))/100.0
      END DO
C   2-PERSON AV HOV TIME
      PURP=3
      call intab(142,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      AVH2TIME(NI)=FLOAT(VAR(NI))/100.0
      END DO
C   2-PERSON AV TOLL TIME
      PURP=4
      call intab(142,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      AVT2TIME(NI)=FLOAT(VAR(NI))/100.0
      END DO
C   3+ PERSON AV HOV TIME
      PURP=5
      call intab(142,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      AVH3TIME(NI)=FLOAT(VAR(NI))/100.0
      END DO
C   3+ PERSON AV TOLL TIME
      PURP=6
      call intab(142,VAR,iz,PURP,dummy,io)
      DO NI=1,MAX_ZONES
      AVT3TIME(NI)=FLOAT(VAR(NI))/100.0
      END DO
      ELSE
      DO NI=1,MAX_ZONES
      AVHTIME(NI)=TAB1DA(NI)
      AVTTIME(NI)=TAB4DA(NI)
      AVH2TIME(NI)=TAB12P(NI)
      AVT2TIME(NI)=TAB112P(NI)
      AVH3TIME(NI)=TAB13P(NI)
      AVT3TIME(NI)=TAB113P(NI)
      END DO
      END IF
C
       IF(EVENTLI.AND.EVENTPT) THEN
       PURP=1
       CALL INTAB(82,VAR,IZ,PURP,DUMMY,IO)
       DO NI=1,MAX_ZONES
       PERSON(NI)=FLOAT(VAR(NI))*0.01
       END DO
       END IF
C
C   INITIALIZE TRIPS MATRIX HERE
C
      LRTWLK=0
      LRTDRV=0
      ALOGSUM=0.0
      TLOGSUM=0.0
      ULOGSUM=0.0
      TRNLSUM=0.0
C
C
C*********************************************************************
C     DESTINATION ZONE LOOP                                          *
C*********************************************************************
C
      DO 200 JZ=1,MAX_IZONES
      DIZ=DISTEQ(IZ)
      DJZ=DISTEQ(JZ)
      CNTYORG=IFIX(ZHHD(6,IZ))
      IF(CNTYORG.GT.2) CNTYORG=2
      CNTYDST=IFIX(ZHHD(6,JZ))
      IF(CNTYDST.GT.2) CNTYDST=2
      CNTYINDX=VMTCODE(CNTYORG,CNTYDST)
      APARKTRP=0.0
C
C   SET PERSON TRIPS FOR SPECIAL EVENT MODEL
C
      IF(EVENTLI.AND.(.NOT.EVENTPT)) THEN
      NI=SPEQUIV(JZ)
      IF(NI.GT.0) THEN
      PERSON(JZ)=TEVENT(IZ,NI)
      ELSE
      PERSON(JZ)=0.0
      END IF
      END IF
C
C   CHECK FOR ZERO PERSON TRIPS AND SKIP DESTINATION
C
      IF((PERSON(JZ).LE.0.0).AND.(.NOT.DEBUG).AND.
     *   (.NOT.LAXTRN).AND.(.NOT.LDEBUG).AND.(.NOT.EVENTSP)) GOTO 200
      IF(EVENTSP.AND.SPEQUIV(JZ).LE.0) GO TO 200
C
C   IS ATTRACTION ZONE ALSO A LAX PARKING LOT ?
C   IS ATTRACTION ZONE A CTA ZONE?
C   IS ATTRACTION ZONE A FLY-AWAY LOT?   
C
      PJZ=0
      CJZ=0
      AJZ=0
      FLYAIND=0
      DO 9444 NI=1,50
      IF(PEQUIV(NI).EQ.JZ) PJZ=NI
      IF(AEQUIV(NI).EQ.JZ) AJZ=NI
      KJZ=0
      IF(NI.GT.10) GO TO 9444
      KJZ=IDINT(FLYADATA(NI,1))
      IF(KJZ.EQ.JZ.AND.LAXTRN) FLYAIND=NI
      IF(CTAZNE(NI).EQ.JZ) CJZ=NI
 9444 CONTINUE
	    IF(.NOT.JOI(JZ).AND.FLYAIND.EQ.0) GOTO 200
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
C
        MODEINC=0
        DO K5=1,5
        IF(K5.EQ.3.OR.K4.EQ.4) CYCLE
        MODEINC(K5,1,3,1)=BUSMODE(K5,1,1)
        MODEINC(K5,2,3,1)=BUSMODE(K5,2,1)
        MODEINC(K5,3,3,1)=BUSMODE(K5,3,1)
        MODEINC(K5,4,3,1)=BUSMODE(K5,4,1)
        MODEINC(K5,1,4,1)=BUSMODE(K5,1,2)
        MODEINC(K5,2,4,1)=BUSMODE(K5,2,2)
        MODEINC(K5,3,4,1)=BUSMODE(K5,3,2)
        MODEINC(K5,4,4,1)=BUSMODE(K5,4,2)
        END DO
C
C   COMPUTE BICYCLE ZONAL LEVEL ATTRIBUTE COMMON UTILITY VALUE
C
      IF(BICYCLE) THEN
      LNINTENP=0.0
      LNINTENA=0.0
      IF(ZHHD(15,IZ).GT.0) LNINTENP=ALOG(ZHHD(15,IZ)*1000.0)
      IF(ZHHD(15,JZ).GT.0) LNINTENA=ALOG(ZHHD(15,JZ)*1000.0)
      UTLZBIK=BZCOEF(3)*LNINTENP + BZCOEF(4)*ZHHD(16,IZ) +
     *       BZCOEF(5)*ZHHD(21,IZ) + BZCOEF(6)*ZHHD(22,IZ) +
     *       BZCOEF(7)*LNINTENA + BZCOEF(8)*ZHHD(17,JZ) +
     *       BZCOEF(9)*ZHHD(21,JZ) + BZCOEF(10)*ZHHD(23,JZ) +
     *       BZCOEF(11)*ZHHD(24,JZ) + BZCOEF(12)*ZHHD(25,JZ) +
     *       BZCOEF(13)*ZHHD(26,JZ) + BZCOEF(14)*ZHHD(27,JZ) +
     *       BZCOEF(15)*ZHHD(28,IZ)*ZHHD(28,JZ)*BSV
C
C   COMPUTE BICYCLE ZONAL LEVEL ATTRIBUTE COMMON UTILITY VALUE
C   FOR USE IN STATION COMPUTATIONS
C
      UTLSBIK=BSTCOEF(3)*LNINTENP + BSTCOEF(4)*ZHHD(16,IZ) +
     *       BSTCOEF(5)*ZHHD(21,IZ) + BSTCOEF(6)*ZHHD(22,IZ) +
     *       BSTCOEF(7)*LNINTENA + BSTCOEF(8)*ZHHD(17,JZ) +
     *       BSTCOEF(9)*ZHHD(21,JZ) + BSTCOEF(10)*ZHHD(23,JZ) +
     *       BSTCOEF(11)*ZHHD(24,JZ) + BSTCOEF(12)*ZHHD(25,JZ) +
     *       BSTCOEF(13)*ZHHD(26,JZ) + BSTCOEF(14)*ZHHD(27,JZ)
C.................................................................
      IF(DEBUG) THEN
      WRITE(26,9029) IZ,JZ,
     *               LNINTENP,ZHHD(16,IZ),ZHHD(21,IZ),ZHHD(22,IZ),
     *               LNINTENA,ZHHD(17,JZ),ZHHD(21,JZ),ZHHD(23,JZ),
     *               ZHHD(24,JZ),ZHHD(25,JZ),ZHHD(26,JZ),
     *               ZHHD(27,JZ),ZHHD(28,IZ),ZHHD(28,JZ),BSV,UTLZBIK,
     *               UTLSBIK
 9029 FORMAT(/1X,'BICYCLE MODE COMMON ZONAL LEVEL UTILITY '/
     *       1X,'-----------------------------------------'/
     *       1X,'PRODUCTION ZONE            =',I10/
     *       1X,'ATTRACTION ZONE            =',I10/
     *       9X,'PRODUCTION VARIABLES----'/
     *       1X,'LN INTENSITY               =',F10.2/
     *       1X,'URBAN FABRIC               =',F10.5/
     *       1X,'% LAND AREA EDUCATION      =',F10.5/
     *       1X,'% 0-CAR HOUSEHOLDS         =',F10.5/
     *       9X,'ATTRACTION VARIABLES----'/
     *       1X,'LN INTENSITY               =',F10.2/
     *       1X,'URBAN FABRIC               =',F10.5/
     *       1X,'% LAND AREA EDUCATION      =',F10.5/
     *       1X,'% PARKING - RACKS          =',F10.5/
     *       1X,'% PARKING - LOCKERS        =',F10.5/
     *       1X,'% PARKING - BICEBERG       =',F10.5/
     *       1X,'% PARKING - ROOM           =',F10.5/
     *       1X,'% PARKING - SHOWERS        =',F10.5/
     *       9X,'INTERCHANGE VARIABLES----'/
     *       1X,'BIKE SHARE PROGRAM - PROD  =',F10.0/
     *       1X,'BIKE SHARE PROGRAM - ATTR  =',F10.0/
     *       1X,'BIKE SHARE VARIABLE        =',F10.5/
     *       1X,'BIKE MODE COMMON UTIL(ZONE)=',F10.5/
     *       1X,'BIKE MODE COMMON UTIL(STA) =',F10.5/)
      END IF
C.....................................................................
      END IF
C	
C....WALK ACCESS STATION UTILITY COMPUTATION -- URBAN RAIL
      do 30,ista=1,5
	    IMODE=2
	    CALL EGRSTA(JZ,WSTA(IMODE,ista),STASTA,STAZNE,
     *   WDSTA(imode,ista),IMODE,STAZONE,STAEGR)
	    CALL WUTL(ista,1,IZ,JZ,WSTA(IMODE,ista),WDSTA(imode,ista),
     *   WUTIL(imode,ista),STASTA,STAZNE,IMODE,STAEGR)
   30 CONTINUE
C...SORT & RETAIN BEST 2 ORIGIN STATIONS
   33 CSORT=.FALSE.
      DO 32 ISTA=2,5
      IF(WUTIL(IMODE,ISTA).EQ.0.0) GO TO 32 
      ZSTATIME(2)=WDIST(IMODE,ISTA)
      ZSTATIME(1)=WDIST(IMODE,(ISTA-1))
      IF(LAXTRN) ZSTATIME=0.0
      IF((WUTIL(IMODE,ISTA)+ZSTATIME(2)).GT.
     *   (WUTIL(IMODE,(ISTA-1))+ZSTATIME(1))) THEN
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
C	
C....BIKE ACCESS STATION UTILITY COMPUTATION -- URBAN RAIL
      do ista=1,5
	    IMODE=2
	    CALL EGRSTA(JZ,BIKSTA(IMODE,ista),STASTA,STAZNE,
     *   BIKDSTA(imode,ista),IMODE,STAZONE,STAEGR)
      CALL WUTL(ista,2,IZ,JZ,BIKSTA(IMODE,ista),BIKDSTA(imode,ista),
     *   BIKUTIL(imode,ista),STASTA,STAZNE,IMODE,STAEGR)
      end do
C...SORT & RETAIN BEST 2 ORIGIN STATIONS
  834 CSORT=.FALSE.
      DO 835 ISTA=2,5
      IF(BIKUTIL(IMODE,ISTA).EQ.0.0) GO TO 835
      IF((BIKUTIL(IMODE,ISTA)+BIKDIST(IMODE,ISTA)).GT.
     *   (BIKUTIL(IMODE,(ISTA-1))+BIKDIST(IMODE,(ISTA-1)))) THEN
      TSTA=BIKSTA(IMODE,ISTA)
      TDSTA=BIKDSTA(IMODE,ISTA)
      TUTIL=BIKUTIL(IMODE,ISTA)
      TDIST=BIKDIST(IMODE,ISTA)
      BIKSTA(IMODE,ISTA)=BIKSTA(IMODE,(ISTA-1))
      BIKDSTA(IMODE,ISTA)=BIKDSTA(IMODE,(ISTA-1))
      BIKUTIL(IMODE,ISTA)=BIKUTIL(IMODE,(ISTA-1))
      BIKDIST(IMODE,ISTA)=BIKDIST(IMODE,(ISTA-1))
      BIKSTA(IMODE,(ISTA-1))=TSTA
      BIKDSTA(IMODE,(ISTA-1))=TDSTA
      BIKUTIL(IMODE,(ISTA-1))=TUTIL
      BIKDIST(IMODE,(ISTA-1))=TDIST
      CSORT=.TRUE.
      END IF
  835 CONTINUE
      IF(CSORT) GO TO 834
      OSTA(IMODE,13)=BIKSTA(IMODE,1)
      OSTA(IMODE,14)=BIKSTA(IMODE,2)
      ASTA(IMODE,13)=BIKDSTA(IMODE,1)
      ASTA(IMODE,14)=BIKDSTA(IMODE,2)
C....WALK ACCESS STATION UTILITY COMPUTATION -- COMMUTER RAIL
      DO 31 ISTA=1,2   
	    IMODE=1
	    CALL EGRSTA(JZ,WSTA(IMODE,ista),STASTA,STAZNE,
     *   WDSTA(imode,ista),IMODE,STAZONE,STAEGR)
	    CALL WUTL(ista,1,IZ,JZ,WSTA(IMODE,ista),WDSTA(imode,ista),
     *   WUTIL(imode,ista),STASTA,STAZNE,IMODE,STAEGR)
	    ASTA(imode,ista)=WDSTA(imode,ista)
c....EVALUATE MULTIPLE PART PATH OPTION
	    CALL EGRSTA2(JZ,WSTA(IMODE,ista),STASTA,STAZNE,
     *   WDSTA((imode+5),ista),IMODE,STAZONE,STAEGR)
	    CALL WUTL(ista,1,IZ,JZ,WSTA(IMODE,ista),WDSTA((imode+5),ista),
     *   WUTIL((imode+5),ista),STASTA,STAZNE,IMODE,STAEGR)
      SC=WDSTA((IMODE+5),ISTA)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      SC2=WDSTA(IMODE,ISTA)-MAX_IZONES
      IF(SC2.LT.0) SC2=MAX_STATIONS
      EGRUTIL1=COEFF(7)*STAZNE(3,SC2,JZ)
      IF(TRNEGR) EGRUTIL1=0.0
      UTIL1=WUTIL(IMODE,ISTA)+EGRUTIL1
      EGRUTIL2=COEFF(7)*STAZNE(3,SC,JZ)
      IF(TRNEGR) EGRUTIL2=0.0
      UTIL2=WUTIL((IMODE+5),ISTA)+EGRUTIL2
      IF((UTIL2.GT.UTIL1).AND.(SC.NE.MAX_STATIONS)) THEN
      CHWIND(IMODE,ISTA)=.TRUE.
      ELSE
      CHWIND(IMODE,ISTA)=.FALSE.
      END IF
      IF(WUTIL(IMODE,ISTA).EQ.0.0.AND.WUTIL((IMODE+5),ISTA).NE.0.0)
     *   CHWIND(IMODE,ISTA)=.TRUE.
C.................................................................
      IF(DEBUG) THEN
      WRITE(26,836) WUTIL(IMODE,ISTA),EGRUTIL1,UTIL1,WDSTA(IMODE,ISTA),
     *              STANAME(SC2),
     *              WUTIL((IMODE+5),ISTA),EGRUTIL2,UTIL2,
     *              WDSTA((IMODE+5),ISTA),STANAME(SC),CHWIND(IMODE,ISTA)
  836 FORMAT(/' STANDARD UTILITY=',F10.5,' WALK EGRESS UTIL=',F8.2,
     *       ' TOTAL UTILITY=',F10.5,' EGRESS STATION=',I4,1X,A37/
     *       ' COMPLEX  UTILITY=',F10.5,' WALK EGRESS UTIL=',F8.2,
     *       ' TOTAL UTILITY=',F10.5,' EGRESS STATION=',I4,1X,A37/
     *       ' WALK ACCESS CHOICE INDICATOR=',L1/)
      END IF
C................................................................
   31 continue
C....BIKE ACCESS STATION UTILITY COMPUTATION -- COMMUTER RAIL
      DO ISTA=1,2   
	    IMODE=1
	    CALL EGRSTA(JZ,BIKSTA(IMODE,ista),STASTA,STAZNE,
     *   BIKDSTA(imode,ista),IMODE,STAZONE,STAEGR)
	    CALL WUTL(ista,2,IZ,JZ,BIKSTA(IMODE,ista),BIKDSTA(imode,ista),
     *   BIKUTIL(imode,ista),STASTA,STAZNE,IMODE,STAEGR)
	    ASTA(imode,(ista+12))=BIKDSTA(imode,ista)
c....EVALUATE MULTIPLE PART PATH OPTION
	    CALL EGRSTA2(JZ,BIKSTA(IMODE,ista),STASTA,STAZNE,
     *   BIKDSTA((imode+5),ista),IMODE,STAZONE,STAEGR)
      CALL WUTL(ista,2,IZ,JZ,BIKSTA(IMODE,ista),
     *   BIKDSTA((imode+5),ista),
     *   BIKUTIL((imode+5),ista),STASTA,STAZNE,IMODE,STAEGR)
      SC=BIKDSTA((IMODE+5),ISTA)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      SC2=BIKDSTA(IMODE,ISTA)-MAX_IZONES
      IF(SC2.LT.0) SC2=MAX_STATIONS
      EGRUTIL1=COEFF(7)*STAZNE(3,SC2,JZ)
      IF(TRNEGR) EGRUTIL1=0.0
      UTIL1=BIKUTIL(IMODE,ISTA)+EGRUTIL1
      EGRUTIL2=COEFF(7)*STAZNE(3,SC,JZ)
      IF(TRNEGR) EGRUTIL2=0.0
      UTIL2=BIKUTIL((IMODE+5),ISTA)+EGRUTIL2
      IF((UTIL2.GT.UTIL1).AND.(SC.NE.MAX_STATIONS)) THEN
      CHBKIND(IMODE,ISTA)=.TRUE.
      ELSE
      CHBKIND(IMODE,ISTA)=.FALSE.
      END IF
      IF(BIKUTIL(IMODE,ISTA).EQ.0.0.AND.BIKUTIL((IMODE+5),ISTA).NE.0.0)
     *   CHBKIND(IMODE,ISTA)=.TRUE.
C.................................................................
      IF(DEBUG) THEN
      WRITE(26,837) BIKUTIL(IMODE,ISTA),EGRUTIL1,UTIL1,
     *              BIKDSTA(IMODE,ISTA),
     *              STANAME(SC2),
     *              BIKUTIL((IMODE+5),ISTA),EGRUTIL2,UTIL2,
     *              BIKDSTA((IMODE+5),ISTA),STANAME(SC),
     *              CHBKIND(IMODE,ISTA)
  837 FORMAT(/' STANDARD UTILITY=',F10.5,' WALK EGRESS UTIL=',F8.2,
     *       ' TOTAL UTILITY=',F10.5,' EGRESS STATION=',I4,1X,A37/
     *       ' COMPLEX  UTILITY=',F10.5,' WALK EGRESS UTIL=',F8.2,
     *       ' TOTAL UTILITY=',F10.5,' EGRESS STATION=',I4,1X,A37/
     *       ' BIKE ACCESS CHOICE INDICATOR=',L1/)
      END IF
C................................................................
      END DO
C....WALK ACCESS STATION UTILITY COMPUTATION -- BRT
      DO ISTA=1,2   
	    IMODE=5
	    CALL EGRSTA(JZ,WSTA(IMODE,ista),STASTA,STAZNE,
     *   WDSTA(imode,ista),IMODE,STAZONE,STAEGR)
	    CALL WUTL(ista,1,IZ,JZ,WSTA(IMODE,ista),WDSTA(imode,ista),
     *   WUTIL(imode,ista),STASTA,STAZNE,IMODE,STAEGR)
	    ASTA(imode,ista)=WDSTA(imode,ista)
c....EVALUATE MULTIPLE PART PATH OPTION
	    CALL EGRSTA2(JZ,WSTA(IMODE,ista),STASTA,STAZNE,
     *   WDSTA((imode+5),ista),IMODE,STAZONE,STAEGR)
	    CALL WUTL(ista,1,IZ,JZ,WSTA(IMODE,ista),WDSTA((imode+5),ista),
     *   WUTIL((imode+5),ista),STASTA,STAZNE,IMODE,STAEGR)
      SC=WDSTA((IMODE+5),ISTA)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      SC2=WDSTA(IMODE,ISTA)-MAX_IZONES
      IF(SC2.LT.0) SC2=MAX_STATIONS
      EGRUTIL1=COEFF(7)*STAZNE(3,SC2,JZ)
      IF(TRNEGR) EGRUTIL1=0.0
      UTIL1=WUTIL(IMODE,ISTA)+EGRUTIL1
      EGRUTIL2=COEFF(7)*STAZNE(3,SC,JZ)
      IF(TRNEGR) EGRUTIL2=0.0
      UTIL2=WUTIL((IMODE+5),ISTA)+EGRUTIL2
      IF((UTIL2.GT.UTIL1).AND.(SC.NE.MAX_STATIONS)) THEN
      CHWIND(IMODE,ISTA)=.TRUE.
      ELSE
      CHWIND(IMODE,ISTA)=.FALSE.
      END IF
      IF(WUTIL(IMODE,ISTA).EQ.0.0.AND.WUTIL((IMODE+5),ISTA).NE.0.0)
     *   CHWIND(IMODE,ISTA)=.TRUE.
C.................................................................
      IF(DEBUG) THEN
      WRITE(26,836) WUTIL(IMODE,ISTA),EGRUTIL1,UTIL1,WDSTA(IMODE,ISTA),
     *              STANAME(SC2),
     *              WUTIL((IMODE+5),ISTA),EGRUTIL2,UTIL2,
     *              WDSTA((IMODE+5),ISTA),STANAME(SC),CHWIND(IMODE,ISTA)
      END IF
C...................................................................
      END DO
C....BIKE ACCESS STATION UTILITY COMPUTATION -- BRT
      DO ISTA=1,2   
	    IMODE=5
	    CALL EGRSTA(JZ,BIKSTA(IMODE,ista),STASTA,STAZNE,
     *   BIKDSTA(imode,ista),IMODE,STAZONE,STAEGR)
	    CALL WUTL(ista,2,IZ,JZ,BIKSTA(IMODE,ista),BIKDSTA(imode,ista),
     *   BIKUTIL(imode,ista),STASTA,STAZNE,IMODE,STAEGR)
	    ASTA(imode,(ista+12))=BIKDSTA(imode,ista)
c....EVALUATE MULTIPLE PART PATH OPTION
	    CALL EGRSTA2(JZ,BIKSTA(IMODE,ista),STASTA,STAZNE,
     *   BIKDSTA((imode+5),ista),IMODE,STAZONE,STAEGR)
      CALL WUTL(ista,2,IZ,JZ,BIKSTA(IMODE,ista),
     *   BIKDSTA((imode+5),ista),
     *   BIKUTIL((imode+5),ista),STASTA,STAZNE,IMODE,STAEGR)
      SC=BIKDSTA((IMODE+5),ISTA)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      SC2=BIKDSTA(IMODE,ISTA)-MAX_IZONES
      IF(SC2.LT.0) SC2=MAX_STATIONS
      EGRUTIL1=COEFF(7)*STAZNE(3,SC2,JZ)
      IF(TRNEGR) EGRUTIL1=0.0
      UTIL1=BIKUTIL(IMODE,ISTA)+EGRUTIL1
      EGRUTIL2=COEFF(7)*STAZNE(3,SC,JZ)
      IF(TRNEGR) EGRUTIL2=0.0
      UTIL2=BIKUTIL((IMODE+5),ISTA)+EGRUTIL2
      IF((UTIL2.GT.UTIL1).AND.(SC.NE.MAX_STATIONS)) THEN
      CHBKIND(IMODE,ISTA)=.TRUE.
      ELSE
      CHBKIND(IMODE,ISTA)=.FALSE.
      END IF
      IF(BIKUTIL(IMODE,ISTA).EQ.0.0.AND.BIKUTIL((IMODE+5),ISTA).NE.0.0)
     *   CHBKIND(IMODE,ISTA)=.TRUE.
C.................................................................
      IF(DEBUG) THEN
      WRITE(26,837) BIKUTIL(IMODE,ISTA),EGRUTIL1,UTIL1,
     *              BIKDSTA(IMODE,ISTA),
     *              STANAME(SC2),
     *              BIKUTIL((IMODE+5),ISTA),EGRUTIL2,UTIL2,
     *              BIKDSTA((IMODE+5),ISTA),STANAME(SC),
     *              CHBKIND(IMODE,ISTA)
      END IF
C...................................................................
      END DO
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
      WRITE(26,9177)
 9177 FORMAT(//1X,'Sorted Station Selection (Walk Access) --'
     *          ' Urban Rail'/
     *       1X,'-------------------------------------------')
      WRITE(26,9182) IZ,WSTA(2,1),STANAME(SC1),WUTIL(2,1),WDIST(2,1),
     *               WSTA(2,2),STANAME(SC2),WUTIL(2,2),WDIST(2,2),
     *               WSTA(2,3),STANAME(SC3),WUTIL(2,3),WDIST(2,3),
     *               WSTA(2,4),STANAME(SC4),WUTIL(2,4),WDIST(2,4),
     *               WSTA(2,5),STANAME(SC5),WUTIL(2,5),WDIST(2,5)
 9182 FORMAT(1X,'ORIGIN   ZONE   NUMBER  =',I9/  
     *       1X,'CLOSEST STATION NUMBER  =',I9,2X,A29/
     *       1X,'                UTILITY =',F9.4/
     *       1X,'        ACCESS  UTILITY =',F9.4/
     *       1X,'SECOND  STATION NUMBER  =',I9,2X,A29/
     *       1X,'                UTILITY =',F9.4/
     *       1X,'        ACCESS  UTILITY =',F9.4/
     *       1X,'THIRD   STATION NUMBER  =',I9,2X,A29/
     *       1X,'                UTILITY =',F9.4/
     *       1X,'        ACCESS  UTILITY =',F9.4/
     *       1X,'FOURTH  STATION NUMBER  =',I9,2X,A29/
     *       1X,'                UTILITY =',F9.4/
     *       1X,'        ACCESS  UTILITY =',F9.4/
     *       1X,'FIFTH   STATION NUMBER  =',I9,2X,A29/
     *       1X,'                UTILITY =',F9.4/
     *       1X,'        ACCESS  UTILITY =',F9.4/)
      SC1=BIKSTA(2,1)-MAX_IZONES
      IF(SC1.LT.0) SC1=MAX_STATIONS 
      SC2=BIKSTA(2,2)-MAX_IZONES
      IF(SC2.LT.0) SC2=MAX_STATIONS 
      SC3=BIKSTA(2,3)-MAX_IZONES
      IF(SC3.LT.0) SC3=MAX_STATIONS
      SC4=BIKSTA(2,4)-MAX_IZONES
      IF(SC4.LT.0) SC4=MAX_STATIONS
      SC5=BIKSTA(2,5)-MAX_IZONES
      IF(SC5.LT.0) SC5=MAX_STATIONS
      WRITE(26,9181)
 9181 FORMAT(//1X,'Sorted Station Selection (Bike Access) --'
     *          ' Urban Rail'/
     *       1X,'-------------------------------------------')
      WRITE(26,9182) IZ,BIKSTA(2,1),STANAME(SC1),BIKUTIL(2,1),
     *           BIKDIST(2,1),
     *           BIKSTA(2,2),STANAME(SC2),BIKUTIL(2,2),BIKDIST(2,2),
     *           BIKSTA(2,3),STANAME(SC3),BIKUTIL(2,3),BIKDIST(2,3),
     *           BIKSTA(2,4),STANAME(SC4),BIKUTIL(2,4),BIKDIST(2,4),
     *           BIKSTA(2,5),STANAME(SC5),BIKUTIL(2,5),BIKDIST(2,5)
      ENDIF
C...................................................................
      IMODE=2
      DO ista=1,2
c....EVALUATE MULTIPLE PART PATH OPTION FOR WALK ACCESS TO URBAN RAIL
	    CALL EGRSTA2(JZ,WSTA(IMODE,ista),STASTA,STAZNE,
     *   WDSTA((imode+5),ista),IMODE,STAZONE,STAEGR)
	    CALL WUTL(ista,1,IZ,JZ,WSTA(IMODE,ista),WDSTA((imode+5),ista),
     *   WUTIL((imode+5),ista),STASTA,STAZNE,IMODE,STAEGR)
      SC=WDSTA((IMODE+5),ISTA)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      SC2=WDSTA(IMODE,ISTA)-MAX_IZONES
      IF(SC2.LT.0) SC2=MAX_STATIONS
      EGRUTIL1=COEFF(7)*STAZNE(3,SC2,JZ)
      IF(TRNEGR) EGRUTIL1=0.0
      UTIL1=WUTIL(IMODE,ISTA)+EGRUTIL1
      EGRUTIL2=COEFF(7)*STAZNE(3,SC,JZ)
      IF(TRNEGR) EGRUTIL2=0.0
      UTIL2=WUTIL((IMODE+5),ISTA)+EGRUTIL2
      IF((UTIL2.GT.UTIL1).AND.(SC.NE.MAX_STATIONS)) THEN
      CHWIND(IMODE,ISTA)=.TRUE.
      ELSE
      CHWIND(IMODE,ISTA)=.FALSE.
      END IF
      IF(WUTIL(IMODE,ISTA).EQ.0.0.AND.WUTIL((IMODE+5),ISTA).NE.0.0)
     *   CHWIND(IMODE,ISTA)=.TRUE.
C.................................................................
      IF(DEBUG) THEN
      WRITE(26,836) WUTIL(IMODE,ISTA),EGRUTIL1,UTIL1,WDSTA(IMODE,ISTA),
     *              STANAME(SC2),
     *              WUTIL((IMODE+5),ISTA),EGRUTIL2,UTIL2,
     *              WDSTA((IMODE+5),ISTA),STANAME(SC),CHWIND(IMODE,ISTA)
      END IF
C................................................................
      end do
      do ista=1,2
c....EVALUATE MULTIPLE PART PATH OPTION
	    CALL EGRSTA2(JZ,BIKSTA(IMODE,ista),STASTA,STAZNE,
     *   BIKDSTA((imode+5),ista),IMODE,STAZONE,STAEGR)
      CALL WUTL(ista,2,IZ,JZ,BIKSTA(IMODE,ista),
     *   BIKDSTA((imode+5),ista),
     *   BIKUTIL((imode+5),ista),STASTA,STAZNE,IMODE,STAEGR)
      SC=BIKDSTA((IMODE+5),ISTA)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      SC2=BIKDSTA(IMODE,ISTA)-MAX_IZONES
      IF(SC2.LT.0) SC2=MAX_STATIONS
      EGRUTIL1=COEFF(7)*STAZNE(3,SC2,JZ)
      IF(TRNEGR) EGRUTIL1=0.0
      UTIL1=BIKUTIL(IMODE,ISTA)+EGRUTIL1
      EGRUTIL2=COEFF(7)*STAZNE(3,SC,JZ)
      IF(TRNEGR) EGRUTIL2=0.0
      UTIL2=BIKUTIL((IMODE+5),ISTA)+EGRUTIL2
      IF((UTIL2.GT.UTIL1).AND.(SC.NE.MAX_STATIONS)) THEN
      CHBKIND(IMODE,ISTA)=.TRUE.
      ELSE
      CHBKIND(IMODE,ISTA)=.FALSE.
      END IF
      IF(BIKUTIL(IMODE,ISTA).EQ.0.0.AND.BIKUTIL((IMODE+5),ISTA).NE.0.0)
     *   CHBKIND(IMODE,ISTA)=.TRUE.
C.................................................................
      IF(DEBUG) THEN
      WRITE(26,837) BIKUTIL(IMODE,ISTA),EGRUTIL1,UTIL1,
     *              BIKDSTA(IMODE,ISTA),
     *              STANAME(SC2),
     *              BIKUTIL((IMODE+5),ISTA),EGRUTIL2,UTIL2,
     *              BIKDSTA((IMODE+5),ISTA),STANAME(SC),
     *              CHBKIND(IMODE,ISTA)
      END IF
C................................................................
      end do
C
C....BUS ACCESS STATION UTILITY COMPUTATION
       do 40,ista=1,2
       IMODE=1
       IF(DEBUG) WRITE(26,41) ISTA,NAME(IMODE)
   41  FORMAT(/' BUS ACCESS #',I1,' ---> ',a13/
     *         ' =================================')
	    CALL EGRSTA(JZ,BSTA(imode,ista),STASTA,STAZNE,
     *   BDSTA(imode,ista),IMODE,STAZONE,STAEGR)
      CALL BUTL(ista,IZ,JZ,BDIST(imode,ista),BSTA(imode,ista),
     *  BDSTA(imode,ista),BUTIL(imode,ista),STASTA,STAZNE,IMODE,STAEGR)
       ASTA(IMODE,(ISTA+2))=BDSTA(IMODE,ISTA)
c....EVALUATE MULTIPLE PART PATH OPTION
	    CALL EGRSTA2(JZ,BSTA(IMODE,ista),STASTA,STAZNE,
     *   BDSTA((imode+5),ista),IMODE,STAZONE,STAEGR)
	    CALL BUTL(ista,IZ,JZ,BDIST(imode,ista),BSTA(IMODE,ista),
     *   BDSTA((imode+5),ista),
     *   BUTIL((imode+5),ista),STASTA,STAZNE,IMODE,STAEGR)
      SC=BDSTA((IMODE+5),ISTA)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      SC2=BDSTA(IMODE,ISTA)-MAX_IZONES
      IF(SC2.LT.0) SC2=MAX_STATIONS
      EGRUTIL1=COEFF(7)*STAZNE(3,SC2,JZ)
      IF(TRNEGR) EGRUTIL1=0.0
      UTIL1=BUTIL(IMODE,ISTA)+EGRUTIL1
      EGRUTIL2=COEFF(7)*STAZNE(3,SC,JZ)
      IF(TRNEGR) EGRUTIL2=0.0
      UTIL2=BUTIL((IMODE+5),ISTA)+EGRUTIL2
      IF((UTIL2.GT.UTIL1).AND.(SC.NE.MAX_STATIONS)) THEN
      CHBIND(IMODE,ISTA)=.TRUE.
      ELSE
      CHBIND(IMODE,ISTA)=.FALSE.
      END IF
      IF(BUTIL(IMODE,ISTA).EQ.0.0.AND.BUTIL((IMODE+5),ISTA).NE.0.0)
     *   CHBIND(IMODE,ISTA)=.TRUE.
C.................................................................
      IF(DEBUG) THEN
      WRITE(26,838) BUTIL(IMODE,ISTA),EGRUTIL1,UTIL1,BDSTA(IMODE,ISTA),
     *              STANAME(SC2),
     *              BUTIL((IMODE+5),ISTA),EGRUTIL2,UTIL2,
     *              BDSTA((IMODE+5),ISTA),STANAME(SC),CHBIND(IMODE,ISTA)
  838 FORMAT(/' STANDARD UTILITY=',F10.5,' WALK EGRESS UTIL=',F8.2,
     *       ' TOTAL UTILITY=',F10.5,' EGRESS STATION=',I4,1X,A37/
     *       ' COMPLEX  UTILITY=',F10.5,' WALK EGRESS UTIL=',F8.2,
     *       ' TOTAL UTILITY=',F10.5,' EGRESS STATION=',I4,1X,A37/
     *       ' BUS ACCESS CHOICE INDICATOR=',L1/)
      END IF
C................................................................
	    IMODE=5
	    IF(DEBUG) WRITE(26,41) ISTA,NAME(IMODE)
	    CALL EGRSTA(JZ,BSTA(imode,ista),STASTA,STAZNE,
     *   BDSTA(imode,ista),IMODE,STAZONE,STAEGR)
      CALL BUTL(ista,IZ,JZ,BDIST(imode,ista),BSTA(imode,ista),
     *  BDSTA(imode,ista),BUTIL(imode,ista),STASTA,STAZNE,IMODE,STAEGR)
      ASTA(IMODE,(ISTA+2))=BDSTA(IMODE,ISTA)
c....EVALUATE MULTIPLE PART PATH OPTION
	    CALL EGRSTA2(JZ,BSTA(IMODE,ista),STASTA,STAZNE,
     *   BDSTA((imode+5),ista),IMODE,STAZONE,STAEGR)
	    CALL BUTL(ista,IZ,JZ,BDIST(imode,ista),BSTA(IMODE,ista),
     *   BDSTA((imode+5),ista),
     *   BUTIL((imode+5),ista),STASTA,STAZNE,IMODE,STAEGR)
      SC=BDSTA((IMODE+5),ISTA)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      SC2=BDSTA(IMODE,ISTA)-MAX_IZONES
      IF(SC2.LT.0) SC2=MAX_STATIONS
      EGRUTIL1=COEFF(7)*STAZNE(3,SC2,JZ)
      IF(TRNEGR) EGRUTIL1=0.0
      UTIL1=BUTIL(IMODE,ISTA)+EGRUTIL1
      EGRUTIL2=COEFF(7)*STAZNE(3,SC,JZ)
      IF(TRNEGR) EGRUTIL2=0.0
      UTIL2=BUTIL((IMODE+5),ISTA)+EGRUTIL2
      IF((UTIL2.GT.UTIL1).AND.(SC.NE.MAX_STATIONS)) THEN
      CHBIND(IMODE,ISTA)=.TRUE.
      ELSE
      CHBIND(IMODE,ISTA)=.FALSE.
      END IF
      IF(BUTIL(IMODE,ISTA).EQ.0.0.AND.BUTIL((IMODE+5),ISTA).NE.0.0)
     *   CHBIND(IMODE,ISTA)=.TRUE.
C.................................................................
      IF(DEBUG) THEN
      WRITE(26,838) BUTIL(IMODE,ISTA),EGRUTIL1,UTIL1,BDSTA(IMODE,ISTA),
     *              STANAME(SC2),
     *              BUTIL((IMODE+5),ISTA),EGRUTIL2,UTIL2,
     *              BDSTA((IMODE+5),ISTA),STANAME(SC),CHBIND(IMODE,ISTA)
      END IF
C................................................................
C
	    IMODE=2
	    IF(DEBUG) WRITE(26,41) ISTA,NAME(IMODE)
	    CALL EGRSTA(JZ,BSTA(imode,ista),STASTA,STAZNE,
     *   BDSTA(imode,ista),IMODE,STAZONE,STAEGR)
      CALL BUTL(ista,IZ,JZ,BDIST(imode,ista),BSTA(imode,ista),
     *  BDSTA(imode,ista),BUTIL(imode,ista),STASTA,STAZNE,IMODE,STAEGR)
      ASTA(IMODE,(ISTA+2))=BDSTA(IMODE,ISTA)
c....EVALUATE MULTIPLE PART PATH OPTION
	    CALL EGRSTA2(JZ,BSTA(IMODE,ista),STASTA,STAZNE,
     *   BDSTA((imode+5),ista),IMODE,STAZONE,STAEGR)
	    CALL BUTL(ista,IZ,JZ,BDIST(imode,ista),BSTA(IMODE,ista),
     *   BDSTA((imode+5),ista),
     *   BUTIL((imode+5),ista),STASTA,STAZNE,IMODE,STAEGR)
      SC=BDSTA((IMODE+5),ISTA)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      SC2=BDSTA(IMODE,ISTA)-MAX_IZONES
      IF(SC2.LT.0) SC2=MAX_STATIONS
      EGRUTIL1=COEFF(7)*STAZNE(3,SC2,JZ)
      IF(TRNEGR) EGRUTIL1=0.0
      UTIL1=BUTIL(IMODE,ISTA)+EGRUTIL1
      EGRUTIL2=COEFF(7)*STAZNE(3,SC,JZ)
      IF(TRNEGR) EGRUTIL2=0.0
      UTIL2=BUTIL((IMODE+5),ISTA)+EGRUTIL2
      IF((UTIL2.GT.UTIL1).AND.(SC.NE.MAX_STATIONS)) THEN
      CHBIND(IMODE,ISTA)=.TRUE.
      ELSE
      CHBIND(IMODE,ISTA)=.FALSE.
      END IF
      IF(BUTIL(IMODE,ISTA).EQ.0.0.AND.BUTIL((IMODE+5),ISTA).NE.0.0)
     *   CHBIND(IMODE,ISTA)=.TRUE.
C.................................................................
      IF(DEBUG) THEN
      WRITE(26,838) BUTIL(IMODE,ISTA),EGRUTIL1,UTIL1,BDSTA(IMODE,ISTA),
     *              STANAME(SC2),
     *              BUTIL((IMODE+5),ISTA),EGRUTIL2,UTIL2,
     *              BDSTA((IMODE+5),ISTA),STANAME(SC),CHBIND(IMODE,ISTA)
      END IF
C................................................................
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
     *          IMODE,STAZONE,STAEGR)
      DSTA(IMODE,IX)=ITEMP2
      CALL PUTL(IX,IZ,JZ,PDIST(imode,IX),ZINDCR(IX),
     *  DSTA(imode,IX),PUTIL(imode,IX),STASTA,STAZNE,TAB1DA,TAB2DA,
     *  IMODE,PNRRAT,PNRRAT2,STAEGR)
      CRPNR(IX)=PNRRAT
      CRPNR2(IX)=PNRRAT2
      CALL KUTL(IX,IZ,JZ,KDIST(imode,IX),ZINDCR(IX),
     *  DSTA(imode,IX),KUTIL(imode,IX),STASTA,STAZNE,TAB1DA,TAB2DA,
     *  IMODE,STAEGR,ZHHD,UBERUTIL(imode,ix),UBERCOST(imode,ix))
C...URBAN RAIL
      IMODE=2
      IF(DEBUG) WRITE(26,341) IX,NAME(IMODE)
	    ITEMP=ZINDUR(IX)+MAX_IZONES
      CALL EGRSTA(JZ,ITEMP,STASTA,STAZNE,ITEMP2,
     *          IMODE,STAZONE,STAEGR)
	    DSTA(IMODE,IX)=ITEMP2
      CALL PUTL(IX,IZ,JZ,PDIST(imode,IX),ZINDUR(IX),
     *  DSTA(imode,IX),PUTIL(imode,IX),STASTA,STAZNE,TAB1DA,TAB2DA,
     *  IMODE,PNRRAT,PNRRAT2,STAEGR)
      CALL KUTL(IX,IZ,JZ,KDIST(imode,IX),ZINDUR(IX),
     *  DSTA(imode,IX),KUTIL(imode,IX),STASTA,STAZNE,TAB1DA,TAB2DA,
     *  IMODE,STAEGR,ZHHD,UBERUTIL(imode,ix),UBERCOST(imode,ix))
C...BRT
      IMODE=5
      IF(DEBUG) WRITE(26,341) IX,NAME(IMODE)
	    ITEMP=ZINDBR(IX)+MAX_IZONES
      CALL EGRSTA(JZ,ITEMP,STASTA,STAZNE,ITEMP2,
     *          IMODE,STAZONE,STAEGR)
	    DSTA(IMODE,IX)=ITEMP2
      CALL PUTL(IX,IZ,JZ,PDIST(imode,IX),ZINDBR(IX),
     *  DSTA(imode,IX),PUTIL(imode,IX),STASTA,STAZNE,TAB1DA,TAB2DA,
     *  IMODE,PNRRAT,PNRRAT2,STAEGR)
      CALL KUTL(IX,IZ,JZ,KDIST(imode,IX),ZINDBR(IX),
     *  DSTA(imode,IX),KUTIL(imode,IX),STASTA,STAZNE,TAB1DA,TAB2DA,
     *  IMODE,STAEGR,ZHHD,UBERUTIL(imode,ix),UBERCOST(imode,ix))
  300 CONTINUE
C.....SORT DRIVE ACCESS STATIONS - COMMUTER RAIL
      IMODE=1
      CALL USORT(1,PSTA,PDIST,ZINDCR,DSTA,PDSTA,PUTIL,
     *           CRPNR,CRPNR2,IMODE,UBERUTIL,UBERCOST)
      OSTA(imode,5)=PSTA(imode,1)
      OSTA(imode,6)=PSTA(imode,2)
      OSTA(imode,7)=PSTA(imode,3)
      OSTA(imode,8)=PSTA(imode,4)
      ASTA(imode,5)=PDSTA(imode,1)
      ASTA(imode,6)=PDSTA(imode,2)
      ASTA(imode,7)=PDSTA(imode,3)
      ASTA(imode,8)=PDSTA(imode,4)
      CALL USORT(2,KSTA,KDIST,ZINDCR,DSTA,KDSTA,KUTIL,
     *           CRDUM,CRDUM,IMODE,UBERUTIL,UBERCOST)
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
      CALL USORT(1,PSTA,PDIST,ZINDUR,DSTA,PDSTA,PUTIL,
     *           CRDUM,CRDUM,IMODE,UBERUTIL,UBERCOST)
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
      CALL USORT(2,KSTA,KDIST,ZINDUR,DSTA,KDSTA,KUTIL,
     *           CRDUM,CRDUM,IMODE,UBERUTIL,UBERCOST)
      OSTA(imode,9)=KSTA(imode,1)
      OSTA(imode,10)=KSTA(imode,2)
      OSTA(imode,11)=KSTA(imode,3)
      OSTA(imode,12)=KSTA(imode,4)
      ASTA(imode,9)=KDSTA(imode,1)
      ASTA(imode,10)=KDSTA(imode,2)
      ASTA(imode,11)=KDSTA(imode,3)
      ASTA(imode,12)=KDSTA(imode,4)
C.....SORT DRIVE ACCESS STATIONS - BRT
      IMODE=5
      CALL USORT(1,PSTA,PDIST,ZINDBR,DSTA,PDSTA,PUTIL,
     *           CRDUM,CRDUM,IMODE,UBERUTIL,UBERCOST)
      OSTA(imode,5)=PSTA(imode,1)
      OSTA(imode,6)=PSTA(imode,2)
      OSTA(imode,7)=PSTA(imode,3)
      OSTA(imode,8)=PSTA(imode,4)
      ASTA(imode,5)=PDSTA(imode,1)
      ASTA(imode,6)=PDSTA(imode,2)
      ASTA(imode,7)=PDSTA(imode,3)
      ASTA(imode,8)=PDSTA(imode,4)
      CALL USORT(2,KSTA,KDIST,ZINDBR,DSTA,KDSTA,KUTIL,
     *           CRDUM,CRDUM,IMODE,UBERUTIL,UBERCOST)
      OSTA(imode,9)=KSTA(imode,1)
      OSTA(imode,10)=KSTA(imode,2)
      OSTA(imode,11)=KSTA(imode,3)
      OSTA(imode,12)=KSTA(imode,4)
      ASTA(imode,9)=KDSTA(imode,1)
      ASTA(imode,10)=KDSTA(imode,2)
      ASTA(imode,11)=KDSTA(imode,3)
      ASTA(imode,12)=KDSTA(imode,4)
C...STORE INDICATOR VALUES - URBAN RAIL
      DO 51 K1=1,12
      SC1=ASTA(2,K1)-MAX_IZONES
      IF(SC1.LT.0) SC1=MAX_STATIONS
      DO 52 K2=1,5 
      IF(STAZNEI(SC1,JZ,2,K2).GT.0) MODEINC(2,K2,K1,2)=1
   52 CONTINUE
   51 CONTINUE
C **************************************************************
C     MULTIPLE PATH EVALUATION FOR COMMUTER RAIL, URBAN RAIL & BRT
C **************************************************************
      do imode=1,5
      IF(IMODE.EQ.3.OR.IMODE.EQ.4) CYCLE
      if(debug) write(26,846) name(imode)
  846 format(/' EVALUATING P&R AND K&R EGRESS FOR ',A13/
     *       ' ----------------------------------'/)
      do ista=1,4
c....EVALUATE MULTIPLE PART PATH OPTION FOR EACH P&R STATION
	    CALL EGRSTA2(JZ,PSTA(IMODE,ista),STASTA,STAZNE,
     *   PDSTA((imode+5),ista),IMODE,STAZONE,STAEGR)
      ITEMP=PSTA(IMODE,ISTA)-MAX_IZONES
      CALL PUTL(ISTA,IZ,JZ,PDIST(imode,ISTA),ITEMP,
     *  PDSTA((imode+5),ISTA),PUTIL((imode+5),ISTA),
     *  STASTA,STAZNE,TAB1DA,TAB2DA,
     *  IMODE,PNRRAT,PNRRAT2,STAEGR)
      SC=PDSTA((IMODE+5),ISTA)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      SC2=PDSTA(IMODE,ISTA)-MAX_IZONES
      IF(SC2.LT.0) SC2=MAX_STATIONS
      EGRUTIL1=COEFF(7)*STAZNE(3,SC2,JZ)
      IF(TRNEGR) EGRUTIL1=0.0
      UTIL1=PUTIL(IMODE,ISTA)+EGRUTIL1
      EGRUTIL2=COEFF(7)*STAZNE(3,SC,JZ)
      IF(TRNEGR) EGRUTIL2=0.0
      UTIL2=PUTIL((IMODE+5),ISTA)+EGRUTIL2
      IF((UTIL2.GT.UTIL1).AND.(SC.NE.MAX_STATIONS)) THEN
      CHPIND(IMODE,ISTA)=.TRUE.
      ELSE
      CHPIND(IMODE,ISTA)=.FALSE.
      END IF
      IF(PUTIL(IMODE,ISTA).EQ.0.0.AND.PUTIL((IMODE+5),ISTA).NE.0.0)
     *   CHPIND(IMODE,ISTA)=.TRUE.
C.................................................................
      IF(DEBUG) THEN
      WRITE(26,839) PUTIL(IMODE,ISTA),EGRUTIL1,UTIL1,PDSTA(IMODE,ISTA),
     *              STANAME(SC2),
     *              PUTIL((IMODE+5),ISTA),EGRUTIL2,UTIL2,
     *              PDSTA((IMODE+5),ISTA),STANAME(SC),CHPIND(IMODE,ISTA)
  839 FORMAT(/' STANDARD UTILITY=',F10.5,' WALK EGRESS UTIL=',F8.2,
     *       ' TOTAL UTILITY=',F10.5,' EGRESS STATION=',I4,1X,A37/
     *       ' COMPLEX  UTILITY=',F10.5,' WALK EGRESS UTIL=',F8.2,
     *       ' TOTAL UTILITY=',F10.5,' EGRESS STATION=',I4,1X,A37/
     *       ' P&R ACCESS CHOICE INDICATOR=',L1/)
      END IF
C................................................................
c....EVALUATE MULTIPLE PART PATH OPTION FOR EACH K&R STATION
	    CALL EGRSTA2(JZ,KSTA(IMODE,ista),STASTA,STAZNE,
     *   KDSTA((imode+5),ista),IMODE,STAZONE,STAEGR)
      ITEMP=KSTA(IMODE,ISTA)-MAX_IZONES
      CALL KUTL(ISTA,IZ,JZ,KDIST(imode,ISTA),ITEMP,
     *  KDSTA((imode+5),ISTA),KUTIL((imode+5),ISTA),
     *  STASTA,STAZNE,TAB1DA,TAB2DA,
     *  IMODE,STAEGR,ZHHD,UBERUTIL(imode,ista),UBERCOST(imode,ista))
      SC=KDSTA((IMODE+5),ISTA)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      SC2=KDSTA(IMODE,ISTA)-MAX_IZONES
      IF(SC2.LT.0) SC2=MAX_STATIONS
      EGRUTIL1=COEFF(7)*STAZNE(3,SC2,JZ)
      IF(TRNEGR) EGRUTIL1=0.0
      UTIL1=KUTIL(IMODE,ISTA)+EGRUTIL1
      EGRUTIL2=COEFF(7)*STAZNE(3,SC,JZ)
      IF(TRNEGR) EGRUTIL2=0.0
      UTIL2=KUTIL((IMODE+5),ISTA)+EGRUTIL2
      IF((UTIL2.GT.UTIL1).AND.(SC.NE.MAX_STATIONS)) THEN
      CHKIND(IMODE,ISTA)=.TRUE.
      ELSE
      CHKIND(IMODE,ISTA)=.FALSE.
      END IF
      IF(KUTIL(IMODE,ISTA).EQ.0.0.AND.KUTIL((IMODE+5),ISTA).NE.0.0)
     *   CHKIND(IMODE,ISTA)=.TRUE.
C.................................................................
      IF(DEBUG) THEN
      WRITE(26,845) KUTIL(IMODE,ISTA),EGRUTIL1,UTIL1,KDSTA(IMODE,ISTA),
     *              STANAME(SC2),
     *              KUTIL((IMODE+5),ISTA),EGRUTIL2,UTIL2,
     *              KDSTA((IMODE+5),ISTA),STANAME(SC),CHKIND(IMODE,ISTA)
  845 FORMAT(/' STANDARD UTILITY=',F10.5,' WALK EGRESS UTIL=',F8.2,
     *       ' TOTAL UTILITY=',F10.5,' EGRESS STATION=',I4,1X,A37/
     *       ' COMPLEX  UTILITY=',F10.5,' WALK EGRESS UTIL=',F8.2,
     *       ' TOTAL UTILITY=',F10.5,' EGRESS STATION=',I4,1X,A37/
     *       ' K&R ACCESS CHOICE INDICATOR=',L1/)
      END IF
C................................................................
      end do
      end do
C
C---------------------------------------------------------------------
C            WALK --> LOCAL BUS UTILITY COMPUTATION                   |
C---------------------------------------------------------------------
C   COMPUTE MODE CHOICE MODEL UTILITY VALUE FOR WALK TO LOCAL BUS
C
      WAIT1A=0.0
      WAIT1B=0.0
      IF(BIVT(1,JZ).GT.0.0) THEN
      IF(LUNREL) LUNRELV(1)=LUNRELM(JZ,1)
      IF(NUNREL) NUNRELV(1)=NUNRELM(JZ,1)
      IF(LCROWD) LCROWDV(1)=LCROWDM(JZ,1)
      IF(NCAPAC) NCAPACV(1)=NCAPACM(JZ,1)
C
C  UTILITY COMPUTATION
C
      LSUM2LB=0.60
      WAIT1A=AMIN1(WAIT1(1,JZ),WAITLT)
      WAIT1B=DIM(WAIT1(1,JZ),WAITLT)
      WBUTL=COEFF(11)*BIVT(1,JZ) + COEFF(12)*WAIT1A +
     *        COEFF(13)*WAIT1B + COEFF(15)*WAIT2(1,JZ) +
     *        COEFF(17)*WALKTFR(1,JZ) +
     *        COEFF(31)*FLOAT(TXFER(1,JZ))  +
     *        COEFF(75)*LUNRELM(JZ,1) +
     *        COEFF(76)*NUNRELM(JZ,1) +
     *        COEFF(77)*LCROWDM(JZ,1) +
     *        COEFF(78)*NCAPACM(JZ,1)
      WLKBACC=WALKACC(1,JZ)
      WLKBEGR=WALKEGR(1,JZ)
      WBUTL=WBUTL/(LSUM1TRN*LSUM2LB)
      ELSE
      WBUTL=0.0
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9043) BIVT(1,JZ),FARE(1,JZ),TXFER(1,JZ),WAIT1(1,JZ),
     *               WAIT1A,WAIT1B,WAIT2(1,JZ),WALKTFR(1,JZ),
     *               WALKACC(1,JZ),WALKEGR(1,JZ),LUNRELM(JZ,1),
     *               NUNRELM(JZ,1),LCROWDM(JZ,1),NCAPACM(JZ,1),
     *               WBUTL
 9043 FORMAT(//1X,'Walk --> Local Bus Level-Of-Service Data'/
     *       1X,  '----------------------------------------'//
     *       1X,'TOTAL IVTT           =',F8.2/
     *       1X,'BUS   FARE           =',F8.2/
     *       1X,'BUS   TRANSFERS      =',I8/
     *       1X,'BUS   1ST WAIT       =',F8.2/
     *       1X,'BUS   1ST WAIT (<5)  =',F8.2/
     *       1X,'BUS   1ST WAIT (>5)  =',F8.2/
     *       1X,'BUS   2ND WAIT       =',F8.2/
     *       1X,'BUS   TRANSFER WALK  =',F8.2/
     *       1X,'BUS   ACCESS   WALK  =',F8.2/
     *       1X,'BUS   EGRESS   WALK  =',F8.2/
     *       1X,'LINK  UNRELIABILITY  =',F8.2/
     *       1X,'STOP  UNRELIABILITY  =',F8.2/
     *       1X,'LINK  CROWDING TIME  =',F8.2/
     *       1X,'STOP  CAPACITY TIME  =',F8.2/
     *       1X,'MODE CHOICE MODEL UTILITY=',F10.5/)
      END IF
C
C---------------------------------------------------------------------
C            BIKE --> LOCAL BUS UTILITY COMPUTATION                   |
C---------------------------------------------------------------------
C   COMPUTE MODE CHOICE MODEL UTILITY VALUE FOR BIKE TO LOCAL BUS
C
      WAIT1A=0.0
      WAIT1B=0.0
      IF(BIVT(6,JZ).GT.0.0) THEN
C
C  UTILITY COMPUTATION
C
      WAIT1A=0.0
      WAIT1B=0.0
      WAIT1A=AMIN1(WAIT1(6,JZ),WAITLT)
      WAIT1B=DIM(WAIT1(6,JZ),WAITLT)
      BBUTL=COEFF(11)*BIVT(6,JZ) + COEFF(12)*WAIT1A +
     *        COEFF(13)*WAIT1B + COEFF(15)*WAIT2(6,JZ) +
     *        COEFF(17)*WALKTFR(6,JZ) + UTLZBIK +
     *        COEFF(32)*FLOAT(TXFER(6,JZ))  +
     *        COEFF(75)*LUNRELMBK(JZ,1) +
     *        COEFF(76)*NUNRELMBK(JZ,1) +
     *        COEFF(77)*LCROWDMBK(JZ,1) +
     *        COEFF(78)*NCAPACMBK(JZ,1)
      BIKBACC=WALKACC(6,JZ)
      BIKBEGR=WALKEGR(6,JZ)
      BBUTL=BBUTL/(LSUM1TRN*LSUM2LB)
      ELSE
      BBUTL=0.0
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9143) BIVT(6,JZ),FARE(6,JZ),TXFER(6,JZ),WAIT1(6,JZ),
     *               WAIT1A,WAIT1B,WAIT2(6,JZ),WALKTFR(6,JZ),
     *               WALKACC(6,JZ),WALKEGR(6,JZ),LUNRELMBK(JZ,1),
     *               NUNRELMBK(JZ,1),LCROWDMBK(JZ,1),NCAPACMBK(JZ,1),
     *               UTLZBIK,BBUTL
 9143 FORMAT(//1X,'Bike --> Local Bus Level-Of-Service Data'/
     *       1X,  '----------------------------------------'//
     *       1X,'TOTAL IVTT           =',F8.2/
     *       1X,'BUS   FARE           =',F8.2/
     *       1X,'BUS   TRANSFERS      =',I8/
     *       1X,'BUS   1ST WAIT       =',F8.2/
     *       1X,'BUS   1ST WAIT (<5)  =',F8.2/
     *       1X,'BUS   1ST WAIT (>5)  =',F8.2/
     *       1X,'BUS   2ND WAIT       =',F8.2/
     *       1X,'BUS   TRANSFER WALK  =',F8.2/
     *       1X,'BUS   ACCESS   BIKE  =',F8.2/
     *       1X,'BUS   EGRESS   BIKE  =',F8.2/
     *       1X,'LINK  UNRELIABILITY  =',F8.2/
     *       1X,'STOP  UNRELIABILITY  =',F8.2/
     *       1X,'LINK  CROWDING TIME  =',F8.2/
     *       1X,'STOP  CAPACITY TIME  =',F8.2/
     *       1X,'COMMON BIKE UTILITY  =',F8.4/
     *       1X,'MODE CHOICE MODEL UTILITY=',F10.5/)
      END IF
C
C---------------------------------------------------------------------
C            WALK --> RAPID BUS UTILITY COMPUTATION                   |
C---------------------------------------------------------------------
C   COMPUTE MODE CHOICE MODEL UTILITY VALUE FOR WALK TO RAPID BUS
C
      WAIT1A=0.0
      WAIT1B=0.0
      IF(RIVT(4,JZ).GT.0.0) THEN
      IF(LUNREL) LUNRELV(2)=LUNRELM(JZ,2)+LUNRELM(JZ,3)
      IF(NUNREL) NUNRELV(2)=NUNRELM(JZ,2)+NUNRELM(JZ,3)
      IF(LCROWD) LCROWDV(2)=LCROWDM(JZ,2)+LCROWDM(JZ,3)
      IF(NCAPAC) NCAPACV(2)=NCAPACM(JZ,2)+NCAPACM(JZ,3)
C...UTILITY COMPUTATIONS
      WAIT1A=AMIN1(WAIT1(4,JZ),WAITLT)
      WAIT1B=DIM(WAIT1(4,JZ),WAITLT)
      WRUTL=COEFF(11)*TIVT(4,JZ) + COEFF(12)*WAIT1A +
     *        COEFF(13)*WAIT1B + COEFF(15)*WAIT2(4,JZ) +
     *        COEFF(17)*WALKTFR(4,JZ) +
     *        COEFF(34)*FLOAT(TXFER(4,JZ))   +
     *        COEFF(75)*(LUNRELM(JZ,2)+LUNRELM(JZ,3)) +
     *        COEFF(76)*(NUNRELM(JZ,2)+NUNRELM(JZ,3)) +
     *        COEFF(77)*(LCROWDM(JZ,2)+LCROWDM(JZ,3)) +
     *        COEFF(78)*(NCAPACM(JZ,2)+NCAPACM(JZ,3))
      WRUTL=WRUTL/(LSUM1TRN*LSUM2RB)
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
     *               WALKACC(4,JZ),WALKEGR(4,JZ),
     *               LUNRELM(JZ,2),LUNRELM(JZ,3),
     *               NUNRELM(JZ,2),NUNRELM(JZ,3),
     *               LCROWDM(JZ,2),LCROWDM(JZ,3),
     *               NCAPACM(JZ,2),NCAPACM(JZ,3),WRUTL
 9093 FORMAT(//1X,'Walk --> Rapid Bus Level-Of-Service Data'/
     *       1X,  '----------------------------------------'//
     *       1X,'TOTAL IVTT           =',F8.2/
     *       1X,'LOCAL IVTT           =',F8.2/
     *       1X,'RAPID IVTT           =',F8.2/
     *       1X,'RAPID FARE           =',F8.2/
     *       1X,'RAPID TRANSFERS      =',I8/
     *       1X,'RAPID 1ST WAIT       =',F8.2/
     *       1X,'RAPID 1ST WAIT (<5)  =',F8.2/
     *       1X,'RAPID 1ST WAIT (>5)  =',F8.2/
     *       1X,'RAPID 2ND WAIT       =',F8.2/
     *       1X,'RAPID TRANSFER WALK  =',F8.2/
     *       1X,'RAPID ACCESS   WALK  =',F8.2/
     *       1X,'RAPID EGRESS   WALK  =',F8.2/
     *       1X,'LOCAL BUS LINK UNREL =',F8.2/
     *       1X,'RAPID BUS LINK UNREL =',F8.2/
     *       1X,'LOCAL BUS STOP UNREL =',F8.2/
     *       1X,'RAPID BUS STOP UNREL =',F8.2/
     *       1X,'LOCAL BUS CROWD TIME =',F8.2/
     *       1X,'RAPID BUS CROWD TIME =',F8.2/
     *       1X,'LOCAL BUS STOP CAPAC =',F8.2/
     *       1X,'RAPID BUS STOP CAPAC =',F8.2/
     *       1X,'MODE CHOICE MODEL UTILITY=',F10.5/)
      END IF
C
C---------------------------------------------------------------------
C            BIKE --> RAPID BUS UTILITY COMPUTATION                   |
C---------------------------------------------------------------------
C   COMPUTE MODE CHOICE MODEL UTILITY VALUE FOR BIKE TO RAPID BUS
C
      WAIT1A=0.0
      WAIT1B=0.0
      IF(RIVT(7,JZ).GT.0.0) THEN
C...UTILITY COMPUTATIONS
      WAIT1A=AMIN1(WAIT1(7,JZ),WAITLT)
      WAIT1B=DIM(WAIT1(7,JZ),WAITLT)
      BRUTL=COEFF(11)*TIVT(7,JZ) + COEFF(12)*WAIT1A +
     *        COEFF(13)*WAIT1B + COEFF(15)*WAIT2(7,JZ) +
     *        COEFF(17)*WALKTFR(7,JZ) + UTLZBIK +
     *        COEFF(35)*FLOAT(TXFER(7,JZ)) +
     *        COEFF(75)*(LUNRELMBK(JZ,2)+LUNRELMBK(JZ,3)) +
     *        COEFF(76)*(NUNRELMBK(JZ,2)+NUNRELMBK(JZ,3)) +
     *        COEFF(77)*(LCROWDMBK(JZ,2)+LCROWDMBK(JZ,3)) +
     *        COEFF(78)*(NCAPACMBK(JZ,2)+NCAPACMBK(JZ,3))
      BRUTL=BRUTL/(LSUM1TRN*LSUM2RB)
      BIKRACC=WALKACC(7,JZ)
      BIKREGR=WALKEGR(7,JZ)
       IF(BIVT(7,JZ).GT.0.0) THEN
       MODEINC(6,1,13,1)=1
       MODEINC(6,1,13,2)=1 
       END IF
      ELSE
      BRUTL=0.0
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9193) TIVT(7,JZ),BIVT(7,JZ),RIVT(7,JZ),
     *               FARE(7,JZ),TXFER(7,JZ),WAIT1(7,JZ),
     *               WAIT1A,WAIT1B,WAIT2(7,JZ),WALKTFR(7,JZ),
     *               WALKACC(7,JZ),WALKEGR(7,JZ),
     *               LUNRELMBK(JZ,2),LUNRELMBK(JZ,3),
     *               NUNRELMBK(JZ,2),NUNRELMBK(JZ,3),
     *               LCROWDMBK(JZ,2),LCROWDMBK(JZ,3),
     *               NCAPACMBK(JZ,2),NCAPACMBK(JZ,3),UTLZBIK,BRUTL
 9193 FORMAT(//1X,'Bike --> Rapid Bus Level-Of-Service Data'/
     *       1X,  '----------------------------------------'//
     *       1X,'TOTAL IVTT           =',F8.2/
     *       1X,'LOCAL IVTT           =',F8.2/
     *       1X,'RAPID IVTT           =',F8.2/
     *       1X,'RAPID FARE           =',F8.2/
     *       1X,'RAPID TRANSFERS      =',I8/
     *       1X,'RAPID 1ST WAIT       =',F8.2/
     *       1X,'RAPID 1ST WAIT (<5)  =',F8.2/
     *       1X,'RAPID 1ST WAIT (>5)  =',F8.2/
     *       1X,'RAPID 2ND WAIT       =',F8.2/
     *       1X,'RAPID TRANSFER WALK  =',F8.2/
     *       1X,'RAPID ACCESS   BIKE  =',F8.2/
     *       1X,'RAPID EGRESS   BIKE  =',F8.2/
     *       1X,'LOCAL BUS LINK UNREL =',F8.2/
     *       1X,'RAPID BUS LINK UNREL =',F8.2/
     *       1X,'LOCAL BUS STOP UNREL =',F8.2/
     *       1X,'RAPID BUS STOP UNREL =',F8.2/
     *       1X,'LOCAL BUS CROWD TIME =',F8.2/
     *       1X,'RAPID BUS CROWD TIME =',F8.2/
     *       1X,'LOCAL BUS STOP CAPAC =',F8.2/
     *       1X,'RAPID BUS STOP CAPAC =',F8.2/
     *       1X,'COMMON BIKE UTILITY  =',F8.4/
     *       1X,'MODE CHOICE MODEL UTILITY=',F10.5/)
      END IF
C---------------------------------------------------------------------
C            WALK --> TRANSITWAY BUS UTILITY COMPUTATION              
C---------------------------------------------------------------------
C   COMPUTE MODE CHOICE MODEL UTILITY VALUE FOR WALK TO TRANSITWAY BUS
C
      WAIT1A=0.0
      WAIT1B=0.0
	    CWTWY=0
	    wtutl=0.0
      if(wivt(3,jz).gt.0.0) then
      IF(LUNREL) LUNRELV(4)=LUNRELM(JZ,7)+LUNRELM(JZ,8)+
     *          LUNRELM(JZ,9)+LUNRELM(JZ,10)
      IF(NUNREL) NUNRELV(4)=NUNRELM(JZ,7)+NUNRELM(JZ,8)+
     *          NUNRELM(JZ,9)+NUNRELM(JZ,10)
      IF(LCROWD) LCROWDV(4)=LCROWDM(JZ,7)+LCROWDM(JZ,8)+
     *          LCROWDM(JZ,9)+LCROWDM(JZ,10)
      IF(NCAPAC) NCAPACV(4)=NCAPACM(JZ,7)+NCAPACM(JZ,8)+
     *          NCAPACM(JZ,9)+NCAPACM(JZ,10)
C
	    WAIT1A=AMIN1(WAIT1(3,JZ),WAITLT)
      WAIT1B=DIM(WAIT1(3,JZ),WAITLT)
      WTUTL=COEFF(11)*TIVT(3,JZ) + COEFF(12)*WAIT1A +
     *        COEFF(13)*WAIT1B + COEFF(15)*WAIT2(3,JZ) +
     *        COEFF(17)*WALKTFR(3,JZ) +
     *        COEFF(40)*FLOAT(TXFER(3,JZ)) +
     *        COEFF(75)*LUNRELV(4) +
     *        COEFF(76)*NUNRELV(4) +
     *        COEFF(77)*LCROWDV(4) +
     *        COEFF(78)*NCAPACV(4) -
     *        (1.0-(WIVT(3,JZ)/TIVT(3,JZ)))*6.0
      WTUTL=WTUTL/(LSUM1TRN*LSUM2TW)
      WLKWTACC=WALKACC(3,JZ)
      WLKWTEGR=WALKEGR(3,JZ)
C...OBTAIN STATION NUMBER
      CWTWY=BUSTAT(1,JZ)
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
 5740   CONTINUE
C       WRITE(75,5742) CWTWY
 5742   FORMAT(' MTAMC 5742 (F) - TRANSITWAY STATION NOT FOUND',
     *      ' FOR NODE ',I6)
        WRITE(97,5747) CWTWY
 5747   FORMAT(I5)
C       STOP 5742
        CWTWY=0
        END IF
 5741   CONTINUE
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
      SC=CWTWY-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(CWTWY.GT.0) X=STADATA((CWTWY-MAX_IZONES),9)
      WRITE(26,9045) TIVT(3,JZ),BIVT(3,JZ),EIVT(3,JZ),WIVT(3,JZ),
     *               RIVT(3,JZ),
     *               FARE(3,JZ),TXFER(3,JZ),WAIT1(3,JZ),WAIT1A,WAIT1B,
     *               WAIT2(3,JZ),WALKTFR(3,JZ),WALKACC(3,JZ),
     *               WALKEGR(3,JZ),
     *               LUNRELM(JZ,7),LUNRELM(JZ,8),
     *               LUNRELM(JZ,9),LUNRELM(JZ,10),
     *               NUNRELM(JZ,7),NUNRELM(JZ,8),
     *               NUNRELM(JZ,9),NUNRELM(JZ,10),
     *               LCROWDM(JZ,7),LCROWDM(JZ,8),
     *               LCROWDM(JZ,9),LCROWDM(JZ,10),
     *               NCAPACM(JZ,7),NCAPACM(JZ,8),
     *               NCAPACM(JZ,9),NCAPACM(JZ,10),
     *               KBTWYW,CWTWY,STANAME(SC),
     *               X,WTUTL
 9045 FORMAT(//1X,'Walk --> Transitway Bus Level-Of-Service Data'/
     *       1X,  '----------------------------------'//
     *       1X,'TOTAL IVTT                =',F8.2/
     *       1X,'LOCAL IVTT                =',F8.2/
     *       1X,'EXPRESS IVTT              =',F8.2/
     *       1X,'TRANSITWAY IVTT           =',F8.2/
     *       1X,'RAPID BUS IVTT            =',F8.2/
     *       1X,'TRANSITWAY FARE           =',F8.2/
     *       1X,'TRANSITWAY TRANSFERS      =',I8/
     *       1X,'TRANSITWAY 1ST WAIT       =',F8.2/
     *       1X,'TRANSITWAY 1ST WAIT (<5)  =',F8.2/
     *       1X,'TRANSITWAY 1ST WAIT (>5)  =',F8.2/
     *       1X,'TRANSITWAY 2ND WAIT       =',F8.2/
     *       1X,'TRANSITWAY WALK TRANSFER  =',F8.2/
     *       1X,'TRANSITWAY WALK ACCESS    =',F8.2/
     *       1X,'TRANSITWAY WALK EGRESS    =',F8.2/
     *       1X,'LOCAL BUS   LINK UNREL    =',F8.2/
     *       1X,'RAPID BUS   LINK UNREL    =',F8.2/
     *       1X,'EXPRESS BUS LINK UNREL    =',F8.2/
     *       1X,'TRANSITWAY  LINK UNREL    =',F8.2/
     *       1X,'LOCAL BUS   NODE UNREL    =',F8.2/
     *       1X,'RAPID BUS   NODE UNREL    =',F8.2/
     *       1X,'EXPRESS BUS NODE UNREL    =',F8.2/
     *       1X,'TRANSITWAY  NODE UNREL    =',F8.2/
     *       1X,'LOCAL BUS   LINK CROWD    =',F8.2/
     *       1X,'RAPID BUS   LINK CROWD    =',F8.2/
     *       1X,'EXPRESS BUS LINK CROWD    =',F8.2/
     *       1X,'TRANSITWAY  LINK CROWD    =',F8.2/
     *       1X,'LOCAL BUS   NODE CAPAC    =',F8.2/
     *       1X,'RAPID BUS   NODE CAPAC    =',F8.2/
     *       1X,'EXPRESS BUS NODE CAPAC    =',F8.2/
     *       1X,'TRANSITWAY  NODE CAPAC    =',F8.2/
     *       1X,'TRANSITWAY BUS TXFER CNST =',F8.2/
     *       1X,'TRANSITWAY ACCESS STATION =',I8,1X,A37/
     *       1X,'TRANSITWAY ACCESS STA WALK=',F8.2/
     *       1X,'MODE CHOICE MODEL UTILITY=',F10.5/)
      END IF
C -----------------------------------------------------------------
C---------------------------------------------------------------------
C            BIKE --> TRANSITWAY BUS UTILITY COMPUTATION              
C---------------------------------------------------------------------
C   COMPUTE MODE CHOICE MODEL UTILITY VALUE FOR WALK TO TRANSITWAY BUS
C
      WAIT1A=0.0
      WAIT1B=0.0
      if(wivt(9,jz).gt.0.0) then
      IF(LUNREL) LUNRELV(4)=LUNRELMBK(JZ,7)+LUNRELMBK(JZ,8)+
     *          LUNRELMBK(JZ,9)+LUNRELMBK(JZ,10)
      IF(NUNREL) NUNRELV(4)=NUNRELMBK(JZ,7)+NUNRELMBK(JZ,8)+
     *          NUNRELMBK(JZ,9)+NUNRELMBK(JZ,10)
      IF(LCROWD) LCROWDV(4)=LCROWDMBK(JZ,7)+LCROWDMBK(JZ,8)+
     *          LCROWDMBK(JZ,9)+LCROWDMBK(JZ,10)
      IF(NCAPAC) NCAPACV(4)=NCAPACMBK(JZ,7)+NCAPACMBK(JZ,8)+
     *          NCAPACMBK(JZ,9)+NCAPACMBK(JZ,10)
C
	    WAIT1A=AMIN1(WAIT1(9,JZ),WAITLT)
      WAIT1B=DIM(WAIT1(9,JZ),WAITLT)
      BTUTL=COEFF(11)*TIVT(9,JZ) + COEFF(12)*WAIT1A +
     *        COEFF(13)*WAIT1B + COEFF(15)*WAIT2(9,JZ) +
     *        COEFF(17)*WALKTFR(9,JZ) + UTLZBIK +
     *        COEFF(41)*FLOAT(TXFER(9,JZ)) +
     *        COEFF(75)*LUNRELV(4) +
     *        COEFF(76)*NUNRELV(4) +
     *        COEFF(77)*LCROWDV(4) +
     *        COEFF(78)*NCAPACV(4) -
     *        (1.0-(WIVT(9,JZ)/TIVT(9,JZ)))*6.0
      BTUTL=BTUTL/(LSUM1TRN*LSUM2TW)
      BIKWTACC=WALKACC(9,JZ)
      BIKWTEGR=WALKEGR(9,JZ)
C...OBTAIN STATION NUMBER
      CBTWY=BUSTAT(3,JZ)
      IF(CBTWY.GT.0) THEN
      DO 5743 K=1,MAX_STATIONS
        IF(STANODE1(K).EQ.CBTWY) THEN
        L=K+MAX_IZONES
        IF(STANUM(K).EQ.4) THEN
        CBTWY=L
        GO TO 5744
        END IF
        END IF
        IF(STANODE2(K).EQ.CBTWY) THEN
        L=K+MAX_IZONES
        IF(STANUM(K).EQ.4) THEN
        CBTWY=L
        GO TO 5744
        END IF
        END IF 
 5743 CONTINUE
      WRITE(26,5745) CBTWY
 5745 FORMAT(' MTAMC 5745 (F) - TRANSITWAY BIKE STATION NOT FOUND',
     *      ' FOR NODE ',I6)
      STOP 5745
      END IF
 5744 CONTINUE
        IF(CBTWY.GT.0) THEN
        BTUTL=BTUTL + COEFF(17)*STADATA((CBTWY-MAX_IZONES),9)
        END IF
        IF(BIVT(9,JZ).GT.0.0) THEN
        BTUTL=BTUTL+KBTWYB
        END IF
	      elseif(wivt(9,jz).le.0.0) then
	      CBTWY=0
	      btutl=0.0
	      endif
C
C....................................................................
      IF(DEBUG) THEN
      X=0.0
      SC=CBTWY-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(CBTWY.GT.0) X=STADATA((CBTWY-MAX_IZONES),9)
      WRITE(26,9245) TIVT(9,JZ),BIVT(9,JZ),EIVT(9,JZ),WIVT(9,JZ),
     *               RIVT(9,JZ),
     *               FARE(9,JZ),TXFER(9,JZ),WAIT1(9,JZ),WAIT1A,WAIT1B,
     *               WAIT2(9,JZ),WALKTFR(9,JZ),WALKACC(9,JZ),
     *               WALKEGR(9,JZ),
     *               LUNRELMBK(JZ,7),LUNRELMBK(JZ,8),
     *               LUNRELMBK(JZ,9),LUNRELMBK(JZ,10),
     *               NUNRELMBK(JZ,7),NUNRELMBK(JZ,8),
     *               NUNRELMBK(JZ,9),NUNRELMBK(JZ,10),
     *               LCROWDMBK(JZ,7),LCROWDMBK(JZ,8),
     *               LCROWDMBK(JZ,9),LCROWDMBK(JZ,10),
     *               NCAPACMBK(JZ,7),NCAPACMBK(JZ,8),
     *               NCAPACMBK(JZ,9),NCAPACMBK(JZ,10),
     *               KBTWYB,CBTWY,STANAME(SC),
     *               X,UTLZBIK,BTUTL
 9245 FORMAT(//1X,'Bike --> Transitway Bus Level-Of-Service Data'/
     *       1X,  '----------------------------------'//
     *       1X,'TOTAL IVTT                =',F8.2/
     *       1X,'LOCAL IVTT                =',F8.2/
     *       1X,'EXPRESS IVTT              =',F8.2/
     *       1X,'TRANSITWAY IVTT           =',F8.2/
     *       1X,'RAPID BUS IVTT            =',F8.2/
     *       1X,'TRANSITWAY FARE           =',F8.2/
     *       1X,'TRANSITWAY TRANSFERS      =',I8/
     *       1X,'TRANSITWAY 1ST WAIT       =',F8.2/
     *       1X,'TRANSITWAY 1ST WAIT (<5)  =',F8.2/
     *       1X,'TRANSITWAY 1ST WAIT (>5)  =',F8.2/
     *       1X,'TRANSITWAY 2ND WAIT       =',F8.2/
     *       1X,'TRANSITWAY WALK TRANSFER  =',F8.2/
     *       1X,'TRANSITWAY BIKE ACCESS    =',F8.2/
     *       1X,'TRANSITWAY BIKE EGRESS    =',F8.2/
     *       1X,'LOCAL BUS   LINK UNREL    =',F8.2/
     *       1X,'RAPID BUS   LINK UNREL    =',F8.2/
     *       1X,'EXPRESS BUS LINK UNREL    =',F8.2/
     *       1X,'TRANSITWAY  LINK UNREL    =',F8.2/
     *       1X,'LOCAL BUS   NODE UNREL    =',F8.2/
     *       1X,'RAPID BUS   NODE UNREL    =',F8.2/
     *       1X,'EXPRESS BUS NODE UNREL    =',F8.2/
     *       1X,'TRANSITWAY  NODE UNREL    =',F8.2/
     *       1X,'LOCAL BUS   LINK CROWD    =',F8.2/
     *       1X,'RAPID BUS   LINK CROWD    =',F8.2/
     *       1X,'EXPRESS BUS LINK CROWD    =',F8.2/
     *       1X,'TRANSITWAY  LINK CROWD    =',F8.2/
     *       1X,'LOCAL BUS   NODE CAPAC    =',F8.2/
     *       1X,'RAPID BUS   NODE CAPAC    =',F8.2/
     *       1X,'EXPRESS BUS NODE CAPAC    =',F8.2/
     *       1X,'TRANSITWAY  NODE CAPAC    =',F8.2/
     *       1X,'TRANSITWAY BUS TXFER CNST =',F8.2/
     *       1X,'TRANSITWAY ACCESS STATION =',I8,1X,A37/
     *       1X,'TRANSITWAY ACCESS STA WALK=',F8.2/
     *       1X,'COMMON BIKE UTILITY       =',F8.4/
     *       1X,'MODE CHOICE MODEL UTILITY =',F10.5/)
      END IF
C---------------------------------------------------------------------
C            DRIVE --> TRANSITWAY BUS UTILITY COMPUTATION            |
C---------------------------------------------------------------------
      CSTAT=MAX_ZONES
      IMODE=4
	    CALL DRVNEW(JZ,DTUTL,TAB2DA,TAB1DA,CSTAT,CDSTAT,IMODE,
     *            STAZNE)
	    IT=CSTAT-MAX_IZONES
	    WALKDT=STAZNE(3,IT,JZ)
	    IS=CSTAT-MAX_IZONES
 	    DTUTL=DTUTL/(LSUM1TRN*LSUM2TW)
	    IF((STAZNEI(IS,JZ,4,1).GT.0).OR.(STAZNEI(IS,JZ,4,2).GT.0).OR.
     *   (STAZNEI(IS,JZ,4,3).GT.0)) THEN
      DTUTL= DTUTL + KBTWYD
      END IF
C---------------------------------------------------------------------
C            WALK --> EXPRESS BUS UTILITY COMPUTATION                |
C---------------------------------------------------------------------
C   COMPUTE MODE CHOICE MODEL UTILITY VALUE FOR WALK TO EXPRESS BUS
C
      WAIT1A=0.0
      WAIT1B=0.0
      if(eivt(2,jz).gt.0.0) then
      IF(LUNREL) LUNRELV(3)=LUNRELM(JZ,4)+LUNRELM(JZ,5)+LUNRELM(JZ,6)
      IF(NUNREL) NUNRELV(3)=NUNRELM(JZ,4)+NUNRELM(JZ,5)+NUNRELM(JZ,6)
      IF(LCROWD) LCROWDV(3)=LCROWDM(JZ,4)+LCROWDM(JZ,5)+LCROWDM(JZ,6)
      IF(NCAPAC) NCAPACV(3)=NCAPACM(JZ,4)+NCAPACM(JZ,5)+NCAPACM(JZ,6)
C...UTILITY COMPUTATIONS
	    WAIT1A=AMIN1(WAIT1(2,JZ),WAITLT)
      WAIT1B=DIM(WAIT1(2,JZ),WAITLT)
      WCUTL=COEFF(11)*TIVT(2,JZ) + COEFF(12)*WAIT1A +
     *        COEFF(13)*WAIT1B + COEFF(15)*WAIT2(2,JZ) +
     *        COEFF(17)*WALKTFR(2,JZ) +
     *        COEFF(37)*FLOAT(TXFER(2,JZ)) +
     *        COEFF(75)* LUNRELV(3) +
     *        COEFF(76)* NUNRELV(3) +
     *        COEFF(77)* LCROWDV(3) +
     *        COEFF(78)* NCAPACV(3) -
     *        (1.0-(EIVT(2,JZ)/TIVT(2,JZ)))*6.0
      WCUTL=WCUTL/(LSUM1TRN*LSUM2EB)
      WLKCACC=WALKACC(2,JZ)
      WLKCEGR=WALKEGR(2,JZ)
C...OBTAIN STATION NUMBER
      CWEXP=BUSTAT(2,JZ)
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
C     WRITE(75,5722) CWEXP
 5722 FORMAT(' MTAMC 5722 (F) - EXPRESS BUS STATION NOT FOUND',
     *       ' FOR NODE ',I6)
      WRITE(96,5747) CWEXP
C     STOP 5722
      CWEXP=0
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
     *               WALKEGR(2,JZ),KBEXPW,DENOM,
     *               LUNRELM(JZ,4),LUNRELM(JZ,5),LUNRELM(JZ,6),
     *               NUNRELM(JZ,4),NUNRELM(JZ,5),NUNRELM(JZ,6),
     *               LCROWDM(JZ,4),LCROWDM(JZ,5),LCROWDM(JZ,6),
     *               NCAPACM(JZ,4),NCAPACM(JZ,5),NCAPACM(JZ,6),
     *               CWEXP,X,WCUTL
 9044 FORMAT(//1X,'Walk --> Express Bus Level-Of-Service Data'/
     *       1X,  '----------------------------------'//
     *       1X,'TOTAL IVTT           =',F8.2/
     *       1X,'LOCAL IVTT           =',F8.2/
     *       1X,'EXPRESS IVTT         =',F8.2/
     *       1X,'RAPID BUS IVTT       =',F8.2/
     *       1X,'EXP   FARE           =',F8.2/
     *       1X,'EXP   TRANSFERS      =',I8/
     *       1X,'EXP   1ST WAIT       =',F8.2/
     *       1X,'EXP   1ST WAIT (<5)  =',F8.2/
     *       1X,'EXP   1ST WAIT (>5)  =',F8.2/
     *       1X,'EXP   2ND WAIT       =',F8.2/
     *       1X,'EXP   WALK TRANSFER  =',F8.2/
     *       1X,'EXP   WALK ACCESS    =',F8.2/
     *       1X,'EXP   WALK EGRESS    =',F8.2/
     *       1X,'EXP   BUS TXFER CNST =',F8.2/
     *       1X,'LCL/EXP IVT RATIO    =',F8.2/
     *       1X,'LCL   LINK UNREL     =',F8.2/
     *       1X,'RB    LINK UNREL     =',F8.2/
     *       1X,'EXP   LINK UNREL     =',F8.2/
     *       1X,'LCL   NODE UNREL     =',F8.2/
     *       1X,'RB    NODE UNREL     =',F8.2/
     *       1X,'EXP   NODE UNREL     =',F8.2/
     *       1X,'LCL   LINK CROWD     =',F8.2/
     *       1X,'RB    LINK CROWD     =',F8.2/
     *       1X,'EXP   LINK CROWD     =',F8.2/
     *       1X,'LCL   NODE CAPAC     =',F8.2/
     *       1X,'RB    NODE CAPAC     =',F8.2/
     *       1X,'EXP   NODE CAPAC     =',F8.2/
     *       1X,'EXP   ACCESS STATION =',I8/
     *       1X,'EXP   ACCESS STA WALK=',F8.2/
     *       1X,'MODE CHOICE MODEL UTILITY=',F10.5/)
      END IF
C---------------------------------------------------------------------
C            BIKE --> EXPRESS BUS UTILITY COMPUTATION                |
C---------------------------------------------------------------------
C   COMPUTE MODE CHOICE MODEL UTILITY VALUE FOR BIKE TO EXPRESS BUS
C
      WAIT1A=0.0
      WAIT1B=0.0
      if(eivt(8,jz).gt.0.0) then
      IF(LUNREL) LUNRELV(3)=LUNRELMBK(JZ,4)+LUNRELMBK(JZ,5)+
     *           LUNRELMBK(JZ,6)
      IF(NUNREL) NUNRELV(3)=NUNRELMBK(JZ,4)+NUNRELMBK(JZ,5)+
     *           NUNRELMBK(JZ,6)
      IF(LCROWD) LCROWDV(3)=LCROWDMBK(JZ,4)+LCROWDMBK(JZ,5)+
     *           LCROWDMBK(JZ,6)
      IF(NCAPAC) NCAPACV(3)=NCAPACMBK(JZ,4)+NCAPACMBK(JZ,5)+
     *           NCAPACMBK(JZ,6)
C...UTILITY COMPUTATIONS
	    WAIT1A=AMIN1(WAIT1(8,JZ),WAITLT)
      WAIT1B=DIM(WAIT1(8,JZ),WAITLT)
      BCUTL=COEFF(11)*TIVT(8,JZ) + COEFF(12)*WAIT1A +
     *        COEFF(13)*WAIT1B + COEFF(15)*WAIT2(8,JZ) +
     *        COEFF(17)*WALKTFR(8,JZ) + UTLZBIK +
     *        COEFF(38)*FLOAT(TXFER(8,JZ)) +
     *        COEFF(75)* LUNRELV(3) +
     *        COEFF(76)* NUNRELV(3) +
     *        COEFF(77)* LCROWDV(3) +
     *        COEFF(78)* NCAPACV(3) -
     *        (1.0-(EIVT(8,JZ)/TIVT(8,JZ)))*6.0
      BCUTL=BCUTL/(LSUM1TRN*LSUM2EB)
      BIKCACC=WALKACC(8,JZ)
      BIKCEGR=WALKEGR(8,JZ) 
C...OBTAIN STATION NUMBER
      CBEXP=BUSTAT(4,JZ)
      IF(CBEXP.GT.0) THEN
      DO 5735 K=1,MAX_STATIONS
        IF(STANODE1(K).EQ.CBEXP) THEN
        L=K+MAX_IZONES
        IF(STANUM(K).EQ.3) THEN
        CBEXP=L
        GO TO 5736
        END IF
        END IF
        IF(STANODE2(K).EQ.CBEXP) THEN
        L=K+MAX_IZONES
        IF(STANUM(K).EQ.3) THEN
        CBEXP=L
        GO TO 5736
        END IF
        END IF 
 5735 CONTINUE
      WRITE(26,5737) CBEXP
 5737 FORMAT(' WARNING - EXPRESS BUS BIKE STATION NOT',
     *       ' FOUND FOR NODE ',I6)
      END IF
 5736 CONTINUE
        IF(CBEXP.GT.0) THEN
        BCUTL=BCUTL + COEFF(17)*STADATA((CBEXP-MAX_IZONES),9)
        END IF
        IF(BIVT(8,JZ).GT.0.0) THEN
        BCUTL=BCUTL+KBEXPB
        END IF
	      elseif(eivt(8,jz).le.0.0) then
	      CBEXP=0
	      bcutl=0.0
	      endif
        if(eivt(8,jz).gt.0) denom=bivt(8,jz)/eivt(8,jz)
 	      if(bustexp.and.(denom.gt.0.5)) wcutl=0.0
 	      if(bustexp.and.(txfer(8,jz).gt.1)) wcutl=0.0
C
C....................................................................
      IF(DEBUG) THEN
      X=0.0
      IF(CBEXP.GT.0) X=STADATA((CBEXP-MAX_IZONES),9)
      WRITE(26,9144) TIVT(8,JZ),BIVT(8,JZ),EIVT(8,JZ),RIVT(8,JZ),
     *               FARE(8,JZ),TXFER(8,JZ),WAIT1(8,JZ),WAIT1A,WAIT1B,
     *               WAIT2(8,JZ),WALKTFR(8,JZ),WALKACC(8,JZ),
     *               WALKEGR(8,JZ),KBEXPB,DENOM,
     *               LUNRELMBK(JZ,4),LUNRELMBK(JZ,5),LUNRELMBK(JZ,6),
     *               NUNRELMBK(JZ,4),NUNRELMBK(JZ,5),NUNRELMBK(JZ,6),
     *               LCROWDMBK(JZ,4),LCROWDMBK(JZ,5),LCROWDMBK(JZ,6),
     *               NCAPACMBK(JZ,4),NCAPACMBK(JZ,5),NCAPACMBK(JZ,6),
     *               CBEXP,X,UTLZBIK,BCUTL
 9144 FORMAT(//1X,'Bike --> Express Bus Level-Of-Service Data'/
     *       1X,  '----------------------------------'//
     *       1X,'TOTAL IVTT           =',F8.2/
     *       1X,'LOCAL IVTT           =',F8.2/
     *       1X,'EXPRESS IVTT         =',F8.2/
     *       1X,'RAPID BUS IVTT       =',F8.2/
     *       1X,'EXP   FARE           =',F8.2/
     *       1X,'EXP   TRANSFERS      =',I8/
     *       1X,'EXP   1ST WAIT       =',F8.2/
     *       1X,'EXP   1ST WAIT (<5)  =',F8.2/
     *       1X,'EXP   1ST WAIT (>5)  =',F8.2/
     *       1X,'EXP   2ND WAIT       =',F8.2/
     *       1X,'EXP   WALK TRANSFER  =',F8.2/
     *       1X,'EXP   BIKE ACCESS    =',F8.2/
     *       1X,'EXP   BIKE EGRESS    =',F8.2/
     *       1X,'EXP   BUS TXFER CNST =',F8.2/
     *       1X,'LCL/EXP IVT RATIO    =',F8.2/
     *       1X,'LCL   LINK UNREL     =',F8.2/
     *       1X,'RB    LINK UNREL     =',F8.2/
     *       1X,'EXP   LINK UNREL     =',F8.2/
     *       1X,'LCL   NODE UNREL     =',F8.2/
     *       1X,'RB    NODE UNREL     =',F8.2/
     *       1X,'EXP   NODE UNREL     =',F8.2/
     *       1X,'LCL   LINK CROWD     =',F8.2/
     *       1X,'RB    LINK CROWD     =',F8.2/
     *       1X,'EXP   LINK CROWD     =',F8.2/
     *       1X,'LCL   NODE CAPAC     =',F8.2/
     *       1X,'RB    NODE CAPAC     =',F8.2/
     *       1X,'EXP   NODE CAPAC     =',F8.2/
     *       1X,'EXP   ACCESS STATION =',I8/
     *       1X,'EXP   ACCESS STA WALK=',F8.2/
     *       1X,'COMMON BIKE UTILITY  =',F8.4/
     *       1X,'MODE CHOICE MODEL UTILITY=',F10.5/)
      END IF
C---------------------------------------------------------------------
C---------------------------------------------------------------------
C            DRIVE --> EXPRESS BUS UTILITY COMPUTATION                
C---------------------------------------------------------------------
      IMODE=3
	    CALL DRVNEW(JZ,DCUTL,TAB2DA,TAB1DA,CSTAE,CDSTAE,IMODE,
     *            STAZNE)
	    IT=CSTAE-MAX_IZONES
	    WALKD=STAZNE(3,IT,JZ)
	    IS=CSTAE-MAX_IZONES
	    DCUTL=DCUTL/(LSUM1TRN*LSUM2EB)
      IF((STAZNEI(IS,JZ,3,1).GT.0).OR.(STAZNEI(IS,JZ,3,2).GT.0)) THEN
      DCUTL=DCUTL + KBEXPD
      END IF
C
C       COMPUTE THE COMMON EXPONENTIATED UTILITY FOR WALK
C
      IF(NMOT) THEN
       IF(WALKTIME) THEN
       WALKT=TAB2DA(JZ)
       WALK1=AMIN1(WALKT,MWALKT)
       WALK2=DIM(WALKT,MWALKT)
       UTILWK= MWALK1*(60.0/WALKSPD)*WALK1
     *            + MWALK2*(60.0/WALKSPD)*WALK2
       ELSE
       UTILWK=WLKLSM(JZ)
       END IF
C
C       COMPUTE THE COMMON EXPONENTIATED UTILITY FOR BICYCLE
C
       IF(BICYCLE) THEN
       UTILBK=BZCOEF(1)*BYCLSM(JZ) + BZCOEF(2)*TAB2DA(JZ) + UTLZBIK
       ELSE
       BIKET=TAB2DA(JZ)
       BIKE1=AMIN1(BIKET,MWALKT)
       BIKE2=DIM(BIKET,MWALKT)
       UTILBK=MBIKE1*(60.0/BIKESPD)*BIKE1
     *            +MBIKE2*(60.0/BIKESPD)*BIKE2
       END IF
C
C       COMPUTE THE COMMON EXPONENTIATED UTILITY FOR E-SCOOTER
C
      UTILSC=0.0
      IF(ESCOOTER) THEN
       IF(TAB2DA(JZ).LT.MAX_SCOOTER) THEN
       SCOTTIME=TAB2DA(JZ)*(60.0/SCOTSPD)
       SCOTCOST=100.0+15.0*SCOTTIME
       BIKE1=AMIN1(TAB2DA(JZ),MWALKT)
       BIKE2=DIM(TAB2DA(JZ),MWALKT)
       UTILSC=MBIKE1*(60.0/SCOTSPD)*BIKE1
     *            +MBIKE2*(60.0/SCOTSPD)*BIKE2
       IF(ZHHD(30,IZ).LT.1.0) UTILSC=0.0
       END IF
      END IF
      END IF
C -------------------------------------------------------------------------------
C
C.................................................................
      IF(DEBUG) THEN
      IF(BICYCLE) THEN
      WRITE(26,9026) IZ,JZ,UTILWK,BYCLSM(JZ),TAB2DA(JZ),
     *               UTLZBIK,UTILBK
 9026 FORMAT(/1X,'NON-MOTORIZED MODE UTILITY COMPUTATIONS'/
     *       1X,'----------------------------------------'/
     *       1X,'PRODUCTION ZONE            =',I10/
     *       1X,'ATTRACTION ZONE            =',I10/
     *       1X,'WALK   MODE UTILITY        =',F10.5//
     *       1X,'BIKE   LOGSUM              =',F10.5/
     *       1X,'INTERCHANGE DISTANCE       =',F10.2/
     *       1X,'COMMON BIKE UTILITY        =',F10.5/
     *       1X,'BIKE   MODE UTILITY        =',F10.5//)
      ELSE
      WRITE(26,9126) IZ,JZ,
     *               WALKT,WALK1,WALK2,BIKET,BIKE1,BIKE2,
     *               UTILWK,UTILBK
 9126 FORMAT(/1X,'NON-MOTORIZED MODE UTILITY COMPUTATIONS'/
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
      IF(ESCOOTER) THEN
      WRITE(26,9127) SCOTTIME,SCOTCOST,TAB2DA(JZ),MAX_SCOOTER,
     *               SCOTSPD,UTILSC
 9127 FORMAT(1X,' E-SCOOTER MODE UTILITY COMPUTATIONS'/
     *       1X,' -----------------------------------'/
     *       1X,' E-SCOOTER TIME            =',F10.2/
     *       1X,' E-SCOOTER COST            =',F10.2/
     *       1X,' TOTAL DISTANCE            =',F10.2/
     *       1X,' MAXIMUM ALLOWABLE DIST    =',F10.2/
     *       1X,' E-SCOOTER SPEED           =',F10.2/
     *       1X,' E-SCOOTER UTILITY         =',F10.5//)
      END IF
      END IF
C.....................................................................
C----------------------------------------------------------------------
C ADJUST PARK&RIDE UTILITY VALUES TO INCLUDE SHADOW PRICE
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
C  DRIVE TO TRANSITWAY BUS
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
	    END IF
C.....................................................................
      IF(DEBUG) THEN
      DO K=1,5
      IF(K.EQ.3.OR.K.EQ.4) CYCLE
      WRITE(26,848) NAME(K),
     *              (CHWIND(K,K1),K1=1,2),
     *              (CHBKIND(K,K2),K2=1,2),
     *              (CHBIND(K,K3),K3=1,2),
     *              (CHPIND(K,K4),K4=1,4),
     *              (CHKIND(K,K5),K5=1,4)
  848 FORMAT(' SUMMARY OF COMPLEX PATH SELECTIONS FOR ',A13/
     *       ' -----------------------------------------------'/
     *       ' WALK ACCESS=',L1,2X,L1/
     *       ' BIKE ACCESS=',L1,2X,L1/
     *       ' BUS  ACCESS=',L1,2X,L1/
     *       ' PNR  ACCESS=',L1,2X,L1,2X,L1,2X,L1/
     *       ' KNR  ACCESS=',L1,2X,L1,2X,L1,2X,L1/)
      WRITE(26,849) (WDSTA((K+5),K1),K1=1,2),
     *              (BIKDSTA((K+5),K2),K2=1,2),
     *              (BDSTA((K+5),K3),K3=1,2),
     *              (PDSTA((K+5),K4),K4=1,4),
     *              (KDSTA((K+5),K5),K5=1,4)
  849 FORMAT(' SUMMARY OF DESTINATION STATION SELECTIONS'/
     *       ' -----------------------------------------'/
     *       ' WALK ACCESS=',I4,2X,I4/
     *       ' BIKE ACCESS=',I4,2X,I4/
     *       ' BUS  ACCESS=',I4,2X,I4/
     *       ' PNR  ACCESS=',I4,2X,I4,2X,I4,2X,I4/
     *       ' KNR  ACCESS=',I4,2X,I4,2X,I4,2X,I4/)
      END DO
      END IF
C................................................................
C
C       UPDATE COMMUTER RAIL AND URBAN RAIL UTILITIES
C       BASED UPON BLENDED PATH RESULTS
C
      DO IMODE=1,5
      IF(IMODE.EQ.3.OR.IMODE.EQ.4) CYCLE
       DO ISTA=1,2
       IF(CHWIND(IMODE,ISTA)) THEN
       WUTIL(IMODE,ISTA)=WUTIL((IMODE+5),ISTA)
       ELSE
       IF(IMODE.EQ.5.AND.WUTIL(IMODE,ISTA).NE.0.0) 
     *     WUTIL(IMODE,ISTA)=WUTIL(IMODE,ISTA)+0.0
       END IF
       IF(CHBKIND(IMODE,ISTA)) 
     *    BIKUTIL(IMODE,ISTA)=BIKUTIL((IMODE+5),ISTA)
       IF(CHBIND(IMODE,ISTA)) BUTIL(IMODE,ISTA)=BUTIL((IMODE+5),ISTA)
       END DO
       DO ISTA=1,4
       IF(CHPIND(IMODE,ISTA)) PUTIL(IMODE,ISTA)=PUTIL((IMODE+5),ISTA)
       IF(CHKIND(IMODE,ISTA)) KUTIL(IMODE,ISTA)=KUTIL((IMODE+5),ISTA)
       END DO
      END DO       
C
C---------------------------------------------------------------------
C        UPPER LEVEL UTILITIES & PROBABLILITY COMPUTATIONS           |
C---------------------------------------------------------------------
C
C
C WALK UTILITY FOR WALK AND BIKE ACCESS TO COMMUTER/URBAN RAIL
C
      MWALKW(1,1)=WDIST(1,1)     
      MWALKW(1,2)=WDIST(1,2)     
      MWALKW(2,1)=WDIST(2,1)     
      MWALKW(2,2)=WDIST(2,2)   
      MWALKW(5,1)=WDIST(5,1)     
      MWALKW(5,2)=WDIST(5,2)     
      MWALKBIK(1,1)=BIKDIST(1,1)     
      MWALKBIK(1,2)=BIKDIST(1,2)     
      MWALKBIK(2,1)=BIKDIST(2,1)     
      MWALKBIK(2,2)=BIKDIST(2,2) 
      MWALKBIK(5,1)=BIKDIST(5,1)     
      MWALKBIK(5,2)=BIKDIST(5,2)               
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9000) IZ,JZ,WUTIL(1,1),WUTIL(1,2),
     *               BIKUTIL(1,1),BIKUTIL(1,2),
     *               BUTIL(1,1),BUTIL(1,2),(PUTIL(1,K1),K1=1,4),
     *               (KUTIL(1,K2),K2=1,4),(UBERUTIL(1,K5),K5=1,4),
     *               WUTIL(2,1),WUTIL(2,2),
     *               BIKUTIL(2,1),BIKUTIL(2,2),
     *               BUTIL(2,1),BUTIL(2,2),(PUTIL(2,K3),K3=1,4),
     *               (UBERUTIL(2,K5),K5=1,4),(KUTIL(2,K4),K4=1,4),
     *               WUTIL(5,1),WUTIL(5,2),
     *               BIKUTIL(5,1),BIKUTIL(5,2),
     *               BUTIL(5,1),BUTIL(5,2),(PUTIL(5,K3),K3=1,4),
     *               (KUTIL(5,K4),K4=1,4),(UBERUTIL(5,K5),K5=1,4),
     *               WBUTL,BBUTL,WRUTL,BRUTL,WCUTL,BCUTL,DCUTL,
     *               WTUTL,BTUTL,DTUTL,UTILWK,UTILBK,UTILSC
 9000 FORMAT(/1X,'SUMMARY OF LOWER LEVEL UTILITY VALUES'/
     *       1X,'---------------------------------------'/
     *       1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10//
     *       1X,'COMMUTER RAIL  WALK-->STA #1 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  WALK-->STA #2 UTILITY=',F10.5//
     *       1X,'COMMUTER RAIL  BIKE-->STA #1 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  BIKE-->STA #2 UTILITY=',F10.5//
     *       1X,'COMMUTER RAIL  BUS -->STA #1 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  BUS -->STA #2 UTILITY=',F10.5//
     *       1X,'COMMUTER RAIL  P&R -->STA #1 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  P&R -->STA #2 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  P&R -->STA #3 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  P&R -->STA #4 UTILITY=',F10.5//
     *       1X,'COMMUTER RAIL  K&R -->STA #1 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  K&R -->STA #2 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  K&R -->STA #3 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  K&R -->STA #4 UTILITY=',F10.5//
     *       1X,'COMMUTER RAIL  UBER-->STA #1 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  UBER-->STA #2 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  UBER-->STA #3 UTILITY=',F10.5/
     *       1X,'COMMUTER RAIL  UBER-->STA #4 UTILITY=',F10.5///
     *       1X,'URBAN    RAIL  WALK-->STA #1 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  WALK-->STA #2 UTILITY=',F10.5//
     *       1X,'URBAN    RAIL  BIKE-->STA #1 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  BIKE-->STA #2 UTILITY=',F10.5//
     *       1X,'URBAN    RAIL  BUS -->STA #1 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  BUS -->STA #2 UTILITY=',F10.5//
     *       1X,'URBAN    RAIL  P&R -->STA #1 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  P&R -->STA #2 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  P&R -->STA #3 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  P&R -->STA #4 UTILITY=',F10.5//
     *       1X,'URBAN    RAIL  K&R -->STA #1 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  K&R -->STA #2 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  K&R -->STA #3 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  K&R -->STA #4 UTILITY=',F10.5//
     *       1X,'URBAN    RAIL  UBER-->STA #1 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  UBER-->STA #2 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  UBER-->STA #3 UTILITY=',F10.5/
     *       1X,'URBAN    RAIL  UBER-->STA #4 UTILITY=',F10.5///
     *       1X,'BUS RAPID TRN  WALK-->STA #1 UTILITY=',F10.5/
     *       1X,'BUS RAPID TRN  WALK-->STA #2 UTILITY=',F10.5//
     *       1X,'BUS RAPID TRN  BIKE-->STA #1 UTILITY=',F10.5/
     *       1X,'BUS RAPID TRN  BIKE-->STA #2 UTILITY=',F10.5//
     *       1X,'BUS RAPID TRN  BUS -->STA #1 UTILITY=',F10.5/
     *       1X,'BUS RAPID TRN  BUS -->STA #2 UTILITY=',F10.5//
     *       1X,'BUS RAPID TRN  P&R -->STA #1 UTILITY=',F10.5/
     *       1X,'BUS RAPID TRN  P&R -->STA #2 UTILITY=',F10.5/
     *       1X,'BUS RAPID TRN  P&R -->STA #3 UTILITY=',F10.5/
     *       1X,'BUS RAPID TRN  P&R -->STA #4 UTILITY=',F10.5//
     *       1X,'BUS RAPID TRN  K&R -->STA #1 UTILITY=',F10.5/
     *       1X,'BUS RAPID TRN  K&R -->STA #2 UTILITY=',F10.5/
     *       1X,'BUS RAPID TRN  K&R -->STA #3 UTILITY=',F10.5/
     *       1X,'BUS RAPID TRN  K&R -->STA #4 UTILITY=',F10.5//
     *       1X,'BUS RAPID TRN  UBER-->STA #1 UTILITY=',F10.5/
     *       1X,'BUS RAPID TRN  UBER-->STA #2 UTILITY=',F10.5/
     *       1X,'BUS RAPID TRN  UBER-->STA #3 UTILITY=',F10.5/
     *       1X,'BUS RAPID TRN  UBER-->STA #4 UTILITY=',F10.5///
     *       1X,'WALK     -> LOCAL   BUS      UTILITY=',F10.5/
     *       1X,'BIKE     -> LOCAL   BUS      UTILITY=',F10.5/
     *       1X,'WALK     -> RAPID   BUS      UTILITY=',F10.5/
     *       1X,'BIKE     -> RAPID   BUS      UTILITY=',F10.5/
     *       1X,'WALK     -> EXPRESS BUS      UTILITY=',F10.5/
     *       1X,'BIKE     -> EXPRESS BUS      UTILITY=',F10.5/
     *       1X,'DRIVE    -> EXPRESS BUS      UTILITY=',F10.5//
     *       1X,'WALK     -> TRANSITWAY       UTILITY=',F10.5/
     *       1X,'BIKE     -> TRANSITWAY       UTILITY=',F10.5/
     *       1X,'DRIVE    -> TRANSITWAY       UTILITY=',F10.5//
     *       1X,'WALK MODE                    UTILITY=',F10.5/
     *       1X,'BIKE MODE                    UTILITY=',F10.5/
     *       1X,'E-SCOOTER MODE               UTILITY=',F10.5/)
C
      WRITE(26,9112) MWALKW(1,1),MWALKW(1,2),
     *               MWALKBIK(1,1),MWALKBIK(1,2),MWALKB(1,1),
     *               MWALKB(1,2),MWALKW(2,1),MWALKW(2,2),
     *               MWALKBIK(2,1),MWALKBIK(2,2),
     *               MWALKB(2,1),MWALKB(2,2),
     *               MWALKW(5,1),MWALKW(5,2),
     *               MWALKBIK(5,1),MWALKBIK(5,2),
     *               MWALKB(5,1),MWALKB(5,2),
     *               WLKBACC,BIKBACC,
     *               WLKRACC,BIKRACC,
     *               WLKCACC,BIKCACC,
     *               WLKWTACC,BIKWTACC,
     *               WALKD,WALKDT,WALKDBRT
 9112 FORMAT(//1X,'ORIGIN WALK TIMES'/
     *       1X,'-----------------'/
     *       1X,'COMMUTER RAIL WALK-->STA #1 TIME/LSUM=',F10.2/
     *       1X,'COMMUTER RAIL WALK-->STA #2 TIME/LSUM=',F10.2/
     *       1X,'COMMUTER RAIL BIKE-->STA #1 LOGSUM   =',F10.2/
     *       1X,'COMMUTER RAIL BIKE-->STA #2 LOGSUM   =',F10.2/
     *       1X,'COMMUTER RAIL BUS -->STA #1 WALK TIME=',F10.2/
     *       1X,'COMMUTER RAIL BUS -->STA #2 WALK TIME=',F10.2//
     *       1X,'URBAN    RAIL WALK-->STA #1 TIME/LSUM=',F10.2/
     *       1X,'URBAN    RAIL WALK-->STA #2 TIME/LSUM=',F10.2/
     *       1X,'URBAN    RAIL BIKE-->STA #1 LOGSUM   =',F10.2/
     *       1X,'URBAN    RAIL BIKE-->STA #2 LOGSUM   =',F10.2/
     *       1X,'URBAN    RAIL BUS -->STA #1 WALK TIME=',F10.2/
     *       1X,'URBAN    RAIL BUS -->STA #2 WALK TIME=',F10.2//
     *       1X,'BUS RAPID TRN WALK-->STA #1 TIME/LSUM=',F10.2/
     *       1X,'BUS RAPID TRN WALK-->STA #2 TIME/LSUM=',F10.2/
     *       1X,'BUS RAPID TRN BIKE-->STA #1 LOGSUM   =',F10.2/
     *       1X,'BUS RAPID TRN BIKE-->STA #2 LOGSUM   =',F10.2/
     *       1X,'BUS RAPID TRN BUS -->STA #1 WALK TIME=',F10.2/
     *       1X,'BUS RAPID TRN BUS -->STA #2 WALK TIME=',F10.2//
     *       1X,'WALK     -> LOCAL BUS       WALK TIME=',F10.2/
     *       1X,'BIKE     -> LOCAL BUS       BIKE TIME=',F10.2//
     *       1X,'WALK     -> RAPID BUS       WALK TIME=',F10.2/
     *       1X,'BIKE     -> RAPID BUS       BIKE TIME=',F10.2//
     *       1X,'WALK     -> EXPRESS BUS     WALK TIME=',F10.2/
     *       1X,'BIKE     -> EXPRESS BUS     BIKE TIME=',F10.2//
     *       1X,'WALK     -> TRANSITWAY BUS  WALK TIME=',F10.2/
     *       1X,'BIKE     -> TRANSITWAY BUS  BIKE TIME=',F10.2//
     *       1X,'DESTINATION WALK TIMES'/
     *       1X,'----------------------'/   
     *       1X,'DRIVE    -> EXPRESS BUS     WALK TIME=',F10.2/
     *       1X,'DRIVE    -> TRANSITWAY BUS  WALK TIME=',F10.2/
     *       1X,'DRIVE    -> BUS RAPID TRN   WALK TIME=',F10.2/)
C
      WRITE(26,9003) (OSTA(1,K),ASTA(1,K),K=1,14)
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
     *       1X,'COMMUTER RAIL  K&R -->STA #4 ',I4,'-->',I4//
     *       1X,'COMMUTER RAIL  BIKE-->STA #1 ',I4,'-->',I4/
     *       1X,'COMMUTER RAIL  BIKE-->STA #2 ',I4,'-->',I4)
      WRITE(26,9103) OSTA(1,1),WDSTA(6,1),CHWIND(1,1),
     *               OSTA(1,2),WDSTA(6,2),CHWIND(1,2),
     *               OSTA(1,3),BDSTA(6,1),CHBIND(1,1),
     *               OSTA(1,4),BDSTA(6,2),CHBIND(1,2),
     *               OSTA(1,5),PDSTA(6,1),CHPIND(1,1),
     *               OSTA(1,6),PDSTA(6,2),CHPIND(1,2),
     *               OSTA(1,7),PDSTA(6,3),CHPIND(1,3),
     *               OSTA(1,8),PDSTA(6,4),CHPIND(1,4),
     *               OSTA(1,9),KDSTA(6,1),CHKIND(1,1),
     *               OSTA(1,10),KDSTA(6,2),CHKIND(1,2),
     *               OSTA(1,11),KDSTA(6,3),CHKIND(1,3),
     *               OSTA(1,12),KDSTA(6,4),CHKIND(1,4),
     *               OSTA(1,13),BIKDSTA(6,1),CHBKIND(1,1),
     *               OSTA(1,14),BIKDSTA(6,2),CHBKIND(1,2)     
 9103 FORMAT(//10X,' BLENDED PATH RESULTS'/
     *       1X,'ORIGIN --> DESTINATION STATION SUMMARY'/
     *       1X,'--------------------------------------'/
     *    1X,'COMMUTER RAIL  WALK-->STA #1 ',I4,'-->',I4,'(',L1,')'/
     *    1X,'COMMUTER RAIL  WALK-->STA #2 ',I4,'-->',I4,'(',L1,')'//
     *    1X,'COMMUTER RAIL  BUS -->STA #1 ',I4,'-->',I4,'(',L1,')'/
     *    1X,'COMMUTER RAIL  BUS -->STA #2 ',I4,'-->',I4,'(',L1,')'//
     *    1X,'COMMUTER RAIL  P&R -->STA #1 ',I4,'-->',I4,'(',L1,')'/
     *    1X,'COMMUTER RAIL  P&R -->STA #2 ',I4,'-->',I4,'(',L1,')'/
     *    1X,'COMMUTER RAIL  P&R -->STA #3 ',I4,'-->',I4,'(',L1,')'/
     *    1X,'COMMUTER RAIL  P&R -->STA #4 ',I4,'-->',I4,'(',L1,')'//
     *    1X,'COMMUTER RAIL  K&R -->STA #1 ',I4,'-->',I4,'(',L1,')'/
     *    1X,'COMMUTER RAIL  K&R -->STA #2 ',I4,'-->',I4,'(',L1,')'/
     *    1X,'COMMUTER RAIL  K&R -->STA #3 ',I4,'-->',I4,'(',L1,')'/
     *    1X,'COMMUTER RAIL  K&R -->STA #4 ',I4,'-->',I4,'(',L1,')'//
     *    1X,'COMMUTER RAIL  BIKE-->STA #1 ',I4,'-->',I4,'(',L1,')'/
     *    1X,'COMMUTER RAIL  BIKE-->STA #2 ',I4,'-->',I4,'(',L1,')')
C  
      WRITE(26,9004) (OSTA(2,K),ASTA(2,K),K=1,14)
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
     *       1X,'URBAN RAIL     K&R -->STA #4 ',I4,'-->',I4//
     *       1X,'URBAN RAIL     BIKE-->STA #1 ',I4,'-->',I4/
     *       1X,'URBAN RAIL     BIKE-->STA #2 ',I4,'-->',I4)
      WRITE(26,9104) OSTA(2,1),WDSTA(7,1),CHWIND(2,1),
     *               OSTA(2,2),WDSTA(7,2),CHWIND(2,2),
     *               OSTA(2,3),BDSTA(7,1),CHBIND(2,1),
     *               OSTA(2,4),BDSTA(7,2),CHBIND(2,2),
     *               OSTA(2,5),PDSTA(7,1),CHPIND(2,1),
     *               OSTA(2,6),PDSTA(7,2),CHPIND(2,2),
     *               OSTA(2,7),PDSTA(7,3),CHPIND(2,3),
     *               OSTA(2,8),PDSTA(7,4),CHPIND(2,4),
     *               OSTA(2,9),KDSTA(7,1),CHKIND(2,1),
     *               OSTA(2,10),KDSTA(7,2),CHKIND(2,2),
     *               OSTA(2,11),KDSTA(7,3),CHKIND(2,3),
     *               OSTA(2,12),KDSTA(7,4),CHKIND(2,4),
     *               OSTA(2,13),BIKDSTA(7,1),CHBKIND(2,1),
     *               OSTA(2,14),BIKDSTA(7,2),CHBKIND(2,2) 
 9104 FORMAT(//10X,' BLENDED PATH RESULTS'/
     *   1X,'ORIGIN --> DESTINATION STATION SUMMARY'/
     *       1X,'--------------------------------------'/
     *   1X,'URBAN RAIL     WALK-->STA #1 ',I4,'-->',I4,'(',L1,')'/
     *   1X,'URBAN RAIL     WALK-->STA #2 ',I4,'-->',I4,'(',L1,')'//
     *   1X,'URBAN RAIL     BUS -->STA #1 ',I4,'-->',I4,'(',L1,')'/
     *   1X,'URBAN RAIL     BUS -->STA #2 ',I4,'-->',I4,'(',L1,')'//
     *   1X,'URBAN RAIL     P&R -->STA #1 ',I4,'-->',I4,'(',L1,')'/
     *   1X,'URBAN RAIL     P&R -->STA #2 ',I4,'-->',I4,'(',L1,')'/
     *   1X,'URBAN RAIL     P&R -->STA #3 ',I4,'-->',I4,'(',L1,')'/
     *   1X,'URBAN RAIL     P&R -->STA #4 ',I4,'-->',I4,'(',L1,')'//
     *   1X,'URBAN RAIL     K&R -->STA #1 ',I4,'-->',I4,'(',L1,')'/
     *   1X,'URBAN RAIL     K&R -->STA #2 ',I4,'-->',I4,'(',L1,')'/
     *   1X,'URBAN RAIL     K&R -->STA #3 ',I4,'-->',I4,'(',L1,')'/
     *   1X,'URBAN RAIL     K&R -->STA #4 ',I4,'-->',I4,'(',L1,')'//
     *   1X,'URBAN RAIL     BIKE-->STA #1 ',I4,'-->',I4,'(',L1,')'/
     *   1X,'URBAN RAIL     BIKE-->STA #2 ',I4,'-->',I4,'(',L1,')')
      WRITE(26,9105) (OSTA(5,K),ASTA(5,K),K=1,14)
 9105 FORMAT(//1X,'ORIGIN --> DESTINATION STATION SUMMARY'/
     *       1X,'--------------------------------------'/
     *       1X,'BUS RAPID TRN  WALK-->STA #1 ',I4,'-->',I4/
     *       1X,'BUS RAPID TRN  WALK-->STA #2 ',I4,'-->',I4//
     *       1X,'BUS RAPID TRN  BUS -->STA #1 ',I4,'-->',I4/
     *       1X,'BUS RAPID TRN  BUS -->STA #2 ',I4,'-->',I4//
     *       1X,'BUS RAPID TRN  P&R -->STA #1 ',I4,'-->',I4/
     *       1X,'BUS RAPID TRN  P&R -->STA #2 ',I4,'-->',I4/
     *       1X,'BUS RAPID TRN  P&R -->STA #3 ',I4,'-->',I4/
     *       1X,'BUS RAPID TRN  P&R -->STA #4 ',I4,'-->',I4//
     *       1X,'BUS RAPID TRN  K&R -->STA #1 ',I4,'-->',I4/
     *       1X,'BUS RAPID TRN  K&R -->STA #2 ',I4,'-->',I4/
     *       1X,'BUS RAPID TRN  K&R -->STA #3 ',I4,'-->',I4/
     *       1X,'BUS RAPID TRN  K&R -->STA #4 ',I4,'-->',I4//
     *       1X,'BUS RAPID TRN  BIKE-->STA #1 ',I4,'-->',I4/
     *       1X,'BUS RAPID TRN  BIKE-->STA #2 ',I4,'-->',I4)
      WRITE(26,9106) OSTA(5,1),WDSTA(10,1),CHWIND(5,1),
     *               OSTA(5,2),WDSTA(10,2),CHWIND(5,2),
     *               OSTA(5,3),BDSTA(10,1),CHBIND(5,1),
     *               OSTA(5,4),BDSTA(10,2),CHBIND(5,2),
     *               OSTA(5,5),PDSTA(10,1),CHPIND(5,1),
     *               OSTA(5,6),PDSTA(10,2),CHPIND(5,2),
     *               OSTA(5,7),PDSTA(10,3),CHPIND(5,3),
     *               OSTA(5,8),PDSTA(10,4),CHPIND(5,4),
     *               OSTA(5,9),KDSTA(10,1),CHKIND(5,1),
     *               OSTA(5,10),KDSTA(10,2),CHKIND(5,2),
     *               OSTA(5,11),KDSTA(10,3),CHKIND(5,3),
     *               OSTA(5,12),KDSTA(10,4),CHKIND(5,4),
     *               OSTA(5,13),BIKDSTA(10,1),CHBKIND(5,1),
     *               OSTA(5,14),BIKDSTA(10,2),CHBKIND(5,2) 
 9106 FORMAT(//10X,' BLENDED PATH RESULTS'/
     *   1X,'ORIGIN --> DESTINATION STATION SUMMARY'/
     *       1X,'--------------------------------------'/
     *   1X,'BUS RAPID TRN  WALK-->STA #1 ',I4,'-->',I4,'(',L1,')'/
     *   1X,'BUS RAPID TRN  WALK-->STA #2 ',I4,'-->',I4,'(',L1,')'//
     *   1X,'BUS RAPID TRN  BUS -->STA #1 ',I4,'-->',I4,'(',L1,')'/
     *   1X,'BUS RAPID TRN  BUS -->STA #2 ',I4,'-->',I4,'(',L1,')'//
     *   1X,'BUS RAPID TRN  P&R -->STA #1 ',I4,'-->',I4,'(',L1,')'/
     *   1X,'BUS RAPID TRN  P&R -->STA #2 ',I4,'-->',I4,'(',L1,')'/
     *   1X,'BUS RAPID TRN  P&R -->STA #3 ',I4,'-->',I4,'(',L1,')'/
     *   1X,'BUS RAPID TRN  P&R -->STA #4 ',I4,'-->',I4,'(',L1,')'//
     *   1X,'BUS RAPID TRN  K&R -->STA #1 ',I4,'-->',I4,'(',L1,')'/
     *   1X,'BUS RAPID TRN  K&R -->STA #2 ',I4,'-->',I4,'(',L1,')'/
     *   1X,'BUS RAPID TRN  K&R -->STA #3 ',I4,'-->',I4,'(',L1,')'/
     *   1X,'BUS RAPID TRN  K&R -->STA #4 ',I4,'-->',I4,'(',L1,')'//
     *   1X,'BUS RAPID TRN  BIKE-->STA #1 ',I4,'-->',I4,'(',L1,')'/
     *   1X,'BUS RAPID TRN  BIKE-->STA #2 ',I4,'-->',I4,'(',L1,')')
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
      DO 18 K=1,14
      DO 18 L=1,5
      IF(L.EQ.3.OR.L.EQ.4) CYCLE
      IF(OSTA(L,K).LE.0) OSTA(L,K)=MAX_ZONES
      IF(ASTA(L,K).LE.0) ASTA(L,K)=MAX_ZONES
      DC=ASTA(L,K)-MAX_IZONES
      IC=STAIND(DC,JZ)
      IF(IC.LT.1) IC=1
      ASTA2(L,K)=IC+(L-1)*4
   18 CONTINUE
C.............................................................................
      IF(DEBUG) THEN
      WRITE(26,9034) ((OSTA(2,K),ASTA(2,K)),K=1,14)
 9034 FORMAT(//10X,' UPDATED STATION NUMBERS'/
     *    1X,'ORIGIN --> DESTINATION STATION SUMMARY'/
     *    1X,'--------------------------------------'/
     *    1X,'URBAN RAIL     WALK-->STA #1 ',I4,'-->',I4/
     *    1X,'URBAN RAIL     WALK-->STA #2 ',I4,'-->',I4//
     *    1X,'URBAN RAIL     BUS -->STA #1 ',I4,'-->',I4/
     *    1X,'URBAN RAIL     BUS -->STA #2 ',I4,'-->',I4//
     *    1X,'URBAN RAIL     P&R -->STA #1 ',I4,'-->',I4/
     *    1X,'URBAN RAIL     P&R -->STA #2 ',I4,'-->',I4/
     *    1X,'URBAN RAIL     P&R -->STA #3 ',I4,'-->',I4/
     *    1X,'URBAN RAIL     P&R -->STA #4 ',I4,'-->',I4//
     *    1X,'URBAN RAIL     K&R -->STA #1 ',I4,'-->',I4/
     *    1X,'URBAN RAIL     K&R -->STA #2 ',I4,'-->',I4/
     *    1X,'URBAN RAIL     K&R -->STA #3 ',I4,'-->',I4/
     *    1X,'URBAN RAIL     K&R -->STA #4 ',I4,'-->',I4//
     *    1X,'URBAN RAIL     BIKE-->STA #1 ',I4,'-->',I4/
     *    1X,'URBAN RAIL     BIKE-->STA #2 ',I4,'-->',I4)
      WRITE(26,9035) CWEXP,CWTWY,CBEXP,CBTWY
 9035 FORMAT(//1X,'WALK ACCESS STATION NUMBERS'/
     *         1X,'---------------------------'/
     *         1X,'EXPRESS BUS=',I5/
     *         1X,'TRANSITWAY =',I5//
     *         1X,'BIKE ACCESS STATION NUMBERS'/
     *         1X,'---------------------------'/
     *         1X,'EXPRESS BUS=',I5/
     *         1X,'TRANSITWAY =',I5/)
      END IF
C.................................................................................
C
C MARKET SEGMENTATION FOR SINGLE MARKET PURPOSES
C AND FOR SPECIAL EVENT TRIPS
C
      IF((NCATS.EQ.1).OR.SPEVENT) THEN
      PERIN(1,JZ)=PERSON(JZ)
      PERIN(2,JZ)=0.0
      PERIN(3,JZ)=0.0
      PERIN(4,JZ)=0.0
      PERIN(5,JZ)=0.0
      END IF
C
C SUMMARIZE PERSON TRIPS BY MARKET SEGMENT AND TRIP LENGTH
C
      PINDEX=IFIX(TAB2DA(JZ)/5.0)+1
      IF(PINDEX.GT.21) PINDEX=21
      PTRIP(PINDEX,1)=PTRIP(PINDEX,1)+PERIN(1,JZ)
      PTRIP(PINDEX,2)=PTRIP(PINDEX,2)+PERIN(2,JZ)
      PTRIP(PINDEX,3)=PTRIP(PINDEX,3)+PERIN(3,JZ)  
      PTRIP(PINDEX,4)=PTRIP(PINDEX,4)+PERIN(4,JZ)
      PTRIP(PINDEX,5)=PTRIP(PINDEX,5)+PERIN(5,JZ)    
C
C  SET PERSON TRIPS FOR DEBUG FUNCTION
C
      IF(DEBUG.AND.FIXIJTRIPS.GT.0) THEN
      PERIN(1,JZ)=FIXIJTRIPS
      PERIN(2,JZ)=FIXIJTRIPS
      PERIN(3,JZ)=FIXIJTRIPS
      PERIN(4,JZ)=FIXIJTRIPS
      PERIN(5,JZ)=FIXIJTRIPS
      PERSON(JZ)=PERIN(1,JZ)+PERIN(2,JZ)+PERIN(3,JZ)+
     *           PERIN(4,JZ)+PERIN(5,JZ)
      WRITE(26,9345) IZ,JZ,PERSON(JZ)
	    WRITE(*,9345) IZ,JZ,PERSON(JZ)
 9345 FORMAT(/,' DEBUG PERSON TRIPS OVERRIDDEN WITH FIXIJTRIPS'/
     *         ' ---------------------------------------------'/
     *        ' ORIGIN ZONE =',I6,' DESTINATION ZONE=',I6,
     *        '  PERSON TRIPS =',F10.2/,1X,72('-'),/)
      END IF
      IF(LDEBUG.AND.FIXIJTRIPS.GT.0.AND.(.NOT.LAXTRN)) THEN
      PERIN(2,JZ)=FIXIJTRIPS
      PERSON(JZ)=FIXIJTRIPS
      IF(.NOT.AIRPASS) THEN
      PERIN(1,JZ)=FIXIJTRIPS
      PERIN(3,JZ)=FIXIJTRIPS
      PERSON(JZ)=PERIN(1,JZ)+PERIN(2,JZ)+PERIN(3,JZ)
      END IF
      WRITE(26,9345) IZ,JZ,PERSON(JZ)
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
C  COMPUTE MARKET SHARE CONSTANTS FOR LOGSUM AVERAGING
C
      IF(TRNUSER) THEN
      NOWALK=MWALK(1)+MWALK(2)+MWALK(3)+MWALK(4)+MWALK(5)+MWALK(6)
      DO K=1,6
      LNWALK(K)=0.0
      IF(MWALK(K).GT.0) LNWALK(K)=LOG(MWALK(K)/NOWALK)
      END DO
      END IF
C
C  COMPUTE FTA-RELATED ACCESS PROPORTIONS
C
      TWALK(1)=MWALK(1)+MWALK(2)+MWALK(3)+MWALK(4)
      TWALK(2)=MWALK(5)+MWALK(6)
C....................................................................
      IF(DEBUG.OR.(LDEBUG.AND.(.NOT.LAXTRN))) THEN
      WRITE(26,9021) PERSON(jz),PERIN(1,JZ),PERIN(2,JZ),PERIN(3,JZ),
     *               PERIN(4,JZ),PERIN(5,JZ),
     *               PWALK(IZ,1),PWALK(IZ,2),
     *               PWALK(JZ,1),PWALK(JZ,2),MWALK(1),
     *               MWALK(2),MWALK(3),MWALK(4),
     *               MWALK(5),MWALK(6),MWALK(7),
     *               TWALK(1),TWALK(2)
 9021 FORMAT(/1X,'MARKET SEGMENTATION COMPUTATIONS'/
     *       1X,'----------------------------------'/
     *       1X,'PERSON TRIPS      TOTAL=',F10.4/
     *       1X,'PERSON TRIPS MKT  GRP 1=',F10.4/
     *       1X,'PERSON TRIPS MKT. GRP 2=',F10.4/
     *       1X,'PERSON TRIPS MKT. GRP 3=',F10.4/
     *       1X,'PERSON TRIPS MKT. GRP 4=',F10.4/
     *       1X,'PERSON TRIPS MKT. GRP 5=',F10.4//
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
     *       1X,'DRIVE SEGMENT   FTA    =',F10.4//)
      IF(TRNUSER) THEN
      WRITE(26,9028) LNWALK
 9028 FORMAT(1X,'LOGSUM AVERAGING CONSTANTS'/
     *       1X,'--------------------------'/
     *       1X,'WALK  SEGMENT    1     =',F10.4/
     *       1X,'WALK  SEGMENT    2     =',F10.4/
     *       1X,'WALK  SEGMENT    3     =',F10.4/
     *       1X,'WALK  SEGMENT    4     =',F10.4/
     *       1X,'DRIVE SEGMENT    5     =',F10.4/
     *       1X,'DRIVE SEGMENT    6     =',F10.4/)
      END IF
      END IF
C....................................................................
C
C  CALCULATE CONDITIONAL MODE PROBABILITIES
C  ....... 5 MARKET SEGMENT  INDEX C
C
      IF(SPEVENT) NCATS=1
      IF(AIRPASS) NCATS=3
      DO 1000 C=1,NCATS
      IF(AIRPASS.AND.C.NE.2) GO TO 1000
      TSHAR=0.0
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
      UTILTAXI=0.0
      UTILUBER=0.0
C ====================  AIR PASSENGER MODEL SECTION ==================
C
C       LAX PARKING LOT CHOICE
C
      IF(LAX.AND.(.NOT.LAXTRN).AND.AJZ.GT.0) THEN
      IF(LDEBUG) WRITE(26,36) C
   36 FORMAT(/' LAX PARKING LOT CHOICE DRIVE ALONE UTILITY',
     *       ' COMPUTATIONS - MARKET SEGMENT ',I1/
     *       ' ------------------------------------------',
     *       '-------------'/
     *       ' PROD  ATTR   LAX      HIGHWAY     ZNELOT ',
     *       '    WALK     SHUTTLE   TRANSIT    TRNLOT'/
     *       ' ZONE  ZONE   LOT    TIME   DIST   UTILITY',
     *       '   UTILITY   UTILITY   UTILITY   UTILITY  KLOTRN'/
     *       ' ----  ----  -----  -----  -----  --------',
     *       '  --------  --------  --------  --------  --------')
      DO 35 NI=1,50
      IF(PEQUIV(NI).LE.0) GO TO 35
      IF(PRKDATA(NI,1).EQ.0) GO TO 35
      KJZ=PEQUIV(NI)
C....EMPLOYEE LOT ?
        IF(PRKDATA(NI,4).GT.0.AND.(.NOT.AIRPASS)) THEN
        WAIT1A=DMIN1(PRKDATA(NI,8),WAITLT) 
        WAIT1B=DIM(PRKDATA(NI,8),WAITLT)
        PRKCST=PRKDATA(NI,6)
        PSPACES=PRKDATA(NI,4)
        SHCOST=PRKDATA(NI,11)
        SFACTR=PRKDATA(NI,13)
        K_PUBPRK=0.0
        SHDPRICE=PRKDATA(NI,19)
C....PUBLIC LOT
        ELSE
        WAIT1A=DMIN1(PRKDATA(NI,7),WAITLT) 
        WAIT1B=DIM(PRKDATA(NI,7),WAITLT)
        PRKCST=PRKDATA(NI,5)
        PSPACES=PRKDATA(NI,3)
        IF(EMPPRK) PSPACES=0
        SHCOST=PRKDATA(NI,10)
        SFACTR=PRKDATA(NI,12)
        K_PUBPRK=KPUBPRK
        SHDPRICE=PRKDATA(NI,19)
        END IF        
C...ZONE TO LOT
      UTILOT(NI,1)=0.0
      IF(PSPACES.GT.0) THEN
      UTILOT(NI,1)=COEFF(21)*FLOAT(ZNELOT(IZ,NI,1))/100.0+
     *             COEFF(60+C)*OPCOST*FLOAT(ZNELOT(IZ,NI,2))/100.0+
     *             COEFF(60+C)*PRKCST+
     *             COEFF(98)*LOG(PSPACES)+
     *             K_PUBPRK/(LSUMT*LSUMA*LSUMS)+
     *             SHDPRICE/(LSUMT*LSUMA*LSUMS)
      END IF
C...LOT TO ATTRACTION ZONE - WALK ACCESS
      ULOTWLK(NI)=0.0
      IF(PRKDATA(NI,2).EQ.1.0) THEN
       IF(KJZ.EQ.JZ) THEN
       ULOTWLK(NI)=(COEFF(27)*PRKDATA(NI,14))/(LSUMA*LSUMS*LSUMT)
       ELSE
        IF(PJZ.GT.0) THEN
        IF(PRKDATA(PJZ,2).EQ.1.0) THEN
        XDIST=SQRT(((SXCOORD(KJZ)-SXCOORD(JZ))**2.0)+
     *  ((SYCOORD(KJZ)-SYCOORD(JZ))**2.0))
        ULOTWLK(NI)=(COEFF(27)*XDIST*20.0)/(LSUMA*LSUMS*LSUMT)
        END IF
        END IF
        END IF
       END IF
C...LOT TO ATTRACTION ZONE - SHUTTLE 
       ULOTSHL(NI)=0.0
       IF(PRKDATA(NI,2).EQ.2.AND.CJZ.GT.0) THEN
       ULOTSHL(NI)=
     *     COEFF(21)*(FLOAT(ZNELOT(JZ,NI,1))/100.0)*SFACTR+
     *     COEFF(22)*WAIT1A+COEFF(23)*WAIT1B+
     *     COEFF(60+C)*SHCOST+
     *     COEFF(27)*PRKDATA(NI,9)

       ULOTSHL(NI)=ULOTSHL(NI)/(LSUMA*LSUMS*LSUMT)
       END IF
C...LOT TO ATTRACTION ZONE - SHUTTLE + APM (ITF OPTION)
      IF(PRKDATA(NI,2).EQ.4.AND.CJZ.GT.0.AND.ITFPRK) THEN
      DO K=1,50
      IF(AEQUIV(K).EQ.JZ) JZIND=K
      END DO
       ULOTSHL(NI)=
     *     COEFF(21)*(FLOAT(ZNELOT(ITFZONE,NI,1))/100.0)*SFACTR+
     *     COEFF(22)*WAIT1A+COEFF(23)*WAIT1B+
     *     COEFF(60+C)*SHCOST+
     *     COEFF(27)*PRKDATA(NI,9) +
     *     LSUMA * LAXIPROB(JZIND,42) -
     *     COEFF(22)*ITFSAVPRK
       ULOTSHL(NI)=ULOTSHL(NI)/(LSUMA*LSUMS*LSUMT)
       END IF
C...LOT TO ATTRACTION ZONE - SHUTTLE + APM (ITF2 OPTION)
      IF(PRKDATA(NI,2).EQ.5.AND.CJZ.GT.0.AND.ITFPRK.AND.
     *   ITFZONE2.GT.0) THEN
      DO K=1,50
      IF(AEQUIV(K).EQ.JZ) JZIND=K
      END DO
       ULOTSHL(NI)=
     *     COEFF(21)*(FLOAT(ZNELOT(ITFZONE2,NI,1))/100.0)*SFACTR+
     *     COEFF(22)*WAIT1A+COEFF(23)*WAIT1B+
     *     COEFF(60+C)*SHCOST+
     *     COEFF(27)*PRKDATA(NI,9) +
     *     LSUMA * LAXI2PROB(JZIND,42) -
     *     COEFF(22)*ITFSAVPRK
       ULOTSHL(NI)=ULOTSHL(NI)/(LSUMA*LSUMS*LSUMT)
       END IF
C....LOT TO ATTRACTION ZONE - TRANSIT
       ULOTTRN(NI)=0.0
       IF(LAXTPROB(NI,AJZ,42).NE.0.0.AND.AJZ.GT.0) THEN
       ULOTTRN(NI)=(LAXTPROB(NI,AJZ,42)+KLOTRN)/(LSUMA*LSUMS*LSUMT)
       END IF
       IF(TRNLOT.AND.PRKDATA(NI,2).EQ.1.0) ULOTTRN(NI)=0.0
       IF(PRKDATA(NI,2).EQ.4.0.AND.ITFPRK) ULOTTRN(NI)=0.0
       IF(PRKDATA(NI,2).EQ.5.0.AND.ITFPRK) ULOTTRN(NI)=0.0
C....CHECK FOR PARKING AND DESTINATION ZONE BEING EQUAL
       IF(JZ.EQ.KJZ) THEN
       ULOTWLK(NI)=(COEFF(27)*3.0)/(LSUMA*LSUMS*LSUMT)
       ULOTSHL(NI)=0.0
       ULOTTRN(NI)=0.0
       END IF
C ----------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,34) IZ,JZ,KJZ,ZNELOT(IZ,NI,1),ZNELOT(IZ,NI,2),
     *             UTILOT(NI,1),ULOTWLK(NI),ULOTSHL(NI),
     *             ULOTTRN(NI),LAXTPROB(NI,AJZ,42),KLOTRN
   34 FORMAT(1X,I4,2X,I4,2X,I5,2X,I5,2X,I5,4(2X,F8.3),2(2X,F10.5))
      END IF
C ----------------------------------------------------------
   35 CONTINUE
      IF(LDEBUG) WRITE(26,4336) C
 4336 FORMAT(/' LAX PARKING LOT CHOICE DRIVE ALONE PROBABILITY',
     *       ' COMPUTATIONS - MARKET SEGMENT ',I1/
     *       ' ------------------------------------------',
     *       '----------------'/
     *  ' PROD  ATTR   LAX      LOT        LOT      WALK    SHUTTLE',
     *            '   TRANSIT    WALK     SHUTTLE   TRANSIT'/
     *  ' ZONE  ZONE   LOT    UTILITY     SHARE     SHARE     SHARE ',
     *            '    SHARE   UTILITY   UTILITY   UTILITY'/
     *  ' ----  ----  -----   --------  -------   -------   -------',
     *            '   -------  --------  --------  --------')
     
C....COMPUTE LOT PROBABILITIES
      DENOM=0.0
      DO 37 NI=1,50
      IF(PEQUIV(NI).LE.0) GO TO 37
      IF(PRKDATA(NI,1).EQ.0) GO TO 37
C
      EUTILOT(NI,1)=0.0
      EUTILOT(NI,2)=0.0
      EUTILOT(NI,3)=0.0
      EUTILOT(NI,4)=0.0
      UTILOT(NI,2)=0.0
      LSLOTE=0.0
      IF(ULOTWLK(NI).NE.0) EUTILOT(NI,2)=EXP(ULOTWLK(NI))
      IF(ULOTSHL(NI).NE.0) EUTILOT(NI,3)=EXP(ULOTSHL(NI))
      IF(ULOTTRN(NI).NE.0) EUTILOT(NI,4)=EXP(ULOTTRN(NI))
      DENOME=EUTILOT(NI,2)+EUTILOT(NI,3)+EUTILOT(NI,4)
      IF(DENOME.NE.0.0) LSLOTE=DLOG(DENOME)
      IF(UTILOT(NI,1).NE.0.0) THEN
      UTILOT(NI,2)=UTILOT(NI,1)+LSUMS*LSUMT*LSLOTE
      EUTILOT(NI,1)=EXP(UTILOT(NI,2))
      END IF    
      IF(DENOME.GT.0) THEN
      DENOM=DENOM+EUTILOT(NI,1)
      ELSE
C....NO EGRESS AVAIALABLE
      EUTILOT(NI,1)=0.0
      END IF
   37 CONTINUE
      IF(DENOM.EQ.0) GO TO 39
      DO 38 NI=1,50
      IF(PEQUIV(NI).LE.0) GO TO 38
      IF(PRKDATA(NI,1).EQ.0) GO TO 38
      KJZ=PEQUIV(NI)
      PROBLOT(NI)=EUTILOT(NI,1)/DENOM
      DENOME=EUTILOT(NI,2)+EUTILOT(NI,3)+EUTILOT(NI,4)
      IF(DENOME.GT.0) THEN
      EPROBLOT(NI,1)=EUTILOT(NI,2)/DENOME
      EPROBLOT(NI,2)=EUTILOT(NI,3)/DENOME
      EPROBLOT(NI,3)=EUTILOT(NI,4)/DENOME
      ELSE
      EPROBLOT(NI,1)=0.0
      EPROBLOT(NI,2)=0.0
      EPROBLOT(NI,3)=0.0
      END IF
C --------------------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,42) IZ,JZ,KJZ,UTILOT(NI,2),PROBLOT(NI),
     *             (EPROBLOT(NI,K),K=1,3),
     *             ULOTWLK(NI),ULOTSHL(NI),ULOTTRN(NI)
   42 FORMAT(1X,I4,2X,I4,2X,I5,2X,F8.3,4(2X,F8.5),3(2X,F8.4))
 3888 FORMAT(I4,',',I4,3(',',F8.4))
      END IF
C ---------------------------------------------------------------
   38 CONTINUE
   39 CONTINUE 
      LSLOT=0.0
      IF(DENOM.NE.0.0) LSLOT=LSUMA*DLOG(DENOM)  
C ----------------------------------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,453) LSLOT
  453 FORMAT(/' PARKING LOT LOGSUM=',F9.3)
      END IF
C ---------------------------------------------------------------------------   
C
C  FLY-AWAY LOT SELECTION
C
      IF(CJZ.GT.0) THEN
      DENOM=0.0
C --------------------------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,445)
  445 FORMAT(//                                        
     *     '    FLY-AWAY       TAZ     LOT      WALK       WALK    ',
     *     ' TRANSIT    PNR   ',
     *     '    PNR       PNR      KNR       KNR       KNR     ',
     *     '  TAXI       ACCESS'/
     *     '    STATION       NODE   UTILITY     DIST    UTILITY   ',
     *     ' UTILITY    TIME  ',
     *     '    DIST    UTILITY    TIME      DIST    UTILITY    ',
     *     'UTILITY    UTILITY'/
     *     ' ---------------  ----  ---------  -------  ',
     *     '---------  ---------  -------',
     *     '  -------  ---------  -------  -------  ---------  ',
     *     '---------  ---------')
      END IF
C ---------------------------------------------------------------------
      DO 443 NI=1,10
      IF(FLYADATA(NI,3).LT.1) GO TO 443
C..COMPUTE LOT TO ZONE UTILITY PORTION
      WAIT1A=DMIN1(FLYADATA(NI,4),WAITLT)
      WAIT1B=DIM(FLYADATA(NI,4),WAITLT)
      UTILFLY=COEFF(21)*FLOAT(FLYZNE(NI,CJZ))/100.0+
     *        COEFF(22)*WAIT1A+
     *        COEFF(23)*WAIT1B+
     *        COEFF(60+C)*FLYADATA(NI,5)+
     *        COEFF(60+C)*FLYADATA(NI,6)+
     *        COEFF(17)*FLYAEGR
      IF(ITFFLY.EQ.1) THEN
      DO K=1,50
      IF(AEQUIV(K).EQ.JZ) JZIND=K
      END DO
      UTILFLY=COEFF(21)*FLOAT(FLYZNE(NI,11))/100.0+
     *        COEFF(22)*WAIT1A+
     *        COEFF(23)*WAIT1B+
     *        COEFF(60+C)*FLYADATA(NI,5)+
     *        COEFF(60+C)*FLYADATA(NI,6)+
     *        COEFF(17)*FLYAEGR +
     *        LSUMA * LAXIPROB(JZIND,42) -
     *        COEFF(22)*ITFSAVFLY
      END IF
      IF(ITFFLY.EQ.2) THEN
      DO K=1,50
      IF(AEQUIV(K).EQ.JZ) JZIND=K
      END DO
      UTILFLY=COEFF(21)*FLOAT(FLYZNE(NI,12))/100.0+
     *        COEFF(22)*WAIT1A+
     *        COEFF(23)*WAIT1B+
     *        COEFF(60+C)*FLYADATA(NI,5)+
     *        COEFF(60+C)*FLYADATA(NI,6)+
     *        COEFF(17)*FLYAEGR +
     *        LSUMA * LAXI2PROB(JZIND,42) -
     *        COEFF(22)*ITFSAVFLY
      END IF
C..COMPUTE ACCESS MODE UTILITIES
C...WALK
      KJZ=IDINT(FLYADATA(NI,1))
      IF(KJZ.LE.0) GO TO 443 
      XDIST=SQRT(((SXCOORD(KJZ)-SXCOORD(IZ))**2.0)+
     *  ((SYCOORD(KJZ)-SYCOORD(IZ))**2.0))
      UTILWLK=COEFF(27)*XDIST*20.0+
     *        KFLYWLK/(LSUMT*LSUMS*LSUMA)
      FLYAEUIL(NI,1)=EXP(UTILWLK+UTILFLY)
C...PUBLIC TRANSIT
      UTILTRN=LAXFPROB(IZ,NI,42)/(LSUMS*LSUMA*LSUMT)
      FLYAEUIL(NI,2)=0.0
      IF(UTILTRN.NE.0.0) 
     *     FLYAEUIL(NI,2)=EXP(UTILTRN+UTILFLY+
     *          (KFLYTRN/(LSUMS*LSUMA*LSUMT)))
C...PARK-N-RIDE
      UTILPNR=COEFF(21)* TAB1DA(KJZ)
     *       +COEFF(60+C)* opcost*(TAB2DA(KJZ))
     *       +COEFF(60+C)* FLYADATA(NI,6)/2.0
     *       +KFLYPNR/(LSUMT*LSUMS*LSUMA)
      FLYAEUIL(NI,3)=EXP(UTILPNR+UTILFLY)
C...KISS-N-RIDE
      UTILKNR=COEFF(21)* TAB42P(KJZ)
     *       +COEFF(60+C)* opcost*(TAB52P(KJZ))
     *       +KFLYKNR/(LSUMT*LSUMS*LSUMA)
      FLYAEUIL(NI,4)=EXP(UTILKNR+UTILFLY)
C...TAXI
      UTILTXI=COEFF(21)*TAB42P(KJZ)
     *       +COEFF(60+C)*(((TAB52P(KJZ)*245)+265.0)*1.15*0.804)
     *       +KFLYTXI/(LSUMT*LSUMS*LSUMA)
      FLYAEUIL(NI,5)=EXP(UTILTXI+UTILFLY)
C...PROBABILITIES
      FLYAEUIL(NI,6)=FLYAEUIL(NI,1)+FLYAEUIL(NI,2)+FLYAEUIL(NI,3)+
     *      FLYAEUIL(NI,4)+FLYAEUIL(NI,5)
      FLYAPROB(NI,1)=FLYAEUIL(NI,1)/FLYAEUIL(NI,6)
      FLYAPROB(NI,2)=FLYAEUIL(NI,2)/FLYAEUIL(NI,6)
      FLYAPROB(NI,3)=FLYAEUIL(NI,3)/FLYAEUIL(NI,6)
      FLYAPROB(NI,4)=FLYAEUIL(NI,4)/FLYAEUIL(NI,6)
      FLYAPROB(NI,5)=1.0-FLYAPROB(NI,1)-FLYAPROB(NI,2)-FLYAPROB(NI,3)
     *                  -FLYAPROB(NI,4)
      FLYAEUIL(NI,7)=0.0
      IF(FLYAEUIL(NI,6).NE.0.0) THEN
      FLYAEUIL(NI,6)=LSUMT*DLOG(FLYAEUIL(NI,6))
      FLYAEUIL(NI,7)=EXP(FLYAEUIL(NI,6))
      DENOM=DENOM+FLYAEUIL(NI,7)
      END IF
      
C -----------------------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,442) FLYNAME(NI),KJZ,UTILFLY,XDIST,
     *              UTILWLK,UTILTRN,TAB1DA(KJZ),
     *              TAB2DA(KJZ),UTILPNR,TAB42P(KJZ),TAB52P(KJZ),
     *              UTILKNR,UTILTXI,FLYAEUIL(NI,6)
  442 FORMAT(1X,A15,2X,I4,2X,F9.3,2X,F7.2,2(2X,F9.3),2(2X,F7.2),
     *       2X,F9.3,2(2X,F7.2),3(2X,F9.3))
      END IF
C ------------------------------------------------------------------------
  443 CONTINUE
C --------------------------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,447)
  447 FORMAT(//                                        
     *     '    FLY-AWAY       TAZ   < -------  PROBABILITIES ------',
     *                '------------> '/
     *     '    STATION       NODE   LOT     WALK   TRANSIT  P&R    ',
     *                ' K&R     TAXI '/
     *     ' ---------------  ----  ------  ------  ------  ------  ',
     *                '------  ------')
      END IF
C ---------------------------------------------------------------------
      DO 446 NI=1,10
      KJZ=IDINT(FLYADATA(NI,1))
      IF(KJZ.LE.0) GO TO 446
      FLYAPROB(NI,6)=FLYAEUIL(NI,7)/DENOM
C --------------------------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,448) FLYNAME(NI),KJZ,FLYAPROB(NI,6),
     *              (FLYAPROB(NI,K),K=1,5)
  448 FORMAT(1X,A15,2X,I4,6(2X,F6.3))
      END IF
C ---------------------------------------------------------------------------
  446 CONTINUE
      LSFLY=LSUMS*DLOG(DENOM)+(ACNST(8)+KFLYAWAY)/LSUMA
      EUTILFLY=EXP(LSFLY)
C ----------------------------------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,449) LSFLY
  449 FORMAT(/' FLY-AWAY LOGSUM=',F9.3)
      END IF
C ---------------------------------------------------------------------------
      END IF
C
C  RENTAL CAR FACILITY SELECTION
C
      IF(AIRPASS.AND.(.NOT.LAXTRN)) THEN
      IF(LDEBUG) WRITE(26,436) C
  436 FORMAT(/' RENTAL CAR FACILITY UTILITY COMPUTATIONS',
     *       ' - MARKET SEGMENT ',I1/
     *       ' -----------------------------------------',
     *       '------------------'/
     *       ' PROD  ATTR         SHUTTLE   TRANSIT'/
     *       ' ZONE  ZONE   TAZ   UTILITY   UTILITY'/
     *       ' ----  ----  -----  --------  --------')
      DO 435 NI=1,10
      KJZ=IDINT(RNTLDATA(NI,1))
      IF(KJZ.EQ.0) GO TO 435
C...LOT TO ATTRACTION ZONE - SHUTTLE
       URNTSHL(NI)=
     *     COEFF(21)*(FLOAT(ZNERNT(JZ,NI,1))/100.0)*RNTLDATA(NI,6)+
     *     COEFF(22)*(RNTLDATA(NI,5)+RNTLDATA(NI,3))+
     *     COEFF(60+C)*RNTLDATA(NI,4)
       URNTSHL(NI)=URNTSHL(NI)/(LSUMA*LSUMS*LSUMT)
C....LOT TO ATTRACTION ZONE - TRANSIT
       URNTTRN(NI)=0.0
       IF(LAXRPROB(NI,AJZ,42).NE.0.0.AND.AJZ.GT.0) THEN
C      URNTTRN(NI)=LAXRPROB(NI,AJZ,42)/(LSUMA*LSUMS*LSUMT)
       URNTTRN(NI)=LAXRPROB(NI,AJZ,42)
       END IF
C ----------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,434) IZ,JZ,KJZ,URNTSHL(NI),URNTTRN(NI)
  434 FORMAT(1X,I4,2X,I4,2X,I5,2(2X,F8.3))
      END IF
C ----------------------------------------------------------
  435 CONTINUE
      IF(LDEBUG) WRITE(26,4346) C
 4346 FORMAT(/' RENTAL FACILITY PROBABILITY COMPUTATIONS',
     *        ' - MARKET SEGMENT ',I1/
     *        ' -----------------------------------------',
     *        '------------------'/
     *  ' PROD  ATTR            LOT   SHUTTLE  TRANSIT'/
     *  ' ZONE  ZONE   TAZ     SHARE    SHARE    SHARE'/
     *  ' ----  ----  -----  -------  -------  -------')
C....COMPUTE PROBABILITIES
      DENOM=0.0
      DO 437 NI=1,10
      KJZ=IDINT(RNTLDATA(NI,1))
      IF(KJZ.EQ.0) GO TO 437
      EUTLRNT(NI,1)=0.0
      EUTLRNT(NI,2)=0.0
      IF(URNTSHL(NI).NE.0) EUTLRNT(NI,1)=EXP(URNTSHL(NI))
      IF(URNTTRN(NI).NE.0) EUTLRNT(NI,2)=EXP(URNTTRN(NI))
      DENOM=EUTLRNT(NI,1)+EUTLRNT(NI,2)
      IF(DENOM.NE.0.0) THEN
      PROBRNT(NI,1)=EUTLRNT(NI,1)/DENOM
      PROBRNT(NI,2)=EUTLRNT(NI,2)/DENOM
      END IF
      IF(CONRAC) THEN
      PROBRNT(NI,1)=0.0
      PROBRNT(NI,2)=1.0
      ELSE
      PROBRNT(NI,1)=1.0
      PROBRNT(NI,2)=0.0
      END IF
C --------------------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,444) IZ,JZ,KJZ,RNTLDATA(NI,2),
     *             PROBRNT(NI,1),PROBRNT(NI,2)
  444 FORMAT(1X,I4,2X,I4,2X,I5,3(1X,F8.5))
      END IF
C ---------------------------------------------------------------
  437 CONTINUE
C
C  AIR PASSENGER MODE CHOICE COMPUTATIONS
C
      UTLATXI=ACOEF(1)*TAB13P(JZ)+ACOEF(2)*0.25+
     *        ACOEF(3)*(265+245*TAB23P(JZ))*LCPI+
     *        ACNST(1)
      UTLARNT=ACOEF(1)*TAB1DA(JZ)+ACOEF(2)*RNTWAIT+
     *        ACOEF(3)*ACOEF(4)*TAB2DA(JZ)*LCPI+
     *        ACNST(2)
      UTLALMO=ACOEF(1)*TAB13P(JZ)+ACOEF(2)*0.25+
     *        ACOEF(3)*(5682.18+138.99*TAB23P(JZ))*LCPI+
     *        ACNST(3)
      UTLADRP=ACOEF(1)*TAB1DA(JZ)+ACOEF(2)*0.25+
     *        ACOEF(3)*OPCOST*TAB2DA(JZ)*LCPI+
     *        ACNST(4)
      UTLAONC=ACOEF(1)*TAB13P(JZ)+ACOEF(2)*1.0+
     *        ACOEF(3)*(599.15+89.84*TAB23P(JZ))*LCPI+
     *        ACNST(6)
C.....ON-CALL ITF OPTIONS
      IF(ITFONC.EQ.1) THEN
      DO K=1,50
      IF(AEQUIV(K).EQ.JZ) JZIND=K
      END DO
      UTLAONC=ACOEF(1)*TAB13P(ITFZONE)+ACOEF(2)*1.0+
     *        ACOEF(3)*(599.15+89.84*TAB23P(ITFZONE))*LCPI+
     *        ACNST(6) +
     *        LSUMA*LAXIPROB(JZIND,42) -
     *        ACOEF(2)*ITFSAVONC
      END IF
      IF(ITFONC.EQ.2.AND.ITFZONE2.GT.0) THEN
      DO K=1,50
      IF(AEQUIV(K).EQ.JZ) JZIND=K
      END DO
      UTLAONC=ACOEF(1)*TAB13P(ITFZONE2)+ACOEF(2)*1.0+
     *        ACOEF(3)*(599.15+89.84*TAB23P(ITFZONE2))*LCPI+
     *        ACNST(6) +
     *        LSUMA*LAXI2PROB(JZIND,42) -
     *        ACOEF(2)*ITFSAVONC
      END IF
C.....TAXI ITF OPTIONS
      UTLATXI1=0.0
      UTLATXI2=0.0
      IF(ITFTXI.AND.ITFZONE.GT.0) THEN
      DO K=1,50
      IF(AEQUIV(K).EQ.JZ) JZIND=K
      END DO
      UTLATXI1=ACOEF(1)*TAB13P(ITFZONE)+ACOEF(2)*0.25+
     *        ACOEF(3)*(265+245*TAB23P(ITFZONE))*LCPI+
     *        ACNST(1) +
     *        LSUMA*LAXIPROB(JZIND,42) +
     *        ACOEF(2)*ITFPEN(1,1)
      UTLATXI1=UTLATXI1/LSUMA
      END IF
      IF(ITFTXI.AND.ITFZONE2.GT.0) THEN
      DO K=1,50
      IF(AEQUIV(K).EQ.JZ) JZIND=K
      END DO
      UTLATXI2=ACOEF(1)*TAB13P(ITFZONE2)+ACOEF(2)*0.25+
     *        ACOEF(3)*(265+245*TAB23P(ITFZONE2))*LCPI+
     *        ACNST(1) +
     *        LSUMA*LAXI2PROB(JZIND,42) +
     *        ACOEF(2)*ITFPEN(2,1)
      UTLATXI2=UTLATXI2/LSUMA
      END IF
C.... LIMO ITF OPTIONS
      UTLALMO1=0.0
      UTLALMO2=0.0
      IF(ITFLMO.AND.ITFZONE.GT.0) THEN
      UTLALMO1=ACOEF(1)*TAB13P(ITFZONE)+ACOEF(2)*0.25+
     *        ACOEF(3)*(5682.18+138.99*TAB23P(ITFZONE))*LCPI+
     *        ACNST(3) +
     *        ACOEF(2)*ITFPEN(1,2)
      UTLALMO1=UTLALMO1/LSUMA
      END IF
      IF(ITFLMO.AND.ITFZONE2.GT.0) THEN
      UTLALMO2=ACOEF(1)*TAB13P(ITFZONE2)+ACOEF(2)*0.25+
     *        ACOEF(3)*(5682.18+138.99*TAB23P(ITFZONE2))*LCPI+
     *        ACNST(3) +
     *        ACOEF(2)*ITFPEN(2,2)
      UTLALMO2=UTLALMO2/LSUMA
      END IF
C.....DROP OFF ITF OPTIONS
      UTLADRP1=0.0
      UTLADRP2=0.0
      IF(ITFDRP.AND.ITFZONE.GT.0) THEN
      UTLADRP1=ACOEF(1)*TAB1DA(ITFZONE)+ACOEF(2)*0.25+
     *        ACOEF(3)*OPCOST*TAB2DA(ITFZONE)*LCPI+
     *        ACNST(4) + 
     *        ACOEF(2)*ITFPEN(1,3)
      UTLADRP1=UTLADRP1/LSUMA
      END IF
      IF(ITFDRP.AND.ITFZONE2.GT.0) THEN
      UTLADRP2=ACOEF(1)*TAB1DA(ITFZONE2)+ACOEF(2)*0.25+
     *        ACOEF(3)*OPCOST*TAB2DA(ITFZONE2)*LCPI+
     *        ACNST(4) +
     *        ACOEF(2)*ITFPEN(2,3)
      UTLADRP2=UTLADRP2/LSUMA
      END IF
      UTLATXI=UTLATXI/LSUMA
      UTLALMO=UTLALMO/LSUMA
      UTLADRP=UTLADRP/LSUMA
      UTLAONC=UTLAONC/LSUMA
      UTLARNT=UTLARNT/LSUMA
      END IF
C.....COMPUTE ITF SHARES
      IF(ITFTXI) THEN
      PROBTXI=0.0
      IF(UTLATXI.NE.0.0)  PROBTXI(1)=EXP(UTLATXI)
      IF(UTLATXI1.NE.0.0) PROBTXI(2)=EXP(UTLATXI1)
      IF(UTLATXI2.NE.0.0) PROBTXI(3)=EXP(UTLATXI2)
      DENOM=PROBTXI(1)+PROBTXI(2)+PROBTXI(3)
      IF(DENOM.GT.0.0) THEN
      DO K=1,3
      PROBTXI(K)=PROBTXI(K)/DENOM
      END DO
      END IF
      END IF
      IF(ITFLMO) THEN
      PROBLMO=0.0
      IF(UTLALMO.NE.0.0)  PROBLMO(1)=EXP(UTLALMO)
      IF(UTLALMO1.NE.0.0) PROBLMO(2)=EXP(UTLALMO1)
      IF(UTLALMO2.NE.0.0) PROBLMO(3)=EXP(UTLALMO2)
      DENOM=PROBLMO(1)+PROBLMO(2)+PROBLMO(3)
      IF(DENOM.GT.0.0) THEN
      DO K=1,3
      PROBLMO(K)=PROBLMO(K)/DENOM
      END DO
      END IF
      END IF
      IF(ITFDRP) THEN
      PROBDRP=0.0
      IF(UTLADRP.NE.0.0)  PROBDRP(1)=EXP(UTLADRP)
      IF(UTLADRP1.NE.0.0) PROBDRP(2)=EXP(UTLADRP1)
      IF(UTLADRP2.NE.0.0) PROBDRP(3)=EXP(UTLADRP2)
      DENOM=PROBDRP(1)+PROBDRP(2)+PROBDRP(3)
      IF(DENOM.GT.0.0) THEN
      DO K=1,3
      PROBDRP(K)=PROBDRP(K)/DENOM
      END DO
      END IF
      END IF
C ------------------------------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,450) C,UTLATXI,UTLALMO,UTLADRP,LSLOT,UTLAONC,LSFLY,
     *                UTLARNT,UTLATXI1,UTLALMO1,UTLADRP1,
     *                UTLATXI2,UTLALMO2,UTLADRP2,PROBTXI(1),PROBLMO(1),
     *                PROBDRP(1),PROBTXI(2),PROBLMO(2),PROBDRP(2),
     *                PROBTXI(3),PROBLMO(3),PROBDRP(3)
  450 FORMAT(/
     *       1X,' AIR PASSENGER MODEL UTILITIES -- MARKET SEGMENT ',I1/
     *       1X,' -----------------------------'/
     *       1X,'                                   PARKING'/
     *       1X,'   TAXI       LIMO      DROPOFF      LOT  ',
     *          '    ON-CALL    FLYAWAY    RENTAL'/
     *       1X,'  UTILITY    UTILITY    UTILITY    UTILITY',
     *          '    UTILITY    UTILITY    UTILITY'/
     *       1X,' ---------  ---------  ---------  ---------',
     *          '  ---------  ---------  ---------'/
     *       7(2X,F9.5)//3(2X,F9.5),' ITF  UTILITIES'/
     *       3(2X,F9.5),' ITF2 UTILITIES'/
     *       3(2X,F9.5),' CTA  PROBABILITIES'/
     *       3(2X,F9.5),' ITF  PROBABILITIES'/
     *       3(2X,F9.5),' ITF2 PROBABILITIES')
      END IF
C -------------------------------------------------------------------------
      END IF
C================== END OF AIR PASSENGER SECTION ============================
C
C     CONVENTIONAL/AUTOMATED VEHICLE LOOP
C
      DO 1800 AV=1,AV4
      AVCV=AV+30
C -------------------------------------------------------------------------
      IF(DEBUG) THEN
      WRITE(26,1453) C,AV,HHLDAV(AV),UBERAV(AV),ZHHD(AVCV,IZ)
 1453 FORMAT(/
     *       1X,' AV/CV MARKET SHARE SUMMARY'/
     *       1X,'-----------------------------'/
     *       1X,' MARKET SEGMENT      =',I6/
     *       1X,' AV/CV MARKET SEGMENT=',I6/
     *       1X,' HOUSEHOLD AV INDEX  =',F6.0/
     *       1X,' UBER      AV INDEX  =',F6.0/
     *       1X,' AV/CV PROPORTION    =',F6.2/)
      END IF
C
C     PARKING COST CHOICE SUBMODEL
C
      IF(AVMDL.AND.ALTPARK) THEN
      HOMECOST=OPCOST*TAB2DA(JZ)
C.............................................................
      IF(DEBUG) THEN
      WRITE(26,55555) JZ,C,HOMECOST,ZHHD(5,JZ),
     *                APRKCST(JZ),APRKZONE(JZ)
55555 FORMAT('     PARKING COST CHOICE MODEL'/
     *       ' --------------------------------'/
     *       ' DESTINATION ZONE               =',I8/
     *       ' MARKET SEGMENT GROUP           =',I8/
     *       ' RETURN TO HOME             COST=',F8.2/
     *       ' AT WORK            PARKING COST=',F8.2/
     *       ' ALTERNATE LOCATION PARKING COST=',F8.2/
     *       ' ALTERNATE LOCATION ZONE        =',I8/)
      END IF
C................................................................
      ALTWRK=COEFF(60+C)*ZHHD(5,JZ)
      IF(HOMECOST.LT.APRKCST(JZ)) THEN
      ALTOTH=COEFF(60+C)*HOMECOST
      HOMEAV=.TRUE.
      ELSE
      ALTOTH=COEFF(60+C)*APRKCST(JZ)
      HOMEAV=.FALSE.
      END IF
      EXPWRK=0.0
      EXPOTH=0.0
      IF(ALTWRK.NE.0.0) EXPWRK=EXP(ALTWRK)
      IF(ALTOTH.NE.0.0) EXPOTH=EXP(ALTOTH)
      DENOM=EXPWRK+EXPOTH
      IF(DENOM.GT.0) THEN
      PROBWRK=EXPWRK/DENOM
      PROBOTH=1.0-PROBWRK
      ELSE
      PROBWRK=1.0
      PROBOTH=0.0
      END IF
      COSTPARK=PROBWRK*ZHHD(5,JZ)+PROBOTH*APRKCST(JZ)
      IF(HOMEAV) COSTPARK=PROBWRK*ZHHD(5,JZ)+PROBOTH*HOMECOST
C......................................................................
      IF(DEBUG) WRITE(26,55556) ALTWRK,ALTOTH,PROBWRK,PROBOTH,HOMEAV,
     *                COSTPARK
55556 FORMAT(' AT  WORK UTILITY VALUE         =',F8.5/
     *       ' NOT WORK UTILITY VALUE         =',F8.5/
     *       ' AT  WORK PROBABILITY           =',F8.5/
     *       ' NOT WORK PROBABILITY           =',F8.5/
     *       ' RETURN TO HOME                 =',7X,L1/
     *       ' AVERAGE PARKING COST           =',F8.2/)
C......................................................................
      ELSE
      COSTPARK=ZHHD(5,JZ)
      PROBWRK=1.0
      PROBOTH=0.0
      END IF
C
C       COMPUTE THE COMMON EXPONENTIATED UTILITY FOR DRIVE-ALONE AUTO
C
C....NON-TOLL 
      UTIL0NT=COEFF(21)* TAB1DA(JZ) * (1.0-HHLDAV(AV))
     *        +COEFF(79) * AVHTIME(JZ) * HHLDAV(AV)
     *        +COEFF(27)*(ZHHD(7,IZ)+ZHHD(7,JZ) )
     *        +COEFF(60+C)* COSTPARK/2.0
     *        +COEFF(60+C)* opcost*(TAB2DA(JZ))
     *        +COEFF(60+C)* TAB3DA(JZ)
      UTIL0NT=UTIL0NT/(LSUM1AUTO*LSUM2DA*LSUM3DA)
C....TOLL
      SAV0T=0.0 
      IF(TAB7DA(JZ).GT.0) SAV0T=(TAB1DA(JZ)-TAB4DA(JZ))/TAB7DA(JZ)
      TMESAV(1)=IIDINT(SAV0T)+1
      IF(SAV0T.EQ.0) TMESAV(1)=31
      IF((TAB4DA(JZ).GT.0).AND.(TAB7DA(JZ).GT.TOLDIST)) THEN
      YTOLDS0=YINTER+YSLOPE*TAB5DA(JZ)
	    YTOLDS0=MIN(YTOLDS0,0.0)
      UTIL0T = COEFF(21)* TAB4DA(JZ) * (1.0-HHLDAV(AV))
     *        +COEFF(79) * AVTTIME(JZ) * HHLDAV(AV)
     *        +COEFF(27)*(ZHHD(7,IZ)+ZHHD(7,JZ) )
     *        +COEFF(60+C)* COSTPARK/2.0
     *        +COEFF(60+C)* opcost* TAB5DA(JZ)
     *        +COEFF(60+C)* TAB6DA(JZ)
     *        +COEFF(60+C)* TOLCST(1)*TAB7DA(JZ) + YTOLDS0
     *        +KTOLL
      UTIL0T=UTIL0T/(LSUM1AUTO*LSUM2DA*LSUM3DA)
      SUTIL0=0.0
      IF(SAV0T.LT.0.0) UTIL0T=0.0
      IF(SAV0T.LT.MINSAV.AND.SAV0T.GT.0) THEN
      SAVAL0=SAV0T/MINSAV
      SAVAL0=(1/SAVAL0)-0.99
      SUTIL0=(SAVAL0*SAVCOEF)/(LSUM1AUTO*LSUM2DA*LSUM3DA)
      END IF
      UTIL0T=UTIL0T + SUTIL0
      END IF
C
C       COMPUTE THE COMMON EXPONENTIATED UTILITY FOR 2-PERSON AUTO
C
C....NON-TOLL/NON-HOV
      UTIL2NT =COEFF(21)* TAB1DA(JZ) * (1.0-HHLDAV(AV))
     *        +COEFF(79) * AVHTIME(JZ) * HHLDAV(AV)
     *        +COEFF(27)*(ZHHD(7,IZ)+ZHHD(7,JZ))
     *        +COEFF(60+C)* COSTPARK/(HOV2P*2.0)
     *        +COEFF(60+C)* opcost* TAB2DA(JZ)/HOV2P
     *        +COEFF(60+C)* TAB3DA(JZ)/HOV2P
      UTIL2NT=UTIL2NT/(LSUM1AUTO*LSUM2SR*LSUM3P2)
C....HOV UTILITY FOR NON-TOLL CHOICE
	    IF(TAB72P(JZ).GT.HOVDIST) THEN
      UTIL2NTH =COEFF(21)* TAB12P(JZ) * (1.0-HHLDAV(AV))
     *        +COEFF(79) * AVH2TIME(JZ) * HHLDAV(AV)
     *        +COEFF(27)*(ZHHD(7,IZ)+ZHHD(7,JZ))
     *        +COEFF(60+C)* COSTPARK/(HOV2P*2.0)
     *        +COEFF(60+C)* opcost* TAB22P(JZ)/HOV2P
     *        +COEFF(60+C)* TAB32P(JZ)/HOV2P
     *        +KHOV2 
      UTIL2NTH=UTIL2NTH/(LSUM1AUTO*LSUM2SR*LSUM3P2)
      HOV2NTL=1
      END IF
C....TOLL/NON-HOV
	    YTOLDS2=YINTER+YSLOPE*TAB142P(JZ)
	    YTOLDS2=MIN(YTOLDS2,0.0)
	    SAV2T=0.0
      IF(TAB142P(JZ).GT.0) SAV2T=(TAB1DA(JZ)-TAB112P(JZ))/TAB142P(JZ)
      TMESAV(3)=IIDINT(SAV2T)+1
      IF(SAV2T.EQ.0) TMESAV(3)=31
      TMESAV(4)=TMESAV(3)
      IF((TAB112P(JZ).GT.0).AND.(TAB142P(JZ).GT.TOLDIST)) THEN
	    UTIL2T = COEFF(21)* TAB112P(JZ) * (1.0-HHLDAV(AV))
     *        +COEFF(79) * AVT2TIME(JZ) * HHLDAV(AV)
     *        +COEFF(27)*( ZHHD(7,IZ)+ZHHD(7,JZ) )
     *        +COEFF(60+C)* COSTPARK/(HOV2P*2.0)
     *        +COEFF(60+C)* OPCOST* TAB122P(JZ)/HOV2P
     *        +COEFF(60+C)* (DSCT2P*TAB132P(JZ))/HOV2P
     *        +COEFF(60+C)* TOLCST(2)*TAB142P(JZ) + YTOLDS2
     *        +KTOLL2
      UTIL2T=UTIL2T/(LSUM1AUTO*LSUM2SR*LSUM3P2)
      SUTIL2=0.0
      IF(SAV2T.LT.0.0) UTIL2T=0.0
      IF(SAV2T.LT.MINSAV.AND.SAV2T.GT.0) THEN
      SAVAL2=SAV2T/MINSAV
      SAVAL2=(1/SAVAL2)-0.99
      SUTIL2=(SAVAL2*SAVCOEF)/(LSUM1AUTO*LSUM2DA*LSUM3DA)
      END IF
      UTIL2T=UTIL2T + SUTIL2 
      END IF
C....HOV UTILITY FOR TOLL CHOICE
	    IF((TAB92P(JZ).GT.HOVDIST).AND.(TAB82P(JZ).GT.TOLDIST)) THEN
      UTIL2TH= COEFF(21)* TAB42P(JZ) * (1.0-HHLDAV(AV))
     *        +COEFF(79) * AVT2TIME(JZ) * HHLDAV(AV)
     *        +COEFF(27)*( ZHHD(7,IZ)+ZHHD(7,JZ) )
     *        +COEFF(60+C)* COSTPARK/(HOV2P*2.0)
     *        +COEFF(60+C)* OPCOST* TAB52P(JZ)/HOV2P
     *        +COEFF(60+C)* (DSCT2P*TAB62P(JZ))/HOV2P
     *        +COEFF(60+C)* TOLCST(2)*TAB82P(JZ) + YTOLDS2
     *        +KTOLL2 +KHOV2
      UTIL2TH=UTIL2TH/(LSUM1AUTO*LSUM2SR*LSUM3P2) 
      HOV2TOL=1
      IF(SAV2T.LT.0.0) UTIL2TH=0.0
      UTIL2TH=UTIL2TH + SUTIL2   
      END IF   
C
C       COMPUTE THE COMMON EXPONENTIATED UTILITY FOR 3 PERSON AUTO
C
C....NON-TOLL/NON-HOV
      UTIL3NT =COEFF(21)* TAB1DA(JZ)
     *        +COEFF(27)*( ZHHD(7,IZ)+ZHHD(7,JZ) )
     *        +COEFF(60+C)* (COSTPARK/(HOV3P*2.0))
     *        +COEFF(60+C)* opcost* (TAB2DA(JZ)/hov3p)
     *        +COEFF(60+C)* TAB3DA(JZ)/hov3p
      UTIL3NT=UTIL3NT/(LSUM1AUTO*LSUM2SR*LSUM3P3) 
C....HOV UTILITY FOR NON-TOLL CHOICE
	    IF(TAB73P(JZ).GT.HOVDIST) THEN
      UTIL3NTH=COEFF(21)* TAB13P(JZ) * (1.0-HHLDAV(AV))
     *        +COEFF(79) * AVH3TIME(JZ) * HHLDAV(AV)
     *        +COEFF(27)*( ZHHD(7,IZ)+ZHHD(7,JZ) )
     *        +COEFF(60+C)* (COSTPARK/(HOV3P*2.0))
     *        +COEFF(60+C)* opcost* (TAB23P(JZ)/hov3p)
     *        +COEFF(60+C)* TAB33P(JZ)/hov3p
     *        +KHOV3
      UTIL3NTH=UTIL3NTH/(LSUM1AUTO*LSUM2SR*LSUM3P3) 
      HOV3NTL=1
      END IF
C....TOLL/NON-HOV
	    YTOLDS3=YINTER+YSLOPE*TAB143P(JZ)
	    YTOLDS3=MIN(YTOLDS3,0.0)
	    SAV3T=0.0
      IF(TAB143P(JZ).GT.0) SAV3T=(TAB1DA(JZ)-TAB113P(JZ))/TAB143P(JZ)
      TMESAV(6)=IIDINT(SAV3T)+1
      IF(SAV3T.EQ.0) TMESAV(6)=31
      TMESAV(7)=TMESAV(6)
      IF((TAB113P(JZ).GT.0).AND.(TAB143P(JZ).GT.TOLDIST)) THEN
	    UTIL3T = COEFF(21)* TAB113P(JZ) * (1.0-HHLDAV(AV))
     *        +COEFF(79) * AVT3TIME(JZ) * HHLDAV(AV)
     *        +COEFF(27)*(ZHHD(7,IZ)+ZHHD(7,JZ))
     *        +COEFF(60+C)* (COSTPARK/(HOV3P*2.0))
     *        +COEFF(60+C)* opcost* (TAB123P(JZ)/hov3p)
     *        +COEFF(60+C)* (DSCT3P*TAB133P(JZ))/hov3p
     *        +COEFF(60+C)* TOLCST(3)*TAB143P(JZ)+ YTOLDS3
     *        +KTOLL3
      UTIL3T=UTIL3T/(LSUM1AUTO*LSUM2SR*LSUM3P3) 
      SUTIL3=0.0
      IF(SAV3T.LT.0.0) UTIL3T=0.0
      IF(SAV3T.LT.MINSAV.AND.SAV3T.GT.0) THEN
      SAVAL3=SAV3T/MINSAV
      SAVAL3=(1/SAVAL3)-0.99
      SUTIL3=(SAVAL3*SAVCOEF)/(LSUM1AUTO*LSUM2DA*LSUM3DA)
      END IF
      UTIL3T=UTIL3T + SUTIL3
      END IF
C....HOV UTILITY FOR TOLL CHOICE
	    IF((TAB93P(JZ).GT.HOVDIST).AND.(TAB83P(JZ).GT.TOLDIST)) THEN
	    UTIL3TH= COEFF(21)* TAB43P(JZ) * (1.0-HHLDAV(AV))
     *        +COEFF(79) * AVT3TIME(JZ) * HHLDAV(AV)
     *        +COEFF(27)*(ZHHD(7,IZ)+ZHHD(7,JZ))
     *        +COEFF(60+C)* (COSTPARK/(HOV3P*2.0))
     *        +COEFF(60+C)* opcost* (TAB53P(JZ)/hov3p)
     *        +COEFF(60+C)* (DSCT3P*TAB63P(JZ))/hov3p
     *        +COEFF(60+C)* TOLCST(3)*TAB83P(JZ)+ YTOLDS3
     *        +KTOLL3 +KHOV3
      UTIL3TH=UTIL3TH/(LSUM1AUTO*LSUM2SR*LSUM3P3)   
       HOV3TOL=1
      IF(SAV3T.LT.0.0) UTIL3TH=0.0
      UTIL3TH=UTIL3TH + SUTIL3    
       END IF
C
C       COMPUTE THE COMMON EXPONENTIATED UTILITY FOR 4+PERSON AUTO
C
C....NON-TOLL/NON-HOV
      UTIL4NT =COEFF(21)* TAB1DA(JZ)
     *        +COEFF(27)*( ZHHD(7,IZ)+ZHHD(7,JZ) )
     *        +COEFF(60+C)* (COSTPARK/(HOV4P*2.0))
     *        +COEFF(60+C)* opcost* (TAB2DA(JZ)/HOV4P)
     *        +COEFF(60+C)* TAB3DA(JZ)/hov4p
      UTIL4NT=UTIL4NT/(LSUM1AUTO*LSUM2SR*LSUM3P4)
C...HOV UTILITY FOR NON-TOLL CHOICE
	    IF(TAB74P(JZ).GT.HOVDIST) THEN
      UTIL4NTH=COEFF(21)* TAB14P(JZ) * (1.0-HHLDAV(AV))
     *        +COEFF(79) * AVH3TIME(JZ) * HHLDAV(AV)
     *        +COEFF(27)*( ZHHD(7,IZ)+ZHHD(7,JZ) )
     *        +COEFF(60+C)* (COSTPARK/(HOV4P*2.0))
     *        +COEFF(60+C)* opcost* (TAB24P(JZ)/HOV4P)
     *        +COEFF(60+C)* TAB34P(JZ)/hov4p
     *        +KHOV4
      UTIL4NTH=UTIL4NTH/(LSUM1AUTO*LSUM2SR*LSUM3P4) 
      HOV4NTL=1
      END IF
C....TOLL/NON-HOV
	    YTOLDS4=YINTER+YSLOPE*TAB144P(JZ)
	    YTOLDS4=MIN(YTOLDS4,0.0)
	    SAV4T=0.0
      IF(TAB144P(JZ).GT.0) SAV4T=(TAB1DA(JZ)-TAB114P(JZ))/TAB144P(JZ)
      TMESAV(9)=IIDINT(SAV4T)+1
      IF(SAV4T.EQ.0) TMESAV(9)=31
      TMESAV(10)=TMESAV(9)
      IF((TAB114P(JZ).GT.0).AND.(TAB144P(JZ).GT.TOLDIST)) THEN
	    UTIL4T = COEFF(21)* TAB114P(JZ) * (1.0-HHLDAV(AV))
     *        +COEFF(79) * AVT3TIME(JZ) * HHLDAV(AV)
     *        +COEFF(27)*(ZHHD(7,IZ)+ZHHD(7,JZ))
     *        +COEFF(60+C)* (COSTPARK/(HOV4P*2.0))
     *        +COEFF(60+C)* opcost* (TAB124P(JZ)/HOV4P)
     *        +COEFF(60+C)* (DSCT4P*TAB134P(JZ))/hov4p
     *        +COEFF(60+C)* TOLCST(4)*TAB144P(JZ)+ YTOLDS4
     *        +KTOLL4
      UTIL4T=UTIL4T/(LSUM1AUTO*LSUM2SR*LSUM3P4) 
      SUTIL4=0.0
      IF(SAV4T.LT.0.0) UTIL4T=0.0
      IF(SAV4T.LT.MINSAV.AND.SAV4T.GT.0) THEN
      SAVAL4=SAV4T/MINSAV
      SAVAL4=(1/SAVAL4)-0.99
      SUTIL4=(SAVAL4*SAVCOEF)/(LSUM1AUTO*LSUM2DA*LSUM3DA)
      END IF
      UTIL4T=UTIL4T + SUTIL4
      END IF
C....HOV UTILITY FOR TOLL CHOICE
	    IF((TAB94P(JZ).GT.HOVDIST).AND.(TAB84P(JZ).GT.TOLDIST)) THEN
	    UTIL4TH= COEFF(21)* TAB44P(JZ) * (1.0-HHLDAV(AV))
     *        +COEFF(79) * AVT3TIME(JZ) * HHLDAV(AV)
     *        +COEFF(27)*(ZHHD(7,IZ)+ZHHD(7,JZ))
     *        +COEFF(60+C)* (COSTPARK/(HOV4P*2.0))
     *        +COEFF(60+C)* opcost* (TAB54P(JZ)/HOV4P)
     *        +COEFF(60+C)* (DSCT4P*TAB64P(JZ))/hov4p
     *        +COEFF(60+C)* TOLCST(4)*TAB84P(JZ)+ YTOLDS4
     *        +KTOLL4 +KHOV4
      UTIL4TH=UTIL4TH/(LSUM1AUTO*LSUM2SR*LSUM3P4) 
       HOV4TOL=1
      IF(SAV4T.LT.0.0) UTIL4TH=0.0
      UTIL4TH=UTIL4TH + SUTIL4    
       END IF
C.....TAXI UTILITY                                                             !Innovations
      IF(TAXIMODE) THEN
  175 CALL RANDOM(RANVAL)
      YINDEX=IFIX(RANVAL*1000.0)
      IF(YINDEX.LE.0) GO TO 175
      ZINDEX=IFIX(ZHHD(29,IZ))
      IF(ZINDEX.EQ.0) ZINDEX=6
      IF(ZINDEX.GT.6) ZINDEX=6
      TXWAIT=TXDIST(YINDEX,ZINDEX)
      INDEX=IFIX(ZHHD(6,IZ))
      TAXICOST=2.85+2.70*TAB52P(JZ)
      TAXITOLL=0.0
      IF(TAB142P(JZ).GT.0.01) TAXITOLL=TAB132P(JZ)
      UTILTAXI= COEFF(11)* TAB42P(JZ)
     *         +COEFF(12)* TXWAIT
     *         +COEFF(60+C)* (TAXICOST*100.0) 
     *         +COEFF(60+C)*  TAXITOLL
     *         +KTAXI(INDEX)
      UTILTAXI=UTILTAXI/LSUM1AUTO
      ELSE
      UTILTAXI=0.0
      TAXICOST=0.0
      TAXITOLL=0.0
      END IF
C.....UBER UTILITY
      IF(UBERMODE) THEN
      IF(UBERAV(AV).EQ.0) THEN
       CALL UBERCOMP(IZ,TAB42P(JZ),TAB52P(JZ),COEFF(12),COEFF(11),
     *              COSTUBER,ACCUBER,KWAIT,ZHHD)
      ELSE
      CALL UBERCOMP(IZ,AVH2TIME(JZ),TAB52P(JZ),COEFF(12),
     *              COEFF(79),COSTUBER,ACCUBER,KWAIT,ZHHD)       
      END IF
      UTILUBER=ACCUBER + COEFF(60+C)* (COSTUBER*100.0)
     *        +KUBERMODE(C)
      UTILUBER=UTILUBER/LSUM1AUTO
      ELSE
      UTILUBER=0.0
      COSTUBER=0.0
      END IF
C.................................................................
      IF(DEBUG) THEN
      WRITE(26,9025) IZ,JZ,TAB1DA(JZ),TAB2DA(JZ),TAB3DA(JZ),
     *               TAB4DA(JZ),TAB5DA(JZ),TAB6DA(JZ),TAB7DA(JZ),
     *               TAB8DA(JZ),YTOLDS0,SAV0T,SAVAL0,
     *               TAB12P(JZ),TAB22P(JZ),TAB32P(JZ),
     *               TAB72P(JZ),
     *               TAB42P(JZ),TAB52P(JZ),TAB62P(JZ),DSCT2P,
     *               TAB82P(JZ),YTOLDS2,TAB92P(JZ),
     *               TAB102P(JZ),TAB112P(JZ),TAB122P(JZ),
     *               TAB132P(JZ),TAB142P(JZ),TAB152P(JZ),
     *               SAV2T,SAVAL2,
     *               TAB13P(JZ),TAB23P(JZ),TAB33P(JZ),
     *               TAB73P(JZ),
     *               TAB43P(JZ),TAB53P(JZ),TAB63P(JZ),DSCT3P,
     *               TAB83P(JZ),YTOLDS3,TAB93P(JZ),
     *               TAB103P(JZ),TAB113P(JZ),TAB123P(JZ),
     *               TAB133P(JZ),TAB143P(JZ),TAB153P(JZ),
     *               SAV3T,SAVAL3,     
     *               TAB14P(JZ),TAB24P(JZ),TAB34P(JZ),
     *               TAB74P(JZ),
     *               TAB44P(JZ),TAB54P(JZ),TAB64P(JZ),DSCT4P,
     *               TAB84P(JZ),YTOLDS4,TAB94P(JZ),
     *               TAB104P(JZ),TAB114P(JZ),TAB124P(JZ),
     *               TAB134P(JZ),TAB144P(JZ),TAB154P(JZ),
     *               SAV4T,SAVAL4,  
     *               COSTPARK,ZHHD(7,IZ),ZHHD(7,JZ),
     *               UTIL0NT,UTIL0T,SUTIL0,
     *               UTIL2NT,UTIL2NTH,UTIL2T,SUTIL2,UTIL2TH,
     *               UTIL3NT,UTIL3NTH,UTIL3T,SUTIL3,UTIL3TH,
     *               UTIL4NT,UTIL4NTH,UTIL4T,SUTIL4,UTIL4TH
      IF(AVMDL) THEN
      WRITE(26,9036) AVHTIME(JZ),AVTTIME(JZ),
     *               AVH2TIME(JZ),AVT2TIME(JZ),
     *               AVH3TIME(JZ),AVT3TIME(JZ)
      END IF
      IF(UBERMODE.OR.TAXIMODE) THEN                                            !Innovation
      WRITE(26,9033) ZHHD(29,IZ),ZINDEX,
     *               TAB42P(JZ),TXWAIT,TAXICOST,TAXITOLL,
     *               TAB132P(JZ),UTILTAXI,
     *               TAB42P(JZ),TAB52P(JZ),KWAIT,COSTUBER,
     *               UTILUBER
      END IF
 9025 FORMAT(/1X,'HIGHWAY MODE UTILITY COMPUTATIONS'/
     *       1X,'---------------------------------------'/
     *       1X,'ORIGIN ZONE                =',I10/
     *       1X,'DESTINATION ZONE           =',I10//
     *       1X,'DA AUTO TIME-    NON TOLL  =',F10.2/
     *       1X,'DA AUTO DISTANCE-NON TOLL  =',F10.2/
     *       1X,'DA AUTO COST-    NON TOLL  =',F10.2//
     *       1X,'DA AUTO TIME-        TOLL  =',F10.2/
     *       1X,'DA AUTO DISTANCE-    TOLL  =',F10.2/
     *       1X,'DA AUTO COST-        TOLL  =',F10.2/
     *       1X,'DA TOLL DISTANCE -   TOLL  =',F10.2/
     *       1X,'DA TOLL FACILITY INDICATOR =',I10/
     *       1X,'DA TOLL DISTANCE CONSTANT  =',F10.2/
     *       1X,'DA TOLL TIME SAVINGS/MILE  =',F10.2/
     *       1X,'DA TOLL TIME SAVINGS VALUE =',F10.2//
     *       1X,'2P AUTO TIME-        HOV   =',F10.2/
     *       1X,'2P AUTO DISTANCE-    HOV   =',F10.2/
     *       1X,'2P AUTO COST-        HOV   =',F10.2/
     *       1X,'2P HOV  LANE DISTANCE      =',F10.2//
     *       1X,'2P AUTO TIME-    TOLL/HOV  =',F10.2/
     *       1X,'2P AUTO DISTANCE-TOLL/HOV  =',F10.2/
     *       1X,'2P AUTO COST-    TOLL/HOV  =',F10.2/
     *       1X,'2P TOLL DISCOUNT   FACTOR  =',F10.4/
     *       1X,'2P TOLL DISTANCE -   TOLL  =',F10.2/
     *       1X,'2P TOLL DISTANCE CONSTANT  =',F10.2/
     *       1X,'2P HOV  LANE DISTANCE      =',F10.2/
     *       1X,'2P TOLL FACILITY INDICATOR =',I10//
     *       1X,'2P AUTO TIME         TOLL  =',F10.2/
     *       1X,'2P AUTO DISTANCE     TOLL  =',F10.2/
     *       1X,'2P AUTO COST         TOLL  =',F10.2/
     *       1X,'2P TOLL DISTANCE     TOLL  =',F10.2/
     *       1X,'2P TOLL FACILITY INDICATOR =',I10/
     *       1X,'2P TOLL TIME SAVINGS/MILE  =',F10.2/
     *       1X,'2P TOLL TIME SAVINGS VALUE =',F10.2//
     *       1X,'3P AUTO TIME-        HOV   =',F10.2/
     *       1X,'3P AUTO DISTANCE-    HOV   =',F10.2/
     *       1X,'3P AUTO COST-        HOV   =',F10.2/
     *       1X,'3P HOV  LANE DISTANCE      =',F10.2//
     *       1X,'3P AUTO TIME-    TOLL/HOV  =',F10.2/
     *       1X,'3P AUTO DISTANCE-TOLL/HOV  =',F10.2/
     *       1X,'3P AUTO COST-    TOLL/HOV  =',F10.2/
     *       1X,'3P TOLL DISCOUNT   FACTOR  =',F10.4/
     *       1X,'3P TOLL DISTANCE -   TOLL  =',F10.2/
     *       1X,'3P TOLL DISTANCE CONSTANT  =',F10.2/
     *       1X,'3P HOV  LANE DISTANCE      =',F10.2/
     *       1X,'3P TOLL FACILITY INDICATOR =',I10//
     *       1X,'3P AUTO TIME         TOLL  =',F10.2/
     *       1X,'3P AUTO DISTANCE     TOLL  =',F10.2/
     *       1X,'3P AUTO COST         TOLL  =',F10.2/
     *       1X,'3P TOLL DISTANCE     TOLL  =',F10.2/
     *       1X,'3P TOLL FACILITY INDICATOR =',I10/
     *       1X,'3P TOLL TIME SAVINGS/MILE  =',F10.2/
     *       1X,'3P TOLL TIME SAVINGS VALUE =',F10.2//
     *       1X,'4P AUTO TIME-        HOV   =',F10.2/
     *       1X,'4P AUTO DISTANCE-    HOV   =',F10.2/
     *       1X,'4P AUTO COST-        HOV   =',F10.2/
     *       1X,'4P HOV  LANE DISTANCE      =',F10.2//
     *       1X,'4P AUTO TIME-    TOLL/HOV  =',F10.2/
     *       1X,'4P AUTO DISTANCE-TOLL/HOV  =',F10.2/
     *       1X,'4P AUTO COST-    TOLL/HOV  =',F10.2/
     *       1X,'4P TOLL DISCOUNT   FACTOR  =',F10.4/
     *       1X,'4P TOLL DISTANCE -   TOLL  =',F10.2/
     *       1X,'4P TOLL DISTANCE CONSTANT  =',F10.2/
     *       1X,'4P HOV  LANE DISTANCE      =',F10.2/
     *       1X,'4P TOLL FACILITY INDICATOR =',I10//
     *       1X,'4P AUTO TIME         TOLL  =',F10.2/
     *       1X,'4P AUTO DISTANCE     TOLL  =',F10.2/
     *       1X,'4P AUTO COST         TOLL  =',F10.2/
     *       1X,'4P TOLL DISTANCE     TOLL  =',F10.2/
     *       1X,'4P TOLL FACILITY INDICATOR =',I10/
     *       1X,'4P TOLL TIME SAVINGS/MILE  =',F10.2/
     *       1X,'4P TOLL TIME SAVINGS VALUE =',F10.2//
     *       1X,'PARKING COST               =',F10.2/
     *       1X,'ORIGIN TERMINAL TIME       =',F10.2/
     *       1X,'DEST.  TERMINAL TIME       =',F10.2//
     *       1X,'DRIVE ALONE NON-TOLL UTIL  =',F10.5/
     *       1X,'DRIVE ALONE TOLL UTILITY   =',F10.5/
     *       1X,'DRIVE ALONE TOLL MIN SAV   =',F10.5//
     *       1X,'2 PERSON NON-TOLL UTILITY  =',F10.5/
     *       1X,'2 PERSON NON-TOLL/HOV UTIL =',F10.5/
     *       1X,'2 PERSON TOLL UTILITY      =',F10.5/
     *       1X,'2 PERSON TOLL MIN SAV      =',F10.5/
     *       1X,'2 PERSON TOLL/HOV UTILITY  =',F10.5//
     *       1X,'3 PERSON NON-TOLL UTILITY  =',F10.5/
     *       1X,'3 PERSON NON-TOLL/HOV UTIL =',F10.5/
     *       1X,'3 PERSON TOLL UTILITY      =',F10.5/
     *       1X,'3 PERSON TOLL MIN SAV      =',F10.5/
     *       1X,'3 PERSON TOLL/HOV UTILITY  =',F10.5//
     *       1X,'4+ PERSON NON-TOLL UTILITY =',F10.5/
     *       1X,'4+ PERSON NON-TOLL/HOV UTIL=',F10.5/
     *       1X,'4+ PERSON TOLL UTILITY     =',F10.5/
     *       1X,'4+ PERSON TOLL MIN SAV     =',F10.5/
     *       1X,'4+ PERSON TOLL/HOV UTIL    =',F10.5/)
 9036 FORMAT(1X,'AUTOMATED VEHICLE VALUES'/
     *       1X,'------------------------'/
     *       1X,'DA AV AUTO TIME- NON TOLL  =',F10.2/
     *       1X,'DA AV AUTO TIME-     TOLL  =',F10.2/
     *       1X,'2P AV AUTO TIME- NON TOLL  =',F10.2/
     *       1X,'2P AV AUTO TIME-     TOLL  =',F10.2/
     *       1X,'3P AV AUTO TIME- NON TOLL  =',F10.2/
     *       1X,'3P AV AUTO TIME-     TOLL  =',F10.2/)
 9033 FORMAT(1X,'TAXI & UBER MODE VALUES'/
     *       1X,'-----------------------'/
     *       1X,'ZONAL FLOATING DENSITY     =',F10.5/
     *       1X,'WAIT TIME CATEGORY         =',I10/
     *       1X,'TAXI AUTO TIME             =',F10.2/
     *       1X,'TAXI WAIT TIME             =',F10.2/
     *       1X,'TAXI COST (DOLLARS)        =',F10.2/
     *       1X,'TAXI TOLL COST             =',F10.2/
     *       1X,'TAXI TOLL LANE DISTANCE    =',F10.2/
     *       1X,'TAXI UTILITY               =',F10.5//
     *       1X,'UBER HIGHWAY TIME          =',F10.2/
     *       1X,'UBER HIGHWAY DISTANCE      =',F10.2/
     *       1X,'UBER WAIT TIME             =',F10.2/
     *       1X,'UBER COST (DOLLARS)        =',F10.2/
     *       1X,'UBER UTILITY               =',F10.5/)
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
      TNMSC=0.0
      TDRV0=0.0
      TDSHR=0.0
      TAXIMAAS=0.0
      TUBER=0.0
      TCR=0.0
      TUR=0.0
      TWAY=0.0
      TEXP =0.0
      TLOC =0.0
      TRPD =0.0
      TBRT=0.0
      TFLY=0.0
      TDRV2=0.0
      TDRV3=0.0
      TDRV4=0.0
      TLOCW=0.0
      TLOCD=0.0
      TLOCB=0.0
      TRPDW=0.0
      TRPDB=0.0
      TRPDD=0.0
      TEXPW=0.0
      TEXPB=0.0
      TEXPD=0.0
      TCRW=0.0
      TCRB=0.0
      TCRBK=0.0
      TCRP=0.0
      TCRK=0.0
      TCRU=0.0
      TCRW1=0.0
      TCRW2=0.0
      TCRBK1=0.0
      TCRBK2=0.0
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
      TURBK=0.0
      TURP=0.0
      TURK=0.0
      TURU=0.0
      TURW1=0.0
      TURW2=0.0
      TURBK1=0.0
      TURBK2=0.0
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
      TBRTW=0.0
      TBRTB=0.0
      TBRTBK=0.0
      TBRTP=0.0
      TBRTK=0.0
      TBRTU=0.0
      TBRTW1=0.0
      TBRTW2=0.0
      TBRTBK1=0.0
      TBRTBK2=0.0
      TBRTB1=0.0
      TBRTB2=0.0
      TBRTP1=0.0
      TBRTP2=0.0
      TBRTP3=0.0
      TBRTP4=0.0
      TBRTK1=0.0
      TBRTK2=0.0
      TBRTK3=0.0
      TBRTK4=0.0
      TWAYW=0.0
      TWAYB=0.0
      TWAYD=0.0
      TCRU1=0.0
      TCRU2=0.0
      TCRU3=0.0
      TCRU4=0.0
      TURU1=0.0
      TURU2=0.0
      TURU3=0.0
      TURU4=0.0
      TBRTU1=0.0
      TBRTU2=0.0
      TBRTU3=0.0
      TBRTU4=0.0
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
      TDROP=0.0
      TLIMO=0.0
      TRNTL=0.0
      TTAXI=0.0
      TONCL=0.0
C
C  PERSON TRIPS BY WALK ACCESS SEGMENT
C
      PERTRP=PERIN(C,JZ)*MWALK(M)*ZHHD(AVCV,IZ)
      if(pertrp.lt.0.001.AND.(.NOT.DEBUG).AND.
     *  (.NOT.LAXTRN).AND.(.NOT.EVENTSP)) GOTO 2000
      UTIL=0.0
      EUTIL=0.0
      IF(M.EQ.7) GO TO 225
      BWALK=0.0
      BBIKE=0.0
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
  210 CONTINUE
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9006) FF,C,M,PERTRP,BWALK1,BWALK2,DBWLK,RWALK1,RWALK2,
     *                DRWLK,CWALK1,CWALK2,DWALK,
     *                TWWALK1,TWWALK2,TDWALK
 9006 FORMAT(A1,1X,'MARKET SEGMENT=',I2,' ACCESS SEGMENT=',I2,
     *            ' PERTRP=',F12.5//
     *       1X,'---------------------------------------------'//
     *       1X,'SUMMARY OF CONSTRAINED WALK TIMES'/
     *       1X,'---------------------------------'/
     *       1X,'WALK  ACCESS  --> LOCAL BUS =',F8.2/
     *       1X,'WALK  EGRESS  --> LOCAL BUS =',F8.2/
     *       1X,'DRIVE EGRESS  --> LOCAL BUS =',F8.2/
     *       1X,'WALK  ACCESS  --> RAPID BUS =',F8.2/
     *       1X,'WALK  EGRESS  --> RAPID BUS =',F8.2/
     *       1X,'DRIVE EGRESS  --> RAPID BUS =',F8.2/
     *       1X,'WALK  ACCESS  --> EXP BUS   =',F8.2/
     *       1X,'WALK  EGRESS  --> EXP BUS   =',F8.2/
     *       1X,'DRIVE EGRESS  --> EXP BUS   =',F8.2/
     *       1X,'WALK  ACCESS  --> TRANSITWAY=',F8.2/
     *       1X,'WALK  EGRESS  --> TRANSITWAY=',F8.2/
     *       1X,'DRIVE EGRESS  --> TRANSITWAY=',F8.2)
      END IF
C....................................................................
C
C  COMPUTE STATION LEVEL UTILITIES FOR COMMUTER RAIL & URBAN RAIL
C  -------------------------------------------------------------
C  COMMUTER RAIL
C  -------------------------------------------------------------
      DO 301,ICH=1,2
C..WALK STATION #1/2
      IST=ICH
      EGRWALK=STAZNE(3,(ASTA(1,ICH)-MAX_IZONES),JZ)
      IF(CHWIND(1,ICH)) THEN
      EGRWALK=STAZNE(3,(WDSTA(6,IST)-MAX_IZONES),JZ)
      END IF
      IF(TRNEGR) EGRWALK=0.0
      IF((WUTIL(1,ICH).NE.0.0).AND.(M.LT.5)) THEN
C            1    2   3   4
      GO TO (371,372,373,374),M
 371  WLKM1=AMIN1(MWALKW(1,IST),SWALK)
      WLKM2=AMIN1(EGRWALK,SWALK)
      WLKM=WLKM1+WLKM2
      GOTO 375
 372  WLKM1=AMIN1(MWALKW(1,IST),SWALK)
      WLKM2=AMIN1(EGRWALK,LWALK)
      WLKM=WLKM1+WLKM2
      GOTO 375
 373  WLKM1=AMIN1(MWALKW(1,IST),LWALK)
      WLKM2=AMIN1(EGRWALK,SWALK)
      WLKM=WLKM1+WLKM2
      GOTO 375
 374  WLKM1=AMIN1(MWALKW(1,IST),LWALK)
      WLKM2=AMIN1(EGRWALK,LWALK)
      WLKM=WLKM1+WLKM2
 375  CONTINUE
C
       SC=OSTA(1,ICH)-MAX_IZONES
       SC2=ASTA(1,ICH)-MAX_IZONES
       IF(CHWIND(1,ICH)) SC2=WDSTA(6,ICH)-MAX_IZONES
       IF(SC.LT.0) SC=MAX_STATIONS
       IF(SC2.LT.0) SC2=MAX_STATIONS
       UTIL(ICH)=WUTIL(1,ICH) + COEFF(7)*WLKM +
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI
       IF(AIRPASS.AND.(.NOT.LAXTRN).AND.APMIND) THEN
       UTIL(ICH)=UTIL(ICH)-
     *  COEFF(10)*FLOAT(INDAPM(ASTA(1,ICH),JZ))*ITFSAVAPM
       END IF
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
      EGRWALK=STAZNE(3,(ASTA(1,ICH)-MAX_IZONES),JZ)
      IF(CHBIND(1,IST)) THEN
      EGRWALK=STAZNE(3,(BDSTA(6,IST)-MAX_IZONES),JZ)
      END IF
      IF(TRNEGR) EGRWALK=0.0
      IF((BUTIL(1,IST).NE.0.0).AND.(M.LT.5)) THEN
C            1    2   3   4
      GO TO (381,382,383,384),M
 381  WLKM1=AMIN1(MWALKB(1,IST),SWALK)
      WLKM2=AMIN1(EGRWALK,SWALK)
      WLKM=WLKM1+WLKM2
      GOTO 385
 382  WLKM1=AMIN1(MWALKB(1,IST),SWALK)
      WLKM2=AMIN1(EGRWALK,LWALK)
      WLKM=WLKM1+WLKM2
      GOTO 385
 383  WLKM1=AMIN1(MWALKB(1,IST),LWALK)
      WLKM2=AMIN1(EGRWALK,SWALK)
      WLKM=WLKM1+WLKM2
      GOTO 385
 384  WLKM1=AMIN1(MWALKB(1,IST),LWALK)
      WLKM2=AMIN1(EGRWALK,LWALK)
      WLKM=WLKM1+WLKM2
 385  CONTINUE
C
      SC=OSTA(1,ICH)-MAX_IZONES
      SC2=ASTA(1,ICH)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      IF(CHBIND(1,IST)) SC2=BDSTA(6,IST)-MAX_IZONES
      UTIL(ICH)=BUTIL(1,IST) + COEFF(7)*WLKM +
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ)+
     *                CRBSKIM(IST,6))*FCPI
       IF(AIRPASS.AND.(.NOT.LAXTRN).AND.APMIND) THEN
       UTIL(ICH)=UTIL(ICH)-
     *  COEFF(10)*FLOAT(INDAPM(ASTA(1,ICH),JZ))*ITFSAVAPM
       END IF
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
 303  CONTINUE
C
C..P&R STATION #1-4
C
      DO 305,ICH=5,8
      IST=ICH-4
      EGRWALK=STAZNE(3,(ASTA(1,ICH)-MAX_IZONES),JZ)
      IF(CHPIND(1,IST)) THEN
      EGRWALK=STAZNE(3,(PDSTA(6,IST)-MAX_IZONES),JZ)
      END IF
      IF(TRNEGR) EGRWALK=0.0
      IF(M.EQ.1.OR.M.EQ.3.OR.M.EQ.5) THEN
      WLKM=AMIN1(EGRWALK,SWALK)
      ELSEIF(M.EQ.2.OR.M.EQ.4.OR.M.EQ.6) THEN
      WLKM=AMIN1(EGRWALK,LWALK)
      ENDIF
      IF(PUTIL(1,IST).NE.0.0) THEN
      SC=OSTA(1,ICH)-MAX_IZONES
      SC2=ASTA(1,ICH)-MAX_IZONES
      IF(CHPIND(1,IST)) SC2=PDSTA(6,IST)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      UTIL(ICH)=PUTIL(1,IST) + COEFF(7)*WLKM+
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI+
     *          (KDTRN(C)+KPNR(C))/(LSUM1TRN*LSUM2CR)
       IF(AIRPASS.AND.(.NOT.LAXTRN).AND.APMIND) THEN
       UTIL(ICH)=UTIL(ICH)-
     *  COEFF(10)*FLOAT(INDAPM(ASTA(1,ICH),JZ))*ITFSAVAPM
       END IF
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
 305  CONTINUE
C
C..K&R STATION #1-4
      DO 307,ICH=9,12
      IST=ICH-8
      EGRWALK=STAZNE(3,(ASTA(1,ICH)-MAX_IZONES),JZ)
      IF(CHKIND(1,IST)) THEN
      EGRWALK=STAZNE(3,(KDSTA(6,IST)-MAX_IZONES),JZ)
      END IF
      IF(TRNEGR) EGRWALK=0.0
      IF(M.EQ.1.OR.M.EQ.3.OR.M.EQ.5) THEN
      WLKM=AMIN1(EGRWALK,SWALK)
      ELSEIF(M.EQ.2.OR.M.EQ.4.OR.M.EQ.6)THEN
      WLKM=AMIN1(EGRWALK,LWALK)
      ENDIF
      IF(KUTIL(1,IST).NE.0.0) THEN
      SC=OSTA(1,ICH)-MAX_IZONES
      SC2=ASTA(1,ICH)-MAX_IZONES
      IF(CHKIND(1,IST)) SC2=KDSTA(6,IST)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      UTIL(ICH)=KUTIL(1,IST) + COEFF(7)*WLKM+
     *    KDTRN(C)/(LSUM1TRN*LSUM2CR)+
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI
       IF(AIRPASS.AND.(.NOT.LAXTRN).AND.APMIND) THEN
       UTIL(ICH)=UTIL(ICH)-
     *  COEFF(10)*FLOAT(INDAPM(ASTA(1,ICH),JZ))*ITFSAVAPM
       END IF
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
 307  CONTINUE
C
C..BIKE STATION #1/2
      DO 308,ICH=1,2
      IST=ICH+12
      IUT=ICH+94
      EGRWALK=STAZNE(3,(ASTA(1,IST)-MAX_IZONES),JZ)
      IF(CHBKIND(1,ICH)) THEN
      EGRWALK=STAZNE(3,(BIKDSTA(6,ICH)-MAX_IZONES),JZ)
      END IF
      IF(TRNEGR) EGRWALK=0.0
      IF((BIKUTIL(1,ICH).NE.0.0).AND.(M.LT.5)) THEN
C            1    2   3   4
      GO TO (391,392,393,394),M
 391  WLKM1=AMIN1(MWALKBIK(1,ICH),SWALK)
      WLKM2=AMIN1(EGRWALK,SWALK)
      WLKM=WLKM1+WLKM2
      GOTO 395
 392  WLKM1=AMIN1(MWALKBIK(1,ICH),SWALK)
      WLKM2=AMIN1(EGRWALK,LWALK)
      WLKM=WLKM1+WLKM2
      GOTO 395
 393  WLKM1=AMIN1(MWALKBIK(1,ICH),LWALK)
      WLKM2=AMIN1(EGRWALK,SWALK)
      WLKM=WLKM1+WLKM2
      GOTO 395
 394  WLKM1=AMIN1(MWALKBIK(1,ICH),LWALK)
      WLKM2=AMIN1(EGRWALK,LWALK)
      WLKM=WLKM1+WLKM2
 395  CONTINUE
C
       SC=OSTA(1,IST)-MAX_IZONES
       SC2=ASTA(1,IST)-MAX_IZONES
       IF(CHBKIND(1,ICH)) SC2=BIKDSTA(6,ICH)-MAX_IZONES
       IF(SC.LT.0) SC=MAX_STATIONS
       IF(SC2.LT.0) SC2=MAX_STATIONS
       UTIL(IUT)=BIKUTIL(1,ICH) + COEFF(7)*WLKM2 +
     *   BSTCOEF(1)*MWALKBIK(1,ICH) +
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI +
     *   UTLSBIK +
     *   BSTCOEF(15)*STADATA(SC,15) + BSTCOEF(16)*STADATA(SC,16) +
     *   BSTCOEF(17)*STADATA(SC,17) + BSTCOEF(18)*STADATA(SC,18) +
     *   BSTCOEF(19)*STADATA(SC,19) +
     *   BSTCOEF(20)*BSV*ZHHD(28,IZ)*STADATA(SC,20) +
     *   BSTCOEF(21)*BSV*ZHHD(28,JZ)*STADATA(SC2,21)
       IF(AIRPASS.AND.(.NOT.LAXTRN).AND.APMIND) THEN
       UTIL(IUT)=UTIL(IUT)-
     *  COEFF(10)*FLOAT(INDAPM(ASTA(1,IST),JZ))*ITFSAVAPM
       END IF
C
       IF(UTIL(IUT).LE.(-100.0)) THEN
       EUTIL(IUT)=0.0
       ELSE
       EUTIL(IUT)=EXP(UTIL(IUT))
       END IF
      END IF
 308  CONTINUE
C
C..UBER STATION #1-4
      DO ICH=9,12
      IST=ICH-8
      IUT=IST+103
      EGRWALK=STAZNE(3,(ASTA(1,ICH)-MAX_IZONES),JZ)
      IF(CHKIND(1,IST)) THEN
      EGRWALK=STAZNE(3,(KDSTA(6,IST)-MAX_IZONES),JZ)
      END IF
      IF(TRNEGR) EGRWALK=0.0
      IF(M.EQ.1.OR.M.EQ.3.OR.M.EQ.5) THEN
      WLKM=AMIN1(EGRWALK,SWALK)
      ELSEIF(M.EQ.2.OR.M.EQ.4.OR.M.EQ.6)THEN
      WLKM=AMIN1(EGRWALK,LWALK)
      ENDIF
      IF(UBERUTIL(1,IST).NE.0.0) THEN
      SC=OSTA(1,ICH)-MAX_IZONES
      SC2=ASTA(1,ICH)-MAX_IZONES
      IF(CHKIND(1,IST)) SC2=KDSTA(6,IST)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      UTIL(IUT)=UBERUTIL(1,IST) + COEFF(7)*WLKM+
     *    KDTRN(C)/(LSUM1TRN*LSUM2CR)+
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI +
     *   COEFF(50+C)*(UBERCOST(1,IST)/(LSUM1TRN*LSUM2CR))
      EUTIL(IUT)=EXP(UTIL(IUT))
      END IF
      END DO
C....................................................................
      IF(DEBUG) THEN
       SC=OSTA(1,13)-MAX_IZONES
       SC2=OSTA(1,14)-MAX_IZONES
      WRITE(26,9005) (UTIL(K),EUTIL(K),K=1,12),
     *               (UTIL(K1),EUTIL(K1),K1=95,96),
     *               (UTIL(K2),EUTIL(K2),K2=104,107),
     *               MWALKBIK(1,1),MWALKBIK(1,2),UTLSBIK,
     *               OSTA(1,13),(STADATA(SC,K2),K2=15,20),
     *               ASTA(1,13),STADATA((ASTA(1,13)-MAX_IZONES),21),
     *               OSTA(1,14),(STADATA(SC2,K3),K3=15,20),
     *               ASTA(1,14),STADATA((ASTA(1,14)-MAX_IZONES),21)
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
     *       1X,'COMMUTER RAIL  K&R -->STA #4 ',F10.5,3X,E12.5//
     *       1X,'COMMUTER RAIL  BIKE-->STA #1 ',F10.5,3X,E12.5/
     *       1X,'COMMUTER RAIL  BIKE-->STA #2 ',F10.5,3X,E12.5//
     *       1X,'COMMUTER RAIL  UBER-->STA #1 ',F10.5,3X,E12.5/
     *       1X,'COMMUTER RAIL  UBER-->STA #2 ',F10.5,3X,E12.5/
     *       1X,'COMMUTER RAIL  UBER-->STA #3 ',F10.5,3X,E12.5/
     *       1X,'COMMUTER RAIL  UBER-->STA #4 ',F10.5,3X,E12.5//
     *       1X,'COMMUTER RAIL  BIKE-->STA #1 ACC LOGSUM=',F10.5/
     *       1X,'COMMUTER RAIL  BIKE-->STA #2 ACC LOGSUM=',F10.5/
     *       1X,'COMMUTER RAIL  BIKE ZONAL LEVEL UTIL   =',F10.5//
     *       1X,'COMMUTER RAIL  BIKE-->STA #1           =',I10/
     *       1X,'COMMUTER RAIL  BIKE-->STA #1 % RACKS   =',F10.1/
     *       1X,'COMMUTER RAIL  BIKE-->STA #1 % LOCKERS =',F10.1/
     *       1X,'COMMUTER RAIL  BIKE-->STA #1 % BICEBERG=',F10.1/
     *       1X,'COMMUTER RAIL  BIKE-->STA #1 % ROOMS   =',F10.1/
     *       1X,'COMMUTER RAIL  BIKE-->STA #1 % SHOWERS =',F10.1/
     *       1X,'COMMUTER RAIL  BIKE-->STA #1 SHARE STA =',F10.1/
     *       1X,'COMMUTER RAIL  BIKE-->STA #1 EGRESS STA=',I10/
     *       1X,'COMMUTER RAIL  BIKE-->STA #1 EGRESS SHR=',F10.1//
     *       1X,'COMMUTER RAIL  BIKE-->STA #2           =',I10/
     *       1X,'COMMUTER RAIL  BIKE-->STA #2 % RACKS   =',F10.1/
     *       1X,'COMMUTER RAIL  BIKE-->STA #2 % LOCKERS =',F10.1/
     *       1X,'COMMUTER RAIL  BIKE-->STA #2 % BICEBERG=',F10.1/
     *       1X,'COMMUTER RAIL  BIKE-->STA #2 % ROOMS   =',F10.1/
     *       1X,'COMMUTER RAIL  BIKE-->STA #2 % SHOWERS =',F10.1/
     *       1X,'COMMUTER RAIL  BIKE-->STA #2 SHARE STA =',F10.1/ 
     *       1X,'COMMUTER RAIL  BIKE-->STA #2 EGRESS STA=',I10/
     *       1X,'COMMUTER RAIL  BIKE-->STA #2 EGRESS SHR=',F10.1/)    
      END IF
C --------------------------------------------------------------
C  URBAN RAIL
C --------------------------------------------------------------
      DO 311,ICH=13,14
C..WALK STATION #1/2
      IST=ICH-12
      EGRWALK=STAZNE(3,(ASTA(2,IST)-MAX_IZONES),JZ)
      IF(CHWIND(2,IST)) THEN
      EGRWALK=STAZNE(3,(WDSTA(7,IST)-MAX_IZONES),JZ)
      END IF
      IF(TRNEGR) EGRWALK=0.0
      IF((WUTIL(2,IST).NE.0.0).AND.(M.LT.5)) THEN
C
C            1    2   3   4
      GO TO (571,572,573,574),M
 571  WLKM1=AMIN1(MWALKW(2,IST),SWALK)
      WLKM2=AMIN1(EGRWALK,SWALK)
      WLKM=WLKM1+WLKM2
      GOTO 575
 572  WLKM1=AMIN1(MWALKW(2,IST),SWALK)
      WLKM2=AMIN1(EGRWALK,LWALK)
      WLKM=WLKM1+WLKM2
      GOTO 575
 573  WLKM1=AMIN1(MWALKW(2,IST),LWALK)
      WLKM2=AMIN1(EGRWALK,SWALK)
      WLKM=WLKM1+WLKM2
      GOTO 575
 574  WLKM1=AMIN1(MWALKW(2,IST),LWALK)
      WLKM2=AMIN1(EGRWALK,LWALK)
      WLKM=WLKM1+WLKM2
 575  CONTINUE
      SC=OSTA(2,IST)-MAX_IZONES
      SC2=ASTA(2,IST)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      IF(CHWIND(2,IST)) SC2=WDSTA(7,IST)-MAX_IZONES
      UTIL(ICH)=WUTIL(2,IST) + COEFF(7)*WLKM + 
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI
       IF(AIRPASS.AND.(.NOT.LAXTRN).AND.APMIND) THEN
       UTIL(ICH)=UTIL(ICH)-
     *  COEFF(1)*FLOAT(INDAPM(OSTA(2,IST),ASTA(2,IST)))*ITFSAVAPM
       END IF
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
      EGRWALK=STAZNE(3,(ASTA(2,INT)-MAX_IZONES),JZ)
      IF(CHBIND(2,IST)) THEN
      EGRWALK=STAZNE(3,(BDSTA(7,IST)-MAX_IZONES),JZ)
      END IF
      IF(TRNEGR) EGRWALK=0.0
      IF((BUTIL(2,IST).NE.0.0).AND.(M.LT.5)) THEN
C
C            1    2   3   4
      GO TO (581,582,583,584),M
 581  WLKM1=AMIN1(MWALKB(2,IST),SWALK)
      WLKM2=AMIN1(EGRWALK,SWALK)
      WLKM=WLKM1+WLKM2
      GOTO 585
 582  WLKM1=AMIN1(MWALKB(2,IST),SWALK)
      WLKM2=AMIN1(EGRWALK,LWALK)
      WLKM=WLKM1+WLKM2
      GOTO 585
 583  WLKM1=AMIN1(MWALKB(2,IST),LWALK)
      WLKM2=AMIN1(EGRWALK,SWALK)
      WLKM=WLKM1+WLKM2
      GOTO 585
 584  WLKM1=AMIN1(MWALKB(2,IST),LWALK)
      WLKM2=AMIN1(EGRWALK,LWALK)
      WLKM=WLKM1+WLKM2
 585  CONTINUE
      SC=OSTA(2,INT)-MAX_IZONES
      SC2=ASTA(2,INT)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      IF(CHBIND(2,IST)) SC2=BDSTA(7,IST)-MAX_IZONES
      UTIL(ICH)=BUTIL(2,IST) + COEFF(7)*WLKM+
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ)+
     *                URBSKIM(IST,6))*FCPI
       IF(AIRPASS.AND.(.NOT.LAXTRN).AND.APMIND) THEN
       UTIL(ICH)=UTIL(ICH)-
     *  COEFF(1)*FLOAT(INDAPM(OSTA(2,INT),ASTA(2,INT)))*ITFSAVAPM
       END IF
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
 313  CONTINUE
C
C..P&R STATION #1-4
C
      DO 315,ICH=17,20
      IST=ICH-16
      INT=ICH-12
      EGRWALK=STAZNE(3,(ASTA(2,INT)-MAX_IZONES),JZ)
      IF(CHPIND(2,IST)) THEN
      EGRWALK=STAZNE(3,(PDSTA(7,IST)-MAX_IZONES),JZ)
      END IF
      IF(TRNEGR) EGRWALK=0.0
      IF(M.EQ.1.OR.M.EQ.3.OR.M.EQ.5) THEN
      WLKM=AMIN1(EGRWALK,SWALK)
      ELSEIF(M.EQ.2.OR.M.EQ.4.OR.M.EQ.6) THEN
      WLKM=AMIN1(EGRWALK,LWALK)
      ENDIF
      IF(PUTIL(2,IST).NE.0.0) THEN
      SC=OSTA(2,INT)-MAX_IZONES
      SC2=ASTA(2,INT)-MAX_IZONES
      IF(CHPIND(2,IST)) SC2=PDSTA(7,IST)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      UTIL(ICH)=PUTIL(2,IST) + COEFF(7)*WLKM+
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI+
     *          (KDTRN(C)+KPNR(C))/(LSUM1TRN*LSUM2UR)
       IF(AIRPASS.AND.(.NOT.LAXTRN).AND.APMIND) THEN
       UTIL(ICH)=UTIL(ICH)-
     *  COEFF(1)*FLOAT(INDAPM(OSTA(2,INT),ASTA(2,INT)))*ITFSAVAPM
       END IF
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
 315  CONTINUE
C
C..K&R STATION #1-4
      DO 317,ICH=21,24
      IST=ICH-20
      INT=ICH-12
      EGRWALK=STAZNE(3,(ASTA(2,INT)-MAX_IZONES),JZ)
      IF(CHKIND(2,IST)) THEN
      EGRWALK=STAZNE(3,(KDSTA(7,IST)-MAX_IZONES),JZ)
      END IF
      IF(TRNEGR) EGRWALK=0.0
      IF(M.EQ.1.OR.M.EQ.3.OR.M.EQ.5) THEN
      WLKM=AMIN1(EGRWALK,SWALK)
      ELSEIF(M.EQ.2.OR.M.EQ.4.OR.M.EQ.6) THEN
      WLKM=AMIN1(EGRWALK,LWALK)
      ENDIF
      IF(KUTIL(2,IST).NE.0.0) THEN
      SC=OSTA(2,INT)-MAX_IZONES
      SC2=ASTA(2,INT)-MAX_IZONES
      IF(CHKIND(2,IST)) SC2=KDSTA(7,IST)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      UTIL(ICH)=KUTIL(2,IST) + COEFF(7)*WLKM+
     *   KDTRN(C)/(LSUM1TRN*LSUM2UR)+
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI
       IF(AIRPASS.AND.(.NOT.LAXTRN).AND.APMIND) THEN
       UTIL(ICH)=UTIL(ICH)-
     *  COEFF(1)*FLOAT(INDAPM(OSTA(2,INT),ASTA(2,INT)))*ITFSAVAPM
       END IF
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
 317  CONTINUE
C
C..BIKE STATION #1/2
      DO 318,ICH=1,2
      IST=ICH+12
      IUT=ICH+96
      EGRWALK=STAZNE(3,(ASTA(2,IST)-MAX_IZONES),JZ)
      IF(CHBKIND(1,ICH)) THEN
      EGRWALK=STAZNE(3,(BIKDSTA(7,ICH)-MAX_IZONES),JZ)
      END IF
      IF(TRNEGR) EGRWALK=0.0
      IF((BIKUTIL(2,ICH).NE.0.0).AND.(M.LT.5)) THEN
C            1    2   3   4
      GO TO (591,592,593,594),M
 591  WLKM1=AMIN1(MWALKBIK(2,ICH),SWALK)
      WLKM2=AMIN1(EGRWALK,SWALK)
      WLKM=WLKM1+WLKM2
      GOTO 595
 592  WLKM1=AMIN1(MWALKBIK(2,ICH),SWALK)
      WLKM2=AMIN1(EGRWALK,LWALK)
      WLKM=WLKM1+WLKM2
      GOTO 595
 593  WLKM1=AMIN1(MWALKBIK(2,ICH),LWALK)
      WLKM2=AMIN1(EGRWALK,SWALK)
      WLKM=WLKM1+WLKM2
      GOTO 595
 594  WLKM1=AMIN1(MWALKBIK(2,ICH),LWALK)
      WLKM2=AMIN1(EGRWALK,LWALK)
      WLKM=WLKM1+WLKM2
 595  CONTINUE
C
       SC=OSTA(2,IST)-MAX_IZONES
       SC2=ASTA(2,IST)-MAX_IZONES
       IF(CHBKIND(2,ICH)) SC2=BIKDSTA(7,ICH)-MAX_IZONES
       IF(SC.LT.0) SC=MAX_STATIONS
       IF(SC2.LT.0) SC2=MAX_STATIONS
       UTIL(IUT)=BIKUTIL(2,ICH) + COEFF(7)*WLKM2 + 
     *   BSTCOEF(1)*MWALKBIK(2,ICH) +
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI + 
     *   UTLSBIK +
     *   BSTCOEF(15)*STADATA(SC,15) + BSTCOEF(16)*STADATA(SC,16) +
     *   BSTCOEF(17)*STADATA(SC,17) + BSTCOEF(18)*STADATA(SC,18) +
     *   BSTCOEF(19)*STADATA(SC,19) +
     *   BSTCOEF(20)*BSV*ZHHD(28,IZ)*STADATA(SC,20) +
     *   BSTCOEF(21)*BSV*ZHHD(28,JZ)*STADATA(SC2,21)
       IF(AIRPASS.AND.(.NOT.LAXTRN).AND.APMIND) THEN
       UTIL(IUT)=UTIL(IUT)-
     *  COEFF(10)*FLOAT(INDAPM(ASTA(2,IST),JZ))*ITFSAVAPM
       END IF
C
       IF(UTIL(IUT).LE.(-100.0)) THEN
       EUTIL(IUT)=0.0
       ELSE
       EUTIL(IUT)=EXP(UTIL(IUT))
       END IF
      END IF
 318  CONTINUE
C
C..UBER STATION #1-4
      DO ICH=21,24
      IST=ICH-20
      INT=ICH-12
      IUT=IST+107
      EGRWALK=STAZNE(3,(ASTA(2,INT)-MAX_IZONES),JZ)
      IF(CHKIND(2,IST)) THEN
      EGRWALK=STAZNE(3,(KDSTA(7,IST)-MAX_IZONES),JZ)
      END IF
      IF(TRNEGR) EGRWALK=0.0
      IF(M.EQ.1.OR.M.EQ.3.OR.M.EQ.5) THEN
      WLKM=AMIN1(EGRWALK,SWALK)
      ELSEIF(M.EQ.2.OR.M.EQ.4.OR.M.EQ.6) THEN
      WLKM=AMIN1(EGRWALK,LWALK)
      ENDIF
      IF(UBERUTIL(2,IST).NE.0.0) THEN
      SC=OSTA(2,INT)-MAX_IZONES
      SC2=ASTA(2,INT)-MAX_IZONES
      IF(CHKIND(2,IST)) SC2=KDSTA(7,IST)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      UTIL(IUT)=UBERUTIL(2,IST) + COEFF(7)*WLKM+
     *   KDTRN(C)/(LSUM1TRN*LSUM2UR)+
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI +
     *   COEFF(50+C)*(UBERCOST(2,IST)/(LSUM1TRN*LSUM2UR))
      EUTIL(IUT)=EXP(UTIL(IUT))
      END IF
      END DO
C....................................................................
      IF(DEBUG) THEN
       SC=OSTA(2,13)-MAX_IZONES
       SC2=OSTA(2,14)-MAX_IZONES
      WRITE(26,9007) (UTIL(K),EUTIL(K),K=13,24),
     *               (UTIL(K1),EUTIL(K1),K1=97,98),
     *               (UTIL(K2),EUTIL(K2),K2=108,111),
     *               MWALKBIK(2,1),MWALKBIK(2,2),UTLSBIK,
     *               OSTA(2,13),(STADATA(SC,K2),K2=15,20),
     *               ASTA(2,13),STADATA((ASTA(2,13)-MAX_IZONES),21),
     *               OSTA(2,14),(STADATA(SC2,K3),K3=15,20),
     *               ASTA(2,14),STADATA((ASTA(2,14)-MAX_IZONES),21)
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
     *       1X,'URBAN RAIL  K&R -->STA #4 ',F10.5,3X,E12.5//
     *       1X,'URBAN RAIL  BIKE-->STA #1 ',F10.5,3X,E12.5/
     *       1X,'URBAN RAIL  BIKE-->STA #2 ',F10.5,3X,E12.5//
     *       1X,'URBAN RAIL  UBER-->STA #1 ',F10.5,3X,E12.5/
     *       1X,'URBAN RAIL  UBER-->STA #2 ',F10.5,3X,E12.5/
     *       1X,'URBAN RAIL  UBER-->STA #3 ',F10.5,3X,E12.5/
     *       1X,'URBAN RAIL  UBER-->STA #4 ',F10.5,3X,E12.5//
     *       1X,'URBAN RAIL  BIKE-->STA #1 ACC LOGSUM=',F10.5/
     *       1X,'URBAN RAIL  BIKE-->STA #2 ACC LOGSUM=',F10.5/
     *       1X,'URBAN RAIL  BIKE ZONAL LEVEL UTIL   =',F10.5//
     *       1X,'URBAN RAIL  BIKE-->STA #1           =',I10/
     *       1X,'URBAN RAIL  BIKE-->STA #1 % RACKS   =',F10.1/
     *       1X,'URBAN RAIL  BIKE-->STA #1 % LOCKERS =',F10.1/
     *       1X,'URBAN RAIL  BIKE-->STA #1 % BICEBERG=',F10.1/
     *       1X,'URBAN RAIL  BIKE-->STA #1 % ROOMS   =',F10.1/
     *       1X,'URBAN RAIL  BIKE-->STA #1 % SHOWERS =',F10.1/
     *       1X,'URBAN RAIL  BIKE-->STA #1 SHARE STA =',F10.1/
     *       1X,'URBAN RAIL  BIKE-->STA #1 EGRESS STA=',I10/
     *       1X,'URBAN RAIL  BIKE-->STA #1 EGRESS SHR=',F10.1//
     *       1X,'URBAN RAIL  BIKE-->STA #2           =',I10/
     *       1X,'URBAN RAIL  BIKE-->STA #2 % RACKS   =',F10.1/
     *       1X,'URBAN RAIL  BIKE-->STA #2 % LOCKERS =',F10.1/
     *       1X,'URBAN RAIL  BIKE-->STA #2 % BICEBERG=',F10.1/
     *       1X,'URBAN RAIL  BIKE-->STA #2 % ROOMS   =',F10.1/
     *       1X,'URBAN RAIL  BIKE-->STA #2 % SHOWERS =',F10.1/
     *       1X,'URBAN RAIL  BIKE-->STA #2 SHARE STA =',F10.1/ 
     *       1X,'URBAN RAIL  BIKE-->STA #2 EGRESS STA=',I10/
     *       1X,'URBAN RAIL  BIKE-->STA #2 EGRESS SHR=',F10.1/)           
      END IF
C --------------------------------------------------------------
C  BRT
C --------------------------------------------------------------
      DO 312,ICH=25,26
C..WALK STATION #1/2
      IST=ICH-24
      EGRWALK=STAZNE(3,(ASTA(5,IST)-MAX_IZONES),JZ)
      IF(CHWIND(5,IST)) THEN
      EGRWALK=STAZNE(3,(WDSTA(10,IST)-MAX_IZONES),JZ)
      END IF
      IF(TRNEGR) EGRWALK=0.0
      IF((WUTIL(5,IST).NE.0.0).AND.(M.LT.5)) THEN
C
C            1    2   3   4
      GO TO (671,672,673,674),M
 671  WLKM1=AMIN1(MWALKW(2,IST),SWALK)
      WLKM2=AMIN1(EGRWALK,SWALK)
      WLKM=WLKM1+WLKM2
      GOTO 675
 672  WLKM1=AMIN1(MWALKW(2,IST),SWALK)
      WLKM2=AMIN1(EGRWALK,LWALK)
      WLKM=WLKM1+WLKM2
      GOTO 675
 673  WLKM1=AMIN1(MWALKW(2,IST),LWALK)
      WLKM2=AMIN1(EGRWALK,SWALK)
      WLKM=WLKM1+WLKM2
      GOTO 675
 674  WLKM1=AMIN1(MWALKW(2,IST),LWALK)
      WLKM2=AMIN1(EGRWALK,LWALK)
      WLKM=WLKM1+WLKM2
 675  CONTINUE
      SC=OSTA(5,IST)-MAX_IZONES
      SC2=ASTA(5,IST)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      IF(CHWIND(5,IST)) SC2=WDSTA(10,IST)-MAX_IZONES
      UTIL(ICH)=WUTIL(5,IST) + COEFF(7)*WLKM + 
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI
       IF(AIRPASS.AND.(.NOT.LAXTRN).AND.APMIND) THEN
       UTIL(ICH)=UTIL(ICH)-
     *  COEFF(1)*FLOAT(INDAPM(OSTA(2,IST),ASTA(2,IST)))*ITFSAVAPM
       END IF
       IF(UTIL(ICH).LE.(-100.0)) THEN
       EUTIL(ICH)=0.0
       ELSE
       EUTIL(ICH)=EXP(UTIL(ICH))
       END IF
      END IF
 312  CONTINUE
C
C
C..BUS STATION #1/2
      DO 333,ICH=27,28
      IST=ICH-26
      INT=ICH-24
      EGRWALK=STAZNE(3,(ASTA(5,INT)-MAX_IZONES),JZ)
      IF(CHBIND(5,IST)) THEN
      EGRWALK=STAZNE(3,(BDSTA(10,IST)-MAX_IZONES),JZ)
      END IF
      IF(TRNEGR) EGRWALK=0.0
      IF((BUTIL(5,IST).NE.0.0).AND.(M.LT.5)) THEN
C
C            1    2   3   4
      GO TO (681,682,683,684),M
 681  WLKM1=AMIN1(MWALKB(2,IST),SWALK)
      WLKM2=AMIN1(EGRWALK,SWALK)
      WLKM=WLKM1+WLKM2
      GOTO 685
 682  WLKM1=AMIN1(MWALKB(2,IST),SWALK)
      WLKM2=AMIN1(EGRWALK,LWALK)
      WLKM=WLKM1+WLKM2
      GOTO 685
 683  WLKM1=AMIN1(MWALKB(2,IST),LWALK)
      WLKM2=AMIN1(EGRWALK,SWALK)
      WLKM=WLKM1+WLKM2
      GOTO 685
 684  WLKM1=AMIN1(MWALKB(2,IST),LWALK)
      WLKM2=AMIN1(EGRWALK,LWALK)
      WLKM=WLKM1+WLKM2
 685  CONTINUE
      SC=OSTA(5,INT)-MAX_IZONES
      SC2=ASTA(5,INT)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      IF(CHBIND(5,IST)) SC2=BDSTA(10,IST)-MAX_IZONES
      UTIL(ICH)=BUTIL(5,IST) + COEFF(7)*WLKM+
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ)+
     *                BRTSKIM(IST,6))*FCPI
       IF(AIRPASS.AND.(.NOT.LAXTRN).AND.APMIND) THEN
       UTIL(ICH)=UTIL(ICH)-
     *  COEFF(1)*FLOAT(INDAPM(OSTA(2,INT),ASTA(2,INT)))*ITFSAVAPM
       END IF
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
 333  CONTINUE
C
C..P&R STATION #1-4
C
      DO 339,ICH=29,32
      IST=ICH-28
      INT=ICH-24
      EGRWALK=STAZNE(3,(ASTA(5,INT)-MAX_IZONES),JZ)
      IF(CHPIND(5,IST)) THEN
      EGRWALK=STAZNE(3,(PDSTA(10,IST)-MAX_IZONES),JZ)
      END IF
      IF(TRNEGR) EGRWALK=0.0
      IF(M.EQ.1.OR.M.EQ.3.OR.M.EQ.5) THEN
      WLKM=AMIN1(EGRWALK,SWALK)
      ELSEIF(M.EQ.2.OR.M.EQ.4.OR.M.EQ.6) THEN
      WLKM=AMIN1(EGRWALK,LWALK)
      ENDIF
      IF(PUTIL(5,IST).NE.0.0) THEN
      SC=OSTA(5,INT)-MAX_IZONES
      SC2=ASTA(5,INT)-MAX_IZONES
      IF(CHPIND(5,IST)) SC2=PDSTA(10,IST)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      UTIL(ICH)=PUTIL(5,IST) + COEFF(7)*WLKM+
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI+
     *          (KDTRN(C)+KPNR(C))/(LSUM1TRN*LSUM2BR)
       IF(AIRPASS.AND.(.NOT.LAXTRN).AND.APMIND) THEN
       UTIL(ICH)=UTIL(ICH)-
     *  COEFF(1)*FLOAT(INDAPM(OSTA(2,INT),ASTA(2,INT)))*ITFSAVAPM
       END IF
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
 339  CONTINUE
C
C..K&R STATION #1-4
      DO 337,ICH=33,36
      IST=ICH-32
      INT=ICH-24
      EGRWALK=STAZNE(3,(ASTA(5,INT)-MAX_IZONES),JZ)
      IF(CHKIND(5,IST)) THEN
      EGRWALK=STAZNE(3,(KDSTA(10,IST)-MAX_IZONES),JZ)
      END IF
      IF(TRNEGR) EGRWALK=0.0
      IF(M.EQ.1.OR.M.EQ.3.OR.M.EQ.5) THEN
      WLKM=AMIN1(EGRWALK,SWALK)
      ELSEIF(M.EQ.2.OR.M.EQ.4.OR.M.EQ.6) THEN
      WLKM=AMIN1(EGRWALK,LWALK)
      ENDIF
      IF(KUTIL(5,IST).NE.0.0) THEN
      SC=OSTA(5,INT)-MAX_IZONES
      SC2=ASTA(5,INT)-MAX_IZONES
      IF(CHKIND(5,IST)) SC2=KDSTA(10,IST)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      UTIL(ICH)=KUTIL(5,IST) + COEFF(7)*WLKM+
     *   KDTRN(C)/(LSUM1TRN*LSUM2BR)+
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI
       IF(AIRPASS.AND.(.NOT.LAXTRN).AND.APMIND) THEN
       UTIL(ICH)=UTIL(ICH)-
     *  COEFF(1)*FLOAT(INDAPM(OSTA(2,INT),ASTA(2,INT)))*ITFSAVAPM
       END IF
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
 337  CONTINUE
C
C..BIKE STATION #1/2
      DO 338,ICH=1,2
      IST=ICH+12
      IUT=ICH+78
      EGRWALK=STAZNE(3,(ASTA(5,IST)-MAX_IZONES),JZ)
      IF(CHBKIND(5,ICH)) THEN
      EGRWALK=STAZNE(3,(BIKDSTA(10,ICH)-MAX_IZONES),JZ)
      END IF
      IF(TRNEGR) EGRWALK=0.0
      IF((BIKUTIL(5,ICH).NE.0.0).AND.(M.LT.5)) THEN
C            1    2   3   4
      GO TO (691,692,693,694),M
 691  WLKM1=AMIN1(MWALKBIK(2,ICH),SWALK)
      WLKM2=AMIN1(EGRWALK,SWALK)
      WLKM=WLKM1+WLKM2
      GOTO 695
 692  WLKM1=AMIN1(MWALKBIK(2,ICH),SWALK)
      WLKM2=AMIN1(EGRWALK,LWALK)
      WLKM=WLKM1+WLKM2
      GOTO 695
 693  WLKM1=AMIN1(MWALKBIK(2,ICH),LWALK)
      WLKM2=AMIN1(EGRWALK,SWALK)
      WLKM=WLKM1+WLKM2
      GOTO 695
 694  WLKM1=AMIN1(MWALKBIK(2,ICH),LWALK)
      WLKM2=AMIN1(EGRWALK,LWALK)
      WLKM=WLKM1+WLKM2
 695  CONTINUE
C
       SC=OSTA(5,IST)-MAX_IZONES
       SC2=ASTA(5,IST)-MAX_IZONES
       IF(CHBKIND(5,ICH)) SC2=BIKDSTA(10,ICH)-MAX_IZONES
       IF(SC.LT.0) SC=MAX_STATIONS
       IF(SC2.LT.0) SC2=MAX_STATIONS
       UTIL(IUT)=BIKUTIL(5,ICH) + COEFF(7)*WLKM2 + 
     *   BSTCOEF(1)*MWALKBIK(5,ICH) +
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI + 
     *   UTLSBIK +
     *   BSTCOEF(15)*STADATA(SC,15) + BSTCOEF(16)*STADATA(SC,16) +
     *   BSTCOEF(17)*STADATA(SC,17) + BSTCOEF(18)*STADATA(SC,18) +
     *   BSTCOEF(19)*STADATA(SC,19) +
     *   BSTCOEF(20)*BSV*ZHHD(28,IZ)*STADATA(SC,20) +
     *   BSTCOEF(21)*BSV*ZHHD(28,JZ)*STADATA(SC2,21)
       IF(AIRPASS.AND.(.NOT.LAXTRN).AND.APMIND) THEN
       UTIL(IUT)=UTIL(IUT)-
     *  COEFF(10)*FLOAT(INDAPM(ASTA(2,IST),JZ))*ITFSAVAPM
       END IF
C
       IF(UTIL(IUT).LE.(-100.0)) THEN
       EUTIL(IUT)=0.0
       ELSE
       EUTIL(IUT)=EXP(UTIL(IUT))
       END IF
      END IF
 338  CONTINUE
C
C..UBER STATION #1-4
      DO ICH=33,36
      IST=ICH-32
      INT=ICH-24
      IUT=IST+111
      EGRWALK=STAZNE(3,(ASTA(5,INT)-MAX_IZONES),JZ)
      IF(CHKIND(5,IST)) THEN
      EGRWALK=STAZNE(3,(KDSTA(10,IST)-MAX_IZONES),JZ)
      END IF
      IF(TRNEGR) EGRWALK=0.0
      IF(M.EQ.1.OR.M.EQ.3.OR.M.EQ.5) THEN
      WLKM=AMIN1(EGRWALK,SWALK)
      ELSEIF(M.EQ.2.OR.M.EQ.4.OR.M.EQ.6) THEN
      WLKM=AMIN1(EGRWALK,LWALK)
      ENDIF
      IF(UBERUTIL(5,IST).NE.0.0) THEN
      SC=OSTA(5,INT)-MAX_IZONES
      SC2=ASTA(5,INT)-MAX_IZONES
      IF(CHKIND(5,IST)) SC2=KDSTA(10,IST)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      UTIL(IUT)=UBERUTIL(5,IST) + COEFF(7)*WLKM+
     *   KDTRN(C)/(LSUM1TRN*LSUM2BR)+
     *   COEFF(50+C)*(STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ))*FCPI +
     *   COEFF(50+C)*(UBERCOST(5,IST)/(LSUM1TRN*LSUM2BR))
      EUTIL(IUT)=EXP(UTIL(IUT))
      END IF
      END DO
C....................................................................
      IF(DEBUG) THEN
       SC=OSTA(5,13)-MAX_IZONES
       SC2=OSTA(5,14)-MAX_IZONES
      WRITE(26,9107) (UTIL(K),EUTIL(K),K=25,36),
     *               (UTIL(K1),EUTIL(K1),K1=79,80),
     *               (UTIL(K2),EUTIL(K2),K2=112,115),
     *               MWALKBIK(5,1),MWALKBIK(5,2),UTLSBIK,
     *               OSTA(5,13),(STADATA(SC,K2),K2=15,20),
     *               ASTA(5,13),STADATA((ASTA(5,13)-MAX_IZONES),21),
     *               OSTA(5,14),(STADATA(SC2,K3),K3=15,20),
     *               ASTA(5,14),STADATA((ASTA(5,14)-MAX_IZONES),21)
 9107 FORMAT(/1X,'SUMMARY OF BUS RAPID TRN UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',8X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',5X,'----------',5X,
     *          '----------'/
     *       1X,'BUS RPD TRN  WALK-->STA #1 ',F10.5,3X,E12.5/
     *       1X,'BUS RPD TRN  WALK-->STA #2 ',F10.5,3X,E12.5//
     *       1X,'BUS RPD TRN  BUS -->STA #1 ',F10.5,3X,E12.5/
     *       1X,'BUS RPD TRN  BUS -->STA #2 ',F10.5,3X,E12.5//
     *       1X,'BUS RPD TRN  P&R -->STA #1 ',F10.5,3X,E12.5/
     *       1X,'BUS RPD TRN  P&R -->STA #2 ',F10.5,3X,E12.5/
     *       1X,'BUS RPD TRN  P&R -->STA #3 ',F10.5,3X,E12.5/
     *       1X,'BUS RPD TRN  P&R -->STA #4 ',F10.5,3X,E12.5//
     *       1X,'BUS RPD TRN  K&R -->STA #1 ',F10.5,3X,E12.5/
     *       1X,'BUS RPD TRN  K&R -->STA #2 ',F10.5,3X,E12.5/
     *       1X,'BUS RPD TRN  K&R -->STA #3 ',F10.5,3X,E12.5/
     *       1X,'BUS RPD TRN  K&R -->STA #4 ',F10.5,3X,E12.5//
     *       1X,'BUS RPD TRN  BIKE-->STA #1 ',F10.5,3X,E12.5/
     *       1X,'BUS RPD TRN  BIKE-->STA #2 ',F10.5,3X,E12.5//
     *       1X,'BUS RPD TRN  UBER-->STA #1 ',F10.5,3X,E12.5/
     *       1X,'BUS RPD TRN  UBER-->STA #2 ',F10.5,3X,E12.5/
     *       1X,'BUS RPD TRN  UBER-->STA #3 ',F10.5,3X,E12.5/
     *       1X,'BUS RPD TRN  UBER-->STA #4 ',F10.5,3X,E12.5//
     *       1X,'BUS RPD TRN  BIKE-->STA #1 ACC LOGSUM=',F10.5/
     *       1X,'BUS RPD TRN  BIKE-->STA #2 ACC LOGSUM=',F10.5/
     *       1X,'BUS RPD TRN  BIKE ZONAL LEVEL UTIL   =',F10.5//
     *       1X,'BUS RPD TRN  BIKE-->STA #1           =',I10/
     *       1X,'BUS RPD TRN  BIKE-->STA #1 % RACKS   =',F10.1/
     *       1X,'BUS RPD TRN  BIKE-->STA #1 % LOCKERS =',F10.1/
     *       1X,'BUS RPD TRN  BIKE-->STA #1 % BICEBERG=',F10.1/
     *       1X,'BUS RPD TRN  BIKE-->STA #1 % ROOMS   =',F10.1/
     *       1X,'BUS RPD TRN  BIKE-->STA #1 % SHOWERS =',F10.1/
     *       1X,'BUS RPD TRN  BIKE-->STA #1 SHARE STA =',F10.1/
     *       1X,'BUS RPD TRN  BIKE-->STA #1 EGRESS STA=',I10/
     *       1X,'BUS RPD TRN  BIKE-->STA #1 EGRESS SHR=',F10.1//
     *       1X,'BUS RPD TRN  BIKE-->STA #2           =',I10/
     *       1X,'BUS RPD TRN  BIKE-->STA #2 % RACKS   =',F10.1/
     *       1X,'BUS RPD TRN  BIKE-->STA #2 % LOCKERS =',F10.1/
     *       1X,'BUS RPD TRN  BIKE-->STA #2 % BICEBERG=',F10.1/
     *       1X,'BUS RPD TRN  BIKE-->STA #2 % ROOMS   =',F10.1/
     *       1X,'BUS RPD TRN  BIKE-->STA #2 % SHOWERS =',F10.1/
     *       1X,'BUS RPD TRN  BIKE-->STA #2 SHARE STA =',F10.1/ 
     *       1X,'BUS RPD TRN  BIKE-->STA #2 EGRESS STA=',I10/
     *       1X,'BUS RPD TRN  BIKE-->STA #2 EGRESS SHR=',F10.1/)           
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
      UTIL(44)=WBUTL + (NMLSUM*BWALK  + 
     *         COEFF(50+C)*FARE(1,JZ)*FCPI   +
     *         KWBUS(C))/(LSUM2LB*LSUM1TRN)
      EUTIL(44)=EXP(UTIL(44))
      END IF
      END IF
C..DRIVE ACCESS TO LOCAL BUS
      IF(WBUTL.NE.0.0) THEN
      MODINC(2)=1
      FAREVAL(2)=FARE(1,JZ)*FCPI
      UTIL(45)=WBUTL + (NMLSUM*DBWLK + COEFF(18)*KDRIV +
     *         KDBUS(C)+KDTRN(C)+ KINFL(C) + 
     *         COEFF(33)*FLOAT(TXFER(1,JZ))  +
     *         COEFF(50+C)*FARE(1,JZ)*FCPI)/(LSUM1TRN*LSUM2LB)
      EUTIL(45)=EXP(UTIL(45))
      END IF
C..BIKE ACCESS TO LOCAL BUS
      IF(M.LT.5) THEN
      IF(BBUTL.NE.0.0) THEN
      MODINC(19)=1
      UTIL(90)=BBUTL + (NMLSUM*(BIKBACC+BIKBEGR)  + 
     *         COEFF(50+C)*FARE(6,JZ)*FCPI   +
     *         KBBUS(C))/(LSUM2LB*LSUM1TRN)
      EUTIL(90)=EXP(UTIL(90))
      END IF
      END IF
C..WALK ACCESS TO RAPID BUS
      IF(M.LT.5) THEN
      IF(WRUTL.NE.0.0) THEN
      MODINC(15)=1
      FAREVAL(3)=FARE(4,JZ)*FCPI
      UTIL(72)=WRUTL + (NMLSUM*RWALK  +
     *         COEFF(50+C)*FARE(4,JZ)*FCPI   +
     *         KWRPD(C))/(LSUM2RB*LSUM1TRN)
      IF(BIVT(4,JZ).GT.0.0) THEN
       UTIL(72)=UTIL(72)+KBRPDW
      END IF
      EUTIL(72)=EXP(UTIL(72))
      END IF
      END IF
C..BIKE ACCESS TO RAPID BUS
      IF(M.LT.5) THEN
      IF(BRUTL.NE.0.0) THEN
      MODINC(20)=1
      UTIL(91)=BRUTL + (NMLSUM*(BIKRACC+BIKREGR)  +
     *         COEFF(50+C)*FARE(7,JZ)*FCPI   +
     *         KBRPD(C))/(LSUM2RB*LSUM1TRN)
      IF(BIVT(7,JZ).GT.0.0) THEN
       UTIL(91)=UTIL(91)+KBRPDB
      END IF
      EUTIL(91)=EXP(UTIL(91))
      END IF
      END IF
C..DRIVE ACCESS TO RAPID BUS
      IF(WRUTL.NE.0.0) THEN
      MODINC(16)=1
      FAREVAL(4)=FARE(4,JZ)*FCPI
      UTIL(73)=WRUTL + (NMLSUM*DRWLK + COEFF(18)*(WLKRACC/10.0) +
     *          KDRPD(C)+KDTRN(C) +
     *          COEFF(36)*FLOAT(TXFER(4,JZ)) +
     *          COEFF(50+C)*FARE(4,JZ)*FCPI+
     *          KINFR(C))/(LSUM1TRN*LSUM2RB)
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
      UTIL(46)=WCUTL +  (NMLSUM*CWALK +
     *         COEFF(50+C)*FARE(2,JZ)*FCPI   +
     *         KWEXP(C))/(LSUM1TRN*LSUM2EB)
      EUTIL(46)=EXP(UTIL(46))
      END IF
      END IF
C..BIKE ACCESS TO EXPRESS BUS
      IF(M.LT.5) THEN
      IF(BCUTL.NE.0.0) THEN
      MODINC(21)=1
      UTIL(92)=BCUTL +  (NMLSUM*(BIKCACC+BIKCEGR) +
     *         COEFF(50+C)*FARE(8,JZ)*FCPI   +
     *         KBEXP(C))/(LSUM1TRN*LSUM2EB)
      EUTIL(92)=EXP(UTIL(92))
      END IF
      END IF
C..DRIVE ACCESS TO EXPRESS BUS
      IF(DCUTL.NE.0.0) THEN
      MODINC(4)=1
      IT=CSTAE-MAX_IZONES
      FAREVAL(6)=STAZNE(4,IT,JZ)*FCPI
      UTIL(47)=DCUTL + (NMLSUM*DWALK +
     *          COEFF(50+C)*STAZNE(4,IT,JZ)*FCPI +
     *          KDEXP(C)+KDTRN(C))/(LSUM1TRN*LSUM2EB)
      EUTIL(47)=EXP(UTIL(47))
      END IF
C..WALK ACCESS TO TRANSITWAY BUS
      IF(M.LT.5) THEN
      IF(WTUTL.NE.0.0) THEN
      MODINC(13)=1
      UTIL(56)=WTUTL +  (NMLSUM*TWWALK +
     *         COEFF(50+C)*FARE(3,JZ)*FCPI  +
     *         KWWAY(C))/(LSUM1TRN*LSUM2TW)
      EUTIL(56)=EXP(UTIL(56))
      END IF
      END IF
C..BIKE ACCESS TO TRANSITWAY BUS
      IF(M.LT.5) THEN
      IF(BTUTL.NE.0.0) THEN
      MODINC(22)=1
      UTIL(93)=BTUTL +  (NMLSUM*(BIKWTACC+BIKWTEGR) +
     *         COEFF(50+C)*FARE(9,JZ)*FCPI  +
     *         KBWAY(C))/(LSUM1TRN*LSUM2TW)
      EUTIL(93)=EXP(UTIL(93))
      END IF
      END IF
C..DRIVE ACCESS TO TRANSITWAY BUS
      IF(DTUTL.NE.0.0) THEN
      MODINC(14)=1
      IT=CSTAT-MAX_IZONES
      UTIL(57)=DTUTL + (NMLSUM*TDWALK +
     *          COEFF(50+C)*STAZNE(4,IT,JZ)*FCPI +
     *          KDWAY(C)+KDTRN(C))/(LSUM1TRN*LSUM2TW)
      EUTIL(57)=EXP(UTIL(57))
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9306) (UTIL(K),EUTIL(K),K=44,47),UTIL(56),EUTIL(56),
     *                UTIL(57),EUTIL(57),UTIL(72),EUTIL(72),
     *                UTIL(73),EUTIL(73),
     *               (UTIL(K),EUTIL(K),K=90,93)
 9306 FORMAT(/1X,'SUMMARY OF TRANSIT MODE UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',12X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',10X,'----------',5X,
     *          '----------'/
     *       1X,'WALK  ACCESS TO LOCAL      BUS=',F10.5,3X,E12.5/
     *       1X,'DRIVE ACCESS TO LOCAL      BUS=',F10.5,3X,E12.5//
     *       1X,'WALK  ACCESS TO EXPRESS    BUS=',F10.5,3X,E12.5/
     *       1X,'DRIVE ACCESS TO EXPRESS    BUS=',F10.5,3X,E12.5//
     *       1X,'WALK  ACCESS TO TRANSITWAY BUS=',F10.5,3X,E12.5/
     *       1X,'DRIVE ACCESS TO TRANSITWAY BUS=',F10.5,3X,E12.5//
     *       1X,'WALK  ACCESS TO RAPID      BUS=',F10.5,3X,E12.5/
     *       1X,'DRIVE ACCESS TO RAPID      BUS=',F10.5,3X,E12.5//
     *       1X,'BIKE  ACCESS TO LOCAL      BUS=',F10.5,3X,E12.5/
     *       1X,'BIKE  ACCESS TO RAPID      BUS=',F10.5,3X,E12.5/
     *       1X,'BIKE  ACCESS TO EXPRESS    BUS=',F10.5,3X,E12.5/
     *       1X,'BIKE  ACCESS TO TRANSITWAY BUS=',F10.5,3X,E12.5)
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
C                                                                              !Innovation
C..TAXI
      IF(UTILTAXI.NE.0.0.AND.TAXIMODE) THEN
	    UTIL(101)=UTILTAXI
      EUTIL(101)=EXP(UTIL(101))
	    ENDIF
C
C..UBER
      IF(UTILUBER.NE.0.0.AND.UBERMODE) THEN
	    UTIL(102)=UTILUBER
      EUTIL(102)=EXP(UTIL(102))
	    ENDIF
C
C..DRIVE-ALONE PROBABILITIES
      DAPROB(1)=1.0
      DAPROB(2)=0.0
      LSDA=0.0
      DENOM=EUTIL(38) + EUTIL(39)
      IF(ZEROCAR.AND.C.EQ.1) DENOM=0.0
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
      NMPROB(3)=0.0
      LSNMOT=0.0
      UTIL(76)=UTILBK+KBIKE(C)/LSUM1NM
      EUTIL(76)=EXP(UTILBK)*EXP(KBIKE(C)/LSUM1NM)
      UTIL(75)=UTILWK
      EUTIL(75)=EXP(UTILWK)
      IF(ESCOOTER.AND.UTILSC.NE.0.0) THEN
      UTIL(122)=UTILSC + COEFF(60+C)*SCOTCOST
     *                 + KSCOT(C)/LSUM1NM
      EUTIL(122)=EXP(UTIL(122))
      ELSE
      UTIL(122)=0.0
      EUTIL(122)=0.0
      END IF
      DENOM=EUTIL(75)+EUTIL(76)+EUTIL(122)
      IF(DENOM.GT.0.0) THEN
      LSNMOT=DLOG(DENOM)
      NMPROB(1)=EUTIL(75)/DENOM
      NMPROB(3)=EUTIL(122)/DENOM
      NMPROB(2)=1.0-NMPROB(1)-NMPROB(3)
      END IF
C..MAAS 
      MAASPROB(1)=1.0
      MAASPROB(2)=0.0
      LSMAAS=0.0
      DENOM=EUTIL(101)+EUTIL(102)
      IF(DENOM.GT.0.0) THEN
      LSMAAS=DLOG(DENOM)
      MAASPROB(2)=EUTIL(102)/DENOM
      MAASPROB(1)=1.0-MAASPROB(2)
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
     *               UTIL(76),EUTIL(76),NMPROB(2),
     *               UTIL(122),EUTIL(122),NMPROB(3),
     *               UTIL(101),EUTIL(101),MAASPROB(1),
     *               UTIL(102),EUTIL(102),MAASPROB(2)
 9016 FORMAT(/1X,'MARKET SEGMENT=',I2,' ACCESS SEGMENT=',I2//
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
     *       1X,'BIKE MODE                ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'E-SCOOTER                ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'TAXI                     ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'UBER                     ',F10.5,3X,E12.5,3X,F6.4/)
      END IF
C
C....................................................................
C --------------------------------------------------------------------
C   ELIMINATE UNAVAILABLE TRANSIT ACCESS MODES FOR LAX LOT CHOICE
C
      IF(LAX.AND.LAXTRN) THEN
      DO NI=2,12
      EUTIL(NI)=0.0
      END DO
      DO NI=14,24
      EUTIL(NI)=0.0
      END DO
      DO NI=26,38
      EUTIL(NI)=0.0
      END DO
      EUTIL(45)=0.0          !Drive to Local Bus
      EUTIL(73)=0.0          !Drive to Rapid Bus
      EUTIL(47)=0.0          !Drive to Express Bus
      EUTIL(57)=0.0          !Drive to Transitway Bus
C....CHECK FOR FEEDER BUS USE IN URBAN RAIL PATH
      LBUSIND=0
      SC2=ASTA(2,1)-MAX_IZONES
      IF(SC2.GT.0)  LBUSIND=STAZNEI(SC2,JZ,2,1)+STAZNEI(SC2,JZ,2,2)+
     *  STAZNEI(SC2,JZ,2,3)+STAZNEI(SC2,JZ,2,4)+STAZNEI(SC2,JZ,2,5)
      IF(LBUSIND.GT.0) EUTIL(13)=0.0
      END IF
C -----------------------------------------------------------------------
C
C   CALCULATE STATION LEVEL PROBABILITIES BY MODE
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
C..BIKE ACCESS - COMMUTER RAIL
      CBKPROB(1)=1.0
      CBKPROB(2)=0.0
      LSCBIK=0.0
      DENOM=EUTIL(95)+EUTIL(96)
      IF(DENOM.GT.0.0) THEN
C     MODINC(5)=1
      LSCBIK=DLOG(DENOM)
      CBKPROB(1)=EUTIL(95)/DENOM
      CBKPROB(2)=1.0-CBKPROB(1)
      END IF
C..UBER ACCESS - COMMUTER RAIL
      CUPROB(1)=1.0
      CUPROB(2)=0.0
      CUPROB(3)=0.0
      CUPROB(4)=0.0
      LSCUR=0.0
      DENOM=EUTIL(104)+EUTIL(105)+EUTIL(106)+EUTIL(107)
      IF(DENOM.GT.0.0) THEN
      LSCUR=DLOG(DENOM)
      CUPROB(1)=EUTIL(104)/DENOM
      CUPROB(2)=EUTIL(105)/DENOM
      CUPROB(3)=EUTIL(106)/DENOM
      CUPROB(4)=1.0-CUPROB(1)-CUPROB(2)-CUPROB(3)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9008) CWPROB(1),CWPROB(2),CBPROB(1),CBPROB(2),
     *               CPPROB(1),CPPROB(2),CPPROB(3),CPPROB(4),
     *               CKPROB(1),CKPROB(2),CKPROB(3),CKPROB(4),
     *               CBKPROB(1),CBKPROB(2),
     *               CUPROB(1),CUPROB(2),CUPROB(3),CUPROB(4),
     *               LSCWLK,LSCBUS,LSCPR,LSCKR,LSCBIK,LSCUR
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
     *       1X,'BIKE  ACCESS STATION #1=',F8.5/
     *       1X,'BIKE  ACCESS STATION #2=',F8.5//
     *       1X,'UBER  ACCESS STATION #1=',F8.5/
     *       1X,'UBER  ACCESS STATION #2=',F8.5/
     *       1X,'UBER  ACCESS STATION #3=',F8.5/
     *       1X,'UBER  ACCESS STATION #4=',F8.5//
     *       1X,'WALK  ACCESS LOGSUM    =',F8.3/
     *       1X,'BUS   ACCESS LOGSUM    =',F8.3/
     *       1X,'P&R   ACCESS LOGSUM    =',F8.3/
     *       1X,'K&R   ACCESS LOGSUM    =',F8.3/
     *       1X,'BIKE  ACCESS LOGSUM    =',F8.3/
     *       1X,'UBER  ACCESS LOGSUM    =',F8.3)
      END IF
C....................................................................
C
C  COMMUTER RAIL ACCESS UTILITIES & PROBABILITIES
C
C...WALK ACCESS
      IF(LSCWLK.NE.0.0) THEN
      UTIL(48)=LSUM3CW*LSCWLK +
     *         KWCR(C)/(LSUM1TRN*LSUM2CR)
      EUTIL(48)=EXP(UTIL(48))
      END IF
C...BUS ACCESS
      IF(LSCBUS.NE.0.0) THEN
      UTIL(49)=LSUM3CB*LSCBUS  +
     *         KBCR(C)/(LSUM1TRN*LSUM2CR)
      EUTIL(49)=EXP(UTIL(49))
      END IF
C...PARK&RIDE ACCESS
      IF(LSCPR.NE.0.0) THEN
      UTIL(50)=LSUM3CP*LSCPR   +
     *         KPCR(C)/(LSUM1TRN*LSUM2CR)
      EUTIL(50)=EXP(UTIL(50))
      END IF
C...LOWER LEVEL K&R/UBER SPLIT
      LSUM4CU=1.0
      IF(LSCKR.NE.0.0) THEN
      UTIL(116)=LSUM4CU*LSCKR
      EUTIL(116)=EXP(UTIL(116))
      END IF
      IF(LSCUR.NE.0.0) THEN
      UTIL(117)=LSUM4CU*LSCUR
      EUTIL(117)=EXP(UTIL(117))
      END IF
      DENOM=EUTIL(116)+EUTIL(117)
      LSCKR2=0.0
      IF(DENOM.GT.0.0) THEN
      LSCKR2=DLOG(DENOM)
      CUPROB(5)=EUTIL(116)/DENOM
      CUPROB(6)=1.0-CUPROB(5)
      END IF
C...KISS&RIDE ACCESS
      IF(LSCKR.NE.0.0) THEN
      UTIL(51)=LSUM3CK*LSCKR2   +
     *         KKCR(C)/(LSUM1TRN*LSUM2CR)
      EUTIL(51)=EXP(UTIL(51))
      END IF
C...BIKE ACCESS
      IF(LSCBIK.NE.0.0) THEN
      UTIL(99)=LSUM3CBK*LSCBIK +
     *         KBKCR(C)/(LSUM1TRN*LSUM2CR)
      EUTIL(99)=EXP(UTIL(99))
      END IF
C...PROBABILITIES
      CWPROB(3)=0.0
      CBPROB(3)=0.0
      CBKPROB(3)=0.0
      CPPROB(5)=0.0
      CKPROB(5)=0.0
      LSCR=0.0
      DENOM=EUTIL(48)+EUTIL(49)+EUTIL(50)+EUTIL(51)+EUTIL(99)
      IF(DENOM.GT.0.0) THEN
      CWPROB(3)=EUTIL(48)/DENOM
      CBPROB(3)=EUTIL(49)/DENOM
      CBKPROB(3)=EUTIL(99)/DENOM
      CPPROB(5)=EUTIL(50)/DENOM
      CKPROB(5)=1.0-CWPROB(3)-CBPROB(3)-CPPROB(5)-CBKPROB(3)
      CKPROB(5)=AMAX1(CKPROB(5),0.0)
      LSCR=DLOG(DENOM)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9009) (UTIL(K1),EUTIL(K1),K1=116,117),
     *                (UTIL(K),EUTIL(K),K=48,51),
     *                UTIL(99),EUTIL(99)
 9009 FORMAT(/1X,'SUMMARY OF COMMUTER RAIL ACCESS UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',11X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',8X,'----------',5X,
     *          '----------'/
     *       1X,'K&R  ACCESS TO COMMUTER RAIL ',F10.5,3X,E12.5/
     *       1X,'UBER ACCESS TO COMMUTER RAIL ',F10.5,3X,E12.5//
     *       1X,'WALK ACCESS TO COMMUTER RAIL ',F10.5,3X,E12.5/
     *       1X,'BUS  ACCESS TO COMMUTER RAIL ',F10.5,3X,E12.5/
     *       1X,'P&R  ACCESS TO COMMUTER RAIL ',F10.5,3X,E12.5/
     *       1X,'K&R  ACCESS TO COMMUTER RAIL ',F10.5,3X,E12.5/
     *       1X,'BIKE ACCESS TO COMMUTER RAIL ',F10.5,3X,E12.5)
      WRITE(26,9010) CWPROB(3),CBPROB(3),CPPROB(5),CKPROB(5),
     *               CBKPROB(3),CUPROB(5),CUPROB(6),LSCR
 9010 FORMAT(/1X,'SUMMARY OF COMMUTER RAIL ACCESS CHOICE PROB:'/
     *       1X,'---------------------------------------------'/
     *       1X,'WALK  ACCESS=',F8.5/ 
     *       1X,'BUS   ACCESS=',F8.5/
     *       1X,'P&R   ACCESS=',F8.5/
     *       1X,'K&R   ACCESS=',F8.5/
     *       1X,'BIKE  ACCESS=',F8.5//
     *       1X,' UBER & K&R CHOICE PROB:'/
     *       1X,' -----------------------'/
     *       1X,'K&R   ACCESS=',F8.5/
     *       1X,'UBER  ACCESS=',F8.5//
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
C..UBER ACCESS - URBAN RAIL
      UUPROB(1)=1.0
      UUPROB(2)=0.0
      UUPROB(3)=0.0
      UUPROB(4)=0.0
      LSUUR=0.0
      DENOM=EUTIL(108)+EUTIL(109)+EUTIL(110)+EUTIL(111)
      IF(DENOM.GT.0.0) THEN
      LSUUR=DLOG(DENOM)
      UUPROB(1)=EUTIL(108)/DENOM
      UUPROB(2)=EUTIL(109)/DENOM
      UUPROB(3)=EUTIL(110)/DENOM
      UUPROB(4)=1.0-UUPROB(1)-UUPROB(2)-UUPROB(3)
      END IF
C..BIKE ACCESS - URBAN RAIL
      UBKPROB(1)=1.0
      UBKPROB(2)=0.0
      LSUBIK=0.0
      DENOM=EUTIL(97)+EUTIL(98)
      IF(DENOM.GT.0.0) THEN
C     MODINC(9)=1
      LSUBIK=DLOG(DENOM)
      UBKPROB(1)=EUTIL(97)/DENOM
      UBKPROB(2)=1.0-UBKPROB(1)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9108) UWPROB(1),UWPROB(2),UBPROB(1),UBPROB(2),
     *               UPPROB(1),UPPROB(2),UPPROB(3),UPPROB(4),
     *               UKPROB(1),UKPROB(2),UKPROB(3),UKPROB(4),
     *               UUPROB(1),UUPROB(2),UUPROB(3),UUPROB(4),
     *               UBKPROB(1),UBKPROB(2),
     *               LSUWLK,LSUBUS,LSUPR,LSUKR,LSUBIK,LSUUR
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
     *       1X,'UBER  ACCESS STATION #1=',F8.5/
     *       1X,'UBER  ACCESS STATION #2=',F8.5/
     *       1X,'UBER  ACCESS STATION #3=',F8.5/
     *       1X,'UBER  ACCESS STATION #4=',F8.5//
     *       1X,'BIKE  ACCESS STATION #1=',F8.5/
     *       1X,'BIKE  ACCESS STATION #2=',F8.5//
     *       1X,'WALK  ACCESS LOGSUM    =',F10.5/
     *       1X,'BUS   ACCESS LOGSUM    =',F10.5/
     *       1X,'P&R   ACCESS LOGSUM    =',F10.5/
     *       1X,'K&R   ACCESS LOGSUM    =',F10.5/
     *       1X,'BIKE  ACCESS LOGSUM    =',F10.5/
     *       1X,'UBER  ACCESS LOGSUM    =',F10.5)
      END IF
C....................................................................
C
C  URBAN RAIL ACCESS UTILITIES & PROBABILITIES
C
      IF(LAX.AND.LAXTRN.AND.AIRPASS) KWUR(C)=AWUR(C)
C...WALK ACCESS
      IF(LSUWLK.NE.0.0) THEN
      UTIL(52)=LSUM3UW*LSUWLK  +
     *         KWUR(C)/(LSUM1TRN*LSUM2UR)
      EUTIL(52)=EXP(UTIL(52))
      END IF
C...BUS ACCESS
      IF(LSUBUS.NE.0.0) THEN
      UTIL(53)=LSUM3UB*LSUBUS  +
     *         KBUR(C)/(LSUM1TRN*LSUM2UR)
      EUTIL(53)=EXP(UTIL(53))
      END IF
C...PARK&RIDE ACCESS
      IF(LSUPR.NE.0.0) THEN
      UTIL(54)=LSUM3UP*LSUPR   +
     *         KPUR(C)/(LSUM1TRN*LSUM2UR)
      EUTIL(54)=EXP(UTIL(54))
      END IF
C...LOWER LEVEL K&R/UBER SPLIT                              
      LSUM4UU=1.0                                           
      IF(LSUKR.NE.0.0) THEN                                 
      UTIL(118)=LSUM4UU*LSUKR                                
      EUTIL(118)=EXP(UTIL(118))                               
      END IF                                                
      IF(LSUUR.NE.0.0) THEN                                 
      UTIL(119)=LSUM4UU*LSUUR                                
      EUTIL(119)=EXP(UTIL(119))                               
      END IF                                                
      DENOM=EUTIL(118)+EUTIL(119)                             
      LSUKR2=0.0                                            
      IF(DENOM.GT.0.0) THEN                                 
      LSUKR2=DLOG(DENOM)                                    
      UUPROB(5)=EUTIL(118)/DENOM                             
      UUPROB(6)=1.0-UUPROB(5)                               
      END IF   
C...KISS&RIDE ACCESS
      IF(LSUKR.NE.0.0) THEN
      UTIL(55)=LSUM3UK*LSUKR2   +
     *         KKUR(C)/(LSUM1TRN*LSUM2UR)
      EUTIL(55)=EXP(UTIL(55))
      END IF
C...BIKE ACCESS
      IF(LSUBIK.NE.0.0) THEN
      UTIL(100)=LSUM3UBK*LSUBIK  +
     *         KBKUR(C)/(LSUM1TRN*LSUM2UR)
      EUTIL(100)=EXP(UTIL(100))
      END IF
C...PROBABILITIES
      UWPROB(3)=0.0
      UBPROB(3)=0.0
      UBKPROB(3)=0.0
      UPPROB(5)=0.0
      UKPROB(5)=0.0
      LSURB=0.0
      DENOM=EUTIL(52)+EUTIL(53)+EUTIL(54)+EUTIL(55)+EUTIL(100)
      IF(DENOM.GT.0.0) THEN
      UWPROB(3)=EUTIL(52)/DENOM
      UBPROB(3)=EUTIL(53)/DENOM
      UBKPROB(3)=EUTIL(100)/DENOM
      UPPROB(5)=EUTIL(54)/DENOM
      UKPROB(5)=1.0-UWPROB(3)-UBPROB(3)-UPPROB(5)-UBKPROB(3)
      UKPROB(5)=AMAX1(UKPROB(5),0.0)
      LSURB=DLOG(DENOM)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9109) (UTIL(K),EUTIL(K),K=118,119),
     *               (UTIL(K),EUTIL(K),K=52,55),
     *                UTIL(100),EUTIL(100)
 9109 FORMAT(/1X,'SUMMARY OF URBAN RAIL ACCESS UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',6X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',5X,'----------',5X,
     *          '----------'/
     *       1X,'K&R  ACCESS TO URBAN RAIL ',F10.5,3X,E12.5/
     *       1X,'UBER ACCESS TO URBAN RAIL ',F10.5,3X,E12.5//
     *       1X,'WALK ACCESS TO URBAN RAIL ',F10.5,3X,E12.5/
     *       1X,'BUS  ACCESS TO URBAN RAIL ',F10.5,3X,E12.5//
     *       1X,'P&R  ACCESS TO URBAN RAIL ',F10.5,3X,E12.5/
     *       1X,'K&R  ACCESS TO URBAN RAIL ',F10.5,3X,E12.5/
     *       1X,'BIKE ACCESS TO URBAN RAIL ',F10.5,3X,E12.5)
      WRITE(26,9110) UUPROB(5),UUPROB(6),
     *               UWPROB(3),UBPROB(3),UPPROB(5),UKPROB(5),
     *               UBKPROB(3),LSURB
 9110 FORMAT(/1X,'SUMMARY OF URBAN RAIL ACCESS CHOICE PROB:'/
     *       1X,'---------------------------------------------'/
     *       1X,'K&R   ACCESS=',F8.5/
     *       1X,'UBER  ACCESS=',F8.5//
     *       1X,'WALK  ACCESS=',F8.5/ 
     *       1X,'BUS   ACCESS=',F8.5/
     *       1X,'P&R   ACCESS=',F8.5/
     *       1X,'K&R   ACCESS=',F8.5/
     *       1X,'BIKE  ACCESS=',F8.5//
     *       1X,'URBAN RAIL LOGSUM=',F10.5)
      END IF
C
C
C  BRT ACCESS UTILITIES & PROBABILITIES
C
C..WALK ACCESS - BRT
      BRWPROB(1)=1.0
      BRWPROB(2)=0.0
      LSBRWLK=0.0
      DENOM=EUTIL(25)+EUTIL(26)
      IF(DENOM.GT.0.0) THEN
C     MODINC(9)=1
      LSBRWLK=DLOG(DENOM)
      BRWPROB(1)=EUTIL(25)/DENOM
      BRWPROB(2)=1.0-BRWPROB(1)
      END IF
C..BUS ACCESS - BRT
      BRBPROB(1)=1.0
      BRBPROB(2)=0.0
      LSBRBUS=0.0
      DENOM=EUTIL(27)+EUTIL(28)
      IF(DENOM.GT.0.0) THEN
C     MODINC(10)=1
      LSBRBUS=DLOG(DENOM)
      BRBPROB(1)=EUTIL(27)/DENOM
      BRBPROB(2)=1.0-BRBPROB(1)
      END IF
C..P&R ACCESS - BRT
      BRPPROB(1)=1.0
      BRPPROB(2)=0.0
      BRPPROB(3)=0.0
      BRPPROB(4)=0.0
      LSBRPR=0.0
      DENOM=EUTIL(29)+EUTIL(30)+EUTIL(31)+EUTIL(32)
      IF(DENOM.GT.0.0) THEN
C     MODINC(11)=1
      LSBRPR=DLOG(DENOM)
      BRPPROB(1)=EUTIL(29)/DENOM
      BRPPROB(2)=EUTIL(30)/DENOM
      BRPPROB(3)=EUTIL(31)/DENOM
      BRPPROB(4)=1.0-BRPPROB(1)-BRPPROB(2)-BRPPROB(3)
      END IF
C..K&R ACCESS - BRT
      BRKPROB(1)=1.0
      BRKPROB(2)=0.0
      BRKPROB(3)=0.0
      BRKPROB(4)=0.0
      LSBRKR=0.0
      DENOM=EUTIL(33)+EUTIL(34)+EUTIL(35)+EUTIL(36)
      IF(DENOM.GT.0.0) THEN
C     MODINC(12)=1
      LSBRKR=DLOG(DENOM)
      BRKPROB(1)=EUTIL(33)/DENOM
      BRKPROB(2)=EUTIL(34)/DENOM
      BRKPROB(3)=EUTIL(35)/DENOM
      BRKPROB(4)=1.0-BRKPROB(1)-BRKPROB(2)-BRKPROB(3)
      END IF
C..UBER ACCESS - BRT
      BRUPROB(1)=1.0
      BRUPROB(2)=0.0
      BRUPROB(3)=0.0
      BRUPROB(4)=0.0
      LSUBR=0.0
      DENOM=EUTIL(112)+EUTIL(113)+EUTIL(114)+EUTIL(115)
      IF(DENOM.GT.0.0) THEN
      LSUBR=DLOG(DENOM)
      BRUPROB(1)=EUTIL(112)/DENOM
      BRUPROB(2)=EUTIL(113)/DENOM
      BRUPROB(3)=EUTIL(114)/DENOM
      BRUPROB(4)=1.0-BRUPROB(1)-BRUPROB(2)-BRUPROB(3)
      END IF
C..BIKE ACCESS - BRT
      BRBKPROB(1)=1.0
      BRBKPROB(2)=0.0
      LSBRBIK=0.0
      DENOM=EUTIL(79)+EUTIL(80)
      IF(DENOM.GT.0.0) THEN
C     MODINC(9)=1
      LSBRBIK=DLOG(DENOM)
      BRBKPROB(1)=EUTIL(79)/DENOM
      BRBKPROB(2)=1.0-BRBKPROB(1)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9118) BRWPROB(1),BRWPROB(2),BRBPROB(1),BRBPROB(2),
     *               BRPPROB(1),BRPPROB(2),BRPPROB(3),BRPPROB(4),
     *               BRKPROB(1),BRKPROB(2),BRKPROB(3),BRKPROB(4),
     *               BRUPROB(1),BRUPROB(2),BRUPROB(3),BRUPROB(4),
     *               BRBKPROB(1),BRBKPROB(2),
     *               LSBRWLK,LSBRBUS,LSBRPR,LSBRKR,LSBRBIK,LSUBR
 9118 FORMAT(/1X,'SUMMARY OF BRT STATION CHOICE PROB: '/
     *       1X,'-------------------------------------'/
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
     *       1X,'UBER  ACCESS STATION #1=',F8.5/
     *       1X,'UBER  ACCESS STATION #2=',F8.5/
     *       1X,'UBER  ACCESS STATION #3=',F8.5/
     *       1X,'UBER  ACCESS STATION #4=',F8.5//
     *       1X,'BIKE  ACCESS STATION #1=',F8.5/
     *       1X,'BIKE  ACCESS STATION #2=',F8.5//
     *       1X,'WALK  ACCESS LOGSUM    =',F10.5/
     *       1X,'BUS   ACCESS LOGSUM    =',F10.5/
     *       1X,'P&R   ACCESS LOGSUM    =',F10.5/
     *       1X,'K&R   ACCESS LOGSUM    =',F10.5/
     *       1X,'BIKE  ACCESS LOGSUM    =',F10.5/
     *       1X,'UBER  ACCESS LOGSUM    =',F10.5)
      END IF
C....................................................................
C
C  BRT ACCESS UTILITIES & PROBABILITIES
C
C...WALK ACCESS
      IF(LSBRWLK.NE.0.0) THEN
      UTIL(64)=LSUM3BRW*LSBRWLK  +
     *         KWBR(C)/(LSUM1TRN*LSUM2BR)
      EUTIL(64)=EXP(UTIL(64))
      END IF
C...BUS ACCESS
      IF(LSBRBUS.NE.0.0) THEN
      UTIL(65)=LSUM3BRB*LSBRBUS  +
     *         KBBR(C)/(LSUM1TRN*LSUM2BR)
      EUTIL(65)=EXP(UTIL(65))
      END IF
C...PARK&RIDE ACCESS
      IF(LSBRPR.NE.0.0) THEN
      UTIL(66)=LSUM3BRP*LSBRPR   +
     *         KPBR(C)/(LSUM1TRN*LSUM2BR)
      EUTIL(66)=EXP(UTIL(66))
      END IF
C...LOWER LEVEL K&R/UBER SPLIT                              
      LSUM4BRU=1.0                                           
      IF(LSBRKR.NE.0.0) THEN                                 
      UTIL(120)=LSUM4BRU*LSBRKR                                
      EUTIL(120)=EXP(UTIL(120))                               
      END IF                                                
      IF(LSUBR.NE.0.0) THEN                                 
      UTIL(121)=LSUM4BRU*LSUBR                                
      EUTIL(121)=EXP(UTIL(121))                               
      END IF                                                
      DENOM=EUTIL(120)+EUTIL(121)                             
      LSBRKR2=0.0                                            
      IF(DENOM.GT.0.0) THEN                                 
      LSBRKR2=DLOG(DENOM)                                    
      BRUPROB(5)=EUTIL(120)/DENOM                             
      BRUPROB(6)=1.0-BRUPROB(5)                               
      END IF   
C...KISS&RIDE ACCESS
      IF(LSBRKR2.NE.0.0) THEN
      UTIL(67)=LSUM3BRK*LSBRKR2   +
     *         KKBR(C)/(LSUM1TRN*LSUM2BR)
      EUTIL(67)=EXP(UTIL(67))
      END IF
C...BIKE ACCESS
      IF(LSBRBIK.NE.0.0) THEN
      UTIL(68)=LSUM3BRBK*LSBKBIK  +
     *         KBKBR(C)/(LSUM1TRN*LSUM2BR)
      EUTIL(68)=EXP(UTIL(68))
      END IF
C...PROBABILITIES
      BRWPROB(3)=0.0
      BRBPROB(3)=0.0
      BRBKPROB(3)=0.0
      BRPPROB(5)=0.0
      BRKPROB(5)=0.0
      LSBRT=0.0
      DENOM=EUTIL(64)+EUTIL(65)+EUTIL(66)+EUTIL(67)+EUTIL(68)
      IF(DENOM.GT.0.0) THEN
      BRWPROB(3)=EUTIL(64)/DENOM
      BRBPROB(3)=EUTIL(65)/DENOM
      BRBKPROB(3)=EUTIL(68)/DENOM
      BRPPROB(5)=EUTIL(66)/DENOM
      BRKPROB(5)=1.0-BRWPROB(3)-BRBPROB(3)-BRPPROB(5)-BRBKPROB(3)
      BRKPROB(5)=AMAX1(BRKPROB(5),0.0)
      LSBRT=DLOG(DENOM)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9119) (UTIL(K),EUTIL(K),K=120,121),
     *               (UTIL(K),EUTIL(K),K=64,68)
 9119 FORMAT(/1X,'SUMMARY OF BRT ACCESS UTILITIES'/
     *       1X,'--------------------------------'/
     *       1X,'                     ',6X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',5X,'----------',5X,
     *          '----------'/
     *       1X,'K&R  ACCESS TO BRT RPD TRN',F10.5,3X,E12.5/
     *       1X,'UBER ACCESS TO BRT RPD TRN',F10.5,3X,E12.5//
     *       1X,'WALK ACCESS TO BUS RPD TRN',F10.5,3X,E12.5/
     *       1X,'BUS  ACCESS TO BUS RPD TRN',F10.5,3X,E12.5//
     *       1X,'P&R  ACCESS TO BUS RPD TRN',F10.5,3X,E12.5/
     *       1X,'K&R  ACCESS TO BUS RPD TRN',F10.5,3X,E12.5/
     *       1X,'BIKE ACCESS TO BUS RPD TRN',F10.5,3X,E12.5)
      WRITE(26,9120) BRUPROB(5),BRUPROB(6),
     *               BRWPROB(3),BRBPROB(3),BRPPROB(5),BRKPROB(5),
     *               BRBKPROB(3),LSBRT
 9120 FORMAT(/1X,'SUMMARY OF BRT ACCESS CHOICE PROB:'/
     *       1X,'-----------------------------------'/
     *       1X,'K&R   ACCESS=',F8.5/
     *       1X,'UBER  ACCESS=',F8.5//
     *       1X,'WALK  ACCESS=',F8.5/ 
     *       1X,'BUS   ACCESS=',F8.5/
     *       1X,'P&R   ACCESS=',F8.5/
     *       1X,'K&R   ACCESS=',F8.5/
     *       1X,'BIKE  ACCESS=',F8.5//
     *       1X,'BRT   LOGSUM=',F10.5)
      END IF
C...............................................................
C...............................................................
C
C  LOCAL & EXPRESS BUS ACCESS UTILITIES AND PROBABILITIES
C
C..LOCAL BUS
      BUSPROB(1)=0.0
      BUSPROB(2)=0.0
      BUSPROB(3)=0.0
      LSLOC=0.0
      DENOM=EUTIL(44)+EUTIL(45)+EUTIL(90)
      IF(DENOM.GT.0.0) THEN
      LSLOC=DLOG(DENOM)
      BUSPROB(1)=EUTIL(44)/DENOM
      BUSPROB(2)=EUTIL(45)/DENOM
      BUSPROB(3)=1.0-BUSPROB(1)-BUSPROB(2)
      END IF
C..RAPID BUS
      RPDPROB(1)=0.0
      RPDPROB(2)=0.0
      RPDPROB(3)=0.0
      LSRPD=0.0
      DENOM=EUTIL(72)+EUTIL(73)+EUTIL(91)
      IF(DENOM.GT.0.0) THEN
      LSRPD=DLOG(DENOM)
      RPDPROB(1)=EUTIL(72)/DENOM
      RPDPROB(2)=EUTIL(73)/DENOM
      RPDPROB(3)=1.0-RPDPROB(1)-RPDPROB(2)
      END IF      
C..EXPRESS BUS
      EXPPROB(1)=0.0
      EXPPROB(2)=0.0
      EXPPROB(3)=0.0
      LSEXP=0.0
      DENOM=EUTIL(46)+EUTIL(47)+EUTIL(92)
      IF(DENOM.GT.0.0) THEN
      LSEXP=DLOG(DENOM)
      EXPPROB(1)=EUTIL(46)/DENOM
      EXPPROB(2)=EUTIL(47)/DENOM
      EXPPROB(3)=1.0-EXPPROB(1)-EXPPROB(2)
      END IF
C..TRANSITWAY BUS
      BWPROB(1)=0.0
      BWPROB(2)=0.0
      BWPROB(3)=0.0
      LSWAY=0.0
      DENOM=EUTIL(56)+EUTIL(57)+EUTIL(93)
      IF(DENOM.GT.0.0) THEN
      LSWAY=DLOG(DENOM)
      BWPROB(1)=EUTIL(56)/DENOM
      BWPROB(2)=EUTIL(57)/DENOM
      BWPROB(3)=1.0-BWPROB(1)-BWPROB(2)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9011) BUSPROB(1),BUSPROB(2),BUSPROB(3),LSLOC,
     *               RPDPROB(1),RPDPROB(2),RPDPROB(3),LSRPD,
     *               EXPPROB(1),EXPPROB(2),EXPPROB(3),
     *               LSEXP,BWPROB(1),BWPROB(2),BWPROB(3),LSWAY
 9011 FORMAT(/1X,'SUMMARY OF LOCAL & EXPRESS BUS CHOICE PROBABILITIES'/
     *       1X,'---------------------------------------------'/
     *       1X,'WALK  ACCESS --> LOCAL       =',F8.5/
     *       1X,'DRIVE ACCESS --> LOCAL       =',F8.5/
     *       1X,'BIKE  ACCESS --> LOCAL       =',F8.5/
     *       1X,'LOCAL BUS LOGSUM             =',F8.2//
     *       1X,'WALK  ACCESS --> RAPID       =',F8.5/
     *       1X,'DRIVE ACCESS --> RAPID       =',F8.5/
     *       1X,'BIKE  ACCESS --> RAPID       =',F8.5/
     *       1X,'RAPID BUS LOGSUM             =',F8.2//
     *       1X,'WALK  ACCESS --> EXPRESS     =',F8.5/
     *       1X,'DRIVE ACCESS --> EXPRESS     =',F8.5/
     *       1X,'BIKE  ACCESS --> EXPRESS     =',F8.5/
     *       1X,'EXPRESS BUS LOGSUM           =',F8.2//
     *       1X,'WALK  ACCESS --> TRANSITWAY  =',F8.5/
     *       1X,'DRIVE ACCESS --> TRANSITWAY  =',F8.5/
     *       1X,'BIKE  ACCESS --> TRANSITWAY  =',F8.5/
     *       1X,'TRANSITWAY BUS LOGSUM        =',F8.2)
	  END IF
C....................................................................
C
C  SHARED RIDE UTILITIES AND PROBABILITIES
C 
      IF(LS2PER.NE.0.0) THEN
	    UTIL(66)=LSUM3P2*LS2PER + K2P(C)/(LSUM1AUTO*LSUM2SR)
      EUTIL(66)=EXP(UTIL(66))
	    ENDIF
C
      IF(LS3PER.NE.0.0) THEN
	    UTIL(67)=LSUM3P3*LS3PER + K3P(C)/(LSUM1AUTO*LSUM2SR)
      EUTIL(67)=EXP(UTIL(67))
	    ENDIF
C
      IF(LS4PER.NE.0.0) THEN
	    UTIL(83)=LSUM3P4*LS4PER + K4P(C)/(LSUM1AUTO*LSUM2SR)
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
      WRITE(26,9012) C,LS2PER,LS3PER,LS4PER,
     *               K2P(C),K3P(C),K4P(C),
     *               UTIL(66),UTIL(67),UTIL(83),
     *               SRPROB(1),SRPROB(2),SRPROB(3),LSSHR
 9012 FORMAT(/1X,'SUMMARY OF SHARED RIDE CHOICE PROBABILITIES'/
     *       1X,'---------------------------------------------'/
     *       1X,'INCOME GROUP     =',I9/
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
      KCCBD=0
      DO E=1,50
      IF(JZ.EQ.CBDZ(E)) THEN
      KCCBD(1)=CCBD(1)
      KCCBD(2)=CCBD(2)
      KCCBD(3)=CCBD(3)
      KCCBD(4)=CCBD(4)
      KCCBD(5)=CCBD(5)
      KCCBD(6)=CCBD(6)
      KCCBD(7)=CCBD(7)
      END IF
      END DO
C..COMMUTER RAIL
      IF(LSCR.NE.0.0) THEN
      UTIL(60)=LSUM2CR*LSCR + KCR(C)/(LSUM1TRN*ADJ1CR) 
     *        + KCCBD(1)/LSUM1TRN
     *        +CRDIST*((1.0/TAB2DA(JZ))**DISTEXP)
      UTIL(60)=UTIL(60) + (ZHHD(16,IZ) + ZHHD(17,JZ))/LSUM1TRN
      EUTIL(60)=EXP(UTIL(60))
      END IF
C
C..URBAN RAIL
      IF(LSURB.NE.0.0) THEN
      UTIL(61)=LSUM2UR*LSURB + KUR(C)/(LSUM1TRN*ADJ1UR) 
     *                     + KCCBD(2)/LSUM1TRN + KLAX/LSUM1TRN 
     *                     + (ZHHD(16,IZ) + ZHHD(17,JZ))/LSUM1TRN +
     *        (PCONST(DIZ) + ACONST(DJZ) + PACONST(DIZ,DJZ))/LSUM1TRN
      ALOGSUM(C,JZ)=ALOGSUM(C,JZ)+(MWALK(M)*UTIL(61)*LSUM1TRN)/
     *                                         COEFF(100)
      EUTIL(61)=EXP(UTIL(61))
      END IF
C..TRANSITWAY
      IF(LSWAY.NE.0.0) THEN
      UTIL(62)=LSUM2TW*LSWAY + KWAY(C)/(LSUM1TRN)+ KCCBD(3)/LSUM1TRN
     *                     + (ZHHD(16,IZ) + ZHHD(17,JZ))/LSUM1TRN
      EUTIL(62)=EXP(UTIL(62))
      END IF
C..EXPRESS BUS
      IF(LSEXP.NE.0.0) THEN
      UTIL(63)=LSUM2EB*LSEXP + KEBUS(C)/(LSUM1TRN)+ KCCBD(4)/LSUM1TRN
     *                     + (ZHHD(16,IZ) + ZHHD(17,JZ))/LSUM1TRN
      EUTIL(63)=EXP(UTIL(63))
      END IF
C..RAPID BUS
      IF(LSRPD.NE.0.0) THEN
      UTIL(74)=LSUM2RB*LSRPD + KRBUS(C)/(LSUM1TRN)+ KCCBD(5)/LSUM1TRN
     *                     + (ZHHD(16,IZ) + ZHHD(17,JZ))/LSUM1TRN +
     *        + (COEFF(50)* ZCARP + COEFF(49) * ZCARA)/LSUM1TRN
      EUTIL(74)=EXP(UTIL(74))
      END IF
C..LOCAL BUS
      IF(LSLOC.NE.0.0) THEN
      UTIL(69)=LSUM2LB*LSLOC + KLBUS(C)/(LSUM1TRN)+ KCCBD(6)/LSUM1TRN
     *                     + (ZHHD(16,IZ) + ZHHD(17,JZ))/LSUM1TRN
     *        + (COEFF(50)* ZCARP + COEFF(49) * ZCARA)/LSUM1TRN
      EUTIL(69)=EXP(UTIL(69))
      END IF
C..BUS RAPID TRANSIT (BRT)
      IF(LSBRT.NE.0.0) THEN
      UTIL(70)=LSUM2BR*LSBRT + KBRT(C)/(LSUM1TRN)+ KCCBD(7)/LSUM1TRN
     *                     + (ZHHD(16,IZ) + ZHHD(17,JZ))/LSUM1TRN
      EUTIL(70)=EXP(UTIL(70))
      END IF
C..TRANSIT PROBABILITIES
      TRNPROB(1)=0.0
      TRNPROB(2)=0.0
      TRNPROB(3)=0.0
      TRNPROB(4)=0.0
      TRNPROB(5)=0.0
      TRNPROB(6)=0.0
      TRNPROB(7)=0.0
      TRNPROB(8)=0.0
      LSTRN=0.0
      DENOM=EUTIL(60)+EUTIL(61)+EUTIL(62)+EUTIL(63)+EUTIL(74)+
     *      EUTIL(69)+EUTIL(70)+EUTILFLY
      IF(DENOM.GT.0.0) THEN
      LSTRN=DLOG(DENOM)
      TRNPROB(1)=EUTIL(60)/DENOM
      TRNPROB(2)=EUTIL(61)/DENOM
      TRNPROB(3)=EUTIL(62)/DENOM
      TRNPROB(4)=EUTIL(63)/DENOM
      TRNPROB(6)=EUTIL(69)/DENOM
      TRNPROB(7)=EUTIL(70)/DENOM
      TRNPROB(5)=EUTIL(74)/DENOM
      TRNPROB(8)=EUTILFLY/DENOM
       IF(TRNPROB(5).GT.0.0) THEN
       TRNPROB(5)=1.0-TRNPROB(1)-TRNPROB(2)-TRNPROB(3)-
     *               TRNPROB(4)-TRNPROB(6)-TRNPROB(7)-
     *               TRNPROB(8)
       ELSE
       TRNPROB(5)=0.0
       END IF
      END IF
C ===================================================================
C SAVE TRANSIT PROBABILITIES & STATION VALUES 
C FOR LAX PARKING LOT CHOICE MODEL
C
      IF(LAX.AND.LAXTRN.AND.PARKIND.GT.0.AND.
     *   M.EQ.1.AND.C.EQ.2.AND.FLYAIND.EQ.0) THEN
      JZIND=0
      DO 8042 K=1,50
      IF(AEQUIV(K).EQ.JZ) JZIND=K
 8042 CONTINUE
      IF(JZIND.LE.0) THEN
      WRITE(26,8043) JZ
      WRITE(*,8043)  JZ
 8043 FORMAT(1X,'MTAMC 8043 (F) NO LAX INDICATOR MATCH FOR',
     *          ' ATTRACTION ZONE ',I4)
      STOP 8043
      END IF 
      LAXTPROB(PARKIND,JZIND,1)=CWPROB(1)
      LAXTPROB(PARKIND,JZIND,2)=CWPROB(2)
      LAXTPROB(PARKIND,JZIND,3)=CBPROB(1)
      LAXTPROB(PARKIND,JZIND,4)=CBPROB(2)
      LAXTPROB(PARKIND,JZIND,5)=CPPROB(1)
      LAXTPROB(PARKIND,JZIND,6)=CPPROB(2)
      LAXTPROB(PARKIND,JZIND,7)=CPPROB(3)
      LAXTPROB(PARKIND,JZIND,8)=CPPROB(4) 
      LAXTPROB(PARKIND,JZIND,9)=CKPROB(1)
      LAXTPROB(PARKIND,JZIND,10)=CKPROB(2)
      LAXTPROB(PARKIND,JZIND,11)=CKPROB(3)
      LAXTPROB(PARKIND,JZIND,12)=CKPROB(4)  
      LAXTPROB(PARKIND,JZIND,13)=UWPROB(1)
      LAXTPROB(PARKIND,JZIND,14)=UWPROB(2)
      LAXTPROB(PARKIND,JZIND,15)=UBPROB(1)
      LAXTPROB(PARKIND,JZIND,16)=UBPROB(2)
      LAXTPROB(PARKIND,JZIND,17)=UPPROB(1)
      LAXTPROB(PARKIND,JZIND,18)=UPPROB(2)
      LAXTPROB(PARKIND,JZIND,19)=UPPROB(3)
      LAXTPROB(PARKIND,JZIND,20)=UPPROB(4) 
      LAXTPROB(PARKIND,JZIND,21)=UKPROB(1)
      LAXTPROB(PARKIND,JZIND,22)=UKPROB(2)
      LAXTPROB(PARKIND,JZIND,23)=UKPROB(3)
      LAXTPROB(PARKIND,JZIND,24)=UKPROB(4)
      LAXTPROB(PARKIND,JZIND,25)=BUSPROB(1)
      LAXTPROB(PARKIND,JZIND,26)=BUSPROB(2)
      LAXTPROB(PARKIND,JZIND,27)=RPDPROB(1)
      LAXTPROB(PARKIND,JZIND,28)=RPDPROB(2) 
      LAXTPROB(PARKIND,JZIND,29)=EXPPROB(1)
      LAXTPROB(PARKIND,JZIND,30)=EXPPROB(2)
      LAXTPROB(PARKIND,JZIND,31)=BWPROB(1)
      LAXTPROB(PARKIND,JZIND,32)=BWPROB(2)
      LAXTPROB(PARKIND,JZIND,33)=BRTPROB(1)
      LAXTPROB(PARKIND,JZIND,34)=BRTPROB(2)
      LAXTPROB(PARKIND,JZIND,35)=TRNPROB(1)
      LAXTPROB(PARKIND,JZIND,36)=TRNPROB(2)      
      LAXTPROB(PARKIND,JZIND,37)=TRNPROB(3)      
      LAXTPROB(PARKIND,JZIND,38)=TRNPROB(4)      
      LAXTPROB(PARKIND,JZIND,39)=TRNPROB(5)      
      LAXTPROB(PARKIND,JZIND,40)=TRNPROB(6)
      LAXTPROB(PARKIND,JZIND,41)=TRNPROB(7)
      LAXTPROB(PARKIND,JZIND,42)=LSTRN
      LAXSTAP(PARKIND,JZIND,1)=OSTA(1,1)
      LAXSTAP(PARKIND,JZIND,2)=OSTA(1,2)
      LAXSTAP(PARKIND,JZIND,3)=OSTA(1,3)
      LAXSTAP(PARKIND,JZIND,4)=OSTA(1,4)
      LAXSTAP(PARKIND,JZIND,5)=OSTA(1,5)
      LAXSTAP(PARKIND,JZIND,6)=OSTA(1,6)
      LAXSTAP(PARKIND,JZIND,7)=OSTA(1,7)
      LAXSTAP(PARKIND,JZIND,8)=OSTA(1,8)
      LAXSTAP(PARKIND,JZIND,9)=OSTA(1,9)
      LAXSTAP(PARKIND,JZIND,10)=OSTA(1,10)
      LAXSTAP(PARKIND,JZIND,11)=OSTA(1,11)
      LAXSTAP(PARKIND,JZIND,12)=OSTA(1,12)
      LAXSTAP(PARKIND,JZIND,13)=OSTA(2,1)
      LAXSTAP(PARKIND,JZIND,14)=OSTA(2,2)
      LAXSTAP(PARKIND,JZIND,15)=OSTA(2,3)
      LAXSTAP(PARKIND,JZIND,16)=OSTA(2,4)
      LAXSTAP(PARKIND,JZIND,17)=OSTA(2,5)
      LAXSTAP(PARKIND,JZIND,18)=OSTA(2,6)
      LAXSTAP(PARKIND,JZIND,19)=OSTA(2,7)
      LAXSTAP(PARKIND,JZIND,20)=OSTA(2,8)
      LAXSTAP(PARKIND,JZIND,21)=OSTA(2,9)
      LAXSTAP(PARKIND,JZIND,22)=OSTA(2,10)
      LAXSTAP(PARKIND,JZIND,23)=OSTA(2,11)
      LAXSTAP(PARKIND,JZIND,24)=OSTA(2,12)
      LAXSTAP(PARKIND,JZIND,30)=CSTAE
      LAXSTAP(PARKIND,JZIND,32)=CSTAT
      LAXSTAP(PARKIND,JZIND,34)=CSTABRT
      LAXSTAA(PARKIND,JZIND,1)=ASTA(1,1)
      LAXSTAA(PARKIND,JZIND,2)=ASTA(1,2)
      LAXSTAA(PARKIND,JZIND,3)=ASTA(1,3)
      LAXSTAA(PARKIND,JZIND,4)=ASTA(1,4)
      LAXSTAA(PARKIND,JZIND,5)=ASTA(1,5)
      LAXSTAA(PARKIND,JZIND,6)=ASTA(1,6)
      LAXSTAA(PARKIND,JZIND,7)=ASTA(1,7)
      LAXSTAA(PARKIND,JZIND,8)=ASTA(1,8)
      LAXSTAA(PARKIND,JZIND,9)=ASTA(1,9)
      LAXSTAA(PARKIND,JZIND,10)=ASTA(1,10)
      LAXSTAA(PARKIND,JZIND,11)=ASTA(1,11)
      LAXSTAA(PARKIND,JZIND,12)=ASTA(1,12)
      LAXSTAA(PARKIND,JZIND,13)=ASTA(2,1)
      LAXSTAA(PARKIND,JZIND,14)=ASTA(2,2)
      LAXSTAA(PARKIND,JZIND,15)=ASTA(2,3)
      LAXSTAA(PARKIND,JZIND,16)=ASTA(2,4)
      LAXSTAA(PARKIND,JZIND,17)=ASTA(2,5)
      LAXSTAA(PARKIND,JZIND,18)=ASTA(2,6)
      LAXSTAA(PARKIND,JZIND,19)=ASTA(2,7)
      LAXSTAA(PARKIND,JZIND,20)=ASTA(2,8)
      LAXSTAA(PARKIND,JZIND,21)=ASTA(2,9)
      LAXSTAA(PARKIND,JZIND,22)=ASTA(2,10)
      LAXSTAA(PARKIND,JZIND,23)=ASTA(2,11)
      LAXSTAA(PARKIND,JZIND,24)=ASTA(2,12) 
      LAXSTAA(PARKIND,JZIND,25)=ASTA(5,1)
C ----------------------------------------------------
      IF(LDETAIL) THEN                             
      WRITE(26,8047) IZ,PARKIND,JZ,JZIND
 8047 FORMAT(' IZ=',I4,' PARKIND=',I2,' JZ=',I4,
     *       ' JZIND=',I2)
      DO 8147 K=1,24
      WRITE(26,8148) K,LAXTPROB(PARKIND,JZIND,K),
     *        LAXSTAP(PARKIND,JZIND,K),LAXSTAA(PARKIND,JZIND,K)
 8148 FORMAT(1X,I2,' LAXTPROB=',F8.3,' OSTA=',I4,' ASTA=',I4)
 8147 CONTINUE
      DO 8149 K=25,34
      WRITE(26,8150) K,LAXTPROB(PARKIND,JZIND,K),
     *                   LAXSTAP(PARKIND,JZIND,K)
 8150 FORMAT(1X,I2,' LAXTPROB=',F8.3,' OSTA=',I4)
 8149 CONTINUE
      DO 8151 K=35,41
      WRITE(26,8152) K,LAXTPROB(PARKIND,JZIND,K)
 8152 FORMAT(1X,I2,'  TRNPROB=',F8.3)
 8151 CONTINUE
      WRITE(26,8153) LAXTPROB(PARKIND,JZIND,42)
 8153 FORMAT(1X,'42','    LSTRN=',F8.3//)
      END IF
C -------------------------------------------------------------------
      END IF
C
C SAVE TRANSIT PROBABILITIES & STATION VALUES 
C FOR FLY-AWAY LOT CHOICE MODEL
C
      IF(LAX.AND.LAXTRN.AND.FLYAIND.GT.0.AND.M.EQ.1.AND.C.EQ.2) THEN
      LAXFPROB(IZ,FLYAIND,1)=CWPROB(1)
      LAXFPROB(IZ,FLYAIND,2)=CWPROB(2)
      LAXFPROB(IZ,FLYAIND,3)=CBPROB(1)
      LAXFPROB(IZ,FLYAIND,4)=CBPROB(2)
      LAXFPROB(IZ,FLYAIND,5)=CPPROB(1)
      LAXFPROB(IZ,FLYAIND,6)=CPPROB(2)
      LAXFPROB(IZ,FLYAIND,7)=CPPROB(3)
      LAXFPROB(IZ,FLYAIND,8)=CPPROB(4) 
      LAXFPROB(IZ,FLYAIND,9)=CKPROB(1)
      LAXFPROB(IZ,FLYAIND,10)=CKPROB(2)
      LAXFPROB(IZ,FLYAIND,11)=CKPROB(3)
      LAXFPROB(IZ,FLYAIND,12)=CKPROB(4)  
      LAXFPROB(IZ,FLYAIND,13)=UWPROB(1)
      LAXFPROB(IZ,FLYAIND,14)=UWPROB(2)
      LAXFPROB(IZ,FLYAIND,15)=UBPROB(1)
      LAXFPROB(IZ,FLYAIND,16)=UBPROB(2)
      LAXFPROB(IZ,FLYAIND,17)=UPPROB(1)
      LAXFPROB(IZ,FLYAIND,18)=UPPROB(2)
      LAXFPROB(IZ,FLYAIND,19)=UPPROB(3)
      LAXFPROB(IZ,FLYAIND,20)=UPPROB(4) 
      LAXFPROB(IZ,FLYAIND,21)=UKPROB(1)
      LAXFPROB(IZ,FLYAIND,22)=UKPROB(2)
      LAXFPROB(IZ,FLYAIND,23)=UKPROB(3)
      LAXFPROB(IZ,FLYAIND,24)=UKPROB(4)
      LAXFPROB(IZ,FLYAIND,25)=BUSPROB(1)
      LAXFPROB(IZ,FLYAIND,26)=BUSPROB(2)
      LAXFPROB(IZ,FLYAIND,27)=RPDPROB(1)
      LAXFPROB(IZ,FLYAIND,28)=RPDPROB(2) 
      LAXFPROB(IZ,FLYAIND,29)=EXPPROB(1)
      LAXFPROB(IZ,FLYAIND,30)=EXPPROB(2)
      LAXFPROB(IZ,FLYAIND,31)=BWPROB(1)
      LAXFPROB(IZ,FLYAIND,32)=BWPROB(2)
      LAXFPROB(IZ,FLYAIND,33)=BRTPROB(1)
      LAXFPROB(IZ,FLYAIND,34)=BRTPROB(2)
      LAXFPROB(IZ,FLYAIND,35)=TRNPROB(1)
      LAXFPROB(IZ,FLYAIND,36)=TRNPROB(2)      
      LAXFPROB(IZ,FLYAIND,37)=TRNPROB(3)      
      LAXFPROB(IZ,FLYAIND,38)=TRNPROB(4)      
      LAXFPROB(IZ,FLYAIND,39)=TRNPROB(5)      
      LAXFPROB(IZ,FLYAIND,40)=TRNPROB(6)
      LAXFPROB(IZ,FLYAIND,41)=TRNPROB(7)
      LAXFPROB(IZ,FLYAIND,42)=LSTRN
      LAXSTAFP(IZ,FLYAIND,1)=OSTA(1,1)
      LAXSTAFP(IZ,FLYAIND,2)=OSTA(1,2)
      LAXSTAFP(IZ,FLYAIND,3)=OSTA(1,3)
      LAXSTAFP(IZ,FLYAIND,4)=OSTA(1,4)
      LAXSTAFP(IZ,FLYAIND,5)=OSTA(1,5)
      LAXSTAFP(IZ,FLYAIND,6)=OSTA(1,6)
      LAXSTAFP(IZ,FLYAIND,7)=OSTA(1,7)
      LAXSTAFP(IZ,FLYAIND,8)=OSTA(1,8)
      LAXSTAFP(IZ,FLYAIND,9)=OSTA(1,9)
      LAXSTAFP(IZ,FLYAIND,10)=OSTA(1,10)
      LAXSTAFP(IZ,FLYAIND,11)=OSTA(1,11)
      LAXSTAFP(IZ,FLYAIND,12)=OSTA(1,12)
      LAXSTAFP(IZ,FLYAIND,13)=OSTA(2,1)
      LAXSTAFP(IZ,FLYAIND,14)=OSTA(2,2)
      LAXSTAFP(IZ,FLYAIND,15)=OSTA(2,3)
      LAXSTAFP(IZ,FLYAIND,16)=OSTA(2,4)
      LAXSTAFP(IZ,FLYAIND,17)=OSTA(2,5)
      LAXSTAFP(IZ,FLYAIND,18)=OSTA(2,6)
      LAXSTAFP(IZ,FLYAIND,19)=OSTA(2,7)
      LAXSTAFP(IZ,FLYAIND,20)=OSTA(2,8)
      LAXSTAFP(IZ,FLYAIND,21)=OSTA(2,9)
      LAXSTAFP(IZ,FLYAIND,22)=OSTA(2,10)
      LAXSTAFP(IZ,FLYAIND,23)=OSTA(2,11)
      LAXSTAFP(IZ,FLYAIND,24)=OSTA(2,12)
      LAXSTAFP(IZ,FLYAIND,25)=OSTA(5,1)
      LAXSTAFP(IZ,FLYAIND,30)=CSTAE
      LAXSTAFP(IZ,FLYAIND,32)=CSTAT
      LAXSTAFA(IZ,FLYAIND,1)=ASTA(1,1)
      LAXSTAFA(IZ,FLYAIND,2)=ASTA(1,2)
      LAXSTAFA(IZ,FLYAIND,3)=ASTA(1,3)
      LAXSTAFA(IZ,FLYAIND,4)=ASTA(1,4)
      LAXSTAFA(IZ,FLYAIND,5)=ASTA(1,5)
      LAXSTAFA(IZ,FLYAIND,6)=ASTA(1,6)
      LAXSTAFA(IZ,FLYAIND,7)=ASTA(1,7)
      LAXSTAFA(IZ,FLYAIND,8)=ASTA(1,8)
      LAXSTAFA(IZ,FLYAIND,9)=ASTA(1,9)
      LAXSTAFA(IZ,FLYAIND,10)=ASTA(1,10)
      LAXSTAFA(IZ,FLYAIND,11)=ASTA(1,11)
      LAXSTAFA(IZ,FLYAIND,12)=ASTA(1,12)
      LAXSTAFA(IZ,FLYAIND,13)=ASTA(2,1)
      LAXSTAFA(IZ,FLYAIND,14)=ASTA(2,2)
      LAXSTAFA(IZ,FLYAIND,15)=ASTA(2,3)
      LAXSTAFA(IZ,FLYAIND,16)=ASTA(2,4)
      LAXSTAFA(IZ,FLYAIND,17)=ASTA(2,5)
      LAXSTAFA(IZ,FLYAIND,18)=ASTA(2,6)
      LAXSTAFA(IZ,FLYAIND,19)=ASTA(2,7)
      LAXSTAFA(IZ,FLYAIND,20)=ASTA(2,8)
      LAXSTAFA(IZ,FLYAIND,21)=ASTA(2,9)
      LAXSTAFA(IZ,FLYAIND,22)=ASTA(2,10)
      LAXSTAFA(IZ,FLYAIND,23)=ASTA(2,11)
      LAXSTAFA(IZ,FLYAIND,24)=ASTA(2,12)
      LAXSTAFA(IZ,FLYAIND,25)=ASTA(5,1) 
C ----------------------------------------------------
      IF(LDETAIL) THEN                             
      WRITE(26,8247) IZ,FLYAIND,JZ
 8247 FORMAT(' IZ=',I4,' FLYAIND=',I2,' JZ=',I4)
      DO 8347 K=1,24
      WRITE(26,8248) K,LAXFPROB(IZ,FLYAIND,K),
     *        LAXSTAFP(IZ,FLYAIND,K),LAXSTAFA(IZ,FLYAIND,K)
 8248 FORMAT(1X,I2,' LAXFPROB=',F8.3,' OSTA=',I4,' ASTA=',I4)
 8347 CONTINUE
      DO 8349 K=25,34 
      WRITE(26,8350) K,LAXFPROB(IZ,FLYAIND,K),
     *                   LAXSTAFP(IZ,FLYAIND,K)
 8350 FORMAT(1X,I2,' LAXFPROB=',F8.3,' OSTA=',I4)
 8349 CONTINUE
      DO 8351 K=35,41
      WRITE(26,8352) K,LAXFPROB(IZ,FLYAIND,K)
 8352 FORMAT(1X,I2,'  TRNPROB=',F8.3)
 8351 CONTINUE
      WRITE(26,8353) LAXFPROB(IZ,FLYAIND,42)
 8353 FORMAT(1X,'42','    LSTRN=',F8.3//)
      END IF
C -------------------------------------------------------------------
      END IF
C
C SAVE TRANSIT PROBABILITIES & STATION VALUES 
C FOR RENTAL FACILITY CHOICE MODEL
C
      IF(LAX.AND.LAXTRN.AND.RNTLIND.GT.0.AND.
     *   M.EQ.1.AND.C.EQ.2.AND.FLYAIND.EQ.0) THEN
      JZIND=0
      DO 8142 K=1,50
      IF(AEQUIV(K).EQ.JZ) JZIND=K
 8142 CONTINUE
      IF(JZIND.LE.0) THEN
      WRITE(26,8143) JZ
      WRITE(*,8143)  JZ
 8143 FORMAT(1X,'MTAMC 8043 (F) NO RENTAL INDICATOR MATCH FOR',
     *          ' ATTRACTION ZONE ',I4)
      STOP 8143
      END IF 
      LAXRPROB(RNTLIND,JZIND,1)=CWPROB(1)
      LAXRPROB(RNTLIND,JZIND,2)=CWPROB(2)
      LAXRPROB(RNTLIND,JZIND,3)=CBPROB(1)
      LAXRPROB(RNTLIND,JZIND,4)=CBPROB(2)
      LAXRPROB(RNTLIND,JZIND,5)=CPPROB(1)
      LAXRPROB(RNTLIND,JZIND,6)=CPPROB(2)
      LAXRPROB(RNTLIND,JZIND,7)=CPPROB(3)
      LAXRPROB(RNTLIND,JZIND,8)=CPPROB(4) 
      LAXRPROB(RNTLIND,JZIND,9)=CKPROB(1)
      LAXRPROB(RNTLIND,JZIND,10)=CKPROB(2)
      LAXRPROB(RNTLIND,JZIND,11)=CKPROB(3)
      LAXRPROB(RNTLIND,JZIND,12)=CKPROB(4)  
      LAXRPROB(RNTLIND,JZIND,13)=UWPROB(1)
      LAXRPROB(RNTLIND,JZIND,14)=UWPROB(2)
      LAXRPROB(RNTLIND,JZIND,15)=UBPROB(1)
      LAXRPROB(RNTLIND,JZIND,16)=UBPROB(2)
      LAXRPROB(RNTLIND,JZIND,17)=UPPROB(1)
      LAXRPROB(RNTLIND,JZIND,18)=UPPROB(2)
      LAXRPROB(RNTLIND,JZIND,19)=UPPROB(3)
      LAXRPROB(RNTLIND,JZIND,20)=UPPROB(4) 
      LAXRPROB(RNTLIND,JZIND,21)=UKPROB(1)
      LAXRPROB(RNTLIND,JZIND,22)=UKPROB(2)
      LAXRPROB(RNTLIND,JZIND,23)=UKPROB(3)
      LAXRPROB(RNTLIND,JZIND,24)=UKPROB(4)
      LAXRPROB(RNTLIND,JZIND,25)=BUSPROB(1)
      LAXRPROB(RNTLIND,JZIND,26)=BUSPROB(2)
      LAXRPROB(RNTLIND,JZIND,27)=RPDPROB(1)
      LAXRPROB(RNTLIND,JZIND,28)=RPDPROB(2) 
      LAXRPROB(RNTLIND,JZIND,29)=EXPPROB(1)
      LAXRPROB(RNTLIND,JZIND,30)=EXPPROB(2)
      LAXRPROB(RNTLIND,JZIND,31)=BWPROB(1)
      LAXRPROB(RNTLIND,JZIND,32)=BWPROB(2)
      LAXRPROB(RNTLIND,JZIND,33)=BRTPROB(1)
      LAXRPROB(RNTLIND,JZIND,34)=BRTPROB(2)
      LAXRPROB(RNTLIND,JZIND,35)=TRNPROB(1)
      LAXRPROB(RNTLIND,JZIND,36)=TRNPROB(2)      
      LAXRPROB(RNTLIND,JZIND,37)=TRNPROB(3)      
      LAXRPROB(RNTLIND,JZIND,38)=TRNPROB(4)      
      LAXRPROB(RNTLIND,JZIND,39)=TRNPROB(5)      
      LAXRPROB(RNTLIND,JZIND,40)=TRNPROB(6)
      LAXRPROB(RNTLIND,JZIND,41)=TRNPROB(7)
      LAXRPROB(RNTLIND,JZIND,42)=LSTRN
      LAXRNTP(RNTLIND,JZIND,1)=OSTA(1,1)
      LAXRNTP(RNTLIND,JZIND,2)=OSTA(1,2)
      LAXRNTP(RNTLIND,JZIND,3)=OSTA(1,3)
      LAXRNTP(RNTLIND,JZIND,4)=OSTA(1,4)
      LAXRNTP(RNTLIND,JZIND,5)=OSTA(1,5)
      LAXRNTP(RNTLIND,JZIND,6)=OSTA(1,6)
      LAXRNTP(RNTLIND,JZIND,7)=OSTA(1,7)
      LAXRNTP(RNTLIND,JZIND,8)=OSTA(1,8)
      LAXRNTP(RNTLIND,JZIND,9)=OSTA(1,9)
      LAXRNTP(RNTLIND,JZIND,10)=OSTA(1,10)
      LAXRNTP(RNTLIND,JZIND,11)=OSTA(1,11)
      LAXRNTP(RNTLIND,JZIND,12)=OSTA(1,12)
      LAXRNTP(RNTLIND,JZIND,13)=OSTA(2,1)
      LAXRNTP(RNTLIND,JZIND,14)=OSTA(2,2)
      LAXRNTP(RNTLIND,JZIND,15)=OSTA(2,3)
      LAXRNTP(RNTLIND,JZIND,16)=OSTA(2,4)
      LAXRNTP(RNTLIND,JZIND,17)=OSTA(2,5)
      LAXRNTP(RNTLIND,JZIND,18)=OSTA(2,6)
      LAXRNTP(RNTLIND,JZIND,19)=OSTA(2,7)
      LAXRNTP(RNTLIND,JZIND,20)=OSTA(2,8)
      LAXRNTP(RNTLIND,JZIND,21)=OSTA(2,9)
      LAXRNTP(RNTLIND,JZIND,22)=OSTA(2,10)
      LAXRNTP(RNTLIND,JZIND,23)=OSTA(2,11)
      LAXRNTP(RNTLIND,JZIND,24)=OSTA(2,12)
      LAXRNTP(RNTLIND,JZIND,25)=OSTA(5,1)
      LAXRNTP(RNTLIND,JZIND,30)=CSTAE
      LAXRNTP(RNTLIND,JZIND,32)=CSTAT
      LAXRNTA(RNTLIND,JZIND,1)=ASTA(1,1)
      LAXRNTA(RNTLIND,JZIND,2)=ASTA(1,2)
      LAXRNTA(RNTLIND,JZIND,3)=ASTA(1,3)
      LAXRNTA(RNTLIND,JZIND,4)=ASTA(1,4)
      LAXRNTA(RNTLIND,JZIND,5)=ASTA(1,5)
      LAXRNTA(RNTLIND,JZIND,6)=ASTA(1,6)
      LAXRNTA(RNTLIND,JZIND,7)=ASTA(1,7)
      LAXRNTA(RNTLIND,JZIND,8)=ASTA(1,8)
      LAXRNTA(RNTLIND,JZIND,9)=ASTA(1,9)
      LAXRNTA(RNTLIND,JZIND,10)=ASTA(1,10)
      LAXRNTA(RNTLIND,JZIND,11)=ASTA(1,11)
      LAXRNTA(RNTLIND,JZIND,12)=ASTA(1,12)
      LAXRNTA(RNTLIND,JZIND,13)=ASTA(2,1)
      LAXRNTA(RNTLIND,JZIND,14)=ASTA(2,2)
      LAXRNTA(RNTLIND,JZIND,15)=ASTA(2,3)
      LAXRNTA(RNTLIND,JZIND,16)=ASTA(2,4)
      LAXRNTA(RNTLIND,JZIND,17)=ASTA(2,5)
      LAXRNTA(RNTLIND,JZIND,18)=ASTA(2,6)
      LAXRNTA(RNTLIND,JZIND,19)=ASTA(2,7)
      LAXRNTA(RNTLIND,JZIND,20)=ASTA(2,8)
      LAXRNTA(RNTLIND,JZIND,21)=ASTA(2,9)
      LAXRNTA(RNTLIND,JZIND,22)=ASTA(2,10)
      LAXRNTA(RNTLIND,JZIND,23)=ASTA(2,11)
      LAXRNTA(RNTLIND,JZIND,24)=ASTA(2,12)
      LAXRNTA(RNTLIND,JZIND,25)=ASTA(5,1)
C ----------------------------------------------------
      IF(LDETAIL) THEN                             
      WRITE(26,8747) IZ,RNTLIND,JZ,JZIND
 8747 FORMAT(' IZ=',I4,' RNTLIND=',I2,' JZ=',I4,' JZIND=',I2)
      DO 8749 K=1,24
      WRITE(26,8748) K,LAXRPROB(RNTLIND,JZIND,K),
     *        LAXRNTP(RNTLIND,JZIND,K),LAXRNTA(RNTLIND,JZIND,K)
 8748 FORMAT(1X,I2,' LAXRPROB=',F8.3,' OSTA=',I4,' ASTA=',I4)
 8749 CONTINUE
      DO 8849 K=25,34 
      WRITE(26,8750) K,LAXRPROB(RNTLIND,JZIND,K),
     *                   LAXRNTP(RNTLIND,JZIND,K)
 8750 FORMAT(1X,I2,' LAXRPROB=',F8.3,' OSTA=',I4)
 8849 CONTINUE
      DO 8751 K=35,41
      WRITE(26,8752) K,LAXRPROB(RNTLIND,JZIND,K)
 8752 FORMAT(1X,I2,'  TRNPROB=',F8.3)
 8751 CONTINUE
      WRITE(26,8853) LAXRPROB(RNTLIND,JZIND,42)
 8853 FORMAT(1X,'42','    LSTRN=',F8.3//)
      END IF
C -------------------------------------------------------------------
      END IF
C
C SAVE TRANSIT PROBABILITIES & STATION VALUES 
C FOR ITF INTERCEPT TRANSIT CHOICE
C
      IF(LAX.AND.LAXTRN.AND.(ITFZONE.EQ.IZ).AND.
     *   M.EQ.1.AND.C.EQ.2.AND.FLYAIND.EQ.0) THEN
      JZIND=0
      DO 8242 K=1,50
      IF(AEQUIV(K).EQ.JZ) JZIND=K
 8242 CONTINUE
      IF(JZIND.LE.0) THEN
      WRITE(26,8243) JZ
      WRITE(*,8243)  JZ
 8243 FORMAT(1X,'MTAMC 8243 (F) NO ITF INDICATOR MATCH FOR',
     *          ' ATTRACTION ZONE ',I4)
      STOP 8243
      END IF 
      LAXIPROB(JZIND,1)=CWPROB(1)
      LAXIPROB(JZIND,2)=CWPROB(2)
      LAXIPROB(JZIND,3)=CBPROB(1)
      LAXIPROB(JZIND,4)=CBPROB(2)
      LAXIPROB(JZIND,5)=CPPROB(1)
      LAXIPROB(JZIND,6)=CPPROB(2)
      LAXIPROB(JZIND,7)=CPPROB(3)
      LAXIPROB(JZIND,8)=CPPROB(4) 
      LAXIPROB(JZIND,9)=CKPROB(1)
      LAXIPROB(JZIND,10)=CKPROB(2)
      LAXIPROB(JZIND,11)=CKPROB(3)
      LAXIPROB(JZIND,12)=CKPROB(4)  
      LAXIPROB(JZIND,13)=UWPROB(1)
      LAXIPROB(JZIND,14)=UWPROB(2)
      LAXIPROB(JZIND,15)=UBPROB(1)
      LAXIPROB(JZIND,16)=UBPROB(2)
      LAXIPROB(JZIND,17)=UPPROB(1)
      LAXIPROB(JZIND,18)=UPPROB(2)
      LAXIPROB(JZIND,19)=UPPROB(3)
      LAXIPROB(JZIND,20)=UPPROB(4) 
      LAXIPROB(JZIND,21)=UKPROB(1)
      LAXIPROB(JZIND,22)=UKPROB(2)
      LAXIPROB(JZIND,23)=UKPROB(3)
      LAXIPROB(JZIND,24)=UKPROB(4)
      LAXIPROB(JZIND,25)=BUSPROB(1)
      LAXIPROB(JZIND,26)=BUSPROB(2)
      LAXIPROB(JZIND,27)=RPDPROB(1)
      LAXIPROB(JZIND,28)=RPDPROB(2) 
      LAXIPROB(JZIND,29)=EXPPROB(1)
      LAXIPROB(JZIND,30)=EXPPROB(2)
      LAXIPROB(JZIND,31)=BWPROB(1)
      LAXIPROB(JZIND,32)=BWPROB(2)
      LAXIPROB(JZIND,33)=BRTPROB(1)
      LAXIPROB(JZIND,34)=BRTPROB(2)
      LAXIPROB(JZIND,35)=TRNPROB(1)
      LAXIPROB(JZIND,36)=TRNPROB(2)      
      LAXIPROB(JZIND,37)=TRNPROB(3)      
      LAXIPROB(JZIND,38)=TRNPROB(4)      
      LAXIPROB(JZIND,39)=TRNPROB(5)      
      LAXIPROB(JZIND,40)=TRNPROB(6)
      LAXIPROB(JZIND,41)=TRNPROB(7)
      LAXIPROB(JZIND,42)=LSTRN
      LAXITFP(JZIND,1)=OSTA(1,1)
      LAXITFP(JZIND,2)=OSTA(1,2)
      LAXITFP(JZIND,3)=OSTA(1,3)
      LAXITFP(JZIND,4)=OSTA(1,4)
      LAXITFP(JZIND,5)=OSTA(1,5)
      LAXITFP(JZIND,6)=OSTA(1,6)
      LAXITFP(JZIND,7)=OSTA(1,7)
      LAXITFP(JZIND,8)=OSTA(1,8)
      LAXITFP(JZIND,9)=OSTA(1,9)
      LAXITFP(JZIND,10)=OSTA(1,10)
      LAXITFP(JZIND,11)=OSTA(1,11)
      LAXITFP(JZIND,12)=OSTA(1,12)
      LAXITFP(JZIND,13)=OSTA(2,1)
      LAXITFP(JZIND,14)=OSTA(2,2)
      LAXITFP(JZIND,15)=OSTA(2,3)
      LAXITFP(JZIND,16)=OSTA(2,4)
      LAXITFP(JZIND,17)=OSTA(2,5)
      LAXITFP(JZIND,18)=OSTA(2,6)
      LAXITFP(JZIND,19)=OSTA(2,7)
      LAXITFP(JZIND,20)=OSTA(2,8)
      LAXITFP(JZIND,21)=OSTA(2,9)
      LAXITFP(JZIND,22)=OSTA(2,10)
      LAXITFP(JZIND,23)=OSTA(2,11)
      LAXITFP(JZIND,24)=OSTA(2,12)
      LAXITFP(JZIND,25)=OSTA(5,1)
      LAXITFP(JZIND,30)=CSTAE
      LAXITFP(JZIND,32)=CSTAT
      LAXITFA(JZIND,1)=ASTA(1,1)
      LAXITFA(JZIND,2)=ASTA(1,2)
      LAXITFA(JZIND,3)=ASTA(1,3)
      LAXITFA(JZIND,4)=ASTA(1,4)
      LAXITFA(JZIND,5)=ASTA(1,5)
      LAXITFA(JZIND,6)=ASTA(1,6)
      LAXITFA(JZIND,7)=ASTA(1,7)
      LAXITFA(JZIND,8)=ASTA(1,8)
      LAXITFA(JZIND,9)=ASTA(1,9)
      LAXITFA(JZIND,10)=ASTA(1,10)
      LAXITFA(JZIND,11)=ASTA(1,11)
      LAXITFA(JZIND,12)=ASTA(1,12)
      LAXITFA(JZIND,13)=ASTA(2,1)
      LAXITFA(JZIND,14)=ASTA(2,2)
      LAXITFA(JZIND,15)=ASTA(2,3)
      LAXITFA(JZIND,16)=ASTA(2,4)
      LAXITFA(JZIND,17)=ASTA(2,5)
      LAXITFA(JZIND,18)=ASTA(2,6)
      LAXITFA(JZIND,19)=ASTA(2,7)
      LAXITFA(JZIND,20)=ASTA(2,8)
      LAXITFA(JZIND,21)=ASTA(2,9)
      LAXITFA(JZIND,22)=ASTA(2,10)
      LAXITFA(JZIND,23)=ASTA(2,11)
      LAXITFA(JZIND,24)=ASTA(2,12)
      LAXITFA(JZIND,25)=ASTA(5,1)
      END IF
C
C SAVE TRANSIT PROBABILITIES & STATION VALUES 
C FOR ITF ZONE 2 INTERCEPT TRANSIT CHOICE
C
      IF(LAX.AND.LAXTRN.AND.(ITFZONE2.EQ.IZ).AND.
     *   M.EQ.1.AND.C.EQ.2.AND.FLYAIND.EQ.0) THEN
      JZIND=0
      DO K=1,50
      IF(AEQUIV(K).EQ.JZ) JZIND=K
      END DO
      IF(JZIND.LE.0) THEN
      WRITE(26,8244) JZ
      WRITE(*,8244)  JZ
 8244 FORMAT(1X,'MTAMC 8244 (F) NO ITF2 INDICATOR MATCH FOR',
     *          ' ATTRACTION ZONE ',I4)
      STOP 8244
      END IF 
      LAXI2PROB(JZIND,1)=CWPROB(1)
      LAXI2PROB(JZIND,2)=CWPROB(2)
      LAXI2PROB(JZIND,3)=CBPROB(1)
      LAXI2PROB(JZIND,4)=CBPROB(2)
      LAXI2PROB(JZIND,5)=CPPROB(1)
      LAXI2PROB(JZIND,6)=CPPROB(2)
      LAXI2PROB(JZIND,7)=CPPROB(3)
      LAXI2PROB(JZIND,8)=CPPROB(4) 
      LAXI2PROB(JZIND,9)=CKPROB(1)
      LAXI2PROB(JZIND,10)=CKPROB(2)
      LAXI2PROB(JZIND,11)=CKPROB(3)
      LAXI2PROB(JZIND,12)=CKPROB(4)  
      LAXI2PROB(JZIND,13)=UWPROB(1)
      LAXI2PROB(JZIND,14)=UWPROB(2)
      LAXI2PROB(JZIND,15)=UBPROB(1)
      LAXI2PROB(JZIND,16)=UBPROB(2)
      LAXI2PROB(JZIND,17)=UPPROB(1)
      LAXI2PROB(JZIND,18)=UPPROB(2)
      LAXI2PROB(JZIND,19)=UPPROB(3)
      LAXI2PROB(JZIND,20)=UPPROB(4) 
      LAXI2PROB(JZIND,21)=UKPROB(1)
      LAXI2PROB(JZIND,22)=UKPROB(2)
      LAXI2PROB(JZIND,23)=UKPROB(3)
      LAXI2PROB(JZIND,24)=UKPROB(4)
      LAXI2PROB(JZIND,25)=BUSPROB(1)
      LAXI2PROB(JZIND,26)=BUSPROB(2)
      LAXI2PROB(JZIND,27)=RPDPROB(1)
      LAXI2PROB(JZIND,28)=RPDPROB(2) 
      LAXI2PROB(JZIND,29)=EXPPROB(1)
      LAXI2PROB(JZIND,30)=EXPPROB(2)
      LAXI2PROB(JZIND,31)=BWPROB(1)
      LAXI2PROB(JZIND,32)=BWPROB(2)
      LAXI2PROB(JZIND,33)=BRTPROB(1)
      LAXI2PROB(JZIND,34)=BRTPROB(2)
      LAXI2PROB(JZIND,35)=TRNPROB(1)
      LAXI2PROB(JZIND,36)=TRNPROB(2)      
      LAXI2PROB(JZIND,37)=TRNPROB(3)      
      LAXI2PROB(JZIND,38)=TRNPROB(4)      
      LAXI2PROB(JZIND,39)=TRNPROB(5)      
      LAXI2PROB(JZIND,40)=TRNPROB(6)
      LAXI2PROB(JZIND,41)=TRNPROB(7)
      LAXI2PROB(JZIND,42)=LSTRN
      LAXITF2P(JZIND,1)=OSTA(1,1)
      LAXITF2P(JZIND,2)=OSTA(1,2)
      LAXITF2P(JZIND,3)=OSTA(1,3)
      LAXITF2P(JZIND,4)=OSTA(1,4)
      LAXITF2P(JZIND,5)=OSTA(1,5)
      LAXITF2P(JZIND,6)=OSTA(1,6)
      LAXITF2P(JZIND,7)=OSTA(1,7)
      LAXITF2P(JZIND,8)=OSTA(1,8)
      LAXITF2P(JZIND,9)=OSTA(1,9)
      LAXITF2P(JZIND,10)=OSTA(1,10)
      LAXITF2P(JZIND,11)=OSTA(1,11)
      LAXITF2P(JZIND,12)=OSTA(1,12)
      LAXITF2P(JZIND,13)=OSTA(2,1)
      LAXITF2P(JZIND,14)=OSTA(2,2)
      LAXITF2P(JZIND,15)=OSTA(2,3)
      LAXITF2P(JZIND,16)=OSTA(2,4)
      LAXITF2P(JZIND,17)=OSTA(2,5)
      LAXITF2P(JZIND,18)=OSTA(2,6)
      LAXITF2P(JZIND,19)=OSTA(2,7)
      LAXITF2P(JZIND,20)=OSTA(2,8)
      LAXITF2P(JZIND,21)=OSTA(2,9)
      LAXITF2P(JZIND,22)=OSTA(2,10)
      LAXITF2P(JZIND,23)=OSTA(2,11)
      LAXITF2P(JZIND,24)=OSTA(2,12)
      LAXITF2P(JZIND,25)=OSTA(5,1)
      LAXITF2P(JZIND,30)=CSTAE
      LAXITF2P(JZIND,32)=CSTAT
      LAXITF2A(JZIND,1)=ASTA(1,1)
      LAXITF2A(JZIND,2)=ASTA(1,2)
      LAXITF2A(JZIND,3)=ASTA(1,3)
      LAXITF2A(JZIND,4)=ASTA(1,4)
      LAXITF2A(JZIND,5)=ASTA(1,5)
      LAXITF2A(JZIND,6)=ASTA(1,6)
      LAXITF2A(JZIND,7)=ASTA(1,7)
      LAXITF2A(JZIND,8)=ASTA(1,8)
      LAXITF2A(JZIND,9)=ASTA(1,9)
      LAXITF2A(JZIND,10)=ASTA(1,10)
      LAXITF2A(JZIND,11)=ASTA(1,11)
      LAXITF2A(JZIND,12)=ASTA(1,12)
      LAXITF2A(JZIND,13)=ASTA(2,1)
      LAXITF2A(JZIND,14)=ASTA(2,2)
      LAXITF2A(JZIND,15)=ASTA(2,3)
      LAXITF2A(JZIND,16)=ASTA(2,4)
      LAXITF2A(JZIND,17)=ASTA(2,5)
      LAXITF2A(JZIND,18)=ASTA(2,6)
      LAXITF2A(JZIND,19)=ASTA(2,7)
      LAXITF2A(JZIND,20)=ASTA(2,8)
      LAXITF2A(JZIND,21)=ASTA(2,9)
      LAXITF2A(JZIND,22)=ASTA(2,10)
      LAXITF2A(JZIND,23)=ASTA(2,11)
      LAXITF2A(JZIND,24)=ASTA(2,12)
      LAXITF2A(JZIND,25)=ASTA(5,1)
      END IF
C =================================================
C
C SAVE TRANSIT LOGSUM FOR LOGSUM AVERAGING
C
      IF(TRNUSER.AND.M.LE.6) THEN
      TRNLSUM(JZ,1,M,C)=UTIL(60)
      TRNLSUM(JZ,2,M,C)=UTIL(61)
      TRNLSUM(JZ,3,M,C)=UTIL(62)
      TRNLSUM(JZ,4,M,C)=UTIL(63)
      TRNLSUM(JZ,5,M,C)=UTIL(74)
      TRNLSUM(JZ,6,M,C)=UTIL(69)
      TRNLSUM(JZ,7,M,C)=UTIL(70)
      END IF
C....................................................................
      IF(DEBUG.OR.(LDEBUG.AND.(.NOT.LAXTRN))) THEN
      WRITE(26,9013) M,(UTIL(K),EUTIL(K),K=60,63),UTIL(69),EUTIL(69),
     *               UTIL(74),EUTIL(74),
     *               UTIL(70),EUTIL(70),LSFLY,EUTILFLY
 9013 FORMAT(/1X,'SUMMARY OF PRIMARY TRANSIT UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,' ACCESS SEGMENT=',I1/
     *       1X,'                     ',6X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',3X,'----------',5X,
     *          '----------'/
     *       1X,'COMMUTER RAIL           ',F10.5,3X,E12.5/
     *       1X,'URBAN RAIL              ',F10.5,3X,E12.5/
     *       1X,'TRANSITWAY BUS          ',F10.5,3X,E12.5/
     *       1X,'EXPRESS BUS             ',F10.5,3X,E12.5/
     *       1X,'LOCAL BUS               ',F10.5,3X,E12.5/
     *       1X,'RAPID BUS               ',F10.5,3X,E12.5/
     *       1X,'BUS RAPID TRANSIT       ',F10.5,3X,E12.5/
     *       1X,'FLY-AWAY (LAX ONLY)     ',F10.5,3X,E12.5/)
      WRITE(26,9014) TRNPROB(1),TRNPROB(2),TRNPROB(3),
     *               TRNPROB(4),TRNPROB(6),TRNPROB(5),
     *               TRNPROB(7),TRNPROB(8),LSTRN
 9014 FORMAT(/1X,'SUMMARY OF TRANSIT CHOICE PROBABILITIES'/
     *       1X,'---------------------------------------------'/
     *       1X,'COMMUTER RAIL  =',F8.5/
     *       1X,'URBAN RAIL     =',F8.5/
     *       1X,'TRANSITWAY BUS =',F8.5/
     *       1X,'EXPRESS BUS    =',F8.5/
     *       1X,'LOCAL BUS      =',F8.5/
     *       1X,'RAPID BUS      =',F8.5/
     *       1X,'BUS RAPID (BRT)=',F8.5/
     *       1X,'FLY-AWAY (LAX) =',F8.5/
     *       1X,'LOGSUM         =',F10.5/)
      END IF
C.....................................................................  
C
C  DRIVE ALONE & SHARED RIDE PROBABILITIES
C
C ..DRIVE-ALONE UTILITY
      IF(LSDA.NE.0.0) THEN
	    UTIL(58)=LSUM2DA*LSUM3DA*LSDA + KDA(C)/LSUM1AUTO
      EUTIL(58)=EXP(UTIL(58))
	    ENDIF
C..SHARED RIDE UTILITY
      IF(LSSHR.NE.0.0) THEN
	    UTIL(59)=LSUM2SR*LSSHR + KSR(C)/LSUM1AUTO
      EUTIL(59)=EXP(UTIL(59))
	    ENDIF
C..AUTO SUBMODE PROBABILITIES
      ATPROB(1)=0.0
      ATPROB(2)=0.0
      LSAUTO=0.0
      DENOM=EUTIL(58)+EUTIL(59)
      IF(DENOM.GT.0.0) THEN
      LSAUTO=DLOG(DENOM)
      ATPROB(1)=EUTIL(58)/DENOM
      ATPROB(2)=1.0-ATPROB(1)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9015) UTIL(58),EUTIL(58),UTIL(59),EUTIL(59)
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
      UTIL(71)=LSUM1AUTO*LSAUTO + KAUT(C)
      EUTIL(71)=EXP(UTIL(71))
      ELSE
      AUTEXP=0.0
      ENDIF
      IF(LAX.AND.(.NOT.LAXTRN)) THEN
      IF(LSLOT.NE.0.0) THEN
      UTIL(71)=LSLOT
      EUTIL(71)=EXP(UTIL(71))
      ELSE
      AUTEXP=0.0
      END IF
      END IF
C..TRANSIT
      IF(LSTRN.NE.0.0) THEN
      ZCARP=DIM(ZHHD(22,IZ),15.0)
      ZCARA=DIM(ZHHD(22,JZ),15.0)
      UTIL(77)=LSUM1TRN*LSTRN + KTRN(C) + KTRNT(PINDEX,C)
      UTIL(77)=UTIL(77) 
      EUTIL(77)=EXP(UTIL(77))
      TLOGSUM(C,JZ)=TLOGSUM(C,JZ)+(MWALK(M)*UTIL(70))/COEFF(100)
      END IF
C..NON-MOTORIZED
      IF(NMOT.AND.(LSNMOT.NE.0.0)) THEN
      UTIL(78)=LSUM1NM*LSNMOT+KNMOT(C)
      EUTIL(78)=EXP(UTIL(78))
      END IF
C..MAAS                                                                        !Innovation
      IF(LSMAAS.NE.0.0) THEN
      UTIL(103)=LSUM1AUTO*LSMAAS + KMAAS(C)
      EUTIL(103)=EXP(UTIL(103))
      END IF
C..NON-TRANSIT EXPOENTIATED UTILITY FOR FTA USERBEN FILE
      AUTEXP=EUTIL(71)+EUTIL(78)+EUTIL(103)                                    !Innovation
C..PROBABILITIES
      MOTOR(1)=0.0
      MOTOR(2)=0.0
      MOTOR(3)=0.0
      MOTOR(4)=0.0
      LSMOT=0.0
      DENOM=EUTIL(71)+EUTIL(77)+EUTIL(78)+EUTIL(103)                           !Innovation
      IF(DENOM.GT.0.0) THEN
      LSMOT=DLOG(DENOM)
      MOTOR(4)=EUTIL(103)/DENOM                                                !Innovation
      MOTOR(1)=EUTIL(71)/DENOM
      MOTOR(3)=EUTIL(78)/DENOM
      MOTOR(2)=1.0-MOTOR(1)-MOTOR(3)-MOTOR(4)                                  !Innovation
      MOTOR(2)=AMAX1(MOTOR(2),0.0)
      ULOGSUM(C,JZ)=ULOGSUM(C,JZ)+(MWALK(M)*LSMOT)/COEFF(100)
      END IF
      IF(M.EQ.1.AND.C.EQ.2.AND.EVENTSP) THEN
      NI=SPEQUIV(JZ)
      MOTLSUM(IZ,NI)=LSMOT
      IF(VDETAIL) WRITE(26,8337) IZ,JZ,C,M,NI,MOTLSUM(IZ,NI)
 8337 FORMAT(' IZ=',I4,' JZ=',I4,' C=',I1,' M=',I1,' NI=',I4,
     *       ' MOTLSUM= ',F10.5)
      END IF
C
C  AIR PASSENGER PROBABILITY COMPUTATIONS
C
      IF(AIRPASS.AND.(.NOT.LAXTRN).AND.LAX.AND.AJZ.GT.0) THEN
      DO NI=1,10
      EUTLAIR(NI)=0.0
      PROBAIR(NI)=0.0
      END DO
      EUTLAIR(1)=EXP(UTLATXI)
      IF(UTLARNT.NE.0.0) EUTLAIR(2)=EXP(UTLARNT)
      EUTLAIR(3)=EXP(UTLALMO)
      EUTLAIR(4)=EXP(UTLADRP)
      IF(LSLOT.NE.0.0) EUTLAIR(5)=EXP(LSLOT+ACNST(5))
      EUTLAIR(6)=EXP(UTLAONC)
      IF(LSTRN.NE.0.0) EUTLAIR(7)=EXP(LSTRN+ACNST(7))
C...COMPUTE PRIVATE SHARES & LOGSUM
      DENOM=EUTLAIR(1)+EUTLAIR(2)+EUTLAIR(3)+EUTLAIR(4)+EUTLAIR(5)
      IF(DENOM.GT.0.0) LSPRV=LSUMA*DLOG(DENOM)
      DO NI=1,5
      PROBAIR(NI)=EUTLAIR(NI)/DENOM
      END DO
C...COMPUTE PUBLIC SHARES & LOGSUM
      DENOM=EUTLAIR(6)+EUTLAIR(7)
      IF(DENOM.GT.0.0) LSPUB=LSUMA*DLOG(DENOM)
      PROBAIR(6)=EUTLAIR(6)/DENOM
      PROBAIR(7)=(EUTLAIR(7)/DENOM)*(1.0-TRNPROB(8))
      PROBAIR(8)=(EUTLAIR(7)/DENOM)*TRNPROB(8)
C...COMPUTE FINAL SHARES
      DENOM=EXP(LSPUB+ACNST(9))+EXP(LSPRV)
      PROBAIR(10)=EXP(LSPRV)/DENOM
      PROBAIR(9)=1.0-PROBAIR(10)
C...COMPUTE THE NON-TRANSIT EXPONENTIATED UTILITY
      AUTEXP=EUTLAIR(1)+EUTLAIR(2)+EUTLAIR(3)+EUTLAIR(4)+EUTLAIR(5)+
     *       EUTLAIR(6)
      AUTEXP=EXP(LSUMA*LOG(AUTEXP))
C ------------------------------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,451) (PROBAIR(K),K=1,10)
  451 FORMAT(/
     *    1X,' AIR PASSENGER MODEL MODE SHARE VALUES'/
     *    1X ' -------------------------------------'/
     *    1X,'                                            PARKING'/
     *    1X,'   TAXI       RENTAL    LIMO      DROPOFF      LOT  ',
     *    '    ON-CALL     TRANSIT    FLYAWAY    PUBLIC     PRIVATE'/
     *    1X,'  SHARE       SHARE     SHARE      SHARE      SHARE ',
     *    '     SHARE       SHARE      SHARE      SHARE      SHARE'/
     *    1X,' ---------   --------  ---------  ---------  ---------',
     *    '  ---------  ---------  ---------  ---------  ---------'/
     *       10(2X,F9.5))
      END IF
C -------------------------------------------------------------------------      
      END IF
C
C  COMPUTE FTA RELATED USER BENEFIT INFORMATION
C
      IF(AIRPASS) THEN
      IF((M.LE.4).AND.(TWALK(1).GT.0.0))
     *    TSHAR(1)=TSHAR(1)+(MWALK(M)/TWALK(1))*(PROBAIR(7)*PROBAIR(9))
      IF((M.EQ.5.OR.M.EQ.6).AND.(TWALK(2).GT.0.0))
     *    TSHAR(2)=TSHAR(2)+(MWALK(M)/TWALK(2))*(PROBAIR(7)*PROBAIR(9))
      ELSE
      IF((M.LE.4).AND.(TWALK(1).GT.0.0)) THEN
          TSHAR(1)=TSHAR(1)+(MWALK(M)/TWALK(1))*MOTOR(2)
      END IF
      IF((M.EQ.5.OR.M.EQ.6).AND.(TWALK(2).GT.0.0)) THEN
          TSHAR(2)=TSHAR(2)+(MWALK(M)/TWALK(2))*MOTOR(2)
      END IF
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9017) UTIL(71),EUTIL(71),
     *               UTIL(77),EUTIL(77),ZHHD(16,JZ),
     *               UTIL(78),EUTIL(78),
     *               UTIL(103),EUTIL(103)
 9017 FORMAT(/1X,'SUMMARY OF BASIC UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',6X,'UTIL',10X,'EUTIL',
     *                                  6X,'DENSITY CONSTANT'/
     *       1X,'                     ',3X,'----------',5X,
     *          '----------',3X,'----------------'/
     *       1X,'AUTO                    ',F10.5,3X,E13.6/
     *       1X,'TRANSIT                 ',F10.5,3X,E13.6,3X,F8.3/
     *       1X,'NON-MOTORIZED           ',F10.5,3X,E13.6/
     *       1X,'MAAS                    ',F10.5,3X,E13.6)
      WRITE(26,9018) MOTOR(1),MOTOR(2),MOTOR(3),MOTOR(4),LSMOT,C,
     *               ALOGSUM(C,JZ),TLOGSUM(C,JZ),ULOGSUM(C,JZ),
     *               TSHAR(1),TSHAR(2)
 9018 FORMAT(/1X,'SUMMARY OF BASIC CHOICE PROBABILITIES'/
     *       1X,'---------------------------------------------'/
     *       1X,'AUTO          =',F8.5/ 
     *       1X,'TRANSIT       =',F8.5/
     *       1X,'NON-MOTORIZED =',F8.5/
     *       1X,'MAAS          =',F8.5//
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
      WRITE(26,9019) TSHAR(1),MWALK(M),TWALK(1),MOTOR(2),
     *               TRNPROB(1),TSHAR(2),TWALK(2)
 9019 FORMAT(/1X,'SUMMARY OF SUMMIT COMPUTATIONS'/
     *        1X,'------------------------------'/
     *        1X,'WALK/DRIVE TRANSIT SHARE=',F8.5/
     *        1X,'           MARKET  SHARE=',F8.5/
     *        1X,'CURRENT    MARKET  SHARE=',F8.5/
     *        1X,'TRANSIT            PROB =',F8.5/
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
      TMAAS=PERTRP*MOTOR(4)                                                    !Innovation
      TAUTO=PERTRP-TTRAN-TNMOT-TMAAS                                           !Innovation
C
C   AIR PASSENGER TRIP MATRIX VALUES
C
      IF(AIRPASS.AND.(.NOT.LAXTRN)) THEN
      TNMOT=0.0
      TTRAN=PERTRP*PROBAIR(9)*(PROBAIR(7)+PROBAIR(8))
      TDROP=PERTRP*PROBAIR(10)*PROBAIR(4)
      TLIMO=PERTRP*PROBAIR(10)*PROBAIR(3)
      TRNTL=PERTRP*PROBAIR(10)*PROBAIR(2)
      TTAXI=PERTRP*PROBAIR(10)*PROBAIR(1)
      TAUTO=PERTRP*PROBAIR(10)*PROBAIR(5)
      TONCL=PERTRP-TTRAN-TDROP-TLIMO-TRNTL-TTAXI-TAUTO
C ---------------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,452) PERTRP,TTRAN,TDROP,TLIMO,TRNTL,TTAXI,TAUTO,TONCL
  452 FORMAT(//' AIR PASSENGER TRIP VALUES'/
     *       ' -------------------------'/
     *       ' PERTRP  =',F8.2/
     *       ' TRANSIT ='F8.2/
     *       ' DROP-OFF='F8.2/
     *       ' LIMO    ='F8.2/
     *       ' RENTAL  ='F8.2/
     *       ' TAXI    ='F8.2/
     *       ' PARKED  ='F8.2/
     *       ' ON-CALL ='F8.2)
      END IF
C --------------------------------------------------------------
      END IF
C
      TDRV0=TAUTO*ATPROB(1)
      TDSHR=TAUTO-TDRV0
C                                                                              !Innovation
      TUBER=TMAAS*MAASPROB(2)
      TAXIMAAS=TMAAS-TUBER
      TAXIMAAS=AMAX1(TAXIMAAS,0.0)
c
      TCR=TTRAN*TRNPROB(1)
      TUR=TTRAN*TRNPROB(2)
      TWAY=TTRAN*TRNPROB(3)
      TEXP =TTRAN*TRNPROB(4)
      TRPD=TTRAN*TRNPROB(5)
      TBRT=TTRAN*TRNPROB(7)
      TFLY=TTRAN*TRNPROB(8)
      IF(TRNPROB(6).GT.0) THEN
      TLOC=TTRAN-(TCR+TUR+TWAY+TEXP+TRPD+TBRT+TFLY)
      ELSE
      TLOC=0.0
      END IF
      IF(JZ.EQ.LAXZ) URLAX=URLAX+TUR
C
      TNMWK=TNMOT*NMPROB(1)
      IF(ESCOOTER) THEN
      TNMSC=TNMOT*NMPROB(3)
      ELSE
      TNMSC=0.0
      END IF
      TNMBK=TNMOT*NMPROB(2)
c
      TDRV2=TDSHR*SRPROB(1)
      TDRV4=TDSHR*SRPROB(3)
      TDRV3=TDSHR-TDRV2-TDRV4
C
      TLOCW=TLOC*BUSPROB(1)
      TLOCB=TLOC*BUSPROB(3)
      TLOCD=TLOC-TLOCW-TLOCB
C
      TRPDW=TRPD*RPDPROB(1)
      TRPDB=TRPD*RPDPROB(3)
      TRPDD=TRPD-TRPDW-TRPDB
C
      TEXPW=TEXP*EXPPROB(1)
      TEXPB=TEXP*EXPPROB(3)
      TEXPD=TEXP-TEXPW-TEXPB
      IF(EIVT(2,JZ).GT.0) THEN
      DENOM=EIVT(2,JZ)/TIVT(2,JZ)
      INDEX=IDINT(DENOM*10)+1
      IF(INDEX.GT.11) INDEX=12
      WEXPTLF(INDEX)=WEXPTLF(INDEX)+TEXPW
      END IF
      
C
      TBRTW=TBRT*BRWPROB(3)
      TBRTB=TBRT*BRBPROB(3)
      TBRTBK=TBRT*BRBKPROB(3)
      TBRTP=TBRT*BRPPROB(5)
      TBRTK=TBRT-(TBRTW+TBRTB+TBRTP+TBRTBK)
      TBRTK=DMAX1(TBRTK,0.0)
      TBRTU=TBRTK*BRUPROB(6)
      TBRTK=TBRTK*BRUPROB(5)
      TBRTU=DMAX1(TBRTU,0.0)
C
      TCRW=TCR*CWPROB(3)
      TCRB=TCR*CBPROB(3)
      TCRBK=TCR*CBKPROB(3)
      TCRP=TCR*CPPROB(5)
      TCRK=TCR-(TCRW+TCRB+TCRP+TCRBK)
      TCRK=DMAX1(TCRK,0.0)
      TCRU=TCRK*CUPROB(6)
      TCRK=TCRK*CUPROB(5)
      TCRU=DMAX1(TCRU,0.0)
C
      TURW=TUR*UWPROB(3)
      TURB=TUR*UBPROB(3)
      TURBK=TUR*UBKPROB(3)
      TURP=TUR*UPPROB(5)
      TURK=TUR-(TURW+TURB+TURP+TURBK)
      TURK=DMAX1(TURK,0.0)
      TURU=TURK*UUPROB(6)
      TURK=TURK*UUPROB(5)
      TURU=DMAX1(TURU,0.0)
C
      TWAYW=TWAY*BWPROB(1)
      TWAYB=TWAY*BWPROB(3)
      TWAYD=TWAY-TWAYW-TWAYB
C
      INDEX=1
      IF(TIVT(3,JZ).GT.0) THEN
      INDEX=IFIX((WIVT(3,JZ)*10.0)/TIVT(3,JZ))+1
      END IF
      INDEX=MIN0(INDEX,11)
      WTRAT(INDEX)=WTRAT(INDEX)+TWAYW
C
      TCRW1=TCRW*CWPROB(1)
      TCRW2=TCRW-TCRW1
C
      TCRB1=TCRB*CBPROB(1)
      TCRB2=TCRB-TCRB1
C
      TCRBK1=TCRBK*CBKPROB(1)
      TCRBK2=TCRBK-TCRBK1
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
      TCRU1=TCRU*CUPROB(1)
      TCRU2=TCRU*CUPROB(2)
      TCRU3=TCRU*CUPROB(3)
      TCRU4=TCRU-(TCRU1+TCRU2+TCRU3)
      TCRU4=DMAX1(TCRU4,0.0)
C
      TURW1=TURW*UWPROB(1)
      TURW2=TURW-TURW1
C
      TURB1=TURB*UBPROB(1)
      TURB2=TURB-TURB1
C
      TURBK1=TURBK*UBKPROB(1)
      TURBK2=TURBK-TURBK1
C
      TURP1=TURP*UPPROB(1)
      TURP2=TURP*UPPROB(2)
      TURP3=TURP*UPPROB(3)
      IF(UPPROB(4).GT.0) THEN
      TURP4=TURP-(TURP1+TURP2+TURP3)
      TURP4=DMAX1(TURP4,0.0)
      ELSE
      TURP4=0.0
      TURP1=TURP-(TURP4+TURP2+TURP3)
      END IF
C
      TURK1=TURK*UKPROB(1)
      TURK2=TURK*UKPROB(2)
      TURK3=TURK*UKPROB(3)
      TURK4=TURK-(TURK1+TURK2+TURK3)
      TURK4=DMAX1(TURK4,0.0)
C
      TURU1=TURU*UUPROB(1)
      TURU2=TURU*UUPROB(2)
      TURU3=TURU*UUPROB(3)
      TURU4=TURU-(TURU1+TURU2+TURU3)
      TURU4=DMAX1(TURU4,0.0)
C
      TBRTW1=TBRTW*BRWPROB(1)
      TBRTW2=TBRTW-TBRTW1
C
      TBRTB1=TBRTB*BRBPROB(1)
      TBRTB2=TBRTB-TBRTB1
C
      TBRTBK1=TBRTBK*BRBKPROB(1)
      TBRTBK2=TBRTBK-TBRTBK1
C
      TBRTP1=TBRTP*BRPPROB(1)
      TBRTP2=TBRTP*BRPPROB(2)
      TBRTP3=TBRTP*BRPPROB(3)
      TBRTP4=TBRTP-(TBRTP1+TBRTP2+TBRTP3)
      TBRTP4=DMAX1(TBRTP4,0.0)
C
      TBRTK1=TBRTK*BRKPROB(1)
      TBRTK2=TBRTK*BRKPROB(2)
      TBRTK3=TBRTK*BRKPROB(3)
      TBRTK4=TBRTK-(TBRTK1+TBRTK2+TBRTK3)
      TBRTK4=DMAX1(TBRTK4,0.0)
C
      TBRTU1=TBRTU*BRUPROB(1)
      TBRTU2=TBRTU*BRUPROB(2)
      TBRTU3=TBRTU*BRUPROB(3)
      TBRTU4=TBRTU-(TBRTU1+TBRTU2+TBRTU3)
      TBRTU4=DMAX1(TBRTU4,0.0)
C
C..DRIVE ALONE TOLL AND NON-TOLL
      TDRV0N=TDRV0*DAPROB(1)
      TDRV0T=TDRV0-TDRV0N 
C..STORE DISTRICT LEVEL TRANSIT
C
      TRNDIST(DIZ,DJZ,1)=TRNDIST(DIZ,DJZ,1)+TLOCW
      TRNDIST(DIZ,DJZ,2)=TRNDIST(DIZ,DJZ,2)+TLOCD
      TRNDIST(DIZ,DJZ,3)=TRNDIST(DIZ,DJZ,3)+TRPDW
      TRNDIST(DIZ,DJZ,4)=TRNDIST(DIZ,DJZ,4)+TRPDD
      TRNDIST(DIZ,DJZ,5)=TRNDIST(DIZ,DJZ,5)+TEXPW
      TRNDIST(DIZ,DJZ,6)=TRNDIST(DIZ,DJZ,6)+TEXPD
      TRNDIST(DIZ,DJZ,7)=TRNDIST(DIZ,DJZ,7)+TWAYW
      TRNDIST(DIZ,DJZ,8)=TRNDIST(DIZ,DJZ,8)+TWAYD
      TRNDIST(DIZ,DJZ,9)=TRNDIST(DIZ,DJZ,9)+TBRTW
      TRNDIST(DIZ,DJZ,10)=TRNDIST(DIZ,DJZ,10)+TBRTB
      TRNDIST(DIZ,DJZ,11)=TRNDIST(DIZ,DJZ,11)+TCRW
      TRNDIST(DIZ,DJZ,12)=TRNDIST(DIZ,DJZ,12)+TCRB 
      TRNDIST(DIZ,DJZ,13)=TRNDIST(DIZ,DJZ,13)+TCRP 
      TRNDIST(DIZ,DJZ,14)=TRNDIST(DIZ,DJZ,14)+TCRK 
      TRNDIST(DIZ,DJZ,15)=TRNDIST(DIZ,DJZ,15)+TURW 
      TRNDIST(DIZ,DJZ,16)=TRNDIST(DIZ,DJZ,16)+TURB 
      TRNDIST(DIZ,DJZ,17)=TRNDIST(DIZ,DJZ,17)+TURP 
      TRNDIST(DIZ,DJZ,18)=TRNDIST(DIZ,DJZ,18)+TURK 
      TRNDIST(DIZ,DJZ,19)=TRNDIST(DIZ,DJZ,19)+TLOC 
      TRNDIST(DIZ,DJZ,20)=TRNDIST(DIZ,DJZ,20)+TRPD 
      TRNDIST(DIZ,DJZ,21)=TRNDIST(DIZ,DJZ,21)+TEXP 
      TRNDIST(DIZ,DJZ,22)=TRNDIST(DIZ,DJZ,22)+TWAY 
      TRNDIST(DIZ,DJZ,23)=TRNDIST(DIZ,DJZ,23)+TBRT 
      TRNDIST(DIZ,DJZ,24)=TRNDIST(DIZ,DJZ,24)+TCR  
      TRNDIST(DIZ,DJZ,25)=TRNDIST(DIZ,DJZ,25)+TUR  
      TRNDIST(DIZ,DJZ,26)=TRNDIST(DIZ,DJZ,26)+TTRAN
      TRNDIST(DIZ,DJZ,27)=TRNDIST(DIZ,DJZ,27)+TBRTP  
      TRNDIST(DIZ,DJZ,28)=TRNDIST(DIZ,DJZ,28)+TBRTK    
      TRNDIST(DIZ,DJZ,29)=TRNDIST(DIZ,DJZ,29)+PERTRP 
C ---------------------------------------------------------------
C  ALLOCATE TO LAX PARKING LOTS
C
      IF(LAX.AND.(.NOT.LAXTRN).AND.AJZ.GT.0) THEN
      TPRKSHL=0.0
      TPRKSHL2=0.0
C............................................................
      IF(LDEBUG) WRITE(26,455) TAUTO,TDRV0,TDRV2,TDRV3,TDRV4
  455 FORMAT(/' LAX PARKING LOT INPUT AUTO TRIPS'/
     *       ' --------------------------------'/
     *       ' TAUT0=',F8.2/
     *       ' TDRV0=',F8.2/
     *       ' TDRV2=',F8.2/
     *       ' TDRV3=',F8.2/
     *       ' TDRV4=',F8.2//
     *       '         LOT             WALK   SHUTTLE   TRN'/
     *       '  LOT   PROB   VEHTRP    TRIPS   TRIPS   TRIPS'/
     *       ' ----   ----  --------  ------  ------  ------')
C............................................................
      DO 43 NI=1,50
      IF(PEQUIV(NI).LE.0) GO TO 43
      IF(PRKDATA(NI,1).EQ.0) GO TO 43
      DENOM=PROBLOT(NI)*
     *           (TDRV0+TDRV2+TDRV3+TDRV4)
      VEHTRP=PROBLOT(NI)*
     *           (TDRV0+TDRV2/HOV2P+TDRV3/HOV3P+TDRV4/HOV4P)
      LOTRIPS(NI,1)=LOTRIPS(NI,1)+VEHTRP
      LOTRIPS(NI,2)=LOTRIPS(NI,2)+DENOM*EPROBLOT(NI,1)
      LOTRIPS(NI,3)=LOTRIPS(NI,3)+DENOM*EPROBLOT(NI,2)
      LOTRIPS(NI,4)=LOTRIPS(NI,4)+DENOM*EPROBLOT(NI,3)
      IF(ITFPRK.AND.PRKDATA(NI,2).EQ.4.0) 
     *    TPRKSHL=DENOM*EPROBLOT(NI,2)+TPRKSHL
      IF(ITFPRK.AND.PRKDATA(NI,2).EQ.5.0) 
     *    TPRKSHL2=DENOM*EPROBLOT(NI,2)+TPRKSHL2
C
C  STORE LAX PARKING LOT - TRANSIT TRIPS
C
      IF(EPROBLOT(NI,3).GT.0) THEN
      LOTTRN(NI,AJZ,1)=LOTTRN(NI,AJZ,1)+
     *  DENOM*EPROBLOT(NI,3)*LAXTPROB(NI,AJZ,35)
      LOTTRN(NI,AJZ,2)=LOTTRN(NI,AJZ,2)+
     *  DENOM*EPROBLOT(NI,3)*LAXTPROB(NI,AJZ,36)
      LOTTRN(NI,AJZ,3)=LOTTRN(NI,AJZ,3)+
     *  DENOM*EPROBLOT(NI,3)*LAXTPROB(NI,AJZ,37)
      LOTTRN(NI,AJZ,4)=LOTTRN(NI,AJZ,4)+
     *  DENOM*EPROBLOT(NI,3)*LAXTPROB(NI,AJZ,38)
      LOTTRN(NI,AJZ,5)=LOTTRN(NI,AJZ,5)+
     *  DENOM*EPROBLOT(NI,3)*LAXTPROB(NI,AJZ,39)
      LOTTRN(NI,AJZ,6)=LOTTRN(NI,AJZ,6)+
     *  DENOM*EPROBLOT(NI,3)*LAXTPROB(NI,AJZ,40)
      LOTTRN(NI,AJZ,7)=LOTTRN(NI,AJZ,7)+
     *  DENOM*EPROBLOT(NI,3)*LAXTPROB(NI,AJZ,41)
      LCR=DENOM*EPROBLOT(NI,3)*LAXTPROB(NI,AJZ,35)
      LUR=DENOM*EPROBLOT(NI,3)*LAXTPROB(NI,AJZ,36)
      LBRT=DENOM*EPROBLOT(NI,3)*LAXTPROB(NI,AJZ,41)
      IF(LCR.GT.0.0) THEN
      ORISTA=LAXSTAP(NI,AJZ,1)-MAX_IZONES
      DESSTA=LAXSTAA(NI,AJZ,1)-MAX_IZONES
      LCRSS(ORISTA,DESSTA)=LCRSS(ORISTA,DESSTA) + LCR
      WCR(NI,ORISTA)=WCR(NI,ORISTA) + LCR
      LCRSTAZ(DESSTA,AJZ)=LCRSTAZ(DESSTA,AJZ) + LCR
      END IF
      IF(LUR.GT.0.0) THEN
      ORISTA=LAXSTAP(NI,AJZ,13)-MAX_IZONES
      DESSTA=LAXSTAA(NI,AJZ,13)-MAX_IZONES
      LURSS(ORISTA,DESSTA)=LURSS(ORISTA,DESSTA) + LUR
      WUR(NI,ORISTA)=WUR(NI,ORISTA) + LUR
      LURSTAZ(DESSTA,AJZ)=LURSTAZ(DESSTA,AJZ) + LUR
      END IF
      IF(LBRT.GT.0.0) THEN
      ORISTA=LAXSTAP(NI,AJZ,25)-MAX_IZONES
      DESSTA=LAXSTAA(NI,AJZ,25)-MAX_IZONES
      LBRTSS(ORISTA,DESSTA)=LBRTSS(ORISTA,DESSTA) + LBRT
      WBRT(NI,ORISTA)=WBRT(NI,ORISTA) + LBRT
      LBRTSTAZ(DESSTA,AJZ)=LBRTSTAZ(DESSTA,AJZ) + LBRT
      END IF
      END IF
C..........................................................
      IF(LDEBUG) THEN
      WRITE(26,454) PEQUIV(NI),PROBLOT(NI),VEHTRP,
     *              LOTRIPS(NI,2),LOTRIPS(NI,3),LOTRIPS(NI,4)
  454 FORMAT(I5,2X,F5.3,2X,F8.2,3(2X,F6.2))
      END IF
C..........................................................
   43 CONTINUE
      IF(LDEBUG) WRITE(26,456) TPRKSHL,TPRKSHL2
  456 FORMAT(/' ITF1 PARKING LOT SHUTTLE TRIPS=',F8.2/
     *        ' ITF2 PARKING LOT SHUTTLE TRIPS=',F8.2)
C
C  ALLOCATE TO FLY-AWAY LOTS
C
      DO 5401 NI=1,10
      IF(FLYADATA(NI,3).LT.1.0) GO TO 5401
      FLYTRIPS(NI,6)=FLYTRIPS(NI,6)+TFLY*FLYAPROB(NI,6)
      FLYTRIPS(NI,1)=FLYTRIPS(NI,1)+TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,1)
      FLYTRIPS(NI,2)=FLYTRIPS(NI,2)+TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,2)
      FLYTRIPS(NI,3)=FLYTRIPS(NI,3)+TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,3)
      FLYTRIPS(NI,4)=FLYTRIPS(NI,4)+TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,4)
      FLYTRIPS(NI,5)=FLYTRIPS(NI,5)+TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,5)
      FLYATRIPS(NI,1)=FLYATRIPS(NI,1)+
     *    TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,2)*LAXFPROB(IZ,NI,35)
      FLYATRIPS(NI,2)=FLYATRIPS(NI,2)+
     *    TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,2)*LAXFPROB(IZ,NI,36)
      FLYATRIPS(NI,3)=FLYATRIPS(NI,3)+
     *    TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,2)*LAXFPROB(IZ,NI,37)
      FLYATRIPS(NI,4)=FLYATRIPS(NI,4)+
     *    TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,2)*LAXFPROB(IZ,NI,38)
      FLYATRIPS(NI,5)=FLYATRIPS(NI,5)+
     *    TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,2)*LAXFPROB(IZ,NI,39)
      FLYATRIPS(NI,6)=FLYATRIPS(NI,6)+
     *    TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,2)*LAXFPROB(IZ,NI,40)
      FLYATRIPS(NI,7)=FLYATRIPS(NI,7)+
     *    TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,2)*LAXFPROB(IZ,NI,41)
      FLYATRIPS(NI,8)=FLYATRIPS(NI,8)+
     *    TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,2)   
      SAVFLY(NI,13)=TFLY*FLYAPROB(NI,6) + SAVFLY(NI,13)
      SAVFLY(NI,1)=TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,1)+ SAVFLY(NI,1)
      SAVFLY(NI,2)=TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,2)+ SAVFLY(NI,2)
      SAVFLY(NI,3)=TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,3)+ SAVFLY(NI,3)
      SAVFLY(NI,4)=TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,4)+ SAVFLY(NI,4)
      SAVFLY(NI,5)=TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,5)+ SAVFLY(NI,5)
      SAVFLY(NI,6)=SAVFLY(NI,6) +
     *    TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,2)*LAXFPROB(IZ,NI,35)
      SAVFLY(NI,7)=SAVFLY(NI,7) +
     *    TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,2)*LAXFPROB(IZ,NI,36)
      SAVFLY(NI,8)=SAVFLY(NI,8) +
     *    TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,2)*LAXFPROB(IZ,NI,37)
      SAVFLY(NI,9)=SAVFLY(NI,9) +
     *    TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,2)*LAXFPROB(IZ,NI,38)
      SAVFLY(NI,10)=SAVFLY(NI,10) +
     *    TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,2)*LAXFPROB(IZ,NI,39)
      SAVFLY(NI,11)=SAVFLY(NI,11) +
     *    TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,2)*LAXFPROB(IZ,NI,40)
      SAVFLY(NI,12)=SAVFLY(NI,12) +
     *    TFLY*FLYAPROB(NI,6)*FLYAPROB(NI,2)*LAXFPROB(IZ,NI,41)     
 5401 CONTINUE
C
C  STORE RENTAL CAR FACILITY TRANSIT TRIPS
C
      DO 5402 NI=1,10
      K=IDINT(RNTLDATA(NI,1))
      IF(K.LE.0) GO TO 5402
C
      IF(PROBRNT(NI,2).GT.0) THEN
      RNTTRN(NI,AJZ,1)=RNTTRN(NI,AJZ,1)+
     *  TRNTL*PROBRNT(NI,2)*LAXRPROB(NI,AJZ,35)
      RNTTRN(NI,AJZ,2)=RNTTRN(NI,AJZ,2)+
     *  TRNTL*PROBRNT(NI,2)*LAXRPROB(NI,AJZ,36)
      RNTTRN(NI,AJZ,3)=RNTTRN(NI,AJZ,3)+
     *  TRNTL*PROBRNT(NI,2)*LAXRPROB(NI,AJZ,37)
      RNTTRN(NI,AJZ,4)=RNTTRN(NI,AJZ,4)+
     *  TRNTL*PROBRNT(NI,2)*LAXRPROB(NI,AJZ,38)
      RNTTRN(NI,AJZ,5)=RNTTRN(NI,AJZ,5)+
     *  TRNTL*PROBRNT(NI,2)*LAXRPROB(NI,AJZ,39)
      RNTTRN(NI,AJZ,6)=RNTTRN(NI,AJZ,6)+
     *  TRNTL*PROBRNT(NI,2)*LAXRPROB(NI,AJZ,40)
      RNTTRN(NI,AJZ,7)=RNTTRN(NI,AJZ,7)+
     *  TRNTL*PROBRNT(NI,2)*LAXRPROB(NI,AJZ,41)
      LCRR=TRNTL*PROBRNT(NI,2)*LAXRPROB(NI,AJZ,35)
      LURR=TRNTL*PROBRNT(NI,2)*LAXRPROB(NI,AJZ,36)
      LBRTR=TRNTL*PROBRNT(NI,2)*LAXRPROB(NI,AJZ,41)
      IF(LCRR.GT.0.0) THEN
      ORISTA=LAXRNTP(NI,AJZ,1)-MAX_IZONES
      DESSTA=LAXRNTA(NI,AJZ,1)-MAX_IZONES
      LCRSSR(ORISTA,DESSTA)=LCRSSR(ORISTA,DESSTA) + LCRR
      WCRR(NI,ORISTA)=WCRR(NI,ORISTA) + LCRR
      LCRRSTAZ(DESSTA,AJZ)=LCRRSTAZ(DESSTA,AJZ) + LCRR
      END IF
      IF(LURR.GT.0.0) THEN
      ORISTA=LAXRNTP(NI,AJZ,13)-MAX_IZONES
      DESSTA=LAXRNTA(NI,AJZ,13)-MAX_IZONES
      LURSSR(ORISTA,DESSTA)=LURSSR(ORISTA,DESSTA) + LURR
      WURR(NI,ORISTA)=WURR(NI,ORISTA) + LURR
      LURRSTAZ(DESSTA,AJZ)=LURRSTAZ(DESSTA,AJZ) + LURR
      END IF
      IF(LBRTR.GT.0.0) THEN
      ORISTA=LAXRNTP(NI,AJZ,25)-MAX_IZONES
      DESSTA=LAXRNTA(NI,AJZ,25)-MAX_IZONES
      LBRTSSR(ORISTA,DESSTA)=LBRTSSR(ORISTA,DESSTA) + LBRTR
      WBRTR(NI,ORISTA)=WBRTR(NI,ORISTA) + LBRTR
      LBRTRSTAZ(DESSTA,AJZ)=LBRTRSTAZ(DESSTA,AJZ) + LBRTR
      END IF
      END IF
 5402 CONTINUE
C
C  STORE ITF FACILITY TRANSIT TRIPS
C
      IF((TONCL.GT.0.AND.ITFONC.GT.0).OR.(TFLY.GT.0.AND.ITFFLY.GT.0).OR.
     *   (TPRKSHL.GT.0.AND.ITFPRK).OR.(TPRKSHL2.GT.0.AND.ITFPRK).OR.
     *    ITFTXI.OR.ITFLMO.OR.ITFDRP) THEN
      TONCLI=0.0
      IF(ITFONC.EQ.1) TONCLI=TONCL
      IF(HTLIND.EQ.1.AND.ITFHTL.EQ.1) TONCLI=TONCL
      IF(HTLIND.EQ.1.AND.ITFHTL.EQ.2) TONCLI=0.0
      IF(LDEBUG) WRITE(26,11111) HTLIND,ITFHTL,TONCLI
11111 FORMAT(' ITF1 HTLIND=',I1,' ITFHTL=',I1,' TONCL=',F10.5)
      TFLYI=TFLY
      IF(ITFFLY.EQ.0) TFLYI=0.0
      IF(.NOT.ITFPRK) TPRKSHL=0.0
      IF(.NOT.ITFPRK) TPRKSHL2=0.0
      IF(.NOT.ITFTXI) THEN
      TTAXI0=0.0
      TTAXI1=0.0
      TTAXI2=0.0
      ELSE
      TTAXI0=TTAXI*PROBTXI(1)
      TTAXI1=TTAXI*PROBTXI(2)
      TTAXI2=TTAXI*PROBTXI(3)
      END IF
      IF(.NOT.ITFLMO) THEN
      TLIMO0=0.0
      TLIMO1=0.0
      TLIMO2=0.0
      ELSE
      TLIMO0=TLIMO*PROBLMO(1)
      TLIMO1=TLIMO*PROBLMO(2)
      TLIMO2=TLIMO*PROBLMO(3)
      END IF
      IF(.NOT.ITFDRP) THEN
      TDROP0=0.0
      TDROP1=0.0
      TDROP2=0.0
      ELSE
      TDROP0=TDROP*PROBDRP(1)
      TDROP1=TDROP*PROBDRP(2)
      TDROP2=TDROP*PROBDRP(3)
      END IF
C.......................................................
      IF(LDEBUG) THEN
      WRITE(26,99999) TTAXI,TTAXI0,TTAXI1,TTAXI2,
     *                TLIMO,TLIMO0,TLIMO1,TLIMO2,
     *                TDROP,TDROP0,TDROP1,TDROP2
99999 FORMAT(//' TTAXI=',4F10.4/' TLIMO=',4F10.4/' TDROP=',4F10.4)
      END IF
C..................................................................
      ITFSUM(1,1)=ITFSUM(1,1)+TTAXI0
      ITFSUM(1,2)=ITFSUM(1,2)+TTAXI1
      ITFSUM(1,3)=ITFSUM(1,3)+TTAXI2
      ITFSUM(1,4)=ITFSUM(1,4)+TTAXI
      ITFSUM(2,1)=ITFSUM(2,1)+TLIMO0
      ITFSUM(2,2)=ITFSUM(2,2)+TLIMO1
      ITFSUM(2,3)=ITFSUM(2,3)+TLIMO2
      ITFSUM(2,4)=ITFSUM(2,4)+TLIMO
      ITFSUM(3,1)=ITFSUM(3,1)+TDROP0
      ITFSUM(3,2)=ITFSUM(3,2)+TDROP1
      ITFSUM(3,3)=ITFSUM(3,3)+TDROP2
      ITFSUM(3,4)=ITFSUM(3,4)+TDROP
C
      ITFTRN(AJZ,1)=ITFTRN(AJZ,1)+
     *  (TONCLI+TFLYI+TPRKSHL+TTAXI1+TLIMO1+TDROP1)*LAXIPROB(AJZ,35)
      ITFTRN(AJZ,2)=ITFTRN(AJZ,2)+
     *  (TONCLI+TFLYI+TPRKSHL+TTAXI1+TLIMO1+TDROP1)*LAXIPROB(AJZ,36)
      ITFTRN(AJZ,3)=ITFTRN(AJZ,3)+
     *  (TONCLI+TFLYI+TPRKSHL+TTAXI1+TLIMO1+TDROP1)*LAXIPROB(AJZ,37)
      ITFTRN(AJZ,4)=ITFTRN(AJZ,4)+
     *  (TONCLI+TFLYI+TPRKSHL+TTAXI1+TLIMO1+TDROP1)*LAXIPROB(AJZ,38)
      ITFTRN(AJZ,5)=ITFTRN(AJZ,5)+
     *  (TONCLI+TFLYI+TPRKSHL+TTAXI1+TLIMO1+TDROP1)*LAXIPROB(AJZ,39)
      ITFTRN(AJZ,6)=ITFTRN(AJZ,6)+
     *  (TONCLI+TFLYI+TPRKSHL+TTAXI1+TLIMO1+TDROP1)*LAXIPROB(AJZ,40)
      ITFTRN(AJZ,7)=ITFTRN(AJZ,7)+
     *  (TONCLI+TFLYI+TPRKSHL+TTAXI1+TLIMO1+TDROP1)*LAXIPROB(AJZ,41)
      LCRI=(TONCLI+TFLYI+TPRKSHL+TTAXI1+TLIMO1+TDROP1)*LAXIPROB(AJZ,35)
      LURI=(TONCLI+TFLYI+TPRKSHL+TTAXI1+TLIMO1+TDROP1)*LAXIPROB(AJZ,36)
      LBRTI=(TONCLI+TFLYI+TPRKSHL+TTAXI1+TLIMO1+TDROP1)*LAXIPROB(AJZ,41)
      IF(LCRI.GT.0.0) THEN
      ORISTA=LAXITFP(AJZ,1)-MAX_IZONES
      DESSTA=LAXITFA(AJZ,1)-MAX_IZONES
      LCRSSI(ORISTA,DESSTA)=LCRSSI(ORISTA,DESSTA) + LCRI
      WCRI(ORISTA)=WCRI(ORISTA) + LCRI
      LCRISTAZ(DESSTA,AJZ)=LCRISTAZ(DESSTA,AJZ) + LCRI
      END IF
      IF(LURI.GT.0.0) THEN
      ORISTA=LAXITFP(AJZ,13)-MAX_IZONES
      DESSTA=LAXITFA(AJZ,13)-MAX_IZONES
      LURSSI(ORISTA,DESSTA)=LURSSI(ORISTA,DESSTA) + LURI
      WURI(ORISTA)=WURI(ORISTA) + LURI
      LURISTAZ(DESSTA,AJZ)=LURISTAZ(DESSTA,AJZ) + LURI
      END IF
      IF(LBRTI.GT.0.0) THEN
      ORISTA=LAXITFP(AJZ,25)-MAX_IZONES
      DESSTA=LAXITFA(AJZ,25)-MAX_IZONES
      LBRTSSI(ORISTA,DESSTA)=LBRTSSI(ORISTA,DESSTA) + LBRTI
      WBRTI(ORISTA)=WBRTI(ORISTA) + LBRTI
      LBRTISTAZ(DESSTA,AJZ)=LBRTISTAZ(DESSTA,AJZ) + LBRTI
      END IF
C.....ITF ZONE 2
      IF(TPRKSHL2.GT.0.OR.ITFONC.EQ.2) THEN
      TONCLI=0.0
      IF(ITFONC.EQ.2) TONCLI=TONCL
      IF(HTLIND.EQ.1.AND.ITFHTL.EQ.2) TONCLI=TONCL
      IF(HTLIND.EQ.1.AND.ITFHTL.EQ.1) TONCLI=0.0
      ITFTRN2(AJZ,1)=ITFTRN2(AJZ,1)+
     *  (TPRKSHL2+TONCLI+TTAXI2+TLIMO2+TDROP2)*LAXI2PROB(AJZ,35)
      ITFTRN2(AJZ,2)=ITFTRN2(AJZ,2)+
     *  (TPRKSHL2+TONCLI+TTAXI2+TLIMO2+TDROP2)*LAXI2PROB(AJZ,36)
      ITFTRN2(AJZ,3)=ITFTRN2(AJZ,3)+
     *  (TPRKSHL2+TONCLI+TTAXI2+TLIMO2+TDROP2)*LAXI2PROB(AJZ,37)
      ITFTRN2(AJZ,4)=ITFTRN2(AJZ,4)+
     *  (TPRKSHL2+TONCLI+TTAXI2+TLIMO2+TDROP2)*LAXI2PROB(AJZ,38)
      ITFTRN2(AJZ,5)=ITFTRN2(AJZ,5)+
     *  (TPRKSHL2+TONCLI+TTAXI2+TLIMO2+TDROP2)*LAXI2PROB(AJZ,39)
      ITFTRN2(AJZ,6)=ITFTRN2(AJZ,6)+
     *  (TPRKSHL2+TONCLI+TTAXI2+TLIMO2+TDROP2)*LAXI2PROB(AJZ,40)
      ITFTRN2(AJZ,7)=ITFTRN2(AJZ,7)+
     *  (TPRKSHL2+TONCLI+TTAXI2+TLIMO2+TDROP2)*LAXI2PROB(AJZ,41)
      LCRI=(TPRKSHL2+TONCLI+TTAXI2+TLIMO2+TDROP2)*LAXI2PROB(AJZ,35)
      LURI=(TPRKSHL2+TONCLI+TTAXI2+TLIMO2+TDROP2)*LAXI2PROB(AJZ,36)
      LBRTI=(TPRKSHL2+TONCLI+TTAXI2+TLIMO2+TDROP2)*LAXI2PROB(AJZ,41)
      IF(LCRI.GT.0.0) THEN
      ORISTA=LAXITF2P(AJZ,1)-MAX_IZONES
      DESSTA=LAXITF2A(AJZ,1)-MAX_IZONES
      LCRSSI(ORISTA,DESSTA)=LCRSSI(ORISTA,DESSTA) + LCRI
      WCRI(ORISTA)=WCRI(ORISTA) + LCRI
      LCRISTAZ(DESSTA,AJZ)=LCRISTAZ(DESSTA,AJZ) + LCRI
      END IF
      IF(LURI.GT.0.0) THEN
      ORISTA=LAXITF2P(AJZ,13)-MAX_IZONES
      DESSTA=LAXITF2A(AJZ,13)-MAX_IZONES
      LURSSI(ORISTA,DESSTA)=LURSSI(ORISTA,DESSTA) + LURI
      WURI(ORISTA)=WURI(ORISTA) + LURI
      LURISTAZ(DESSTA,AJZ)=LURISTAZ(DESSTA,AJZ) + LURI
      END IF
      IF(LBRTI.GT.0.0) THEN
      ORISTA=LAXITF2P(AJZ,25)-MAX_IZONES
      DESSTA=LAXITF2A(AJZ,25)-MAX_IZONES
      LBRTSSI(ORISTA,DESSTA)=LBRTSSI(ORISTA,DESSTA) + LBRTI
      WBRTI(ORISTA)=WBRTI(ORISTA) + LBRTI
      LBRTISTAZ(DESSTA,AJZ)=LBRTISTAZ(DESSTA,AJZ) + LBRTI
      END IF
      END IF
      END IF
C --------------------------------------------------------------
      END IF
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
C ===================================================
C
C  SAVE DISTRICT LEVEL TOLL FACILITY TRIPS
C
      IF(TAB163P(JZ).EQ.2) THEN
      TOLLDIST(DIZ,DJZ)=TOLLDIST(DIZ,DJZ)+TDRV3NH
      END IF
C ===================================================
C
C  INTRAZONAL COMPUTATIONS
C
      IF(IZ.EQ.JZ) THEN
      TTRAN=0.0
      TNMOT=0.0
      TAUTO=0.0
      TNMWK=0.0
      TNMBK=0.0
      TNMSC=0.0
      TDRV0=0.0
      TDSHR=0.0
      TCR=0.0
      TUR=0.0
      TWAY=0.0
      TBRT=0.0
      TEXP =0.0
      TLOC =0.0
      TRPD =0.0
      TFLY=0.0
      TDRV2=0.0
      TDRV3=0.0
      TLOCW=0.0
      TLOCD=0.0
      TRPDW=0.0
      TRPDB=0.0
      TRPDD=0.0
      TEXPW=0.0
      TEXPB=0.0
      TEXPD=0.0
      TBRTW=0.0
      TBRTB=0.0
      TBRTBK=0.0
      TBRTP=0.0
      TBRTK=0.0
      TBRTU=0.0
      TCRW=0.0
      TCRB=0.0
      TCRBK=0.0
      TCRP=0.0
      TCRK=0.0
      TCRU=0.0
      TCRW1=0.0
      TCRW2=0.0
      TCRB1=0.0
      TCRB2=0.0
      TCRBK1=0.0
      TCRBK2=0.0
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
      TURBK=0.0
      TURP=0.0
      TURK=0.0
      TURU=0.0
      TURW1=0.0
      TURW2=0.0
      TURB1=0.0
      TURB2=0.0
      TURBK1=0.0
      TURBK2=0.0
      TURP1=0.0
      TURP2=0.0
      TURP3=0.0
      TURP4=0.0
      TURK1=0.0
      TURK2=0.0
      TURK3=0.0
      TURK4=0.0
      TBRTW1=0.0
      TBRTW2=0.0
      TBRTB1=0.0
      TBRTB2=0.0
      TBRTBK1=0.0
      TBRTBK2=0.0
      TBRTP1=0.0
      TBRTP2=0.0
      TBRTP3=0.0
      TBRTP4=0.0
      TBRTK1=0.0
      TBRTK2=0.0
      TBRTK3=0.0
      TBRTK4=0.0
      TWAYW=0.0
      TWAYB=0.0
      TWAYD=0.0
      TCRU1=0.0
      TCRU2=0.0
      TCRU3=0.0
      TCRU4=0.0
      TURU1=0.0
      TURU2=0.0
      TURU3=0.0
      TURU4=0.0
      TBRTU1=0.0
      TBRTU2=0.0
      TBRTU3=0.0
      TBRTU4=0.0
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
      TDROP=0.0
      TLIMO=0.0
      TRNTL=0.0
      TTAXI=0.0
      TONCL=0.0
      TUBER=0.0
      TAXIMAAS=0.0
      DENOM=0.0
      IF(INTRADST(IZ).LE.0) THEN
      WRITE(26,8445) IZ
 8445 FORMAT(' MTAMC 8445 (W) INTRAZONAL DISTANCE FOR ZONE ',
     *       I4,' WAS ZERO, RESET TO 1.0')
      INTRADST(IZ)=1.0
      END IF
      IF(WLKLSM(JZ).LT.(-999.9)) WLKLSM(JZ)=-999.9
      IF(BYCLSM(JZ).LT.(-999.9)) BYCLSM(JZ)=-999.9
      INTRAUTL(1)=COEFF(100)*(60/INTRASPD)*INTRADST(IZ)
      INTRAUTL(2)=WLKLSM(JZ)+KINTRAWK(C)
      IF(WALKTIME) THEN
      INTRAUTL(2)=COEFF(12)*(60/3.0)*INTRADST(IZ) +KINTRAWK(C)     
      END IF
      INTRAUTL(3)=BYCLSM(JZ)+KINTRABK(C)
      IF(.NOT.BICYCLE) THEN
      INTRAUTL(3)=COEFF(12)*(60/10.0)*INTRADST(IZ)+KINTRABK(C)
      END IF
      IF(ESCOOTER) THEN
       SCOTTIME=INTRADST(IZ)*(60.0/SCOTSPD)
       SCOTCOST=100.0+15.0*SCOTTIME
       BIKE1=AMIN1(INTRADST(IZ),MWALKT)
       BIKE2=DIM(INTRADST(IZ),MWALKT)
       UTILSC=MBIKE1*(60.0/SCOTSPD)*BIKE1
     *            +MBIKE2*(60.0/SCOTSPD)*BIKE2
       INTRAUTL(4)=UTILSC + + COEFF(60+C)*SCOTCOST
     *                   + KINTRASC(C)
       INTRAEXP(4)=EXP(INTRAUTL(4))
      ELSE
       INTRAUTL(4)=0.0
       INTRAEXP(4)=0.0
      END IF
      DO K=1,3
      INTRAEXP(K)=EXP(INTRAUTL(K))
      DENOM=DENOM+INTRAEXP(K)
      END DO
      DENOM=DENOM+INTRAEXP(4)
      INTRAPRB(2)=INTRAEXP(2)/DENOM
      INTRAPRB(3)=INTRAEXP(3)/DENOM
      INTRAPRB(4)=INTRAEXP(4)/DENOM
      INTRAPRB(1)=1.0-INTRAPRB(2)-INTRAPRB(3)-INTRAPRB(4)
      TNMWK=PERTRP*INTRAPRB(2)
      TNMBK=PERTRP*INTRAPRB(3)
      TNMSC=PERTRP*INTRAPRB(4)
      TNMOT=TNMWK+TNMBK+TNMSC
      TAUTO=PERTRP-TNMWK-TNMBK-TNMSC
      TDRV0=PDRV0(C)*TAUTO
      TDSHR=TAUTO-TDRV0
      TDRV2=PDRV2(C)*TDSHR
      TDRV4=PDRV4(C)*(TDSHR-TDRV2)
      TDRV3=TDSHR-TDRV2-TDRV4
      TDRV0N=TDRV0
      TDRV2NN=TDRV2
      TDRV3NN=TDRV3
      TDRV4NN=TDRV4
      LRTWLK(JZ)=0
      LRTDRV(JZ)=0
      INTRATRP(1,C)=INTRATRP(1,C)+TDRV0
      INTRATRP(2,C)=INTRATRP(2,C)+TDRV2
      INTRATRP(3,C)=INTRATRP(3,C)+TDRV3
      INTRATRP(4,C)=INTRATRP(4,C)+TDRV4
      INTRATRP(5,C)=INTRATRP(5,C)+TNMWK
      INTRATRP(6,C)=INTRATRP(6,C)+TNMBK
      INTRATRP(7,C)=INTRATRP(7,C)+PERTRP
      INTRATRP(8,C)=INTRATRP(8,C)+TNMSC
C...................................................................
      IF(DEBUG) THEN
      WRITE(26,9423) C,M,INTRAUTL,INTRAEXP,INTRAPRB,TAUTO,TNMWK,TNMBK,
     *               TNMSC
 9423 FORMAT('          INTRAZONAL COMPUTATIONS'/
     *       '          -----------------------'/
     *       '     MARKET GROUP=',I1,' MARKET SEGMENT=',I1/
     *       ' VARIABLE    AUTO      WALK      BIKE    E-SCOOTER'/
     *       ' --------  --------  --------  --------  --------'/
     *       '  UTILITY',4(2X,F8.5)/
     *       '  EXPONEN',4(2X,F8.6)/
     *       '   SHARE ',4(2X,F8.4)/
     *       '   TRIPS ',4(2X,F8.2))
      END IF
C................................................................... 
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9421) C,M,TTRAN,TAUTO,TNMOT,TMAAS,TDRV0,
     *               TDSHR,TCR,TUR,TWAY,TEXP,TLOC,TRPD,TBRT,
     *               TDRV2,TDRV3,TDRV4,TNMWK,TNMBK,TNMSC,TAXIMAAS,
     *               TUBER
      WRITE(26,9022) TLOCW,TLOCD,TLOCB,TRPDW,TRPDD,TRPDB,
     *               TCRW,TCRB,TCRBK,TCRP,TCRK,TCRU,TCRW1,TCRW2,
     *               TCRB1,TCRB2,TCRBK1,TCRBK2
      WRITE(26,9024) TCRP1,TCRP2,TCRP3,TCRP4,
     *               TCRK1,TCRK2,TCRK3,TCRK4,
     *               TCRU1,TCRU2,TCRU3,TCRU4
      WRITE(26,9023) TURW,TURB,TURBK,TURP,TURU,TURK,TURW1,TURW2,
     *               TURB1,TURB2,TURBK1,TURBK2
      WRITE(26,9425) TURP1,TURP2,TURP3,TURP4,
     *               TURK1,TURK2,TURK3,TURK4,
     *               TURU1,TURU2,TURU3,TURU4
      WRITE(26,9027) TWAYW,TWAYB,TWAYD,TEXPW,TEXPB,TEXPD,
     *               TBRTW,TBRTB,TBRTBK,TBRTP,TBRTK,TBRTU,
     *               TBRTW1,TBRTW2,
     *               TBRTB1,TBRTB2,TBRTBK1,TBRTBK2,
     *               TBRTP1,TBRTP2,TBRTP3,TBRTP4,
     *               TBRTK1,TBRTK2,TBRTK3,TBRTK4,
     *               TBRTU1,TBRTU2,TBRTU3,TBRTU4
 9421 FORMAT(/1X,'SUMMARY OF MODAL TRIP VALUES'/
     *       1X,'-----------------------------------'/
     *       1X,'MARKET SEGMENT=',I2,' ACCESS SEGMENT=',I2//
     *       1X,'TRANSIT                        =',E12.5/
     *       1X,'AUTO                           =',E12.5/
     *       1X,'NON-MOTORIZED                  =',E12.5/
     *       1X,'MAAS                           =',E12.5//
     *       1X,'DRIVE ALONE                    =',E12.5/
     *       1X,'SHARED RIDE                    =',E12.5//
     *       1X,'COMMUTER RAIL                  =',E12.5/
     *       1X,'URBAN RAIL                     =',E12.5/
     *       1X,'TRANSITWAY BUS                 =',E12.5/
     *       1X,'EXPRESS BUS                    =',E12.5/
     *       1X,'LOCAL BUS                      =',E12.5/
     *       1X,'RAPID BUS                      =',E12.5/
     *       1X,'BUS RAPID TRANSIT              =',E12.5//
     *       1X,'2  PERSON AUTO                 =',E12.5/
     *       1X,'3+ PERSON AUTO                 =',E12.5/
     *       1X,'4+ PERSON AUTO                 =',E12.5/
     *       1X,'NON-MOTORIZED WALK             =',E12.5/
     *       1X,'NON-MOTORIZED BIKE             =',E12.5/
     *       1X,'NON-MOTORIZED E-SCOOTER        =',E12.5/
     *       1X,'TAXI                           =',E12.5/
     *       1X,'UBER                           =',E12.5/)
 9022 FORMAT(1X,'WALK  --> LOCAL BUS            =',E12.5/
     *       1X,'DRIVE --> LOCAL BUS            =',E12.5/
     *       1X,'BIKE  --> LOCAL BUS            =',E12.5/
     *       1X,'WALK  --> RAPID BUS            =',E12.5/
     *       1X,'DRIVE --> RAPID BUS            =',E12.5/
     *       1X,'BIKE  --> RAPID BUS            =',E12.5/
     *       1X,'WALK  --> COMMUTER RAIL        =',E12.5/
     *       1X,'BUS   --> COMMUTER RAIL        =',E12.5/
     *       1X,'BIKE  --> COMMUTER RAIL        =',E12.5/
     *       1X,'P&R   --> COMMUTER RAIL        =',E12.5/
     *       1X,'K&R   --> COMMUTER RAIL        =',E12.5/
     *       1X,'UBER  --> COMMUTER RAIL        =',E12.5//
     *       1X,'COMMUTER RAIL WALK  -STATION #1=',E12.5/
     *       1X,'COMMUTER RAIL WALK  -STATION #2=',E12.5/
     *       1X,'COMMUTER RAIL BUS   -STATION #1=',E12.5/
     *       1X,'COMMUTER RAIL BUS   -STATION #2=',E12.5/
     *       1X,'COMMUTER RAIL BIKE  -STATION #1=',E12.5/
     *       1X,'COMMUTER RAIL BIKE  -STATION #2=',E12.5)
 9024 FORMAT(1X,'COMMUTER RAIL PNR   -STATION #1=',E12.5/
     *       1X,'COMMUTER RAIL PNR   -STATION #2=',E12.5/
     *       1X,'COMMUTER RAIL PNR   -STATION #3=',E12.5/
     *       1X,'COMMUTER RAIL PNR   -STATION #4=',E12.5/
     *       1X,'COMMUTER RAIL KNR   -STATION #1=',E12.5/
     *       1X,'COMMUTER RAIL KNR   -STATION #2=',E12.5/
     *       1X,'COMMUTER RAIL KNR   -STATION #3=',E12.5/
     *       1X,'COMMUTER RAIL KNR   -STATION #4=',E12.5/
     *       1X,'COMMUTER RAIL UBER  -STATION #1=',E12.5/
     *       1X,'COMMUTER RAIL UBER  -STATION #2=',E12.5/
     *       1X,'COMMUTER RAIL UBER  -STATION #3=',E12.5/
     *       1X,'COMMUTER RAIL UBER  -STATION #4=',E12.5/)
 9023 FORMAT(1X,'WALK  --> URBAN RAIL           =',E12.5/
     *       1X,'BUS   --> URBAN RAIL           =',E12.5/
     *       1X,'BIKE  --> URBAN RAIL           =',E12.5/
     *       1X,'P&R   --> URBAN RAIL           =',E12.5/
     *       1X,'K&R   --> URBAN RAIL           =',E12.5/
     *       1X,'UBER  --> URBAN RAIL           =',E12.5//
     *       1X,'URBAN RAIL WALK     -STATION #1=',E12.5/
     *       1X,'URBAN RAIL WALK     -STATION #2=',E12.5/
     *       1X,'URBAN RAIL BUS      -STATION #1=',E12.5/
     *       1X,'URBAN RAIL BUS      -STATION #2=',E12.5/
     *       1X,'URBAN RAIL BIKE     -STATION #1=',E12.5/
     *       1X,'URBAN RAIL BIKE     -STATION #2=',E12.5)
 9425 FORMAT(1X,'URBAN RAIL PNR      -STATION #1=',E12.5/
     *       1X,'URBAN RAIL PNR      -STATION #2=',E12.5/
     *       1X,'URBAN RAIL PNR      -STATION #3=',E12.5/
     *       1X,'URBAN RAIL PNR      -STATION #4=',E12.5/
     *       1X,'URBAN RAIL KNR      -STATION #1=',E12.5/
     *       1X,'URBAN RAIL KNR      -STATION #2=',E12.5/
     *       1X,'URBAN RAIL KNR      -STATION #3=',E12.5/
     *       1X,'URBAN RAIL KNR      -STATION #4=',E12.5/
     *       1X,'URBAN RAIL UBER     -STATION #1=',E12.5/
     *       1X,'URBAN RAIL UBER     -STATION #2=',E12.5/
     *       1X,'URBAN RAIL UBER     -STATION #3=',E12.5/
     *       1X,'URBAN RAIL UBER     -STATION #4=',E12.5/)
 9027 FORMAT(1X,'WALK  --> TRANSITWAY BUS       =',E12.5/
     *       1X,'BIKE  --> TRANSITWAY BUS       =',E12.5/
     *       1X,'DRIVE --> TRANSITWAY BUS       =',E12.5//
     *       1X,'WALK  --> EXPRESS BUS          =',E12.5/
     *       1X,'BIKE  --> EXPRESS BUS          =',E12.5/
     *       1X,'DRIVE --> EXPRESS BUS          =',E12.5//
     *       1X,'WALK  --> BUS RAPID TRANSIT    =',E12.5/
     *       1X,'BUS   --> BUS RAPID TRANSIT    =',E12.5/
     *       1X,'BIKE  --> BUS RAPID TRANSIT    =',E12.5/
     *       1X,'P&R   --> BUS RAPID TRANSIT    =',E12.5/
     *       1X,'K&R   --> BUS RAPID TRANSIT    =',E12.5/
     *       1X,'UBER  --> BUS RAPID TRANSIT    =',E12.5//
     *       1X,'BUS RPD TRN WALK    -STATION #1=',E12.5/
     *       1X,'BUS RPD TRN WALK    -STATION #2=',E12.5/
     *       1X,'BUS RPD TRN BUS     -STATION #1=',E12.5/
     *       1X,'BUS RPD TRN BUS     -STATION #2=',E12.5/
     *       1X,'BUS RPD TRN BIKE    -STATION #1=',E12.5/
     *       1X,'BUS RPD TRN BIKE    -STATION #2=',E12.5/
     *       1X,'BUS RPD TRN PNR     -STATION #1=',E12.5/
     *       1X,'BUS RPD TRN PNR     -STATION #2=',E12.5/
     *       1X,'BUS RPD TRN PNR     -STATION #3=',E12.5/
     *       1X,'BUS RPD TRN PNR     -STATION #4=',E12.5/
     *       1X,'BUS RPD TRN KNR     -STATION #1=',E12.5/
     *       1X,'BUS RPD TRN KNR     -STATION #2=',E12.5/
     *       1X,'BUS RPD TRN KNR     -STATION #3=',E12.5/
     *       1X,'BUS RPD TRN KNR     -STATION #4=',E12.5/
     *       1X,'BUS RPD TRN UBER    -STATION #1=',E12.5/
     *       1X,'BUS RPD TRN UBER    -STATION #2=',E12.5/
     *       1X,'BUS RPD TRN UBER    -STATION #3=',E12.5/
     *       1X,'BUS RPD TRN UBER    -STATION #4=',E12.5/)
      END IF
C....................................................................
C
C  STORE SUMMARY STATISTICS
C
C..LOGSUM DATA
      ALOGSUM((C+5),JZ)=ALOGSUM((C+5),JZ)+ TURW+TURB+TURP+TURK
      TLOGSUM((C+5),JZ)=TLOGSUM((C+5),JZ)+ TTRAN
      ULOGSUM((C+5),JZ)=ULOGSUM((C+5),JZ)+ PERTRP
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
      TESUM(90,C)=TESUM(90,C) + TNMSC
      TESUM(80,C)=TESUM(80,C) + TAXIMAAS
      IF(UBERAV(AV).EQ.0) THEN
      TESUM(81,C)=TESUM(81,C) + TUBER
      ELSE
      TESUM(89,C)=TESUM(89,C) + TUBER      
      END IF
      TESUM(82,C)=TESUM(82,C) + TAXIMAAS + TUBER
      INDEX=IFIX(ZHHD(6,IZ))
      TAXISUM(INDEX)=TAXISUM(INDEX)+TAXIMAAS
      TAXISUM(6)=TAXISUM(6)+TAXIMAAS
C
      CALSUM(1,C)=CALSUM(1,C)+TDRV0
      CALSUM(2,C)=CALSUM(2,C)+TDRV2
      CALSUM(3,C)=CALSUM(3,C)+TDRV3
      CALSUM(4,C)=CALSUM(4,C)+TDRV4
C..TRANSIT PRIMARY/SUBMODE LEVEL
      TESUM(7,C)=TESUM(7,C) + TLOCW
      TESUM(8,C)=TESUM(8,C) + TLOCD
      TESUM(65,C)=TESUM(65,C) + TLOCB
      TESUM(9,C)=TESUM(9,C) + TEXPW
      TESUM(70,C)=TESUM(70,C) + TEXPB
      TESUM(10,C)=TESUM(10,C) + TEXPD
      TESUM(11,C)=TESUM(11,C) + TCRW
      TESUM(12,C)=TESUM(12,C) + TCRB
      TESUM(74,C)=TESUM(74,C) + TCRBK
      TESUM(13,C)=TESUM(13,C) + TCRP
      TESUM(14,C)=TESUM(14,C) + TCRK
      TESUM(83,C)=TESUM(83,C) + TCRU
      TESUM(15,C)=TESUM(15,C) + TURW
      TESUM(16,C)=TESUM(16,C) + TURB
      TESUM(75,C)=TESUM(75,C) + TURBK
      TESUM(17,C)=TESUM(17,C) + TURP
      TESUM(18,C)=TESUM(18,C) + TURK
      TESUM(84,C)=TESUM(84,C) + TURU
      TESUM(19,C)=TESUM(19,C) + TWAYW
      TESUM(68,C)=TESUM(68,C) + TWAYB
      TESUM(20,C)=TESUM(20,C) + TWAYD
      TESUM(21,C)=TESUM(21,C) + TTRAN
      TESUM(48,C)=TESUM(48,C) + TRPDW
      TESUM(66,C)=TESUM(66,C) + TRPDB
      TESUM(49,C)=TESUM(49,C) + TRPDD
      TESUM(56,C)=TESUM(56,C) + TBRTW
      TESUM(72,C)=TESUM(72,C) + TBRTBK
      TESUM(57,C)=TESUM(57,C) + TBRTB
      TESUM(58,C)=TESUM(58,C) + TBRTP
      TESUM(59,C)=TESUM(59,C) + TBRTK
      TESUM(85,C)=TESUM(85,C) + TBRTU
      TESUM(61,C)=TESUM(61,C) + TBRT
C..RAPID TRANSFERS
      IF(BIVT(4,JZ).GT.0) THEN
       TESUM(53,C)=TESUM(53,C) + TRPDW
       TESUM(60,C)=TESUM(60,C) + TRPDD
      END IF
      IF(BIVT(7,JZ).GT.0) TESUM(67,C)=TESUM(67,C) + TRPDB
C..TRANSITWAY TRANSFERS
      IF(BIVT(3,JZ).GT.0)
     * TESUM(54,C)=TESUM(54,C) + TWAYW
      IT=CSTAT-MAX_IZONES
      IF((STAZNEI(IT,JZ,4,1).GT.0).OR.(STAZNEI(IT,JZ,4,2).GT.0).OR.
     *   (STAZNEI(IT,JZ,4,3).GT.0)) THEN
      TESUM(62,C)=TESUM(62,C)+ TWAYD
      END IF
      IF(BIVT(9,JZ).GT.0) TESUM(69,C)=TESUM(69,C)+TWAYB
C..EXPRESS TRANSFERS
      IF(BIVT(2,JZ).GT.0)
     * TESUM(55,C)=TESUM(55,C) + TEXPW
      IT=CSTAE-MAX_IZONES
      IF((STAZNEI(IT,JZ,3,1).GT.0).OR.(STAZNEI(IT,JZ,3,2).GT.0)) THEN
      TESUM(63,C)=TESUM(63,C)+ TEXPD
      END IF
      IF(BIVT(8,JZ).GT.0) TESUM(71,C)=TESUM(71,C)+TEXPB
C
C..TOTAL TRANSIT TRIPS TO THE LA CBD
C
      DO E=1,50
      IF(JZ.EQ.CBDZ(E)) THEN
      CBDTRAN=CBDTRAN+TTRAN
      CBDTRCR=CBDTRCR+TCR
      END IF
      END DO
C..COMMUTER RAIL STATION LEVEL
      TESUM(22,C)=TESUM(22,C) + TCRW1
      TESUM(23,C)=TESUM(23,C) + TCRW2
      TESUM(24,C)=TESUM(24,C) + TCRB1
      TESUM(25,C)=TESUM(25,C) + TCRB2
      TESUM(76,C)=TESUM(76,C) + TCRBK1
      TESUM(77,C)=TESUM(77,C) + TCRBK2
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
      TESUM(78,C)=TESUM(78,C) + TURBK1
      TESUM(79,C)=TESUM(79,C) + TURBK2
      TESUM(39,C)=TESUM(39,C) + TURP1
      TESUM(40,C)=TESUM(40,C) + TURP2
      TESUM(41,C)=TESUM(41,C) + TURP3
      TESUM(42,C)=TESUM(42,C) + TURP4
      TESUM(43,C)=TESUM(43,C) + TURK1
      TESUM(44,C)=TESUM(44,C) + TURK2
      TESUM(45,C)=TESUM(45,C) + TURK3
      TESUM(46,C)=TESUM(46,C) + TURK4
      TESUM(47,C)=TESUM(47,C) + TUR
C...AIR PASSENGER SUMMARY
      IF(AIRPASS) THEN
      AESUM(1)=AESUM(1)+TDRV0
      AESUM(2)=AESUM(2)+TDRV2
      AESUM(3)=AESUM(3)+TDRV3
      AESUM(4)=AESUM(4)+TDRV4
      AESUM(5)=AESUM(5)+TDROP
      AESUM(6)=AESUM(6)+TLIMO
      AESUM(7)=AESUM(7)+TRNTL
      AESUM(8)=AESUM(8)+TTAXI
      AESUM(9)=AESUM(9)+TONCL
      AESUM(10)=AESUM(10)+TTRAN-TFLY
      AESUM(11)=AESUM(11)+TFLY
      AESUM(12)=AESUM(12)+PERTRP
      END IF
C..SAVE COMMUTER RAIL AND URBAN TRIPS FOR CCR PROCESSING
      TCRT(1)=TCRW1
      TCRT(2)=TCRW2
      TCRT(3)=TCRB1
      TCRT(4)=TCRB2
      TCRT(5)=TCRP1
      TCRT(6)=TCRP2
      TCRT(7)=TCRP3
      TCRT(8)=TCRP4
      TCRT(9)=TCRK1
      TCRT(10)=TCRK2
      TCRT(11)=TCRK3
      TCRT(12)=TCRK4
      TCRT(13)=TCRBK1
      TCRT(14)=TCRBK2
      TCRT(15)=TCRU1
      TCRT(16)=TCRU2
      TCRT(17)=TCRU3
      TCRT(18)=TCRU4
      TURT(1)=TURW1
      TURT(2)=TURW2
      TURT(3)=TURB1
      TURT(4)=TURB2
      TURT(5)=TURP1
      TURT(6)=TURP2
      TURT(7)=TURP3
      TURT(8)=TURP4
      TURT(9)=TURK1
      TURT(10)=TURK2
      TURT(11)=TURK3
      TURT(12)=TURK4
      TURT(13)=TURBK1
      TURT(14)=TURBK2
      TURT(15)=TURU1
      TURT(16)=TURU2
      TURT(17)=TURU3
      TURT(18)=TURU4
      TBRTT(1)=TBRTW1
      TBRTT(2)=TBRTW2
      TBRTT(3)=TBRTB1
      TBRTT(4)=TBRTB2
      TBRTT(5)=TBRTP1
      TBRTT(6)=TBRTP2
      TBRTT(7)=TBRTP3
      TBRTT(8)=TBRTP4
      TBRTT(9)=TBRTK1
      TBRTT(10)=TBRTK2
      TBRTT(11)=TBRTK3
      TBRTT(12)=TBRTK4
      TBRTT(13)=TBRTBK1
      TBRTT(14)=TBRTBK2
      TBRTT(15)=TBRTU1
      TBRTT(16)=TBRTU2
      TBRTT(17)=TBRTU3
      TBRTT(18)=TBRTU4
C
C  SUMMARIZE NUMBER OF TRANSFERS
C
C..LOCAL BUS
      INDEX=TXFER(1,JZ)+1
      INDEX=MIN0(INDEX,4)
      BUSTXFER(1,INDEX)=BUSTXFER(1,INDEX)+TLOCW
      BUSTXFER(11,INDEX)=BUSTXFER(11,INDEX)+TLOCD
      INDEX=TXFER(6,JZ)+1
      INDEX=MIN0(INDEX,4)
      BUSTXFER(6,INDEX)=BUSTXFER(6,INDEX)+TLOCB
C..EXPRESS BUS
      INDEX=TXFER(2,JZ)+1
      INDEX=MIN0(INDEX,4)
      BUSTXFER(2,INDEX)=BUSTXFER(2,INDEX)+TEXPW
      INDEX=TXFER(8,JZ)+1
      INDEX=MIN0(INDEX,4)
      BUSTXFER(7,INDEX)=BUSTXFER(7,INDEX)+TEXPB
      ORISTA=CSTAE-MAX_IZONES
      IF(ORISTA.GT.0) THEN
      INDEX=STAZNE(2,ORISTA,JZ)+1
      INDEX=MIN0(INDEX,4)
      BUSTXFER(12,INDEX)=BUSTXFER(12,INDEX)+TEXPD
      END IF
C..TRANSITWAY
      INDEX=TXFER(3,JZ)+1
      INDEX=MIN0(INDEX,4)
      BUSTXFER(3,INDEX)=BUSTXFER(3,INDEX)+TWAYW
      INDEX=TXFER(9,JZ)+1
      INDEX=MIN0(INDEX,4)
      BUSTXFER(8,INDEX)=BUSTXFER(8,INDEX)+TWAYB
      ORISTA=CSTAT-MAX_IZONES
      IF(ORISTA.GT.0) THEN
      INDEX=STAZNE(2,ORISTA,JZ)+1
      INDEX=MIN0(INDEX,4)
      BUSTXFER(13,INDEX)=BUSTXFER(13,INDEX)+TWAYD
      END IF
C..RAPID BUS
      INDEX=TXFER(4,JZ)+1
      INDEX=MIN0(INDEX,4)
      BUSTXFER(4,INDEX)=BUSTXFER(4,INDEX)+TRPDW
      BUSTXFER(14,INDEX)=BUSTXFER(14,INDEX)+TRPDD
      INDEX=TXFER(7,JZ)+1
      INDEX=MIN0(INDEX,4)
      BUSTXFER(9,INDEX)=BUSTXFER(9,INDEX)+TRPDB
C..STATION-TO-STATION TRANSFERS
      DO IMODE=1,5
      IF(IMODE.EQ.3.OR.IMODE.EQ.4) CYCLE
      DO K=1,18
      K1=K
      IF(K.GT.14) K1=K-6
      IF(IMODE.EQ.1) DENOM=TCRT(K)
      IF(IMODE.EQ.2) DENOM=TURT(K)
      IF(IMODE.EQ.5) DENOM=TBRTT(K)
      ORISTA=OSTA(IMODE,K1)-MAX_IZONES
      DESSTA=ASTA(IMODE,K1)-MAX_IZONES
      IF(ORISTA.GT.0.AND.DESSTA.GT.0) THEN
      INDEX=STASTA(2,ORISTA,DESSTA)+1
      INDEX=MIN0(INDEX,4)
      STATXF(IMODE,INDEX)=STATXF(IMODE,INDEX)+DENOM
      END IF
      END DO
      END DO
C
C     TOLL FACILITY INDICATOR SUMMARY ARRAY
C
      CALL TSUM(TOLLSUM,TAB8DA(JZ),TDRV0T,1,TOLLTLF,TAB5DA(JZ),
     *                  TAB5DA(JZ),TOLLSAV,TMESAV)
      CALL TSUM(TOLLSUM,TAB162P(JZ),TDRV2NH,2,TOLLTLF,TAB22P(JZ),
     *                  TAB22P(JZ),TOLLSAV,TMESAV)
      CALL TSUM(TOLLSUM,TAB102P(JZ),TDRV2TH,3,TOLLTLF,TAB52P(JZ),
     *                  TAB92P(JZ),TOLLSAV,TMESAV)
      CALL TSUM(TOLLSUM,TAB152P(JZ),TDRV2TN,4,TOLLTLF,TAB122P(JZ),
     *                  TAB142P(JZ),TOLLSAV,TMESAV)
      CALL TSUM(TOLLSUM,TAB163P(JZ),TDRV3NH,5,TOLLTLF,TAB23P(JZ),
     *                  TAB23P(JZ),TOLLSAV,TMESAV)  
      CALL TSUM(TOLLSUM,TAB103P(JZ),TDRV3TH,6,TOLLTLF,TAB53P(JZ),
     *                  TAB93P(JZ),TOLLSAV,TMESAV)  
      CALL TSUM(TOLLSUM,TAB153P(JZ),TDRV3TN,7,TOLLTLF,TAB123P(JZ),
     *                  TAB143P(JZ),TOLLSAV,TMESAV)
      CALL TSUM(TOLLSUM,TAB164P(JZ),TDRV4NH,8,TOLLTLF,TAB24P(JZ),
     *                  TAB24P(JZ),TOLLSAV,TMESAV)  
      CALL TSUM(TOLLSUM,TAB104P(JZ),TDRV4TH,9,TOLLTLF,TAB54P(JZ),
     *                  TAB94P(JZ),TOLLSAV,TMESAV)  
      CALL TSUM(TOLLSUM,TAB154P(JZ),TDRV4TN,10,TOLLTLF,TAB124P(JZ),
     *                  TAB144P(JZ),TOLLSAV,TMESAV)
C..............................................
      IF(DEBUG) THEN
      DO K=1,8
      WRITE(26,9229) K,(TOLLSUM(K,K1),K1=1,10)
 9229 FORMAT(' K=',I1,' TOLLSUM=',10(1X,F8.2))
      END DO
      END IF
C...............................................
      IF(CALIB.AND.(IZ.NE.JZ)) THEN
      QINDEX=IFIX(TAB2DA(JZ)*10.0)+1
      IF(QINDEX.GT.150) QINDEX=150
      NMOTLF(QINDEX,C,1)=NMOTLF(QINDEX,C,1)+TNMWK
      NMOTLF(QINDEX,6,1)=NMOTLF(QINDEX,6,1)+TNMWK
      NMOTLF(QINDEX,C,2)=NMOTLF(QINDEX,C,2)+TNMBK
      NMOTLF(QINDEX,6,2)=NMOTLF(QINDEX,6,2)+TNMBK
      END IF      
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
C..METRO RAPID TRANSIT
      MODETRP(6,1,1,1)=MODETRP(6,1,1,1)+TRPDW*FLOAT(MODEINC(6,1,1,1))
      MODETRP(6,1,1,2)=MODETRP(6,1,1,2)+TRPDD*FLOAT(MODEINC(6,1,1,2))
C
C     PARKING LOT CHOICE MODEL TRIP OUTPUT
C
      IF(AVMDL.AND.ALTPARK.AND.(HHLDAV(AV).EQ.1)) THEN
C...........................................................
      IF(DEBUG) THEN
      WRITE(26,9625) TDRV0,TDRV2,TDRV3,TRDV4,PROBOTH,
     *               HOMEAV,APRKZONE(JZ)
 9625 FORMAT(/' PARKING LOT CHOICE MODEL OUTPUT'/
     *       ' -------------------------------'/
     *       ' DRIVE ALONE         =',F8.2/
     *       ' 2-PERSON            =',F8.2/
     *       ' 3-PERSON            =',F8.2/
     *       ' 4+PERSON            =',F8.2/
     *       ' NON-HOME PROBABILITY=',F8.5/
     *       ' RETURN HOME         =',7X,L1/
     *       ' ALT PARKING ZONE    =',4X,I4/)
      END IF
C............................................................
      IF(HOMEAV) THEN
      APARKTRP(1,1)=APARKTRP(1,1)+PROBOTH*TDRV0
      APARKTRP(2,1)=APARKTRP(2,1)+PROBOTH*TDRV2
      APARKTRP(3,1)=APARKTRP(3,1)+PROBOTH*TDRV3
      APARKTRP(4,1)=APARKTRP(4,1)+PROBOTH*TDRV4
      OINDEX=VMTCODE(CNTYDST,CNTYORG)
      VMT(29,OINDEX)=VMT(29,OINDEX)+PROBOTH*TDRV0*TAB2DA(JZ)
      VMT(30,OINDEX)=VMT(30,OINDEX)+PROBOTH*TDRV2*TAB2DA(JZ)
      VMT(31,OINDEX)=VMT(31,OINDEX)+PROBOTH*TDRV3*TAB2DA(JZ)
      VMT(32,OINDEX)=VMT(32,OINDEX)+PROBOTH*TDRV4*TAB2DA(JZ)
      ELSE
      APARKTRP(1,2)=APARKTRP(1,2)+PROBOTH*TDRV0
      APARKTRP(2,2)=APARKTRP(2,2)+PROBOTH*TDRV2
      APARKTRP(3,2)=APARKTRP(3,2)+PROBOTH*TDRV3
      APARKTRP(4,2)=APARKTRP(4,2)+PROBOTH*TDRV4
      CNTYPRK=IFIX(ZHHD(6,APRKZONE(JZ)))
      DINDEX=VMTCODE(CNTYDST,CNTYPRK)
      VMT(33,DINDEX)=VMT(33,DINDEX)+PROBOTH*TDRV0*TAB2DA(JZ)
      VMT(34,DINDEX)=VMT(34,DINDEX)+PROBOTH*TDRV2*TAB2DA(JZ)
      VMT(35,DINDEX)=VMT(35,DINDEX)+PROBOTH*TDRV3*TAB2DA(JZ)
      VMT(36,DINDEX)=VMT(36,DINDEX)+PROBOTH*TDRV4*TAB2DA(JZ)  
      END IF
      END IF
C-----------------------------------------------------------------------------
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
      STASUM((OSTA(1,13)-MAX_IZONES),19)=
     *                    STASUM((OSTA(1,13)-MAX_IZONES),19)+TCRBK1
      STASUM((OSTA(1,14)-MAX_IZONES),19)=
     *                    STASUM((OSTA(1,14)-MAX_IZONES),19)+TCRBK2
      STASUM((OSTA(1,9)-MAX_IZONES),25)=
     *                   STASUM((OSTA(1,9)-MAX_IZONES),25)+TCRU1
      STASUM((OSTA(1,10)-MAX_IZONES),25)=
     *                    STASUM((OSTA(1,10)-MAX_IZONES),25)+TCRU2
      STASUM((OSTA(1,11)-MAX_IZONES),25)=
     *                    STASUM((OSTA(1,11)-MAX_IZONES),25)+TCRU3
      STASUM((OSTA(1,12)-MAX_IZONES),25)=
     *                    STASUM((OSTA(1,12)-MAX_IZONES),25)+TCRU4
      CALL VMTCOMP(1,IZ,TCRT,OSTA,VMT,VMTCODE,ZHHD)
C..COMMUTER RAIL STATION EGRESS SUMMARY MATRIX
      IMODE=1
      CALL EGRSUM(IZ,JZ,IMODE,OSTA,ASTA,TCRT,STASUM2,
     *                  CHWIND,CHBKIND,CHBIND,CHPIND,CHKIND,
     *                  STASUM4,STASUM5,WDSTA,BIKDSTA,BDSTA,
     *                  PDSTA,KDSTA,STAEGR,STAVOL)
      CALL ACCSUM(IZ,JZ,C,IMODE,TCRT,
     *                  CHWIND,CHBKIND,CHBIND,CHPIND,CHKIND,
     *                  WDSTA,BIKDSTA,BDSTA,
     *                  PDSTA,KDSTA,XTESUM,XDISTRN,TRIPS)
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
      STASUM((OSTA(2,13)-MAX_IZONES),20)=
     *                    STASUM((OSTA(2,13)-MAX_IZONES),20)+TURBK1
      STASUM((OSTA(2,14)-MAX_IZONES),20)=
     *                    STASUM((OSTA(2,14)-MAX_IZONES),20)+TURBK2
      STASUM((OSTA(2,9)-MAX_IZONES),26)=
     *                    STASUM((OSTA(2,9)-MAX_IZONES),26)+TURU1
      STASUM((OSTA(2,10)-MAX_IZONES),26)=
     *                    STASUM((OSTA(2,10)-MAX_IZONES),26)+TURU2
      STASUM((OSTA(2,11)-MAX_IZONES),26)=
     *                    STASUM((OSTA(2,11)-MAX_IZONES),26)+TURU3
      STASUM((OSTA(2,12)-MAX_IZONES),26)=
     *                    STASUM((OSTA(2,12)-MAX_IZONES),26)+TURU4
      CALL VMTCOMP(2,IZ,TURT,OSTA,VMT,VMTCODE,ZHHD)
C..URBAN RAIL STATION EGRESS SUMMARY MATRIX
      IMODE=2
      CALL EGRSUM(IZ,JZ,IMODE,OSTA,ASTA,TURT,STASUM2,
     *                  CHWIND,CHBKIND,CHBIND,CHPIND,CHKIND,
     *                  STASUM4,STASUM5,WDSTA,BIKDSTA,BDSTA,
     *                  PDSTA,KDSTA,STAEGR,STAVOL)
      CALL ACCSUM(IZ,JZ,C,IMODE,TURT,
     *                  CHWIND,CHBKIND,CHBIND,CHPIND,CHKIND,
     *                  WDSTA,BIKDSTA,BDSTA,
     *                  PDSTA,KDSTA,XTESUM,XDISTRN,TRIPS)
C....MARKET SEGMENT SUMMARY
      STASUM3((ASTA(2,1)-MAX_IZONES),ASTA2(2,1),C)=
     *   STASUM3((ASTA(2,1)-MAX_IZONES),ASTA2(2,1),C)+TURW1
      STASUM3((ASTA(2,2)-MAX_IZONES),ASTA2(2,2),C)=
     *   STASUM3((ASTA(2,2)-MAX_IZONES),ASTA2(2,2),C)+TURW2
      STASUM3((ASTA(2,3)-MAX_IZONES),ASTA2(2,3),C)=
     *   STASUM3((ASTA(2,3)-MAX_IZONES),ASTA2(2,3),C)+TURB1
      STASUM3((ASTA(2,4)-MAX_IZONES),ASTA2(2,4),C)=
     *   STASUM3((ASTA(2,4)-MAX_IZONES),ASTA2(2,4),C)+TURB2
      STASUM3((ASTA(2,5)-MAX_IZONES),ASTA2(2,5),C)=
     *   STASUM3((ASTA(2,5)-MAX_IZONES),ASTA2(2,5),C)+TURP1
      STASUM3((ASTA(2,6)-MAX_IZONES),ASTA2(2,6),C)=
     *   STASUM3((ASTA(2,6)-MAX_IZONES),ASTA2(2,6),C)+TURP2
      STASUM3((ASTA(2,7)-MAX_IZONES),ASTA2(2,7),C)=
     *   STASUM3((ASTA(2,7)-MAX_IZONES),ASTA2(2,7),C)+TURP3
      STASUM3((ASTA(2,8)-MAX_IZONES),ASTA2(2,8),C)=
     *   STASUM3((ASTA(2,8)-MAX_IZONES),ASTA2(2,8),C)+TURP4
      STASUM3((ASTA(2,9)-MAX_IZONES),ASTA2(2,9),C)=
     *   STASUM3((ASTA(2,9)-MAX_IZONES),ASTA2(2,9),C)+TURK1
      STASUM3((ASTA(2,10)-MAX_IZONES),ASTA2(2,10),C)=
     *   STASUM3((ASTA(2,10)-MAX_IZONES),ASTA2(2,10),C)+TURK2
      STASUM3((ASTA(2,11)-MAX_IZONES),ASTA2(2,11),C)=
     *   STASUM3((ASTA(2,11)-MAX_IZONES),ASTA2(2,11),C)+TURK3
      STASUM3((ASTA(2,12)-MAX_IZONES),ASTA2(2,12),C)=
     *   STASUM3((ASTA(2,12)-MAX_IZONES),ASTA2(2,12),C)+TURK4
      STASUM3((ASTA(2,13)-MAX_IZONES),ASTA2(2,13),C)=
     *   STASUM3((ASTA(2,13)-MAX_IZONES),ASTA2(2,13),C)+TURBK1
      STASUM3((ASTA(2,14)-MAX_IZONES),ASTA2(2,14),C)=
     *   STASUM3((ASTA(2,14)-MAX_IZONES),ASTA2(2,14),C)+TURBK2
C..BRT STATION SUMMARY MATRIX
      STASUM((OSTA(5,1)-MAX_IZONES),15)=
     *                    STASUM((OSTA(5,1)-MAX_IZONES),15)+TBRTW1
      STASUM((OSTA(5,2)-MAX_IZONES),15)=
     *                    STASUM((OSTA(5,2)-MAX_IZONES),15)+TBRTW2
      STASUM((OSTA(5,3)-MAX_IZONES),16)=
     *                    STASUM((OSTA(5,3)-MAX_IZONES),16)+TBRTB1
      STASUM((OSTA(5,4)-MAX_IZONES),16)=
     *                    STASUM((OSTA(5,4)-MAX_IZONES),16)+TBRTB2
      STASUM((OSTA(5,5)-MAX_IZONES),17)=
     *                    STASUM((OSTA(5,5)-MAX_IZONES),17)+TBRTP1
      STASUM((OSTA(5,6)-MAX_IZONES),17)=
     *                    STASUM((OSTA(5,6)-MAX_IZONES),17)+TBRTP2
      STASUM((OSTA(5,7)-MAX_IZONES),17)=
     *                    STASUM((OSTA(5,7)-MAX_IZONES),17)+TBRTP3
      STASUM((OSTA(5,8)-MAX_IZONES),17)=
     *                    STASUM((OSTA(5,8)-MAX_IZONES),17)+TBRTP4
      STASUM((OSTA(5,9)-MAX_IZONES),18)=
     *                    STASUM((OSTA(5,9)-MAX_IZONES),18)+TBRTK1
      STASUM((OSTA(5,10)-MAX_IZONES),18)=
     *                    STASUM((OSTA(5,10)-MAX_IZONES),18)+TBRTK2
      STASUM((OSTA(5,11)-MAX_IZONES),18)=
     *                    STASUM((OSTA(5,11)-MAX_IZONES),18)+TBRTK3
      STASUM((OSTA(5,12)-MAX_IZONES),18)=
     *                    STASUM((OSTA(5,12)-MAX_IZONES),18)+TBRTK4
      STASUM((OSTA(5,13)-MAX_IZONES),23)=
     *                    STASUM((OSTA(5,13)-MAX_IZONES),23)+TBRTBK1
      STASUM((OSTA(5,14)-MAX_IZONES),23)=
     *                    STASUM((OSTA(5,14)-MAX_IZONES),23)+TBRTBK2
      STASUM((OSTA(5,9)-MAX_IZONES),27)=
     *                    STASUM((OSTA(5,9)-MAX_IZONES),27)+TBRTU1
      STASUM((OSTA(5,10)-MAX_IZONES),27)=
     *                    STASUM((OSTA(5,10)-MAX_IZONES),27)+TBRTU2
      STASUM((OSTA(5,11)-MAX_IZONES),27)=
     *                    STASUM((OSTA(5,11)-MAX_IZONES),27)+TBRTU3
      STASUM((OSTA(5,12)-MAX_IZONES),27)=
     *                    STASUM((OSTA(5,12)-MAX_IZONES),27)+TBRTU4
      CALL VMTCOMP(5,IZ,TBRTT,OSTA,VMT,VMTCODE,ZHHD)
C..BRT STATION EGRESS SUMMARY MATRIX
      IMODE=5
      CALL EGRSUM(IZ,JZ,IMODE,OSTA,ASTA,TBRTT,STASUM2,
     *                  CHWIND,CHBKIND,CHBIND,CHPIND,CHKIND,
     *                  STASUM4,STASUM5,WDSTA,BIKDSTA,BDSTA,
     *                  PDSTA,KDSTA,STAEGR,STAVOL)
      CALL ACCSUM(IZ,JZ,C,IMODE,TBRTT,
     *                  CHWIND,CHBKIND,CHBIND,CHPIND,CHKIND,
     *                  WDSTA,BIKDSTA,BDSTA,
     *                  PDSTA,KDSTA,XTESUM,XDISTRN,TRIPS)
C..TRANSITWAY STATION SUMMARY
      IF(CWTWY.GT.0.AND.CWTWY.LT.MAX_ZONES) THEN
      STASUM((CWTWY-MAX_IZONES),11)=
     *                    STASUM((CWTWY-MAX_IZONES),11) + TWAYW
      END IF
      IF(CSTAT.GT.0.AND.CSTAT.LT.MAX_ZONES) THEN
      STASUM((CSTAT-MAX_IZONES),12)=
     *                    STASUM((CSTAT-MAX_IZONES),12) + TWAYD
      END IF
      IF(CBTWY.GT.0.AND.CBTWY.LT.MAX_ZONES) THEN
      STASUM((CBTWY-MAX_IZONES),21)=
     *                    STASUM((CBTWY-MAX_IZONES),21) + TWAYB
      END IF
      IF(TWAYW.GT.0.AND.CWTWY.LE.0.0) MISTRP(1)=MISTRP(1)+TWAYW
      IF(TWAYD.GT.0.AND.CSTAT.LE.0.0) MISTRP(3)=MISTRP(3)+TWAYD
      IF(TWAYB.GT.0.AND.CBTWY.LE.0.0) MISTRP(2)=MISTRP(2)+TWAYB
C..EXPRESS BUS STATION SUMMARY
      IF(CWEXP.GT.0.AND.CWEXP.LT.MAX_ZONES) THEN
      STASUM((CWEXP-MAX_IZONES),13)=
     *                    STASUM((CWEXP-MAX_IZONES),13) + TEXPW
      END IF
      IF(CSTAE.GT.0.AND.CSTAE.LT.MAX_ZONES) THEN
      STASUM((CSTAE-MAX_IZONES),14)=
     *                    STASUM((CSTAE-MAX_IZONES),14) + TEXPD
      END IF
      IF(CBEXP.GT.0.AND.CBEXP.LT.MAX_ZONES) THEN
      STASUM((CBEXP-MAX_IZONES),22)=
     *                    STASUM((CBEXP-MAX_IZONES),22) + TEXPB
      END IF
      IF(TEXPW.GT.0.AND.CWEXP.LE.0.0) MISTRP(4)=MISTRP(4)+TEXPW
      IF(TEXPD.GT.0.AND.CSTAE.LE.0.0) MISTRP(6)=MISTRP(6)+TEXPD
      IF(TEXPB.GT.0.AND.CBEXP.LE.0.0) MISTRP(5)=MISTRP(5)+TEXPB
C-----------------------------------------------------------------------
C
C
C..SAVE TRIP LENGTH (DISTANCE) DISTRIBUTION DATA - 
C  COMMUTER RAIL TRIPS
      K=IFIX(TAB2DA(JZ)/5.0)+1
      K=MAX0(K,1)
      K=MIN0(K,15)
      ZNEDIST(1,K)=ZNEDIST(1,K) + TCRW1 + TCRW2
      ZNEDIST(2,K)=ZNEDIST(2,K) + TCRB1 + TCRB2
      ZNEDIST(3,K)=ZNEDIST(3,K) + TCRP1+TCRP2+TCRP3+TCRP4
      ZNEDIST(4,K)=ZNEDIST(4,K) + TCRK1+TCRK2+TCRK3+TCRK4
C URBAN RAIL TRIPS
      ZNEDIST(5,K)=ZNEDIST(5,K) + TURW1 + TURW2
      ZNEDIST(6,K)=ZNEDIST(6,K) + TURB1 + TURB2
      ZNEDIST(7,K)=ZNEDIST(7,K) + TURP1+TURP2+TURP3+TURP4
      ZNEDIST(8,K)=ZNEDIST(8,K) + TURK1+TURK2+TURK3+TURK4
C BRT TRIPS
      ZNEDIST(9,K)=ZNEDIST(9,K) + TBRTW1 + TBRTW2
      ZNEDIST(10,K)=ZNEDIST(10,K) + TBRTB1 + TBRTB2
      ZNEDIST(11,K)=ZNEDIST(11,K) + TBRTP1+TBRTP2+TBRTP3+TBRTP4
      ZNEDIST(12,K)=ZNEDIST(12,K) + TBRTK1+TBRTK2+TBRTK3+TBRTK4
C
C SUMMARIZE TRANSIT TRIPS BY MARKET SEGMENT AND TRIP LENGTH
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
C..SUMMARIZE COMMUTER RAIL PNR TRIPS BY
C  RATIO OF DRIVE ACCESS IVT/CR IVT
C
      KL=IFIX(CRPNR(1)*10.0)
      IF(KL.GE.0.AND.KL.LT.1000) THEN
      IF(KL.EQ.0) KL=1001
      CRPNRSUM(KL)=CRPNRSUM(KL)+TCRP1
      END IF
      KL=IFIX(CRPNR(2)*10.0)
      IF(KL.GE.0.AND.KL.LT.1000) THEN
      IF(KL.EQ.0) KL=1001
      CRPNRSUM(KL)=CRPNRSUM(KL)+TCRP2
      END IF
      KL=IFIX(CRPNR(3)*10.0)
      IF(KL.GE.0.AND.KL.LT.1000) THEN
      IF(KL.EQ.0) KL=1001 
      CRPNRSUM(KL)=CRPNRSUM(KL)+TCRP3
      END IF
      KL=IFIX(CRPNR(4)*10.0)
      IF(KL.GT.0.AND.KL.LT.1000) THEN
      IF(KL.EQ.0) KL=1001 
      CRPNRSUM(KL)=CRPNRSUM(KL)+TCRP4
      END IF
C
C..SUMMARIZE COMMUTER RAIL PNR TRIPS BY
C  RATIO OF DRIVE ACCESS DISTANCE/TOTAL DISTANCE
C
      KL=IFIX(CRPNR2(1)*10.0)
      IF(KL.GE.0.AND.KL.LT.1000) THEN
      IF(KL.EQ.0) KL=1001
      CRPNRRAT(KL)=CRPNRRAT(KL)+TCRP1
      END IF
      KL=IFIX(CRPNR2(2)*10.0)
      IF(KL.GE.0.AND.KL.LT.1000) THEN
      IF(KL.EQ.0) KL=1001 
      CRPNRRAT(KL)=CRPNRRAT(KL)+TCRP2
      END IF
      KL=IFIX(CRPNR2(3)*10.0)
      IF(KL.GE.0.AND.KL.LT.1000) THEN
      IF(KL.EQ.0) KL=1001 
      CRPNRRAT(KL)=CRPNRRAT(KL)+TCRP3
      END IF
      KL=IFIX(CRPNR2(4)*10.0)
      IF(KL.GE.0.AND.KL.LT.1000) THEN
      IF(KL.EQ.0) KL=1001 
      CRPNRRAT(KL)=CRPNRRAT(KL)+TCRP4
      END IF
C
C..SUMMARIZE DRIVE TO EXPRESS BUS, TRANSITWAY
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
C
C..SUMMARIZE DRIVE TO EXPRESS BUS, TRANSIWAY
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
C...................................................
      IF(DEBUG) THEN
      WRITE(26,9555) CRPNR(1),TCRP1,CRPNR(2),TCRP2,
     *               CRPNR(3),TCRP3,CRPNR(4),TCRP4,
     *               CRPNR2(1),TCRP1,CRPNR2(2),TCRP2,
     *               CRPNR2(3),TCRP3,CRPNR2(4),TCRP4
 9555 FORMAT(/,1X,'SUMMARY OF CR DRIVE IVT/CR IVT RESULTS'/
     *         1X,'--------------------------------------'/
     *         3X,'CRPNR(1)=',F7.5,' TCRP1=',E12.5/
     *         3X,'CRPNR(2)=',F7.5,' TCRP2=',E12.5/
     *         3X,'CRPNR(3)=',F7.5,' TCRP3=',E12.5/
     *         3X,'CRPNR(4)=',F7.5,' TCRP4=',E12.5//
     *         1X,'SUMMARY OF CR DRIVE DST/TOTAL DIST RESULTS'/
     *         1X,'--------------------------------------'/
     *         3X,'CRPNR2(1)=',F7.5,' TCRP1=',E12.5/
     *         3X,'CRPNR2(2)=',F7.5,' TCRP2=',E12.5/
     *         3X,'CRPNR2(3)=',F7.5,' TCRP3=',E12.5/
     *         3X,'CRPNR2(4)=',F7.5,' TCRP4=',E12.5/)
      END IF
C...........................................................
C
C
C..FILL OUTPUT TRIPS MATRIX
c...origin zone to origin station - FOR DRIVE CAN COMBINE MODES
c...walk to commuter rail
      orista=osta(1,1)-MAX_IZONES
      wlkcr(iz,orista)=wlkcr(iz,orista) + sngl(tcrw1)
      orista=osta(1,2)-MAX_IZONES
      wlkcr(iz,orista)=wlkcr(iz,orista) + sngl(tcrw2)
c...bike to commuter rail
      orista=osta(1,13)-MAX_IZONES
      bikcr(iz,orista)=bikcr(iz,orista) + sngl(tcrbk1)
      orista=osta(1,14)-MAX_IZONES
      bikcr(iz,orista)=bikcr(iz,orista) + sngl(tcrbk2)
c...bus to commuter rail
      orista=osta(1,3)-MAX_IZONES
      bcr(iz,orista)=bcr(iz,orista) + sngl(tcrb1)
      orista=osta(1,4)-MAX_IZONES
      bcr(iz,orista)=bcr(iz,orista) + sngl(tcrb2)
c...walk to urban rail
      orista=osta(2,1)-MAX_IZONES
      wlkur(iz,orista)=wlkur(iz,orista) + sngl(turw1)
      orista=osta(2,2)-MAX_IZONES
      wlkur(iz,orista)=wlkur(iz,orista) + sngl(turw2)
c...bike to urban rail
      orista=osta(2,13)-MAX_IZONES
      bikur(iz,orista)=bikur(iz,orista) + sngl(turbk1)
      orista=osta(2,14)-MAX_IZONES
      bikur(iz,orista)=bikur(iz,orista) + sngl(turbk2)
c...bus to urban rail
      orista=osta(2,3)-MAX_IZONES
      bur(iz,orista)=bur(iz,orista) + sngl(turb1)
      orista=osta(2,4)-MAX_IZONES
      bur(iz,orista)=bur(iz,orista) + sngl(turb2)
c...walk to bus rapid transit
      orista=osta(5,1)-MAX_IZONES
      wlkbrt(iz,orista)=wlkbrt(iz,orista) + sngl(tbrtw1)
      orista=osta(5,2)-MAX_IZONES
      wlkbrt(iz,orista)=wlkbrt(iz,orista) + sngl(tbrtw2)
c...bike to bus rapid transit
      orista=osta(5,13)-MAX_IZONES
      bikbrt(iz,orista)=bikbrt(iz,orista) + sngl(tbrtbk1)
      orista=osta(5,14)-MAX_IZONES
      bikbrt(iz,orista)=bikbrt(iz,orista) + sngl(tbrtbk2)
c...bus to bus rapid transit
      orista=osta(5,3)-MAX_IZONES
      bbrt(iz,orista)=bbrt(iz,orista) + sngl(tbrtb1)
      orista=osta(5,4)-MAX_IZONES
      bbrt(iz,orista)=bbrt(iz,orista) + sngl(tbrtb2)
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
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcru1)
      orista=osta(1,10)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrk2)
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcru2)
      orista=osta(1,11)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrk3)
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcru3)
      orista=osta(1,12)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrk4)
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcru4)
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
      dtran(iz,orista)=dtran(iz,orista) + sngl(turu1)
      orista=osta(2,10)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turk2)
      dtran(iz,orista)=dtran(iz,orista) + sngl(turu2)
      orista=osta(2,11)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turk3)
      dtran(iz,orista)=dtran(iz,orista) + sngl(turu3)
      orista=osta(2,12)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turk4)
      dtran(iz,orista)=dtran(iz,orista) + sngl(turu4)
c....drive to bus rapid transit
      orista=osta(5,5)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tbrtp1)
      orista=osta(5,6)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tbrtp2)
      orista=osta(5,7)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tbrtp3)
      orista=osta(5,8)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tbrtp4)
      orista=osta(5,9)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tbrtk1)
      dtran(iz,orista)=dtran(iz,orista) + sngl(tbrtu1)
      orista=osta(5,10)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tbrtk2)
      dtran(iz,orista)=dtran(iz,orista) + sngl(tbrtu2)
      orista=osta(5,11)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tbrtk3)
      dtran(iz,orista)=dtran(iz,orista) + sngl(tbrtu3)
      orista=osta(5,12)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tbrtk4)
      dtran(iz,orista)=dtran(iz,orista) + sngl(tbrtu4)
c...drive to express bus
      orista=cstae-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(texpd)
c...drive to transitway  bus
      orista=cstat-MAX_IZONES
      if(orista.gt.0) then
      dtran(iz,orista)=dtran(iz,orista) + sngl(twayd)
      end if
c
c...Station-to-Station tables
c
      LSHARE=2.0
c...commuter rail
      IMODE=1
      CALL STSSUM(IMODE,OSTA,ASTA,TCRT,CRSS,CRURBRTSS,
     *                  CHWIND,CHBKIND,CHBIND,CHPIND,CHKIND,
     *                  WDSTA,BIKDSTA,BDSTA,PDSTA,KDSTA,LSHARE)
c...urban rail
      IMODE=2
      CALL STSSUM(IMODE,OSTA,ASTA,TURT,URSS,CRURBRTSS,
     *                  CHWIND,CHBKIND,CHBIND,CHPIND,CHKIND,
     *                  WDSTA,BIKDSTA,BDSTA,PDSTA,KDSTA,LSHARE)
c...bus rapid transit
      IMODE=5
      CALL STSSUM(IMODE,OSTA,ASTA,TBRTT,BRTSS,CRURBRTSS,
     *                  CHWIND,CHBKIND,CHBIND,CHPIND,CHKIND,
     *                  WDSTA,BIKDSTA,BDSTA,PDSTA,KDSTA,LSHARE)
c
c...Station-to-Station Tables for Low Income
c
      IF((C.EQ.1).AND.(LOWRAIL)) THEN
      IF(NCATS.EQ.1) THEN
      LDIST=LDISTEQ(IZ)
      LSHARE=LOWDIST(LDIST)
      ELSE
      LSHARE=1.0
      END IF
c...commuter rail
      IMODE=1
      CALL STSSUM(IMODE,OSTA,ASTA,TCRT,CRSSL,CRURBRTSSL,
     *                  CHWIND,CHBKIND,CHBIND,CHPIND,CHKIND,
     *                  WDSTA,BIKDSTA,BDSTA,PDSTA,KDSTA,LSHARE)
c...urban rail
      IMODE=2
      CALL STSSUM(IMODE,OSTA,ASTA,TURT,URSSL,CRURBRTSSL,
     *                  CHWIND,CHBKIND,CHBIND,CHPIND,CHKIND,
     *                  WDSTA,BIKDSTA,BDSTA,PDSTA,KDSTA,LSHARE)
c...bus rapid transit
      IMODE=5
      CALL STSSUM(IMODE,OSTA,ASTA,TBRTT,BRTSSL,CRURBRTSSL,
     *                  CHWIND,CHBKIND,CHBIND,CHPIND,CHKIND,
     *                  WDSTA,BIKDSTA,BDSTA,PDSTA,KDSTA,LSHARE)
      END IF
c
c...Express Bus & Transitway drive access station to dest. zone
c
      orista=cstae-MAX_IZONES
      ebstaz(orista,jz)=ebstaz(orista,jz) + sngl(texpd)
      orista=cstat-MAX_IZONES
      if(orista.gt.0) then
      twstaz(orista,jz)=twstaz(orista,jz) + sngl(twayd)
      end if
c
c...Destination Station to Destination Zone
c
c...commuter rail
      IMODE=1
      CALL EGRTRP(JZ,IMODE,OSTA,ASTA,TCRT,CRSTAZ,ALLSTAZ,
     *                  CHWIND,CHBKIND,CHBIND,CHPIND,CHKIND,
     *                  WDSTA,BIKDSTA,BDSTA,PDSTA,KDSTA,STAEGR,
     *                  CRWLK,ALLWLK,CRTNC,ALLTNC)
c...urban rail
      IMODE=2
      CALL EGRTRP(JZ,IMODE,OSTA,ASTA,TURT,URSTAZ,ALLSTAZ,
     *                  CHWIND,CHBKIND,CHBIND,CHPIND,CHKIND,
     *                  WDSTA,BIKDSTA,BDSTA,PDSTA,KDSTA,STAEGR,
     *                  URWLK,ALLWLK,URTNC,ALLTNC)
c...bus rapid transit
      IMODE=5
      CALL EGRTRP(JZ,IMODE,OSTA,ASTA,TBRTT,BRTSTAZ,ALLSTAZ,
     *                  CHWIND,CHBKIND,CHBIND,CHPIND,CHKIND,
     *                  WDSTA,BIKDSTA,BDSTA,PDSTA,KDSTA,STAEGR,
     *                  BRTWLK,ALLWLK,BRTTNC,ALLTNC)
C
      IF(HHLDAV(AV).EQ.0) THEN
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
      VMT(1,CNTYINDX)=VMT(1,CNTYINDX)+TDRV0N*TAB2DA(JZ)
      VMT(2,CNTYINDX)=VMT(2,CNTYINDX)+TDRV0T*TAB5DA(JZ)
      VMT(3,CNTYINDX)=VMT(3,CNTYINDX)+TDRV2NH*TAB22P(JZ)
      VMT(4,CNTYINDX)=VMT(4,CNTYINDX)+TDRV2NN*TAB2DA(JZ)
      VMT(5,CNTYINDX)=VMT(5,CNTYINDX)+TDRV2TH*TAB52P(JZ)
      VMT(6,CNTYINDX)=VMT(6,CNTYINDX)+TDRV2TN*TAB122P(JZ)
      VMT(7,CNTYINDX)=VMT(7,CNTYINDX)+TDRV3NH*TAB23P(JZ)
      VMT(8,CNTYINDX)=VMT(8,CNTYINDX)+TDRV3NN*TAB2DA(JZ)
      VMT(9,CNTYINDX)=VMT(9,CNTYINDX)+TDRV3TH*TAB53P(JZ)
      VMT(10,CNTYINDX)=VMT(10,CNTYINDX)+TDRV3TN*TAB123P(JZ)
      VMT(11,CNTYINDX)=VMT(11,CNTYINDX)+TDRV4NH*TAB24P(JZ)
      VMT(12,CNTYINDX)=VMT(12,CNTYINDX)+TDRV4NN*TAB2DA(JZ)
      VMT(13,CNTYINDX)=VMT(13,CNTYINDX)+TDRV4TH*TAB54P(JZ)
      VMT(14,CNTYINDX)=VMT(14,CNTYINDX)+TDRV4TN*TAB124P(JZ)
      ELSE
      AVTRIPS( 1,JZ)=AVTRIPS( 1,JZ)+TDRV0N
      AVTRIPS( 2,JZ)=AVTRIPS( 2,JZ)+TDRV0T
      AVTRIPS( 3,JZ)=AVTRIPS( 3,JZ)+TDRV2NH
      AVTRIPS( 4,JZ)=AVTRIPS( 4,JZ)+TDRV2NN
      AVTRIPS( 5,JZ)=AVTRIPS( 5,JZ)+TDRV2TH
      AVTRIPS( 6,JZ)=AVTRIPS( 6,JZ)+TDRV2TN
      AVTRIPS( 7,JZ)=AVTRIPS( 7,JZ)+TDRV3NH
      AVTRIPS( 8,JZ)=AVTRIPS( 8,JZ)+TDRV3NN
      AVTRIPS( 9,JZ)=AVTRIPS( 9,JZ)+TDRV3TH
      AVTRIPS(10,JZ)=AVTRIPS(10,JZ)+TDRV3TN
      AVTRIPS(11,JZ)=AVTRIPS(11,JZ)+TDRV4NH
      AVTRIPS(12,JZ)=AVTRIPS(12,JZ)+TDRV4NN
      AVTRIPS(13,JZ)=AVTRIPS(13,JZ)+TDRV4TH
      AVTRIPS(14,JZ)=AVTRIPS(14,JZ)+TDRV4TN  
      VMT(15,CNTYINDX)=VMT(15,CNTYINDX)+TDRV0N*TAB2DA(JZ)
      VMT(16,CNTYINDX)=VMT(16,CNTYINDX)+TDRV0T*TAB5DA(JZ)
      VMT(17,CNTYINDX)=VMT(17,CNTYINDX)+TDRV2NH*TAB22P(JZ)
      VMT(18,CNTYINDX)=VMT(18,CNTYINDX)+TDRV2NN*TAB2DA(JZ)
      VMT(19,CNTYINDX)=VMT(19,CNTYINDX)+TDRV2TH*TAB52P(JZ)
      VMT(20,CNTYINDX)=VMT(20,CNTYINDX)+TDRV2TN*TAB122P(JZ)
      VMT(21,CNTYINDX)=VMT(21,CNTYINDX)+TDRV3NH*TAB23P(JZ)
      VMT(22,CNTYINDX)=VMT(22,CNTYINDX)+TDRV3NN*TAB2DA(JZ)
      VMT(23,CNTYINDX)=VMT(23,CNTYINDX)+TDRV3TH*TAB53P(JZ)
      VMT(24,CNTYINDX)=VMT(24,CNTYINDX)+TDRV3TN*TAB123P(JZ)
      VMT(25,CNTYINDX)=VMT(25,CNTYINDX)+TDRV4NH*TAB24P(JZ)
      VMT(26,CNTYINDX)=VMT(26,CNTYINDX)+TDRV4NN*TAB2DA(JZ)
      VMT(27,CNTYINDX)=VMT(27,CNTYINDX)+TDRV4TH*TAB54P(JZ)
      VMT(28,CNTYINDX)=VMT(28,CNTYINDX)+TDRV4TN*TAB124P(JZ)    
      END IF
      IF(UBERAV(AV).EQ.0) THEN
      TRIPS(88,JZ)=TRIPS(88,JZ)+TUBER
      VMT(37,CNTYINDX)=VMT(37,CNTYINDX)+TUBER
      ELSE
      AVTRIPS(15,JZ)=AVTRIPS(15,JZ)+TUBER   
      VMT(38,CNTYINDX)=VMT(38,CNTYINDX)+TUBER         
      END IF
      VMT(39,CNTYINDX)=VMT(39,CNTYINDX)+TAXIMAAS
      TRIPS(58,JZ)=TRIPS(58,JZ)+TNMWK
      TRIPS(59,JZ)=TRIPS(59,JZ)+TNMBK
      TRIPS(87,JZ)=TRIPS(87,JZ)+TAXIMAAS
      TRIPS(89,JZ)=TRIPS(89,JZ)+TNMSC
  255 CONTINUE
      TRIPS(11,JZ)=TRIPS(11,JZ)+TLOCW
      TRIPS(53,JZ)=TRIPS(53,JZ)+TLOCB
      TRIPS(12,JZ)=TRIPS(12,JZ)+TLOCD
      TRIPS(13,JZ)=TRIPS(13,JZ)+TEXPW
      TRIPS(55,JZ)=TRIPS(55,JZ)+TEXPB
      TRIPS(14,JZ)=TRIPS(14,JZ)+TEXPD
      TRIPS(15,JZ)=TRIPS(15,JZ)+TCRW1
      TRIPS(16,JZ)=TRIPS(16,JZ)+TCRW2
      TRIPS(17,JZ)=TRIPS(17,JZ)+TCRB1
      TRIPS(18,JZ)=TRIPS(18,JZ)+TCRB2
      TRIPS(60,JZ)=TRIPS(60,JZ)+TCRBK1+TCRBK2
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
      TRIPS(61,JZ)=TRIPS(61,JZ)+TURBK1+TURBK2
      TRIPS(31,JZ)=TRIPS(31,JZ)+TURP1
      TRIPS(32,JZ)=TRIPS(32,JZ)+TURP2
      TRIPS(33,JZ)=TRIPS(33,JZ)+TURP3
      TRIPS(34,JZ)=TRIPS(34,JZ)+TURP4
      TRIPS(35,JZ)=TRIPS(35,JZ)+TURK1
      TRIPS(36,JZ)=TRIPS(36,JZ)+TURK2
      TRIPS(37,JZ)=TRIPS(37,JZ)+TURK3
      TRIPS(38,JZ)=TRIPS(38,JZ)+TURK4
      TRIPS(39,JZ)=TRIPS(39,JZ)+TWAYW
      TRIPS(56,JZ)=TRIPS(56,JZ)+TWAYB
      TRIPS(40,JZ)=TRIPS(40,JZ)+TWAYD
      TRIPS(41,JZ)=TRIPS(41,JZ)+TCR
      TRIPS(42,JZ)=TRIPS(42,JZ)+TUR
      TRIPS(43,JZ)=TRIPS(43,JZ)+TWAY
      TRIPS(44,JZ)=TRIPS(44,JZ)+TRPDW
      TRIPS(54,JZ)=TRIPS(54,JZ)+TRPDB
      TRIPS(45,JZ)=TRIPS(45,JZ)+TRPDD
      TRIPS(46,JZ)=TRIPS(46,JZ)+TBRTW1
      TRIPS(47,JZ)=TRIPS(47,JZ)+TBRTW2
      TRIPS(48,JZ)=TRIPS(48,JZ)+TBRTB1
      TRIPS(57,JZ)=TRIPS(57,JZ)+TBRTB2
      TRIPS(62,JZ)=TRIPS(62,JZ)+TBRTP1
      TRIPS(63,JZ)=TRIPS(63,JZ)+TBRTP2
      TRIPS(64,JZ)=TRIPS(64,JZ)+TBRTP3
      TRIPS(65,JZ)=TRIPS(65,JZ)+TBRTP4
      TRIPS(66,JZ)=TRIPS(66,JZ)+TBRTK1
      TRIPS(67,JZ)=TRIPS(67,JZ)+TBRTK2
      TRIPS(68,JZ)=TRIPS(68,JZ)+TBRTK3
      TRIPS(69,JZ)=TRIPS(69,JZ)+TBRTK4
      TRIPS(70,JZ)=TRIPS(70,JZ)+TBRTBK1+TBRTBK2
C
      TRIPS(75,JZ)=TRIPS(75,JZ)+TCRU1
      TRIPS(76,JZ)=TRIPS(76,JZ)+TCRU2
      TRIPS(77,JZ)=TRIPS(77,JZ)+TCRU3
      TRIPS(78,JZ)=TRIPS(78,JZ)+TCRU4
      TRIPS(79,JZ)=TRIPS(79,JZ)+TURU1
      TRIPS(80,JZ)=TRIPS(80,JZ)+TURU2
      TRIPS(81,JZ)=TRIPS(81,JZ)+TURU3
      TRIPS(82,JZ)=TRIPS(82,JZ)+TURU4
      TRIPS(83,JZ)=TRIPS(83,JZ)+TBRTU1
      TRIPS(84,JZ)=TRIPS(84,JZ)+TBRTU2
      TRIPS(85,JZ)=TRIPS(85,JZ)+TBRTU3
      TRIPS(86,JZ)=TRIPS(86,JZ)+TBRTU4
C
C SUMMARIZE AUTO PERSON TRIPS BY TOLL/NON TOLL & HOV
C
      IF(HHLDAV(AV).EQ.0) THEN
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
C....AUTOMATED VEHICLE SUMMARY
      ELSE
      TOLSUM(15,C)=TOLSUM(15,C)+TDRV0N
      TOLSUM(16,C)=TOLSUM(16,C)+TDRV0T
      TOLSUM(17,C)=TOLSUM(17,C)+TDRV2NH
      TOLSUM(18,C)=TOLSUM(18,C)+TDRV2NN
      TOLSUM(19,C)=TOLSUM(19,C)+TDRV2TH
      TOLSUM(20,C)=TOLSUM(20,C)+TDRV2TN
      TOLSUM(21,C)=TOLSUM(21,C)+TDRV3NH
      TOLSUM(22,C)=TOLSUM(22,C)+TDRV3NN
      TOLSUM(23,C)=TOLSUM(23,C)+TDRV3TH
      TOLSUM(24,C)=TOLSUM(24,C)+TDRV3TN
      TOLSUM(25,C)=TOLSUM(25,C)+TDRV4NH
      TOLSUM(26,C)=TOLSUM(26,C)+TDRV4NN
      TOLSUM(27,C)=TOLSUM(27,C)+TDRV4TH
      TOLSUM(28,C)=TOLSUM(28,C)+TDRV4TN
      END IF
C...PARKING LOCATION
      IF(HHLDAV(AV).EQ.1) THEN
      TESUM(86,C)=TESUM(86,C) + TAUTO*PROBWRK
      IF(HOMEAV) THEN
      TESUM(87,C)=TESUM(87,C) + TAUTO*PROBOTH
      ELSE
      TESUM(88,C)=TESUM(88,C) + TAUTO*PROBOTH
      END IF
      END IF
C.....................................................................
C
C END OF M-INDEX WALK LOOP
C
 2000 CONTINUE
C
C END OF C-INDEX MARKET SEGMENT LOOP
C
C
C OUTPUT FTA USER BENEFIT RECORD
C
      IF(USERBEN.AND.(.NOT.LAXTRN)) THEN
      IF(CAPRES.AND.(ITER.LT.(NITER-1))) GO TO 1001
       IF(IZ.EQ.JZ) THEN
       AUTEXP=0.99
       TWALK(1)=1.0
       TWALK(2)=0.0
       TSHAR(1)=0.0
       TSHAR(2)=0.0
       END IF
       WRITE(29) IZ,JZ,C,PERIN(C,JZ),PERIN(C,JZ),AUTEXP,
     *          TWALK(1),TSHAR(1),TWALK(2),TSHAR(2)
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
c ----- Automated Vehicle Segmentation Loop End
 1800 CONTINUE
C
 1000 CONTINUE
C
C AVERAGE TRANSIT LOGSUM COMPUTATIONS
C
      IF(TRNUSER) THEN
      DO C=1,5
       DO II=1,7
        ETRNLSUM=0.0
        DO M=1,6
        IF(MWALK(M).GT.0) ETRNLSUM(M)=EXP(TRNLSUM(JZ,II,M,C)+LNWALK(M))
        ETRNLSUM(7)=ETRNLSUM(7)+ETRNLSUM(M)    
        IF(VDETAIL) WRITE(26,8102) 
     *           C,II,M,TRNLSUM(JZ,II,M,C),
     *           LNWALK(M),ETRNLSUM
 8102   FORMAT(' C=',I1,' II=',I1,' M=',I1,' TRNLSUM=',F8.4,
     *         ' LNWALK=',1X,F8.4,' ETRNLSUM=',7(1X,F8.4))
        END DO
        IF(ETRNLSUM(7).NE.0.0) THEN
        TRNLSUM(JZ,II,7,C)=LOG(ETRNLSUM(7))/COEFF(100)
        END IF
       END DO
      END DO
C ----------------------------------------------------------      
      IF(DEBUG) THEN
      WRITE(26,9779)
 9779 FORMAT(1X,'AVERAGE LOGSUM VALUES'/
     *       1X,'---------------------'/
     *       1X,'  Transit Mode    Inc 1     Inc 2      Inc 3'/
     *       1X,'---------------  --------  --------  --------')
      DO II=1,7
      WRITE(26,9778) TRNAME(II),(TRNLSUM(JZ,II,7,C),C=1,3)
 9778 FORMAT(A15,3(2X,F8.2))
      END DO
      END IF
C ----------------------------------------------------------   
      END IF
C
C  OUTPUT ALTERNATIVE PARKING LOCATION/HOME TRIPS
C
      IF(AVMDL.AND.ALTPARK.AND.TRIPSOUT) THEN
C..................................................................
      IF(DEBUG) THEN
      WRITE(26,9726) (APARKTRP(K,1),K=1,4),APRKZONE(JZ),
     *               (APARKTRP(K1,2),K1=1,4)
 9726 FORMAT(' ALTERNATIVE PARKING TRIP SUMMARY'/
     *       ' --------------------------------'/
     *       ' --- RETURN HOME ---'/
     *       ' DRIVE ALONE=',F8.2/
     *       ' 2 PERSON   =',F8.2/
     *       ' 3 PERSON   =',F8.2/
     *       ' 4+PERSON   =',F8.2/
     *       ' --- PARKING ZONE ',I4,' ---'/
     *       ' DRIVE ALONE=',F8.2/
     *       ' 2 PERSON   =',F8.2/
     *       ' 3 PERSON   =',F8.2/
     *       ' 4+PERSON   =',F8.2/)
      END IF
C........................................................................
      RECNO = ((JZ-1)*MXZONES) + IZ
      WRITE(251,REC=RECNO) APARKTRP(1,1)
      WRITE(252,REC=RECNO) APARKTRP(2,1)
      WRITE(253,REC=RECNO) APARKTRP(3,1)
      WRITE(254,REC=RECNO) APARKTRP(4,1)
      IF(APRKZONE(JZ).GT.0) THEN
      RECNO = ((JZ-1)*MXZONES) + APRKZONE(JZ)
      READ(255,REC=RECNO) MFVAL
      MFVAL=MFVAL+APARKTRP(1,2)
      WRITE(255,REC=RECNO) MFVAL
      READ(256,REC=RECNO) MFVAL
      MFVAL=MFVAL+APARKTRP(2,2)
      WRITE(256,REC=RECNO) MFVAL
      READ(257,REC=RECNO) MFVAL
      MFVAL=MFVAL+APARKTRP(3,2)
      WRITE(257,REC=RECNO) MFVAL
      READ(258,REC=RECNO) MFVAL
      MFVAL=MFVAL+APARKTRP(4,2)
      WRITE(258,REC=RECNO) MFVAL
      END IF
      END IF
  200 CONTINUE
C
C  END OF THE J-ZONE LOOP
C
C
C     OUTPUT TRIPS MATRIX FOR JZ HERE
C
 1121 CONTINUE
C
C     OUTPUT SPECIAL EVENT PERSON TRIP MATRICES
C
      IF(EVENTLI.AND.(.NOT.EVENTPT)) THEN
      DO JZ=1,MAX_ZONES
      ROW(JZ)=IFIX(PERSON(JZ)*100.0)
      END DO
      PURP=1
      CALL OUTAB(82,ROW,IZ,PURP,DUMMY,IO)
      END IF 
C
C   OUTPUT AVERAGE TRANSIT LOGSUM VALUES (IF REQUESTED)
C
      IF(TRNUSER) THEN
      DO 3892 M=1,5
      DO 3893 K=1,7
      DO 3894 JZ=1,MAX_ZONES
      TLSUMZ(JZ)=IDINT(TRNLSUM(JZ,K,7,M)*100.0)
 3894 CONTINUE
      PURP=(M-1)*7+K
      CALL OUTAB(69,TLSUMZ,IZ,PURP,DUMMY,IO)
 3893 CONTINUE
 3892 CONTINUE
      END IF
C
C   OUTPUT HIGHWAY TRIP MATRICES
C
      if(CALIB.AND.(CITER.LT.MAXCALIT)) goto 100
      if(CAPRES.AND.(ITER.LT.(NITER-1))) goto 100
      IF(.NOT.TRIPSOUT) GO TO 99
      IF(LAXTRN) GO TO 99
      IF(AIRPASS.AND.(.NOT.RECYC)) GO TO 99
C...DA NON-TOLL
      rem21=0.0
      PURP=1
      DO 3301,JJ=1,MAX_ZONES
      if(trips(1,jj).gt.0.0) then
      temp=(trips(1,jj)*100.0) + rem21
      ROW(JJ)=IFIX(temp)
      rem21=temp-row(jj)
      else
      row(jj)=0
      endif
 3301 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...DA TOLL
      PURP=2
      rem22=0.0
      DO 3302,JJ=1,MAX_ZONES
      if(trips(2,jj).gt.0.0) then
      temp=(trips(2,jj)*100.0) + rem22
      ROW(JJ)=IFIX(temp)
      rem22=temp-row(jj)
      else
      row(jj)=0
      endif
 3302 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...2-PERSON NON-TOLL HOV
      PURP=3
      rem23=0.0
      DO 3303,JJ=1,MAX_ZONES
      if(trips(3,jj).gt.0.0) then
      temp=(trips(3,jj)*100.0) + rem23
      ROW(JJ)=IFIX(temp)
      rem23=temp-row(jj)
      else
      row(jj)=0
      endif
 3303 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...2-PERSON NON-TOLL NON-HOV
      PURP=4
      rem24=0.0
      DO 3304,JJ=1,MAX_ZONES
      if(trips(4,jj).gt.0.0) then
      temp=(trips(4,jj)*100.0) + rem24
      ROW(JJ)=IFIX(temp)
      rem24=temp-row(jj)
      else
      row(jj)=0
      endif
 3304 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...2-PERSON TOLL HOV
      PURP=5
      rem25=0.0
      DO 3305,JJ=1,MAX_ZONES
      if(trips(5,jj).gt.0.0) then
      temp=(trips(5,jj)*100.0) + rem25
      ROW(JJ)=IFIX(temp)
      rem25=temp-row(jj)
      else
      row(jj)=0
      endif
 3305 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...2-PERSON TOLL NON-HOV
      PURP=6
      rem26=0.0
      DO 3306,JJ=1,MAX_ZONES
      if(trips(6,jj).gt.0.0) then
      temp=(trips(6,jj)*100.0) + rem26
      ROW(JJ)=IFIX(temp)
      rem26=temp-row(jj)
      else
      row(jj)=0
      endif
 3306 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...3-PERSON NON-TOLL HOV
      PURP=7
      rem27=0.0
      DO 3307,JJ=1,MAX_ZONES
      if(trips(7,jj).gt.0.0) then
      temp=(trips(7,jj)*100.0) + rem27
      ROW(JJ)=IFIX(temp)
      rem27=temp-row(jj)
      else
      row(jj)=0
      endif
 3307 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...3-PERSON NON-TOLL NON-HOV
      PURP=8
      rem28=0.0
      DO 3308,JJ=1,MAX_ZONES
      if(trips(8,jj).gt.0.0) then
      temp=(trips(8,jj)*100.0) + rem28
      ROW(JJ)=IFIX(temp)
      rem28=temp-row(jj)
      else
      row(jj)=0
      endif
 3308 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...3-PERSON TOLL HOV
      PURP=9
      rem29=0.0
      DO 3309,JJ=1,MAX_ZONES
      if(trips(9,jj).gt.0.0) then
      temp=(trips(9,jj)*100.0) + rem29
      ROW(JJ)=IFIX(temp)
      rem29=temp-row(jj)
      else
      row(jj)=0
      endif
 3309 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...3-PERSON TOLL NON-HOV
      PURP=10
      rem20=0.0
      DO 3310,JJ=1,MAX_ZONES
      if(trips(10,jj).gt.0.0) then
      temp=(trips(10,jj)*100.0) + rem20
      ROW(JJ)=IFIX(temp)
      rem20=temp-row(jj)
      else
      row(jj)=0
      endif
 3310 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...4-PERSON NON-TOLL HOV
      PURP=11
      rem35=0.0
      DO 3347,JJ=1,MAX_ZONES
      if(trips(49,jj).gt.0.0) then
      temp=(trips(49,jj)*100.0) + rem35
      ROW(JJ)=IFIX(temp)
      rem35=temp-row(jj)
      else
      row(jj)=0
      endif
 3347 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...4-PERSON NON-TOLL NON-HOV
      PURP=12
      rem36=0.0
      DO 3348,JJ=1,MAX_ZONES
      if(trips(50,jj).gt.0.0) then
      temp=(trips(50,jj)*100.0) + rem36
      ROW(JJ)=IFIX(temp)
      rem36=temp-row(jj)
      else
      row(jj)=0
      endif
 3348 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...4-PERSON TOLL HOV
      PURP=13
      rem37=0.0
      DO 3349,JJ=1,MAX_ZONES
      if(trips(51,jj).gt.0.0) then
      temp=(trips(51,jj)*100.0) + rem37
      ROW(JJ)=IFIX(temp)
      rem37=temp-row(jj)
      else
      row(jj)=0
      endif
 3349 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...4-PERSON TOLL NON-HOV
      PURP=14
      rem38=0.0
      DO 3342,JJ=1,MAX_ZONES
      if(trips(52,jj).gt.0.0) then
      temp=(trips(52,jj)*100.0) + rem38
      ROW(JJ)=IFIX(temp)
      rem38=temp-row(jj)
      else
      row(jj)=0
      endif
 3342 CONTINUE
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...TAXI
      PURP=15
      rem37=0.0
      DO JJ=1,MAX_ZONES
      if(trips(87,jj).gt.0.0.and.(taximode)) then
      temp=(trips(87,jj)*100.0) + rem37
      ROW(JJ)=IFIX(temp)
      rem37=temp-row(jj)
      else
      row(jj)=0
      endif
      END DO
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C
C...UBER
      PURP=16
      rem38=0.0
      DO JJ=1,MAX_ZONES
      if(trips(88,jj).gt.0.0.and.(ubermode)) then
      temp=(trips(88,jj)*100.0) + rem38
      ROW(JJ)=IFIX(temp)
      rem38=temp-row(jj)
      else
      row(jj)=0
      endif
      END DO
      CALL OUTAB(21,ROW,IZ,PURP,DUMMY,IO)
C ------------------------------------------------------
C OUTPUT AV TRIP MATRICES
C ------------------------------------------------------
      IF(AVMDL) THEN
C...DA NON-TOLL
      rem21=0.0
      PURP=1
      DO JJ=1,MAX_ZONES
      if(AVTRIPS(1,jj).gt.0.0) then
      temp=(AVTRIPS(1,jj)*100.0) + rem21
      ROW(JJ)=IFIX(temp)
      rem21=temp-row(jj)
      else
      row(jj)=0
      endif
      END DO
      CALL OUTAB(144,ROW,IZ,PURP,DUMMY,IO)
C
C...DA TOLL
      PURP=2
      rem22=0.0
      DO JJ=1,MAX_ZONES
      if(AVTRIPS(2,jj).gt.0.0) then
      temp=(AVTRIPS(2,jj)*100.0) + rem22
      ROW(JJ)=IFIX(temp)
      rem22=temp-row(jj)
      else
      row(jj)=0
      endif
      END DO
      CALL OUTAB(144,ROW,IZ,PURP,DUMMY,IO)
C
C...2-PERSON NON-TOLL HOV
      PURP=3
      rem23=0.0
      DO JJ=1,MAX_ZONES
      if(AVTRIPS(3,jj).gt.0.0) then
      temp=(AVTRIPS(3,jj)*100.0) + rem23
      ROW(JJ)=IFIX(temp)
      rem23=temp-row(jj)
      else
      row(jj)=0
      endif
      END DO
      CALL OUTAB(144,ROW,IZ,PURP,DUMMY,IO)
C
C...2-PERSON NON-TOLL NON-HOV
      PURP=4
      rem24=0.0
      DO JJ=1,MAX_ZONES
      if(AVTRIPS(4,jj).gt.0.0) then
      temp=(AVTRIPS(4,jj)*100.0) + rem24
      ROW(JJ)=IFIX(temp)
      rem24=temp-row(jj)
      else
      row(jj)=0
      endif
      END DO
      CALL OUTAB(144,ROW,IZ,PURP,DUMMY,IO)
C
C...2-PERSON TOLL HOV
      PURP=5
      rem25=0.0
      DO JJ=1,MAX_ZONES
      if(AVTRIPS(5,jj).gt.0.0) then
      temp=(AVTRIPS(5,jj)*100.0) + rem25
      ROW(JJ)=IFIX(temp)
      rem25=temp-row(jj)
      else
      row(jj)=0
      endif
      END DO
      CALL OUTAB(144,ROW,IZ,PURP,DUMMY,IO)
C
C...2-PERSON TOLL NON-HOV
      PURP=6
      rem26=0.0
      DO JJ=1,MAX_ZONES
      if(AVTRIPS(6,jj).gt.0.0) then
      temp=(AVTRIPS(6,jj)*100.0) + rem26
      ROW(JJ)=IFIX(temp)
      rem26=temp-row(jj)
      else
      row(jj)=0
      endif
      END DO
      CALL OUTAB(144,ROW,IZ,PURP,DUMMY,IO)
C
C...3-PERSON NON-TOLL HOV
      PURP=7
      rem27=0.0
      DO JJ=1,MAX_ZONES
      if(AVTRIPS(7,jj).gt.0.0) then
      temp=(AVTRIPS(7,jj)*100.0) + rem27
      ROW(JJ)=IFIX(temp)
      rem27=temp-row(jj)
      else
      row(jj)=0
      endif
      END DO
      CALL OUTAB(144,ROW,IZ,PURP,DUMMY,IO)
C
C...3-PERSON NON-TOLL NON-HOV
      PURP=8
      rem28=0.0
      DO JJ=1,MAX_ZONES
      if(AVTRIPS(8,jj).gt.0.0) then
      temp=(AVTRIPS(8,jj)*100.0) + rem28
      ROW(JJ)=IFIX(temp)
      rem28=temp-row(jj)
      else
      row(jj)=0
      endif
      END DO
      CALL OUTAB(144,ROW,IZ,PURP,DUMMY,IO)
C
C...3-PERSON TOLL HOV
      PURP=9
      rem29=0.0
      DO JJ=1,MAX_ZONES
      if(AVTRIPS(9,jj).gt.0.0) then
      temp=(AVTRIPS(9,jj)*100.0) + rem29
      ROW(JJ)=IFIX(temp)
      rem29=temp-row(jj)
      else
      row(jj)=0
      endif
      END DO
      CALL OUTAB(144,ROW,IZ,PURP,DUMMY,IO)
C
C...3-PERSON TOLL NON-HOV
      PURP=10
      rem20=0.0
      DO JJ=1,MAX_ZONES
      if(AVTRIPS(10,jj).gt.0.0) then
      temp=(AVTRIPS(10,jj)*100.0) + rem20
      ROW(JJ)=IFIX(temp)
      rem20=temp-row(jj)
      else
      row(jj)=0
      endif
      END DO
      CALL OUTAB(144,ROW,IZ,PURP,DUMMY,IO)
C
C...4-PERSON NON-TOLL HOV
      PURP=11
      rem35=0.0
      DO JJ=1,MAX_ZONES
      if(AVTRIPS(11,jj).gt.0.0) then
      temp=(AVTRIPS(11,jj)*100.0) + rem35
      ROW(JJ)=IFIX(temp)
      rem35=temp-row(jj)
      else
      row(jj)=0
      endif
      END DO
      CALL OUTAB(144,ROW,IZ,PURP,DUMMY,IO)
C
C...4-PERSON NON-TOLL NON-HOV
      PURP=12
      rem36=0.0
      DO JJ=1,MAX_ZONES
      if(AVTRIPS(12,jj).gt.0.0) then
      temp=(AVTRIPS(12,jj)*100.0) + rem36
      ROW(JJ)=IFIX(temp)
      rem36=temp-row(jj)
      else
      row(jj)=0
      endif
      END DO
      CALL OUTAB(144,ROW,IZ,PURP,DUMMY,IO)
C
C...4-PERSON TOLL HOV
      PURP=13
      rem37=0.0
      DO JJ=1,MAX_ZONES
      if(AVTRIPS(13,jj).gt.0.0) then
      temp=(AVTRIPS(13,jj)*100.0) + rem37
      ROW(JJ)=IFIX(temp)
      rem37=temp-row(jj)
      else
      row(jj)=0
      endif
      END DO
      CALL OUTAB(144,ROW,IZ,PURP,DUMMY,IO)
C
C...4-PERSON TOLL NON-HOV
      PURP=14
      rem38=0.0
      DO JJ=1,MAX_ZONES
      if(AVTRIPS(14,jj).gt.0.0) then
      temp=(AVTRIPS(14,jj)*100.0) + rem38
      ROW(JJ)=IFIX(temp)
      rem38=temp-row(jj)
      else
      row(jj)=0
      endif
      END DO
      CALL OUTAB(144,ROW,IZ,PURP,DUMMY,IO)
C
C...UBER
      PURP=15
      rem38=0.0
      DO JJ=1,MAX_ZONES
      if(AVTRIPS(15,jj).gt.0.0.and.(ubermode).and.(avmdl)) then
      temp=(AVTRIPS(15,jj)*100.0) + rem38
      ROW(JJ)=IFIX(temp)
      rem38=temp-row(jj)
      else
      row(jj)=0
      endif
      END DO
      CALL OUTAB(144,ROW,IZ,PURP,DUMMY,IO)
      END IF
C
C OUTPUT LOGSUM VALUES
C
      IF(LOGSUM) THEN
C..URBAN RAIL LOGSUM
      DO 3511 C=1,10
      rem30=0.0
      DO 3510 JJ=1,MAX_ZONES
      temp=ALOGSUM(C,JJ)*100.0+rem30
      ROW(JJ)=IFIX(temp)
      if(c.gt.5) then
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
      DO 3512 C=1,10
      rem31=0.0
      DO 3513 JJ=1,MAX_ZONES
      temp=TLOGSUM(C,JJ)*100.0+rem31
      ROW(JJ)=IFIX(temp)
      if(c.gt.5) then
       if(TLOGSUM(c,jj).gt.0.0) then
       rem31=temp-row(jj)
       else
       row(jj)=0
       end if
      end if
 3513 CONTINUE
      PURP=C+10
      CALL OUTAB(28,ROW,IZ,PURP,DUMMY,IO)
 3512 CONTINUE
C..TOTAL LOGSUM
      DO 3514 C=1,10
      rem32=0.0
      DO 3515 JJ=1,MAX_ZONES
      temp=ULOGSUM(C,JJ)*100.0+rem32
      ROW(JJ)=IFIX(temp)
      if(c.gt.5) then
       if(ULOGSUM(c,jj).gt.0.0) then
       rem32=temp-row(jj)
       else
       row(jj)=0
       end if
      end if
 3515 CONTINUE
      PURP=C+20
      CALL OUTAB(28,ROW,IZ,PURP,DUMMY,IO)
 3514 CONTINUE
      END IF
C ----------------------------------------------------------------  
C OUTPUT TRANSIT TRIP TABLES
C ZONE TO ZONE BASIS
C ----------------------------------------------------------------
C...WALK TO LOCAL BUS
      PURP=1
      rem1=0.0
      DO 3311,JJ=1,MAX_ZONES
      if(trips(11,jj).gt.0.0) then
      temp=(trips(11,jj)*100.0)+ rem1
      ROW(JJ)=IFIX(temp)
      rem1=temp-row(jj)
      else
      row(jj)=0
      end if
 3311 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...DRIVE TO LOCAL BUS
      PURP=2
      rem2=0.0
      DO 3312,JJ=1,MAX_ZONES
      if(trips(12,jj).gt.0.0) then
      temp=(trips(12,jj)*100.0)+ rem2
      ROW(JJ)=IFIX(temp)
      rem2=temp-row(jj)
      else
      row(jj)=0
      end if
 3312 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...WALK TO EXPRESS BUS
      PURP=3
      rem3=0.0
      DO 3313,JJ=1,MAX_ZONES
      if(trips(13,jj).gt.0.0) then
      temp=(trips(13,jj)*100.)+ rem3
      ROW(JJ)=IFIX(temp)
      rem3=temp-row(jj)
      else
      row(jj)=0
      end if
 3313 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...DRIVE TO EXPRESS BUS
      PURP=4
      rem4=0.0
      DO 3314,JJ=1,MAX_ZONES
      if(trips(14,jj).gt.0.0) then
      temp=(trips(14,jj)*100.0)+ rem4
      ROW(JJ)=IFIX(temp)
      rem4=temp-row(jj)
      else
      row(jj)=0
      end if
 3314 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...WALK TO COMMUTER RAIL
      PURP=5
      rem5=0.0
      DO 3315,JJ=1,MAX_ZONES
      if((trips(15,jj)+trips(16,jj)).gt.0.0) then
      temp=((trips(15,jj)+ trips(16,jj))*100.0) + rem5
      ROW(JJ)=IFIX(temp)
      rem5=temp-row(jj)
      else
      row(jj)=0
      end if
 3315 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...BUS TO COMMUTER RAIL
      PURP=6
      rem6=0.0
      DO 3316,JJ=1,MAX_ZONES
      if((trips(17,jj)+trips(18,jj)).gt.0.0) then
      temp=((trips(17,jj)+ trips(18,jj))*100.0) + rem6
      ROW(JJ)=IFIX(temp)
      rem6=temp-row(jj)
      else
      row(jj)=0
      end if
 3316 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...PARK-N-RIDE TO COMMUTER RAIL
      PURP=7
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
 3325 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...KISS-N-RIDE TO COMMUTER RAIL
      PURP=8
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
 3326 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...WALK TO URBAN RAIL
      PURP=9
      rem9=0.0
      DO 3327,JJ=1,MAX_ZONES
      if((trips(27,jj)+trips(28,jj)).gt.0.0) then
      temp=((trips(27,jj)+ trips(28,jj))*100.0) + rem9
      ROW(JJ)=IFIX(temp)
      rem9=temp-row(jj)
      else
      row(jj)=0
      end if
 3327 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...BUS TO URBAN RAIL
      PURP=10
      rem10=0.0
      DO 3320,JJ=1,MAX_ZONES
      if((trips(29,jj)+trips(30,jj)).gt.0.0) then
      temp=((trips(29,jj)+ trips(30,jj))*100.0) + rem10
      ROW(JJ)=IFIX(temp)
      rem10=temp-row(jj)
      else
      row(jj)=0
      end if
 3320 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...PARK-N-RIDE TO URBAN RAIL
      PURP=11
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
 3321 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...KISS-N-RIDE TO URBAN RAIL
      PURP=12
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
 3322 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...WALK TO TRANSITWAY
      PURP=13
      rem13=0.0
      DO 3323,JJ=1,MAX_ZONES
      if(trips(39,jj).gt.0.0) then
      temp=(trips(39,jj)*100.0) + rem13
      ROW(JJ)=IFIX(temp)
      rem13=temp-row(jj)
      else
      row(jj)=0
      end if
 3323 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...DRIVE TO TRANSITWAY 
      PURP=14
      rem14=0.0
      DO 3324,JJ=1,MAX_ZONES
      if(trips(40,jj).gt.0.0) then
      temp=(trips(40,jj)*100.0) + rem14
      ROW(JJ)=IFIX(temp)
      rem14=temp-row(jj)
      else
      row(jj)=0
      end if
 3324 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...WALK TO RAPID BUS
      PURP=15
      rem15=0.0
      DO 3333,JJ=1,MAX_ZONES
      if(trips(44,jj).gt.0.0) then
      temp=(trips(44,jj)*100.0) + rem15
      ROW(JJ)=IFIX(temp)
      rem15=temp-row(jj)
      else
      row(jj)=0
      end if
 3333 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...DRIVE TO RAPID BUS 
      PURP=16
      rem16=0.0
      DO 3334,JJ=1,MAX_ZONES
      if(trips(45,jj).gt.0.0) then
      temp=(trips(45,jj)*100.0) + rem16
      ROW(JJ)=IFIX(temp)
      rem16=temp-row(jj)
      else
      row(jj)=0
      end if
 3334 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...WALK TO BUS RAPID TRANSIT
      PURP=17
      rem17=0.0
      DO 3335,JJ=1,MAX_ZONES
      if((trips(46,jj)+trips(47,jj)).gt.0.0) then
      temp=((trips(46,jj)+trips(47,jj))*100.0) + rem17
      ROW(JJ)=IFIX(temp)
      rem17=temp-row(jj)
      else
      row(jj)=0
      end if
 3335 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...BUS TO BUS RAPID TRANSIT
      PURP=18
      rem18=0.0
      DO 3336,JJ=1,MAX_ZONES
      if((trips(48,jj)+trips(57,jj)).gt.0.0) then
      temp=((trips(48,jj)+trips(57,jj))*100.0) + rem18
      ROW(JJ)=IFIX(temp)
      rem18=temp-row(jj)
      else
      row(jj)=0
      end if
 3336 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...PNR TO BUS RAPID TRANSIT
      PURP=19
      tnew=0.0
      itot=0
      rem19=0.0
      DO 3337,JJ=1,MAX_ZONES
      if((trips(62,jj)+trips(63,jj)+trips(64,jj)+trips(65,jj)).gt.0.0)
     *    then
      temp=((trips(62,jj)+trips(63,jj)+trips(64,jj)+trips(65,jj))
     *      *100.0) + rem19
      ROW(JJ)=IFIX(temp)
      rem19=temp-row(jj)
      else
      row(jj)=0
      end if
 3337 CONTINUE
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...BIKE TO LOCAL BUS
      PURP=20
      rem60=0.0
      DO JJ=1,MAX_ZONES
      if(trips(53,jj).gt.0.0) then
      temp=(trips(53,jj)*100.0) + rem60
      ROW(JJ)=IFIX(temp)
      rem60=temp-row(jj)
      else
      row(jj)=0
      end if
      end do
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...BIKE TO RAPID BUS
      PURP=21
      rem61=0.0
      DO JJ=1,MAX_ZONES
      if(trips(54,jj).gt.0.0) then
      temp=(trips(54,jj)*100.0) + rem61
      ROW(JJ)=IFIX(temp)
      rem61=temp-row(jj)
      else
      row(jj)=0
      end if
      end do
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...BIKE TO EXPRESS BUS
      PURP=22
      rem62=0.0
      DO JJ=1,MAX_ZONES
      if(trips(55,jj).gt.0.0) then
      temp=(trips(55,jj)*100.0) + rem62
      ROW(JJ)=IFIX(temp)
      rem62=temp-row(jj)
      else
      row(jj)=0
      end if
      end do
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...BIKE TO TRANSITWAY BUS
      PURP=23
      rem63=0.0
      DO JJ=1,MAX_ZONES
      if(trips(56,jj).gt.0.0) then
      temp=(trips(56,jj)*100.0) + rem63
      ROW(JJ)=IFIX(temp)
      rem63=temp-row(jj)
      else
      row(jj)=0
      end if
      end do
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...BIKE TO BUS RAPID TRANSIT        
      PURP=24
      rem64=0.0
      DO JJ=1,MAX_ZONES
      if(trips(70,jj).gt.0.0) then
      temp=(trips(70,jj)*100.0) + rem64
      ROW(JJ)=IFIX(temp)
      rem64=temp-row(jj)
      else
      row(jj)=0
      end if
      end do
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...WALK ONLY
      PURP=25
      rem65=0.0
      DO JJ=1,MAX_ZONES
      if(trips(58,jj).gt.0.0) then
      temp=(trips(58,jj)*100.0) + rem65
      ROW(JJ)=IFIX(temp)
      rem65=temp-row(jj)
      else
      row(jj)=0
      end if
      end do
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...BICYCLE ONLY
      PURP=26
      rem66=0.0
      DO JJ=1,MAX_ZONES
      if(trips(59,jj).gt.0.0) then
      temp=(trips(59,jj)*100.0) + rem66
      ROW(JJ)=IFIX(temp)
      rem66=temp-row(jj)
      else
      row(jj)=0
      end if
      end do
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...BICYCLE TO COMMUTER RAIL
      PURP=27
      rem67=0.0
      DO JJ=1,MAX_ZONES
      if(trips(60,jj).gt.0.0) then
      temp=(trips(60,jj)*100.0) + rem67
      ROW(JJ)=IFIX(temp)
      rem67=temp-row(jj)
      else
      row(jj)=0
      end if
      end do
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...BICYCYLE TO URBAN RAIL
      PURP=28
      rem68=0.0
      DO JJ=1,MAX_ZONES
      if(trips(61,jj).gt.0.0) then
      temp=(trips(61,jj)*100.0) + rem68
      ROW(JJ)=IFIX(temp)
      rem68=temp-row(jj)
      else
      row(jj)=0
      end if
      end do
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...KNR TO BUS RAPID TRANSIT
      PURP=29
      rem69=0.0
      DO JJ=1,MAX_ZONES
      if((trips(66,jj)+trips(67,jj)+trips(68,jj)+trips(69,jj)).gt.0.0)
     *    then
      temp=((trips(66,jj)+trips(67,jj)+trips(68,jj)+trips(69,jj))
     *      *100.0) + rem69
      ROW(JJ)=IFIX(temp)
      rem69=temp-row(jj)
      else
      row(jj)=0
      end if
      END DO
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...BUS RAPID TRANSIT TO COMMUTER RAIL
      PURP=30
      rem69=0.0
      DO JJ=1,MAX_ZONES
      if((trips(71,jj)).gt.0.0)
     *    then
      temp=(trips(71,jj)*100.0) + rem69
      ROW(JJ)=IFIX(temp)
      rem69=temp-float(row(jj))
      else
      row(jj)=0
      end if
      END DO
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...COMMUTER RAIL TO BUS RAPID TRANSIT
      PURP=31
      rem69=0.0
      DO JJ=1,MAX_ZONES
      if((trips(72,jj)).gt.0.0)
     *    then
      temp=(trips(72,jj)*100.0) + rem69
      ROW(JJ)=IFIX(temp)
      rem69=temp-float(row(jj))
      else
      row(jj)=0
      end if
      END DO
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...BUS RAPID TRANSIT TO URBAN RAIL
      PURP=32
      rem69=0.0
      DO JJ=1,MAX_ZONES
      if((trips(73,jj)).gt.0.0)
     *    then
      temp=(trips(73,jj)*100.0) + rem69
      ROW(JJ)=IFIX(temp)
      rem69=temp-float(row(jj))
      else
      row(jj)=0
      end if
      END DO
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...URBAN RAIL TO BUS RAPID TRANSIT
      PURP=33
      rem69=0.0
      DO JJ=1,MAX_ZONES
      if((trips(74,jj)).gt.0.0)
     *    then
      temp=(trips(74,jj)*100.0) + rem69
      ROW(JJ)=IFIX(temp)
      rem69=temp-float(row(jj))
      else
      row(jj)=0
      end if
      END DO
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...UBER TO COMMUTER RAIL
      PURP=34
      rem69=0.0
      DO JJ=1,MAX_ZONES
      if((trips(75,jj)+trips(76,jj)+trips(77,jj)+trips(78,jj)).gt.0.0
     *    .and.(ubertrn)) then
      temp=((trips(75,jj)+trips(76,jj)+
     *       trips(77,jj)+trips(78,jj))*100.0) + rem69
      ROW(JJ)=IFIX(temp)
      rem69=temp-float(row(jj))
      else
      row(jj)=0
      end if
      END DO
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...UBER TO URBAN RAIL
      PURP=35
      rem69=0.0
      DO JJ=1,MAX_ZONES
      if((trips(79,jj)+trips(80,jj)+trips(81,jj)+trips(82,jj)).gt.0.0
     *    .and.(ubertrn)) then
      temp=((trips(79,jj)+trips(80,jj)+
     *       trips(81,jj)+trips(82,jj))*100.0) + rem69
      ROW(JJ)=IFIX(temp)
      rem69=temp-float(row(jj))
      else
      row(jj)=0
      end if
      END DO
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...UBER TO BRT
      PURP=36
      rem69=0.0
      DO JJ=1,MAX_ZONES
      if((trips(83,jj)+trips(84,jj)+trips(85,jj)+trips(86,jj)).gt.0.0
     *    .and.(ubertrn)) then
      temp=((trips(83,jj)+trips(84,jj)+
     *       trips(85,jj)+trips(86,jj))*100.0) + rem69
      ROW(JJ)=IFIX(temp)
      rem69=temp-float(row(jj))
      else
      row(jj)=0
      end if
      END DO
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C...E-SCOOTER
      PURP=37
      rem69=0.0
      DO JJ=1,MAX_ZONES
      if(trips(89,jj).gt.0.0.and.(escooter))  then
      temp=(trips(89,jj)*100.0) + rem69
      ROW(JJ)=IFIX(temp)
      rem69=temp-float(row(jj))
      else
      row(jj)=0
      end if
      END DO
      CALL OUTAB(24,ROW,IZ,PURP,DUMMY,IO)
C
C  OUTPUT FLY-AWAY TRIPS BY MODE
C
      IF(LAX.AND.(.NOT.LAXTRN).AND.TRIPSOUT) THEN
      DO T=1,13
      PURP=T
      ROW=0
      DO NI=1,10
      JJ=IDINT(FLYADATA(NI,1))
      IF(SAVFLY(NI,T).LE.0) CYCLE
      TEMP=SAVFLY(NI,T)*1000.0
      ROW(JJ)=IFIX(TEMP)
      END DO
      CALL OUTAB(95,ROW,IZ,PURP,DUMMY,IO)
      END DO
      SAVFLY=0.0
      END IF
   99 CONTINUE
C
C  END OF THE I-ZONE LOOP
C  
  100 CONTINUE
C
C  OUTPUT REST OF TRIP TABLES
C
C
      IF(CALIB.AND.(CITER.LT.MAXCALIT)) GOTO 9200
      IF(CAPRES.AND.(ITER.LT.(NITER-1))) GO TO 9200
      IF(.NOT.TRIPSOUT.OR.(LAXTRN).OR.(EVENTSP)) GO TO 9200
      IF(AIRPASS.AND.(.NOT.RECYC)) GO TO 9200
      IF(HCALIB) GO TO 9200
      IF(LOWRAIL) THEN
C
C   LOW INCOME STATION-TO-STATION MATRICES
C
C...URBAN RAIL
      DO II=1,MAX_ZONES
      ROW=0
	    PURP=1
      REM1=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      if(URSSL(IN,IJ).gt.0.0) then
	    TEMP=(URSSL(IN,IJ)*100.0)+REM1
	    ROW(JJ)=IFIX(TEMP)
	    REM1=TEMP-ROW(JJ)
      ELSE
      ROW(JJ)=0
      END IF
      END DO 
      ENDIF 
      CALL OUTAB(128,ROW,II,PURP,DUMMY,IO)
C..BLENDED PATH
      ROW=0
	    PURP=2
      REM1=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      if(CRURBRTSSL(IN,IJ).gt.0.0) then
	    TEMP=(CRURBRTSSL(IN,IJ)*100.0)+REM1
	    ROW(JJ)=IFIX(TEMP)
	    REM1=TEMP-ROW(JJ)
      ELSE
      ROW(JJ)=0
      END IF
      END DO 
      ENDIF 
      CALL OUTAB(128,ROW,II,PURP,DUMMY,IO)
C..COMMUTER RAIL
      ROW=0
	    PURP=3
      REM1=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      if(CRSSL(IN,IJ).gt.0.0) then
	    TEMP=(CRSSL(IN,IJ)*100.0)+REM1
	    ROW(JJ)=IFIX(TEMP)
	    REM1=TEMP-ROW(JJ)
      ELSE
      ROW(JJ)=0
      END IF
      END DO 
      ENDIF 
      CALL OUTAB(128,ROW,II,PURP,DUMMY,IO)
C..BRT
      ROW=0
	    PURP=4
      REM1=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      if(BRTSSL(IN,IJ).gt.0.0) then
	    TEMP=(BRTSSL(IN,IJ)*100.0)+REM1
	    ROW(JJ)=IFIX(TEMP)
	    REM1=TEMP-ROW(JJ)
      ELSE
      ROW(JJ)=0
      END IF
      END DO 
      ENDIF 
      CALL OUTAB(128,ROW,II,PURP,DUMMY,IO)
      END DO
      END IF
C
C...BUS TO COMMUTER RAIL ORIGIN ZONE TO ORIGIN STATION
C
      DO 3318,II=1,MAX_ZONES
	DO 3319,IT=1,MAX_ZONES
 3319   ROW(IT)=0
	PURP=1
        rem71=0.0
	IF(II.LE.MAX_IZONES) THEN
	DO 3317,IJ=1,MAX_STATIONS
	IF(STANUM(IJ).NE.1) GOTO 3317
	JJ=IJ+MAX_IZONES
        if(bcr(ii,ij).gt.0.0) then
	TEMP=(BCR(II,IJ)*100.0)+rem71
	ROW(JJ)=IFIX(TEMP)
	rem71=TEMP-row(jj)
        else
        row(jj)=0
        end if
 3317 CONTINUE
       ENDIF
       CALL OUTAB(22,ROW,II,PURP,DUMMY,IO)
C
C...COMMUTER RAIL STATION-TO-STATION
C
	DO 3410,IT=1,MAX_ZONES
 3410   ROW(IT)=0
	PURP=2
        rem72=0.0
	IF(II.GT.MAX_IZONES) THEN
	 IF(STANUM((II-MAX_IZONES)).EQ.1) THEN
	DO 3414,IJ=1,MAX_STATIONS
	IF(STANUM(IJ).NE.1) GOTO 3414
	JJ=IJ+MAX_IZONES
        IN=II-MAX_IZONES
        if(CRSS(iN,ij).gt.0.0) then
	TEMP=(CRSS(IN,IJ)*100.0)+rem72
	ROW(JJ)=IFIX(TEMP)
	rem72=TEMP-row(jj)
        else
        row(jj)=0
        end if
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
        rem73=0.0
        IF(II.GT.MAX_IZONES) THEN
	IF(STANUM((II-MAX_IZONES)).EQ.1) THEN
	DO 3424,IJ=1,MAX_IZONES
        IN=II-MAX_IZONES
        if(CRSTAZ(in,ij).gt.0.0) then
	TEMP=(CRSTAZ(In,IJ)*100.0)+rem73
	ROW(ij)=IFIX(TEMP)
	rem73=TEMP-row(ij)
        else
        row(ij)=0
        end if
        tnew=tnew+CRSTAZ(in,ij)
        itot=itot+row(ij)
 3424 CONTINUE
       ENDIF
       ENDIF
       CALL OUTAB(22,ROW,II,PURP,DUMMY,IO)
C
C...WALK TO COMMUTER RAIL ORIGIN ZONE TO ORIGIN STATION
C
	    DO IT=1,MAX_ZONES
      ROW(IT)=0
      END DO
	    PURP=4
	    rem74=0.0
	    IF(II.LE.MAX_IZONES) THEN
	    DO 4317 IJ=1,MAX_STATIONS
	    IF(STANUM(IJ).NE.1) GOTO 4317
	    JJ=IJ+MAX_IZONES
        if(wlkcr(ii,ij).gt.0.0) then
	    TEMP=(WLKCR(II,IJ)*100.0)+rem74
	    ROW(JJ)=IFIX(TEMP)
	    rem74=TEMP-row(jj)
        else
        row(jj)=0
        end if
 4317  CONTINUE
       ENDIF
       CALL OUTAB(22,ROW,II,PURP,DUMMY,IO)
C
C...BIKE TO COMMUTER RAIL ORIGIN ZONE TO ORIGIN STATION
C
	    DO IT=1,MAX_ZONES
      ROW(IT)=0
      END DO
	    PURP=5
	    rem75=0.0
	    IF(II.LE.MAX_IZONES) THEN
	    DO 4318 IJ=1,MAX_STATIONS
	    IF(STANUM(IJ).NE.1) GOTO 4318
	    JJ=IJ+MAX_IZONES
        if(bikcr(ii,ij).gt.0.0) then
	    TEMP=(BIKCR(II,IJ)*100.0)+rem75
	    ROW(JJ)=IFIX(TEMP)
	    rem75=TEMP-row(jj)
        else
        row(jj)=0
        end if
 4318  CONTINUE
       ENDIF
       CALL OUTAB(22,ROW,II,PURP,DUMMY,IO)
C
C...COMMUTER RAIL STATION-TO-STATION FOR URBAN RAIL EGRESS
C
      ROW=0
	    PURP=6
      rem78=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      if(CR2URSS(iN,ij).gt.0.0) then
	    TEMP=(CR2URSS(IN,IJ)*100.0)+rem78
	    ROW(JJ)=IFIX(TEMP)
	    rem78=TEMP-row(jj)
      else
      row(jj)=0
      end if
       end do
       ENDIF
       CALL OUTAB(22,ROW,II,PURP,DUMMY,IO)
C
C...COMMUTER RAIL STATION-TO-STATION FOR URBAN RAIL ACCESS
C
      ROW=0
	    PURP=7
      rem78=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      if(UR2CRSS(iN,ij).gt.0.0) then
	    TEMP=(UR2CRSS(IN,IJ)*100.0)+rem78
	    ROW(JJ)=IFIX(TEMP)
	    rem78=TEMP-row(jj)
      else
      row(jj)=0
      end if
       end do
       ENDIF
       CALL OUTAB(22,ROW,II,PURP,DUMMY,IO)
C
C...COMMUTER RAIL STATION-TO-STATION FOR BRT ACCESS
C
      ROW=0
	    PURP=8
      rem78=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      if(BRTCRSS(iN,ij).gt.0.0) then
	    TEMP=(BRTCRSS(IN,IJ)*100.0)+rem78
	    ROW(JJ)=IFIX(TEMP)
	    rem78=TEMP-row(jj)
      else
      row(jj)=0
      end if
       end do
       ENDIF
       CALL OUTAB(22,ROW,II,PURP,DUMMY,IO)
C
C...COMMUTER RAIL STATION-TO-STATION FOR BRT EGRESS
C
      ROW=0
	    PURP=9
      rem78=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      if(CRBRTSS(iN,ij).gt.0.0) then
	    TEMP=(CRBRTSS(IN,IJ)*100.0)+rem78
	    ROW(JJ)=IFIX(TEMP)
	    rem78=TEMP-row(jj)
      else
      row(jj)=0
      end if
       end do
       ENDIF
       CALL OUTAB(22,ROW,II,PURP,DUMMY,IO)
C
C...COMMUTER RAIL WALK EGRESS (STATION-TO-ZONE)
C
        row=0.0
	      PURP=10
        rem73=0.0
        IF(II.GT.MAX_IZONES) THEN
	IF(STANUM((II-MAX_IZONES)).EQ.1) THEN
	      DO IJ=1,MAX_IZONES
        IN=II-MAX_IZONES
        if(CRWLK(in,ij).gt.0.0) then
	TEMP=(CRWLK(In,IJ)*100.0)+rem73
	ROW(ij)=IFIX(TEMP)
	rem73=TEMP-row(ij)
        else
        row(ij)=0
        end if
       end do
       ENDIF
       ENDIF
       CALL OUTAB(22,ROW,II,PURP,DUMMY,IO)
C
C...COMMUTER RAIL UBER EGRESS (STATION-TO-ZONE)
C
        row=0.0
	      PURP=11
        rem73=0.0
        IF(II.GT.MAX_IZONES) THEN
	IF(STANUM((II-MAX_IZONES)).EQ.1) THEN
	      DO IJ=1,MAX_IZONES
        IN=II-MAX_IZONES
        if(CRTNC(in,ij).gt.0.0.and.(ubertrn)) then
	TEMP=(CRTNC(In,IJ)*100.0)+rem73
	ROW(ij)=IFIX(TEMP)
	rem73=TEMP-row(ij)
        else
        row(ij)=0
        end if
       end do
       ENDIF
       ENDIF
       CALL OUTAB(22,ROW,II,PURP,DUMMY,IO)
 3318 CONTINUE
C
C
C...BUS TO URBAN RAIL ORIGIN ZONE TO ORIGIN STATION
C
      DO 3518,II=1,MAX_ZONES
       DO 3519,IT=1,MAX_ZONES
 3519   ROW(IT)=0
	PURP=1
        tnew=0.0
        itot=0
        rem41=0.0
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
        rem42=0.0
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
        rem43=0.0
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
C
C...WALK TO URBAN RAIL ORIGIN ZONE TO ORIGIN STATION
C
	    DO IT=1,MAX_ZONES
      ROW(IT)=0
      END DO
	    PURP=4
	    rem76=0.0
	    IF(II.LE.MAX_IZONES) THEN
	    DO 4325 IJ=1,MAX_STATIONS
	    IF(STANUM(IJ).NE.2) GOTO 4325
	    JJ=IJ+MAX_IZONES
        if(wlkur(ii,ij).gt.0.0) then
	    TEMP=(WLKUR(II,IJ)*100.0)+rem76
	    ROW(JJ)=IFIX(TEMP)
	    rem76=TEMP-row(jj)
        else
        row(jj)=0
        end if
 4325  CONTINUE
       ENDIF
       CALL OUTAB(23,ROW,II,PURP,DUMMY,IO)
C
C...BIKE TO URBAN RAIL ORIGIN ZONE TO ORIGIN STATION
C
	    DO IT=1,MAX_ZONES
      ROW(IT)=0
      END DO
	    PURP=5
	    rem77=0.0
	    IF(II.LE.MAX_IZONES) THEN
	    DO 4326 IJ=1,MAX_STATIONS
	    IF(STANUM(IJ).NE.2) GOTO 4326
	    JJ=IJ+MAX_IZONES
        if(bikur(ii,ij).gt.0.0) then
	    TEMP=(BIKUR(II,IJ)*100.0)+rem77
	    ROW(JJ)=IFIX(TEMP)
	    rem77=TEMP-row(jj)
        else
        row(jj)=0
        end if
 4326  CONTINUE
       ENDIF
       CALL OUTAB(23,ROW,II,PURP,DUMMY,IO)
C
C...URBAN RAIL STATION-TO-STATION FOR COMMUTER RAIL EGRESS
C
      ROW=0
	    PURP=6
      rem78=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      if(CRURSS(iN,ij).gt.0.0) then
	    TEMP=(CRURSS(IN,IJ)*100.0)+rem78
	    ROW(JJ)=IFIX(TEMP)
	    rem78=TEMP-row(jj)
      else
      row(jj)=0
      end if
       end do
       ENDIF
       CALL OUTAB(23,ROW,II,PURP,DUMMY,IO)
C
C...URBAN RAIL STATION-TO-STATION FOR COMMUTER RAIL ACCESS
C
      ROW=0
	    PURP=7
      rem78=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      if(URCRSS(iN,ij).gt.0.0) then
	    TEMP=(URCRSS(IN,IJ)*100.0)+rem78
	    ROW(JJ)=IFIX(TEMP)
	    rem78=TEMP-row(jj)
      else
      row(jj)=0
      end if
       end do
       ENDIF
       CALL OUTAB(23,ROW,II,PURP,DUMMY,IO)
C
C...URBAN RAIL STATION-TO-STATION FOR BRT EGRESS
C
      ROW=0
	    PURP=8
      rem78=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      if(BRTURSS(iN,ij).gt.0.0) then
	    TEMP=(BRTURSS(IN,IJ)*100.0)+rem78
	    ROW(JJ)=IFIX(TEMP)
	    rem78=TEMP-row(jj)
      else
      row(jj)=0
      end if
       end do
       ENDIF
       CALL OUTAB(23,ROW,II,PURP,DUMMY,IO)
C
C...URBAN RAIL STATION-TO-STATION FOR BRT ACCESS
C
      ROW=0
	    PURP=9
      rem78=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      if(URBRTSS(iN,ij).gt.0.0) then
	    TEMP=(URBRTSS(IN,IJ)*100.0)+rem78
	    ROW(JJ)=IFIX(TEMP)
	    rem78=TEMP-row(jj)
      else
      row(jj)=0
      end if
       end do
       ENDIF
       CALL OUTAB(23,ROW,II,PURP,DUMMY,IO)
C
C...URBAN RAIL WALK EGRESS (STATION-TO-ZONE)
C
	      ROW=0
	      PURP=10
        rem43=0.0
        IF(II.GT.MAX_IZONES) THEN
	IF(STANUM((II-MAX_IZONES)).EQ.2) THEN
	      DO IJ=1,MAX_IZONES
        IN=II-MAX_IZONES
        if(URWLK(in,ij).gt.0.0) then
	TEMP=(URWLK(In,IJ)*100.0)+rem43
	ROW(ij)=IFIX(TEMP)
	rem43=TEMP-row(ij)
        else
        row(ij)=0
        end if
       END DO
       ENDIF
       ENDIF
       CALL OUTAB(23,ROW,II,PURP,DUMMY,IO)
C
C...URBAN RAIL UBER EGRESS (STATION-TO-ZONE)
C
	      ROW=0
	      PURP=11
        rem43=0.0
        IF(II.GT.MAX_IZONES) THEN
	IF(STANUM((II-MAX_IZONES)).EQ.2) THEN
	      DO IJ=1,MAX_IZONES
        IN=II-MAX_IZONES
        if(URTNC(in,ij).gt.0.0.and.(ubertrn)) then
	TEMP=(URTNC(In,IJ)*100.0)+rem43
	ROW(ij)=IFIX(TEMP)
	rem43=TEMP-row(ij)
        else
        row(ij)=0
        end if
       END DO
       ENDIF
       ENDIF
       CALL OUTAB(23,ROW,II,PURP,DUMMY,IO)
 3518  CONTINUE
C
C
C...BUS TO BRT ORIGIN ZONE TO ORIGIN STATION
C
      DO 7518,II=1,MAX_ZONES
      ROW=0
	    PURP=1
      rem1261=0.0
	    IF(II.LE.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    IF(STANUM(IJ).NE.5) CYCLE
	    JJ=IJ+MAX_IZONES
      if(BBRT(ii,ij).gt.0.0) then
	    TEMP=(BBRT(II,IJ)*100.0)+rem1261
	    ROW(JJ)=IFIX(TEMP)
	    rem1261=TEMP-row(jj)
      else
      row(jj)=0
      end if
      END DO
      ENDIF
      CALL OUTAB(126,ROW,II,PURP,DUMMY,IO)
C
C...BRT STATION-TO-STATION
C
      ROW=0
	    PURP=2
      rem1262=0.0
      IF(II.GT.MAX_IZONES) THEN
	    IF(STANUM((II-MAX_IZONES)).EQ.5) THEN
	    DO IJ=1,MAX_STATIONS
	    IF(STANUM(IJ).NE.5) CYCLE
	    JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      if(BRTSS(iN,ij).gt.0.0) then
	    TEMP=(BRTSS(IN,IJ)*100.0)+rem1262
	    ROW(JJ)=IFIX(TEMP)
	    rem1262=TEMP-row(jj)
      else
      row(jj)=0
      end if
       END DO
       ENDIF
       ENDIF
       CALL OUTAB(126,ROW,II,PURP,DUMMY,IO)
c
C...BRT STATION TO DESTINATION ZONE
C
      ROW=0
	    PURP=3
      rem1263=0.0
      IF(II.GT.MAX_IZONES) THEN
	    IF(STANUM((II-MAX_IZONES)).EQ.5) THEN
	    DO IJ=1,MAX_IZONES
      IN=II-MAX_IZONES
       if(BRTSTAZ(in,ij).gt.0.0) then
	     TEMP=(BRTSTAZ(In,IJ)*100.0)+rem1263
	     ROW(ij)=IFIX(TEMP)
	     rem1263=TEMP-row(ij)
       else
       row(ij)=0
       end if
       END DO
       ENDIF
       ENDIF
       CALL OUTAB(126,ROW,II,PURP,DUMMY,IO)
C
C...WALK TO BRT ORIGIN ZONE TO ORIGIN STATION
C
      ROW=0
	    PURP=4
	    rem1266=0.0
	    IF(II.LE.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    IF(STANUM(IJ).NE.5) CYCLE
	    JJ=IJ+MAX_IZONES
      if(WLKBRT(ii,ij).gt.0.0) then
	    TEMP=(WLKBRT(II,IJ)*100.0)+rem1266
	    ROW(JJ)=IFIX(TEMP)
	    rem1266=TEMP-row(jj)
      else
      row(jj)=0
      end if
      END DO
      ENDIF
      CALL OUTAB(126,ROW,II,PURP,DUMMY,IO)
C
C...BIKE TO BRT ORIGIN ZONE TO ORIGIN STATION
C
      ROW=0
	    PURP=5
	    rem1267=0.0
	    IF(II.LE.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    IF(STANUM(IJ).NE.5) CYCLE
	    JJ=IJ+MAX_IZONES
      if(BIKBRT(ii,ij).gt.0.0) then
	    TEMP=(BIKBRT(II,IJ)*100.0)+rem1267
	    ROW(JJ)=IFIX(TEMP)
	     rem1267=TEMP-row(jj)
       else
       row(jj)=0
       end if
       END DO
       ENDIF
       CALL OUTAB(126,ROW,II,PURP,DUMMY,IO)
C
C...BRT STATION-TO-STATION FOR COMMUTER RAIL EGRESS
C
      ROW=0
	    PURP=6
      rem78=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      if(CR2BRTSS(iN,ij).gt.0.0) then
	    TEMP=(CR2BRTSS(IN,IJ)*100.0)+rem78
	    ROW(JJ)=IFIX(TEMP)
	    rem78=TEMP-row(jj)
      else
      row(jj)=0
      end if
       end do
       ENDIF
       CALL OUTAB(126,ROW,II,PURP,DUMMY,IO)
C
C...BRT STATION-TO-STATION FOR COMMUTER RAIL ACCESS
C
      ROW=0
	    PURP=7
      rem78=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      if(BRT2CRSS(iN,ij).gt.0.0) then
	    TEMP=(BRT2CRSS(IN,IJ)*100.0)+rem78
	    ROW(JJ)=IFIX(TEMP)
	    rem78=TEMP-row(jj)
      else
      row(jj)=0
      end if
       end do
       ENDIF
       CALL OUTAB(126,ROW,II,PURP,DUMMY,IO)
C
C...BRT STATION-TO-STATION FOR URBAN RAIL EGRESS
C
      ROW=0
	    PURP=8
      rem78=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      if(BRT2URSS(iN,ij).gt.0.0) then
	    TEMP=(BRT2URSS(IN,IJ)*100.0)+rem78
	    ROW(JJ)=IFIX(TEMP)
	    rem78=TEMP-row(jj)
      else
      row(jj)=0
      end if
       end do
       ENDIF
       CALL OUTAB(126,ROW,II,PURP,DUMMY,IO)
C
C...BRT STATION-TO-STATION FOR URBAN RAIL ACCESS
C
      ROW=0
	    PURP=9
      rem78=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      if(UR2BRTSS(iN,ij).gt.0.0) then
	    TEMP=(UR2BRTSS(IN,IJ)*100.0)+rem78
	    ROW(JJ)=IFIX(TEMP)
	    rem78=TEMP-row(jj)
      else
      row(jj)=0
      end if
       end do
       ENDIF
       CALL OUTAB(126,ROW,II,PURP,DUMMY,IO)
C
C...BRT WALK EGRESS (STATION-TO-ZONE)
C
      ROW=0
	    PURP=10
      rem1263=0.0
      IF(II.GT.MAX_IZONES) THEN
	    IF(STANUM((II-MAX_IZONES)).EQ.5) THEN
	    DO IJ=1,MAX_IZONES
      IN=II-MAX_IZONES
       if(BRTWLK(in,ij).gt.0.0) then
	     TEMP=(BRTWLK(In,IJ)*100.0)+rem1263
	     ROW(ij)=IFIX(TEMP)
	     rem1263=TEMP-row(ij)
       else
       row(ij)=0
       end if
       END DO
       ENDIF
       ENDIF
       CALL OUTAB(126,ROW,II,PURP,DUMMY,IO)
C
C...BRT UBER EGRESS (STATION-TO-ZONE)
C
      ROW=0
	    PURP=11
      rem1263=0.0
      IF(II.GT.MAX_IZONES) THEN
	    IF(STANUM((II-MAX_IZONES)).EQ.5) THEN
	    DO IJ=1,MAX_IZONES
      IN=II-MAX_IZONES
       if(BRTTNC(in,ij).gt.0.0.and.(ubertrn)) then
	     TEMP=(BRTTNC(In,IJ)*100.0)+rem1263
	     ROW(ij)=IFIX(TEMP)
	     rem1263=TEMP-row(ij)
       else
       row(ij)=0
       end if
       END DO
       ENDIF
       ENDIF
       CALL OUTAB(126,ROW,II,PURP,DUMMY,IO)
 7518  CONTINUE
C
C...BLENDED STATION-TO-STATION
C
      DO 7519,II=1,MAX_ZONES
      ROW=0
	    PURP=1
      rem1272=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_STATIONS
	    JJ=IJ+MAX_IZONES
      IN=II-MAX_IZONES
      if(CRURBRTSS(IN,IJ).gt.0.0) then
	    TEMP=(CRURBRTSS(IN,IJ)*100.0)+rem1272
	    ROW(JJ)=IFIX(TEMP)
	    rem1272=TEMP-row(jj)
      else
      row(jj)=0
      end if
       END DO
       ENDIF
       CALL OUTAB(127,ROW,II,PURP,DUMMY,IO)
C
C...BLENDED STATION TO DESTINATION ZONE
C
      ROW=0
	    PURP=2
      rem1273=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_IZONES
      IN=II-MAX_IZONES
       if(ALLSTAZ(in,ij).gt.0.0) then
	     TEMP=(ALLSTAZ(In,IJ)*100.0)+rem1273
	     ROW(ij)=IFIX(TEMP)
	     rem1273=TEMP-row(ij)
       else
       row(ij)=0
       end if
       END DO
       ENDIF
       CALL OUTAB(127,ROW,II,PURP,DUMMY,IO)
C
C...BLENDED STATION WALK EGRESS (STATION-TO-ZONE)
C
      ROW=0
	    PURP=3
      rem1273=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_IZONES
      IN=II-MAX_IZONES
       if(ALLWLK(in,ij).gt.0.0) then
	     TEMP=(ALLWLK(In,IJ)*100.0)+rem1273
	     ROW(ij)=IFIX(TEMP)
	     rem1273=TEMP-row(ij)
       else
       row(ij)=0
       end if
       END DO
       ENDIF
       CALL OUTAB(127,ROW,II,PURP,DUMMY,IO)
C
C...BLENDED STATION UBER EGRESS (STATION-TO-ZONE)
C
      ROW=0
	    PURP=4
      rem1273=0.0
      IF(II.GT.MAX_IZONES) THEN
	    DO IJ=1,MAX_IZONES
      IN=II-MAX_IZONES
       if(ALLTNC(in,ij).gt.0.0.and.(ubertrn)) then
	     TEMP=(ALLTNC(In,IJ)*100.0)+rem1273
	     ROW(ij)=IFIX(TEMP)
	     rem1273=TEMP-row(ij)
       else
       row(ij)=0
       end if
       END DO
       ENDIF
       CALL OUTAB(127,ROW,II,PURP,DUMMY,IO)
 7519  CONTINUE
C
C
C...EXPRESS BUS STATION TO DESTINATION ZONE
      DO 3722,II=1,MAX_ZONES 
	DO 3720,IT=1,MAX_ZONES
 3720   ROW(IT)=0
	PURP=1
        tnew=0.0
        itot=0
        rem51=0.0
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
        rem52=0.0
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
c
c
C...    ALL DRIVE TO TRANSIT
C
	DO 3819,IT=1,MAX_ZONES
 3819   ROW(IT)=0
	PURP=3
        tnew=0.0
        itot=0
        rem53=0.0
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
C
C OUTPUT LAX PARKING LOT TRANSIT TRIPS
C
      IF(LAX.AND.(.NOT.LAXTRN).AND.TRIPSOUT) THEN
      DO 3920,II=1,MAX_ZONES
      DO 3921,PURP=1,16
       DO IJ=1,MAX_ZONES
       ROW(IJ)=0
       END DO
      IF(PURP.LE.10) THEN
      DO 3922,NI=1,50
      IF(PEQUIV(NI).EQ.II) THEN
       DO 3923,IJ=1,MAX_ZONES
        IF(PURP.LE.7) THEN
        AJZ=0
        DO 3924,IT=1,50
        IF(AEQUIV(IT).EQ.IJ) AJZ=IT
 3924   CONTINUE
        IF(AJZ.GT.0) ROW(IJ)=IFIX(LOTTRN(NI,AJZ,PURP)*100.0)
        ELSE
        IF(IJ.LE.MAX_IZONES) GO TO 3923
        IT=IJ-MAX_IZONES
        IF(PURP.EQ.8) ROW(IJ)=IFIX(WCR(NI,IT)*100.0)
        IF(PURP.EQ.9) ROW(IJ)=IFIX(WUR(NI,IT)*100.0)
        IF(PURP.EQ.10) ROW(IJ)=IFIX(WBRT(NI,IT)*100.0)
        END IF
 3923  CONTINUE
      END IF
 3922 CONTINUE
      CALL OUTAB(79,ROW,II,PURP,DUMMY,IO)
      END IF
C....
      IF(PURP.EQ.11.OR.PURP.EQ.12.OR.PURP.EQ.13) THEN
      IF(II.GT.MAX_IZONES) THEN
      DO 3925,IJ=1,MAX_ZONES
        AJZ=0
        DO 3926,IT=1,50
        IF(AEQUIV(IT).EQ.IJ) AJZ=IT
 3926   CONTINUE
        IT=II-MAX_IZONES
        IF(PURP.EQ.11.AND.AJZ.GT.0) ROW(IJ)=IFIX(LCRSTAZ(IT,AJZ)*100.0)
        IF(PURP.EQ.12.AND.AJZ.GT.0) ROW(IJ)=IFIX(LURSTAZ(IT,AJZ)*100.0)
        IF(PURP.EQ.13.AND.AJZ.GT.0) ROW(IJ)=IFIX(LBRTSTAZ(IT,AJZ)*100.0)
 3925 CONTINUE
      END IF
      CALL OUTAB(79,ROW,II,PURP,DUMMY,IO)
      END IF
C....
      IF(PURP.EQ.14.OR.PURP.EQ.15.OR.PURP.EQ.16) THEN
      IF(II.GT.MAX_IZONES) THEN
      DO 3927,IJ=1,MAX_ZONES
      IT=II-MAX_IZONES
      IQ=IJ-MAX_IZONES
      IF(IJ.LE.MAX_IZONES) GO TO 3927
      IF(PURP.EQ.14) ROW(IJ)=IFIX(LCRSS(IT,IQ)*100.0)
      IF(PURP.EQ.15) ROW(IJ)=IFIX(LURSS(IT,IQ)*100.0)
      IF(PURP.EQ.16) ROW(IJ)=IFIX(LBRTSS(IT,IQ)*100.0)
 3927 CONTINUE
      END IF
      CALL OUTAB(79,ROW,II,PURP,DUMMY,IO)
      END IF
 3921 CONTINUE
 3920 CONTINUE
C
C     OUTPUT RENTAL CAR FACILITY TRANSIT TRIPS
C
      DO 3930,II=1,MAX_ZONES
      DO 3931,PURP=1,16
       DO IJ=1,MAX_ZONES
       ROW(IJ)=0
       END DO
      IF(PURP.LE.10) THEN
      DO 3932,NI=1,10
      IF(IDINT(RNTLDATA(NI,1)).EQ.II) THEN
       DO 3933,IJ=1,MAX_ZONES
        IF(PURP.LE.7) THEN
        AJZ=0
        DO 3934,IT=1,50
        IF(AEQUIV(IT).EQ.IJ) AJZ=IT
 3934   CONTINUE
        IF(AJZ.GT.0) ROW(IJ)=IFIX(RNTTRN(NI,AJZ,PURP)*100.0)
        ELSE
        IF(IJ.LE.MAX_IZONES) GO TO 3933
        IT=IJ-MAX_IZONES
        IF(PURP.EQ.8) ROW(IJ)=IFIX(WCRR(NI,IT)*100.0)
        IF(PURP.EQ.9) ROW(IJ)=IFIX(WURR(NI,IT)*100.0)
        IF(PURP.EQ.10) ROW(IJ)=IFIX(WBRTR(NI,IT)*100.0)
        END IF
 3933  CONTINUE
      END IF
 3932 CONTINUE
      CALL OUTAB(80,ROW,II,PURP,DUMMY,IO)
      END IF
C....
      IF(PURP.EQ.11.OR.PURP.EQ.12.OR.PURP.EQ.13) THEN
      IF(II.GT.MAX_IZONES) THEN
      DO 3935,IJ=1,MAX_ZONES
        AJZ=0
        DO 3936,IT=1,50
        IF(AEQUIV(IT).EQ.IJ) AJZ=IT
 3936   CONTINUE
        IT=II-MAX_IZONES
       IF(PURP.EQ.11.AND.AJZ.GT.0) ROW(IJ)=IFIX(LCRRSTAZ(IT,AJZ)*100.0)
       IF(PURP.EQ.12.AND.AJZ.GT.0) ROW(IJ)=IFIX(LURRSTAZ(IT,AJZ)*100.0)
       IF(PURP.EQ.13.AND.AJZ.GT.0) ROW(IJ)=IFIX(LBRTRSTAZ(IT,AJZ)*100.0)
 3935 CONTINUE
      END IF
      CALL OUTAB(80,ROW,II,PURP,DUMMY,IO)
      END IF
C....
      IF(PURP.EQ.14.OR.PURP.EQ.15.OR.PURP.EQ.16) THEN
      IF(II.GT.MAX_IZONES) THEN
      DO 3937,IJ=1,MAX_ZONES
      IT=II-MAX_IZONES
      IQ=IJ-MAX_IZONES
      IF(IJ.LE.MAX_IZONES) GO TO 3937
      IF(PURP.EQ.14) ROW(IJ)=IFIX(LCRSSR(IT,IQ)*100.0)
      IF(PURP.EQ.15) ROW(IJ)=IFIX(LURSSR(IT,IQ)*100.0)
      IF(PURP.EQ.16) ROW(IJ)=IFIX(LBRTSSR(IT,IQ)*100.0)
 3937 CONTINUE
      END IF
      CALL OUTAB(80,ROW,II,PURP,DUMMY,IO)
      END IF
 3931 CONTINUE
 3930 CONTINUE
C..........................................................
C
C     OUTPUT ITF FACILITY TRANSIT TRIPS
C
      IF(ITFZONE.GT.0) THEN
      DO 3940,II=1,MAX_ZONES
      DO 3941,PURP=1,16
       DO IJ=1,MAX_ZONES
       ROW(IJ)=0
       END DO
      IF(PURP.LE.10) THEN
      IF(ITFZONE.EQ.II) THEN
       DO 3943,IJ=1,MAX_ZONES
        IF(PURP.LE.7) THEN
        AJZ=0
        DO 3944,IT=1,50
        IF(AEQUIV(IT).EQ.IJ) AJZ=IT
 3944   CONTINUE
        IF(AJZ.GT.0) ROW(IJ)=IFIX(ITFTRN(AJZ,PURP)*100.0)
        ELSE
        IF(IJ.LE.MAX_IZONES) GO TO 3943
        IT=IJ-MAX_IZONES
        IF(PURP.EQ.8) ROW(IJ)=IFIX(WCRI(IT)*100.0)
        IF(PURP.EQ.9) ROW(IJ)=IFIX(WURI(IT)*100.0)
        IF(PURP.EQ.10) ROW(IJ)=IFIX(WBRTI(IT)*100.0)
        END IF
 3943  CONTINUE
      END IF
C
      IF(ITFZONE2.EQ.II) THEN
       DO IJ=1,MAX_ZONES
        IF(PURP.LE.7) THEN
        AJZ=0
        DO IT=1,50
        IF(AEQUIV(IT).EQ.IJ) AJZ=IT
        END DO
        IF(AJZ.GT.0) ROW(IJ)=IFIX(ITFTRN2(AJZ,PURP)*100.0)
        END IF
        END DO
      END IF
C
      CALL OUTAB(94,ROW,II,PURP,DUMMY,IO)
      END IF
C....
      IF(PURP.EQ.11.OR.PURP.EQ.12.OR.PURP.EQ.13) THEN
      IF(II.GT.MAX_IZONES) THEN
      DO 3945,IJ=1,MAX_ZONES
        AJZ=0
        DO 3946,IT=1,50
        IF(AEQUIV(IT).EQ.IJ) AJZ=IT
 3946   CONTINUE
        IT=II-MAX_IZONES
       IF(PURP.EQ.11.AND.AJZ.GT.0) ROW(IJ)=IFIX(LCRISTAZ(IT,AJZ)*100.0)
       IF(PURP.EQ.12.AND.AJZ.GT.0) ROW(IJ)=IFIX(LURISTAZ(IT,AJZ)*100.0)
       IF(PURP.EQ.13.AND.AJZ.GT.0) ROW(IJ)=IFIX(LBRTISTAZ(IT,AJZ)*100.0)
 3945 CONTINUE
      END IF
      CALL OUTAB(94,ROW,II,PURP,DUMMY,IO)
      END IF
C....
      IF(PURP.EQ.14.OR.PURP.EQ.15.OR.PURP.EQ.16) THEN
      IF(II.GT.MAX_IZONES) THEN
      DO 3947,IJ=1,MAX_ZONES
      IT=II-MAX_IZONES
      IQ=IJ-MAX_IZONES
      IF(IJ.LE.MAX_IZONES) GO TO 3947
      IF(PURP.EQ.14) ROW(IJ)=IFIX(LCRSSI(IT,IQ)*100.0)
      IF(PURP.EQ.15) ROW(IJ)=IFIX(LURSSI(IT,IQ)*100.0)
      IF(PURP.EQ.16) ROW(IJ)=IFIX(LBRTSSI(IT,IQ)*100.0)
 3947 CONTINUE
      END IF
      CALL OUTAB(94,ROW,II,PURP,DUMMY,IO)
      END IF
 3941 CONTINUE
 3940 CONTINUE
      END IF
C
      END IF
C
C     OUTPUT ALTERNATIVE PARKING TRIP MATRICES
C
      IF(AVMDL.AND.ALTPARK) THEN
      write(*,9728)
 9728 format(/' Output Alternative Parking Trip Matrices'/)
      DO T=1,8
      CLOSE((250+T),STATUS='KEEP')
      END DO
      open(251,file='mf251.emx',recl=4,access='direct',
     *       status='unknown',form='binary')
      open(252,file='mf252.emx',recl=4,access='direct',
     *       status='unknown',form='binary')
      open(253,file='mf253.emx',recl=4,access='direct',
     *       status='unknown',form='binary')
      open(254,file='mf254.emx',recl=4,access='direct',
     *       status='unknown',form='binary')
      open(255,file='mf255.emx',recl=4,access='direct',
     *       status='unknown',form='binary')
      open(256,file='mf256.emx',recl=4,access='direct',
     *       status='unknown',form='binary')
      open(257,file='mf257.emx',recl=4,access='direct',
     *       status='unknown',form='binary')
      open(258,file='mf258.emx',recl=4,access='direct',
     *       status='unknown',form='binary')
      do iz=1,max_zones
      nk=iz
      nk=mod(nk,500)
      if(nk.eq.0) write(*,8002) iz
      do unit=251,258
      frow=0.0
	    do jz=1,max_zones
      recno = ((iz-1)*mxzones) + jz
      read(unit,rec=recno) mfval
      frow(jz)=mfval
      end do
      rem38=0.0
      do jj=1,max_zones
      if(frow(jj).gt.0.0) then
      temp=(frow(jj)*100.0) + rem38
      row(jj)=ifix(temp)
      rem38=temp-row(jj)
      else
      row(jj)=0
      endif
      end do
      purp=unit-250
      call outab(143,row,iz,purp,dummy,io)    
      end do
      end do
      do t=1,8
      close((250+T),STATUS='delete')
      end do
      END IF
C
C SUMMARIZE MODAL TRIP VALUES FOR REGION
C
 9200 CONTINUE
      IF(LAXTRN) GO TO 1505
      IF(EVENTSP) GO TO 1505
      IF(SPEVENT.AND.(.NOT.EVENTSP).AND.(.NOT.EVENTLI)) GO TO 1505
      IF(HCALIB.AND.HFACT) GO TO 1505
      IF(RECYC) GO TO 8001
      IF(.NOT.AIRPASS) WRITE(26,9201)
 9201 FORMAT(//,30X,'R E P O R T   1',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE AND MARKET SEGMENT',//,
     *       1X,'  MARKET   ','         ','    2    ','      3    ',
     *          '   4+   ','         ',
     *          '         ',
     *          '         ','         '/
     *       1X,' SEGMENT  ','  DRIVE  ','  PERSON ','    PERSON ',
     *          ' PERSON ','          ','   NON   '/
     *       1X,'  LEVEL  ','   ALONE ','    AUTO ','      AUTO ',
     *          '   AUTO ','  TRANSIT ',' MOTORIZED ','   MAAS     ',
     *          '    TOTAL      '/
     *       1X,'---------','---------','---------','----------',
     *          '----------','----------',
     *          '----------',
     *          '-----------','----------','-----')
      DO 400 C=1,NCATS
      IF(.NOT.AIRPASS) WRITE(26,9202) C,(TESUM(T,C),T=1,3),
     *                  TESUM(64,C),TESUM(5,C),
     *                  TESUM(50,C),TESUM(82,C),TESUM(6,C)
 9202 FORMAT(5X,I1,2X,8F10.0)
      DO 401 K=1,90
      TESUM(K,6)=TESUM(K,6) + TESUM(K,C)
      IF(K.LE.28) TOLSUM(K,6)=TOLSUM(K,6) + TOLSUM(K,C)
      IF(K.LE.4)  CALSUM(K,6)=CALSUM(K,6) + CALSUM(K,C)
      IF(K.LE.42) XTESUM(K,6)=XTESUM(K,6) + XTESUM(K,C)
  401 CONTINUE
  400 CONTINUE
      IF(.NOT.AIRPASS) WRITE(26,9203) (TESUM(T,6),T=1,3),
     *                TESUM(64,6),TESUM(5,6),
     *                TESUM(50,6),TESUM(82,6),TESUM(6,6)
 9203 FORMAT(/,2X,'TOTAL',1X,8F10.0)
      IF(LAX.AND.(.NOT.LAXTRN)) THEN
      PNRTRP=0.0
      PNRTRP(1)=TESUM(1,6)/TESUM(6,6)
      PNRTRP(2)=TESUM(2,6)/TESUM(6,6)
      PNRTRP(3)=TESUM(3,6)/TESUM(6,6)
      PNRTRP(4)=TESUM(64,6)/TESUM(6,6)
      PNRTRP(5)=TESUM(5,6)/TESUM(6,6)
      PNRTRP(6)=TESUM(50,6)/TESUM(6,6)
      IF(.NOT.AIRPASS) WRITE(26,9703) (PNRTRP(K),K=1,6)
 9703 FORMAT(/,1X,'PERCENT',7F10.4)
      END IF
      IF(NMOT.AND.(.NOT.AIRPASS)) THEN
      WRITE(26,9901)
 9901 FORMAT(//,30X,'R E P O R T   1A',/,
     *          20X,
     *          'SUMMARIZE NON-MOTORIZED TRIPS BY MARKET SEGMENT',//,
     *       1X,'  MARKET   '/
     *       1X,'  SEGMENT ',/
     *       1X,'  LEVEL  ','   WALK  ','   BIKE    E-SCOOTER'/
     *       1X,'---------','---------','---------  ----------'/)
      DO 9400 C=1,NCATS
      WRITE(26,9902) C,TESUM(51,C),TESUM(52,C),TESUM(90,C)
 9902 FORMAT(5X,I1,2X,3F10.0)
 9400 CONTINUE
      WRITE(26,9903) TESUM(51,6),TESUM(52,6),TESUM(90,6)
 9903 FORMAT(/,2X,'TOTAL',1X,3F10.0)
      END IF
C
      WRITE(26,9905)
 9905 FORMAT(//,30X,'R E P O R T   1G',/,
     *          20X,
     *          'SUMMARIZE MAAS TRIPS BY MARKET SEGMENT',//,
     *       1X,'  MARKET   '/
     *       1X,'  SEGMENT ','        ','   CONV  ','    AV'/
     *       1X,'  LEVEL  ','   TAXI  ','   UBER  ','   UBER  '/
     *       1X,'---------','---------','---------','---------'/)
      DO C=1,NCATS
      WRITE(26,9907) C,TESUM(80,C),TESUM(81,C),TESUM(89,C)
 9907 FORMAT(5X,I1,2X,3F10.0)
      END DO
      WRITE(26,9908) TESUM(80,6),TESUM(81,6),TESUM(89,6)
 9908 FORMAT(/,2X,'TOTAL',1X,3F10.0)
      IF(ALTPARK) THEN
      WRITE(26,9906)
 9906 FORMAT(//,30X,'R E P O R T   1H',/,
     *          20X,
     *        'SUMMARIZE PARKING LOCATION TRIPS BY MARKET SEGMENT',//,
     *       1X,'  MARKET   '/
     *       1X,'  SEGMENT','    AT   ','  RETURN ',' EXTERNAL'/
     *       1X,'  LEVEL  ','   WORK  ','   HOME  ','   LOT   '/
     *       1X,'---------','---------','---------','---------')
      DO C=1,NCATS
      WRITE(26,9907) C,TESUM(86,C),TESUM(87,C),TESUM(88,C)
      END DO
      WRITE(26,9908) TESUM(86,6),TESUM(87,6),TESUM(88,6)
      END IF
C..........................................................................
      IF(AIRPASS) THEN
      WRITE(26,9261)
 9261 FORMAT(//,30X,'R E P O R T   1',/,
     *          20X,'SUMMARIZE AIR PASSENGER TRIPS BY MODE',//,
     *       1X,'              2         3        4+'/     
     *       1X,'  DRIVE     PERSON    PERSON   PERSON            ',
     *          '   LIMO      RENTAL             ON-CALL ',
     *          '  PUBLIC      FLY'/     
     *       1X,'  ALONE      AUTO      AUTO     AUTO     DROPOFF ',
     *          '  TOWN CAR    CAR       TAXI    SHUTTLE ',
     *          '  TRANSIT     AWAY     TOTAL'/  
     *       1X,' --------  --------  --------  --------  --------',
     *          '  --------  --------  --------  --------',
     *          '  --------  --------  ---------')
      WRITE(26,9262) AESUM
 9262 FORMAT(12(2X,F8.1))
      DO NI=1,11
      PNRTRP(NI)=AESUM(NI)/AESUM(12)
      END DO
      WRITE(26,9733) (PNRTRP(K),K=1,11)
 9733 FORMAT(/11(2X,F8.4))
      PNRTRP(1)=PNRTRP(1)+PNRTRP(2)+PNRTRP(3)+PNRTRP(4)
      DENOM=AESUM(1)+AESUM(2)+AESUM(3)+AESUM(4)
      OPEN(78,FILE=AIRCALIB,STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(78,9734) AESUM(5),PNRTRP(5),DENOM,PNRTRP(1),
     *               AESUM(7),PNRTRP(7),AESUM(6),PNRTRP(6),
     *               AESUM(8),PNRTRP(8),
     *               AESUM(9),PNRTRP(9),AESUM(10),PNRTRP(10),
     *               AESUM(11),PNRTRP(11),
     *               AESUM(1),AESUM(2),AESUM(3),AESUM(4)
 9734 FORMAT('DROPOFF,',F8.1,',',F8.4/
     *       'DRIVE,',F8.1,',',F8.4/
     *       'RENTAL,',F8.1,',',F8.4/
     *       'LIMO,',F8.1,',',F8.4/
     *       'TAXI,',F8.1,',',F8.4/
     *       'ON_CALL,',F8.1,',',F8.4/
     *       'TRANSIT,',F8.1,',',F8.4/
     *       'FLY_AWAY,',F8.1,',',F8.4/
     *       'DRIVE_ALONE,',F8.1/
     *       '2_PERSON,',F8.1/
     *       '3_PERSON,',F8.1/
     *       '4_PERSON,',F8.1)
      IF(ITFTXI.OR.ITFLMO.OR.ITFDRP) THEN
      WRITE(26,9735) (ITFSUM(1,K),K=1,4),(ITFSUM(2,K1),K1=1,4),
     *               (ITFSUM(3,K2),K2=1,4)
 9735 FORMAT(//,30X,'R E P O R T   2',/,
     *          20X,'SUMMARIZE ITF DISTRIBUTION',//,
     *         '  TYPE    CTA      ITF 1     ITF 2     TOTAL'/
     *         ' -----  --------  --------  --------  --------'/
     *         '  TAXI',4(2X,F8.1)/
     *         '  LIMO',4(2X,F8.1)/
     *         '  DROP',4(2X,F8.1))
      END IF
      END IF
C ........................................................................
C
C SUMMARIZE AUTO TRIP VALUES FOR REGION
C
      WRITE(26,9225)
 9225 FORMAT(//,30X,'R E P O R T   1B',/,
     *          20X,
     *          'SUMMARIZE AUTO PERSON TRIPS BY MARKET SEGMENT',//,
     *       1X,'  MARKET ',' DRIVE   ','   DRIVE ',' 2 PERSON',
     *          ' 2 PERSON',' 2 PERSON',' 2 PERSON',' 3 PERSON',
     *          ' 3 PERSON',' 3 PERSON',' 3 PERSON',' 4 PERSON',
     *          ' 4 PERSON',' 4 PERSON',' 4 PERSON'/
     *       1X,' SEGMENT ',' ALONE   ','   ALONE ',' NON TOLL',
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
      DO 430 C=1,NCATS
      IF(.NOT.AIRPASS) WRITE(26,9302) C,(TOLSUM(T,C),T=1,14)
 9302 FORMAT(5X,I1,3X,14F9.0)
  430 CONTINUE
      WRITE(26,9303) (TOLSUM(T,6),T=1,14)
 9303 FORMAT(/,2X,'TOTAL',2X,14F9.0)
      IF(AVMDL) THEN
      WRITE(26,9226)
 9226 FORMAT(//,30X,'R E P O R T   1B(2)',/,
     *          20X,
     *          'SUMMARIZE AUTO PERSON TRIPS BY MARKET SEGMENT',/,
     *      20X,'       AUTOMATED HOUSEHOLD VEHICLES'//
     *       1X,'  MARKET ',' DRIVE   ','   DRIVE ',' 2 PERSON',
     *          ' 2 PERSON',' 2 PERSON',' 2 PERSON',' 3 PERSON',
     *          ' 3 PERSON',' 3 PERSON',' 3 PERSON',' 4 PERSON',
     *          ' 4 PERSON',' 4 PERSON',' 4 PERSON'/
     *       1X,' SEGMENT ',' ALONE   ','   ALONE ',' NON TOLL',
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
      DO C=1,NCATS
      IF(.NOT.AIRPASS) WRITE(26,9302) C,(TOLSUM(T,C),T=15,28)
      END DO
      WRITE(26,9303) (TOLSUM(T,6),T=15,28)
      END IF
C
C SUMMARIZE TRANSIT TRIP VALUES FOR REGION
C
      WRITE(26,9204)
 9204 FORMAT(//,30X,'R E P O R T   2A',/,
     *          20X,
     *          'SUMMARIZE TRANSIT TRIPS BY MARKET SEGMENT',//,
     *       1X,' MARKET  ','   LOCAL  ','  LOCAL   ',' EXPRESS  ',
     *                      '  EXPRESS ','TRANSITWAY','TRANSITWAY',
     *                                   '  RAPID   ','  RAPID   '/
     *       1X,' SEGMENT ','    BUS   ','   BUS    ','   BUS    ',
     *                      '    BUS   ','   BUS    ','   BUS    ',
     *                                   '   BUS    ','   BUS    '/
     *       1X,'  LEVEL  ','    WALK  ','  DRIVE   ','   WALK   ',
     *                      '   DRIVE  ','   WALK   ','  DRIVE   ',
     *                                   '   WALK   ','  DRIVE   '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------','----------',
     *                                   '----------','----------')
      DO 410 C=1,NCATS
      IF(.NOT.AIRPASS) WRITE(26,9205) C,(TESUM(T,C),T=7,10),
     *               (TESUM(L,C),L=19,20),
     *               TESUM(48,C),TESUM(49,C)
 9205 FORMAT(5X,I1,2X,11F10.0)
      IF(CALIB) THEN
      IF(C.EQ.1) WRITE(67,5210) CITER
 5210 FORMAT(' Calibration Iteration=',i2/
     *       'MODE,LCLWLK,LCLDRV,EXPWLK,EXPDRV,TWYWLK,TWYDRV,RPDWLK,',
     *       'RPDDRV,LCLBIK,RPDBIK,TWYBIK,',
     *       'EXPBIK')
      WRITE(67,5211) C,(TESUM(T,C),T=7,10),(TESUM(L,C),L=19,20),
     *               TESUM(48,C),TESUM(49,C),TESUM(65,C),
     *               TESUM(66,C),TESUM(68,C),
     *               TESUM(70,C)
 5211 FORMAT(I1,16(',',F10.1))
      END IF
  410 CONTINUE
      WRITE(26,9206) (TESUM(T,6),T=7,10),(TESUM(L,6),L=19,20),
     *                TESUM(48,6),TESUM(49,6)
 9206 FORMAT(/,2X,'TOTAL',1X,11F10.0)
      WRITE(26,9224)
 9224 FORMAT(//,30X,'R E P O R T   2A',/,
     *          20X,
     *          'SUMMARIZE TRANSIT TRIPS BY MARKET SEGMENT'//,
     *       1X,' MARKET  ','   LOCAL  ','  RAPID   ','TRANSITWAY',
     *                      '  EXPRESS '/
     *       1X,' SEGMENT ','    BUS   ','   BUS    ','   BUS    ',
     *                      '    BUS   '/
     *       1X,'  LEVEL  ','    BIKE  ','   BIKE   ','   BIKE   ',
     *                      '    BIKE  '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------')
      DO C=1,NCATS
      IF(.NOT.AIRPASS) WRITE(26,9205) C,TESUM(65,C),TESUM(66,C),
     *                 TESUM(68,C),TESUM(70,C)
      END DO
      WRITE(26,9206) TESUM(65,6),TESUM(66,6),TESUM(68,6),TESUM(70,6)
C
C SUMMARIZE AVAILABLE PERSON TRIPS BY TRANSIT MODE AND MARKET SEGMENT
C
      IF(CALIB) THEN
      WRITE(67,5212) (TESUM(T,6),T=7,10),(TESUM(L,6),L=19,20),
     *                TESUM(48,6),TESUM(49,6),TESUM(65,6),TESUM(66,6),
     *                TESUM(68,6),TESUM(70,6)
 5212 FORMAT('TOTAL',16(',',F10.1))
C
C SUMMARIZE TRIPS WITH LOCAL BUS TRANSFER
C
      WRITE(26,6801)
 6801 FORMAT(//,30X,'R E P O R T   2A',/,
     *          20X,
     *        'SUMMARIZE WALK ACCESS TRANSIT TRIPS WITH BUS TRANSFER',
     *        ' BY MARKET SEGMENT',//,
     *       1X,' INCOME  ','  RAPID   ','TRANSITWAY',' EXPRESS  '/
     *       1X,'  GROUP  ','   BUS    ','   BUS    ','   BUS    '/
     *       1X,'  LEVEL  ',' TRANSFERS',' TRANSFERS',' TRANSFERS'/
     *       1X,'---------','----------','----------','----------')
      DO 6802 C=1,NCATS
      WRITE(26,9205) C,TESUM(53,C),TESUM(54,C),TESUM(55,C)
 6802 CONTINUE
      WRITE(26,9206) TESUM(53,6),TESUM(54,6),TESUM(55,6)
      WRITE(26,6803)
 6803 FORMAT(//,30X,'R E P O R T   2A',/,
     *          20X,
     *        'SUMMARIZE DRIVE ACCESS TRANSIT TRIPS WITH BUS TRANSFER',
     *        ' BY MARKET SEGMENT',//,
     *       1X,' MARKET  ','  RAPID   ','TRANSITWAY',' EXPRESS  '/
     *       1X,' SEGMENT ','   BUS    ','   BUS    ','   BUS    '/
     *       1X,'  LEVEL  ',' TRANSFERS',' TRANSFERS',' TRANSFERS'/
     *       1X,'---------','----------','----------','----------')
      DO 6804 C=1,NCATS
      WRITE(26,9205) C,TESUM(60,C),TESUM(62,C),TESUM(63,C)
 6804 CONTINUE
      WRITE(26,9206) TESUM(60,6),TESUM(62,6),TESUM(63,6)
      END IF
C
C COMMUTER RAIL, URBAN RAIL & BRT
C
      WRITE(26,9207)
 9207 FORMAT(//,30X,'R E P O R T   2B',/,
     *          20X,
     *          'SUMMARIZE COMMUTER RAIL BY MARKET SEGMENT',//,
     *       1X,' MARKET  ','          ','          ','          ',
     *          '   PARK   ','   KISS   ','          '/
     *       1X,' SEGMENT ','          ','          ','          ',
     *          '   AND    ','    AND   ','          '/
     *       1X,'  LEVEL  ','    WALK  ','   BIKE   ','   BUS    ',
     *          '   RIDE   ','    RIDE  ','  UBER    ','  TOTAL   '/
     *       1X,'---------','----------','----------','----------',
     *          '----------','----------','----------','----------')
      DO 420 C=1,NCATS
      IF(.NOT.AIRPASS) WRITE(26,9208) C,TESUM(11,C),TESUM(74,C),
     *                (TESUM(T,C),T=12,14),TESUM(83,C),TESUM(34,C)
 9208 FORMAT(5X,I1,2X,7F10.0)
      IF(CALIB) THEN
      IF(C.EQ.1) WRITE(67,9260)
 9260 FORMAT('COMMUTER_RAIL,WALK,BIKE,BUS,PNR,KNR,TOTAL')
      WRITE(67,5208) C,TESUM(11,C),TESUM(74,C),
     *               (TESUM(T,C),T=12,14),TESUM(34,C)
 5208 FORMAT(I1,6(',',F10.1))
      END IF
  420 CONTINUE
      WRITE(26,9209) TESUM(11,6),TESUM(74,6),
     *              (TESUM(T,6),T=12,14),TESUM(83,6),TESUM(34,6)
 9209 FORMAT(/,2X,'TOTAL',1X,7F10.0)
      IF(CALIB) THEN
      WRITE(67,5209) TESUM(11,6),TESUM(74,6),(TESUM(T,6),T=12,14),
     *               TESUM(34,6)
 5209 FORMAT('TOTAL',6(',',F10.1))
      END IF
C
      WRITE(26,9326)
 9326 FORMAT(//,30X,'R E P O R T   2C',/,
     *          20X,
     *          'SUMMARIZE URBAN RAIL BY MARKET SEGMENT',//,
     *       1X,' MARKET  ','          ','          ','          ',
     *          '   PARK   ','   KISS   ','          '/
     *       1X,' SEGMENT ','          ','          ','          ',
     *          '   AND    ','    AND   ','          '/
     *       1X,'  LEVEL  ','    WALK  ','   BIKE   ','   BUS    ',
     *          '   RIDE   ','    RIDE  ','  UBER    ','  TOTAL   '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------','----------',
     *                      '----------')
      DO 421 C=1,NCATS
      IF(.NOT.AIRPASS) WRITE(26,9208) C,TESUM(15,C),TESUM(75,C),
     *                 (TESUM(T,C),T=16,18),TESUM(84,C),TESUM(47,C)
      IF(CALIB) THEN
      IF(C.EQ.1) WRITE(67,9259)
 9259 FORMAT('URBAN_RAIL,WALK,BIKE,BUS,PNR,KNR,TOTAL')
      WRITE(67,5208) C,TESUM(15,C),TESUM(75,C),
     *              (TESUM(T,C),T=16,18),TESUM(47,C)
      END IF
  421 CONTINUE
      WRITE(26,9209) TESUM(15,6),TESUM(75,6),(TESUM(T,6),T=16,18),
     *               TESUM(84,6),TESUM(47,6)
      IF(CALIB) THEN
      WRITE(67,5209) TESUM(15,6),TESUM(75,6),(TESUM(T,6),T=16,18),
     *               TESUM(47,6)
      END IF
      WRITE(26,9626)
 9626 FORMAT(//,30X,'R E P O R T   2D',/,
     *          20X,
     *          'SUMMARIZE BUS RAPID TRANSIT BY MARKET SEGMENT',//,
     *       1X,' MARKET  ','          ','          ','          ',
     *          '   PARK   ','   KISS   ','          '/
     *       1X,' SEGMENT ','          ','          ','          ',
     *          '   AND    ','    AND   ','          '/
     *       1X,'  LEVEL  ','    WALK  ','   BIKE   ','   BUS    ',
     *          '   RIDE   ','    RIDE  ','  UBER    ','  TOTAL   '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------','----------',
     *                      '----------')
      DO C=1,NCATS
      IF(.NOT.AIRPASS) WRITE(26,9208) C,TESUM(56,C),TESUM(72,C),
     *                 (TESUM(T,C),T=57,59),TESUM(85,C),TESUM(61,C)
      IF(CALIB) THEN
      IF(C.EQ.1) WRITE(67,9659)
 9659 FORMAT('BRT,WALK,BIKE,BUS,PNR,KNR,TOTAL')
      WRITE(67,5208) C,TESUM(56,C),TESUM(72,C),
     *              (TESUM(T,C),T=57,59),TESUM(61,C)
      END IF
      END DO
      WRITE(26,9209) TESUM(56,6),TESUM(72,6),(TESUM(T,6),T=57,59),
     *               TESUM(85,6),TESUM(61,6)
      IF(CALIB) THEN
      WRITE(67,5209) TESUM(56,6),TESUM(72,6),(TESUM(T,6),T=57,59),
     *               TESUM(61,6)
      END IF
      WRITE(26,9356)
 9356 FORMAT(//,30X,'R E P O R T   2E',/,
     *          20X,
     *  'SUMMARIZE URBAN RAIL TO COMMUTER RAIL BY MARKET SEGMENT',//,
     *       1X,' MARKET  ','          ','          ','          ',
     *          '   PARK   ','   KISS   ','          '/
     *       1X,' SEGMENT ','          ','          ','          ',
     *          '   AND    ','    AND   ','          '/
     *       1X,'  LEVEL  ','    WALK  ','   BIKE   ','   BUS    ',
     *          '   RIDE   ','    RIDE  ','  UBER    ','  TOTAL   '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------','----------',
     *                      '----------')
      DO C=1,NCATS
      RTEMP=XTESUM(1,C)+XTESUM(2,C)+XTESUM(3,C)+
     *      XTESUM(4,C)+XTESUM(5,C)+XTESUM(37,C)
      IF(.NOT.AIRPASS) WRITE(26,9208) C,XTESUM(1,C),XTESUM(5,C),
     *    (XTESUM(T,C),T=2,4),XTESUM(37,C),RTEMP
      IF(CALIB) THEN
      IF(C.EQ.1) WRITE(67,9660)
 9660 FORMAT('UR_CR,WALK,BIKE,BUS,PNR,KNR,TOTAL')
      WRITE(67,5208) C,XTESUM(1,C),XTESUM(5,C),
     *    (XTESUM(T,C),T=2,4),RTEMP 
      END IF
      END DO
      RTEMP=XTESUM(1,6)+XTESUM(2,6)+XTESUM(3,6)+
     *      XTESUM(4,6)+XTESUM(5,6)+XTESUM(37,6)
      WRITE(26,9209) XTESUM(1,6),XTESUM(5,6),(XTESUM(T,6),T=2,4),
     *               XTESUM(37,6),RTEMP
      IF(CALIB) THEN
      WRITE(67,5209) XTESUM(1,6),XTESUM(5,6),(XTESUM(T,6),T=2,4),RTEMP
      END IF    
      WRITE(26,9357)
 9357 FORMAT(//,30X,'R E P O R T   2F',/,
     *          20X,
     *  'SUMMARIZE BRT TO COMMUTER RAIL BY MARKET SEGMENT',//,
     *       1X,' MARKET  ','          ','          ','          ',
     *          '   PARK   ','   KISS   ','          '/
     *       1X,' SEGMENT ','          ','          ','          ',
     *          '   AND    ','    AND   ','          '/
     *       1X,'  LEVEL  ','    WALK  ','   BIKE   ','   BUS    ',
     *          '   RIDE   ','    RIDE  ','  UBER    ','  TOTAL   '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------','----------',
     *                      '----------')
      DO C=1,NCATS
      RTEMP=XTESUM(6,C)+XTESUM(7,C)+XTESUM(8,C)+
     *      XTESUM(9,C)+XTESUM(10,C)+XTESUM(38,C)
      IF(.NOT.AIRPASS) WRITE(26,9208) C,XTESUM(6,C),XTESUM(10,C),
     *     (XTESUM(T,C),T=7,9),XTESUM(38,C),RTEMP
      IF(CALIB) THEN
      IF(C.EQ.1) WRITE(67,9661)
 9661 FORMAT('BR_CR,WALK,BIKE,BUS,PNR,KNR,TOTAL')
      WRITE(67,5208) C,XTESUM(6,C),XTESUM(10,C),
     *    (XTESUM(T,C),T=7,9),RTEMP 
      END IF
      END DO
      RTEMP=XTESUM(6,6)+XTESUM(7,6)+XTESUM(8,6)+
     *      XTESUM(9,6)+XTESUM(10,6)+XTESUM(38,6)
      WRITE(26,9209) XTESUM(6,6),XTESUM(10,6),(XTESUM(T,6),T=7,9),
     *               XTESUM(38,6),RTEMP
      IF(CALIB) THEN
      WRITE(67,5209) XTESUM(6,6),XTESUM(10,6),(XTESUM(T,6),T=7,9),RTEMP
      END IF  
      WRITE(26,9358)
 9358 FORMAT(//,30X,'R E P O R T   2G',/,
     *          20X,
     *  'SUMMARIZE BRT TO URBAN RAIL BY MARKET SEGMENT',//,
     *       1X,' MARKET  ','          ','          ','          ',
     *          '   PARK   ','   KISS   ','          '/
     *       1X,' SEGMENT ','          ','          ','          ',
     *          '   AND    ','    AND   ','          '/
     *       1X,'  LEVEL  ','    WALK  ','   BIKE   ','   BUS    ',
     *          '   RIDE   ','    RIDE  ','  UBER    ','  TOTAL   '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------','----------',
     *                      '----------')
      DO C=1,NCATS
      RTEMP=XTESUM(11,C)+XTESUM(12,C)+XTESUM(13,C)+
     *      XTESUM(14,C)+XTESUM(15,C)+XTESUM(39,C)
      IF(.NOT.AIRPASS) WRITE(26,9208) C,XTESUM(11,C),XTESUM(15,C),
     *     (XTESUM(T,C),T=12,14),XTESUM(39,C),RTEMP
      IF(CALIB) THEN
      IF(C.EQ.1) WRITE(67,9662)
 9662 FORMAT('BR_UR,WALK,BIKE,BUS,PNR,KNR,TOTAL')
      WRITE(67,5208) C,XTESUM(11,C),XTESUM(15,C),
     *    (XTESUM(T,C),T=12,14),RTEMP 
      END IF
      END DO
      RTEMP=XTESUM(11,6)+XTESUM(12,6)+XTESUM(13,6)+
     *      XTESUM(14,6)+XTESUM(15,6)+XTESUM(39,6)
      WRITE(26,9209) XTESUM(11,6),XTESUM(15,6),
     *      (XTESUM(T,6),T=12,14),XTESUM(39,6),RTEMP
      IF(CALIB) THEN
      WRITE(67,5209) XTESUM(11,6),XTESUM(15,6),(XTESUM(T,6),T=12,14),
     * RTEMP
      END IF  
      WRITE(26,9511)
 9511 FORMAT(//,30X,'R E P O R T   2H',/,
     *          20X,
     *  'SUMMARIZE URBAN RAIL TO BRT BY MARKET SEGMENT',//,
     *       1X,' MARKET  ','          ','          ','          ',
     *          '   PARK   ','   KISS   ','          '/
     *       1X,' SEGMENT ','          ','          ','          ',
     *          '   AND    ','    AND   ','          '/
     *       1X,'  LEVEL  ','    WALK  ','   BIKE   ','   BUS    ',
     *          '   RIDE   ','    RIDE  ','  UBER    ','  TOTAL   '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------','----------',
     *                      '----------')
      DO C=1,NCATS
      RTEMP=XTESUM(19,C)+XTESUM(20,C)+XTESUM(21,C)+
     *      XTESUM(22,C)+XTESUM(23,C)+XTESUM(40,C)
      IF(.NOT.AIRPASS) WRITE(26,9208) C,XTESUM(19,C),XTESUM(23,C),
     *     (XTESUM(T,C),T=20,22),XTESUM(40,C),RTEMP
      IF(CALIB) THEN
      IF(C.EQ.1) WRITE(67,9512)
 9512 FORMAT('UR_BR,WALK,BIKE,BUS,PNR,KNR,TOTAL')
      WRITE(67,5208) C,XTESUM(19,C),XTESUM(23,C),
     *    (XTESUM(T,C),T=20,22),RTEMP 
      END IF
      END DO
      RTEMP=XTESUM(19,6)+XTESUM(20,6)+XTESUM(21,6)+
     *      XTESUM(22,6)+XTESUM(23,6)+XTESUM(40,6)
      WRITE(26,9209) XTESUM(19,6),XTESUM(23,6),
     *      (XTESUM(T,6),T=20,22),XTESUM(40,6),RTEMP
      IF(CALIB) THEN
      WRITE(67,5209) XTESUM(19,6),XTESUM(23,6),(XTESUM(T,6),T=20,22),
     * RTEMP
      END IF  
      WRITE(26,9513)
 9513 FORMAT(//,30X,'R E P O R T   2I',/,
     *          20X,
     *  'SUMMARIZE COMMUTER RAIL TO URBAN RAIL BY MARKET SEGMENT',//,
     *       1X,' MARKET  ','          ','          ','          ',
     *          '   PARK   ','   KISS   ','          '/
     *       1X,' SEGMENT ','          ','          ','          ',
     *          '   AND    ','    AND   ','          '/
     *       1X,'  LEVEL  ','    WALK  ','   BIKE   ','   BUS    ',
     *          '   RIDE   ','    RIDE  ','  UBER    ','  TOTAL   '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------','----------',
     *                      '----------')
      DO C=1,NCATS
      RTEMP=XTESUM(25,C)+XTESUM(26,C)+XTESUM(27,C)+
     *      XTESUM(28,C)+XTESUM(29,C)+XTESUM(41,C)
      IF(.NOT.AIRPASS) WRITE(26,9208) C,XTESUM(25,C),XTESUM(29,C),
     *     (XTESUM(T,C),T=26,28),XTESUM(41,C),RTEMP
      IF(CALIB) THEN
      IF(C.EQ.1) WRITE(67,9514)
 9514 FORMAT('CR_UR,WALK,BIKE,BUS,PNR,KNR,TOTAL')
      WRITE(67,5208) C,XTESUM(25,C),XTESUM(29,C),
     *    (XTESUM(T,C),T=26,28),RTEMP 
      END IF
      END DO
      RTEMP=XTESUM(25,6)+XTESUM(26,6)+XTESUM(27,6)+
     *      XTESUM(28,6)+XTESUM(29,6)+XTESUM(41,6)
      WRITE(26,9209) XTESUM(25,6),XTESUM(29,6),
     *      (XTESUM(T,6),T=26,28),XTESUM(41,6),RTEMP
      IF(CALIB) THEN
      WRITE(67,5209) XTESUM(25,6),XTESUM(29,6),(XTESUM(T,6),T=26,28),
     * RTEMP
      END IF 
      WRITE(26,9517)
 9517 FORMAT(//,30X,'R E P O R T   2J',/,
     *          20X,
     *  'SUMMARIZE COMMUTER RAIL TO BRT BY MARKET SEGMENT',//,
     *       1X,' MARKET  ','          ','          ','          ',
     *          '   PARK   ','   KISS   ','          '/
     *       1X,' SEGMENT ','          ','          ','          ',
     *          '   AND    ','    AND   ','          '/
     *       1X,'  LEVEL  ','    WALK  ','   BIKE   ','   BUS    ',
     *          '   RIDE   ','    RIDE  ','  UBER    ','  TOTAL   '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------','----------',
     *                      '----------')
      DO C=1,NCATS
      RTEMP=XTESUM(31,C)+XTESUM(32,C)+XTESUM(33,C)+
     *      XTESUM(34,C)+XTESUM(35,C)+XTESUM(42,C)
      IF(.NOT.AIRPASS) WRITE(26,9208) C,XTESUM(31,C),XTESUM(35,C),
     *     (XTESUM(T,C),T=32,34),XTESUM(42,C),RTEMP
      IF(CALIB) THEN
      IF(C.EQ.1) WRITE(67,9516)
 9516 FORMAT('CR_BR,WALK,BIKE,BUS,PNR,KNR,TOTAL')
      WRITE(67,5208) C,XTESUM(31,C),XTESUM(35,C),
     *    (XTESUM(T,C),T=32,34),RTEMP 
      END IF
      END DO
      RTEMP=XTESUM(31,6)+XTESUM(32,6)+XTESUM(33,6)+
     *      XTESUM(34,6)+XTESUM(35,6)+XTESUM(42,6)
      WRITE(26,9209) XTESUM(31,6),XTESUM(35,6),
     *      (XTESUM(T,6),T=32,34),XTESUM(42,6),RTEMP
      IF(CALIB) THEN
      WRITE(67,5209) XTESUM(31,6),XTESUM(35,6),(XTESUM(T,6),T=32,34),
     * XTESUM(36,6),RTEMP
      END IF 
      IF((CALIB.OR.TXFSUM).AND.(DEBUG)) THEN
C...METRO RAPID
      XPCT(1)=(MODETRP(6,1,1,1)/TESUM(48,6))*100.0
      XPCT(2)=(MODETRP(6,1,1,2)/TESUM(49,6))*100.0
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
C---------------------------------------------------------------
      IF(DEBUG) THEN
      WRITE(26,9216) (TESUM(T,6),T=1,80)
 9216 FORMAT(/1X,'TOTAL INTERCHANGE SUMMARY VALUES'/
     *        1X,'--------------------------------'//
     *        1X,'TESUM=',10F9.0/
     *        1X,'      ',10F9.0/
     *        1X,'      ',10F9.0/
     *        1X,'      ',10F9.0/
     *        1X,'      ',10F9.0/
     *        1X,'      ',10F9.0/
     *        1X,'      ',10F9.0/
     *        1X,'      ',10F9.0)
      END IF
C---------------------------------------------------------------
C
C  SUMMARIZE STATION MODE OF ACCESS DATA
C
      CALL MACCEGR(STASUM,STASUM2)
      IF(CALIB) THEN
      WRITE(26,9210)
 9210 FORMAT(//,30X,'R E P O R T   3A',/,
     *          20X,
     *          'SUMMARIZE COMMUTER RAIL STATION ACCESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','  BIKE  ','   BUS  ','  PARK  ','  KISS  ',
     *          '        ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' ACCESS ',' ACCESS ',' ACCESS ','  RIDE  ','  RIDE  ',
     *          '  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------','--------',
     *          '-------  ')
        TTWLK=0.0
	      TTBUS=0.0
	      TTBIK=0.0
	      TTPNR=0.0
	      TTKNR=0.0  
	      TTOTAL=0.0
      DO 500 K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      STASUM(K,5)=STASUM(K,1)+STASUM(K,2)+STASUM(K,3)+STASUM(K,4)+
     *            STASUM(K,19)
        TTWLK=TTWLK+STASUM(K,1)
	    TTBUS=TTBUS+STASUM(K,2)
	    TTBIK=TTBIK+STASUM(K,19)
	    TTPNR=TTPNR+STASUM(K,3)
	    TTKNR=TTKNR+STASUM(K,4)
	    TTOTAL=TTOTAL+STASUM(K,5)
      IF(STASUM(K,5).GT.0.1) THEN
	 WRITE(26,9211) KS,STANAME(K),STASUM(K,1),STASUM(K,19),
     *                  (STASUM(K,L),L=2,5)
 9211    FORMAT(2X,I4,3X,A29,1X,6F8.0)
      END IF
  500 CONTINUE
        WRITE(26,9337) TTWLK,TTBIK,TTBUS,TTPNR,TTKNR,TTOTAL
 9337   FORMAT(/3X,'TOTAL',31X,6F8.0)
      TTWLK=0.0
      TTBUS=0.0
      TTBIK=0.0
      TTPNR=0.0
      TTOTAL=0.0
      DENOM=0.0
      WRITE(26,9212)
 9212 FORMAT(//,30X,'R E P O R T   3A',/,
     *          20X,
     *          'SUMMARIZE COMMUTER RAIL STATION EGRESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','   BUS  ','  BIKE  ',' DRIVE  ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' EGRESS ',' EGRESS ',' EGRESS ',' EGRESS ','  TOTAL '/
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------','------  ')
      DO 1501 K=1,MAX_STATIONS
      IF(STANUM(K).NE.1) GO TO 1501
      KS=K+MAX_IZONES
      DENOM=STASUM2(K,1)+STASUM2(K,2)+STASUM2(K,3)+STASUM2(K,4)
      TTWLK=TTWLK+STASUM2(K,1)
      TTBUS=TTBUS+STASUM2(K,2)
      TTBIK=TTBIK+STASUM2(K,3)
      TTPNR=TTPNR+STASUM2(K,4)
      TTOTAL=TTOTAL+DENOM
      IF(DENOM.GT.0.1) THEN
	    WRITE(26,9213) KS,STANAME(K),
     *                  (STASUM2(K,L),L=1,4),DENOM
 9213  FORMAT(2X,I4,3X,A29,1X,5F8.0)
      END IF
 1501 CONTINUE
      WRITE(26,9337) TTWLK,TTBUS,TTBIK,TTPNR,TTOTAL
C
      WRITE(26,9310)
 9310 FORMAT(//,30X,'R E P O R T   3B',/,
     *          20X,
     *          'SUMMARIZE URBAN RAIL STATION ACCESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','  BIKE  ','   BUS  ','  PARK  ','  KISS  ',
     *          '        ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' ACCESS ',' ACCESS ',' ACCESS ','  RIDE  ','  RIDE  ',
     *          '  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------','--------',
     *          '-------  ')
C
        TTWLK=0.0
        TTBIK=0.0
	      TTBUS=0.0
	      TTPNR=0.0
	      TTKNR=0.0
	      TTOTAL=0.0
      DO 1502 K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      STASUM(K,10)=STASUM(K,6)+STASUM(K,7)+STASUM(K,8)+STASUM(K,9)+
     *             STASUM(K,20)
        TTWLK=TTWLK+STASUM(K,6)
        TTBIK=TTBIK+STASUM(K,20)
	      TTBUS=TTBUS+STASUM(K,7)
	      TTPNR=TTPNR+STASUM(K,8)
	      TTKNR=TTKNR+STASUM(K,9)
	      TTOTAL=TTOTAL+STASUM(K,10)
      IF(STASUM(K,10).GT.0.1) THEN
	WRITE(26,9211) KS,STANAME(K),STASUM(K,6),STASUM(K,20),
     *               (STASUM(K,L),L=7,10)
      END IF
 1502 CONTINUE
      WRITE(26,9337) TTWLK,TTBIK,TTBUS,TTPNR,TTKNR,TTOTAL
      TTWLK=0.0
      TTBUS=0.0
      TTBIK=0.0
      TTPNR=0.0
      TTOTAL=0.0
      DENOM=0.0
      WRITE(26,9311)
 9311 FORMAT(//,30X,'R E P O R T   3B',/,
     *          20X,
     *          'SUMMARIZE URBAN RAIL STATION EGRESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','   BUS  ','  BIKE  ',' DRIVE  ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' EGRESS ',' EGRESS ',' EGRESS ',' EGRESS ','  TOTAL '/
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------','------  ')
      DO 1506 K=1,MAX_STATIONS
      IF(STANUM(K).NE.2) GO TO 1506
      KS=K+MAX_IZONES
      DENOM=STASUM2(K,1)+STASUM2(K,2)+STASUM2(K,3)+STASUM2(K,4)
      TTWLK=TTWLK+STASUM2(K,1)
      TTBUS=TTBUS+STASUM2(K,2)
      TTBIK=TTBIK+STASUM2(K,3)
      TTPNR=TTPNR+STASUM2(K,4)
      TTOTAL=TTOTAL+DENOM
      IF(DENOM.GT.0.1) THEN
	    WRITE(26,9213) KS,STANAME(K),
     *                  (STASUM2(K,L),L=1,4),DENOM
      END IF
 1506 CONTINUE
      WRITE(26,9337) TTWLK,TTBUS,TTBIK,TTPNR,TTOTAL
C
      WRITE(26,9610)
 9610 FORMAT(//,30X,'R E P O R T   3C',/,
     *          20X,
     *          'SUMMARIZE BUS RAPID TRANSIT STATION ACCESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','  BIKE  ','   BUS  ','  PARK  ','  KISS  ',
     *          '        ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' ACCESS ',' ACCESS ',' ACCESS ','  RIDE  ','  RIDE  ',
     *          '  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------','--------',
     *          '-------  ')
C
        TTWLK=0.0
        TTBIK=0.0
	      TTBUS=0.0
	      TTPNR=0.0
	      TTKNR=0.0
	      TTOTAL=0.0
      DO K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      STASUM(K,24)=STASUM(K,15)+STASUM(K,16)+STASUM(K,17)+STASUM(K,18)+
     *             STASUM(K,23)
        TTWLK=TTWLK+STASUM(K,15)
        TTBIK=TTBIK+STASUM(K,23)
	      TTBUS=TTBUS+STASUM(K,16)
	      TTPNR=TTPNR+STASUM(K,17)
	      TTKNR=TTKNR+STASUM(K,18)
	      TTOTAL=TTOTAL+STASUM(K,24)
      IF(STASUM(K,24).GT.0.1) THEN
	WRITE(26,9211) KS,STANAME(K),STASUM(K,15),STASUM(K,23),
     *               (STASUM(K,L),L=16,18),STASUM(K,24)
      END IF
      END DO
      WRITE(26,9337) TTWLK,TTBIK,TTBUS,TTPNR,TTKNR,TTOTAL
      TTWLK=0.0
      TTBUS=0.0
      TTBIK=0.0
      TTPNR=0.0
      TTOTAL=0.0
      DENOM=0.0
      WRITE(26,9611)
 9611 FORMAT(//,30X,'R E P O R T   3C',/,
     *          20X,
     *          'SUMMARIZE BUS RAPID TRANSIT STATION EGRESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','   BUS  ','  BIKE  ',' DRIVE  ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' EGRESS ',' EGRESS ',' EGRESS ',' EGRESS ','  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------','------  ')
      DO K=1,MAX_STATIONS
      IF(STANUM(K).NE.5) CYCLE
      KS=K+MAX_IZONES
      DENOM=STASUM2(K,1)+STASUM2(K,2)+STASUM2(K,3)+STASUM2(K,4)
      TTWLK=TTWLK+STASUM2(K,1)
      TTBUS=TTBUS+STASUM2(K,2)
      TTBIK=TTBIK+STASUM2(K,3)
      TTPNR=TTPNR+STASUM2(K,4)
      TTOTAL=TTOTAL+DENOM
      IF(DENOM.GT.0.1) THEN
	    WRITE(26,9213) KS,STANAME(K),
     *                  (STASUM2(K,L),L=1,4),DENOM
      END IF
      END DO
      WRITE(26,9337) TTWLK,TTBUS,TTBIK,TTPNR,TTOTAL
      TBRTEGR=TTBUS/TTOTAL
C
      WRITE(26,9612)
 9612 FORMAT(//,30X,'R E P O R T   3D',/,
     *          20X,
     *          'SUMMARIZE BLENDED STATION ACCESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','  BIKE  ','   BUS  ','  PARK  ','  KISS  ',
     *          '        ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' ACCESS ',' ACCESS ',' ACCESS ','  RIDE  ','  RIDE  ',
     *          '  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------','--------',
     *          '-------  ')
C
        TTWLK=0.0
        TTBIK=0.0
	      TTBUS=0.0
	      TTPNR=0.0
	      TTKNR=0.0
	      TTOTAL=0.0
	      DENOM=0.0
      DO K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      DENOM=STASUM4(K,1)+STASUM4(K,2)+STASUM4(K,3)+STASUM4(K,4)+
     *             STASUM4(K,5)
        TTWLK=TTWLK+STASUM4(K,1)
        TTBIK=TTBIK+STASUM4(K,5)
	      TTBUS=TTBUS+STASUM4(K,2)
	      TTPNR=TTPNR+STASUM4(K,3)
	      TTKNR=TTKNR+STASUM4(K,4)
	      TTOTAL=TTOTAL+DENOM
        IF(DENOM.GT.0.1) THEN
	      WRITE(26,9211) KS,STANAME(K),STASUM4(K,1),STASUM4(K,5),
     *               (STASUM4(K,L),L=2,4),DENOM
      END IF
      END DO
      WRITE(26,9337) TTWLK,TTBIK,TTBUS,TTPNR,TTKNR,TTOTAL
      TTWLK=0.0
      TTBUS=0.0
      TTBIK=0.0
      TTPNR=0.0
      TTOTAL=0.0
      DENOM=0.0
      WRITE(26,9613)
 9613 FORMAT(//,30X,'R E P O R T   3D',/,
     *          20X,
     *          'SUMMARIZE BLENDED STATION EGRESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','   BUS  ','  BIKE  ',' DRIVE  ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' EGRESS ',' EGRESS ',' EGRESS ',' EGRESS ','  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------','------  ')
      DO K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      DENOM=STASUM5(K,1)+STASUM5(K,2)+STASUM5(K,3)+STASUM5(K,4)
      TTWLK=TTWLK+STASUM5(K,1)
      TTBUS=TTBUS+STASUM5(K,2)
      TTBIK=TTBIK+STASUM5(K,3)
      TTPNR=TTPNR+STASUM5(K,4)
      TTOTAL=TTOTAL+DENOM
      IF(DENOM.GT.0.1) THEN
	    WRITE(26,9213) KS,STANAME(K),
     *                  (STASUM5(K,L),L=1,4),DENOM
      END IF
      END DO
      WRITE(26,9337) TTWLK,TTBUS,TTBIK,TTPNR,TTOTAL
C
      DO M=1,6
      WRITE(26,9712) NAME(FRMODE(M)),NAME(TOMODE(M))
 9712 FORMAT(//,30X,'R E P O R T   3F',/,
     *          20X,
     *          'SUMMARIZE  ',A13,' TO ',A13,' ACCESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','  BIKE  ','   BUS  ','  PARK  ','  KISS  ',
     *          '        ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' ACCESS ',' ACCESS ',' ACCESS ','  RIDE  ','  RIDE  ',
     *          '  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------','--------',
     *          '-------  ')
C
        TTWLK=0.0
        TTBIK=0.0
	      TTBUS=0.0
	      TTPNR=0.0
	      TTKNR=0.0
	      TTOTAL=0.0
	      DENOM=0.0
      DO K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      DENOM=STASUM6(K,1,M)+STASUM6(K,2,M)+STASUM6(K,3,M)+STASUM6(K,4,M)+
     *             STASUM6(K,5,M)
        TTWLK=TTWLK+STASUM6(K,1,M)
        TTBIK=TTBIK+STASUM6(K,5,M)
	      TTBUS=TTBUS+STASUM6(K,2,M)
	      TTPNR=TTPNR+STASUM6(K,3,M)
	      TTKNR=TTKNR+STASUM6(K,4,M)
	      TTOTAL=TTOTAL+DENOM
        IF(DENOM.GT.0.1) THEN
	      WRITE(26,9211) KS,STANAME(K),STASUM6(K,1,M),STASUM6(K,5,M),
     *               (STASUM6(K,L,M),L=2,4),DENOM
      END IF
      END DO
      WRITE(26,9337) TTWLK,TTBIK,TTBUS,TTPNR,TTKNR,TTOTAL
      TTWLK=0.0
      TTBUS=0.0
      TTBIK=0.0
      TTPNR=0.0
      TTOTAL=0.0
      DENOM=0.0
      WRITE(26,9713) NAME(FRMODE(M)),NAME(TOMODE(M))
 9713 FORMAT(//,30X,'R E P O R T   3F',/,
     *          20X,
     *      'SUMMARIZE  ',A13,' TO ',A13,' EGRESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ',' TRANSIT','  BIKE  ',' DRIVE  ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' EGRESS ',' EGRESS ',' EGRESS ',' EGRESS ','  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------','------  ')
      DO K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      DENOM=STASUM7(K,1,M)+STASUM7(K,2,M)+STASUM7(K,3,M)+STASUM7(K,4,M)
      TTWLK=TTWLK+STASUM7(K,1,M)
      TTBUS=TTBUS+STASUM7(K,2,M)
      TTBIK=TTBIK+STASUM7(K,3,M)
      TTPNR=TTPNR+STASUM7(K,4,M)
      TTOTAL=TTOTAL+DENOM
      IF(DENOM.GT.0.1) THEN
	    WRITE(26,9213) KS,STANAME(K),
     *                  (STASUM7(K,L,M),L=1,4),DENOM
      END IF
      END DO
      WRITE(26,9337) TTWLK,TTBUS,TTBIK,TTPNR,TTOTAL
      END DO
      END IF
C
C    FIXED GUIDEWAY MODE-TO-MODE BLENDED VOLUMES
C
      TTWLK=STAVOL(1,1)+STAVOL(1,2)+STAVOL(1,5)
      TTBUS=STAVOL(2,1)+STAVOL(2,2)+STAVOL(2,5)
      TTPNR=STAVOL(5,1)+STAVOL(5,2)+STAVOL(5,5)
      PNRTRP(1)=STAVOL(1,1)+STAVOL(2,1)+STAVOL(5,1)
      PNRTRP(2)=STAVOL(1,2)+STAVOL(2,2)+STAVOL(5,2)
      PNRTRP(3)=STAVOL(1,5)+STAVOL(2,5)+STAVOL(5,5)
      PNRTRP(4)=PNRTRP(1)+PNRTRP(2)+PNRTRP(3)
      WRITE(26,9614) STAVOL(1,1),STAVOL(1,2),STAVOL(1,5),TTWLK,
     *               STAVOL(2,1),STAVOL(2,2),STAVOL(2,5),TTBUS,
     *               STAVOL(5,1),STAVOL(5,2),STAVOL(5,5),TTPNR,
     *               PNRTRP(1),PNRTRP(2),PNRTRP(3),PNRTRP(4)
 9614 FORMAT(//,30X,'R E P O R T   3E',/,
     *          20X,
     *          'SUMMARIZE BLENDED MODE-TO-MODE VOLUMES',//
     *      17X,'COMMUTER   URBAN    BUS RAPID'/
     *      17X,'  RAIL      RAIL     TRANSIT   TOTAL'/
     *      17X,'--------  --------  -------- --------'/
     *       1X,'COMMUTER RAIL',4(2X,F8.0)/
     *       1X,'   URBAN RAIL',4(2X,F8.0)/
     *       1X,'BUS RAPID TRN',4(2X,F8.0)//
     *       1X,'   TOTAL     ',4(2X,F8.0)/)

C
C    OUTPUT URBAN EGRESS VOLUMES, IF REQUESED
C
      IF(NHBSTA.NE.'   ') THEN
      OPEN(71,FILE=NHBSTA,STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(71,9342)
 9342 FORMAT('URBAN RAIL STATION ACCESS VOLUMES',/,
     *       'STATION',/,
     *       'NUMBER,WALK,BIKE,BUS,PARK_RIDE,KISS_RIDE,CR,TOTAL',
     *       ',ZONE,LOGSUM')
      DO 1518 K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      IF(STASUM(K,10).GT.0.1) THEN
      WRITE(71,9346) KS,STASUM(K,6),STASUM(K,20),STASUM(K,7),
     *               STASUM(K,8),STASUM(K,9),STASUM(K,10),
     *               CLOSEZONE(K),CLOSEDIST(K)
 9346 FORMAT(I4,6(',',F8.1),',',I4,',',F6.2)
      END IF
 1518 CONTINUE
      WRITE(71,9347)
 9347 FORMAT('9999,0,0,0,0,0,0,0,0,0')
      WRITE(71,9341)
 9341 FORMAT('URBAN RAIL STATION EGRESS VOLUMES',/,
     *       4(',mktseg1'),4(',mktseg2'),4(',mktseg3'),
     *       4(',mktseg4'),4(',mktseg5')/
     *       'STATION',5(',WALK,BUS,CR,'),/,
     *       'NUMBER',5(',EGRESS,EGRESS,EGRESS,TOTAL'))
      DO 1516 K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      DO 1517 C=1,NCATS
      STASUM3(K,7,C)=STASUM3(K,5,C)+STASUM3(K,6,C)+STASUM3(K,1,C)+
     *               STASUM3(K,2,C)
 1517 CONTINUE
      IF((STASUM3(K,7,1)+STASUM3(K,7,2)+STASUM3(K,7,3)).GT.0.1) THEN
	    WRITE(71,9343) KS,
     * ((STASUM3(K,5,C),STASUM3(K,6,C),STASUM3(K,2,C),
     *   STASUM3(K,7,C)),C=1,NCATS)
 9343 FORMAT(I4,20(',',F8.1))
      END IF
 1516 CONTINUE
      END IF
C
C
      IF(TWYSK) THEN
      WRITE(26,9231)
 9231 FORMAT(//,30X,'R E P O R T  3C',/,
     *          20X,
     *         'SUMMARIZE TRANSITWAY BUS STATION ACCESS VOLUMES',//,
     *      1X,'STATION','                                      ',
     *         '  WALK   ','      BIKE  ','   DRIVE    ',
     *         '    TOTAL    ',/,
     *      1X,'------------------------------------------------',
     *         '---------','--------','------------','--------------')
      TTWLK=0.0
      TTBUS=0.0
      TTDRV=0.0
      TTOTAL=0.0
	    DO K=1,MAX_STATIONS
	    KS=K+MAX_IZONES
	    TEMP=STASUM(K,12)+STASUM(K,11)+STASUM(K,21)
	    IF(TEMP.GT.0.1) THEN
	    TTWLK=TTWLK+STASUM(K,11)
	    TTBUS=TTBUS+STASUM(K,21)
	    TTDRV=TTDRV+STASUM(K,12)
	    TTOTAL=TTOTAL+TEMP
	    WRITE(26,9233) KS,STANAME(K),STASUM(K,11),
     *                 STASUM(K,21),STASUM(K,12),TEMP
 9233 FORMAT(2X,I4,3X,A29,1X,4(4X,F8.0))
      ENDIF
      END DO
      TEMP=MISTRP(1)+MISTRP(2)+MISTRP(3)
      TTWLK=TTWLK+MISTRP(1)
      TTBUS=TTBUS+MISTRP(2)
      TTDRV=TTDRV+MISTRP(3)
      TTOTAL=TTOTAL+TEMP
      WRITE(26,9338) (MISTRP(K),K=1,3),TEMP
 9338 FORMAT(9X,'MISSING',23X,4(4X,F8.0))
      WRITE(26,9336) TTWLK,TTBUS,TTDRV,TTOTAL
      END IF
C
      WRITE(26,9331)
 9331 FORMAT(//,30X,'R E P O R T  3D',/,
     *          20X,
     *         'SUMMARIZE EXPRESS BUS STATION ACCESS VOLUMES',//,
     *      1X,'STATION','                                      ',
     *         '  WALK   ','      BIKE  ','   DRIVE    ',
     *         '    TOTAL    ',/,
     *      1X,'------------------------------------------------',
     *         '---------','--------','------------','--------------')
        TTWLK=0.0
        TTBUS=0.0
        TTDRV=0.0
        TTOTAL=0.0
	DO 1504,K=1,MAX_STATIONS
	KS=K+MAX_IZONES
	TEMP=STASUM(K,14)+STASUM(K,13)+STASUM(K,22)
	IF(TEMP.GT.0.1) THEN
	TTWLK=TTWLK+STASUM(K,13)
	TTBUS=TTBUS+STASUM(K,22)
	TTDRV=TTDRV+STASUM(K,14)
	TTOTAL=TTOTAL+TEMP
	WRITE(26,9333) KS,STANAME(K),STASUM(K,13),
     * STASUM(K,22),STASUM(K,14),TEMP
 9333 FORMAT(2X,I4,3X,A29,1X,4(4X,F8.0))
      ENDIF
 1504 CONTINUE
      TEMP=MISTRP(4)+MISTRP(5)+MISTRP(6)
      TTWLK=TTWLK+MISTRP(4)
      TTBUS=TTBUS+MISTRP(5)
      TTDRV=TTDRV+MISTRP(6)
      TTOTAL=TTOTAL+TEMP
      WRITE(26,9338) (MISTRP(K),K=4,6),TEMP
      WRITE(26,9336) TTWLK,TTBUS,TTDRV,TTOTAL
 9336 FORMAT(/2X,'TOTAL',32X,4(4X,F8.0))
C
 1505 CONTINUE
C
C OUTPUT PERSON TRIPS BY MARKET SEGMENT
C
      IF(CALIB) THEN
      WRITE(44,9602) CITER
 9602 FORMAT( 'Calibration Iteration=,',I4,/,',,',
     *        5('PERSON TRIPS,'),5('TRANSIT TRIPS,'),,
     *        5('COMMUTER RAIL,'),5(' URBAN RAIL,'),
     *        5(' TRANSITWAY,'),5('EXPRESS BUS,'),,
     *        5('LOCAL BUS,'),5('RAPID BUS,'),5('BUS RAPID TRANSIT,')/
     *        'Fr,To', 
     *        9(',MKT_1,MKT_2,MKT_3,MKT_4,MKT_5')/
     *        45('-----'))
      DO 9600 K=1,21
      K1=(K-1)*5
      K2=K*5
      WRITE(44,9601) K1,K2,PTRIP(K,1),PTRIP(K,2),PTRIP(K,3),
     *                     PTRIP(K,4),PTRIP(K,5),
     *                     TTRIP(K,1),TTRIP(K,2),TTRIP(K,3),
     *                     TTRIP(K,4),TTRIP(K,5),
     *                     TTRIP2(K,1,1),TTRIP2(K,2,1),TTRIP2(K,3,1),
     *                     TTRIP2(K,4,1),TTRIP2(K,5,1),
     *                     TTRIP2(K,1,2),TTRIP2(K,2,2),TTRIP2(K,3,2),
     *                     TTRIP2(K,4,2),TTRIP2(K,5,2),
     *                     TTRIP2(K,1,3),TTRIP2(K,2,3),TTRIP2(K,3,3),
     *                     TTRIP2(K,4,3),TTRIP2(K,5,3),
     *                     TTRIP2(K,1,4),TTRIP2(K,2,4),TTRIP2(K,3,4),
     *                     TTRIP2(K,4,4),TTRIP2(K,5,4),
     *                     TTRIP2(K,1,5),TTRIP2(K,2,5),TTRIP2(K,3,5),
     *                     TTRIP2(K,4,5),TTRIP2(K,5,5),
     *                     TTRIP2(K,1,6),TTRIP2(K,2,6),TTRIP2(K,3,6),
     *                     TTRIP2(K,4,6),TTRIP2(K,5,6),
     *                     TTRIP2(K,1,7),TTRIP2(K,2,7),TTRIP2(K,3,7),
     *                     TTRIP2(K,4,7),TTRIP2(K,5,7) 
 9601 FORMAT(I3,',',I3,45(',',F10.1))
 9600 CONTINUE
      IF(HCALIB) GO TO 9569
C 
C SUMMARIZE COMMUTER RAIL PNR DRIVE ACCESS RATIO FREQ. DISTR.
C
      WRITE(40,9558) CITER
 9558 FORMAT(  'CALIBRATION_ITERATION=,',I4/
     *         'COMMUTER,DRIVE,DRIVE'/
     *         'RAIL,TIME,DISTANCE'/
     *         'RATIO,PNR_TRIPS,PNR_TRIPS'/
     *         3('------,'))
      WRITE(40,9653) CRPNRSUM(1001),CRPNRRAT(1001)
 9653 FORMAT('0.0+,',F10.3,',',F10.3)
      DO 9552 K=1,1000
      TEMP=FLOAT(K)/10.0
      IF(CRPNRSUM(K).GT.0.001.OR.CRPNRRAT(K).GT.0.001) 
     *      WRITE(40,9553) TEMP,CRPNRSUM(K),CRPNRRAT(K)
 9553 FORMAT(F5.1,',',F10.3,',',F10.3)
 9552 CONTINUE
      WRITE(40,9556)
 9556 FORMAT(//' ZONE,TO,ZONE,DISTANCE,FREQUENCY,DISTRIBUTION',/,
     *        ',',4('COMMUTER_RAIL,'),4('URBAN_RAIL,'),4('BRT,')/
     *       'DIST,WALK,BUS,PNR,KNR,WALK,BUS,PNR,KNR,WALK,BUS,PNR,KNR'/
     *        12('-----,'))
      DO 9554 K=1,15
      WRITE(40,9557) (K-1),(ZNEDIST(K1,K),K1=1,12)
 9557 FORMAT(I2,4(',',F8.0),8(',',F8.0))
 9554 CONTINUE
C
C SUMMARIZE DRIVE TO EXPRESS BUS AND TRANSITWAY
C DISTANCE AND TIME FROM ZONE TO STATION
C
      OPEN(38,FILE='TWYEXP.CSV',STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(38,9562)
 9562 FORMAT('ZONE,TO,STATION,TIME,AND,DISTANCE,SUMMARY'/
     *       ',,',10(',EXPBUS'),10(',TWAY')/
     *       ',,',4(',MKT1,MKT2,MKT3,MKT4,MKT5')/
     *       'FROM,-,TO',5(',DIST'),5(',TIME'),5(',DIST'),5(',TIME'))
      DO 9563 K=1,100
      WRITE(38,9564) (K-1),K,
     *               TWYEXP(K,1,1,1),TWYEXP(K,2,1,1),TWYEXP(K,3,1,1),
     *               TWYEXP(K,4,1,1),TWYEXP(K,5,1,1),
     *               TWYEXP(K,1,2,1),TWYEXP(K,2,2,1),TWYEXP(K,3,2,1),
     *               TWYEXP(K,4,2,1),TWYEXP(K,5,2,1),
     *               TWYEXP(K,1,1,2),TWYEXP(K,2,1,2),TWYEXP(K,3,1,2),
     *               TWYEXP(K,4,1,2),TWYEXP(K,5,1,2),
     *               TWYEXP(K,1,2,2),TWYEXP(K,2,2,2),TWYEXP(K,3,2,2),
     *               TWYEXP(K,4,2,2),TWYEXP(K,5,2,2)
 9564 FORMAT(I3,',-,',I3,30(',',F9.3))
 9563 CONTINUE
      TWYEXP=0.0
C
C SUMMARIZE DRIVE TO EXPRESS BUS AND TRANSITWAY
C DRIVE ACCESS RATIO
C
      OPEN(39,FILE='TWYEXP2.CSV',STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(39,9565)
 9565 FORMAT('DRIVE,ACCESS,RATIO,SUMMARY'/
     *       5(',EXPBUS'),5(',TWAY')/
     *       'RATIO',2(',MKT1,MKT2,MKT3,MKT4,MKT5'))
      DO 9566 K=1,100
      TEMP=FLOAT(K)/10.0
      WRITE(39,9567) TEMP,
     *               TWYEXP2(K,1,1),TWYEXP2(K,2,1),TWYEXP2(K,3,1),
     *               TWYEXP2(K,4,1),TWYEXP2(K,5,1),
     *               TWYEXP2(K,1,2),TWYEXP2(K,2,2),TWYEXP2(K,3,2),
     *               TWYEXP2(K,4,2),TWYEXP2(K,5,2)
 9567 FORMAT(F4.1,15(',',F9.3))
 9566 CONTINUE
      TWYEXP2=0.0
C
C SUMMARIZE WALK TO EXPRESS BUS IVT RATIO
C
      DO K=1,12
      TEMP=FLOAT(K-1)/10.0
      IF(WEXPTLF(K).GT.0) THEN
      WRITE(98,9568)  TEMP,WEXPTLF(K)
 9568 FORMAT(F4.1,',',F8.2)
      END IF
      END DO
      END IF
C
      IF((.NOT.SPEVENT).AND.(.NOT.LAX)) THEN
      WRITE(26,3915)
 3915 FORMAT(//20X,'          R E P O R T  10'/
     *         20X,'   INTRAZONAL FORECASTING RESULTS'/
     *        ' MARKET  DRIVE        2         3        4+'/
     *        ' SEGMENT ALONE     PERSON    PERSON    PERSON     WALK',
     *        '     BICYCLE  E-SCOOTER   TOTAL'/
     *        ' -----  --------  --------  --------  --------  --------',
     *        '  --------  --------  --------')
      DO K=1,NCATS
      WRITE(26,3914) K,(INTRATRP(K1,K),K1=1,6),INTRATRP(8,K),
     *                  INTRATRP(7,K)
 3914 FORMAT(3X,I1,3X,1X,F8.1,7(2X,F8.0))
      DO K1=1,8
      INTRATRP(K1,6)=INTRATRP(K1,6)+INTRATRP(K1,K)
      END DO
      END DO
      WRITE(26,3916) (INTRATRP(K1,6),K1=1,6),INTRATRP(8,6),
     *                INTRATRP(7,6)
 3916 FORMAT(/1X,'TOTAL',1X,F9.1,7(2X,F8.0))
      END IF
C
C SUMMARIZE TOLL FACILITY USAGE
C
      IF(ITER.EQ.0.AND.(.NOT.SPEVENT).AND.CALIB) THEN
      OPEN(106,FILE='TOLLSUM.CSV',STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(106,9228)
 9228 FORMAT(',DRIVE,2_PERSON,2_PERSON,2_PERSON,',
     *       '3_PERSON,3_PERSON,3_PERSON,',
     *       '4_PERSON,4_PERSON,4_PERSON,'/
     *       'HOT_FACILITY,ALONE_TOLL,HOV,TOLL_HOV,TOLL,',
     *       'HOV,TOLL_HOV,TOLL,HOV,TOLL_HOV,TOLL')
      OPEN(107,FILE='TOLLTLF.CSV',STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(107,9232)
 9232 FORMAT(',DRIVE,2_PERSON,2_PERSON,2_PERSON,',
     *       '3_PERSON,3_PERSON,3_PERSON,',
     *       '4_PERSON,4_PERSON,4_PERSON,',
     *       '2_PERSON,3_PERSON,4_PERSON'/
     *       'HOT_FACILITY,ALONE_TOLL,HOV,TOLL(HOV),TOLL,',
     *       'HOV,TOLL(HOV),TOLL,HOV,TOLL(HOV),TOLL,',
     *       'HOV(TOLL),HOV(TOLL),HOV(TOLL)')
      OPEN(109,FILE='TOLLSAV.CSV',STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(109,9239)
 9239 FORMAT(',DRIVE,2_PERSON,2_PERSON,',
     *       '3_PERSON,3_PERSON,',
     *       '4_PERSON,4_PERSON,'/
     *       'HOT_FACILITY,ALONE_TOLL,TOLL(HOV),TOLL,',
     *       'TOLL(HOV),TOLL,TOLL(HOV),TOLL')
      END IF
      IF(CALIB) THEN
      WRITE(106,9230) CITER
      WRITE(107,9230) CITER
 9230 FORMAT('CALIBRATION,ITERATION=,',I3)
      DO K=1,8
      WRITE(106,9227) TLNAME(K),(TOLLSUM(K,K1),K1=1,10)
 9227 FORMAT(A18,10(',',F8.1))
       DO K2=1,10
       TOLLSUM(9,K2)=TOLLSUM(9,K2)+TOLLSUM(K,K2)
       END DO
      END DO
      WRITE(106,9227) TLNAME(9),(TOLLSUM(9,K1),K1=1,10)
      TOLLSUM=0.0
      DO K2=1,8
      WRITE(107,9235) TLNAME(K2)
 9235 FORMAT(A18)
      DO K=1,102
        DO K3=1,13
        TOLLTLF(K,9,K3)=TOLLTLF(K,9,K3)+TOLLTLF(K,K2,K3)
        END DO
      WRITE(107,9234) (K-1),(TOLLTLF(K,K2,K1),K1=1,13)
 9234 FORMAT(I3,13(',',F8.1))
      END DO
      END DO
      WRITE(107,9235) TLNAME(9)
      DO K=1,102
      WRITE(107,9234) (K-1),(TOLLTLF(K,9,K1),K1=1,13)
      END DO
      TOLLTLF=0.0
      OPEN(108,FILE='TOLLDIST.CSV',STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(108,9237)
 9237 FORMAT('District,Name,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15',
     *       ',16,17,18,19,20,Total')
      DO K=1,20
        DO K1=1,20
        TOLLDIST(21,K1)=TOLLDIST(21,K1)+TOLLDIST(K,K1)
        TOLLDIST(K,21)=TOLLDIST(K,21)+TOLLDIST(K,K1)
        TOLLDIST(21,21)=TOLLDIST(21,21)+TOLLDIST(K,K1)
        END DO
      WRITE(108,9236) K,DNAME(K),(TOLLDIST(K,K1),K1=1,21)
 9236 FORMAT(I2,',',A10,21(',',F10.2))
      END DO
      WRITE(108,9238) DNAME(21),(TOLLDIST(21,K1),K1=1,21)
 9238 FORMAT(',',A10,21(',',F10.2))
      TOLLDIST=0.0
C
C...  TIME SAVINGS TLF OUTPUT
C
      DO K=1,8
      WRITE(109,9241) TLNAME(K)
 9241 FORMAT(A18)
      DO K1=1,30
      WRITE(109,9240) (K1-1),TOLLSAV(K1,K,1),TOLLSAV(K1,K,3),
     *                TOLLSAV(K1,K,4),
     *                TOLLSAV(K1,K,6),TOLLSAV(K1,K,7),TOLLSAV(K1,K,9),
     *                TOLLSAV(K1,K,10)
 9240 FORMAT(I2,7(',',F10.2))
        DO K2=1,10
        TOLLSAV(K1,9,K2)=TOLLSAV(K1,9,K2)+TOLLSAV(K1,K,K2)
        END DO
      END DO
      END DO
      WRITE(109,9241) TLNAME(9)
      DO K1=1,30
      WRITE(109,9240) (K1-1),TOLLSAV(K1,9,1),TOLLSAV(K1,9,3),
     *                TOLLSAV(K1,9,4),
     *                TOLLSAV(K1,9,6),TOLLSAV(K1,9,7),TOLLSAV(K1,9,9),
     *                TOLLSAV(K1,9,10)
      END DO
      END IF
C
C SUMMARIZE NON-MOTORIZED FREQUENCY DISTRIBUTIONS
C
      IF(CALIB) THEN
      WRITE(45,9773)
 9773 FORMAT(6(',WALK'),6(',BIKE')/
     *       'DIST,1,2,3,4,5,TOT,1,2,3,4,5,TOT')
      DO K=1,150
      TOTHH=(K-1)/10.0
      WRITE(45,9772) TOTHH,(NMOTLF(K,K1,1),K1=1,6),
     *               (NMOTLF(K,K2,2),K2=1,6)
 9772 FORMAT(F4.1,12(',',F10.2))
      END DO
      NMOTLF=0.0
      END IF
C
C SUMMARIZE TRANSIT TRIPS BY DISTRICT
C
      IF(CALIB) THEN
       IF(ITER.EQ.0) THEN
       OPEN(110,FILE='TRNDIST.CSV',STATUS='UNKNOWN',
     *          FORM='FORMATTED')
       OPEN(117,FILE='TRNDIST_MODE.CSV',STATUS='UNKNOWN',
     *          FORM='FORMATTED')
       END IF
      WRITE(110,9230) CITER
      WRITE(117,9230) CITER
      DO K2=1,29
      WRITE(110,9243) TNAME(K2),DNAME
 9243 FORMAT(A18/'District',21(',',A10))
       DO K=1,20
        DO K1=1,20
        TRNDIST(21,K1,K2)=TRNDIST(21,K1,K2)+TRNDIST(K,K1,K2)
        TRNDIST(K,21,K2)=TRNDIST(K,21,K2)+TRNDIST(K,K1,K2)
        TRNDIST(21,21,K2)=TRNDIST(21,21,K2)+TRNDIST(K,K1,K2)
        END DO
        END DO
       DO K=1,20
       WRITE(110,9242) DNAME(K),(TRNDIST(K,K1,K2),K1=1,21)
 9242  FORMAT(A10,21(',',F10.2))
       END DO
      WRITE(110,9244) DNAME(21),(TRNDIST(21,K1,K2),K1=1,21)
 9244 FORMAT(A10,21(',',F10.2))
      END DO
C.. 
      DO K=1,20
      DO K1=1,20
      DO K2=1,5
      XDISTRN(K,K1,16)=XDISTRN(K,K1,16)+XDISTRN(K,K1,K2)
      XDISTRN(K,K1,17)=XDISTRN(K,K1,17)+XDISTRN(K,K1,(K2+5))
      XDISTRN(K,K1,18)=XDISTRN(K,K1,18)+XDISTRN(K,K1,(K2+10)) 
      XDISTRN(K,K1,24)=XDISTRN(K,K1,24)+XDISTRN(K,K1,(K2+18))
      XDISTRN(K,K1,30)=XDISTRN(K,K1,30)+XDISTRN(K,K1,(K2+24))
      XDISTRN(K,K1,36)=XDISTRN(K,K1,36)+XDISTRN(K,K1,(K2+30))
      END DO
      END DO
      END DO
      DO K2=1,36
      WRITE(110,9243) XTNAME(K2),DNAME
       DO K=1,20
        DO K1=1,20
        XDISTRN(21,K1,K2)=XDISTRN(21,K1,K2)+XDISTRN(K,K1,K2)
        XDISTRN(K,21,K2)=XDISTRN(K,21,K2)+XDISTRN(K,K1,K2)
        XDISTRN(21,21,K2)=XDISTRN(21,21,K2)+XDISTRN(K,K1,K2)
        END DO
        END DO
       DO K=1,20
       WRITE(110,9242) DNAME(K),(XDISTRN(K,K1,K2),K1=1,21)
       END DO
      WRITE(110,9244) DNAME(21),(XDISTRN(21,K1,K2),K1=1,21)
      END DO  
C.....MODE SUMMARIES
      DO K=1,21
      DO K1=1,21
      YDISTRN(K,K1,1)=TRNDIST(K,K1,24)+XDISTRN(K,K1,16)+
     *                XDISTRN(K,K1,17)
      YDISTRN(K,K1,2)=TRNDIST(K,K1,25)-XDISTRN(K,K1,16)+
     *                XDISTRN(K,K1,18)
      YDISTRN(K,K1,3)=TRNDIST(K,K1,23)-XDISTRN(K,K1,17)-
     *                XDISTRN(K,K1,18)
      END DO
      END DO 
      DO K2=19,22
      WRITE(117,9243) TNAME(K2),DNAME
       DO K=1,20
       WRITE(117,9242) DNAME(K),(TRNDIST(K,K1,K2),K1=1,21)
       END DO
      WRITE(117,9244) DNAME(21),(TRNDIST(21,K1,K2),K1=1,21)
      END DO
      DO K2=1,3
      WRITE(117,9243) YTNAME(K2),DNAME
       DO K=1,20
       WRITE(117,9242) DNAME(K),(YDISTRN(K,K1,K2),K1=1,21)
       END DO
      WRITE(117,9244) DNAME(21),(YDISTRN(21,K1,K2),K1=1,21)
      END DO       
      WRITE(117,9243) TNAME(26),DNAME
       DO K=1,20
       WRITE(117,9242) DNAME(K),(TRNDIST(K,K1,26),K1=1,21)
       END DO
      WRITE(117,9244) DNAME(21),(TRNDIST(21,K1,26),K1=1,21)
      TRNDIST=0.0
      XDISTRN=0.0
      YDISTRN=0.0
      END IF
C
C SUMMARIZE STATION-TO-STATION TRANSFER ACTIVITY
C
      IF(.NOT.LAXTRN) THEN
      WRITE(26,9435)
 9435 FORMAT(///20X,'          R E P O R T  11'/
     *       20X,'   STATION-TO-STATION TRANSFER SUMMARY'/
     *       20X,' ----------------------------------------'/
     *       '  FROM         STATION            TO           STATION'/
     *       ' STATION         NAME           STATION          NAME',
     *       '           TRIPS'/
     *       ' -------  --------------------  -------  ',
     *       '--------------------  ---------')
      DO SC=1,MAX_STATIONS
      DO SC2=1,MAX_STATIONS
      IF(XFERMATX(SC,SC2).GT.0) THEN
      WRITE(26,9434) (SC+MAX_IZONES),STANAME(SC),
     *               (SC2+MAX_IZONES),STANAME(SC2),
     *               XFERMATX(SC,SC2)
      WRITE(130,9436) (SC+MAX_IZONES),STANAME(SC),
     *               (SC2+MAX_IZONES),STANAME(SC2),
     *               XFERMATX(SC,SC2)
 9434 FORMAT(2X,I4,4X,A20,3X,I4,4X,A20,2X,F10.0)
 9436 FORMAT(I4,',',A37,',',I4,',',A37,',',F10.0)
      END IF
      END DO
      END DO
      END IF
      XFERMATX=0.0
C
C SUMMARIZE TAXI TRIPS BY COUNTY
C
      IF(TAXIMODE) THEN
      WRITE(26,9437)
 9437 FORMAT(///20X,'          R E P O R T  12'/
     *       20X,'      TAXI TRIPS BY COUNTY OF ORIGIN     '/
     *       20X,' ----------------------------------------'/)
      DO T=1,6
      WRITE(26,9438) COUNTY(T),TAXISUM(T)
 9438 FORMAT(10X,A12,2X,F10.0)
      END DO
      END IF
C
C SUMMARIZE VMT ESTIMATES
C
      DO K=1,48
      DO K1=1,4
      VMT(K,5)=VMT(K,5)+VMT(K,K1)
      END DO
      END DO
      WRITE(26,4401)((VMT(K,K1),K1=1,5),K=1,48)
 4401 FORMAT(///20X,'          R E P O R T  13'/
     *       20X,'      VMT COMPUTATIONS BY GEOGRAPHY      '/
     *       20X,' ----------------------------------------'/
     *       2X,'  TRIP CATEGORY               LA-LA       LA-OTH    ',
     *          '  OTH-LA     OTH-OTH      TOTAL'/
     *       2X,' -------------------------  ----------  ----------  ',
     *          '----------  ----------  ----------'/
     *       35X,'  CONVENTIONAL VEHICLES'/
     *       2X,' DRIVE ALONE NON-TOLL     ',5(2X,F10.0)/
     *       2X,' DRIVE ALONE     TOLL     ',5(2X,F10.0)/
     *       2X,' 2 PERSON NON-TOLL     HOV',5(2X,F10.0)/
     *       2X,' 2 PERSON NON-TOLL NON-HOV',5(2X,F10.0)/
     *       2X,' 2 PERSON     TOLL     HOV',5(2X,F10.0)/
     *       2X,' 2 PERSON     TOLL NON-HOV',5(2X,F10.0)/
     *       2X,' 3 PERSON NON-TOLL     HOV',5(2X,F10.0)/
     *       2X,' 3 PERSON NON-TOLL NON-HOV',5(2X,F10.0)/
     *       2X,' 3 PERSON     TOLL     HOV',5(2X,F10.0)/
     *       2X,' 3 PERSON     TOLL NON-HOV',5(2X,F10.0)/
     *       2X,' 4+PERSON NON-TOLL     HOV',5(2X,F10.0)/
     *       2X,' 4+PERSON NON-TOLL NON-HOV',5(2X,F10.0)/
     *       2X,' 4+PERSON     TOLL     HOV',5(2X,F10.0)/
     *       2X,' 4+PERSON     TOLL NON-HOV',5(2X,F10.0)/
     *       35X,'  AUTOMATED VEHICLES'/
     *       2X,' DRIVE ALONE NON-TOLL     ',5(2X,F10.0)/
     *       2X,' DRIVE ALONE     TOLL     ',5(2X,F10.0)/
     *       2X,' 2 PERSON NON-TOLL     HOV',5(2X,F10.0)/
     *       2X,' 2 PERSON NON-TOLL NON-HOV',5(2X,F10.0)/
     *       2X,' 2 PERSON     TOLL     HOV',5(2X,F10.0)/
     *       2X,' 2 PERSON     TOLL NON-HOV',5(2X,F10.0)/
     *       2X,' 3 PERSON NON-TOLL     HOV',5(2X,F10.0)/
     *       2X,' 3 PERSON NON-TOLL NON-HOV',5(2X,F10.0)/
     *       2X,' 3 PERSON     TOLL     HOV',5(2X,F10.0)/
     *       2X,' 3 PERSON     TOLL NON-HOV',5(2X,F10.0)/
     *       2X,' 4+PERSON NON-TOLL     HOV',5(2X,F10.0)/
     *       2X,' 4+PERSON NON-TOLL NON-HOV',5(2X,F10.0)/
     *       2X,' 4+PERSON     TOLL     HOV',5(2X,F10.0)/
     *       2X,' 4+PERSON     TOLL NON-HOV',5(2X,F10.0)/
     *       35X,'  RETURN HOME VEHICLES'/
     *       2X,' DRIVE ALONE              ',5(2X,F10.0)/
     *       2X,' 2 PERSON                 ',5(2X,F10.0)/
     *       2X,' 3 PERSON                 ',5(2X,F10.0)/
     *       2X,' 4+PERSON                 ',5(2X,F10.0)/
     *       35X,'  ALTERNATIVE PARKING VEHICLES'/
     *       2X,' DRIVE ALONE              ',5(2X,F10.0)/
     *       2X,' 2 PERSON                 ',5(2X,F10.0)/
     *       2X,' 3 PERSON                 ',5(2X,F10.0)/
     *       2X,' 4+PERSON                 ',5(2X,F10.0)/
     *       35X,'  MOBILITY AS A SERVICE'/
     *       2X,' UBER - CONVENTIONAL      ',5(2X,F10.0)/
     *       2X,' UBER - AUTOMATED         ',5(2X,F10.0)/
     *       2X,' TAXI                     ',5(2X,F10.0)/
     *       35X,'  COMMUTER RAIL'/
     *       2X,' PARK-AND-RIDE            ',5(2X,F10.0)/
     *       2X,' KISS-AND-RIDE            ',5(2X,F10.0)/
     *       2X,' TNC(UBER)                ',5(2X,F10.0)/
     *       35X,'  URBAN RAIL'/
     *       2X,' PARK-AND-RIDE            ',5(2X,F10.0)/
     *       2X,' KISS-AND-RIDE            ',5(2X,F10.0)/
     *       2X,' TNC(UBER)                ',5(2X,F10.0)/
     *       35X,'  BUS RAPID TRANSIT (BRT)'/
     *       2X,' PARK-AND-RIDE            ',5(2X,F10.0)/
     *       2X,' KISS-AND-RIDE            ',5(2X,F10.0)/
     *       2X,' TNC(UBER)                ',5(2X,F10.0)/)
C
C
C SELF-CALIBRATION BIAS CONSTANT CALIBRATION
C
 9569 CONTINUE
      IF(CALIB) THEN
	 CITER=CITER+1
          IF (CITER.EQ.1) THEN
          OPEN(51,FILE='CONSTANT.OUT',STATUS='UNKNOWN',FORM='FORMATTED')
          END IF
	    CALL SCALIB(CITER,TESUM,CALSUM,PTRIP,TTRIP,
     *            CBDTRAN,INTRATRP,XTESUM,STAVOL,TBRTEGR,CBDTRCR)
       DO K=1,11
       TEMP=FLOAT(K-1)/10.0
       WRITE(111,8106) TEMP,WTRAT(K)
 8106  FORMAT(F4.2,',',F10.2)
       END DO
      IF (CITER.GT.MAXCALIT) GO TO 9515
      IF ((CITER.EQ.MAXCALIT).AND.(.NOT.HCALIB)) TRIPSOUT=.TRUE.
C
C COMPUTE STATION --> STATION UTILITY VALUES
C
      IF(CCODE(67)) THEN
      STASTA=0.0
      STASTA2=0.0
      XFERSTA=0.0
      CLOSE(10,STATUS='KEEP')
      CLOSE(12,STATUS='KEEP')
      CLOSE(17,STATUS='KEEP')
      CLOSE(112,STATUS='KEEP')
      CLOSE(113,STATUS='KEEP')
      CLOSE(114,STATUS='KEEP')
      CLOSE(72,STATUS='KEEP')
      CLOSE(9,STATUS='KEEP')
      call prepio(bcrsk,10)
      call prepio(bursk,12)
      call prepio(brtsk,17)
      call prepio(xferurt,112)
      call prepio(xfercmr,113)
      call prepio(xferbrt,114)
      WRITE(*,9900)
      IMODE=1
      IF(.NOT.NHBDIR)
     *  CALL STATION(STASTA,IMODE,INDAPM,NHBACC,NHBUTL)
      IMODE=2
      CALL STATION(STASTA,IMODE,INDAPM,NHBACC,NHBUTL)
      IMODE=5
      IF(.NOT.NHBDIR) 
     *  CALL STATION(STASTA,IMODE,INDAPM,NHBACC,NHBUTL)
      IF(.NOT.NHBDIR) CALL STATION2(STASTA,STASTA2)
      WRITE(*,9900)
      END IF
C
C EGRESS PROBABILITY COMPUTATIONS
C
      IF(CCODE(71)) THEN
	    IMODE=5
	    CALL EGRPROB(STAZNE,IMODE,STAZNEI,INDAPM,STAEGR,STAZONE,ZHHD)            !Innovation
      END IF
      GO TO 8001
      END IF
C
C.TAXI SELF CALIBRATION FUNCTION
C
      IF(TCALIB) THEN
      CITER=CITER+1
      IF (CITER.EQ.1) THEN
      OPEN(51,FILE='CONSTANT.OUT',STATUS='UNKNOWN',FORM='FORMATTED')
      END IF    
      WRITE(26,1) CITER
      WRITE(51,1) CITER
    1 FORMAT(//25X,'Calibration Iteration #',I2,/,
     *         25X,'---------------------------',/)
      WRITE(*,1) CITER
      WRITE(26,4851)
 4851 FORMAT(/15X,'Taxi Mode Constants',/,
     *        15X,'-------------------',/,
     *  1X,'                Obs     Est    Existing           ',
     *     '    New'/
     *  1X,'  County       Value   Value   Constant Adjustment',
     *     '  Constant'/
     *  1X,'------------  ------- ------- --------- ----------',
     *     ' ---------')
      POBS=0.0
      PEST=0.0
      DENOM=0.0
      NCON=0.0
      MINVAL=-15.0
   	  DO INC=1,5
      POBS=TAXIOBS(INC)
      PEST=TAXISUM(INC)
      RCON=0.0
      IF(POBS.GT.0.0.AND.PEST.GT.0) THEN
      RCON=DLOG(POBS/PEST)
      END IF
      ECON=KTAXI(INC)
      NCON=ECON+ADJFCT*RCON
      NCON=DMAX1(NCON,MINVAL)
      IF(POBS.EQ.0.0) NCON=MINVAL
      KTAXI(INC)=NCON
      WRITE(26,4871) COUNTY(INC),POBS,PEST,ECON,RCON,NCON
 4871 FORMAT(1X,A12,1X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4881) INC,KTAXI(INC)
 4881 FORMAT('KTAXI(',I1,')=',F9.5)
      END DO 
      IF (CITER.GT.TITER) GO TO 9515  
      GO TO 8001
      END IF
C
C SUMMARIZE LAX PARKING LOT CHOICE RESULTS
C
      IF(LAX.AND.(.NOT.LAXTRN)) THEN
      PNRTRP(1)=0.0
      PNRTRP(2)=0.0
      PNRTRP(3)=0.0
      PNRTRP(4)=0.0
      PNRTRP(5)=0.0
      WRITE(26,334)
  334 FORMAT(//20X,'          R E P O R T  4'/
     *   20X,'     LAX PARKING LOT CHOICE RESULTS'//
     *       ' PARKING   TOTAL        MODE DISTRIBUTION       ',
     *       '  AVAILABLE   V/C'/
     *       '   LOT    VEHICLES    WALK     SHUTTLE   TRANSIT',
     *       '   SPACES    RATIO'/
     *       ' -------  --------  --------  --------  --------',
     *       '  --------  ------')
      DO 44 NI=1,50
      KJZ=PEQUIV(NI)
      IF(PEQUIV(NI).LE.0) GO TO 44
      IF(PRKDATA(NI,1).EQ.0) GO TO 44
      VCRATIO=0.0
      IF(AIRPASS) THEN
      PSPACES=PRKDATA(NI,3)*PRKDATA(NI,18)
      ELSE
      PSPACES=PRKDATA(NI,4)*PRKDATA(NI,17)+
     *        PRKDATA(NI,3)*PRKDATA(NI,18)
      END IF
      IF(PSPACES.GT.0) VCRATIO=LOTRIPS(NI,1)/(2*PSPACES*LAXOCC)
      SHDPRICE=0.0
      IF(VCRATIO.GT.1.0) THEN
      SHDPRICE=DLOG(1.0/VCRATIO)
      CNVRGE=.FALSE.
      END IF
      PRKDATA(NI,19)=PRKDATA(NI,19)+0.5*SHDPRICE
      WRITE(26,45) KJZ,(LOTRIPS(NI,K),K=1,4),PSPACES,VCRATIO
   45 FORMAT(I8,5(2X,F8.1),2X,F6.2)
      PNRTRP(1)=PNRTRP(1)+LOTRIPS(NI,1)
      PNRTRP(2)=PNRTRP(2)+LOTRIPS(NI,2)
      PNRTRP(3)=PNRTRP(3)+LOTRIPS(NI,3)
      PNRTRP(4)=PNRTRP(4)+LOTRIPS(NI,4)
      PNRTRP(5)=PNRTRP(5)+PSPACES
   44 CONTINUE
      VCRATIO=PNRTRP(1)/(2*PNRTRP(5))
      WRITE(26,345) (PNRTRP(K),K=1,5),VCRATIO
  345 FORMAT(/2X,'TOTAL',1X,5(2X,F8.1),2X,F6.2)
      PNRTRP(1)=0.0
      PNRTRP(2)=0.0
      PNRTRP(3)=0.0
      PNRTRP(4)=0.0
      PNRTRP(5)=0.0
      PNRTRP(6)=0.0
      PNRTRP(7)=0.0    
      PNRTRP(10)=0.0
      WRITE(26,335)
  335 FORMAT(//20X,'          R E P O R T  5'/
     *    20X,'    LAX PARKING TRANSIT CHOICE RESULTS'//
     *       ' PARKING   ATTR    TRANSIT'/
     *       '   LOT     ZONE     TRIPS    MODE'/
     *       ' -------  -------  --------  ----')
      DO 3836 NI=1,50
      KJZ=PEQUIV(NI)
      IF(KJZ.LE.0) GO TO 3836
      DO 3837 JZ=1,50
      K1=AEQUIV(JZ)
      DO 3839 K=1,7
      IF(LOTTRN(NI,JZ,K).GT.0) WRITE(26,336) KJZ,K1,LOTTRN(NI,JZ,K),K,
     *    TRNAME(K)
      PNRTRP(10)=PNRTRP(10)+LOTTRN(NI,JZ,K)
      PNRTRP(K)=PNRTRP(K)+LOTTRN(NI,JZ,K)
  336 FORMAT(I8,2X,I7,2X,F8.3,2X,I2,3X,'(',A13,')')
 3839 CONTINUE
 3837 CONTINUE
 3836 CONTINUE
      WRITE(26,3845) PNRTRP(10),(PNRTRP(K),K=1,7)
 3845 FORMAT(/19X,F8.2//,
     *  10X,'  LAX PARKING LOT CHOICE'/
     *  10X,'  PRIMARY MODE TRANSIT CHOICE RESULTS'//
     *       1X,'COMMUTER RAIL  =',F8.2/
     *       1X,'URBAN RAIL     =',F8.2/
     *       1X,'TRANSITWAY BUS =',F8.2/
     *       1X,'EXPRESS BUS    =',F8.2/
     *       1X,'RAPID BUS      =',F8.2/
     *       1X,'LOCAL BUS      =',F8.2/
     *       1X,'BUS RAPID (BRT)=',F8.2)
C
C  FLY-AWAY LOT CHOICE RESULTS
C
      PNRTRP(1)=0.0
      PNRTRP(2)=0.0
      PNRTRP(3)=0.0
      PNRTRP(4)=0.0
      PNRTRP(5)=0.0
      PNRTRP(6)=0.0
      WRITE(26,346)
  346 FORMAT(//20X,'          R E P O R T  6'/
     *   20X,'     FLY-AWAY LOT CHOICE RESULTS'//
     *       '   FLY-AWAY               TOTAL    <------------ MODE ',
     *       'DISTRIBUTION --------------->'/
     *       '   LOCATION        TAZ  PASSENGERS    WALK    TRANSIT ',
     *       '    PNR       KNR      TAXI'/
     *       ' ---------------  ----  ----------  --------  --------',
     *       '  --------  --------  --------')
      DO 347 NI=1,10
      IF(FLYADATA(NI,3).LT.1.0) GO TO 347
      KJZ=IDINT(FLYADATA(NI,1))
      WRITE(26,348) FLYNAME(NI),KJZ,FLYTRIPS(NI,6),
     *              (FLYTRIPS(NI,K),K=1,5)
  348 FORMAT(1X,A15,2X,I4,2X,F10.1,5(2X,F8.1))
      PNRTRP(1)=PNRTRP(1)+FLYTRIPS(NI,6)
      PNRTRP(2)=PNRTRP(2)+FLYTRIPS(NI,1)
      PNRTRP(3)=PNRTRP(3)+FLYTRIPS(NI,2)
      PNRTRP(4)=PNRTRP(4)+FLYTRIPS(NI,3)
      PNRTRP(5)=PNRTRP(5)+FLYTRIPS(NI,4)
      PNRTRP(6)=PNRTRP(6)+FLYTRIPS(NI,5)      
  347 CONTINUE
      WRITE(26,349) (PNRTRP(K),K=1,6)
  349 FORMAT(/1X,'TOTAL',18X,F10.1,5(2X,F8.1))
      IF(AIRPASS) WRITE(78,9379) PNRTRP(5),PNRTRP(4),PNRTRP(6),
     *               PNRTRP(3),PNRTRP(2)
 9379 FORMAT('KNR,',F8.1/
     *       'PNR,',F8.1/
     *       'TAXI,',F8.1/
     *       'TRANSIT,',F8.1/
     *       'WALK,',F8.1)
C
C  FLY-AWAY TRANSIT SUB-CHOICE RESULTS
C
      PNRTRP(1)=0.0
      PNRTRP(2)=0.0
      PNRTRP(3)=0.0
      PNRTRP(4)=0.0
      PNRTRP(5)=0.0
      PNRTRP(6)=0.0
      PNRTRP(7)=0.0
      PNRTRP(8)=0.0
      WRITE(26,386)
  386 FORMAT(//20X,'          R E P O R T  7'/
     *   20X,'     FLY-AWAY LOT TRANSIT CHOICE RESULTS'//
     *       '                                   <------------------',
     *       '---- MODE DISTRIBUTION ------------------------->'/
     *       '   FLY-AWAY               TOTAL      METRO     URBAN  ',
     *       '   TRANSIT  EXPRESS    LOCAL     RAPID'/
     *       '   LOCATION        TAZ   TRANSIT      LINK      RAIL  ',
     *       '    WAY       BUS       BUS       BUS       BRT'/
     *       ' ---------------  ----  ----------  --------  --------',
     *        '  --------  --------  --------  --------  --------'/)
      DO 387 NI=1,10
      IF(FLYADATA(NI,3).LT.1.0) GO TO 387
      KJZ=IDINT(FLYADATA(NI,1))
      WRITE(26,388) FLYNAME(NI),KJZ,FLYATRIPS(NI,8),
     *              (FLYATRIPS(NI,K),K=1,7)
  388 FORMAT(1X,A15,2X,I4,2X,F10.1,7(2X,F8.1))
      PNRTRP(1)=PNRTRP(1)+FLYATRIPS(NI,8)
      PNRTRP(2)=PNRTRP(2)+FLYATRIPS(NI,1)
      PNRTRP(3)=PNRTRP(3)+FLYATRIPS(NI,2)
      PNRTRP(4)=PNRTRP(4)+FLYATRIPS(NI,3)
      PNRTRP(5)=PNRTRP(5)+FLYATRIPS(NI,4)
      PNRTRP(6)=PNRTRP(6)+FLYATRIPS(NI,5)
      PNRTRP(7)=PNRTRP(7)+FLYATRIPS(NI,6)
      PNRTRP(8)=PNRTRP(8)+FLYATRIPS(NI,7)      
  387 CONTINUE
      WRITE(26,389) (PNRTRP(K),K=1,8)
  389 FORMAT(/1X,'TOTAL',18X,F10.1,7(2X,F8.1))
      PNRTRP(1)=0.0
      PNRTRP(2)=0.0
      PNRTRP(3)=0.0
      PNRTRP(4)=0.0
      PNRTRP(5)=0.0
      PNRTRP(6)=0.0
      PNRTRP(7)=0.0    
      PNRTRP(10)=0.0
      WRITE(26,390)
  390 FORMAT(//20X,'          R E P O R T  8'/
     *    20X,'    LAX RENTAL LOT TRANSIT CHOICE RESULTS'//
     *       ' RENTAL    ATTR    TRANSIT'/
     *       '   LOT     ZONE     TRIPS    MODE'/
     *       ' -------  -------  --------  ----')
      DO 3840 NI=1,10
      KJZ=IDINT(RNTLDATA(NI,1))
      IF(KJZ.LE.0) GO TO 3840
      DO 3847 JZ=1,50
      K1=AEQUIV(JZ)
      DO 3842 K=1,7
      IF(RNTTRN(NI,JZ,K).GT.0.001) WRITE(26,336) KJZ,K1,
     *    RNTTRN(NI,JZ,K),K,TRNAME(K)
      PNRTRP(10)=PNRTRP(10)+RNTTRN(NI,JZ,K)
      PNRTRP(K)=PNRTRP(K)+RNTTRN(NI,JZ,K)
 3842 CONTINUE
 3847 CONTINUE
 3840 CONTINUE
      WRITE(26,3849) PNRTRP(10),(PNRTRP(K),K=1,7)
 3849 FORMAT(/19X,F8.2//,
     *  10X,'  RENTAL FACILITY EGRESS MODE CHOICE'/
     *  10X,'  PRIMARY MODE TRANSIT CHOICE RESULTS'//
     *       1X,'COMMUTER RAIL  =',F8.2/
     *       1X,'URBAN RAIL     =',F8.2/
     *       1X,'TRANSITWAY BUS =',F8.2/
     *       1X,'EXPRESS BUS    =',F8.2/
     *       1X,'LOCAL BUS      =',F8.2/
     *       1X,'RAPID BUS      =',F8.2/
     *       1X,'BUS RAPID (BRT)=',F8.2)
      IF(ITFZONE.GT.0) THEN
      PNRTRP=0.0
      WRITE(26,3913)
 3913 FORMAT(//20X,'          R E P O R T  9'/
     *    20X,'    ITF FACILITY TRANSIT CHOICE RESULTS'//
     *       '   ITF     ATTR    TRANSIT'/
     *       '   ZNE     ZONE     TRIPS    MODE'/
     *       ' -------  -------  --------  ----')
      KJZ=ITFZONE
      DO 3857 JZ=1,50
      K1=AEQUIV(JZ)
      DO 3852 K=1,7
      IF(ITFTRN(JZ,K).GT.0.001) WRITE(26,336) KJZ,K1,
     *    ITFTRN(JZ,K),K,TRNAME(K)
      PNRTRP(10)=PNRTRP(10)+ITFTRN(JZ,K)
      PNRTRP(K)=PNRTRP(K)+ITFTRN(JZ,K)
 3852 CONTINUE
 3857 CONTINUE
      IF(ITFZONE2.GT.0) THEN
      KJZ=ITFZONE2
      DO JZ=1,50
      K1=AEQUIV(JZ)
      DO K=1,7
      IF(ITFTRN2(JZ,K).GT.0.001) WRITE(26,336) KJZ,K1,
     *    ITFTRN2(JZ,K),K,TRNAME(K)
      PNRTRP(10)=PNRTRP(10)+ITFTRN2(JZ,K)
      PNRTRP(K)=PNRTRP(K)+ITFTRN2(JZ,K)
      END DO
      END DO      
      END IF
      WRITE(26,3859) PNRTRP(10),(PNRTRP(K),K=1,7)
 3859 FORMAT(/19X,F8.2//,
     *  10X,'  ITF FACILITY EGRESS MODE CHOICE'/
     *  10X,'  PRIMARY MODE TRANSIT CHOICE RESULTS'//
     *       1X,'COMMUTER RAIL  =',F8.2/
     *       1X,'URBAN RAIL     =',F8.2/
     *       1X,'TRANSITWAY BUS =',F8.2/
     *       1X,'EXPRESS BUS    =',F8.2/
     *       1X,'LOCAL BUS      =',F8.2/
     *       1X,'RAPID BUS      =',F8.2/
     *       1X,'BUS RAPID (BRT)=',F8.2)
      END IF
      END IF
C
C ITERATION COUNTER FOR STATION PARKING CAPACITY RESTRAINT
C
      IF(CAPRES.AND.(.NOT.LAXTRN)) THEN
      ITER=ITER+1
      IF((ITER.LE.NITER).AND.(.NOT.CNVRGE)) THEN
      IF(LAX) THEN
      WRITE(*,8555) ITER
      WRITE(26,8555) ITER
 8555 FORMAT(/1X,'LAX Parking Lot Capacity Restraint Iteration=',I2/,
     *        1X,'-----------------------------------------------'/)      
      ELSE
      WRITE(*,8005) ITER
      WRITE(26,8005) ITER
 8005 FORMAT(/1X,'Station Capacity Restraint Iteration=',I2)
      CALL STACAP(ITER,stasum,EQUIV)
      ENDIF
      ENDIF
      ENDIF
C
C  WRITE OUT FINAL SHADOW PRICE & NUMBER OF DRIVE TRIPS PER STATION
C  FOR INPUT IN SUBSEQUENT RUNS
C
      IF((ITER.EQ.NITER).AND.(.NOT.LAX)) THEN
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
C RESET ARRAYS FOR NEXT ITERATION
C
 8001  CONTINUE
       TESUM=0.0
       XTESUM=0.0
       STASUM=0.0
       STASUM2=0.0
       STASUM3=0.0
       STASUM4=0.0
       STASUM5=0.0
       STASUM6=0.0
       STASUM7=0.0
       ZNEDIST=0.0
       FLYATRIPS=0.0
       FLYTRIPS=0.0
       SAVFLY=0.0
       LOTRIPS=0.0
       LOTTRN=0.0
       RNTTRN=0.0
       ITFTRN=0.0
       ITFTRN2=0.0
       BCR=0.0
       BUR=0.0
       BBRT=0.0
       DTRAN=0.0
       CRSTAZ=0.0
       URSTAZ=0.0
       EBSTAZ=0.0
       TWSTAZ=0.0
       BRTSTAZ=0.0
       LCRSTAZ=0.0
       LURSTAZ=0.0
       WCR=0.0
       WUR=0.0
       LCRRSTAZ=0.0
       LURRSTAZ=0.0
       LCRISTAZ=0.0
       LURISTAZ=0.0
       WCRR=0.0
       WURR=0.0
       WCRI=0.0
       WURI=0.0
       CRSS=0.0
       URSS=0.0
       CRURSS=0.0
       URCRSS=0.0
       URBRTSS=0.0
       BRTURSS=0.0
       URCRSS2=0.0
       LCRSS=0.0
       LURSS=0.0
       LCRSSR=0.0
       LURSSR=0.0
       LCRSSI=0.0
       LURSSI=0.0
       PTRIP=0.0
       TTRIP=0.0
       TTRIP2=0.0
       TWYEXP=0.0
       MODETRP=0.0
       INTRATRP=0.0
       MISTRP=0.0
       BUSTXFER=0.0
       STATXF=0.0
       RALEGR=0.0
       WTRAT=0.0
       TAXISUM=0.0
C
C CHECK FOR ITERATION COUNT
C
	    IF(LAX.AND.LAXTRN) THEN
	    LAXTRN=.FALSE.
      IF(SAVPROB) CALL AIRPROB(LAXTPROB,LAXSTAP,LAXSTAA,
     *                   LAXRPROB,LAXRNTP,LAXRNTA,
     *                   LAXIPROB,LAXITFP,LAXITFA,
     *                   LAXI2PROB,LAXITF2P,LAXITF2A,
     *                   LAXFPROB,LAXSTAFP,LAXSTAFA)
	    GO TO 8000
	    END IF
	    IF(SPEVENT.AND.EVENTSP) THEN
	    EVENTSP=.FALSE.
	    GO TO 8000
	    END IF
	    IF(SPEVENT.AND.(.NOT.EVENTSP)) THEN
	     IF(.NOT.EVENTLI) THEN
	     EVENTLI=.TRUE.
	     GO TO 8000
	     END IF
	    END IF
	    IF(CNVRGE.AND.LAX) THEN
	     IF(.NOT.RECYC) THEN
	     RECYC=.TRUE.
	     NITER=ITER
	     GO TO 8000
	     ELSE
       WRITE(26,8104)
       WRITE(*,8104)
 8104  FORMAT(/' LAX Station Capacity Restraint Closure Acheived')
       GO TO 9515
       END IF
	    END IF
      IF(LAX) CNVRGE=.TRUE.
	    IF(ITER.LT.NITER) GO TO 8000
	    IF(CALIB.OR.TCALIB) GO TO 8000
C
C DELETE DEBUG WORK FILES
C
      IF(DEBUG) THEN
      CLOSE(31,STATUS='DELETE')
      CLOSE(27,STATUS='DELETE')
      CLOSE(33,STATUS='DELETE')
      CLOSE(34,STATUS='DELETE')
      CLOSE(32,STATUS='DELETE')
      END IF
C
C SUMMARIZE MISSING WALK ACCESS STATIONS FOR EXPRESS BUS
C AND TRANSTIWAY
C
 9515 CONTINUE
      CLOSE(96,STATUS='KEEP')
      CLOSE(97,STATUS='KEEP')
      OPEN(96,FILE='EXPBUS.ERR',STATUS='OLD',FORM='FORMATTED')
      OPEN(97,FILE='TWYBUS.ERR',STATUS='OLD',FORM='FORMATTED')
      INDEX=0
 8201 READ(96,5747,END=8202) CWEXP
      EXISTS=.FALSE.
      DO K=1,50
      IF(CWEXP.EQ.STAEXP(K)) EXISTS=.TRUE.
      END DO
      IF(.NOT.EXISTS) THEN
      INDEX=INDEX+1
      IF(INDEX.GT.50) THEN
      WRITE(*,8203) 
 8203 FORMAT(//' INDEX GT 50'//)
      END IF
      STAEXP(INDEX)=CWEXP
      END IF
      GO TO 8201
 8202 WRITE(26,8204) (STAEXP(K),K=1,INDEX)
 8204 FORMAT(//' WALK TO EXPRESS MISSING STATION NODES'/
     *         ' -------------------------------------'/
     *  5(10I6/))
      CLOSE(96,STATUS='DELETE')
      INDEX=0
 8205 READ(97,5747,END=8206) CWTWY
      EXISTS=.FALSE.
      DO K=1,50
      IF(CWTWY.EQ.STATWY(K)) EXISTS=.TRUE.
      END DO
      IF(.NOT.EXISTS) THEN
      INDEX=INDEX+1
      IF(INDEX.GT.50) THEN
      WRITE(*,8203) 
      END IF
      STATWY(INDEX)=CWTWY
      END IF
      GO TO 8205
 8206 WRITE(26,8207) (STATWY(K),K=1,INDEX)
 8207 FORMAT(//' WALK TO TRANSITWAY MISSING STATION NODES'/
     *         ' ----------------------------------------'/
     *  5(10I6/))
      CLOSE(97,STATUS='DELETE')
      
C
C
C PROGRAM COMPLETION
C
C      IF(MAXIZ.GT.0.AND.MAXJZ.GT.0) THEN
C      WRITE(26,8105) MAXIZ,MAXJZ,MAXVALUE
C8105  FORMAT(//' MAXIZ=',I4,' MAZJZ=',I4,' MAXVALUE=',F10.2)
C      END IF
       CALL GETTIM(IHR,IMIN,ISEC,I100)
       WRITE(26,8004) IHR,IMIN,ISEC
       WRITE(*,8004) IHR,IMIN,ISEC
 8004  FORMAT(/' Program Completed: ',I2,':',I2,':',I2)
      END