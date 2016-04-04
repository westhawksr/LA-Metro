C***********************************************************************
C     SUBROUTINE SCALIB: CALIBRATES BIAS CONSTANT VALUES               *
C***********************************************************************
      SUBROUTINE SCALIB(ITER,TESUM,CALSUM,
     *                  PTRIP,TTRIP,CBDTRAN,INTRATRP)
      INCLUDE 'stadat.com'
      INCLUDE 'param.com'
      include 'mtamcpar.inc'
C
C     DECLARATIONS NEEDED FOR THIS SUBROUTINE
C
      INTEGER*2   ITER,TEMODE(34),TMODE
      REAL*8      POBS,PEST,RCON,ECON,NCON,DENOBS,DENEST
      REAL*8      TO,TE
      REAL*8      TOTAL,WLKRAPID,BIKRAPID,WLKTWY,WLKEXP
      REAL*8      DRVRAPID,WLKBRT,DRVBRT,DRVTWY,DRVEXP,BIKEXP,BIKTWY
      REAL*8      BIKBRT
      REAL*8      WLTXFER,BLTXFER,DLTXFER,WRTXFER,BRTXFER,DRTXFER,
     *            WETXFER,BETXFER,DETXFER,WTTXFER,BTTXFER,DTTXFER,
     *            WBTXFER,BBTXFER,DBTXFER,CBUSEGR,UBUSEGR
      REAL*8      IAUTO(5),IWALK(5),IBIKE(5)
      REAL*8      TEMP1,TEMP2,TEMP3
      REAL*8      OESUM(34,6),DFSUM(34,6),PCSUM(34,6)
      REAL*8      COESUM(4,6),DFSUMC(4,5),PCSUMC(4,5)
      REAL*8      TESUM(80,6),CALSUM(4,6)
      REAL*4      CBDTRAN,INTRATRP(7,6)
      REAL*8      TMP(37,5),DENOM
      REAL*8      OTRIP(21,5),INCDAT(5),PTRIP(21,5),TTRIP(21,5)
      REAL*8      CVALUE(8),EQUIVT
      REAL*8      OTRN(7,6),ETRN(7,6),DTRN(7,6)
      INTEGER*4   C, INC, K, MODE, T,T1,T2,MAXT
      INTEGER*4   INCGP,PINDEX,AMODE
      LOGICAL     CCODE(65),EXISTS
      CHARACTER*80 HEADER
      DATA        TEMODE/1,2,3,51,5,6,7,8,9,10,
     *                   11,12,13,14,15,16,17,18,19,20,
     *                   48,49,52,50,56,57,64,65,66,70,
     *                   68,72,74,75/
C
      WRITE(26,1) ITER
      WRITE(51,1) ITER
    1 FORMAT(//25X,'Calibration Iteration #',I2,/,
     *           25X,'---------------------------',//)
      WRITE(*,1) ITER
C
C     EVALUATE MINIMUM VALUE
C
      DENOM=MINVAL/(LSUMS*LSUMA)
      IF(DENOM.LT.(-71.0)) THEN
         MINVAL=(-70.0)*LSUMS*LSUMA
      END IF
      WRITE(26,3) MINVAL
      WRITE(*,3) MINVAL
    3 FORMAT(/1X,' Minimum Allowable Constant=',F10.3//)
C
C     READ OBSERVED CALIBRATION VALUES FROM UNIT 50
C
      INQUIRE(FILE=CTVINP,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(*,500) 
      WRITE(26,500)
  500 FORMAT(/' SCALIB 500 (F) CALIBRATION TARGET VALUE FILE',
     *       ' NOT FOUND'/)
      STOP
      END IF
      OPEN(50,FILE=CTVINP,STATUS='OLD',FORM='FORMATTED')
      READ(50,4) HEADER
    4 FORMAT(A80)
      READ(50,*,END=100) CCODE
      WRITE(26,2) CCODE
    2 FORMAT(' CCODE( 1)=',L1,' KTRN,KSR,K3P,K4P,KNMOT,KBIKE',
     *                        ' (TRANSIT,SHARED RIDE,3-PERSON,',
     *                        '4-PERSON,NON-MOTORIZED,BIKE)'/
     *       ' CCODE( 2)=',L1,' KDTRN  (DRIVE ACCESS TO TRANSIT'/
     *       ' CCODE( 3)=',L1,' NOT USED'/
     *       ' CCODE( 4)=',L1,' KCR    (COMMUTER RAIL)'/
     *       ' CCODE( 5)=',L1,' KUR    (URBAN RAIL)'/
     *       ' CCODE( 6)=',L1,' KEBUS  (EXPRESS BUS)'/
     *       ' CCODE( 7)=',L1,' KWAY   (TRANSITWAY)'/
     *       ' CCODE( 8)=',L1,' KBKCR  (BIKE TO COMMUTER RAIL)'/
     *       ' CCODE( 9)=',L1,' KBKUR  (BIKE TO URBAN RAIL)'/
     *       ' CCODE(10)=',L1,' KBCR   (BUS TO COMMUTER RAIL)'/
     *       ' CCODE(11)=',L1,' KBUR   (BUS TO URBAN RAIL)'/
     *       ' CCODE(12)=',L1,' KBRPDW (BUS TRANSFER TO RAPID WALK)'/
     *       ' CCODE(13)=',L1,' KBTWYW (BUS TRANSFER TO WALK ',
     *                                 'TRANSITWAY)'/
     *       ' CCODE(14)=',L1,' KBEXPW (BUS TRANSFER TO WALK EXPRESS)'/
     *       ' CCODE(15)=',L1,' KINFL  (INFORMAL DRIVE ACCESS TO ',
     *                                 'LOCAL BUS)'/
     *       ' CCODE(16)=',L1,' KINFR  (INFORMAL DRIVE ACCESS TO ',
     *                                 'RAPID BUS)'/
     *       ' CCODE(17)=',L1,' KTRNT  (TRANSIT CONSTANT BY DISTANCE)'/
     *       ' CCODE(18)=',L1,' KBRPDB (BUS TRANSFER TO BIKE RAPID)'/
     *       ' CCODE(19)=',L1,' KBRT   (BRT)'/
     *       ' CCODE(20)=',L1,' KRBUS  (RAPID BUS)'/
     *       ' CCODE(21)=',L1,' KBBRTW (BUS TRANSFER TO WALK BRT)'/
     *       ' CCODE(22)=',L1,' KBRPDD (BUS TRANSFER TO DRIVE RAPID)'/
     *       ' CCODE(23)=',L1,' KBBRTD (BUS TRANSFER TO DRIVE BRT)'/
     *       ' CCODE(24)=',L1,' CCBD   (CBD CONSTANT)'/
     *       ' CCODE(25)=',L1,' KBTWYD (BUS TRANSFER TO DRIVE ',
     *                        'TRANSITWAY)'/
     *       ' CCODE(26)=',L1,' KBEXPD (BUS TRANSFER TO DRIVE ',
     *                        'EXPRESS)'/
     *       ' CCODE(27)=',L1,' KBBUS  (BIKE TO LOCAL BUS)'/
     *       ' CCODE(28)=',L1,' KBRPD  (BIKE TO RAPID BUS)'/
     *       ' CCODE(29)=',L1,' KBWAY  (BIKE TO TRANSITWAY)'/
     *       ' CCODE(30)=',L1,' KBEXP  (BIKE TO EXPRESS)'/
     *       ' CCODE(31)=',L1,' KBBRT  (BIKE TO BRT)'/
     *       ' CCODE(32)=',L1,' KBTWYB (BUS TRANSFERS TO BIKE ',
     *                        'TRANSITWAY)'/
     *       ' CCODE(33)=',L1,' KBEXPB (BUS TRANSFERS TO BIKE ',
     *                        'EXPRESS)'/
     *       ' CCODE(34)=',L1,' KBBRTB (BUS TRANSFERS TO BRT)'/
     *       ' CCODE(35)=',L1,' KINTRAWK (INTRAZONAL WALK)'/
     *       ' CCODE(36)=',L1,' KINTRABK (INTRAZONAL BIKE)'/
     *       ' CCODE(37)=',L1,' KPUR (PNR TO URBAN RAIL)'/
     *       ' CCODE(38)=',L1,' KWCR (WALK TO COMMUTER RAIL)'/
     *       ' CCODE(39)=',L1,' KPNR (PNR TO COMMUTER & URBAN'/
     *       ' CCODE(40)=',L1,' KWUR (WALK TO URBAN RAIL)'/
     *       ' CCODE(41)=',L1,' KWWAY (WALK TO TRANSITWAY'/
     *       ' CCODE(42)=',L1,' DRIVE TO EXPRESS BUS (KDEXP)'/
     *       ' CCODE(43)=',L1,' KPCR (PNR TO COMMUTER RAIL)'/
     *       ' CCODE(44)=',L1,' KDA(5) (DRIVE ALONE SEGMENT 5)'/
     *       ' CCODE(45)=',L1,' KAUT(1) (AUTO CONSTANT SEGMENT 1)'/
     *       ' CCODE(46)=',L1,' COEFF(31) (WALK TO LOCAL',
     *                        ' BUS TRANSFERS)'/
     *       ' CCODE(47)=',L1,' COEFF(32) (BIKE TO LOCAL',
     *                        ' BUS TRANSFERS)'/
     *       ' CCODE(48)=',L1,' KWBUS (WALK TO LOCAL BUS)'/
     *       ' CCODE(49)=',L1,' COEFF(33) (DRIVE TO LOCAL',
     *                        ' BUS TRANSFERS)'/
     *       ' CCODE(50)=',L1,' COEFF(34) (WALK TO RAPID',
     *                        ' BUS TRANSFERS)'/
     *       ' CCODE(51)=',L1,' COEFF(35) (BIKE TO RAPID',
     *                        ' BUS TRANSFERS)'/
     *       ' CCODE(52)=',L1,' COEFF(36) (DRIVE TO RAPID',
     *                        ' BUS TRANSFERS)'/
     *       ' CCODE(53)=',L1,' COEFF(37) (WALK TO EXPRESS',
     *                        ' BUS TRANSFERS)'/
     *       ' CCODE(54)=',L1,' COEFF(38) (BIKE TO EXPRESS',
     *                        ' BUS TRANSFERS)'/
     *       ' CCODE(55)=',L1,' COEFF(39) (DRIVE TO EXPRESS',
     *                        ' BUS TRANSFERS)'/
     *       ' CCODE(56)=',L1,' COEFF(40) (WALK TO TRANSITWAY',
     *                        ' BUS TRANSFERS)'/
     *       ' CCODE(57)=',L1,' COEFF(41) (BIKE TO TRANSITWAY',
     *                        ' BUS TRANSFERS)'/
     *       ' CCODE(58)=',L1,' COEFF(42) (DRIVE TO TRANSITWAY',
     *                        ' BUS TRANSFERS)'/
     *       ' CCODE(59)=',L1,' COEFF(43) (WALK TO BRT',
     *                        ' BUS TRANSFERS)'/
     *       ' CCODE(60)=',L1,' COEFF(44) (BIKE TO BRT',
     *                        ' BUS TRANSFERS)'/
     *       ' CCODE(61)=',L1,' COEFF(45) (DRIVE TO BRT',
     *                        ' BUS TRANSFERS)'/
     *       ' CCODE(62)=',L1,' COEFF(46) (COMMUTER',
     *                        ' RAIL BUS EGRESS)'/
     *       ' CCODE(63)=',L1,' COEFF(47) (URBAN',
     *                        ' RAIL BUS EGRESS)'/
     *       ' CCODE(64)=',L1,' KWCR(1) (WALK TO COMMUTER RAIL)'/
     *       ' CCODE(65)=',L1,' KWUR(1) (WALK TO URBAN RAIL)'/)
      READ(50,4) HEADER
      READ(50,*) BIKRAPID,DRVRAPID,WLKRAPID,BIKEXP,DRVEXP,WLKEXP,
     *           BIKTWY,DRVTWY,WLKTWY,BIKBRT,DRVBRT,WLKBRT
      READ(50,4) HEADER
      READ(50,*) IAUTO(1),IWALK(1),IBIKE(1),IAUTO(2),IWALK(2),IBIKE(2),
     *           IAUTO(3),IWALK(3),IBIKE(3),IAUTO(4),IWALK(4),IBIKE(4),
     *           IAUTO(5),IWALK(5),IBIKE(5)
      READ(50,4) HEADER
      READ(50,*) WLTXFER,BLTXFER,DLTXFER,WRTXFER,BRTXFER,DRTXFER,
     *           WETXFER,BETXFER,DETXFER,WTTXFER,BTTXFER,DTTXFER,
     *           WBTXFER,BBTXFER,DBTXFER,CBUSEGR,UBUSEGR
  45     READ(50,*,END=100) INCGP,MODE,TOTAL
         IF(INCGP.EQ.99) GO TO 100
         IF((MODE.LE.0).OR.(MODE.GT.37).OR.(INCGP.LE.0).OR.
     *      (INCGP.GT.NCATS)) THEN
         WRITE(26,103) MODE,INCGP
  103    FORMAT(' SCALIB 45 (F) ERROR IN CALIBRATION INDICES ',
     *          ' MODE=',I3,' MARKET SEGMENT=',I2)
         STOP 45
         END IF
         TMP(MODE,INCGP)=TOTAL
C ------------------------------------------
         IF(DEBUG) THEN
         WRITE(26,102) MODE,INCGP,TOTAL
  102    FORMAT(2X,I2,2X,I2,2X,F8.0)
         END IF
C --------------------------------------------
         GOTO 45
  100 CONTINUE
  150 READ(50,*,END=300) PINDEX,INCDAT
      IF(PINDEX.LT.1.OR.PINDEX.GT.21) THEN
      WRITE(26,151) PINDEX
  151 FORMAT(' SCALIB 151 (F) ILLOGICAL VALUE (',I3,') FOR TRIP',
     *       ' LENGTH INDICATOR IN OBSERVED TRANSIT SHARE FILE')
      STOP 151
      END IF
      OTRIP(PINDEX,1)=INCDAT(1)
      OTRIP(PINDEX,2)=INCDAT(2)
      OTRIP(PINDEX,3)=INCDAT(3)
      OTRIP(PINDEX,4)=INCDAT(4)
      OTRIP(PINDEX,5)=INCDAT(5)
      MAXT=PINDEX
C ---------------------------------------------------------
      IF(DEBUG) THEN
      WRITE(26,152) PINDEX,INCDAT,OTRIP(PINDEX,1),
     *              OTRIP(PINDEX,2),OTRIP(PINDEX,3),
     *              OTRIP(PINDEX,4),OTRIP(PINDEX,5),MAXT
  152 FORMAT(' PINDEX=',I2,' INCDAT=',5(1X,F6.3),
     *       ' OTRIP=',5(1X,F6.3),' MAXT=',I2)
      END IF
C ---------------------------------------------------------
      GO TO 150
  300 CLOSE(50,STATUS='KEEP')
      IF(CCODE(3)) THEN
      CCODE(3)=.FALSE.
      CCODE(39)=.TRUE.
      END IF
C
C SUM OBSERVED DATA TO OESUM VARIABLE
C
C COESUM(1) = DRIVE-ALONE
C COESUM(2) = 2-PERSON
C COESUM(3) = 3+-PERSON
C COESUM(4) = 4+ PERSON
C
C OESUM(1) = DRIVE-ALONE
C OESUM(2) = 2-PERSON
C OESUM(3) = 3+-PERSON
C OESUM(4) = WALK
C OESUM(5) = TOTAL TRANSIT TRIPS
C OESUM(6) = TOTAL PERSON TRIPS
C OESUM(7)  = WALK TO LOCAL BUS
C OESUM(8)  = DRV TO LOCAL BUS
C OESUM(9)  = WALK TO EXP BUS
C OESUM(10) = DRV TO EXP BUS
C OESUM(11) = WALK TO COMMUTER RAIL
C OESUM(12) = BUS TO COMMUTER RAIL
C OESUM(13) = PNR TO COMMUTER RAIL
C OESUM(14) = KNR TO COMMUTER RAIL
C OESUM(15) = WALK TO URBAN RAIL
C OESUM(16) = BUS TO URBAN RAIL
C OESUM(17) = PNR TO URBAN RAIL
C OESUM(18) = KNR TO URBAN RAIL
C OESUM(19) = WALK TO TWAY BUS
C OESUM(20) = DRV TO TWAY BUS
C OESUM(21) = WALK TO RAPID BUS
C OESUM(22) = DRV  TO RAPID BUS
C OESUM(23) = BIKE
C OESUM(24) = TOTAL NON-MOTORIZED
C OESUM(25) = WALK TO BRT
C OESUM(26) = DRV  TO BRT
C OESUM(27) = 4+ PERSON AUTO
C OESUM(28) = BIKE TO LOCAL BUS
C OESUM(29) = BIKE TO RAPID BUS
C OESUM(30) = BIKE TO EXPRESS BUS
C OESUM(31) = BIKE TO TRANSITWAY
C OESUM(32) = BIKE TO BRT
C OESUM(33) = BIKE TO COMMUTER RAIL
C OESUM(34) = BIKE TO URBAN RAIL
C
C
      DO 311,INC=1,NCATS
         COESUM(1,INC) = TMP(2,INC)
         COESUM(2,INC) = TMP(3,INC)
         COESUM(3,INC) = TMP(4,INC)
         COESUM(4,INC) = TMP(27,INC)
C...AUTO MODES
	       OESUM(1,INC) = TMP(2,INC)
         OESUM(2,INC) = TMP(3,INC)
         OESUM(3,INC) = TMP(4,INC)
         OESUM(27,INC)=  TMP(27,INC)
C...NON-MOTORIZED MODES
         OESUM(4,INC)  = TMP(25,INC)
         OESUM(23,INC) = TMP(26,INC)
         OESUM(24,INC) = TMP(25,INC)+TMP(26,INC)
C...LOCAL BUS
         OESUM(7,INC) = TMP(5,INC)
         OESUM(8,INC) = TMP(6,INC)+TMP(7,INC)
         OESUM(28,INC)= TMP(31,INC)
         OTRN(1,INC)  = OESUM(7,INC)+OESUM(8,INC)+OESUM(28,INC)
         ETRN(1,INC)  = TESUM(7,INC)+TESUM(8,INC)+TESUM(65,INC)
C...EXPRESS BUS
         OESUM(9,INC) = TMP(8,INC)
         OESUM(10,INC)= TMP(9,INC)+TMP(10,INC)
         OESUM(30,INC)= TMP(32,INC)
         OTRN(4,INC)  = OESUM(9,INC)+OESUM(10,INC)+OESUM(30,INC)
         ETRN(4,INC)  = TESUM(9,INC)+TESUM(10,INC)+TESUM(70,INC)
C...COMMUTER RAIL
         OESUM(11,INC)= TMP(18,INC)
         OESUM(13,INC)= TMP(19,INC)
         OESUM(14,INC)= TMP(20,INC)
         OESUM(12,INC)= TMP(21,INC)
         OESUM(33,INC)= TMP(35,INC)
         OTRN(7,INC)=OESUM(11,INC)+OESUM(12,INC)+OESUM(13,INC)+
     *               OESUM(14,INC)+OESUM(33,INC)
         ETRN(7,INC)=TESUM(11,INC)+TESUM(12,INC)+TESUM(13,INC)+
     *               TESUM(14,INC)+TESUM(74,INC)
C...URBAN RAIL
         OESUM(15,INC)= TMP(14,INC)
         OESUM(17,INC)= TMP(15,INC)
         OESUM(18,INC)= TMP(16,INC)
         OESUM(16,INC)= TMP(17,INC)
         OESUM(34,INC)= TMP(34,INC)
         OTRN(6,INC)=OESUM(15,INC)+OESUM(16,INC)+OESUM(17,INC)+
     *               OESUM(18,INC)+OESUM(34,INC)
         ETRN(6,INC)=TESUM(15,INC)+TESUM(16,INC)+TESUM(17,INC)+
     *               TESUM(18,INC)+TESUM(75,INC)
C...TWAY BUS
         OESUM(19,INC)= TMP(11,INC)
         OESUM(20,INC)= TMP(12,INC)+TMP(13,INC)
         OESUM(31,INC)= TMP(33,INC)
         OTRN(3,INC)=OESUM(19,INC)+OESUM(20,INC)+OESUM(31,INC)
         ETRN(3,INC)=TESUM(19,INC)+TESUM(20,INC)+TESUM(68,INC)
C...RAPID BUS
         OESUM(21,INC)= TMP(22,INC)
         OESUM(22,INC)= TMP(23,INC)+TMP(24,INC)
         OESUM(29,INC)= TMP(36,INC)
         OTRN(2,INC)=OESUM(21,INC)+OESUM(22,INC)+OESUM(29,INC)
         ETRN(2,INC)=TESUM(48,INC)+TESUM(49,INC)+TESUM(66,INC)
C...BRT
         OESUM(25,INC)= TMP(28,INC)
         OESUM(26,INC)= TMP(29,INC) + TMP(30,INC)
         OESUM(32,INC)= TMP(37,INC)
         OTRN(5,INC)=OESUM(25,INC)+OESUM(26,INC)+OESUM(32,INC)
         ETRN(5,INC)=TESUM(56,INC)+TESUM(57,INC)+TESUM(72,INC)
C...TOTAL TRANSIT
         OESUM(5,INC)=OESUM(7,INC)+OESUM(8,INC)+OESUM(9,INC)+
     *                OESUM(10,INC)+OESUM(11,INC)+OESUM(12,INC)+
     *                OESUM(13,INC)+OESUM(14,INC)+OESUM(15,INC)+
     *                OESUM(16,INC)+OESUM(17,INC)+OESUM(18,INC)+
     *                OESUM(19,INC)+OESUM(20,INC)+OESUM(21,INC)+
     *                OESUM(22,INC)+OESUM(25,INC)+OESUM(26,INC)+
     *                OESUM(28,INC)+OESUM(29,INC)+OESUM(30,INC)+
     *                OESUM(31,INC)+OESUM(32,INC)+OESUM(33,INC)+
     *                OESUM(34,INC)
  311 CONTINUE
C
C...TOTAL PERSON & TOTAL NON-MOTORIZED
        DO 313,INC=1,NCATS
         DO 312,IMO=1,34
	        IF(IMO.LE.7) THEN
	        OTRN(IMO,6)=OTRN(IMO,6)+OTRN(IMO,INC)
	        ETRN(IMO,6)=ETRN(IMO,6)+ETRN(IMO,INC)
	        END IF
              IF(imo.eq.6.or.imo.eq.5) GOTO 312
              if(imo.eq.24) goto 312
              OESUM(6,INC)=OESUM(6,INC)+OESUM(IMO,INC)
	        OESUM(IMO,6)=OESUM(IMO,6)+OESUM(IMO,INC)
	        OESUM(6,6)=OESUM(6,6)+OESUM(IMO,INC)
	        IF(IMO.LE.4) COESUM(IMO,6)=COESUM(IMO,6)+COESUM(IMO,INC)
  312    CONTINUE
	        OESUM(5,6)=OESUM(5,6)+OESUM(5,INC)
	        OESUM(24,6)=OESUM(24,6)+OESUM(24,INC)
  313    CONTINUE
C
C    DFSUM = ABSOLUTE DIFFERENCE
C    PCSUM = PERCENT DIFFERENCE
C

         DO 400,INC=1,6
         DO 410,MODE=1,34
            TMODE=TEMODE(MODE)
            DFSUM(MODE,INC)=TESUM(TMODE,INC)-OESUM(MODE,INC)
            IF(OESUM(MODE,INC).GT.0.0) THEN
               PCSUM(MODE,INC)=(DFSUM(MODE,INC)/OESUM(MODE,INC))*100.0
            ELSE
               PCSUM(MODE,INC)=0.0
            END IF
            IF(MODE.LE.7) DTRN(MODE,INC)=ETRN(MODE,INC)-OTRN(MODE,INC)
  410    CONTINUE
  400    CONTINUE

C
         DO 430,INC=1,NCATS
         DO 430,MODE=1,4
         DFSUMC(MODE,INC)=CALSUM(MODE,INC)-COESUM(MODE,INC)
         IF(COESUM(MODE,INC).GT.0.0) THEN
         PCSUMC(MODE,INC)=(DFSUMC(MODE,INC)/
     *    COESUM(MODE,INC)) * 100.0
         ELSE
         PCSUMC(MODE,INC)=0.0
         ENDIF
 430     CONTINUE
C Write out observed data, and comparisons ..........................
C
  450 CONTINUE
C
C----------------------------------------------------------------------
C
      WRITE(26,1017)
 1017 FORMAT(//,20X,'R E P O R T   1A  (O B S E R V E D  D A T A)',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE AND Market GROUP',//,
     *       1X,'Market   ','         ','    2    ','      3   ',
     *          '      4+   ','         ','         ','   TOTAL  '/
     *       1X,'Segment  ','  DRIVE  ','  PERSON ','    PERSON ',
     *          '    PERSON ','   NON   ',' TRANSIT ','   PERSON '/
     *       1X,'         ','  ALONE  ','  AUTO   ','     AUTO  ',
     *          '     AUTO  ','MOTORIZED','   TRIPS ','   TRIPS '/
     *       1X,'---------','---------','---------','----------',
     *          '----------','----------','----------','----------')
      DO 2015 C=1,NCATS
         WRITE(26,9002) C,(OESUM(T,C),T=1,3),OESUM(27,C),OESUM(24,C),
     *                     OESUM(5,C),OESUM(6,C)
 9002    FORMAT(5X,I1,2X,7F10.0)
 2015 CONTINUE
      WRITE(26,9003) (OESUM(T,6),T=1,3),OESUM(27,6),OESUM(24,6),
     *                OESUM(5,6),OESUM(6,6)
 9003 FORMAT(/,2X,'TOTAL',1X,7F10.0)
      IF(.NOT.AIRPASS) WRITE(26,9201)
 9201 FORMAT(//,20X,'R E P O R T   2A  (E S T I M A T E D  D A T A)',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE AND Market  LEVEL',//,
     *       1X,'Market   ','         ','    2    ','      3   ',
     *          '      4+   ','         ','         ','   TOTAL  '/
     *       1X,'Segment  ','  DRIVE  ','  PERSON ','    PERSON ',
     *          '    PERSON ','   NON   ',' TRANSIT ','   PERSON '/
     *       1X,'         ','  ALONE  ','  AUTO   ','     AUTO  ',
     *          '     AUTO  ','MOTORIZED','   TRIPS ','   TRIPS '/
     *       1X,'---------','---------','---------','----------',
     *          '----------','----------','----------','----------')
      DO 455 C=1,NCATS
      WRITE(26,9002) C,(TESUM(T,C),T=1,3),
     *                  TESUM(64,C),TESUM(50,C),
     *                  TESUM(5,C),TESUM(6,C)
  455 CONTINUE
      WRITE(26,9003) (TESUM(T,6),T=1,3),
     *                TESUM(64,6),TESUM(50,6),
     *                TESUM(5,6),TESUM(6,6)
      WRITE(26,1019)
 1019 FORMAT(//,20X,'R E P O R T   3A  (ESTIMATED-OBSERVED DATA)',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE AND Market GROUP',//,
     *       1X,'Market   ','         ','    2    ','      3   ',
     *          '      4+   ','         ','         ','   TOTAL  '/
     *       1X,'Segment  ','  DRIVE  ','  PERSON ','    PERSON ',
     *          '    PERSON ','   NON   ',' TRANSIT ','   PERSON '/
     *       1X,'         ','  ALONE  ','  AUTO   ','     AUTO  ',
     *          '     AUTO  ','MOTORIZED','   TRIPS ','   TRIPS '/
     *       1X,'---------','---------','---------','----------',
     *          '----------','----------','----------','----------')
      DO 2025 C=1,NCATS
         WRITE(26,9002) C,(DFSUM(T,C),T=1,3),DFSUM(27,C),DFSUM(24,C),
     *                     DFSUM(5,C),DFSUM(6,C)
 2025 CONTINUE
      WRITE(26,9003) (DFSUM(T,6),T=1,3),DFSUM(27,6),DFSUM(24,6),
     *                DFSUM(5,6),DFSUM(6,6)
C
C
      WRITE(26,1018)
 1018 FORMAT(//,20X,'R E P O R T   1B  (O B S E R V E D  D A T A)',/,
     *          20X,
     *          'SUMMARIZE TRANSIT TRIPS BY MarketSegment       ',//,
     *       1X,'Market   ','   LOCAL   ','  LOCAL  ',' EXPRESS ',
     *          '  EXRESS  ',
     *          ' COMMUTER  ',
     *          ' COMMUTER ',' COMMUTER ',' COMMUTER  ',' URBAN   ',
     *          '  URBAN   ','  URBAN   ','  URBAN    ',' TWAY    ',
     *          '  TWAY    ','  RAPID   ','  RAPID    ','   BRT   ',
     *          '   BRT    '/
     *       1X,'GROUP    ','    BUS   ','     BUS   ','  BUS   ',
     *          '   BUS     ',
     *          '  RAIL   ',
     *          '  RAIL   ','   RAIL     ',' RAIL    ','   RAIL    ',
     *          '  RAIL   ','   RAIL     ',' RAIL  ','    BUS    ',
     *          '   BUS    ','   BUS      ','  BUS   ','     BUS    ',
     *          '  BUS   '/
     *       1X,'         ','   WALK  ','    DRIVE  ','  WALK  ',
     *          '   DRIVE  ',
     *          '   WALK  ',
     *          '    BUS   ','    PNR     ','   KNR     ','   WALK   ',
     *          '    BUS   ','  PNR     ','  KNR     ','  WALK   ',
     *          '   DRIVE  ','   WALK    ','  DRIVE    ','  WALK   ',
     *          '  DRIVE    '/
     *       1X,'---------','---------','---------','----------',
     *          '----------',
     *          '----------',
     *          '---------','---------','---------','-----------',
     *          '---------','---------','---------','-----------',
     *          '---------','---------','----------------',
     *          '---------','-----------')
      DO 2016 C=1,NCATS
         WRITE(26,9005) C,(OESUM(T,C),T=7,22),OESUM(25,C),OESUM(26,C)
 9005    FORMAT(5X,I1,2X,18F10.0)
 2016 CONTINUE
      WRITE(26,9006) (OESUM(T,6),T=7,22),OESUM(25,6),OESUM(26,6)
 9006 FORMAT(/,2X,'TOTAL',1X,18F10.0)
      WRITE(26,1028)
 1028 FORMAT(//,20X,'R E P O R T   2B  (E S T I M A T E D  D A T A)',/,
     *          20X,
     *          'SUMMARIZE TRANSIT TRIPS BY MarketSegment       ',//,
     *       1X,'Market   ','   LOCAL   ','  LOCAL  ',' EXPRESS ',
     *          '  EXRESS  ',
     *          ' COMMUTER  ',
     *          ' COMMUTER ',' COMMUTER ',' COMMUTER  ',' URBAN   ',
     *          '  URBAN   ','  URBAN   ','  URBAN    ',' TWAY    ',
     *          '  TWAY    ','  RAPID   ','  RAPID    ','   BRT   ',
     *          '   BRT    '/
     *       1X,'GROUP    ','    BUS   ','     BUS   ','  BUS   ',
     *          '   BUS     ',
     *          '  RAIL   ',
     *          '  RAIL   ','   RAIL     ',' RAIL    ','   RAIL    ',
     *          '  RAIL   ','   RAIL     ',' RAIL  ','    BUS    ',
     *          '   BUS    ','   BUS      ','  BUS   ','     BUS    ',
     *          '  BUS   '/
     *       1X,'         ','   WALK  ','    DRIVE  ','  WALK  ',
     *          '   DRIVE  ',
     *          '   WALK  ',
     *          '    BUS   ','    PNR     ','   KNR     ','   WALK   ',
     *          '    BUS   ','  PNR     ','  KNR     ','  WALK   ',
     *          '   DRIVE  ','   WALK    ','  DRIVE    ','  WALK   ',
     *          '  DRIVE    '/
     *       1X,'---------','---------','---------','----------',
     *          '----------',
     *          '----------',
     *          '---------','---------','---------','-----------',
     *          '---------','---------','---------','-----------',
     *          '---------','---------','----------------',
     *          '---------','-----------')
      DO C=1,NCATS
      WRITE(26,9005) C,(TESUM(K,C),K=7,10),(TESUM(K1,C),K1=11,14),
     *      (TESUM(K2,C),K2=15,18),(TESUM(K3,C),K3=19,20),
     *      (TESUM(K4,C),K4=48,49),(TESUM(K5,C),K5=56,57)
      END DO
      WRITE(26,9006) (TESUM(K,6),K=7,10),(TESUM(K1,6),K1=11,14),
     *      (TESUM(K2,6),K2=15,18),(TESUM(K3,6),K3=19,20),
     *      (TESUM(K4,6),K4=48,49),(TESUM(K5,6),K5=56,57)
      WRITE(26,1038)
 1038 FORMAT(//,20X,'R E P O R T   3B  (ESTIMATED-OBSERVED DATA)',/,
     *          20X,
     *          'SUMMARIZE TRANSIT TRIPS BY MarketSegment       ',//,
     *       1X,'Market   ','   LOCAL   ','  LOCAL  ',' EXPRESS ',
     *          '  EXRESS  ',
     *          ' COMMUTER  ',
     *          ' COMMUTER ',' COMMUTER ',' COMMUTER  ',' URBAN   ',
     *          '  URBAN   ','  URBAN   ','  URBAN    ',' TWAY    ',
     *          '  TWAY    ','  RAPID   ','  RAPID    ','   BRT   ',
     *          '   BRT    '/
     *       1X,'GROUP    ','    BUS   ','     BUS   ','  BUS   ',
     *          '   BUS     ',
     *          '  RAIL   ',
     *          '  RAIL   ','   RAIL     ',' RAIL    ','   RAIL    ',
     *          '  RAIL   ','   RAIL     ',' RAIL  ','    BUS    ',
     *          '   BUS    ','   BUS      ','  BUS   ','     BUS    ',
     *          '  BUS   '/
     *       1X,'         ','   WALK  ','    DRIVE  ','  WALK  ',
     *          '   DRIVE  ',
     *          '   WALK  ',
     *          '    BUS   ','    PNR     ','   KNR     ','   WALK   ',
     *          '    BUS   ','  PNR     ','  KNR     ','  WALK   ',
     *          '   DRIVE  ','   WALK    ','  DRIVE    ','  WALK   ',
     *          '  DRIVE    '/
     *       1X,'---------','---------','---------','----------',
     *          '----------',
     *          '----------',
     *          '---------','---------','---------','-----------',
     *          '---------','---------','---------','-----------',
     *          '---------','---------','----------------',
     *          '---------','-----------')
      DO C=1,NCATS
      WRITE(26,9005) C,(DFSUM(T,C),T=7,22),DFSUM(25,C),DFSUM(26,C)
      END DO
      WRITE(26,9006) (DFSUM(T,6),T=7,22),DFSUM(25,6),DFSUM(26,6)
C
      WRITE(26,9224)
 9224 FORMAT(//,20X,'R E P O R T   1C  (O B S E R V E D  D A T A)',/,
     *          20X,
     *          'SUMMARIZE TRANSIT TRIPS BY Market GROUP',//,
     *       1X,' Market  ','   LOCAL  ','  RAPID   ','TRANSITWAY',
     *                      '  EXPRESS ','   BRT    ','  URBAN   ',
     *                      ' COMMUTER '/
     *       1X,' Segment ','    BUS   ','   BUS    ','   BUS    ',
     *                      '    BUS   ','   BUS    ','  RAIL    ',
     *                      '   RAIL   '/
     *       1X,'  LEVEL  ','    BIKE  ','   BIKE   ','   BIKE   ',
     *                      '    BIKE  ','  BIKE   ','   BIKE   ',
     *                      '    BIKE  '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------','----------',
     *                      '----------')
      DO C=1,NCATS
      WRITE(26,9005) C,OESUM(28,C),OESUM(29,C),
     *                 OESUM(31,C),OESUM(30,C),OESUM(32,C),
     *                 OESUM(34,C),OESUM(33,C)
      END DO
      WRITE(26,9006) OESUM(28,6),OESUM(29,6),OESUM(31,6),OESUM(30,6),
     *               OESUM(32,6),OESUM(34,6),OESUM(33,6)
      WRITE(26,9324)
 9324 FORMAT(//,20X,'R E P O R T   2C  (E S T I M A T E D  D A T A)',/,
     *          20X,
     *          'SUMMARIZE TRANSIT TRIPS BY Market GROUP',//,
     *       1X,' Market  ','   LOCAL  ','  RAPID   ','TRANSITWAY',
     *                      '  EXPRESS ','   BRT    ','  URBAN   ',
     *                      ' COMMUTER '/
     *       1X,' Segment ','    BUS   ','   BUS    ','   BUS    ',
     *                      '    BUS   ','   BUS    ','  RAIL    ',
     *                      '   RAIL   '/
     *       1X,'  LEVEL  ','    BIKE  ','   BIKE   ','   BIKE   ',
     *                      '    BIKE  ','   BIKE   ',' BIKE   ',
     *                      '    BIKE  '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------','----------',
     *                      '----------')
      DO C=1,NCATS
      WRITE(26,9005) C,TESUM(65,C),TESUM(66,C),TESUM(68,C),TESUM(70,C),
     *                 TESUM(72,C),TESUM(75,C),TESUM(74,C)
      END DO
      WRITE(26,9006) TESUM(65,6),TESUM(66,6),TESUM(68,6),TESUM(70,6),
     *                 TESUM(72,6),TESUM(75,6),TESUM(74,6)
      WRITE(26,9424)
 9424 FORMAT(//,20X,'R E P O R T   3C  (ESTIMATED-OBSERVED DATA)',/,
     *          20X,
     *          'SUMMARIZE TRANSIT TRIPS BY Market GROUP',//,
     *       1X,' Market  ','   LOCAL  ','  RAPID   ','TRANSITWAY',
     *                      '  EXPRESS ','   BRT    ','  URBAN   ',
     *                      ' COMMUTER '/
     *       1X,' Segment ','    BUS   ','   BUS    ','   BUS    ',
     *                      '    BUS   ','   BUS    ','  RAIL    ',
     *                      '   RAIL   '/
     *       1X,'  LEVEL  ','    BIKE  ','   BIKE   ','   BIKE   ',
     *                      '    BIKE  ','   BIKE   ','   BIKE   ',
     *                      '    BIKE  '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------','----------',
     *                      '----------')
      DO C=1,NCATS
      WRITE(26,9005) C,DFSUM(28,C),DFSUM(29,C),
     *                 DFSUM(31,C),DFSUM(30,C),DFSUM(32,C),
     *                 DFSUM(33,C),DFSUM(34,C)
      END DO
      WRITE(26,9006) DFSUM(28,6),DFSUM(29,6),DFSUM(31,6),DFSUM(30,6),
     *               DFSUM(32,6),DFSUM(34,6),DFSUM(33,6)
      WRITE(26,9624)
 9624 FORMAT(//,20X,'R E P O R T   1D  (O B S E R V E D  D A T A)',/,
     *          20X,
     *          'SUMMARIZE TRANSIT TRIPS BY Market GROUP',//,
     *       1X,' Market  ','   LOCAL  ','  RAPID   ','TRANSITWAY',
     *                      '  EXPRESS ','   BRT    ','  URBAN   ',
     *                      ' COMMUTER '/
     *       1X,' Segment ','    BUS   ','   BUS    ','   BUS    ',
     *                      '    BUS   ','   BUS    ','  RAIL    ',
     *                      '   RAIL   '/
     *       1X,'  LEVEL  ','   TOTAL  ','  TOTAL   ','  TOTAL   ',
     *                      '   TOTAL  ',' TOTAL   ','  TOTAL   ',
     *                      '   TOTAL  '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------','----------',
     *                      '----------')
      DO C=1,NCATS
      WRITE(26,9005) C,(OTRN(K,C),K=1,7)
      END DO
      WRITE(26,9006) (OTRN(K,6),K=1,7)
      WRITE(26,9625)
 9625 FORMAT(//,20X,'R E P O R T   2D  (E S T I M A T E D  D A T A)',/,
     *          20X,
     *          'SUMMARIZE TRANSIT TRIPS BY Market GROUP',//,
     *       1X,' Market  ','   LOCAL  ','  RAPID   ','TRANSITWAY',
     *                      '  EXPRESS ','   BRT    ','  URBAN   ',
     *                      ' COMMUTER '/
     *       1X,' Segment ','    BUS   ','   BUS    ','   BUS    ',
     *                      '    BUS   ','   BUS    ','  RAIL    ',
     *                      '   RAIL   '/
     *       1X,'  LEVEL  ','   TOTAL  ','  TOTAL   ','  TOTAL   ',
     *                      '   TOTAL  ',' TOTAL   ','  TOTAL   ',
     *                      '   TOTAL  '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------','----------',
     *                      '----------')
      DO C=1,NCATS
      WRITE(26,9005) C,(ETRN(K,C),K=1,7)
      END DO
      WRITE(26,9006) (ETRN(K,6),K=1,7)
      WRITE(26,9626)
 9626 FORMAT(//,20X,'R E P O R T   3D  (ESTIMATED-OBSERVED DATA)',/,
     *          20X,
     *          'SUMMARIZE TRANSIT TRIPS BY Market GROUP',//,
     *       1X,' Market  ','   LOCAL  ','  RAPID   ','TRANSITWAY',
     *                      '  EXPRESS ','   BRT    ','  URBAN   ',
     *                      ' COMMUTER '/
     *       1X,' Segment ','    BUS   ','   BUS    ','   BUS    ',
     *                      '    BUS   ','   BUS    ','  RAIL    ',
     *                      '   RAIL   '/
     *       1X,'  LEVEL  ','   TOTAL  ','  TOTAL   ','  TOTAL   ',
     *                      '   TOTAL  ',' TOTAL   ','  TOTAL   ',
     *                      '   TOTAL  '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------','----------',
     *                      '----------')
      DO C=1,NCATS
      WRITE(26,9005) C,(DTRN(K,C),K=1,7)
      END DO
      WRITE(26,9006) (DTRN(K,6),K=1,7)
C
      WRITE(26,1317)
 1317 FORMAT(//,20X,'R E P O R T   1E  (O B S E R V E D  D A T A)',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE AND MARKET GROUP',//,
     *       1X,'Market   '/
     *       1X,'Segment  '/
     *       1X,'         ','   WALK  ',' BICYCLE ','   TOTAL   '/
     *       1X,'---------','---------','---------','----------')
      DO C=1,NCATS
         WRITE(26,9002) C,OESUM(4,C),OESUM(23,C),OESUM(24,C)
      END DO
      WRITE(26,9003) OESUM(4,6),OESUM(23,6),OESUM(24,6)
      WRITE(26,1318)
 1318 FORMAT(//,20X,'R E P O R T   2E  (E S T I M A T E D  D A T A)',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE AND MARKET GROUP',//,
     *       1X,'Market   '/
     *       1X,'Segment  '/
     *       1X,'         ','   WALK  ',' BICYCLE ','   TOTAL   '/
     *       1X,'---------','---------','---------','----------')
      DO C=1,NCATS
      WRITE(26,9002) C,TESUM(51,C),TESUM(52,C),TESUM(50,C)
      END DO
      WRITE(26,9003) TESUM(51,6),TESUM(52,6),TESUM(50,6)
      WRITE(26,1319)
 1319 FORMAT(//,20X,'R E P O R T   3E  (ESTIMATED-OBSERVED DATA)',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE AND MARKET GROUP',//,
     *       1X,'Market   '/
     *       1X,'Segment  '/
     *       1X,'         ','   WALK  ',' BICYCLE ','   TOTAL   '/
     *       1X,'---------','---------','---------','----------')
      DO C=1,NCATS
         WRITE(26,9002) C,DFSUM(4,C),DFSUM(23,C),DFSUM(24,C)
      END DO
      WRITE(26,9003) DFSUM(4,6),DFSUM(23,6),DFSUM(24,6)
C
       DO K=1,15
       DO K1=1,4
       BUSTXFER(K,5)=BUSTXFER(K,5)+BUSTXFER(K,K1)
       IF(K.LE.2) THEN
       STATXF(K,5)=STATXF(K,5)+STATXF(K,K1)
       RALEGR(K,5)=RALEGR(K,5)+RALEGR(K,K1)
       END IF
       END DO 
       END DO
       WRITE(26,9516) (BUSTXFER(1,K1),K1=1,5),
     *                (BUSTXFER(2,K2),K2=1,5),
     *                (BUSTXFER(3,K2),K2=1,5),
     *                (BUSTXFER(4,K2),K2=1,5),
     *                (BUSTXFER(5,K2),K2=1,5),
     *                (BUSTXFER(6,K2),K2=1,5),
     *                (BUSTXFER(7,K2),K2=1,5),
     *                (BUSTXFER(8,K2),K2=1,5),
     *                (BUSTXFER(9,K2),K2=1,5),
     *                (BUSTXFER(10,K2),K2=1,5),
     *                (BUSTXFER(11,K2),K2=1,5),
     *                (BUSTXFER(12,K2),K2=1,5),
     *                (BUSTXFER(13,K2),K2=1,5),
     *                (BUSTXFER(14,K2),K2=1,5),
     *                (BUSTXFER(15,K2),K2=1,5),
     *                (STATXF(1,K2),K2=1,5),
     *                (STATXF(2,K2),K2=1,5),
     *                (RALEGR(1,K2),K2=1,5),
     *                (RALEGR(2,K2),K2=1,5)
 9516  FORMAT(//'       BUS TRANSFER INCIDENCE SUMMARY'/
     *          ' -----------------------------------------'/
     *          ' WALK ACCESS     0          1         2   ',
     *          '     3+      TOTAL'/
     *          ' -----------  --------  --------  --------',
     *          '  --------  --------'/
     *          ' LOCAL BUS  ',5(2X,F8.0)/
     *          ' EXPRESS BUS',5(2X,F8.0)/
     *          ' TRANSITWAY ',5(2X,F8.0)/
     *          ' RAPID BUS  ',5(2X,F8.0)/
     *          ' BRT        ',5(2X,F8.0)//
     *          ' BIKE ACCESS     0          1         2   ',
     *          '     3+       TOTAL'/
     *          ' -----------  --------  --------  --------',
     *          '  --------  --------'/
     *          ' LOCAL BUS  ',5(2X,F8.0)/
     *          ' EXPRESS BUS',5(2X,F8.0)/
     *          ' TRANSITWAY ',5(2X,F8.0)/
     *          ' RAPID BUS  ',5(2X,F8.0)/
     *          ' BRT        ',5(2X,F8.0)//
     *          ' DRIVE ACCESS    0          1         2   ',
     *          '     3+       TOTAL'/
     *          ' -----------  --------  --------  --------',
     *          '  --------  --------'/
     *          ' LOCAL BUS  ',5(2X,F8.0)/
     *          ' EXPRESS BUS',5(2X,F8.0)/
     *          ' TRANSITWAY ',5(2X,F8.0)/
     *          ' RAPID BUS  ',5(2X,F8.0)/
     *          ' BRT        ',5(2X,F8.0)//
     *          '    STATION LEVEL TRANSFER INCIDENCE SUMMARY'/
     *          ' -----------------------------------------'/
     *          ' ALL ACCESS      0          1         2   ',
     *          '     3+      TOTAL'/
     *          ' -----------  --------  --------  --------',
     *          '  --------  --------'/
     *          ' COMMUTER RL',5(2X,F8.0)/
     *          ' URBAN RAIL ',5(2X,F8.0)//
     *          '    RAIL EGRESS TRANSFER INCIDENCE SUMMARY'/
     *          ' -----------------------------------------'/
     *          ' BUS EGRESS      0          1         2   ',
     *          '     3+      TOTAL'/
     *          ' -----------  --------  --------  --------',
     *          '  --------  --------'/
     *          ' COMMUTER RL',5(2X,F8.0)/
     *          ' URBAN RAIL ',5(2X,F8.0)/)     
C----------------------------------------------------------------------
C
C
C This next section here starts revising the constants...
C  
C----------------------------------------------------------------------
      WRITE(26,6868) ITER
 6868 FORMAT(///' REVISED CONSTANT COMPUTATION FOR ITERATION=',I2/
     *       ' ---------------------------------------------'//)
C     Calculate Non-Motorized Constants
C
  175 IF(CCODE(1)) THEN
      WRITE(26,6850)
 6850 FORMAT(/15X,'Non-Motorized Constants',/,
     *        15X,'-----------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
	DO 6860 INC=1,NCATS
         IF(OESUM(6,INC).GT.0) THEN
             POBS=(OESUM(4,INC)+OESUM(23,INC))/OESUM(6,INC)
         ELSE
            POBS=0.0
         END IF
         IF(TESUM(6,INC).GT.0) THEN
            PEST=TESUM(50,INC)/TESUM(6,INC)
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KNMOT(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=-50.0
         NCON=DMAX1(NCON,-50.0)
         KNMOT(INC)=NCON
         WRITE(26,6870) (INC),POBS,PEST,ECON,RCON,NCON
 6870    FORMAT(1X,' KNMOT(',I1,')',1X,2(1X,F7.4),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,6880) INC,KNMOT(INC)
 6880 FORMAT('KNMOT(',I1,')=',F9.5)
 6860 CONTINUE
C
C     Calculate Bicycle Constants
C
      WRITE(26,7850)
 7850 FORMAT(/15X,'Bicycle Constants',/,
     *        15X,'-----------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
	DO 7860 INC=1,NCATS
         IF(OESUM(24,INC).GT.0) THEN
             POBS=OESUM(23,INC)/OESUM(24,INC)
         ELSE
            POBS=0.0
         END IF
         IF(TESUM(50,INC).GT.0) THEN
            PEST=TESUM(52,INC)/TESUM(50,INC)
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KBIKE(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=-50.0
         NCON=DMAX1(NCON,-50.0)
         KBIKE(INC)=NCON
         WRITE(26,7870) (INC),POBS,PEST,ECON,RCON,NCON
 7870    FORMAT(1X,' KBIKE(',I1,')',1X,2(1X,F7.4),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,7880) INC,KBIKE(INC)
 7880 FORMAT('KBIKE(',I1,')=',F9.5)
 7860 CONTINUE
C
C     Calculate Transit Constants
C
      IF(CCODE(17)) THEN
      DO 4863 INC=1,NCATS
      WRITE(26,4853) INC
 4853 FORMAT(/10X,'Stratified Transit Constants-Market Segment ',I1/
     *        10X,'--------------------------------------------',/,
     *  1X,'  Trip       Obs     Est    Existing               New'/
     *  1X,' Length    Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
	DO 4864 T=1,21
	   POBS=0.0
	   PEST=0.0
	   IF(T.GE.MAXT) THEN
	    IF(T.GT.MAXT) THEN
	    PTRIP(MAXT,INC)=PTRIP(MAXT,INC)+PTRIP(T,INC)
	    TTRIP(MAXT,INC)=TTRIP(MAXT,INC)+TTRIP(T,INC)
	    END IF
	   IF(T.LT.21) GO TO 4864
	   END IF
             POBS=OTRIP(T,INC)
             PEST=TTRIP(T,INC)
         IF(T.EQ.21) THEN
             PEST=TTRIP(MAXT,INC)
             POBS=OTRIP(MAXT,INC)
         END IF
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         IF(T.LT.MAXT) THEN
         ECON=KTRNT(T,INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=-70.0
         NCON=DMAX1(NCON,-70.0)
         KTRNT(T,INC)=NCON
         T1=(T-1)*5
         T2=T*5
         WRITE(26,4873) T1,T2,POBS,PEST,ECON,RCON,NCON
 4873    FORMAT(1X,I3,'-',I3,3X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
         WRITE(51,4883) T,INC,KTRNT(T,INC)
 4883    FORMAT('KTRNT(',I2,',',I1,')=',F9.5)
         ELSE
         ECON=KTRNT(MAXT,INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=-70.0
         NCON=DMAX1(NCON,-70.0)
         DO 4865 K=MAXT,21
         KTRNT(K,INC)=NCON
         T1=(K-1)*5
         T2=K*5
         WRITE(26,4873) T1,T2,POBS,PEST,ECON,RCON,NCON
         WRITE(51,4883) K,INC,KTRNT(K,INC)
 4865 CONTINUE
      END IF
 4864 CONTINUE
 4863 CONTINUE
      END IF
      WRITE(26,4850)
 4850 FORMAT(/15X,'Transit Constants',/,
     *        15X,'-----------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
	DO 4860 INC=1,NCATS
         POBS=OESUM(5,INC)
         PEST=TESUM(5,INC)
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KTRN(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KTRN(INC)=NCON
         IF(CCODE(17)) THEN
         KTRN(INC)=0.0
         RCON=0.0
         NCON=0.0
         END IF
         WRITE(26,4870) (INC),POBS,PEST,ECON,RCON,NCON
 4870    FORMAT(1X,' KTRN(',I1,')',2X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      IF(.NOT.CCODE(17)) WRITE(51,4880) INC,KTRN(INC)
 4880 FORMAT('KTRN(',I1,')=',F9.5)
 4860 CONTINUE
C
C----------------------------------------------------------------------
C     Calculate Shared Ride Constants
C
      WRITE(26,4750)
 4750 FORMAT(/15X,'Shared Ride Constants',/,
     *        15X,'---------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO 4760 INC=1,NCATS
         DENOBS=0.0
         DENEST=0.0
         POBS=0.0
         PEST=0.0
         DENOBS=COESUM(1,INC)+COESUM(2,INC)+COESUM(3,INC)+
     *          COESUM(4,INC)+DENOBS
         DENEST=CALSUM(1,INC)+CALSUM(2,INC)+CALSUM(3,INC)+
     *          CALSUM(4,INC)+DENEST
         POBS=COESUM(2,INC)+COESUM(3,INC)+POBS+COESUM(4,INC)
         PEST=CALSUM(2,INC)+CALSUM(3,INC)+PEST+CALSUM(4,INC)
         POBS=POBS/DENOBS
         PEST=PEST/DENEST
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KSR(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KSR(INC)=NCON
         WRITE(26,4770) INC,POBS,PEST,ECON,RCON,NCON
 4770    FORMAT(1X,' KSR(',I1,')',3X,2(1X,F7.4),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4761) INC,KSR(INC)
 4761 FORMAT('KSR(',I1,')=',F9.5)
 4760 CONTINUE
C
C----------------------------------------------------------------------
C     Calculate 3+ Person Auto Constants
C
      WRITE(26,4550)
 4550 FORMAT(/15X,'3+ Person Auto Constants',/,
     *        15X,'------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO 2560 INC=1,NCATS
         DENOBS=0.0
         DENEST=0.0
         POBS=0.0
         PEST=0.0
         DENOBS=COESUM(2,INC)+COESUM(3,INC)+DENOBS+
     *          COESUM(4,INC)
         DENEST=CALSUM(2,INC)+CALSUM(3,INC)+DENEST+
     *          CALSUM(4,INC)
         POBS=COESUM(3,INC)+POBS
         PEST=CALSUM(3,INC)+PEST
         POBS=POBS/DENOBS
         PEST=PEST/DENEST
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=K3P(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         K3P(INC)=NCON
         WRITE(26,4570) INC,POBS,PEST,ECON,RCON,NCON
 4570    FORMAT(1X,' K3P(',I1,')',3X,2(1X,F7.4),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4561) INC,K3P(INC)
 4561 FORMAT('K3P(',I1,')=',F9.5)
 2560 CONTINUE
C
C----------------------------------------------------------------------
C     Calculate 4+ Person Auto Constants
C
      WRITE(26,4551)
 4551 FORMAT(/15X,'4+ Person Auto Constants',/,
     *        15X,'------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO 2563 INC=1,NCATS
         DENOBS=0.0
         DENEST=0.0
         POBS=0.0
         PEST=0.0
         DENOBS=COESUM(2,INC)+COESUM(3,INC)+DENOBS+
     *          COESUM(4,INC)
         DENEST=CALSUM(2,INC)+CALSUM(3,INC)+DENEST+
     *          CALSUM(4,INC)
         POBS=COESUM(4,INC)+POBS
         PEST=CALSUM(4,INC)+PEST
         POBS=POBS/DENOBS
         PEST=PEST/DENEST
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=K4P(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,-40.0)
         K4P(INC)=NCON
         WRITE(26,4571) INC,POBS,PEST,ECON,RCON,NCON
 4571    FORMAT(1X,' K4P(',I1,')',3X,2(1X,F7.4),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4562) INC,K4P(INC)
 4562 FORMAT('K4P(',I1,')=',F9.5)
 2563 CONTINUE
      END IF
C
C     Calculate Drive to Local Bus Constants
C
      IF(CCODE(15)) THEN
      WRITE(26,4851)
 4851 FORMAT(/15X,'Drive to Local Bus Constants',/,
     *        15X,'----------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      POBS=0.0
      PEST=0.0
      DENOM=0.0
      NCON=0.0
   	   DO 4861 INC=1,NCATS
       POBS=POBS+OESUM(8,INC)
       PEST=PEST+TESUM(8,INC)
 4861  CONTINUE
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KINFL
         NCON=ECON+ADJFCT*RCON
         NCON=DMAX1(NCON,MINVAL)
         IF(POBS.EQ.0.0) NCON=MINVAL
         KINFL=NCON
         WRITE(26,4871) POBS,PEST,ECON,RCON,NCON
 4871    FORMAT(1X,' KINFL',4X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4881) KINFL
 4881 FORMAT('KINFL=',F9.5)
      END IF
C
C     Calculate Drive to Rapid Bus Constants
C
      IF(CCODE(16)) THEN
      WRITE(26,4852)
 4852 FORMAT(/15X,'Drive to Rapid Bus Constants',/,
     *        15X,'----------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      POBS=0.0
      PEST=0.0
	DO 4862 INC=1,NCATS
       POBS=POBS+OESUM(22,INC)
       PEST=PEST+TESUM(49,INC)
 4862 CONTINUE
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KINFR
         NCON=ECON+ADJFCT*RCON
         NCON=DMAX1(NCON,MINVAL)
         IF(POBS.EQ.0.0) NCON=MINVAL
         KINFR=NCON
         WRITE(26,4872) POBS,PEST,ECON,RCON,NCON
 4872    FORMAT(1X,' KINFR',4X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4882) KINFR
 4882 FORMAT('KINFR=',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Drive to Transit Constants
C
      IF(CCODE(2)) THEN
      WRITE(26,4611)
 4611 FORMAT(/15X,'Drive to Transit Constants',/,
     *        15X,'--------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO 4620 INC=1,NCATS
            POBS=OESUM(8,INC)+OESUM(10,INC)+OESUM(13,INC)+
     *           OESUM(14,INC)+OESUM(17,INC)+OESUM(18,INC)+
     *           OESUM(20,INC)+OESUM(22,INC)+OESUM(26,INC)
            PEST=TESUM(8,INC)+TESUM(10,INC)+TESUM(13,INC)+
     *           TESUM(14,INC)+TESUM(17,INC)+TESUM(18,INC)+
     *           TESUM(20,INC)+TESUM(49,INC)+TESUM(57,INC)
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KDTRN(INC)
         NCON=ECON+ADJFCT*RCON
         NCON=DMAX1(NCON,MINVAL)
         IF(POBS.EQ.0.0) NCON=MINVAL
         KDTRN(INC)=NCON
         WRITE(26,4631) (INC),POBS,PEST,ECON,RCON,NCON
 4631    FORMAT(1X,'KDTRN(',I1,')',2X,2(1X,F7.0),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4641) INC,KDTRN(INC)
 4641 FORMAT('KDTRN(',I1,')=',F9.5)
 4620 CONTINUE 
      END IF
C
C----------------------------------------------------------------------
C     Calculate Commuter Rail Constants
C
      IF(CCODE(4)) THEN
      WRITE(26,4610)
 4610 FORMAT(/15X,'Commuter Rail Constants',/,
     *        15X,'-----------------------',/,
     *  1X,' Market'/
     *  1X,'Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=0.0
         PEST=0.0
      DO 2620 INC=1,NCATS
      POBS=POBS+OESUM(11,INC)+OESUM(12,INC)+OESUM(13,INC)+OESUM(14,INC)+
     *         OESUM(33,INC)
      PEST=PEST+TESUM(11,INC)+TESUM(12,INC)+TESUM(13,INC)+TESUM(14,INC)+
     *         TESUM(74,INC)
 2620 CONTINUE
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KCR(1)
         NCON=ECON+ADJFCT*RCON
         NCON=DMAX1(NCON,MINVAL)
         IF(POBS.EQ.0.0) NCON=MINVAL
         KCR(1)=NCON
         KCR(2)=NCON
         KCR(3)=NCON
         KCR(4)=NCON
         KCR(5)=NCON
         EQUIVT=(-1.0*NCON)/COEFF(100)
         WRITE(26,4630) POBS,PEST,ECON,RCON,NCON,EQUIVT
 4630    FORMAT(1X,'KCR ' ' ',5X,2(1X,F7.0),1X,
     A                F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      WRITE(51,4640) KCR
 4640 FORMAT('KCR=',F9.5,4(',',F9.5))
      END IF
C
C----------------------------------------------------------------------
C     Calculate Urban Rail Constants
C
      IF(CCODE(5)) THEN
      WRITE(26,5610)
 5610 FORMAT(/15X,'Urban Rail Constants',/,
     *        15X,'--------------------',/,
     *  1X,' Market'/
     *  1X,'Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=0.0
         PEST=0.0
      DO 5620 INC=1,NCATS
      POBS=POBS+OESUM(15,INC)+OESUM(16,INC)+OESUM(17,INC)+OESUM(18,INC)+
     *         OESUM(34,INC)
      PEST=PEST+TESUM(15,INC)+TESUM(16,INC)+TESUM(17,INC)+TESUM(18,INC)+
     *         TESUM(75,INC)
 5620 CONTINUE
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KUR(1)
         NCON=ECON+ADJFCT*RCON
         NCON=DMAX1(NCON,MINVAL)
         IF(POBS.EQ.0.0) NCON=MINVAL
         KUR(1)=NCON
         KUR(2)=NCON
         KUR(3)=NCON
         KUR(4)=NCON
         KUR(5)=NCON
         EQUIVT=(-1.0*NCON)/COEFF(100)
         WRITE(26,5630) POBS,PEST,ECON,RCON,NCON,EQUIVT
 5630    FORMAT(1X,'KUR ' ' ',5X,2(1X,F7.0),1X,
     A      F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      WRITE(51,5640) KUR
 5640 FORMAT('KUR=',F9.5,4(',',F9.5))
      END IF
C
C----------------------------------------------------------------------
C     Calculate Express Bus Constants
C
      IF(CCODE(6)) THEN
      WRITE(26,6650)
 6650 FORMAT(/15X,'Express Bus Constants',/,
     *        15X,'------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=0.0
         PEST=0.0
      DO 6620 INC=1,NCATS
         POBS=POBS+OESUM(9,INC)+OESUM(10,INC)+OESUM(30,INC)
         PEST=PEST+TESUM(9,INC)+TESUM(10,INC)+TESUM(70,INC)
 6620 CONTINUE
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KEBUS(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KEBUS(1)=NCON
         KEBUS(2)=NCON
         KEBUS(3)=NCON
         KEBUS(4)=NCON
         KEBUS(5)=NCON
         EQUIVT=(-1.0*NCON)/COEFF(100)
         WRITE(26,6670) POBS,PEST,ECON,RCON,NCON,EQUIVT
 6670    FORMAT(1X,'KEBUS' ' ',4X,2(1X,F7.0),1X,
     A        F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      WRITE(51,6680) KEBUS
 6680 FORMAT('KEBUS=',F9.5,4(',',F9.5))
      END IF
C----------------------------------------------------------------------
C     Calculate Transitway Bus Constants
C
      IF(CCODE(7)) THEN
      WRITE(26,7650)
 7650 FORMAT(/15X,'Transitway Bus Constants',/,
     *        15X,'------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=0.0
         PEST=0.0
      DO 7660 INC=1,NCATS
         POBS=POBS+OESUM(19,INC)+OESUM(20,INC)+OESUM(31,INC)
         PEST=PEST+TESUM(19,INC)+TESUM(20,INC)+TESUM(68,INC)
 7660 CONTINUE
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KWAY(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KWAY(1)=NCON
         KWAY(2)=NCON
         KWAY(3)=NCON
         KWAY(4)=NCON
         KWAY(5)=NCON
         EQUIVT=(-1.0*NCON)/COEFF(100)
         WRITE(26,7670) POBS,PEST,ECON,RCON,NCON,EQUIVT
 7670    FORMAT(1X,'KWAY ' ' ',4X,2(1X,F7.0),1X,
     A        F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      WRITE(51,7680) KWAY
 7680 FORMAT('KWAY=',F9.5,4(',',F9.5))
      END IF
C
C
C----------------------------------------------------------------------
C     Calculate BRT Constants
C
      IF(CCODE(19)) THEN
      WRITE(26,5712)
 5712 FORMAT(/15X,'BRT Constants',/,
     *        15X,'-------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=0.0
         PEST=0.0
      DO 5722 INC=1,NCATS
         POBS=POBS+OESUM(25,INC)+OESUM(26,INC)+OESUM(32,INC)
         PEST=PEST+TESUM(56,INC)+TESUM(57,INC)+TESUM(72,INC)
 5722 CONTINUE
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KBRT(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBRT(1)=NCON
         KBRT(2)=NCON
         KBRT(3)=NCON
         KBRT(4)=NCON
         KBRT(5)=NCON
         EQUIVT=(-1.0*NCON)/COEFF(100)
         WRITE(26,5732) POBS,PEST,ECON,RCON,NCON,EQUIVT
 5732    FORMAT(1X,'KBRT   ',3X,2(1X,F7.0),1X,
     A               F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      WRITE(51,5742) KBRT
 5742 FORMAT('KBRT=',F9.5,4(',',F9.5))
      END IF
C
C----------------------------------------------------------------------
C     Calculate Rapid Bus Constants
C
      IF(CCODE(20)) THEN
      WRITE(26,5710)
5710  FORMAT(/15X,'Rapid Bus Constants',/,
     *        15X,'------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=0.0
         PEST=0.0
      DO 5723 INC=1,NCATS
         POBS=POBS+OESUM(21,INC)+OESUM(22,INC)+OESUM(29,INC)
         PEST=PEST+TESUM(48,INC)+TESUM(49,INC)+TESUM(66,INC)
 5723 CONTINUE
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KRBUS(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KRBUS(1)=NCON
         KRBUS(2)=NCON
         KRBUS(3)=NCON
         KRBUS(4)=NCON
         KRBUS(5)=NCON
         EQUIVT=(-1.0*NCON)/COEFF(100)
         WRITE(26,5930) POBS,PEST,ECON,RCON,NCON,EQUIVT
 5930    FORMAT(1X,'KRBUS  ',3X,2(1X,F7.0),1X,
     A                F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      WRITE(51,5740) KRBUS
 5740 FORMAT('KRBUS=',F9.5,4(',',F9.5))
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bike to Commuter Rail
C
      IF(CCODE(8)) THEN
      WRITE(26,4710)
 4710 FORMAT(/15X,'Bike to Commuter Rail',/,
     *        15X,'---------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=0.0
         PEST=0.0
      DO 4720 INC=1,NCATS
         POBS=POBS+OESUM(33,INC)
         PEST=PEST+TESUM(74,INC)
 4720 CONTINUE
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KBKCR(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBKCR(1)=NCON
         KBKCR(2)=NCON
         KBKCR(3)=NCON
         KBKCR(4)=NCON
         KBKCR(5)=NCON
         WRITE(26,4730) POBS,PEST,ECON,RCON,NCON
 4730    FORMAT(1X,'KBKCR' ' ',4X,2(1X,F7.0),1X,
     A         F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4740) KBKCR
 4740 FORMAT('KBKCR=',F9.5,4(',',F9.5))
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bike to Urban Rail
C
      IF(CCODE(9)) THEN
      WRITE(26,4715)
 4715 FORMAT(/15X,'Bike to Urban Rail',/,
     *        15X,'------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=0.0
         PEST=0.0
      DO INC=1,NCATS
         POBS=POBS+OESUM(34,INC)
         PEST=PEST+TESUM(75,INC)
      END DO
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KBKUR(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBKUR(1)=NCON
         KBKUR(2)=NCON
         KBKUR(3)=NCON
         KBKUR(4)=NCON
         KBKUR(5)=NCON
         WRITE(26,4735) POBS,PEST,ECON,RCON,NCON
 4735    FORMAT(1X,'KBKUR' ' ',4X,2(1X,F7.0),1X,
     A         F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4745) KBKUR
 4745 FORMAT('KBKUR=',F9.5,4(',',F9.5))
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bike to Local Bus
C
      IF(CCODE(27)) THEN
      WRITE(26,4711)
 4711 FORMAT(/15X,'Bike to Local Bus',/,
     *        15X,'-----------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=0.0
         PEST=0.0
         DO INC=1,NCATS
         POBS=POBS+OESUM(28,INC)
         PEST=PEST+TESUM(65,INC)
         END DO
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KBBUS(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBBUS(1)=NCON
         KBBUS(2)=NCON
         KBBUS(3)=NCON
         KBBUS(4)=NCON
         KBBUS(5)=NCON
         WRITE(26,4731) POBS,PEST,ECON,RCON,NCON
 4731    FORMAT(1X,'KBBUS' ' ',4X,2(1X,F7.0),1X,
     A         F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4741) KBBUS
 4741 FORMAT('KBBUS=',F9.5,4(',',F9.5))
      END IF
C
C
C----------------------------------------------------------------------
C     Calculate Bike to Rapid Bus
C
      IF(CCODE(28)) THEN
      WRITE(26,4712)
 4712 FORMAT(/15X,'Bike to Rapid Bus',/,
     *        15X,'-----------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=0.0
         PEST=0.0
         DO INC=1,NCATS
         POBS=POBS+OESUM(29,INC)
         PEST=PEST+TESUM(66,INC)
         END DO
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KBRPD(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBRPD(1)=NCON
         KBRPD(2)=NCON
         KBRPD(3)=NCON
         KBRPD(4)=NCON
         KBRPD(5)=NCON
         WRITE(26,4732) POBS,PEST,ECON,RCON,NCON
 4732    FORMAT(1X,'KBRPD' ' ',4X,2(1X,F7.0),1X,
     A         F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4742) KBRPD
 4742 FORMAT('KBRPD=',F9.5,4(',',F9.5))
      END IF
C
C
C----------------------------------------------------------------------
C     Calculate Bike to Transitway
C
      IF(CCODE(29)) THEN
      WRITE(26,4713)
 4713 FORMAT(/15X,'Bike to Transitway',/,
     *        15X,'------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=0.0
         PEST=0.0
         DO INC=1,NCATS
         POBS=POBS+OESUM(31,INC)
         PEST=PEST+TESUM(68,INC)
         END DO
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KBWAY(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBWAY(1)=NCON
         KBWAY(2)=NCON
         KBWAY(3)=NCON
         KBWAY(4)=NCON
         KBWAY(5)=NCON
         WRITE(26,4733) POBS,PEST,ECON,RCON,NCON
 4733    FORMAT(1X,'KBWAY' ' ',4X,2(1X,F7.0),1X,
     A         F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4743) KBWAY
 4743 FORMAT('KBWAY=',F9.5,4(',',F9.5))
      END IF
C
C
C
C----------------------------------------------------------------------
C     Calculate Bike to Express Bus
C
      IF(CCODE(30)) THEN
      WRITE(26,4714)
 4714 FORMAT(/15X,'Bike to Express Bus',/,
     *        15X,'-------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=0.0
         PEST=0.0
         DO INC=1,NCATS
         POBS=POBS+OESUM(30,INC)
         PEST=PEST+TESUM(70,INC)
         END DO
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KBEXP(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBEXP(1)=NCON
         KBEXP(2)=NCON
         KBEXP(3)=NCON
         KBEXP(4)=NCON
         KBEXP(5)=NCON
         WRITE(26,4734) POBS,PEST,ECON,RCON,NCON
 4734    FORMAT(1X,'KBEXP' ' ',4X,2(1X,F7.0),1X,
     A         F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4744) KBEXP
 4744 FORMAT('KBEXP=',F9.5,4(',',F9.5))
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bike to BRT
C
      IF(CCODE(31)) THEN
      WRITE(26,4716)
 4716 FORMAT(/15X,'Bike to BRT',/,
     *        15X,'-----------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=0.0
         PEST=0.0
         DO INC=1,NCATS
         POBS=POBS+OESUM(32,INC)
         PEST=PEST+TESUM(72,INC)
         END DO
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KBBRT(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBBRT(1)=NCON
         KBBRT(2)=NCON
         KBBRT(3)=NCON
         KBBRT(4)=NCON
         KBBRT(5)=NCON
         WRITE(26,4736) POBS,PEST,ECON,RCON,NCON
 4736    FORMAT(1X,'KBBRT' ' ',4X,2(1X,F7.0),1X,
     A         F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4746) KBBRT
 4746 FORMAT('KBBRT=',F9.5,4(',',F9.5))
      END IF
C
C
C----------------------------------------------------------------------
C     Calculate Bus to CR Constants
C
      IF(CCODE(10)) THEN
      WRITE(26,4110)
 4110 FORMAT(/15X,'Bus to Commuter Rail Constants',/,
     *        15X,'----------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      POBS=0.0
      PEST=0.0
      DO 4120 INC=1,NCATS
         POBS=POBS+OESUM(12,INC)
         PEST=PEST+TESUM(12,INC)
 4120 CONTINUE      
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KBCR(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBCR(1)=NCON
         KBCR(2)=NCON
         KBCR(3)=NCON
         KBCR(4)=NCON
         KBCR(5)=NCON
         WRITE(26,4130) POBS,PEST,ECON,RCON,NCON
 4130    FORMAT(1X,'KBCR   ',3X,2(1X,F7.0),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4140) KBCR
 4140 FORMAT('KBCR=',F9.5,4(',',F9.5))
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bus to UR Constants
C
      IF(CCODE(11)) THEN
      WRITE(26,4411)
 4411 FORMAT(/15X,'Bus to Urban Rail Constants',/,
     *        15X,'----------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      POBS=0.0
      PEST=0.0
      DENOBS=0.0
      DENEST=0.0
      DO INC=1,NCATS
         POBS=POBS+OESUM(16,INC)
         PEST=PEST+TESUM(16,INC)
      END DO
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KBUR(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBUR(1)=NCON
         KBUR(2)=NCON
         KBUR(3)=NCON
         KBUR(4)=NCON
         KBUR(5)=NCON
         WRITE(26,4431) POBS,PEST,ECON,RCON,NCON
 4431    FORMAT(1X,'KBUR   ',3X,2(1X,F7.0),1X,
     *                      F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4442) KBUR
 4442 FORMAT('KBUR=',F9.5,4(',',F9.5))
      END IF
C
C----------------------------------------------------------------------
C     Calculate Walk to UR Constants
C
      IF(CCODE(40)) THEN
      WRITE(26,7411)
 7411 FORMAT(/15X,'Walk to Urban Rail Constants',/,
     *        15X,'----------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      POBS=0.0
      PEST=0.0
      DO INC=1,NCATS
         POBS=POBS+OESUM(15,INC)
         PEST=PEST+TESUM(15,INC)
      END DO
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KWUR(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KWUR(1)=NCON
         KWUR(2)=NCON
         KWUR(3)=NCON
         KWUR(4)=NCON
         KWUR(5)=NCON
         WRITE(26,7431) POBS,PEST,ECON,RCON,NCON
 7431    FORMAT(1X,'KWUR   ',3X,2(1X,F7.0),1X,
     *                      F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,7442) KWUR
 7442 FORMAT('KWUR=',F9.5,4(',',F9.5))
      END IF
C
C----------------------------------------------------------------------
C     Calculate Rapid Bus Transfer Constants
C
      IF(CCODE(12)) THEN
      WRITE(26,5711)
 5711 FORMAT(/15X,'Walk to Rapid Bus Transfer Constant',/,
     *        15X,'-----------------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
      DO 5721 INC=1,NCATS
         TE=TE+TESUM(53,INC)
         DENEST=DENEST+TESUM(48,INC)
 5721 CONTINUE
         POBS=WLKRAPID
         IF(DENEST.GT.0) THEN
            PEST=TE/DENEST
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KBRPDW
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBRPDW=NCON
         WRITE(26,5731) POBS,PEST,ECON,RCON,NCON
 5731    FORMAT(1X,'KBRPDW ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,5741) KBRPDW
 5741 FORMAT('KBRPDW=',F9.5)
      END IF
C....
      IF(CCODE(22)) THEN
      WRITE(26,5781)
 5781 FORMAT(/15X,'Drive to Rapid Bus Transfer Constant',/,
     *        15X,'------------------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
      DO 5782 INC=1,NCATS
         TE=TE+TESUM(60,INC)
         DENEST=DENEST+TESUM(49,INC)
 5782 CONTINUE
         POBS=DRVRAPID
         IF(DENEST.GT.0) THEN
            PEST=TE/DENEST
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KBRPDD
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBRPDD=NCON
         WRITE(26,5783) POBS,PEST,ECON,RCON,NCON
 5783    FORMAT(1X,'KBRPDD ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,5784) KBRPDD
 5784 FORMAT('KBRPDD=',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Walk Access Transitway Transfer Constants
C
      IF(CCODE(13)) THEN
      WRITE(26,5811)
 5811 FORMAT(/15X,'Transitway Walk Access Transfer Constant',/,
     *        15X,'----------------------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
      DO 5821 INC=1,NCATS
         TE=TE+TESUM(54,INC)
         DENEST=DENEST+TESUM(19,INC)
 5821 CONTINUE
         POBS=WLKTWY
         IF(DENEST.GT.0) THEN
            PEST=TE/DENEST
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KBTWYW
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBTWYW=NCON
         WRITE(26,5831) POBS,PEST,ECON,RCON,NCON
 5831    FORMAT(1X,'KBTWYW ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,5841) KBTWYW
 5841 FORMAT('KBTWYW=',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Walk Access Express Bus Transfer Constants
C
      IF(CCODE(14)) THEN
      WRITE(26,5911)
 5911 FORMAT(/15X,'Express Bus Walk Access Transfer Constant',/,
     *        15X,'-----------------------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
      DO 5921 INC=1,NCATS
         TE=TE+TESUM(55,INC)
         DENEST=DENEST+TESUM(9,INC)
 5921 CONTINUE
         POBS=WLKEXP
         IF(DENEST.GT.0) THEN
            PEST=TE/DENEST
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KBEXPW
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBEXPW=NCON
         WRITE(26,5931) POBS,PEST,ECON,RCON,NCON
 5931    FORMAT(1X,'KBEXPW ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,5941) KBEXPW
 5941 FORMAT('KBEXPW=',F9.5)
      END IF
C----------------------------------------------------------------------
C     Calculate BRT Bus Transfer Constants
C
      IF(CCODE(21)) THEN
      WRITE(26,5411)
 5411 FORMAT(/15X,'Walk to BRT Bus Transfer Constant',/,
     *        15X,'---------------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
      DO 5421 INC=1,NCATS
         TE=TE+TESUM(58,INC)
         DENEST=DENEST+TESUM(56,INC)
 5421 CONTINUE
         POBS=WLKBRT
         IF(DENEST.GT.0) THEN
            PEST=TE/DENEST
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KBBRTW
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBBRTW=NCON
         WRITE(26,5431) POBS,PEST,ECON,RCON,NCON
 5431    FORMAT(1X,'KBBRTW ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,5441) KBBRTW
 5441 FORMAT('KBBRTW=',F9.5)
      END IF
      IF(CCODE(23)) THEN
      WRITE(26,5481)
 5481 FORMAT(/15X,'Drive to BRT Bus Transfer Constant',/,
     *        15X,'------------------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
      DO 5482 INC=1,NCATS
         TE=TE+TESUM(61,INC)
         DENEST=DENEST+TESUM(57,INC)
 5482 CONTINUE
         POBS=DRVBRT
         IF(DENEST.GT.0) THEN
            PEST=TE/DENEST
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KBBRTD
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBBRTD=NCON
         WRITE(26,5483) POBS,PEST,ECON,RCON,NCON
 5483    FORMAT(1X,'KBBRTD ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,5484) KBBRTD
 5484 FORMAT('KBBRTD=',F9.5)
      END IF
C----------------------------------------------------------------------
C     Calculate Drive Access Transitway Transfer Constants
C
      IF(CCODE(25)) THEN
      WRITE(26,5812)
 5812 FORMAT(/15X,'Transitway Drive Access Transfer Constant',/,
     *        15X,'-----------------------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
      DO 5822 INC=1,NCATS
         TE=TE+TESUM(62,INC)
         DENEST=DENEST+TESUM(20,INC)
 5822 CONTINUE
         POBS=DRVTWY
         IF(DENEST.GT.0) THEN
            PEST=TE/DENEST
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KBTWYD
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBTWYD=NCON
         WRITE(26,5832) POBS,PEST,ECON,RCON,NCON
 5832    FORMAT(1X,'KBTWYD ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,5842) KBTWYD
 5842 FORMAT('KBTWYD=',F9.5)

      END IF
C
C----------------------------------------------------------------------
C     Calculate Drive Access Express Bus Transfer Constants
C
      IF(CCODE(26)) THEN
      WRITE(26,5912)
 5912 FORMAT(/15X,'Express Bus Drive Access Transfer Constant',/,
     *        15X,'------------------------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
      DO 5922 INC=1,NCATS
         TE=TE+TESUM(63,INC)
         DENEST=DENEST+TESUM(10,INC)
 5922 CONTINUE
         POBS=DRVEXP
         IF(DENEST.GT.0) THEN
            PEST=TE/DENEST
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KBEXPD
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBEXPD=NCON
         WRITE(26,5932) POBS,PEST,ECON,RCON,NCON
 5932    FORMAT(1X,'KBEXPD ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,5945) KBEXPD
 5945 FORMAT('KBEXPD=',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bike Access Rapid Bus Transfer Constants
C
      IF(CCODE(18)) THEN
      WRITE(26,5730)
 5730 FORMAT(/15X,'Rapid Bus Bike Access Transfer Constant',/,
     *        15X,'---------------------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
         DO INC=1,NCATS
         TE=TE+TESUM(67,INC)
         DENEST=DENEST+TESUM(66,INC)
         END DO
         POBS=BIKRAPID
         IF(DENEST.GT.0) THEN
            PEST=TE/DENEST
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KBRPDB
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBRPDB=NCON
         WRITE(26,5530) POBS,PEST,ECON,RCON,NCON
 5530    FORMAT(1X,'KBRPDB ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,5830) KBRPDB
 5830 FORMAT('KBRPDB=',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bike Access Transitway Bus Transfer Constants
C
      IF(CCODE(32)) THEN
      WRITE(26,7731)
 7731 FORMAT(/15X,'Transitway Bus Bike Access Transfer Constant',/,
     *        15X,'--------------------------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
         DO INC=1,NCATS
         TE=TE+TESUM(69,INC)
         DENEST=DENEST+TESUM(68,INC)
         END DO
         POBS=BIKTWY
         IF(DENEST.GT.0) THEN
            PEST=TE/DENEST
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KBTWYB
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBTWYB=NCON
         WRITE(26,7732) POBS,PEST,ECON,RCON,NCON
 7732    FORMAT(1X,'KBTWYB ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,7733) KBTWYB
 7733 FORMAT('KBTWYB=',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bike Access Express Bus Transfer Constants
C
      IF(CCODE(33)) THEN
      WRITE(26,7734)
 7734 FORMAT(/15X,'Express Bus Bike Access Transfer Constant',/,
     *        15X,'-----------------------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
         DO INC=1,NCATS
         TE=TE+TESUM(71,INC)
         DENEST=DENEST+TESUM(70,INC)
         END DO
         POBS=BIKEXP
         IF(DENEST.GT.0) THEN
            PEST=TE/DENEST
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KBEXPB
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBEXPB=NCON
         WRITE(26,7735) POBS,PEST,ECON,RCON,NCON
 7735    FORMAT(1X,'KBEXPB ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,7736) KBEXPB
 7736 FORMAT('KBEXPB=',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bike Access BRT Transfer Constants
C
      IF(CCODE(34)) THEN
      WRITE(26,7737)
 7737 FORMAT(/15X,'BRT Bike Access Transfer Constant',/,
     *        15X,'---------------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
         DO INC=1,NCATS
         TE=TE+TESUM(73,INC)
         DENEST=DENEST+TESUM(72,INC)
         END DO
         POBS=BIKBRT
         IF(DENEST.GT.0) THEN
            PEST=TE/DENEST
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KBBRTB
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBBRTB=NCON
         WRITE(26,7738) POBS,PEST,ECON,RCON,NCON
 7738    FORMAT(1X,'KBBRTB ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,7739) KBBRTB
 7739 FORMAT('KBBRTB=',F9.5)
      END IF
C----------------------------------------------------------------------
C     Calculate Overall CBD Constant for Transit
C
      IF(CCODE(24)) THEN
      WRITE(26,5713)
 5713 FORMAT(/15X,'Overall CBD Constant for Transit',/,
     *        15X,'--------------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'            Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=OBCBDTR
         PEST=CBDTRAN
         RCON=LOG(POBS/PEST)
         ECON=CCBD(1)
         NCON=ECON+ADJFCT*RCON
         CCBD(1)=NCON
         CCBD(2)=NCON
         CCBD(3)=NCON
         CCBD(4)=NCON
         CCBD(5)=NCON
         CCBD(6)=NCON
         CCBD(7)=NCON
         WRITE(26,5733) POBS,PEST,ECON,RCON,NCON
 5733    FORMAT(1X,'CCBD   ',3X,2(1X,F7.0),1X,
     A               F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,5743) CCBD
 5743 FORMAT('CCBD=',F9.5,6(',',F9.5))
      ELSE
      WRITE(26,5714)
 5714 FORMAT(/15X,'Overall CBD Comparison for Transit',/,
     *        15X,'----------------------------------',/,
     *  1X,'             Obs     Est  '/
     *  1X,'            Value   Value '/
     *  1X,'---------  ------- -------')
      POBS=OBCBDTR
      PEST=CBDTRAN
      WRITE(26,5734) POBS,PEST
 5734 FORMAT(1X,'CCBD   ',3X,2(1X,F7.0))      
      END IF
C
C----------------------------------------------------------------------
C     Calculate Intrazonal Walk Constants
C
      IF(CCODE(35)) THEN
      WRITE(26,2612)
 2612 FORMAT(/15X,'Intrazonal Walk',/,
     *        15X,'---------------',/,
     *  1X,' Market'/
     *  1X,'Segment        Obs     Est    Existing               New'/
     *  1X,' Level       Percent Percent  Constant Adjustment  ',
     *      'Constant'/
     *  1X,'---------    ------- ------- --------- ---------- ',
     *     '---------')
      DO 2613 INC=1,NCATS
          POBS=IWALK(INC)
          PEST=INTRATRP(5,INC)/INTRATRP(7,INC)
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KINTRAWK(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=-25.0
         NCON=DMAX1(NCON,-50.0)
         KINTRAWK(INC)=NCON
         WRITE(26,2614) (INC),POBS,PEST,ECON,RCON,NCON
 2614    FORMAT(1X,'KINTRAWK(',I1,')',1X,2(1X,F7.4),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,2615) INC,KINTRAWK(INC)
 2615 FORMAT('KINTRAWK(',I1,')=',F9.5)
 2613 CONTINUE 
      END IF
C
C----------------------------------------------------------------------
C     Calculate Intrazonal Bicycle Constants
C
      IF(CCODE(36)) THEN
      WRITE(26,2712)
 2712 FORMAT(/15X,'Intrazonal Bike',/,
     *        15X,'---------------',/,
     *  1X,' Market'/
     *  1X,'Segment        Obs     Est    Existing               New'/
     *  1X,' Level       Percent Percent  Constant Adjustment  ',
     *      'Constant'/
     *  1X,'---------    ------- ------- --------- ---------- ',
     *     '---------')
      DO 2713 INC=1,NCATS
          POBS=IBIKE(INC)
          PEST=INTRATRP(6,INC)/INTRATRP(7,INC)
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KINTRABK(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=-25.0
         NCON=DMAX1(NCON,-50.0)
         KINTRABK(INC)=NCON
         WRITE(26,2714) (INC),POBS,PEST,ECON,RCON,NCON
 2714    FORMAT(1X,'KINTRABK(',I1,')',1X,2(1X,F7.4),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,2715) INC,KINTRABK(INC)
 2715 FORMAT('KINTRABK(',I1,')=',F9.5)
 2713 CONTINUE 
      END IF
C
C----------------------------------------------------------------------
C     Calculate PNR to Urban Rail Constant
C
      IF(CCODE(37)) THEN
      WRITE(26,4421)
 4421 FORMAT(/15X,'Park-n-Ride to Urban Rail Constants',/,
     *        15X,'-----------------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      POBS=0.0
      PEST=0.0
      DO  INC=1,NCATS
         POBS=POBS+OESUM(17,INC)
         PEST=PEST+TESUM(17,INC)
      END DO
         RCON=0.0
         IF(PEST.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KPUR(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KPUR(1)=NCON
         KPUR(2)=NCON
         KPUR(3)=NCON
         KPUR(4)=NCON
         KPUR(5)=NCON
         WRITE(26,4441) POBS,PEST,ECON,RCON,NCON
 4441    FORMAT(1X,'KPUR   ',3X,2(1X,F7.0),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4443) KPUR
 4443 FORMAT('KPUR=',F9.5,4(',',F9.5))
      END IF
C----------------------------------------------------------------------
C     Calculate Walk to Commuter Rail Constants
C
      IF(CCODE(38)) THEN
      WRITE(26,4310)
 4310 FORMAT(/15X,'Walk to Commuter Rail Constants',/,
     *        15X,'----------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level       Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      POBS=0.0
      PEST=0.0
      DO INC=1,NCATS
         POBS=POBS+OESUM(11,INC)
         PEST=PEST+TESUM(11,INC)
      END DO      
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KWCR(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KWCR(1)=NCON
         KWCR(2)=NCON
         KWCR(3)=NCON
         KWCR(4)=NCON
         KWCR(5)=NCON
         WRITE(26,4311) POBS,PEST,ECON,RCON,NCON
 4311    FORMAT(1X,'KWCR   ',3X,2(1X,F7.0),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4312) KWCR
 4312 FORMAT('KWCR=',F9.5,4(',',F9.5))
      END IF
C
C----------------------------------------------------------------------
C     Calculate Park-n-Ride to Transit Constants
C     Urban and Commuter Rail Only
C
      IF(CCODE(39)) THEN
      WRITE(26,4812)
 4812 FORMAT(/15X,'Park-n-Ride to Commuter/Urban Rail',/,
     *        15X,'----------------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO INC=1,NCATS
          POBS=OESUM(13,INC)+OESUM(17,INC)
          PEST=TESUM(13,INC)+TESUM(17,INC)
         RCON=0.0
         IF(PEST.GT.0.0.AND.POBS.GT.0.0) THEN
         RCON=DLOG(POBS/PEST)
         END IF
         ECON=KPNR(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KPNR(INC)=NCON
         WRITE(26,4813) (INC),POBS,PEST,ECON,RCON,NCON
 4813    FORMAT(1X,'KPNR(',I1,')',3X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4615) INC,KPNR(INC)
 4615 FORMAT('KPNR(',I1,')=',F9.5)
      END DO
      END IF
C
C
C----------------------------------------------------------------------
C     Calculate Walk to Transitway
C
      IF(CCODE(41)) THEN
      WRITE(26,4817)
 4817 FORMAT(/15X,'Walk to Transitway',/,
     *  1X,' Market'/
     *        15X,'------------------',/,
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DO INC=1,NCATS
         TO=TO+OESUM(19,INC)
         TE=TE+TESUM(19,INC)
         END DO
         RCON=0.0
         IF(TE.GT.0) THEN
            RCON=DLOG(TO/TE)
         END IF
         ECON=KWWAY(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KWWAY(1)=NCON
         KWWAY(2)=NCON
         KWWAY(3)=NCON
         KWWAY(4)=NCON
         KWWAY(5)=NCON
         WRITE(26,4815) TO,TE,ECON,RCON,NCON
 4815    FORMAT(1X,'KWWAY' ' ',4X,2(1X,F7.0),1X,
     *         F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4816) KWWAY
 4816 FORMAT('KWWAY=',F9.5,4(',',F9.5))
      END IF
C
C
C----------------------------------------------------------------------
C     Calculate Drive to Express Bus
C
      IF(CCODE(42)) THEN
      WRITE(26,4819)
 4819 FORMAT(/15X,'Drive to Express Bus',/,
     *  1X,' Market'/
     *        15X,'------------------',/,
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DO INC=1,NCATS
         TO=TO+OESUM(10,INC)
         TE=TE+TESUM(10,INC)
         END DO
         RCON=0.0
         IF(TE.GT.0) THEN
            RCON=DLOG(TO/TE)
         END IF
         ECON=KDEXP(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KDEXP(1)=NCON
         KDEXP(2)=NCON
         KDEXP(3)=NCON
         KDEXP(4)=NCON
         KDEXP(5)=NCON
         WRITE(26,4820) TO,TE,ECON,RCON,NCON
 4820    FORMAT(1X,'KDEXP' ' ',4X,2(1X,F7.0),1X,
     *         F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4821) KDEXP
 4821 FORMAT('KDEXP=',F9.5,4(',',F9.5))
      END IF
C
C----------------------------------------------------------------------
C     Calculate PNR to Commuter Rail Constant
C
      IF(CCODE(43)) THEN
      WRITE(26,4822)
 4822 FORMAT(/15X,'Park-n-Ride to Commuter Rail Constants',/,
     *        15X,'--------------------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      POBS=0.0
      PEST=0.0
      DO  INC=1,NCATS
         POBS=POBS+OESUM(13,INC)
         PEST=PEST+TESUM(13,INC)
      END DO
         RCON=0.0
         IF(PEST.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KPCR(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KPCR(1)=NCON
         KPCR(2)=NCON
         KPCR(3)=NCON
         KPCR(4)=NCON
         KPCR(5)=NCON
         WRITE(26,4823) POBS,PEST,ECON,RCON,NCON
 4823    FORMAT(1X,'KPCR   ',3X,2(1X,F7.0),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4824) KPCR
 4824 FORMAT('KPCR=',F9.5,4(',',F9.5))
      END IF
C
C----------------------------------------------------------------------
C     Calculate Drive Alone Constant - Market Segment 5 Only
C
      IF(CCODE(44)) THEN
      WRITE(26,4825)
 4825 FORMAT(/15X,'Drive Alone Constant',/,
     *        15X,'--------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs          Est    Existing               ',
     *     'New'/
     *  1X,' Level       Value       Value   Constant Adjustment  ',
     *     'Constant'/
     *  1X,'---------  ---------- ---------- --------- ---------- ',
     *     '---------')
      POBS=0.0
      PEST=0.0
      POBS=OESUM(1,5)
      PEST=TESUM(1,5)
         RCON=0.0
         IF(PEST.GT.0.AND.POBS.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KDA(5)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KDA(5)=NCON
         DENOBS=(1.0-(PEST/POBS))*100.0
         WRITE(26,4826) POBS,PEST,ECON,RCON,NCON,DENOBS
 4826    FORMAT(1X,'KDA(5) ',3X,2(1X,F10.0),1X,
     A                             F9.5,1X,F10.5,1X,F9.5/
     *          1X,' PERCENT ERROR=',F6.2,'%')
      WRITE(51,4827) KDA(5)
 4827 FORMAT('KDA(5)=',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate General Auto Constant - Market Segment 1 Only
C
      IF(CCODE(45)) THEN
      WRITE(26,4830)
 4830 FORMAT(/15X,'Auto Constant',/,
     *        15X,'-------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs          Est    Existing               ',
     *     'New'/
     *  1X,' Level       Value       Value   Constant Adjustment  ',
     *     'Constant'/
     *  1X,'---------  ---------- ---------- --------- ---------- ',
     *     '---------')
      POBS=0.0
      PEST=0.0
      POBS=OESUM(1,1)+OESUM(2,1)+OESUM(3,1)+OESUM(27,1)
      PEST=TESUM(1,1)+TESUM(2,1)+TESUM(3,1)+TESUM(64,1)
         RCON=0.0
         IF(PEST.GT.0.AND.POBS.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KAUT(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KAUT(1)=NCON
         DENOBS=(1.0-(PEST/POBS))*100.0
         WRITE(26,4828) POBS,PEST,ECON,RCON,NCON,DENOBS
 4828    FORMAT(1X,'KAUT(1)',3X,2(1X,F10.0),1X,
     A                             F9.5,1X,F10.5,1X,F9.5/
     *          1X,' PERCENT ERROR=',F6.2,'%')
      WRITE(51,4829) KAUT(1)
 4829 FORMAT('KAUT(1)=',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Walk to Local Bus Constants
C
      IF(CCODE(48)) THEN
      WRITE(26,4661)
 4661 FORMAT(/15X,'Walk to Local Bus Constants',/,
     *        15X,'--------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO INC=1,NCATS
            POBS=OESUM(7,INC)
            PEST=TESUM(7,INC)
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KWBUS(INC)
         NCON=ECON+ADJFCT*RCON
         NCON=DMAX1(NCON,MINVAL)
         IF(POBS.EQ.0.0) NCON=MINVAL
         KWBUS(INC)=NCON
         WRITE(26,4662) INC,POBS,PEST,ECON,RCON,NCON
 4662    FORMAT(1X,'KWBUS(',I1,')',2X,2(1X,F7.0),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,4663) INC,KWBUS(INC)
 4663 FORMAT('KWBUS(',I1,')=',F9.5)
      END DO 
      END IF
C
C     OUTPUT CONSTANT VALUES FOR THIS ITERATION
C
      WRITE(52,906) ITER,KTRN,KSR,K3P,K4P,KNMOT,KBIKE,
     *              KDTRN,KPNR,KCR(1),KUR(1),KEBUS(1),
     *              KWAY(1),KBKCR(1),KBKUR(1),KBCR(1),
     *              KBUR(1),KBRPDW,KBTWYW,KBEXPW,KINFL,
     *              KINFR,KBRPDB,KBRT(1),KRBUS(1),KBBRTW,
     *              KBRPDD,KBBRTD,CCBD(1),KBTWYD,KBEXPD,KBBUS(1),
     *              KBRPD(1),KBWAY(1),KBEXP(1),KBBRT(1),
     *              KBTWYB,KBEXPB,KBBRTB,KINTRAWK,KINTRABK,KPUR(1),
     *              KWCR(1)
  906 FORMAT(I3,90(',',F9.5))
      WRITE(53,907) ITER
  907 FORMAT('ITER=,',I3)
      DO IMO=1,21
      WRITE(53,908) IMO,(KTRNT(IMO,INC),INC=1,NCATS)
  908 FORMAT(I2,5(',',F9.5))
      END DO
C
C----------------------------------------------------------------------
C     Calculate Walk to Local Bus Transfer Constants
C
      IF(CCODE(46)) THEN
      WRITE(26,5913)
 5913 FORMAT(/15X,'Walk Local Bus Transfer Constant',/,
     *        15X,'--------------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'  Name     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=WLTXFER
         DENOM=BUSTXFER(1,1)+BUSTXFER(1,2)+BUSTXFER(1,3)+BUSTXFER(1,4)
         PEST=BUSTXFER(1,2)+BUSTXFER(1,3)+BUSTXFER(1,4)
         IF(DENOM.GT.0) THEN
         PEST=PEST/DENOM
         ELSE
         PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=COEFF(31)
         IF(RCON.GT.0.AND.ECON.EQ.0) RCON=0.0
         NCON=ECON+ADJFCT2*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         COEFF(31)=NCON
         WRITE(26,5933) 31,POBS,PEST,ECON,RCON,NCON
 5933    FORMAT(1X,'COEFF(',I2,')',1X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(51,5946) 31,COEFF(31)
 5946 FORMAT(4X,'COEFF(',I2,')=',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bike to Local Bus Transfer Constants
C
      IF(CCODE(47)) THEN
      WRITE(26,5914)
 5914 FORMAT(/15X,'Bike Local Bus Transfer Constant',/,
     *        15X,'--------------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'  Name     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=BLTXFER
         DENOM=BUSTXFER(6,1)+BUSTXFER(6,2)+BUSTXFER(6,3)+BUSTXFER(6,4)
         PEST=BUSTXFER(6,2)+BUSTXFER(6,3)+BUSTXFER(6,4)
         IF(DENOM.GT.0) THEN
         PEST=PEST/DENOM
         ELSE
         PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=COEFF(32)
         IF(RCON.GT.0.AND.ECON.EQ.0) RCON=0.0
         NCON=ECON+ADJFCT2*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         COEFF(32)=NCON
         WRITE(26,5933) 32,POBS,PEST,ECON,RCON,NCON
         WRITE(51,5946) 32,COEFF(32)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Drive to Local Bus Transfer Constants
C
      IF(CCODE(49)) THEN
      WRITE(26,5916)
 5916 FORMAT(/15X,'Drive Local Bus Transfer Constant',/,
     *        15X,'---------------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'  Name     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=DLTXFER
         DENOM=BUSTXFER(11,1)+BUSTXFER(11,2)+BUSTXFER(11,3)+
     *         BUSTXFER(11,4)
         PEST=BUSTXFER(11,2)+BUSTXFER(11,3)+BUSTXFER(11,4)
         IF(DENOM.GT.0) THEN
         PEST=PEST/DENOM
         ELSE
         PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=COEFF(33)
         IF(RCON.GT.0.AND.ECON.EQ.0) RCON=0.0
         NCON=ECON+ADJFCT2*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         COEFF(33)=NCON
         WRITE(26,5933) 33,POBS,PEST,ECON,RCON,NCON
         WRITE(51,5946) 33,COEFF(33)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Walk to Rapid Bus Transfer Constants
C
      IF(CCODE(50)) THEN
      WRITE(26,5917)
 5917 FORMAT(/15X,'Walk Rapid Bus Transfer Constant',/,
     *        15X,'--------------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'  Name     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=WRTXFER
         DENOM=BUSTXFER(4,1)+BUSTXFER(4,2)+BUSTXFER(4,3)+BUSTXFER(4,4)
         PEST=BUSTXFER(4,2)+BUSTXFER(4,3)+BUSTXFER(4,4)
         IF(DENOM.GT.0) THEN
         PEST=PEST/DENOM
         ELSE
         PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=COEFF(34)
         IF(RCON.GT.0.AND.ECON.EQ.0) RCON=0.0
         NCON=ECON+ADJFCT2*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         COEFF(34)=NCON
         WRITE(26,5933) 34,POBS,PEST,ECON,RCON,NCON
         WRITE(51,5946) 34,COEFF(34)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bike to Rapid Bus Transfer Constants
C
      IF(CCODE(51)) THEN
      WRITE(26,5918)
 5918 FORMAT(/15X,'Bike Rapid Bus Transfer Constant',/,
     *        15X,'--------------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'  Name     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=BRTXFER
         DENOM=BUSTXFER(9,1)+BUSTXFER(9,2)+BUSTXFER(9,3)+BUSTXFER(9,4)
         PEST=BUSTXFER(9,2)+BUSTXFER(9,3)+BUSTXFER(9,4)
         IF(DENOM.GT.0) THEN
         PEST=PEST/DENOM
         ELSE
         PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=COEFF(35)
         IF(RCON.GT.0.AND.ECON.EQ.0) RCON=0.0
         NCON=ECON+ADJFCT2*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         COEFF(35)=NCON
         WRITE(26,5933) 35,POBS,PEST,ECON,RCON,NCON
         WRITE(51,5946) 35,COEFF(35)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Drive to Rapid Bus Transfer Constants
C
      IF(CCODE(52)) THEN
      WRITE(26,5919)
 5919 FORMAT(/15X,'Drive Rapid Bus Transfer Constant',/,
     *        15X,'---------------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'  Name     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=DRTXFER
         DENOM=BUSTXFER(14,1)+BUSTXFER(14,2)+BUSTXFER(14,3)+
     *         BUSTXFER(14,4)
         PEST=BUSTXFER(14,2)+BUSTXFER(14,3)+BUSTXFER(14,4)
         IF(DENOM.GT.0) THEN
         PEST=PEST/DENOM
         ELSE
         PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=COEFF(36)
         IF(RCON.GT.0.AND.ECON.EQ.0) RCON=0.0
         NCON=ECON+ADJFCT2*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         COEFF(36)=NCON
         WRITE(26,5933) 36,POBS,PEST,ECON,RCON,NCON
         WRITE(51,5946) 36,COEFF(36)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Walk to Express Bus Transfer Constants
C
      IF(CCODE(53)) THEN
      WRITE(26,5920)
 5920 FORMAT(/15X,'Walk Express Bus Transfer Constant',/,
     *        15X,'----------------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'  Name     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=WETXFER
         DENOM=BUSTXFER(2,1)+BUSTXFER(2,2)+BUSTXFER(2,3)+BUSTXFER(2,4)
         PEST=BUSTXFER(2,2)+BUSTXFER(2,3)+BUSTXFER(2,4)
         IF(DENOM.GT.0) THEN
         PEST=PEST/DENOM
         ELSE
         PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=COEFF(37)
         IF(RCON.GT.0.AND.ECON.EQ.0) RCON=0.0
         NCON=ECON+ADJFCT2*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         COEFF(37)=NCON
         WRITE(26,5933) 37,POBS,PEST,ECON,RCON,NCON
         WRITE(51,5946) 37,COEFF(37)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bike to Express Bus Transfer Constants
C
      IF(CCODE(54)) THEN
      WRITE(26,5923)
 5923 FORMAT(/15X,'Bike Express Bus Transfer Constant',/,
     *        15X,'----------------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'  Name     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=BETXFER
         DENOM=BUSTXFER(7,1)+BUSTXFER(7,2)+BUSTXFER(7,3)+BUSTXFER(7,4)
         PEST=BUSTXFER(7,2)+BUSTXFER(7,3)+BUSTXFER(7,4)
         IF(DENOM.GT.0) THEN
         PEST=PEST/DENOM
         ELSE
         PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=COEFF(38)
         IF(RCON.GT.0.AND.ECON.EQ.0) RCON=0.0
         NCON=ECON+ADJFCT2*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         COEFF(38)=NCON
         WRITE(26,5933) 38,POBS,PEST,ECON,RCON,NCON
         WRITE(51,5946) 38,COEFF(38)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Drive to Express Bus Transfer Constants
C
      IF(CCODE(55)) THEN
      WRITE(26,5924)
 5924 FORMAT(/15X,'Drive Express Bus Transfer Constant',/,
     *        15X,'------------------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'  Name     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=DETXFER
         DENOM=BUSTXFER(12,1)+BUSTXFER(12,2)+BUSTXFER(12,3)+
     *         BUSTXFER(12,4)
         PEST=BUSTXFER(12,2)+BUSTXFER(12,3)+BUSTXFER(12,4)
         IF(DENOM.GT.0) THEN
         PEST=PEST/DENOM
         ELSE
         PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=COEFF(39)
         IF(RCON.GT.0.AND.ECON.EQ.0) RCON=0.0
         NCON=ECON+ADJFCT2*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         COEFF(39)=NCON
         WRITE(26,5933) 39,POBS,PEST,ECON,RCON,NCON
         WRITE(51,5946) 39,COEFF(39)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Walk to Transitway Transfer Constants
C
      IF(CCODE(56)) THEN
      WRITE(26,5925)
 5925 FORMAT(/15X,'Walk Transitway Bus Transfer Constant',/,
     *        15X,'-------------------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'  Name     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=WTTXFER
         DENOM=BUSTXFER(3,1)+BUSTXFER(3,2)+BUSTXFER(3,3)+BUSTXFER(3,4)
         PEST=BUSTXFER(3,2)+BUSTXFER(3,3)+BUSTXFER(3,4)
         IF(DENOM.GT.0) THEN
         PEST=PEST/DENOM
         ELSE
         PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=COEFF(40)
         IF(RCON.GT.0.AND.ECON.EQ.0) RCON=0.0
         NCON=ECON+ADJFCT2*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         COEFF(40)=NCON
         WRITE(26,5933) 40,POBS,PEST,ECON,RCON,NCON
         WRITE(51,5946) 40,COEFF(40)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bike to Transitway Transfer Constants
C
      IF(CCODE(57)) THEN
      WRITE(26,5926)
 5926 FORMAT(/15X,'Bike Transitway Transfer Constant',/,
     *        15X,'---------------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'  Name     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=BTTXFER
         DENOM=BUSTXFER(8,1)+BUSTXFER(8,2)+BUSTXFER(8,3)+BUSTXFER(8,4)
         PEST=BUSTXFER(8,2)+BUSTXFER(8,3)+BUSTXFER(8,4)
         IF(DENOM.GT.0) THEN
         PEST=PEST/DENOM
         ELSE
         PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=COEFF(41)
         IF(RCON.GT.0.AND.ECON.EQ.0) RCON=0.0
         NCON=ECON+ADJFCT2*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         COEFF(41)=NCON
         WRITE(26,5933) 41,POBS,PEST,ECON,RCON,NCON
         WRITE(51,5946) 41,COEFF(41)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Drive to Transitway Bus Transfer Constants
C
      IF(CCODE(58)) THEN
      WRITE(26,5927)
 5927 FORMAT(/15X,'Drive Transitway-- Transfer Constant',/,
     *        15X,'------------------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'  Name     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=DTTXFER
         DENOM=BUSTXFER(13,1)+BUSTXFER(13,2)+BUSTXFER(13,3)+
     *         BUSTXFER(13,4)
         PEST=BUSTXFER(13,2)+BUSTXFER(13,3)+BUSTXFER(13,4)
         IF(DENOM.GT.0) THEN
         PEST=PEST/DENOM
         ELSE
         PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=COEFF(42)
         IF(RCON.GT.0.AND.ECON.EQ.0) RCON=0.0
         NCON=ECON+ADJFCT2*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         COEFF(42)=NCON
         WRITE(26,5933) 42,POBS,PEST,ECON,RCON,NCON
         WRITE(51,5946) 42,COEFF(42)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Walk to BRT Transfer Constants
C
      IF(CCODE(59)) THEN
      WRITE(26,5928)
 5928 FORMAT(/15X,'Walk BRT Transfer Constant',/,
     *        15X,'--------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'  Name     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=WBTXFER
         DENOM=BUSTXFER(5,1)+BUSTXFER(5,2)+BUSTXFER(5,3)+BUSTXFER(5,4)
         PEST=BUSTXFER(5,2)+BUSTXFER(5,3)+BUSTXFER(5,4)
         IF(DENOM.GT.0) THEN
         PEST=PEST/DENOM
         ELSE
         PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=COEFF(43)
         IF(RCON.GT.0.AND.ECON.EQ.0) RCON=0.0
         NCON=ECON+ADJFCT2*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         COEFF(43)=NCON
         WRITE(26,5933) 43,POBS,PEST,ECON,RCON,NCON
         WRITE(51,5946) 43,COEFF(43)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bike to BRT Transfer Constants
C
      IF(CCODE(60)) THEN
      WRITE(26,5929)
 5929 FORMAT(/15X,'Bike BRT Transfer Constant',/,
     *        15X,'---------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'  Name     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=BTTXFER
         DENOM=BUSTXFER(10,1)+BUSTXFER(10,2)+BUSTXFER(10,3)+
     *         BUSTXFER(10,4)
         PEST=BUSTXFER(10,2)+BUSTXFER(10,3)+BUSTXFER(10,4)
         IF(DENOM.GT.0) THEN
         PEST=PEST/DENOM
         ELSE
         PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=COEFF(44)
         IF(RCON.GT.0.AND.ECON.EQ.0) RCON=0.0
         NCON=ECON+ADJFCT2*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         COEFF(44)=NCON
         WRITE(26,5933) 44,POBS,PEST,ECON,RCON,NCON
         WRITE(51,5946) 44,COEFF(44)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Drive to BRT Transfer Constants
C
      IF(CCODE(61)) THEN
      WRITE(26,5934)
 5934 FORMAT(/15X,'Drive BRT-- Transfer Constant',/,
     *        15X,'-----------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'  Name     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=DBTXFER
         DENOM=BUSTXFER(15,1)+BUSTXFER(15,2)+BUSTXFER(15,3)+
     *         BUSTXFER(15,4)
         PEST=BUSTXFER(15,2)+BUSTXFER(15,3)+BUSTXFER(15,4)
         IF(DENOM.GT.0) THEN
         PEST=PEST/DENOM
         ELSE
         PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=COEFF(45)
         IF(RCON.GT.0.AND.ECON.EQ.0) RCON=0.0
         NCON=ECON+ADJFCT2*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         COEFF(45)=NCON
         WRITE(26,5933) 45,POBS,PEST,ECON,RCON,NCON
         WRITE(51,5946) 45,COEFF(45)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Commuter Rail Bus Egress Constants
C
      IF(CCODE(62)) THEN
      WRITE(26,5935)
 5935 FORMAT(/15X,'Commuter Rail Egress Constant',/,
     *        15X,'-----------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'  Name     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=CBUSEGR
         DENOM=RALEGR(1,5)+RALEGR(1,6)
         PEST=RALEGR(1,5)
         IF(DENOM.GT.0) THEN
         PEST=PEST/DENOM
         ELSE
         PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=COEFF(46)
         IF(RCON.GT.0.AND.ECON.EQ.0) RCON=0.0
         NCON=ECON+ADJFCT2*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         COEFF(46)=NCON
         WRITE(26,5933) 46,POBS,PEST,ECON,RCON,NCON
         WRITE(51,5946) 46,COEFF(46)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Urban Rail Bus Egress Constants
C
      IF(CCODE(63)) THEN
      WRITE(26,5936)
 5936 FORMAT(/15X,'Urban Rail Egress Constant',/,
     *        15X,'--------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'  Name     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         POBS=UBUSEGR
         DENOM=RALEGR(2,5)+RALEGR(2,6)
         PEST=RALEGR(2,5)
         IF(DENOM.GT.0) THEN
         PEST=PEST/DENOM
         ELSE
         PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=COEFF(47)
         IF(RCON.GT.0.AND.ECON.EQ.0) RCON=0.0
         NCON=ECON+ADJFCT2*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         COEFF(47)=NCON
         WRITE(26,5933) 47,POBS,PEST,ECON,RCON,NCON
         WRITE(51,5946) 47,COEFF(47)
      END IF
C----------------------------------------------------------------------
C     Calculate Walk to Commuter Rail Constants
C
      IF(CCODE(64)) THEN
      WRITE(26,4310)
      POBS=0.0
      PEST=0.0
         POBS=OESUM(11,1)
         PEST=TESUM(11,1) 
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KWCR(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KWCR(1)=NCON
         WRITE(26,4311) POBS,PEST,ECON,RCON,NCON
      WRITE(51,4312) KWCR(1)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Walk to UR Constants
C
      IF(CCODE(65)) THEN
      WRITE(26,7411)
      POBS=0.0
      PEST=0.0
         POBS=OESUM(15,1)
         PEST=TESUM(15,1)
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KWUR(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KWUR(1)=NCON
         WRITE(26,7431) POBS,PEST,ECON,RCON,NCON
      WRITE(51,7442) KWUR(1)
      END IF
C
C----------------------------------------------------------------------
C End of constant adjustments
C----------------------------------------------------------------------
  170 CONTINUE
      TESUM=0.0
	    OESUM=0.0
      DFSUM=0.0
      PCSUM=0.0
      OTRN=0.0
      ETRN=0.0
      DTRN=0.0
      INTRATRP=0.0
      DFSUMC=0.0
      PCSUMC=0.0
	    CALSUM=0.0
	    COESUM=0.0
      PTRIP=0.0
      TTRIP=0.0
      OTRIP=0.0
      TMP=0.0
 999  RETURN
      END
