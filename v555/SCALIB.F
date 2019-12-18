C***********************************************************************
C     SUBROUTINE SCALIB: CALIBRATES BIAS CONSTANT VALUES               *
C***********************************************************************
      SUBROUTINE SCALIB(ITER,TESUM,CALSUM,stasum,HHFACT,KKUR,
     *                  STRHBW,PTRIP,TTRIP,CBDTRAN)
      INCLUDE 'stadat.com'
      INCLUDE 'param.com'
      include 'mtamcpar.inc'
C
C     DECLARATIONS NEEDED FOR THIS SUBROUTINE
C
      INTEGER*2   ITER,NCATS
      REAL*8      POBS,PEST,RCON,ECON,NCON,DENOBS,DENEST
      REAL*8      MINVAL,MAXVAL
      REAL*8      TO,TE
      REAL*8      TOTAL,ING,BUSRAPID,BUSTWY,BUSEXP,LAXUR
      REAL*8      DRVRAPID,BUSBRT,DRVBRT,DRVTWY,DRVEXP
      REAL*8      NHHFACT(3),TEMP1,TEMP2,TEMP3
      REAL*8      OESUM(27,4),DFSUM(27,4),PCSUM(27,4)
      REAL*8      COESUM(5,3,5),DFSUMC(5,3,5),PCSUMC(5,3,5)
      REAL*8      TESUM(64,4),CALSUM(5,3,5)
      REAL*4      HHFACT(3),KKUR(3),CBDTRAN
      REAL*8      STASUM(MAX_STATIONS,18),TMP(27,3,5),DENOM
      REAL*8      OTRIP(21,3),INCDAT(3),PTRIP(21,3),TTRIP(21,3)
      REAL*8      CVALUE(8),EQUIVT
      INTEGER*4   C, INC, K, MODE, T,T1,T2,MAXT
      INTEGER*4   CTY
      INTEGER*4   INCGP,PINDEX,AMODE
      LOGICAL     CCODE(26),STRHBW,EXISTS
      CHARACTER*12 COUNTY(5)
      CHARACTER*80 STRING
C
      DATA        COUNTY/'LOS ANGELES ','ORANGE     ','RIVERSIDE  ',
     *                   'SAN BERNARD ','VENTURA    '/
C
C     PREPARE MATRIX FILES FOR NEXT ITERATION
C
C
      MINVAL=-15.0
      MAXVAL=0.0
      NCATS=3
      IF(NHB) NCATS=1
      WRITE(26,1) ITER
      WRITE(7,1) ITER
    1 FORMAT(//25X,'Calibration Iteration #',I2,/,
     *           25X,'---------------------------',//)
      WRITE(*,1) ITER
      WRITE(77,1) ITER
C
C     EVALUATE MINIMUM VALUE
C
      DENOM=MINVAL/(LSUMS*LSUMA)
      IF(DENOM.LT.(-51.0)) THEN
         MINVAL=(-50.0)*LSUMS*LSUMA
      END IF
      WRITE(26,3) MINVAL
      WRITE(*,3) MINVAL
    3 FORMAT(/1X,' Minimum Allowable Constant=',F10.3//)
C
C     READ OBSERVED CALIBRATION VALUES FROM UNIT 8
C
      INQUIRE(FILE=CTVINP,EXIST=EXISTS)
      IF(.NOT.EXISTS) GO TO 300
      OPEN(8,FILE=CTVINP,STATUS='OLD',FORM='FORMATTED')
      READ(8,*,END=100) CCODE
      WRITE(26,2) CCODE
    2 FORMAT(' CCODE( 1)=',L1,' KTRN,KSR,K3P,K4P,KNMOT,KBIKE'/
     *       ' CCODE( 2)=',L1,' KDTRN'/
     *       ' CCODE( 3)=',L1,' KPNR'/
     *       ' CCODE( 4)=',L1,' KCR'/
     *       ' CCODE( 5)=',L1,' KUR'/
     *       ' CCODE( 6)=',L1,' KEBUS'/
     *       ' CCODE( 7)=',L1,' KWAY'/
     *       ' CCODE( 8)=',L1,' KLBUS'/
     *       ' CCODE( 9)=',L1,' KRBUS'/
     *       ' CCODE(10)=',L1,' KBCR'/
     *       ' CCODE(11)=',L1,' KBUR'/
     *       ' CCODE(12)=',L1,' KBRPDW'/
     *       ' CCODE(13)=',L1,' KBTWYW'/
     *       ' CCODE(14)=',L1,' KBEXPW'/
     *       ' CCODE(15)=',L1,' KINFL'/
     *       ' CCODE(16)=',L1,' KINFR'/
     *       ' CCODE(17)=',L1,' KTRN (TRIP LENGTH)'/
     *       ' CCODE(18)=',L1,' KURLAX'/
     *       ' CCODE(19)=',L1,' KBRT'/
     *       ' CCODE(20)=',L1,' KRBUS'/
     *       ' CCODE(21)=',L1,' KBBRTW'/
     *       ' CCODE(22)=',L1,' KBRPDD'/
     *       ' CCODE(23)=',L1,' KBBRTD'/
     *       ' CCODE(24)=',L1,' CCBD'/
     *       ' CCODE(25)=',L1,' KBTWYD'/
     *       ' CCODE(26)=',L1,' KBEXPD'/)
      READ(8,*) BUSRAPID,DRVRAPID,BUSTWY,DRVTWY,BUSEXP,DRVEXP,LAXUR
  45     READ(8,*,END=100) INCGP,CTY,MODE,IGN,TOTAL
         IF(INCGP.EQ.99) GO TO 100
         IF((MODE.LE.0).OR.(MODE.GT.27).OR.(INCGP.LE.0).OR.
     *      (INCGP.GT.3).OR.(CTY.LE.0).OR.(CTY.GT.5)) THEN
         WRITE(26,103) MODE,INCGP,CTY
  103    FORMAT(' SCALIB 45 (F) ERROR IN CALIBRATION INDICES ',
     *          ' MODE=',I3,' INCOME GROUP=',I2,' COUNTY=',I2)
         STOP 16
         END IF
         TMP(MODE,INCGP,CTY)=TOTAL
C ------------------------------------------
         IF(DEBUG) THEN
         WRITE(26,102) MODE,INCGP,CTY,TOTAL
  102    FORMAT(2X,I2,2X,I2,2X,I2,2X,F8.0)
         END IF
C --------------------------------------------
         GOTO 45
  100 CONTINUE
  150 READ(8,*,END=300) PINDEX,INCDAT
      IF(PINDEX.LT.1.OR.PINDEX.GT.21) THEN
      WRITE(26,151) PINDEX
  151 FORMAT(' SCALIB 151 (F) ILLOGICAL VALUE (',I3,') FOR TRIP',
     *       ' LENGTH INDICATOR IN OBSERVED TRANSIT SHARE FILE')
      STOP 16
      END IF
      OTRIP(PINDEX,1)=INCDAT(1)
      OTRIP(PINDEX,2)=INCDAT(2)
      OTRIP(PINDEX,3)=INCDAT(3)
      MAXT=PINDEX
C ---------------------------------------------------------
      IF(DEBUG) THEN
      WRITE(26,152) PINDEX,INCDAT,OTRIP(PINDEX,1),
     *              OTRIP(PINDEX,2),OTRIP(PINDEX,3),MAXT
  152 FORMAT(' PINDEX=',I2,' INCDAT=',3(1X,F6.3),
     *       ' OTRIP=',3(1X,F6.3),' MAXT=',I2)
      END IF
C ---------------------------------------------------------
      GO TO 150
  300 CLOSE(8,STATUS='KEEP')
C
C READ BRT CALIBRATION TARGET VALUES
C
      INQUIRE(FILE='Orange_BRT.csv',EXIST=EXISTS)
      IF(EXISTS) THEN
      OPEN(8,FILE='Orange_BRT.csv',STATUS='OLD',
     *       FORM='FORMATTED')
      READ(8,*,END=100) CCODE
      WRITE(26,2) CCODE
      READ(8,*) PINDEX,BUSBRT,DRVBRT
  200 READ(8,*,END=250) INCGP,AMODE,CVALUE
      IF(AMODE.EQ.1) THEN
      AMODE=25
      ELSE
      AMODE=26
      END IF
      OESUM(AMODE,INCGP)=OESUM(AMODE,INCGP)+CVALUE(PINDEX)
      OESUM(AMODE,4)=OESUM(AMODE,4)+CVALUE(PINDEX)
      GO TO 200
  250 CONTINUE
      CLOSE(8,STATUS='KEEP')
      WRITE(26,5017)
 5017 FORMAT(//,20X,'R E P O R T   1D  (O B S E R V E D  D A T A)',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE AND INCOME GROUP',//,
     *       1X,' INCOME  ','  BRT    ','   BRT   ',/
     *       1X,'  GROUP  ','  WALK   ','  DRIVE  ',/
     *       1X,'---------','---------','---------')
      DO 5015 C=1,3
         WRITE(26,9002) C,OESUM(25,C),OESUM(26,C)
         DFSUM(25,C)=TESUM(56,C)-OESUM(25,C)
         DFSUM(26,C)=TESUM(57,C)-OESUM(26,C)
 5015 CONTINUE
      WRITE(26,9003) OESUM(25,4),OESUM(26,4)
      DFSUM(25,4)=TESUM(56,4)-OESUM(25,4)
      DFSUM(26,4)=TESUM(57,4)-OESUM(26,4)
      WRITE(26,5117)
 5117 FORMAT(//,20X,'R E P O R T   2D (ESTIMATED-OBSERVED)',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE AND INCOME GROUP',//,
     *       1X,' INCOME  ','  BRT    ','   BRT   ',/
     *       1X,'  GROUP  ','  WALK   ','  DRIVE  ',/
     *       1X,'---------','---------','---------')
      DO 5115 C=1,3
         WRITE(26,9002) C,DFSUM(25,C),DFSUM(26,C)
 5115 CONTINUE
      WRITE(26,9003) DFSUM(25,4),DFSUM(26,4)
      END IF
C
C READ RAPID BUS CALIBRATION TARGET VALUES
C
      INQUIRE(FILE='Rapid_Bus.csv',EXIST=EXISTS)
      IF(EXISTS) THEN
      OPEN(8,FILE='Rapid_Bus.csv',STATUS='OLD',
     *       FORM='FORMATTED')
      READ(8,*) BUSRAPID,DRVRAPID
  201 READ(8,*,END=251) INCGP,AMODE,CVALUE
      IF(AMODE.EQ.1) THEN
      AMODE=21
      ELSE
      AMODE=22
      END IF
      OESUM(AMODE,INCGP)=OESUM(AMODE,INCGP)+CVALUE(PINDEX)
      OESUM(AMODE,4)=OESUM(AMODE,4)+CVALUE(PINDEX)
      GO TO 201
  251 CONTINUE
      CLOSE(8,STATUS='KEEP')
      WRITE(26,5018)
 5018 FORMAT(//,20X,'R E P O R T   1E  (O B S E R V E D  D A T A)',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE AND INCOME GROUP',//,
     *       1X,' INCOME  ','  RAPID  ','  RAPID  ',/
     *       1X,'  GROUP  ','  WALK   ','  DRIVE  ',/
     *       1X,'---------','---------','---------')
      DO 5016 C=1,3
         WRITE(26,9002) C,OESUM(21,C),OESUM(22,C)
         DFSUM(21,C)=TESUM(48,C)-OESUM(21,C)
         DFSUM(22,C)=TESUM(49,C)-OESUM(22,C)
 5016 CONTINUE
      WRITE(26,9003) OESUM(21,4),OESUM(22,4)
      DFSUM(21,4)=TESUM(48,4)-OESUM(21,4)
      DFSUM(22,4)=TESUM(49,4)-OESUM(22,4)
       WRITE(26,5118)
 5118 FORMAT(//,20X,'R E P O R T   2E (ESTIMATED-OBSERVED)',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE AND INCOME GROUP',//,
     *       1X,' INCOME  ','  RAPID  ','  RAPID  ',/
     *       1X,'  GROUP  ','  WALK   ','  DRIVE  ',/
     *       1X,'---------','---------','---------')
      DO 5116 C=1,3
         WRITE(26,9002) C,DFSUM(21,C),DFSUM(22,C)
 5116 CONTINUE
      WRITE(26,9003) DFSUM(21,4),DFSUM(22,4)
       GO TO 175
      END IF
C
C SUM OBSERVED DATA TO OESUM VARIABLE
C
C COESUM(1) = DRIVE-ALONE
C COESUM(2) = 2-PERSON
C COESUM(3) = 3+-PERSON
C COESUM(4) = TOTAL
C COESUM(5) = 4+ PERSON
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
C OESUM(19) = WALK TO TWAY BUS
C OESUM(20) = DRV TO TWAY BUS
C OESUM(21) = WALK TO RAPID BUS
C OESUM(22) = DRV  TO RAPID BUS
C OESUM(25) = WALK TO BRT
C OESUM(26) = DRV  TO BRT
C OESUM(15) = WALK TO URBAN RAIL
C OESUM(16) = BUS TO URBAN RAIL
C OESUM(17) = PNR TO URBAN RAIL
C OESUM(18) = KNR TO URBAN RAIL
C OESUM(11) = WALK TO COMMUTER RAIL
C OESUM(12) = BUS TO COMMUTER RAIL
C OESUM(13) = PNR TO COMMUTER RAIL
C OESUM(14) = KNR TO COMMUTER RAIL
C OESUM(23) = BIKE
C OESUM(24) = NON-MOTORIZED
C OESUM(27) = 4+ PERSON AUTO
C
C
      DO 311,INC=1,3
      DO 310,ICO=1,5
         COESUM(1,INC,ICO) = TMP(2,INC,ICO)
         COESUM(2,INC,ICO) = TMP(3,INC,ICO)
         COESUM(3,INC,ICO) = TMP(4,INC,ICO)
         COESUM(5,INC,ICO) = TMP(27,INC,ICO)
	   COESUM(4,INC,ICO) = TMP(2,INC,ICO)+TMP(3,INC,ICO)
     *     + TMP(4,INC,ICO) + TMP(5,INC,ICO) +TMP(6,INC,ICO)
     *     + TMP(7,INC,ICO) + TMP(8,INC,ICO) +TMP(9,INC,ICO)
     *     + TMP(10,INC,ICO) + TMP(11,INC,ICO) +TMP(12,INC,ICO)
     *     + TMP(13,INC,ICO) + TMP(14,INC,ICO) +TMP(15,INC,ICO)
     *     + TMP(16,INC,ICO) + TMP(17,INC,ICO) +TMP(18,INC,ICO)
     *     + TMP(19,INC,ICO) + TMP(20,INC,ICO) +TMP(21,INC,ICO)
     *     + TMP(22,INC,ICO) + TMP(23,INC,ICO) +TMP(24,INC,ICO)
C...AUTO MODES
	 OESUM(1,INC) = OESUM(1,INC)+TMP(2,INC,ICO)
         OESUM(2,INC) = OESUM(2,INC)+TMP(3,INC,ICO)
         OESUM(3,INC) = OESUM(3,INC)+TMP(4,INC,ICO)
         OESUM(27,INC)= OESUM(27,INC) + TMP(27,INC,ICO)
C...NON-MOTORIZED MODES
         OESUM(4,INC)  = OESUM(4,INC) +TMP(25,INC,ICO)
         OESUM(23,INC) = OESUM(23,INC)+TMP(26,INC,ICO)
         OESUM(24,INC) = OESUM(24,INC)+TMP(25,INC,ICO)
     *                                +TMP(26,INC,ICO)
C...LOCAL BUS
         OESUM(7,INC) = OESUM(7,INC)+TMP(5,INC,ICO)
         OESUM(8,INC) = OESUM(8,INC)+TMP(6,INC,ICO)+TMP(7,INC,ICO)
C...EXPRESS BUS
         OESUM(9,INC) = OESUM(9,INC)+TMP(8,INC,ICO)
         OESUM(10,INC)= OESUM(10,INC)+TMP(9,INC,ICO)+TMP(10,INC,ICO)
C...COMMUTER RAIL
         OESUM(11,INC)= OESUM(11,INC)+TMP(18,INC,ICO)
         OESUM(13,INC)= OESUM(13,INC)+TMP(19,INC,ICO)
         OESUM(14,INC)= OESUM(14,INC)+TMP(20,INC,ICO)
         OESUM(12,INC)= OESUM(12,INC)+TMP(21,INC,ICO)
C...URBAN RAIL
         OESUM(15,INC)= OESUM(15,INC)+TMP(14,INC,ICO)
         OESUM(17,INC)= OESUM(17,INC)+TMP(15,INC,ICO)
         OESUM(18,INC)= OESUM(18,INC)+TMP(16,INC,ICO)
         OESUM(16,INC)= OESUM(16,INC)+TMP(17,INC,ICO)
C...TWAY BUS
         OESUM(19,INC)= OESUM(19,INC)+TMP(11,INC,ICO)
         OESUM(20,INC)= OESUM(20,INC)+TMP(12,INC,ICO)+TMP(13,INC,ICO)
C...RAPID BUS
         OESUM(21,INC)= OESUM(21,INC)+TMP(22,INC,ICO)
         OESUM(22,INC)= OESUM(22,INC)+TMP(23,INC,ICO)+TMP(24,INC,ICO)
  310 CONTINUE
         OESUM(5,INC)=OESUM(7,INC)+OESUM(8,INC)+OESUM(9,INC)+
     *                OESUM(10,INC)+OESUM(11,INC)+OESUM(12,INC)+
     *                OESUM(13,INC)+OESUM(14,INC)+OESUM(15,INC)+
     *                OESUM(16,INC)+OESUM(17,INC)+OESUM(18,INC)+
     *                OESUM(19,INC)+OESUM(20,INC)+OESUM(21,INC)+
     *                OESUM(22,INC)
  311 CONTINUE
C
        DO 313,INC=1,3
         DO 312,IMO=1,27
              IF(imo.eq.6.or.imo.eq.5) GOTO 312
              if(imo.ge.24.and.imo.le.26) goto 312
              OESUM(6,INC)=OESUM(6,INC)+OESUM(IMO,INC)
	        OESUM(IMO,4)=OESUM(IMO,4)+OESUM(IMO,INC)
	        OESUM(6,4)=OESUM(6,4)+OESUM(IMO,INC)
  312    CONTINUE
	        OESUM(5,4)=OESUM(5,4)+OESUM(5,INC)
	        OESUM(24,4)=OESUM(24,4)+OESUM(24,INC)
  313    CONTINUE
C
C    DFSUM = ABSOLUTE DIFFERENCE
C    PCSUM = PERCENT DIFFERENCE
C

         DO 400,INC=1,3
         DO 410,MODE=1,5
            DFSUM(MODE,INC)=TESUM(MODE,INC)-OESUM(MODE,INC)
            IF(OESUM(MODE,INC).GT.0.0) THEN
               PCSUM(MODE,INC)=(DFSUM(MODE,INC)/OESUM(MODE,INC))*100.0
            ELSE
               PCSUM(MODE,INC)=0.0
            END IF
C
  410    CONTINUE
         DO 420,MODE=7,21
            DFSUM(MODE,INC)=TESUM(MODE,INC)-OESUM(MODE,INC)
            IF(OESUM(MODE,INC).GT.0.0) THEN
               PCSUM(MODE,INC)=(DFSUM(MODE,INC)/OESUM(MODE,INC))*100.0
            ELSE
               PCSUM(MODE,INC)=0.0
            END IF
  420    CONTINUE
            DFSUM(4,INC)=TESUM(51,INC)-OESUM(4,INC)
            DFSUM(21,INC)=TESUM(48,INC)-OESUM(21,INC)
            DFSUM(22,INC)=TESUM(49,INC)-OESUM(22,INC)
            DFSUM(23,INC)=TESUM(52,INC)-OESUM(23,INC)
            DFSUM(24,INC)=TESUM(50,INC)-OESUM(24,INC)
            DFSUM(27,INC)=TESUM(64,INC)-OESUM(27,INC)
  400    CONTINUE

C
         DO 430,INC=1,3
         DO 430,MODE=1,5
         DO 430,ICO=1,5
         DFSUMC(MODE,INC,ICO)=CALSUM(MODE,INC,ICO)-COESUM(MODE,INC,ICO)
         IF(COESUM(MODE,INC,ICO).GT.0.0) THEN
         PCSUMC(MODE,INC,ICO)=(DFSUMC(MODE,INC,ICO)/
     *    COESUM(MODE,INC,ICO)) * 100.0
         ELSE
         PCSUMC(MODE,INC,ICO)=0.0
         ENDIF
 430     CONTINUE
C
C     COMPUTE ABSOLUTE AND PERCENT DIFFERENCE
C     OBSERVED VERSUS ESTIMATED
C
C     CHECK FOR HHFACT CLOSE TO 1.0
C     RE-COMPUTE IF REQUIRED
C
      IF(NHB) GO TO 450
      NHHFACT(1)=(OESUM(6,1)/OESUM(6,4))/(TESUM(6,1)/TESUM(6,4))
      NHHFACT(2)=(OESUM(6,2)/OESUM(6,4))/(TESUM(6,2)/TESUM(6,4))
      NHHFACT(3)=(OESUM(6,3)/OESUM(6,4))/(TESUM(6,3)/TESUM(6,4))
C
      IF(.NOT.STRHBW) THEN
      IF(NHHFACT(1).LE.0.95.OR.NHHFACT(1).GE.1.05.OR.
     *   NHHFACT(2).LE.0.95.OR.NHHFACT(2).GE.1.05.OR.
     *  NHHFACT(3).LE.0.95.OR.NHHFACT(3).GE.1.05) THEN
      temp1=nhhfact(1)*hhfact(1)
      temp2=nhhfact(2)*hhfact(2)
      temp3=nhhfact(3)*hhfact(3)
      HHFACT(1)=(0.5*(temp1-HHFACT(1)))+HHFACT(1)
      HHFACT(2)=(0.5*(temp2-HHFACT(2)))+HHFACT(2)
      HHFACT(3)=(0.5*(temp3-HHFACT(3)))+HHFACT(3)
      WRITE(26,8383) HHFACT(1),HHFACT(2),HHFACT(3),
     *               NHHFACT(1),NHHFACT(2),NHHFACT(3)
      WRITE(*,8383) HHFACT(1),HHFACT(2),HHFACT(3),
     *               NHHFACT(1),NHHFACT(2),NHHFACT(3)
 8383 FORMAT(//1X,'NEW HOUSEHOLD INCOME FACTORS:',3F10.4,/,
     *         1X,'----------------------------'/
     *         1X,'CLOSURE FACTORS ----------> :',3f10.3/)
      GOTO 450
      ELSE
       IF(.NOT.HFACT) THEN
       HFACT=.TRUE.
       WRITE(26,8384) HHFACT(1),HHFACT(2),HHFACT(3),
     *                NHHFACT(1),NHHFACT(2),NHHFACT(3)
 8384  FORMAT(//1X,'FINAL HOUSEHOLD INCOME FACTORS:',3F10.4,/,
     *          1X,'----------------------------'/
     *          1X,'FINAL HOUSEHOLD CLOSURE VALUES:',3F10.3)
       ITER=ITER-1
       GO TO 170
       END IF
      END IF
      END IF
C Write out observed data, and comparisons ..........................
C
  450 CONTINUE
      IF(NHB) HFACT=.TRUE.
      IF(ITER.EQ.1) THEN
C
C----------------------------------------------------------------------
C
      WRITE(26,1017)
 1017 FORMAT(//,20X,'R E P O R T   1A  (O B S E R V E D  D A T A)',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE AND INCOME GROUP',//,
     *       1X,'INCOME   ','         ','    2    ','      3   ',
     *          '      4+   ','         ','         ','   TOTAL  '/
     *       1X,' GROUP   ','  DRIVE  ','  PERSON ','    PERSON ',
     *          '    PERSON ','   NON   ',' TRANSIT ','   PERSON '/
     *       1X,'         ','  ALONE  ','  AUTO   ','     AUTO  ',
     *          '     AUTO  ','MOTORIZED','   TRIPS ','   TRIPS '/
     *       1X,'---------','---------','---------','----------',
     *          '----------','----------','----------','----------')
      DO 2015 C=1,3
         WRITE(26,9002) C,(OESUM(T,C),T=1,3),OESUM(27,C),OESUM(24,C),
     *                     OESUM(5,C),OESUM(6,C)
 9002    FORMAT(5X,I1,2X,7F10.0)
 2015 CONTINUE
      WRITE(26,9003) (OESUM(T,4),T=1,3),OESUM(27,4),OESUM(24,4),
     *                OESUM(5,4),OESUM(6,4)
 9003 FORMAT(/,2X,'TOTAL',1X,7F10.0)
C
C
      WRITE(26,3017)
 3017 FORMAT(//,20X,'R E P O R T  1B  (O B S E R V E D   D A T A)',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE, INCOME GROUP AND COUNTY',//,
     *       1X,'INCOME   ','COUNTY   ','  DRIVE   ','   2-PERSON ',
     *          '  3-PERSON',' 4 PERSON',/,
     *       1X,'GROUP    ','         ','  ALONE   ','     AUTO   ',
     *          '    AUTO    ','   AUTO    ',/,
     *       1X,'---------','---------','----------','----------',
     *          '-----------','-----------',//)
      DO 4015,C=1,3
      DO 4015,IC=1,5
      WRITE(26,9312) C,COUNTY(IC),(COESUM(T,C,IC),T=1,3),COESUM(5,C,IC)
 9312 FORMAT(5X,I1,2X,A12,1X,4F10.0)
 4015 CONTINUE
C
C----------------------------------------------------------------------
C
      WRITE(26,1018)
 1018 FORMAT(//,20X,'R E P O R T   1C  (O B S E R V E D  D A T A)',/,
     *          20X,
     *          'SUMMARIZE TRANSIT TRIPS BY INCOME GROUP        ',//,
     *       1X,'INCOME   ','   LOCAL   ','  LOCAL  ',' EXPRESS ',
     *          '  EXRESS  ',
     *          ' COMMUTER  ',
     *          ' COMMUTER ',' COMMUTER ',' COMMUTER  ',' URBAN   ',
     *          '  URBAN   ','  URBAN   ','  URBAN    ',' TWAY    ',
     *          '  TWAY    ','  RAPID   ','  RAPID    '/
     *       1X,'GROUP    ','    BUS   ','     BUS   ','  BUS   ',
     *          '   BUS     ',
     *          '  RAIL   ',
     *          '  RAIL   ','   RAIL     ',' RAIL    ','   RAIL    ',
     *          '  RAIL   ','   RAIL     ',' RAIL  ','    BUS    ',
     *          '   BUS    ','   BUS      ','  BUS   '/
     *       1X,'         ','   WALK  ','    DRIVE  ','  WALK  ',
     *          '   DRIVE  ',
     *          '   WALK  ',
     *          '    BUS   ','    PNR     ','   KNR     ','   WALK   ',
     *          '    BUS   ','  PNR     ','  KNR     ','  WALK   ',
     *          '   DRIVE  ','   WALK    ','  DRIVE    '/
     *       1X,'---------','---------','---------','----------',
     *          '----------',
     *          '----------',
     *          '---------','---------','---------','-----------',
     *          '---------','---------','---------','-----------',
     *          '---------','---------','----------------')
      DO 2016 C=1,3
         WRITE(26,9005) C,(OESUM(T,C),T=7,22)
 9005    FORMAT(5X,I1,2X,16F10.0)
 2016 CONTINUE
      WRITE(26,9006) (OESUM(T,4),T=7,22)
 9006 FORMAT(/,2X,'TOTAL',1X,16F10.0)
C
      END IF
      IF(.NOT.HFACT) GO TO 170
C
C----------------------------------------------------------------------
C
      WRITE(26,1015)
 1015 FORMAT(//,20X,'R E P O R T   2A  (Observed - Estimated Trips)',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE AND INCOME GROUP',//,
     *       1X,'INCOME   ','         ','    2    ','      3    ',
     *          '      4+   ',/,
     *       1X,' GROUP   ','  DRIVE  ','  PERSON ','    PERSON ',
     *          '    PERSON ','   NON    '/
     *       1X,'         ','  ALONE  ','  AUTO   ','     AUTO  ',
     *          '     AUTO  ',' MOTORIZED'/
     *       1X,'---------','---------','---------','-----------',
     *          '-----------','----------')
      DO 2002 C=1,3
         WRITE(26,9002) C,(DFSUM(T,C),T=1,3),DFSUM(27,C),DFSUM(24,C)
 2002 CONTINUE
C
C
      WRITE(26,3027)
 3027 FORMAT(//,20X,'R E P O R T  2B  (Observed - Estimated Trips)',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE, INCOME GROUP AND COUNTY',//,
     *       1X,'INCOME   ','COUNTY   ','  DRIVE   ','   2-PERSON ',
     *          '   3-PERSON','   4+ PERSON',/,
     *       1X,'GROUP    ','         ','  ALONE   ','     AUTO   ',
     *          '   AUTO    ','   AUTO    ',/,
     *       1X,'---------','---------','----------','----------',
     *          '-----------','-----------',//)
      DO 4025,C=1,3
      DO 4025,IC=1,5
      WRITE(26,9312) C,COUNTY(IC),(DFSUMC(T,C,IC),T=1,3),DFSUMC(5,C,IC)
 4025 CONTINUE
C
C
C----------------------------------------------------------------------
C
      WRITE(26,3015)
 3015 FORMAT(//,20X,'R E P O R T   2C  (Observed - Estimated Trips)',/,
     *          20X,
     *          'SUMMARIZE TRANSIT TRIPS BY INCOME GROUP        ',//,
     *       1X,'INCOME   ','   LOCAL   ','  LOCAL  ',' EXPRESS ',
     *          '  EXRESS  ',
     *          ' COMMUTER  ',
     *          ' COMMUTER ',' COMMUTER ',' COMMUTER  ',' URBAN   ',
     *          '  URBAN   ','  URBAN   ','  URBAN    ',' TWAY    ',
     *          '  TWAY    ','  RAPID   ','  RAPID    '/
     *       1X,'GROUP    ','    BUS   ','     BUS   ','  BUS   ',
     *          '   BUS     ',
     *          '  RAIL   ',
     *          '  RAIL   ','   RAIL     ',' RAIL    ','   RAIL    ',
     *          '  RAIL   ','   RAIL     ',' RAIL  ','    BUS    ',
     *          '   BUS    ','   BUS      ','  BUS   '/
     *       1X,'         ','   WALK  ','    DRIVE  ','  WALK  ',
     *          '   DRIVE  ',
     *          '   WALK  ',
     *          '    BUS   ','    PNR     ','   KNR     ','   WALK   ',
     *          '    BUS   ','  PNR     ','  KNR     ','  WALK   ',
     *          '   DRIVE  ','   WALK    ','  DRIVE    '/
     *       1X,'---------','---------','---------','----------',
     *          '----------',
     *          '----------',
     *          '---------','---------','---------','-----------',
     *          '---------','---------','---------','-----------',
     *          '---------','---------','----------------')
      DO 3002 C=1,3
         WRITE(26,9005) C,(DFSUM(T,C),T=7,22)
 3002 CONTINUE
C
C
C----------------------------------------------------------------------
      WRITE(26,1016)
 1016 FORMAT(//,20X,'R E P O R T   3A  (Estimated / Observed Trips)',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE AND INCOME GROUP',//,
     *       1X,'INCOME   ','         ','    2    ','      3    ',
     *          '      4+   ',/,
     *       1X,' GROUP   ','  DRIVE  ','  PERSON ','    PERSON ',
     *          '    PERSON ',/,
     *       1X,'         ','  ALONE  ','  AUTO   ','     AUTO  ',
     *          '     AUTO  ',/,
     *       1X,'---------','---------','---------','----------',
     *          '----------')
      DO 2005 C=1,3
         WRITE(26,9002) C,(PCSUM(T,C),T=1,3),PCSUM(5,C)
 2005 CONTINUE
C
C
      WRITE(26,3037)
 3037 FORMAT(//,20X,'R E P O R T  3B  (Estimated / Observed Trips)',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE, INCOME GROUP AND COUNTY',//,
     *       1X,'INCOME   ','COUNTY   ','  DRIVE   ',' 2-PERSON ',
     *          '3 -PERSON','4+-PERSON',/,
     *       1X,'GROUP    ','         ','  ALONE   ','   AUTO   ',
     *          ' AUTO    ',' AUTO    ',/,
     *       1X,'---------','---------','----------','----------',
     *          '---------','---------',//)
      DO 4035,C=1,3
      DO 4035,IC=1,5
      WRITE(26,9312) C,COUNTY(IC),(PCSUMC(T,C,IC),T=1,3),PCSUMC(5,C,IC)
 4035 CONTINUE
C
C
C
C----------------------------------------------------------------------
      WRITE(26,3016)
 3016 FORMAT(//,20X,'R E P O R T   3C  (Estimated / Observed Trips)',/,
     *          20X,
     *          'SUMMARIZE TRANSIT TRIPS BY INCOME GROUP        ',//,
     *       1X,'INCOME   ','   LOCAL   ','  LOCAL  ',' EXPRESS ',
     *          '  EXRESS  ',
     *          ' COMMUTER  ',
     *          ' COMMUTER ',' COMMUTER ',' COMMUTER  ',' URBAN   ',
     *          '  URBAN   ','  URBAN   ','  URBAN    ',' TWAY    ',
     *          '  TWAY    ','  RAPID   ','  RAPID    '/
     *       1X,'GROUP    ','    BUS   ','     BUS   ','  BUS   ',
     *          '   BUS     ',
     *          '  RAIL   ',
     *          '  RAIL   ','   RAIL     ',' RAIL    ','   RAIL    ',
     *          '  RAIL   ','   RAIL     ',' RAIL  ','    BUS    ',
     *          '   BUS    ','   BUS      ','  BUS   '/
     *       1X,'         ','   WALK  ','    DRIVE  ','  WALK  ',
     *          '   DRIVE  ',
     *          '   WALK  ',
     *          '    BUS   ','    PNR     ','   KNR     ','   WALK   ',
     *          '    BUS   ','  PNR     ','  KNR     ','  WALK   ',
     *          '   DRIVE  ','   WALK    ','  DRIVE    '/
     *       1X,'---------','---------','---------','----------',
     *          '----------',
     *          '----------',
     *          '---------','---------','---------','-----------',
     *          '---------','---------','---------','-----------',
     *          '---------','---------','----------------')
      DO 3005 C=1,3
         WRITE(26,9005) C,(PCSUM(T,C),T=7,22)
 3005 CONTINUE
C
C----------------------------------------------------------------------
C
C
C This next section here starts revising the constants...
C  
C----------------------------------------------------------------------
C     Calculate Non-Motorized Constants
C
  175 IF(CCODE(1)) THEN
      WRITE(26,6850)
 6850 FORMAT(/15X,'Non-Motorized Constants',/,
     *        15X,'-----------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
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
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KNMOT(INC)=NCON
         WRITE(26,6870) (INC),POBS,PEST,ECON,RCON,NCON
 6870    FORMAT(1X,' KNMOT(',I1,')',2X,2(1X,F7.4),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,6880) INC,KNMOT(INC)
 6880 FORMAT('KNMOT(',I1,')=',F9.5)
 6860 CONTINUE
C
C     Calculate Bicycle Constants
C
      WRITE(26,7850)
 7850 FORMAT(/15X,'Bicycle Constants',/,
     *        15X,'-----------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
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
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KBIKE(INC)=NCON
         WRITE(26,7870) (INC),POBS,PEST,ECON,RCON,NCON
 7870    FORMAT(1X,' KBIKE(',I1,')',2X,2(1X,F7.4),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,7880) INC,KBIKE(INC)
 7880 FORMAT('KBIKE(',I1,')=',F9.5)
 7860 CONTINUE
C
C     Calculate Transit Constants
C
      IF(CCODE(17)) THEN
      DO 4863 INC=1,NCATS
      WRITE(26,4853) INC
 4853 FORMAT(/10X,'Stratified Transit Constants-Income Group ',I1/
     *        10X,'--------------------------------------------',//,
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
         IF(PTRIP(T,INC).GT.0) THEN
             POBS=OTRIP(T,INC)
             PEST=TTRIP(T,INC)/PTRIP(T,INC)
         END IF
         IF(T.EQ.21) THEN
             PEST=TTRIP(MAXT,INC)/PTRIP(MAXT,INC)
             POBS=OTRIP(MAXT,INC)
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         IF(T.LT.MAXT) THEN
         ECON=KTRNT(T,INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KTRNT(T,INC)=NCON
         T1=(T-1)*5
         T2=T*5
         WRITE(26,4873) T1,T2,POBS,PEST,ECON,RCON,NCON
 4873    FORMAT(1X,I3,'-',I3,3X,2(1X,F7.4),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
         WRITE(7,4883) T,INC,KTRNT(T,INC)
 4883    FORMAT('KTRNT(',I2,',',I1,')=',F9.5)
         ELSE
         ECON=KTRNT(MAXT,INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         DO 4865 K=MAXT,21
         KTRNT(K,INC)=NCON
         T1=(K-1)*5
         T2=K*5
         WRITE(26,4873) T1,T2,POBS,PEST,ECON,RCON,NCON
         WRITE(7,4883) K,INC,KTRNT(K,INC)
 4865 CONTINUE
      END IF
 4864 CONTINUE
 4863 CONTINUE
      END IF
      WRITE(26,4850)
 4850 FORMAT(/15X,'Transit Constants',/,
     *        15X,'-----------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
	DO 4860 INC=1,NCATS
         IF(OESUM(6,INC).GT.0) THEN
             POBS=OESUM(5,INC)/OESUM(6,INC)
         ELSE
            POBS=0.0
         END IF
         IF(TESUM(6,INC).GT.0) THEN
            PEST=TESUM(5,INC)/TESUM(6,INC)
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KTRN(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KTRN(INC)=NCON
         IF(CCODE(17)) THEN
         KTRN(INC)=0.0
         RCON=0.0
         NCON=0.0
         END IF
         WRITE(26,4870) (INC),POBS,PEST,ECON,RCON,NCON
 4870    FORMAT(1X,' KTRN(',I1,')',2X,2(1X,F7.4),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      IF(.NOT.CCODE(17)) WRITE(7,4880) INC,KTRN(INC)
 4880 FORMAT('KTRN(',I1,')=',F9.5)
 4860 CONTINUE
C
C----------------------------------------------------------------------
C     Calculate Shared Ride Constants
C
      WRITE(26,4750)
 4750 FORMAT(/15X,'Shared Ride Constants',/,
     *        15X,'---------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO 4760 INC=1,NCATS
         DENOBS=0.0
         DENEST=0.0
         POBS=0.0
         PEST=0.0
      DO 4762 ICO=1,5
         DENOBS=COESUM(1,INC,ICO)+COESUM(2,INC,ICO)+COESUM(3,INC,ICO)+
     *          COESUM(5,INC,ICO)+DENOBS
         DENEST=CALSUM(1,INC,ICO)+CALSUM(2,INC,ICO)+CALSUM(3,INC,ICO)+
     *          CALSUM(5,INC,ICO)+DENEST
         POBS=COESUM(2,INC,ICO)+COESUM(3,INC,ICO)+POBS+COESUM(5,INC,ICO)
         PEST=CALSUM(2,INC,ICO)+CALSUM(3,INC,ICO)+PEST+CALSUM(5,INC,ICO)
 4762    CONTINUE
         POBS=POBS/DENOBS
         PEST=PEST/DENEST
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ICO=1
         ECON=KSR(INC,ICO)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KSR(INC,ICO)=NCON
         DO 4763 ICO=1,5
         KSR(INC,ICO)=KSR(INC,1)
         WRITE(26,4770) INC,ICO,POBS,PEST,ECON,RCON,NCON
 4770    FORMAT(1X,' KSR(',I1,',',I1,')',1X,2(1X,F7.4),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,4761) INC,ICO,KSR(INC,ICO)
 4761 FORMAT('KSR(',I1,',',I1,')=',F9.5)
 4763 CONTINUE
 4760 CONTINUE
C
C----------------------------------------------------------------------
C     Calculate 3+ Person Auto Constants
C
      WRITE(26,4550)
 4550 FORMAT(/15X,'3+ Person Auto Constants',/,
     *        15X,'------------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO 2560 INC=1,NCATS
         DENOBS=0.0
         DENEST=0.0
         POBS=0.0
         PEST=0.0
      DO 2561 ICO=1,5
         DENOBS=COESUM(2,INC,ICO)+COESUM(3,INC,ICO)+DENOBS+
     *          COESUM(5,INC,ICO)
         DENEST=CALSUM(2,INC,ICO)+CALSUM(3,INC,ICO)+DENEST+
     *          CALSUM(5,INC,ICO)
         POBS=COESUM(3,INC,ICO)+POBS
         PEST=CALSUM(3,INC,ICO)+PEST
 2561    CONTINUE
         POBS=POBS/DENOBS
         PEST=PEST/DENEST
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ICO=1
         ECON=K3P(INC,ICO)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         K3P(INC,ICO)=NCON
         DO 2562 ICO=1,5
         K3P(INC,ICO)=K3P(INC,1)
         WRITE(26,4570) INC,ICO,POBS,PEST,ECON,RCON,NCON
 4570    FORMAT(1X,' K3P(',I1,',',I1,')',1X,2(1X,F7.4),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,4561) INC,ICO,K3P(INC,ICO)
 4561 FORMAT('K3P(',I1,',',I1,')=',F9.5)
 2562 CONTINUE
 2560 CONTINUE
C
C----------------------------------------------------------------------
C     Calculate 4+ Person Auto Constants
C
C      IF(SKMP4) THEN
      WRITE(26,4551)
 4551 FORMAT(/15X,'4+ Person Auto Constants',/,
     *        15X,'------------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO 2563 INC=1,NCATS
         DENOBS=0.0
         DENEST=0.0
         POBS=0.0
         PEST=0.0
      DO 2564 ICO=1,5
         DENOBS=COESUM(2,INC,ICO)+COESUM(3,INC,ICO)+DENOBS+
     *          COESUM(5,INC,ICO)
         DENEST=CALSUM(2,INC,ICO)+CALSUM(3,INC,ICO)+DENEST+
     *          CALSUM(5,INC,ICO)
         POBS=COESUM(5,INC,ICO)+POBS
         PEST=CALSUM(5,INC,ICO)+PEST
 2564    CONTINUE
         POBS=POBS/DENOBS
         PEST=PEST/DENEST
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ICO=1
         ECON=K4P(INC,ICO)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         K4P(INC,ICO)=NCON
         DO 2565 ICO=1,5
         K4P(INC,ICO)=K4P(INC,1)
         WRITE(26,4571) INC,ICO,POBS,PEST,ECON,RCON,NCON
 4571    FORMAT(1X,' K4P(',I1,',',I1,')',1X,2(1X,F7.4),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,4562) INC,ICO,K4P(INC,ICO)
 4562 FORMAT('K4P(',I1,',',I1,')=',F9.5)
 2565 CONTINUE
 2563 CONTINUE
      END IF
C      END IF
C
C     Calculate Drive to Local Bus Constants
C
      IF(CCODE(15)) THEN
      WRITE(26,4851)
 4851 FORMAT(/15X,'Drive to Local Bus Constants',/,
     *        15X,'----------------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      POBS=0.0
      PEST=0.0
      DENOM=0.0
      NCON=0.0
C      INC=1
   	   DO 4861 INC=1,NCATS
       POBS=POBS+OESUM(8,INC)
       PEST=PEST+TESUM(8,INC)
       DENOM=DENOM+OESUM(6,INC)
       NCON=NCON+TESUM(6,INC)
 4861  CONTINUE
       POBS=POBS/DENOM
       PEST=PEST/NCON
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KINFL(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KINFL(1)=NCON
         KINFL(2)=NCON
         KINFL(3)=NCON
         WRITE(26,4871) POBS,PEST,ECON,RCON,NCON
 4871    FORMAT(1X,' KINFL',4X,2(1X,F7.4),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,4881) KINFL
 4881 FORMAT('KINFL(1)=',F9.5/
     *       'KINFL(2)=',F9.5/
     *       'KINFL(3)=',F9.5)
      END IF
C
C     Calculate Drive to Rapid Bus Constants
C
      IF(CCODE(16)) THEN
      WRITE(26,4852)
 4852 FORMAT(/15X,'Drive to Rapid Bus Constants',/,
     *        15X,'----------------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      POBS=0.0
      PEST=0.0
      DENOM=0.0
      NCON=0.0
	DO 4862 INC=1,3
       POBS=POBS+OESUM(22,INC)
       PEST=PEST+TESUM(49,INC)
       DENOM=DENOM+TESUM(5,INC)
       NCON=NCON+TESUM(5,INC)
 4862 CONTINUE
       POBS=POBS/DENOM
       PEST=PEST/NCON
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KINFR
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KINFR=NCON
         WRITE(26,4872) POBS,PEST,ECON,RCON,NCON
 4872    FORMAT(1X,' KINFR',4X,2(1X,F7.4),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,4882) KINFR
 4882 FORMAT('KINFR=',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Drive to Transit Constants
C
      IF(CCODE(2)) THEN
      WRITE(26,4611)
 4611 FORMAT(/15X,'Drive to Transit Constants',/,
     *        15X,'--------------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO 4620 INC=1,NCATS
            POBS=OESUM(8,INC)+OESUM(10,INC)+OESUM(13,INC)+
     *           OESUM(14,INC)+OESUM(17,INC)+OESUM(18,INC)+
     *           OESUM(20,INC)+OESUM(22,INC)
            POBS=POBS/OESUM(5,INC)
            PEST=TESUM(8,INC)+TESUM(10,INC)+TESUM(13,INC)+
     *           TESUM(14,INC)+TESUM(17,INC)+TESUM(18,INC)+
     *           TESUM(20,INC)+TESUM(49,INC)
            PEST=PEST/TESUM(5,INC)
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KDTRN(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KDTRN(INC)=NCON
         WRITE(26,4631) (INC),POBS,PEST,ECON,RCON,NCON
 4631    FORMAT(1X,'KDTRN(',I1,')',3X,2(1X,F7.4),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,4641) INC,KDTRN(INC)
 4641 FORMAT('KDTRN(',I1,')=',F9.5)
 4620 CONTINUE 
      END IF
C
C----------------------------------------------------------------------
C     Calculate Park-n-Ride to Transit Constants
C     Urban and Commuter Rail Only
C
      IF(CCODE(3)) THEN
      WRITE(26,4612)
 4612 FORMAT(/15X,'Park-n-Ride to Transit Constants',/,
     *        15X,'--------------------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO 4613 INC=1,NCATS
          POBS=OESUM(13,INC)+OESUM(14,INC)+OESUM(17,INC)+OESUM(18,INC)
          POBS=(OESUM(13,INC)+OESUM(17,INC))/POBS
          PEST=TESUM(13,INC)+TESUM(14,INC)+TESUM(17,INC)+TESUM(18,INC)
          PEST=(TESUM(13,INC)+TESUM(17,INC))/PEST
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KPNR(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KPNR(INC)=NCON
         WRITE(26,4614) (INC),POBS,PEST,ECON,RCON,NCON
 4614    FORMAT(1X,'KPNR(',I1,')',3X,2(1X,F7.4),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,4615) INC,KPNR(INC)
 4615 FORMAT('KPNR(',I1,')=',F9.5)
 4613 CONTINUE 
      END IF
C
C----------------------------------------------------------------------
C     Calculate Commuter Rail Constants
C
      IF(CCODE(4)) THEN
      WRITE(26,4610)
 4610 FORMAT(/15X,'Commuter Rail Constants',/,
     *        15X,'-----------------------',//,
     *  1X,' Income'/
     *  1X,' Group      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
      DO 2620 INC=1,NCATS
         TO=TO+OESUM(11,INC)+OESUM(12,INC)+OESUM(13,INC)+OESUM(14,INC)
         DENOBS=DENOBS+OESUM(5,INC)
         TE=TE+TESUM(11,INC)+TESUM(12,INC)+TESUM(13,INC)+TESUM(14,INC)
         DENEST=DENEST+TESUM(5,INC)
 2620 CONTINUE
         IF(DENOBS.GT.0) THEN
            POBS=TO/DENOBS
         ELSE
            POBS=0.0
         END IF
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
         ECON=KCR(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KCR(1)=NCON
         KCR(2)=NCON
         KCR(3)=NCON
         EQUIVT=(-1.0*NCON)/COEFF(100)
         WRITE(26,4630) POBS,PEST,ECON,RCON,NCON,EQUIVT
 4630    FORMAT(1X,'KCR ' ' ',3X,2(1X,F7.4),1X,
     A                F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      WRITE(7,4640) KCR
 4640 FORMAT('KCR(1)=',F9.5/
     *       'KCR(2)=',F9.5/
     *       'KCR(3)=',F9.5)
      WRITE(77,7771) KCR(1)
 7771 FORMAT('KCR,',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Urban Rail Constants
C
      IF(CCODE(5)) THEN
      WRITE(26,5610)
 5610 FORMAT(/15X,'Urban Rail Constants',/,
     *        15X,'---------------',//,
     *  1X,' Income'/
     *  1X,' Group      Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
      DO 5620 INC=1,NCATS
         TO=TO+OESUM(15,INC)+OESUM(16,INC)+OESUM(17,INC)+OESUM(18,INC)
         DENOBS=DENOBS+OESUM(5,INC)
         TE=TE+TESUM(15,INC)+TESUM(16,INC)+TESUM(17,INC)+TESUM(18,INC)
         DENEST=DENEST+TESUM(5,INC)
 5620 CONTINUE
         IF(DENOBS.GT.0) THEN
            POBS=TO/DENOBS
         ELSE
            POBS=0.0
         END IF
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
         ECON=KUR(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KUR(1)=NCON
         KUR(2)=NCON
         KUR(3)=NCON
         EQUIVT=(-1.0*NCON)/COEFF(100)
         WRITE(26,5630) POBS,PEST,ECON,RCON,NCON,EQUIVT
 5630    FORMAT(1X,'KUR ' ' ',3X,2(1X,F7.4),1X,
     A      F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      WRITE(7,5640) KUR
 5640 FORMAT('KUR(1)=',F9.5/
     *       'KUR(2)=',F9.5/
     *       'KUR(3)=',F9.5)
      WRITE(77,7772) KUR(1)
 7772 FORMAT('KUR,',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Express Bus Constants
C
      IF(CCODE(6)) THEN
      WRITE(26,6650)
 6650 FORMAT(/15X,'Express Bus Constants',/,
     *        15X,'------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
      DO 6620 INC=1,NCATS
         TO=TO+OESUM(9,INC)+OESUM(10,INC)
         DENOBS=DENOBS+OESUM(5,INC)
         TE=TE+TESUM(9,INC)+TESUM(10,INC)
         DENEST=DENEST+TESUM(5,INC)
 6620 CONTINUE
         IF(DENOBS.GT.0) THEN
            POBS=TO/DENOBS
         ELSE
            POBS=0.0
         END IF
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
         ECON=KEBUS(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KEBUS(1)=NCON
         KEBUS(2)=NCON
         KEBUS(3)=NCON
         EQUIVT=(-1.0*NCON)/COEFF(100)
         WRITE(26,6670) POBS,PEST,ECON,RCON,NCON,EQUIVT
 6670    FORMAT(1X,'KEBUS' ' ',3X,2(1X,F7.4),1X,
     A        F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      WRITE(7,6680) KEBUS
 6680 FORMAT('KEBUS(1)=',F9.5/
     *       'KEBUS(2)=',F9.5/
     *       'KEBUS(3)=',F9.5)
      WRITE(77,7773) KEBUS(1)
 7773 FORMAT('KEBUS,',F9.5)
      END IF
C----------------------------------------------------------------------
C     Calculate Transitway Bus Constants
C
      IF(CCODE(7)) THEN
      WRITE(26,7650)
 7650 FORMAT(/15X,'Transitway Bus Constants',/,
     *        15X,'------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
      DO 7660 INC=1,NCATS
         TO=TO+OESUM(19,INC)+OESUM(20,INC)
         DENOBS=DENOBS+OESUM(5,INC)
         TE=TE+TESUM(19,INC)+TESUM(20,INC)
         DENEST=DENEST+TESUM(5,INC)
 7660 CONTINUE
         IF(DENOBS.GT.0) THEN
            POBS=TO/DENOBS
         ELSE
            POBS=0.0
         END IF
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
         ECON=KWAY(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KWAY(1)=NCON
         KWAY(2)=NCON
         KWAY(3)=NCON
         EQUIVT=(-1.0*NCON)/COEFF(100)
         WRITE(26,7670) POBS,PEST,ECON,RCON,NCON,EQUIVT
 7670    FORMAT(1X,'KWAY ' ' ',3X,2(1X,F7.4),1X,
     A        F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      WRITE(7,7680) KWAY
 7680 FORMAT('KWAY(1)=',F9.5/
     *       'KWAY(2)=',F9.5/
     *       'KWAY(3)=',F9.5)
      WRITE(77,7774) KWAY(1)
 7774 FORMAT('KWAY,',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Local Bus Constants
C
      IF(CCODE(8)) THEN
      WRITE(26,4710)
 4710 FORMAT(/15X,'Local Bus Constants',/,
     *        15X,'-------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
      DO 4720 INC=1,NCATS
         TO=TO+OESUM(7,INC)+OESUM(8,INC)
         DENOBS=DENOBS+OESUM(5,INC)
         TE=TE+TESUM(7,INC)+TESUM(8,INC)
         DENEST=DENEST+TESUM(5,INC)
 4720 CONTINUE
         IF(DENOBS.GT.0) THEN
            POBS=TO/DENOBS
         ELSE
            POBS=0.0
         END IF
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
         ECON=KLBUS(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KLBUS(1)=NCON
         KLBUS(2)=NCON
         KLBUS(3)=NCON
         EQUIVT=(-1.0*NCON)/COEFF(100)
         WRITE(26,4730) POBS,PEST,ECON,RCON,NCON,EQUIVT
 4730    FORMAT(1X,'KLBUS' ' ',3X,2(1X,F7.4),1X,
     A         F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      WRITE(7,4740) KLBUS
 4740 FORMAT('KLBUS(1)=',F9.5/
     *       'KLBUS(2)=',F9.5/
     *       'KLBUS(3)=',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Rapid Bus Constants
C
      IF(CCODE(9)) THEN
      WRITE(26,5710)
 5710 FORMAT(/15X,'Rapid Bus Constants',/,
     *        15X,'------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
      DO 5720 INC=1,NCATS
         TO=TO+OESUM(21,INC)+OESUM(22,INC)
         DENOBS=DENOBS+OESUM(5,INC)
         TE=TE+TESUM(48,INC)+TESUM(49,INC)
         DENEST=DENEST+TESUM(5,INC)
 5720 CONTINUE
         IF(DENOBS.GT.0) THEN
            POBS=TO/DENOBS
         ELSE
            POBS=0.0
         END IF
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
         ECON=KRBUS(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KRBUS(1)=NCON
         KRBUS(2)=NCON
         KRBUS(3)=NCON
         WRITE(26,5730) POBS,PEST,ECON,RCON,NCON
 5730    FORMAT(1X,'KRBUS  ',3X,2(1X,F7.4),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,5740) KRBUS
 5740 FORMAT('KRBUS(1)=',F9.5/
     *       'KRBUS(2)=',F9.5/
     *       'KRBUS(3)=',F9.5)
      WRITE(77,7775) KRBUS(1)
 7775 FORMAT('KRBUS,',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bus to CR Constants
C
      IF(CCODE(10)) THEN
      WRITE(26,4110)
 4110 FORMAT(/15X,'Bus to Commuter Rail Constants',/,
     *        15X,'----------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      POBS=0.0
      PEST=0.0
      DENOBS=0.0
      DENEST=0.0
      DO 4120 INC=1,3
         DENOBS=DENOBS+OESUM(11,INC)+OESUM(12,INC)+
     *                 OESUM(13,INC)+OESUM(14,INC)
         DENEST=DENEST+TESUM(11,INC)+TESUM(12,INC)+
     *                 TESUM(13,INC)+TESUM(14,INC)
         POBS=POBS+OESUM(12,INC)
         PEST=PEST+TESUM(12,INC)
 4120 CONTINUE      
         IF(DENOBS.GT.0) THEN
            POBS=POBS/DENOBS
         ELSE
            POBS=0.0
         END IF
         IF(DENEST.GT.0) THEN
            PEST=PEST/DENEST
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KBCR(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KBCR(1)=NCON
         KBCR(2)=NCON
         KBCR(3)=NCON
         WRITE(26,4130) POBS,PEST,ECON,RCON,NCON
 4130    FORMAT(1X,'KBCR   ',3X,2(1X,F7.4),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,4140) KBCR
 4140 FORMAT('KBCR(1)=',F9.5/
     *       'KBCR(2)=',F9.5/
     *       'KBCR(3)=',F9.5)
      WRITE(77,7776) KBCR(1)
 7776 FORMAT('KBCR,',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bus to UR Constants
C
      IF(CCODE(11)) THEN
      WRITE(26,4411)
 4411 FORMAT(/15X,'Bus to Urban Rail Constants',/,
     *        15X,'----------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      POBS=0.0
      PEST=0.0
      DENOBS=0.0
      DENEST=0.0
      DO 4420 INC=1,3
         DENOBS=DENOBS+OESUM(15,INC)+OESUM(16,INC)+
     *                 OESUM(17,INC)+OESUM(18,INC)
         DENEST=DENEST+TESUM(15,INC)+TESUM(16,INC)+
     *                 TESUM(17,INC)+TESUM(18,INC)
         POBS=POBS+OESUM(16,INC)
         PEST=PEST+TESUM(16,INC)
 4420 CONTINUE
         IF(DENOBS.GT.0) THEN
            POBS=POBS/DENOBS
         ELSE
            POBS=0.0
         END IF
         IF(DENEST.GT.0) THEN
            PEST=PEST/DENEST
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF((POBS.GT.0.0.AND.POBS.LT.1.0).AND.
     *      (PEST.GT.0.0.AND.PEST.LT.1.0)) THEN
            RCON=DLOG(((POBS*PEST)-POBS)/((POBS*PEST)-PEST))
         END IF
         ECON=KBUR(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KBUR(1)=NCON
         KBUR(2)=NCON
         KBUR(3)=NCON
         WRITE(26,4431) POBS,PEST,ECON,RCON,NCON
 4431    FORMAT(1X,'KBUR   ',3X,2(1X,F7.4),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,4442) KBUR
 4442 FORMAT('KBUR(1)=',F9.5/
     *       'KBUR(2)=',F9.5/
     *       'KBUR(3)=',F9.5)
      WRITE(77,7877) KBUR(1)
 7877 FORMAT('KBUR,',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Rapid Bus Transfer Constants
C
      IF(CCODE(12)) THEN
      WRITE(26,5711)
 5711 FORMAT(/15X,'Walk to Rapid Bus Transfer Constant',/,
     *        15X,'-----------------------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
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
         POBS=BUSRAPID
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
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KBRPDW=NCON
         WRITE(26,5731) POBS,PEST,ECON,RCON,NCON
 5731    FORMAT(1X,'KBRPDW ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,5741) KBRPDW
 5741 FORMAT('KBRPDW=',F9.5)
      WRITE(77,7577) KBRPDW
 7577 FORMAT('KBRPDW,',F9.5)
      END IF
C....
      IF(CCODE(22)) THEN
      WRITE(26,5781)
 5781 FORMAT(/15X,'Drive to Rapid Bus Transfer Constant',/,
     *        15X,'------------------------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
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
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KBRPDD=NCON
         WRITE(26,5783) POBS,PEST,ECON,RCON,NCON
 5783    FORMAT(1X,'KBRPDD ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,5784) KBRPDD
 5784 FORMAT('KBRPDD=',F9.5)
      WRITE(77,7677) KBRPDD
 7677 FORMAT('KBRPDD,',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Walk Access Transitway Transfer Constants
C
      IF(CCODE(13)) THEN
      WRITE(26,5811)
 5811 FORMAT(/15X,'Transitway Walk Access Transfer Constant',/,
     *        15X,'----------------------------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
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
         POBS=BUSTWY
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
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KBTWYW=NCON
         WRITE(26,5831) POBS,PEST,ECON,RCON,NCON
 5831    FORMAT(1X,'KBTWYW ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,5841) KBTWYW
 5841 FORMAT('KBTWYW=',F9.5)
      WRITE(77,7878) KBTWYW
 7878 FORMAT('KBTWYW,',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Walk Access Express Bus Transfer Constants
C
      IF(CCODE(14)) THEN
      WRITE(26,5911)
 5911 FORMAT(/15X,'Express Bus Walk Access Transfer Constant',/,
     *        15X,'-----------------------------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
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
         POBS=BUSEXP
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
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KBEXPW=NCON
         WRITE(26,5931) POBS,PEST,ECON,RCON,NCON
 5931    FORMAT(1X,'KBEXPW ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,5941) KBEXPW
 5941 FORMAT('KBEXPW=',F9.5)
      WRITE(77,7779) KBEXPW
 7779 FORMAT('KBEXPW,',F9.5)
      END IF
C
C----------------------------------------------------------------------
C      Calculate Urban Rail LAX Destination Constant
C
       IF(CCODE(18)) THEN
       ECON=KURLAX
       RCON=DLOG(LAXUR/URLAX)
       NCON=ECON+ADJFCT*RCON
       KURLAX=NCON
       WRITE(26,5943)
 5943 FORMAT(/15X,'Urban Rail LAX Constant',/,
     *        15X,'-----------------------',//,
     *  1X,'       '/
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'            Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
       WRITE(26,5944) LAXUR,URLAX,ECON,RCON,NCON
 5944  FORMAT(1X,'KURLAX ',3X,2(1X,F7.1),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
       WRITE(7,5942) KURLAX
 5942  FORMAT('KURLAX=',F9.5)
       URLAX=0.0
       END IF
C
C----------------------------------------------------------------------
C     Calculate BRT Constants
C
      IF(CCODE(19)) THEN
      WRITE(26,5712)
 5712 FORMAT(/15X,'BRT Constants',/,
     *        15X,'-------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
     *  1X,' Level     Percent Percent  Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
      DO 5722 INC=1,3
         TO=TO+OESUM(25,INC)+OESUM(26,INC)
         DENOBS=DENOBS+TESUM(5,INC)
         TE=TE+TESUM(56,INC)+TESUM(57,INC)
         DENEST=DENEST+TESUM(5,INC)
 5722 CONTINUE
         IF(DENOBS.GT.0) THEN
            POBS=TO/DENOBS
         ELSE
            POBS=0.0
         END IF
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
         ECON=KBRT(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KBRT(1)=NCON
         KBRT(2)=NCON
         KBRT(3)=NCON
         EQUIVT=(-1.0*NCON)/COEFF(100)
         WRITE(26,5732) POBS,PEST,ECON,RCON,NCON,EQUIVT
 5732    FORMAT(1X,'KBRT   ',3X,2(1X,F7.4),1X,
     A               F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      WRITE(7,5742) KBRT
 5742 FORMAT('KBRT(1)=',F9.5/
     *       'KBRT(2)=',F9.5/
     *       'KBRT(3)=',F9.5)
      WRITE(77,7777) KBRT(1)
 7777 FORMAT('KBRT,',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Rapid Bus Constants
C
      IF(CCODE(20)) THEN
      WRITE(26,5710)
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
      DO 5723 INC=1,3
         TO=TO+OESUM(21,INC)+OESUM(22,INC)
         DENOBS=DENOBS+TESUM(5,INC)
         TE=TE+TESUM(48,INC)+TESUM(49,INC)
         DENEST=DENEST+TESUM(5,INC)
 5723 CONTINUE
         IF(DENOBS.GT.0) THEN
            POBS=TO/DENOBS
         ELSE
            POBS=0.0
         END IF
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
         ECON=KRBUS(1)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KRBUS(1)=NCON
         KRBUS(2)=NCON
         KRBUS(3)=NCON
         EQUIVT=(-1.0*NCON)/COEFF(100)
         WRITE(26,5930) POBS,PEST,ECON,RCON,NCON,EQUIVT
 5930    FORMAT(1X,'KRBUS  ',3X,2(1X,F7.4),1X,
     A                F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      WRITE(7,5740) KRBUS
      WRITE(77,7775) KRBUS(1)
      END IF
C
C----------------------------------------------------------------------
C     Calculate BRT Bus Transfer Constants
C
      IF(CCODE(21)) THEN
      WRITE(26,5411)
 5411 FORMAT(/15X,'Walk to BRT Bus Transfer Constant',/,
     *        15X,'---------------------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
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
         POBS=BUSBRT
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
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KBBRTW=NCON
         WRITE(26,5431) POBS,PEST,ECON,RCON,NCON
 5431    FORMAT(1X,'KBBRTW ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,5441) KBBRTW
 5441 FORMAT('KBBRTW=',F9.5)
      WRITE(77,7477) KBBRTW
 7477 FORMAT('KBBRTW,',F9.5)
      END IF
      IF(CCODE(23)) THEN
      WRITE(26,5481)
 5481 FORMAT(/15X,'Drive to BRT Bus Transfer Constant',/,
     *        15X,'------------------------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
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
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KBBRTD=NCON
         WRITE(26,5483) POBS,PEST,ECON,RCON,NCON
 5483    FORMAT(1X,'KBBRTD ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,5484) KBBRTD
 5484 FORMAT('KBBRTD=',F9.5)
      WRITE(77,7447) KBBRTD
 7447 FORMAT('KBBRTD,',F9.5)
      END IF
C----------------------------------------------------------------------
C     Calculate Overall CBD Constant for Transit
C
      IF(CCODE(24)) THEN
      WRITE(26,5713)
 5713 FORMAT(/15X,'Overall CBD Constant for Transit',/,
     *        15X,'--------------------------------',//,
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
 5733    FORMAT(1X,'CCBD   ',3X,2(1X,F7.1),1X,
     A               F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,5743) CCBD
 5743 FORMAT('CCBD(1)=',F9.5/
     *       'CCBD(2)=',F9.5/
     *       'CCBD(3)=',F9.5/
     *       'CCBD(4)=',F9.5/
     *       'CCBD(5)=',F9.5/
     *       'CCBD(6)=',F9.5/
     *       'CCBD(7)=',F9.5)
      WRITE(77,7778) CCBD(1)
 7778 FORMAT('CCBD,',F9.5)
      ELSE
      WRITE(26,5714)
 5714 FORMAT(/15X,'Overall CBD Comparison for Transit',/,
     *        15X,'----------------------------------',//,
     *  1X,'             Obs     Est  '/
     *  1X,'            Value   Value '/
     *  1X,'---------  ------- -------')
      POBS=OBCBDTR
      PEST=CBDTRAN
      WRITE(26,5734) POBS,PEST
 5734 FORMAT(1X,'CCBD   ',3X,2(1X,F7.1))      
      END IF
C
C----------------------------------------------------------------------
C     Calculate Drive Access Transitway Transfer Constants
C
      IF(CCODE(25)) THEN
      WRITE(26,5812)
 5812 FORMAT(/15X,'Transitway Drive Access Transfer Constant',/,
     *        15X,'-----------------------------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
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
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KBTWYD=NCON
         WRITE(26,5832) POBS,PEST,ECON,RCON,NCON
 5832    FORMAT(1X,'KBTWYD ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,5842) KBTWYD
 5842 FORMAT('KBTWYD=',F9.5)
      WRITE(77,7888) KBTWYD
 7888 FORMAT('KBTWYD,',F9.5)

      END IF
C
C----------------------------------------------------------------------
C     Calculate Drive Access Express Bus Transfer Constants
C
      IF(CCODE(26)) THEN
      WRITE(26,5912)
 5912 FORMAT(/15X,'Express Bus Drive Access Transfer Constant',/,
     *        15X,'------------------------------------------',//,
     *  1X,' Income'/
     *  1X,' Group       Obs     Est    Existing               New'/
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
         IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KBEXPD=NCON
         WRITE(26,5932) POBS,PEST,ECON,RCON,NCON
 5932    FORMAT(1X,'KBEXPD ',3X,2(1X,F7.4),1X,
     *                     F9.5,1X,F10.5,1X,F9.5)
      WRITE(7,5945) KBEXPD
 5945 FORMAT('KBEXPD=',F9.5)
      WRITE(77,7789) KBEXPD
 7789 FORMAT('KBEXPD,',F9.5)
      END IF
C
C----------------------------------------------------------------------
C End of constant adjustments
C----------------------------------------------------------------------
  170 CONTINUE
      DO 900 INC=1,4
         DO 900 MODE=1,64
            TESUM(MODE,INC)=0.0
  900 CONTINUE
      DO 903 INC=1,4
	DO 903,MODE=1,27
	      OESUM(MODE,INC)=0.0
            DFSUM(MODE,INC)=0.0
            PCSUM(MODE,INC)=0.0
  903 CONTINUE
       DO 901,INC=1,3
       DO 901,IMO=1,5
       DO 901,ICO=1,5
         DFSUMC(IMO,INC,ICO)=0.0
         PCSUMC(IMO,INC,ICO)=0.0
  901  CONTINUE
      do 905,ic=1,5
	do 905,im=1,3
	do 905,it=1,5
	calsum(ic,im,it)=0.0
 905	continue
      DO 359 C=1,3
      DO 360 K=1,21
      PTRIP(K,C)=0.0
      TTRIP(K,C)=0.0
 360  CONTINUE
 359  CONTINUE
 999  RETURN
      END
