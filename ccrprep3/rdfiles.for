C**********************************************************
C   SUBROUTINE RDFILES: OBTAIN DATA SET NAMES FOR INPUT   *
C                       FILES                             *
C**********************************************************
      SUBROUTINE RDFILES(CTLERR,NAMES)
      LOGICAL      CTLERR(11),EXISTS
      INTEGER*4    TIME(4),DATE(3),STAT1
      INTEGER*4    IT,ST,TT,UT,VT,XT,LEN(50),RLEN
      CHARACTER*1  COMMA,BLANK
      CHARACTER*10 FILES(50)
      CHARACTER*90 NAMES(50)
      CHARACTER*90 STRING
      DATA         COMMA/','/,BLANK/' '/
      DATA         FILES/'FCCRLOC=','FCCRRPD=','FCCREXP=','FCCRTWY=',
     *                   'FCCRBRT=','FCCRURB=','FCCRCRT=','FCCROUT=',
     *                   'FREPORT=','FCCRBUS=','FCCRFGW=',
     *                    39*' '/
      DATA         LEN/11*7,39*0/
C
C WRITE PROGRAM HEADER INFORMATION
C
      CALL GETTIM(TIME(1),TIME(2),TIME(3),TIME(4))
      CALL GETDAT(DATE(3),DATE(1),DATE(2))
      WRITE(*,1000) DATE,(TIME(T),T=1,3)
 1000 FORMAT(/'              PROGRAM CCRPREP3'/
     *        '     PREPARE CAPACITY, CROWDING & RELIABILITY'/
     *        '               INPUT MATRICES'/
     *        '          [Version Date: 17Jul19]'//
     *        '          Date: ',I2,'/',I2,'/',I4/
     *        '          Time: ',I2,':',I2,':',I2,///)
C
C SEARCH FOR ONE FILE NAME AT A TIME
C
      DO 100 IT=1,11
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
C SUMMARIZE FILE NAMES
C
      OPEN(26,FILE=NAMES(9),STATUS='UNKNOWN',FORM='FORMATTED',ERR=304)
      WRITE(26,1000) DATE,(TIME(T),T=1,3)
C 
C PRINT FILE INPUT/OUTPUT SUMMARY
C
      WRITE(26,500) (NAMES(K),K=1,7),NAMES(10),NAMES(11),NAMES(8)
  500 FORMAT(1X,'SUMMARY OF FILE INPUTS & OUTPUTS'/
     *       1X,'--------------------------------------------------'/
     *       1X,'FCCRLOC= ',a40,' PENALTY FILE - LOCAL      BUS'/
     *       1X,'FCCRRPD= ',a40,' PENALTY FILE - RAPID      BUS'/
     *       1X,'FCCREXP= ',a40,' PENALTY FILE - EXPRESS    BUS'/
     *       1X,'FCCRTWY= ',a40,' PENALTY FILE - TRANSITWAY BUS'/
     *       1X,'FCCRBRT= ',a40,' PENALTY FILE - BRT        BUS'/
     *       1X,'FCCRURB= ',a40,' PENALTY FILE - URBAN     RAIL'/
     *       1X,'FCCRCRT= ',a40,' PENALTY FILE - COMMUTER  RAIL'/
     *       1X,'FCCRBUS= ',a40,' PENALTY FILE - BUS ACCESS/EGRESS'/
     *       1X,'FCCRFGW= ',a40,' PENALTY FILE - FIXED GUIDEWAY ',
     *                          'BLENDED PATH'/
     *       1X,'FCCROUT= ',a40,' PENALTY FILE - OUTPUT'/)
C
      DO 400 IT=1,11
      IF(IT.EQ.8.OR.IT.EQ.9) CYCLE
C
C VERIFY EXISTENCE OF INPUT FILES
C
      INQUIRE(FILE=NAMES(IT),EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(*,9004) NAMES(IT)
      WRITE(26,9004) NAMES(IT)
 9004 FORMAT(/1X,'INPUT FILE= ',A60,' DOES NOT EXIST'/)
      CTLERR(IT)=.TRUE.
      END IF
  400 CONTINUE
      OPEN(96,FILE=NAMES(8),STATUS='UNKNOWN',FORM='UNFORMATTED',ERR=302)
      RETURN
  304 WRITE(*,301) NAMES(9)
  301 FORMAT(/' RDFILES 301 (F) ERROR OPENING REPORT FILE ',A50)
      STOP
      RETURN
  302 WRITE(*,303) NAMES(8)
  303 FORMAT(/' RDFILES 303 (F) ERROR OPENING OUTPUT FILE ',A50)
      STOP
      RETURN
      END
