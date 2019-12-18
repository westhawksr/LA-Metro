C***********************************************************************
C                                                                      *
C     PROGRAM CCRPREP3: MERGE PHASE 2 INPUT MATRICES                   *
C                      CAPACITY, CROWDING AND RELIABILITY              *
C                                                                      *
C              DUNBAR TRANSPORTATION,LLC                               *
C                SAN RAMON, CALIFORNIA                                 *
C                                                                      *
C***********************************************************************
      INCLUDE 'CONTROL.INC'
      INCLUDE 'LUNCOM.INC'
      INCLUDE 'TPCOM.INC'
      INTEGER*2    TIME(4),DATE(3),STAT1
      INTEGER*4    TRIPS(4000),LUNIT(40)
      INTEGER*2    IZ,JZ,T,TABLEI(80),DUMMY,IO(500),PURP
      INTEGER*2    IT,ITX
      INTEGER*4    VAR(5500),IVALUE(5500,26,9)
      INTEGER*4    LREL(5500,7) 
      INTEGER*8    MTXSUM(9,26),OUTSUM(7)
      CHARACTER*90 NAMES(50),CONTROL
      CHARACTER*10 MTXNAME(7)
      CHARACTER*1  FF
      LOGICAL      CTLERR(11)
      DATA         TABLEI/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
     *                   16,17,18,19,20,21,22,23,24,25,26,27,28,
     *                   29,30,31,32,33,34,35,36,37,38,39,40,
     *                   41,42,43,44,45,46,47,48,49,50,51,52,
     *                   53,54,55,56,57,58,59,60,61,62,63,64,
     *                   65,66,67,68,69,70,71,72,73,74,75,76,
     *                   77,78,79,80/
      DATA         LUNIT/10*0,11,12,13,14,15,16,17,23*0/
      DATA         NAMES/50*' '/
      DATA         MTXNAME/'LOCAL','RAPID','EXPRESS','TRANSITWAY',
     *                     'ACCESS','STA-TO-STA','BLENDED'/
      FF=CHAR(12)
      MTXSUM=0
      OUTSUM=0
C
C     OPEN AND READ THE CONTROL FILE
C
      CALL GETARG(1,CONTROL,STAT1)
      IF(STAT1.LE.0) THEN
        WRITE(*,'('' CCRPREP3 (F) : CONTROL FILE NOT SPECIFIED'')')
        STOP " ENTER THE CONTROL FILE NAME ON THE COMMAND LINE."
      END IF
      OPEN(10,FILE=CONTROL,STATUS='OLD',FORM='FORMATTED',
     *     ERR=2)
      REWIND 10
      GO TO 3
    2 WRITE(*,9001)
 9001 FORMAT(/,1X,'CCRPREP3 9001 (F) ERROR OPENING CONTROL CARD INPUT',
     *       ' FILE')
      STOP 8
    3 REWIND 10
      CALL RDFILES(CTLERR,NAMES)

C
C OPEN TRANPLAN INPUT MATRIX FILES
C
      DO 10 IT=1,11
      IF(IT.EQ.8.OR.IT.EQ.9) CYCLE
      IF(.NOT.CTLERR(IT)) THEN
      OPEN((IT+10),FILE=NAMES(IT),STATUS='OLD',FORM='UNFORMATTED')
C
C SUMMARIZE HEADER INFORMATION
C
      WRITE(26,9003) NAMES(IT)
 9003 FORMAT(1X,'INPUT FILE NAME=',A80)
      READ(IT+10) HEAD1,HEAD2
      WRITE(26,9004) FNAME,FUNCT,GDATE,GTIME,FTYPE,GNAME,MAXZON,NUMPUR
 9004 FORMAT(1X,'FNAME=',1X,A8,1X,4A8,/
     *       1X,'FUNCT=',1X,4A8,/,
     *       1X,'GDATE=',1X,A8,/,
     *       1X,'GTIME=',1X,A8,/,
     *       1X,'FTYPE=',1X,A8,/,
     *       1X,'GNAME=',1X,A8,/,
     *       1X,'MAXZON=',I9/,
     *       1X,'NUMPUR=',I9/)
      IF(MAXZON.GT.5500) THEN
      WRITE(26,9005) MAXZON
 9005 FORMAT(1X,'THE NUMBER OF ZONES (',I4,') EXCEEDS 5500')
      STOP 8
      END IF
      ELSE
      WRITE(26,9007)
 9007 FORMAT(/' CCRPREP3 (F) 9007 PROGRAM TERMINATED DUE TO ',
     *       ' MISSING INPUT FILES'/)
      WRITE(*,9007)
      STOP 9007
      END IF
   10 CONTINUE
C
C OPEN OUTPUT MERGED MATRIX
C
      NUMPUR=7
      TABLES=2**7-1
      WRITE(96) HEAD1, HEAD2
C ----------------------------------------------------------------------    
      DO 100 IZ=1,MAXZON
C ----------------------------------------------------------------------
      nk=mod(IZ,100)
      if(nk.EQ.0) WRITE(*,9006) IZ
 9006 FORMAT(' Processing Origin Zone=',I5)
C
C READ PENALTY MATRICES
C  
      DO 101 IT=1,11
      IF(IT.EQ.8.OR.IT.EQ.9) CYCLE
      IF(CTLERR(IT)) GO TO 101
      DO 102 PURP=1,26
      CALL INTAB((IT+10),VAR,IZ,PURP,DUMMY,IO)
C...STORE IN VALUE MATRIX
      IT2=IT
      IF(IT.GT.9) IT2=IT-2
      DO 103 JZ=1,MAXZON
      IVALUE(JZ,PURP,IT2)=VAR(JZ)
      MTXSUM(IT2,PURP)=MTXSUM(IT2,PURP)+VAR(JZ)
  103 CONTINUE
  102 CONTINUE
  101 CONTINUE
C.....PLACE VALUES IN OUTPUT MATRIX
      DO 105 JZ=1,MAXZON
C.....LOCAL BUS
      LREL(JZ,1)=IVALUE(JZ,11,1)+IVALUE(JZ,14,1)+IVALUE(JZ,15,1)+
     *           IVALUE(JZ,16,1)+IVALUE(JZ,17,1)+IVALUE(JZ,18,1)+
     *           IVALUE(JZ,19,1)+IVALUE(JZ,20,1)
C.....RAPID BUS
      LREL(JZ,2)=IVALUE(JZ,11,2)+IVALUE(JZ,14,2)+IVALUE(JZ,15,2)+
     *           IVALUE(JZ,16,2)+IVALUE(JZ,17,2)+IVALUE(JZ,18,2)+
     *           IVALUE(JZ,19,2)+IVALUE(JZ,20,2)+IVALUE(JZ,24,2)
C.....EXPRESS BUS
      LREL(JZ,3)=IVALUE(JZ,11,3)+IVALUE(JZ,14,3)+IVALUE(JZ,15,3)+
     *           IVALUE(JZ,16,3)+IVALUE(JZ,17,3)+IVALUE(JZ,18,3)+
     *           IVALUE(JZ,19,3)+IVALUE(JZ,20,3)+IVALUE(JZ,24,3)+
     *           IVALUE(JZ,12,3)+IVALUE(JZ,21,3)+IVALUE(JZ,22,3)+
     *           IVALUE(JZ,23,3)
C.....TRANSITWAY
      LREL(JZ,4)=IVALUE(JZ,11,4)+IVALUE(JZ,14,4)+IVALUE(JZ,15,4)+
     *           IVALUE(JZ,16,4)+IVALUE(JZ,17,4)+IVALUE(JZ,18,4)+
     *           IVALUE(JZ,19,4)+IVALUE(JZ,20,4)+IVALUE(JZ,24,4)+
     (           IVALUE(JZ,12,4)+IVALUE(JZ,21,4)+IVALUE(JZ,22,4)+
     *           IVALUE(JZ,23,4)+IVALUE(JZ,25,4)
C.....FIXED GUIDEWAY
      LREL(JZ,6)=IVALUE(JZ,26,5)+IVALUE(JZ,13,6)+IVALUE(JZ,10,7)
C.....FIXED GUIDEWAY BLENDED PATH
      LREL(JZ,7)=IVALUE(JZ,10,9)+IVALUE(JZ,13,9)+IVALUE(JZ,26,9)
C.....BUS ACCESS/EGRESS TO FIXED GUIDEWAY
      LREL(JZ,5)=IVALUE(JZ,11,8)+IVALUE(JZ,14,8)+IVALUE(JZ,15,8)+
     *           IVALUE(JZ,16,8)+IVALUE(JZ,17,8)+IVALUE(JZ,18,8)+
     *           IVALUE(JZ,19,8)+IVALUE(JZ,20,8)+IVALUE(JZ,24,8)+
     *           IVALUE(JZ,12,8)+IVALUE(JZ,21,8)+IVALUE(JZ,22,8)+
     *           IVALUE(JZ,23,8)+IVALUE(JZ,25,8)
  105 CONTINUE
C.....WRITE OUTPUT TABLES
      DO 106 IT=1,7
      DO 107 JZ=1,MAXZON
      VAR(JZ)=LREL(JZ,IT)
      IF(VAR(JZ).GT.32766) THEN
      WRITE(26,7001) IZ,JZ,IT,VAR(JZ)
 7001 FORMAT(' CCPREP3 7001 (W) ENCOUNTERED VALUE GREATER THAN 32766',
     *       ' FOR ORIGIN ZONE ',I4,' DESTINATION ZONE ',I4,
     *       ' OUTPUT TABLE ',I1,' VALUE=',I8)
      WRITE(*,7001) IZ,JZ,IT,VAR(JZ)
      END IF
      OUTSUM(IT)=OUTSUM(IT)+VAR(JZ)
  107 CONTINUE
      CALL OUTAB(96,VAR,IZ,IT,DUMMY,IO)
  106 CONTINUE
  100 CONTINUE
C
      DO IT=1,11
      IF(IT.EQ.8.OR.IT.EQ.9) CYCLE
      WRITE(26,8003) NAMES(IT)
 8003 FORMAT(//1X,' INPUT MATRIX SUMMARY TOTALS FOR ',A50/
     *         1X,'---------------------------------'//
     *         1X,'  TABLE          TOTAL '/
     *         1X,'  -----       ------------')
      IT2=IT
      IF(IT.GT.9) IT2=IT-2
      DO T=1,26
      WRITE(26,8002) T,MTXSUM(IT2,T)
 8002 FORMAT(5X,I2,8X,I12)
      END DO
      END DO
      WRITE(26,8004) NAMES(8)
 8004 FORMAT(//1X,' OUTPUT MATRIX SUMMARY TOTALS FOR ',A50/
     *         1X,'----------------------------------'//
     *         1X,'  TABLE          TOTAL '/
     *         1X,'  -----       ------------')
      DO T=1,7
      WRITE(26,8005) T,OUTSUM(T),MTXNAME(T)
 8005 FORMAT(5X,I2,8X,I12,2X,A15)
      END DO
C
      CALL GETTIM(TIME(1),TIME(2),TIME(3),TIME(4))
      WRITE(26,9002) (TIME(T),T=1,3)
      WRITE(*,9002) (TIME(T),T=1,3)
 9002 FORMAT(/'    PROGRAM CCRPREP3 Completed'/
     *        '     Time: ',I2,':',I2,':',I2,///)
      END
