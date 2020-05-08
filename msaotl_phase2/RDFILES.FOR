C**********************************************************
C   SUBROUTINE RDFILES: OBTAIN DATA SET NAMES FOR INPUT   *
C                       FILES                             *
C**********************************************************
      SUBROUTINE RDFILES(CTLERR,NAMES)
      INCLUDE 'CONTROL.INC'
      INCLUDE 'LUNCOM.INC'
      INCLUDE 'TPCOM.INC'
      LOGICAL      CTLERR,EXISTS
      INTEGER*4    IT,ST,TT,UT,VT,XT,LEN(19),RLEN,T
      CHARACTER*1  COMMA,BLANK
      CHARACTER*12  FILES(19)
      CHARACTER*80 NAMES(19)
      CHARACTER*80 STRING
      DATA         COMMA/','/,BLANK/' '/
      DATA         FILES/'CR_ITER0=','UR_ITER0=',
     *                   'BRT_ITER0=','BLEND_ITER0=',
     *                   'BUS_ITER0=','NBUS_ITER0=',
     *                   'CR_ITER1=','UR_ITER1=',
     *                   'BRT_ITER1=','BLEND_ITER1=',
     *                   'BUS_ITER1=','NBUS_ITER1=',
     *                   'CROUT=','UROUT=',
     *                   'BRTOUT=','BLENDOUT=',
     *                   'BUSOUT=','NBUSOUT=',
     *                   'FRPORT='/
C     DATA         NAMES/19*' '/
      DATA         LEN/8,8,9,11,9,10,8,8,9,11,9,10,5,5,6,8,6,7,6/
      CTLERR=.FALSE.
C
C SEARCH FOR ONE FILE NAME AT A TIME
C
      DO 100 IT=1,19
      REWIND 10
    1 READ(10,101,END=100) STRING
  101 FORMAT(A80)
      RLEN=80-LEN(IT)
      DO 200 ST=1,RLEN
      TT=ST+LEN(IT)
      IF(STRING(ST:TT).EQ.FILES(IT)) THEN
C
C NOW SEARCH FOR COMMA
C
      DO 300 UT=TT,80
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
      OPEN(29,FILE=NAMES(19),STATUS='UNKNOWN',FORM='FORMATTED')
C
C WRITE PROGRAM BANNER, DATE & TIME
C
      CALL GETDAT(IYR,IMON,IDAY)
      CALL GETTIM(IHR,IMIN,ISEC,I100)
      IYR=IYR-2000
      WRITE(29,7) IMON,IDAY,IYR,IHR,IMIN,ISEC
      WRITE(*,7) IMON,IDAY,IYR,IHR,IMIN,ISEC
    7 FORMAT(/'              PROGRAM MSAOTL'/
     *        '         MSA COMPUTATION PROGRAM'/
     *        '          [Version Date: 13Jun16]'/
     *        1X,'         [Updated: 16Apr20]'/
     *        1X,'           Date: ',I2,'/',I2,'/',I2/
     *        1X,'           Time: ',I2,':',I2,':',I2,///)
C 
C PRINT FILE INPUT/OUTPUT SUMMARY
C
      WRITE(29,500) NAMES
  500 FORMAT(1X,'SUMMARY OF FILE INPUTS & OUTPUTS'/
     *   1X,'--------------------------------------------------'/
     *   1X,'CR_ITER0    ',a40,' COMMUTER RAIL MATRIX INPUT - ITER 0'/
     *   1X,'UR_ITER0    ',a40,' URBAN    RAIL MATRIX INPUT - ITER 0'/
     *   1X,'BRT_ITER0   ',a40,' BRT           MATRIX INPUT - ITER 0'/
     *   1X,'BLEND_ITER0 ',a40,' BLENDED       MATRIX INPUT - ITER 0'/
     *   1X,'BUS_ITER0   ',a40,' ZONE TO ZONE TRANSIT INPUT - ITER 0'/
     *   1X,'NBUS_ITER0  ',a40,' STATION/ZONE TRANSIT INPUT - ITER 0'//
     *   1X,'CR_ITER1    ',a40,' COMMUTER RAIL MATRIX INPUT - ITER 1'/  
     *   1X,'UR_ITER1    ',a40,' URBAN    RAIL MATRIX INPUT - ITER 1'/  
     *   1X,'BRT_ITER1   ',a40,' BRT           MATRIX INPUT - ITER 1'/
     *   1X,'BLEND_ITER1 ',a40,' BLENDED       MATRIX INPUT - ITER 1'/
     *   1X,'BUS_ITER1   ',a40,' ZONE TO ZONE TRANSIT INPUT - ITER 1'/  
     *   1X,'NBUS_ITER1  ',a40,' STATION/ZONE TRANSIT INPUT - ITER 1'//
     *   1X,'CROUT       ',a40,' COMMUTER RAIL MATRIX OUTPUT'/  
     *   1X,'UROUT       ',a40,' URBAN    RAIL MATRIX OUTPUT'/  
     *   1X,'BRTOUT      ',a40,' BRT           MATRIX OUTPUT'/
     *   1X,'BLENDOUT    ',a40,' BLENDED       MATRIX OUTPUT'/
     *   1X,'BUSOUT      ',a40,' ZONE TO ZONE TRANSIT OUTPUT'/  
     *   1X,'NBUSOUT     ',a40,' STATION/ZONE TRANSIT OUTPUT'// 
     *   1X,'FRPORT      ',a40,' REPORT FILE OUTPUT'/)
C
C CHECK FOR PRESENCE OF INPUT FILES
C
      DO T=1,12
      INQUIRE (FILE=NAMES(T),EXIST=EXISTS)
      IF(EXISTS) THEN
	     OPEN((T+10),FILE=NAMES(T),STATUS='UNKNOWN',
     *         FORM='UNFORMATTED')
      READ(T+10) HEAD1,HEAD2
C
C SUMMARIZE HEADER INFORMATION
C
      WRITE(29,7007) NAMES(T)
 7007 FORMAT(1X,'INPUT FILE NAME=',A40)
      WRITE(29,7001) FNAME,FUNCT,GDATE,GTIME,FTYPE,GNAME
 7001 FORMAT(1X,'FNAME=',1X,A8,1X,4A8,/
     *       1X,'FUNCT=',1X,4A8,/,
     *       1X,'GDATE=',1X,A8,/,
     *       1X,'GTIME=',1X,A8,/,
     *       1X,'FTYPE=',1X,A8,/,
     *       1X,'GNAME=',1X,A8)
      WRITE(29,7002) ((TITLE(I,J),I=1,10),J=1,3)
 7002 FORMAT(3(10A8,/))
      IF(MAXZON.GT.5500) THEN
      WRITE(29,8014) MAXZON
 8014 FORMAT(1X,'THE NUMBER OF ZONES (',I4,') EXCEEDS 5500')
      STOP 8
      END IF
      ELSE
      WRITE(29,8001) FILES(T)
 8001 FORMAT(//' ** INPUT FILE ',A11,' NOT FOUND **'//)
      CTLERR=.TRUE.
      END IF
      END DO
      RETURN
      END
