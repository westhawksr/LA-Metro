C---------------------------------------------------------------------------
C     DRIVE -- EXPRESS BUS & TRANSITWAY BEST PATH INFO SUBROUTINE
C---------------------------------------------------------------------------
      SUBROUTINE DRVBUS(JZ,CSTA,CDSTA,IMODE,VALUES)
      include 'mtamcpar.inc'
      INTEGER*2 JZ,CSTA,CDSTA,SMODE,SIC,SDC,SJZ,IMODE
      REAL*4    VALUES(15)
      REAL*4       INVEHL,INVEHE,INVEHT,
     *             WAIT1,TRWAIT,TRANSF,FARE5,
     *             INVEH2,WALKACC,WALKEGR,
     *             WALKTFR,SELUTL,STAUTL1
      REAL*4       INVEH2J,INVEHLJ,INVEHEJ,INVEHTJ,
     *             INVEHRJ,WAIT1J,TRWAITJ,
     *             TRANSFJ,FARE5J,WALKEGRJ,
     *             WALKTFRJ,WALKACCJ,
     *             STAZNE1,STAZNE2
C
      IF(CSTA.EQ.MAX_ZONES) RETURN
      IF(CSTA.LE.0) RETURN
      REWIND 34
 6300 READ(34,END=6310) SMODE,SIC,SJZ,INVEH2J,
     *               INVEHLJ,INVEHEJ,INVEHTJ,
     *               INVEHRJ,WAIT1J,TRWAITJ,
     *               TRANSFJ,FARE5J,WALKEGRJ,
     *               WALKTFRJ,WALKACCJ,
     *               STAZNE1,STAZNE2
      IF(SMODE.EQ.IMODE.AND.CSTA.EQ.SIC.AND.SJZ.EQ.JZ) GO TO 6350
      GO TO 6300
 6310 WRITE(26,6228) CSTA,JZ
 6228 FORMAT(//' (DRVBUS) NO MATCH FOR CSTA=',I4,' JZ=',I4/)
      STOP 16
 6350 CONTINUE
      VALUES(1)=WAIT1J
      VALUES(2)=TRWAITJ
      VALUES(3)=TRANSFJ
      VALUES(4)=0.0
      VALUES(5)=0.0
      VALUES(6)=WALKEGRJ
      VALUES(7)=WALKACCJ+WALKTFRJ
      VALUES(8)=INVEHLJ
      VALUES(9)=INVEHEJ
      VALUES(10)=0.0
      VALUES(11)=0.0
      VALUES(12)=0.0
      VALUES(13)=FARE5J 
      VALUES(14)=INVEHRJ
      VALUES(15)=0.0
      IF(SMODE.EQ.4) THEN
      VALUES(10)=INVEHTJ
      VALUES(15)=0.0
      END IF
      IF(SMODE.EQ.5) THEN
      VALUES(10)=0.0
      VALUES(15)=INVEHTJ
      END IF
      RETURN
      END
C---------------------------------------------------------------------------
C     RESET SKIM VALUES ARRAY TO ZERO
C---------------------------------------------------------------------------
      SUBROUTINE RESETV(VALUES)
      REAL*4    VALUES(15)
      DO 1 I=1,15
      VALUES(I)=0.0
    1 CONTINUE
      RETURN
      END
