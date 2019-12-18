C-------------------------------------------------
C     SAVE AIR PASSENGER PROBABILITIES
C-------------------------------------------------
	SUBROUTINE AIRPROB(LAXTPROB,LAXSTAP,LAXSTAA,
     *                   LAXRPROB,LAXRNTP,LAXRNTA,
     *                   LAXIPROB,LAXITFP,LAXITFA,
     *                   LAXI2PROB,LAXITF2P,LAXITF2A,
     *                   LAXFPROB,LAXSTAFP,LAXSTAFA)
      INCLUDE 'param.com'
      INCLUDE 'stadat.com'
      INCLUDE 'mtamcpar.inc'	    
	INTEGER*2     IZ,JZ
      INTEGER*2     LAXSTAP(50,50,34),LAXSTAA(50,50,25)
      INTEGER*2     LAXRNTP(10,50,34),LAXRNTA(10,50,25)
      INTEGER*2     LAXITFP(50,34),LAXITFA(50,34)
      INTEGER*2     LAXITF2P(50,34),LAXITF2A(50,34)
      INTEGER*2     LAXSTAFP(MAX_IZONES,10,34)
      INTEGER*2     LAXSTAFA(MAX_IZONES,10,25)
      REAL*8        LAXTPROB(50,50,42),LAXFPROB(MAX_IZONES,10,42)
      REAL*8        LAXRPROB(10,50,42),LAXIPROB(50,42)
      REAL*8        LAXI2PROB(50,42)
C
	OPEN(131,FILE=FAIRPROB,STATUS='UNKNOWN',FORM='BINARY')
C...PARKING LOT
	    DO IZ=1,50
	    DO JZ=1,50
	    WRITE(131) IZ,JZ,(LAXTPROB(IZ,JZ,K),K=1,42)
	    END DO
	    END DO
	    DO IZ=1,50
	    DO JZ=1,50
	    WRITE(131) IZ,JZ,(LAXSTAP(IZ,JZ,K),K=1,34)
	    END DO
	    END DO	    
	    DO IZ=1,50
	    DO JZ=1,50
	    WRITE(131) IZ,JZ,(LAXSTAA(IZ,JZ,K),K=1,25)
	    END DO
	    END DO
C...RENTAL
	    DO IZ=1,10
	    DO JZ=1,50
	    WRITE(131) IZ,JZ,(LAXRPROB(IZ,JZ,K),K=1,42)
	    END DO
	    END DO
	    DO IZ=1,10
	    DO JZ=1,50
	    WRITE(131) IZ,JZ,(LAXRNTP(IZ,JZ,K),K=1,34)
	    END DO
	    END DO	    
	    DO IZ=1,10
	    DO JZ=1,50
	    WRITE(131) IZ,JZ,(LAXRNTA(IZ,JZ,K),K=1,25)
	    END DO
	    END DO	 	
C...ITF ZONE 1
	    DO JZ=1,50
	    WRITE(131) JZ,(LAXIPROB(JZ,K),K=1,42)
	    END DO
	    DO JZ=1,50
	    WRITE(131) JZ,(LAXITFP(JZ,K),K=1,34)
	    END DO	    
	    DO JZ=1,50
	    WRITE(131) JZ,(LAXITFA(JZ,K),K=1,34)
	    END DO	
C...ITF ZONE 2
	    DO JZ=1,50
	    WRITE(131) JZ,(LAXI2PROB(JZ,K),K=1,42)
	    END DO
	    DO JZ=1,50
	    WRITE(131) JZ,(LAXITF2P(JZ,K),K=1,34)
	    END DO	    
	    DO JZ=1,50
	    WRITE(131) JZ,(LAXITF2A(JZ,K),K=1,34)
	    END DO	
C...FLYAWAY
	    DO IZ=1,MAX_IZONES
	    DO JZ=1,10
	    WRITE(131) IZ,JZ,(LAXFPROB(IZ,JZ,K),K=1,42)
	    END DO
	    END DO
	    DO IZ=1,MAX_IZONES
	    DO JZ=1,10
	    WRITE(131) IZ,JZ,(LAXSTAFP(IZ,JZ,K),K=1,34)
	    END DO
	    END DO	    
	    DO IZ=1,MAX_IZONES
	    DO JZ=1,10
	    WRITE(131) IZ,JZ,(LAXSTAFA(IZ,JZ,K),K=1,25)
	    END DO
	    END DO	    
	    RETURN
	    END
