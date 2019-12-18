C *******************************************************************
C Parameter Declarations for the MTAMC Mode Choice Model
C                                Application Program
C
C *******************************************************************

      integer*2    MAX_ZONES,MAX_IZONES,MAX_STATIONS,MAX_WAIT,MAX_TLF,
     *             BMAX_IZONES,BMAX_STATIONS
C ----------------------------------------------------------------
C     LOS ANGELES MTA PARAMETERS (ORIGINAL)
C ----------------------------------------------------------------          
C      parameter(MAX_ZONES     = 3720)
C      parameter(MAX_IZONES    = 2988)
C      parameter(MAX_STATIONS  = 732)
C ----------------------------------------------------------------
C     LOS ANGELES MTA PARAMETERS (NEW)
C ----------------------------------------------------------------          
C      parameter(MAX_ZONES     = 3731)
C      parameter(MAX_IZONES    = 3057)
C      parameter(MAX_STATIONS  = 674)
C ----------------------------------------------------------------
C     RIVERSIDE PARAMETERS
C ----------------------------------------------------------------
C     parameter(MAX_ZONES     = 3524)
C     parameter(MAX_IZONES    = 3217)
C     parameter(MAX_STATIONS  = 307)
C     parameter(BMAX_IZONES   = 3217)
C     parameter(BMAX_STATIONS = 307)
C ----------------------------------------------------------------
C     GOLD LINE PARAMETERS
C ----------------------------------------------------------------
C     parameter(MAX_ZONES     = 3730)
C     parameter(MAX_IZONES    = 3057)
C     parameter(MAX_STATIONS  = 673)
C ----------------------------------------------------------------
C     LOS ANGELES MTA PARAMETERS (SPECIAL)
C ----------------------------------------------------------------          
C      parameter(MAX_ZONES     = 3730)
C      parameter(MAX_IZONES    = 3057)
C      parameter(MAX_STATIONS  = 673)
C ----------------------------------------------------------------
C     LOS ANGELES MTA PARAMETERS (LAX EXTENSION VERSION)
C ----------------------------------------------------------------          
C      parameter(MAX_ZONES     = 3800)
C      parameter(MAX_IZONES    = 3057)
C      parameter(MAX_STATIONS  = 743)
C      parameter(BMAX_IZONES   = 3057)
C      parameter(BMAX_STATIONS = 743)
C      parameter(MAX_TLF       = 700)
C      parameter(MAX_WAIT      = 400)
C ----------------------------------------------------------------
C     LOS ANGELES MTA PARAMETERS (WSAB)
C ----------------------------------------------------------------          
       parameter(MAX_ZONES     = 3800)
       parameter(MAX_IZONES    = 3057)
       parameter(MAX_STATIONS  = 743)