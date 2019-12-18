C *******************************************************************
C Parameter Declarations for the MTAMC Mode Choice Model
C                                Application Program
C
C *******************************************************************

      integer*2    MAX_ZONES,MAX_IZONES,MAX_STATIONS
C ----------------------------------------------------------------
C     LOS ANGELES MTA PARAMETERS (ORIGINAL)
C ----------------------------------------------------------------          
C      parameter(MAX_ZONES     = 3720)
C      parameter(MAX_IZONES    = 2988)
C      parameter(MAX_STATIONS  = 732)
C ----------------------------------------------------------------
C     LOS ANGELES MTA PARAMETERS (NEW)
C ----------------------------------------------------------------          
       parameter(MAX_ZONES     = 3731)
       parameter(MAX_IZONES    = 3057)
       parameter(MAX_STATIONS  = 674)
       parameter(BMAX_IZONES   = 3057)
       parameter(BMAX_STATIONS = 674)
C ----------------------------------------------------------------
C     RIVERSIDE PARAMETERS
C ----------------------------------------------------------------
C     parameter(MAX_ZONES     = 3524)
C     parameter(MAX_IZONES    = 3217)
C     parameter(MAX_STATIONS  = 307)
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