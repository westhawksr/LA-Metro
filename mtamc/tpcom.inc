C=====> *COMDECK.TPCOM INSERTED HERE
C***********************************************************************
C***********************************************************************
C
C     DCO TRANPLAN -- MASTER COMMON BLOCK FOR ALL ROUTINES
C
      COMMON /EXEC1/ HEAD1(110)
      CHARACTER*8 HEAD1,FNAME(5),FUNCT(4),TITLE(10,3),PTITLE(4,15),
     1            GDATE,GTIME,FTYPE,GNAME,EQUPCT(50)
      EQUIVALENCE (HEAD1(1),FNAME),(HEAD1(6),GDATE),(HEAD1(7),GTIME),
     1            (HEAD1(8),FUNCT),(HEAD1(12),FTYPE),(HEAD1(13),GNAME),
     2            (HEAD1(14),TITLE),(HEAD1(44),PTITLE,EQUPCT)
C
      COMMON /EXEC2/ HEAD2(60)
      INTEGER*2 HEAD2,MAXZON,NUMPUR,NERR,MAXNEX,MAXNI,NLINK,
     1          LTPEN,LINKGP(3),SCNLIN,ASSGRP,NODATA,NUMPRO,
     2          TABLES,LODPCT(10),NUMITR,LODPUR,NMSELK,NMWEAV,
     3          MOD30T(2),MAXLIN,MAXVEH,TRNPUR
      INTEGER*4 CAPAC,VOLUME,MINX,MAXX,MINY,MAXY,NMTURN,DISCNV,TLINK4
      integer*2 TESSUM,WSA,LCOST,LUSER,LARGXY,FDOT,EQUILM,LARGIM,       <test_CB>
     1          SCAG,LARGND                                             <test_CB>
      EQUIVALENCE (HEAD2( 3),TLINK4)
      EQUIVALENCE (HEAD2(1),MAXZON),(HEAD2(2),NUMPUR),
     1            (HEAD2(3),NERR),(HEAD2(4),MAXNEX),(HEAD2(5),MAXNI),
     2            (HEAD2(6),NLINK),(HEAD2(7),LCOST),(HEAD2(8),LUSER),
     3            (HEAD2(9),LTPEN),(HEAD2(10),LINKGP),
     4            (HEAD2(13),CAPAC),(HEAD2(15),VOLUME),
     5            (HEAD2(17),SCNLIN),(HEAD2(18),ASSGRP),
     6            (HEAD2(19),NODATA),(HEAD2(20),NUMPRO),
     7            (HEAD2(21),TESSUM),(HEAD2(22),TABLES),
     8            (HEAD2(23),LODPCT),(HEAD2(33),NUMITR),
     9            (HEAD2(34),LODPUR),(HEAD2(35),NMSELK),
     X            (HEAD2(36),NMTURN),(HEAD2(38),NMWEAV),
     1            (HEAD2(39),MINX),(HEAD2(41),MAXX),
     2            (HEAD2(43),MINY),(HEAD2(45),MAXY),
     3            (HEAD2(47),WSA),(HEAD2(48),MOD30T),
     4            (HEAD2(50),MAXLIN),(HEAD2(51),MAXVEH),
     5            (HEAD2(52),LARGXY),(HEAD2(53),FDOT),
     6            (HEAD2(54),EQUILM),(HEAD2(55),DISCNV),
     7            (HEAD2(56),LARGIM),(HEAD2(57),SCAG),
     8            (HEAD2(58),TRNPUR),(HEAD2(59),LARGND)
      COMMON /BIGLNK/ NLINK4
      INTEGER*4 NLINK4
C
C***********************************************************************