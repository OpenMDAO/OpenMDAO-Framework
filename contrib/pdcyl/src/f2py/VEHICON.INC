C
C VEHCON DECLARATIONS AND COMMON BLOCK
C     TYPE         VARIABLE       VAL  DESCRIPTION         
C.......................................................................
C
C      REAL         CKI1,CKI1B(12),CKI1T(12) 
C                                      Conductivity of 1st insulation 
C                                      layer
C      REAL         CKI2,CKI2B(12),CKI2T(12)
C                                      Conductivity of 2nd insulation 
C                                      layer
C      REAL         DF,DFB(12),DFT(12)
C                                      Density of Frame material
C      REAL         DS,DSB(12),DST(12)
C                                      Density of Shell material
C      REAL         DTANK,DTANKB(12),DTANKT(12)
C                                      Density of TANK
C      REAL         EF,EFB(12),EFT(12)
C                                      Young's modulus Frame material
C      REAL         ES,ESB(12),EST(12)
C                                      Young's modulus Shell material
C      REAL         FCS,FCSB(12),FCST(12)
C                                      Compressive yeild strength of shell
C                                      material
C      REAL         FTANK,FTANKB(12),FTANKT(12)
C                                      Tensile Ultimate Strength of tank
C      REAL         FTF,FTFB(12),FTFT(12)
C                                      Tensile Ultimate Strength of Frame
C      REAL         FTS,FTSB(12),FTST(12)
C                                      Tensile Ultimate Strength of Shell
C      INTEGER      JCON,JCONB(12),JCONT(12)
C                                      Indicator for different structural
C                                      concepts, Bottom 
C                                    JCON  Press.   Tank  Tension  Heat. 
C                                          Stab.          Ties?    Abs.
C                                                                  Material
C                                      1   Yes       Int.   Yes   Body&Tank
C                                      2   No    Non-Int.   No    Tank
C                                      3   No        Int.   Yes   Body&Tank Cool
C                                      4   No       None.   Yes   Body      Cool
C                                      5   No        Int.   Yes   Tank      Hot
C                                      6   No       None.   Yes   Nothing   Hot
C      INTEGER      KCON,KCONB(12),KCONT(12)   
C                                      Indicator for different structural
C                                      concepts, Bottom
C                                      2 = Simply Stiffened Shell, Frames,
C                                          sized for minimum weight in buckling
C                                      3 = Z-Stiffened Shell, Frames, Best 
C                                          buckling
C                                      4 = Z-Stiffened Shell, Frames, Buckling
C                                          minimum gage compromise
C                                      5 = Z-Stiffened Shell, Frames, Buckling
C                                          pressure compromise
C                                      6 = Truss-core sandwich, Frames, Best
C                                          buckling
C                                      8 = Truss-core sandwich, No Frames, Best
C                                          buckling
C                                      9 = Truss-core sandwich, No Frames,
C                                          Buckling minimum gage-pressure
C                                          compromise
C      REAL         PG,PGB(12),PGT(12)
C                                      Pressure in structure
C      REAL         RHO1,RHO1B(12),RHO1T(12)
C                                      Density of 1st (inner) layer of
C                                      insulation.
C      REAL         RHO2,RHO2B(12),RHO2T(12)
C                                      Density of 2nd (outer) layer of
C                                      insulation
C      REAL         TBOD, TBODB(12),TBODT(12)
C                                      Inner body temperature
C      REAL         TEMP, TEMPB(12),TEMPT(12)
C                                      Maximum allowable temperature for
C                                      structures material
C      REAL         TI, TIB(12), TIT(12)
C                                      Temperature at interface
C      REAL         TMG, TMGB(12), TMGT(12)
C                                      Thickness Minimum Gage of shell material
C      REAL         TS, TSB(12), TST(12)
C                                      outer Surface temperature
C      REAL         TT, TTB(12), TTT(12)
C                                      Time when tank becomes dry
C      REAL         UWCP, UWCPB(12), UWCPT(12)
C                                      Unit Weight of Cover Panel
C      REAL         UWTPSD, UWTPST(12), UWTPSB(12)
C                                      Unit Weight of distrubuted TPS weight incr. 
C      REAL         CB,CBB(12),CBT(12)
C                                      Heat capacity of Body 
      INTEGER      ICON
C
      REAL         EFF
C
      REAL         SE
C
      REAL         CK
C
      REAL         CPR
C
      REAL         CTHICK
C  
      INTEGER      KONTPS
C                                      TPS indicator
C                                      = 0: Input CKI's vs Station
C                                      = 1: Input CKI as function of Temp.
C                                           using ATMPCK array
      INTEGER      NTMPCK
C                                      Number of CKI's vs. ATMPCK entries
C      REAL         ATMPCK(12)
C                                      Array of temps for CKI's
C      REAL         CPSOD,CPSODT(12),CPSODB(12)
C                                      Cover Panel Stand-off distance (ft.)
C      REAL         KDEST(12),KDESB(12)
C                                      Material Knock-Down Factor
C                                      Young's modulus Shell
C      REAL         KDEFT(12),KDEFB(12)
C                                      Material Knock-Down Factor
C                                      Young's modulus Frame
C      REAL         KDFTST(12),KDFTSB(12)
C                                      Material Knock-Down Factor
C                                      Tensile Ultimate Strength of 
C                                      Shell
C      REAL         KDFCST(12),KDFCSB(12)
C                                      Material Knock-Down Factor
C                                      Compressive yeild strength of shell
C                                      material
C      REAL         KDFTFT(12),KDFTFB(12)
C                                      Material Knock-Down Factor
C                                      Tensile Ultimate Strength of 
C                                      Frame
C
       REAL         KDE    
C                                      Material Knock-Down Factor
C                                      Young's modulus, fuselage 
C
       REAL         KDF
C                                      Material Knock-Down Factor
C                                      for compressive yield strength 
C                                      and ultimate tensile strength of 
C                                      fuselage 
C
C
C123456789C123456789C123456789C123456789C123456789C123456789C123456789
      COMMON /VEHICON/ CKI1   , CKI2   , DF     , DS     , DTANK  ,
     1  EF     , ES     , FCS    , FTANK  , FTF    , FTS    ,
     2  JCON   , KCON   , PG     , RHO1   , RHO2   , TBOD   , 
     3  TEMP   , TI     , TMG    , TS     , TT     , UWCP   ,   
     4  CBB(12)  ,CKI1B(12),CKI2B(12),DFB(12)   ,DSB(12)  ,DTANKB(12), 
     5  EFB(12)  ,ESB(12)  ,FCSB(12) ,FTANKB(12),FTFB(12) ,FTSB(12)  ,
     6  JCONB(12),KCONB(12),PGB(12)  ,RHO1B(12) ,RHO2B(12),TBODB(12) , 
     8  TEMPB(12),TIB(12)  ,TMGB(12) ,TSB(12)   ,TTB(12)  ,UWCPB(12) ,   
     9  CBT(12)  ,CKI1T(12),CKI2T(12),DFT(12)   ,DST(12)  ,DTANKT(12), 
     $  EFT(12)  ,EST(12)  ,FCST(12) ,FTANKT(12),FTFT(12) ,FTST(12)  ,
     1  JCONT(12),KCONT(12),PGT(12)  ,RHO1T(12) ,RHO2T(12),TBODT(12) , 
     2  TEMPT(12),TIT(12)  ,TMGT(12) ,TST(12)   ,TTT(12)  ,UWCPT(12) ,
     3  ICON   , EFF    , SE     , CK     , CPR    , CTHICK , 
     4  CB     , KONTPS , NTMPCK ,ATMPCK(12) ,CPSODT(12) ,CPSODB(12) , 
     6  CPSOD  ,UWTPST(12)  , UWTPSB(12) , UWTPSD , KDE    , KDF
