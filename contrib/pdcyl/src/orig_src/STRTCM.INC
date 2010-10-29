C=======================================================================
C
C  Commmon for global interaction with Structures module.
C
C    PS        Plasticity factor
C    TMGW      Minimum gage thickness for wing (in)
C    EFFW      Buckling efficiency of the web
C    EFFC      Buckling efficiency of the covers
C    ESW       Young's modulus for wing material (psi)
C    FCSW      Ultimate compressive strength of wing (psi)
C    DSW       Density of the wing material (lb/in^3)
C    KDEW      Knock down factor for Young's modulus of wing
C    KDFW      Knock down factor for ultimate strength of wing
C    CS1       Position of structural wing box from leading edge (percent root chord)
C    CS2       Position of structural wing box from trailing edge (percent root chord)
C    CLAQR     Ratio of body lift to wing lift
C    FTST      Tensile strength on top of fuselage (psi)
C    FTSB      Tensile strength on bottom of fuselage (psi)
C    FCST      Compressive strength of fuselage top (psi)
C    FCSB      Compressive strength of fuselage bottom (psi)
C    EST       Young's modulus for the shells of fuselage top (psi)
C    ESB       Young's modulus for the shells of fuselage bottom (psi)
C    EFT       Young's modulus for the frames of fuselage top (psi)
C    EFB       Young's modulus for the frames of fuselage bottom (psi)
C    DST       Density of shell material on fuselage top (lb/in^3)
C    DSB       Density of shell material on fuselage bottom (lb/in^3)
C    DFT       Density of frame material on fuselage top (lb/in^3)
C    DFB       Density of frame material on fuselage bottom (lb/in^3)
C    TMGT      Minimum gage thickness for fuselage top (in)
C    TMGB      Minimum gage thickness for fuselage bottom (in)
C    KDE       Knock down factor for Young's modulus of fuselage
C    KDF       Knock down factor for ultimate strength of fuselage
C    AXAC      Axial acceleration (g's)
C    CMAN      Weight fraction at the manuever condition
C    PGT       Gage pressure on the fuselage top (psi)
C    PGB       Gage pressure on the fuselage bottom (psi)
C    WFBUMP    Lift to weight fraction at bump condition (at gross weight)
C    WFLAND    Lift to weight fraction at landing condition 
C    VSINK     Design sink velocity at landing (ft/sec)
C    STROKE    Stroke of landing gear (ft)
C    CLRG1     Nose landing gear length (fraction of total fuselage length)
C    CLRG2     Main landing gear length (fraction of total fuselage length)
C    WFGR1     Nose landing gear weight fraction
C    WFGR2     Main landing gear weight fraction
C    GFRL      Ratio of impact force taken by nose gear to main gear
C    CLRGW1    Wing gear position as a fraction of structural semispan (set 1)
C    CLRGW2    Wing gear position as a fraction of structural semispan (set 2)
C    WTFF      Weight fraction of fuel (at maximum weight)
C    CBUM      Weight fraction at bump
C    CLAN      Weight fraction at landing
C    WGNO      Wing non-optimal factor for structural weight
C    SLFMB     Static load factor for bumps
C    WCW       Fuselage non-optimal factor for structural weight      


C   Output variables for beam method to be added to Structures Module
C    
C    WEBSB     Smeared thickness of ? (in)
C    STTO      Smeared thickness of ? (in) 
C    TBCOV     Smeared thickness of covers (in)
C    TORK      Total wing torque  on the carrythrough structure (ft lbs)
C    LBOX      Length of wing box (ft)
C    WBOX      Width of wing box (ft)
C    TBOX      Thickness of wing box (ft)
C    WSHEAR    Shear structure weight (half weight, since upper and lower) (lbs)
C    WBEND     Bending structure weight (half weight, since upper and lower) (lbs)
C    WSWING    Total bending and shear structure weight (lbs)
C    WSHBOX    Wing shear carrythrough structure weight (lbs)
C    WBDBOX    Wing bending carrythrough structure weight (lbs)
C    WTOBOX    Wing torsion carrythrough structure weight (lbs)
C    WWBOX     Wing total carrythrough structure weight (lbs)
C    WWING     Wing weight total (lbs)
C    DELTIP    Maximum deflection of tip (ft)
C    CNTRLA    Control surface area (ft^2)
C    CTST      Structural planform area (ft^2)
C    WSFUS     Fuselage shell weight (lbs)
C    WSFUSS    Fuselage shell unit weight (lbs/ft^2)
C    WSFFR     Fuselage frames weight (lbs)
C    WSFFRS    Fuselage frames unit weight (lbs/ft^2)
C    WNOP      Non-optimum weight (lbs)
C    WNOPS     Non-optimum unit weight (lbs/ft^2)
C    WSEC      Fuselage ? weight (lbs)
C    WSECS     Fuselage ? unit weight (lbs/ft^2)
C    WBDWOP    Fuselage total weight (lbs)
C    WBDS      Fuselage total unit weight (lbs/ft^2)
C    WVLPEN    Fuselage volume penalty weight (lbs)
C    WBODY     Fuselage total weight (including volume penalty) (lbs)
C    
C    
C    
C    Variables needed from within DATATR to structures module
C    --------------------------------------------------------
C    
C    VLNOSE    Fuselage nose volume (ft^2)
C    VLTAIL    Fuselage tail volume (ft^2)
C    VOLB      Fuselage total volume (ft^3)
C    BODL      Fuselage length (ft)
C    BDMAX     Fuselage maximum body width (ft)
C    SWETB     Wetted area of fuselage (ft^2)
C    TR        Taper ratio
C    AR        Aspect ratio
C    SPAN      Span (ft)
C    CROOT     Root chord (ft)
C    XWLOC     Location of lifting surface (ft)
C    SWPLE     Leading edge sweep
C    SWWET     Wetted area of wing (ft^2)
C    SWING     Wing planform area (ft^2)
C    SHT       Horizontal tail planform area (ft^2)
C    SVT       Vertical tail planform area (ft^2)
C    FRN       Fineness ratio of nose
C    FRAB      Fineness ratio of tail
C    TCROOT    Root chord t/c
C    TCTIP     Tip chord t/c
C    EN        Number of engines
C    ENWING    Number of engines located on wing
C    NPODS     Number of wing pods
C    XPOD      Location array of wing pods
C    PODLNG    Wing pod length (ft)
C    
C    TWISTW       
C    THTNOS      
C    THTAB      
C    SEXT      AREA OF LEADING EDGE EXTENSION (STRAKE)
C    XEXT      X LOCATION OF STRAKE CENTROID.
C    SAFT      AREA OF THE TRAILING EDGE EXTENSION
C    XAFT      X LOCATION OF TRAILING EDGE EXTENSION CENTROID.
C    SWSTR     Wetted Area of the Strake
C    SWAFEX    Wetted Area of the Aft Extension
C    
C    DESLF     Design load factor
C    WEMPTY    AIRCRAFT EMPTY WEIGHT
C    WGTO      Gross weight (lbs)
C    WHT       Horizontal tail weight (lbs)
C    WVT       Vertical tail weight (lbs)
C    WCAND     WEIGHT OF CANARDWVT      Vertical tail weight (lbs)
C    ULTLF     Ultimate load factor
C    WFUEL     Fuel weight (lbs)
C    WENG      ENGINE WEIGHT (PER ENGINE)
C    WNA       Nacelle weight (lbs)
C    
C
      REAL
     > PSX, TMGWX, EFFWX, EFFCX, ESWX, FCSWX, DSWX, 
     > KDEWX, KDFWX, CS1X, CS2X, CLAQRX, FTSTX, FTSBX, FCSTX, FCSBX,  
     > ESTX, ESBX, EFTX, EFBX, DSTX, DSBX, DFTX, DFBX, TMGTX, TMGBX,  
     > KDEX, KDFX, AXACX, CMANX, PGTX, PGBX, WFBUMPX, WFLANDX, VSINKX, 
     > STROKEX, CLRG1X, CLRG2X, WFGR1X, WFGR2X, GFRLX,CLRGW1X, CLRGW2X, 
     > WTFFX, CBUMX, CLANX, WGNOX, SLFMBX, WCWX, WEBSBX, STTOX, 
     > TBCOVX, TORKX, LBOXX, WBOXX, TBOXX, WSHEARX, WBENDX, WSWINGX,  
     > WSHBOXX, WBDBOXX, WTOBOXX, WWBOXX, WWING, DELTIPX, CNTRLAX,  
     > CTSTX, WSFUSX, WSFUSSX, WSFFRX, WSFFRSX, WNOPX, WNOPSX, WSECX, 
     > WSECSX, WBDWOPX, WBDSX, WVLPENX, WBODY, VLNOSE, VLTAIL, XCG,
     > VOLB, BODL, BDMAX, SWETB, TRWG, ARWG, SPANWG, ROOTWG, XWLOC1,
     > SWPLE(4), SWETWG, SWG, SHT, SVT, FRN, FRAB, TCRWG, TCTWG,
     > EN, ENWING, XREF(10), YREF(10), PODL, TWISTW(3), THTNOS, THTAB,
     > SEXT, XEXT, SLEXT, SAFT, XAFT, SWSTR, SWAFEX, DESLF, WEMPTY,
     > WGTO, WHT, WCAND, WVT, ULTLF, WFUELX, WENG, WNA, XWLOC2

      INTEGER 
     > NPODS

      COMMON /STRTCM/ PSX, TMGWX, EFFWX, EFFCX, ESWX, FCSWX, DSWX, 
     1 KDEWX, KDFWX, CS1X, CS2X, CLAQRX, FTSTX, FTSBX, FCSTX, FCSBX,  
     2 ESTX, ESBX, EFTX, EFBX, DSTX, DSBX, DFTX, DFBX, TMGTX, TMGBX,  
     3 KDEX, KDFX, AXACX, CMANX, PGTX, PGBX, WFBUMPX, WFLANDX, VSINKX, 
     4 STROKEX, CLRG1X, CLRG2X, WFGR1X, WFGR2X, GFRLX,CLRGW1X, CLRGW2X, 
     5 WTFFX, CBUMX, CLANX, WGNOX, SLFMBX, WCWX, WEBSBX, STTOX, 
     5 TBCOVX, TORKX, LBOXX, WBOXX, TBOXX, WSHEARX, WBENDX, WSWINGX,  
     6 WSHBOXX, WBDBOXX, WTOBOXX, WWBOXX, WWING, DELTIPX, CNTRLAX,  
     7 CTSTX, WSFUSX, WSFUSSX, WSFFRX, WSFFRSX, WNOPX, WNOPSX, WSECX, 
     8 WSECSX, WBDWOPX, WBDSX, WVLPENX, WBODY, VLNOSE, VLTAIL, XCG,
     9 VOLB, BODL, BDMAX, SWETB, TRWG, ARWG, SPANWG, ROOTWG, XWLOC1,
     1 SWPLE, SWETWG, SWG, SHT, SVT, FRN, FRAB, TCRWG, TCTWG,
     2 EN, ENWING, XREF, YREF, PODL, TWISTW, THTNOS, THTAB,
     3 SEXT, XEXT, SLEXT, SAFT, XAFT, SWSTR, SWAFEX, DESLF, WEMPTY,
     4 WGTO, WHT, WCAND, WVT, ULTLF, WFUELX, WENG, WNA, XWLOC2, NPODS
C=======================================================================
C
