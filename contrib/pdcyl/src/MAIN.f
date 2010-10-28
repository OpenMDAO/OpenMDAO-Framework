      PROGRAM MAIN  
C#######################################################################
C
C PDCYL
C
C This structural weight estimation code was pulled from ACSYNT and then
C wrapped for use in ModelCenter
C
C Work done by: Bill Fredericks starting in Feb 2007
C
C#######################################################################
C2345&*89 123456789 123456789 123456789 123456789 123456789 123456789 12

C############################
C Input Variable Declerations

C     Aircraft Character String
      CHARACTER*50 AIRCRAFT  
C     3 - Print out more in output file
      INTEGER  ICALC   

C Wing
C Geometry
C Wing sweep referenced to the leading edge  (deg)
      REAL     WSWEEP  
C Wing Aspect Ratio
      REAL     WAR   
C Wing taper ratio
      REAL     WTAPER  
C Wing thickness-to-cord at root
      REAL     WTCROOT
C Wing thickness-to-cord at tip
      REAL     WTCTIP  
C WAREA        Wing planform area                         (ft^2)
      REAL     WAREA      
C Material Properties
C Plasticity factor
      REAL     PS  
C Min. gage thickness for the wing           (inches)
      REAL     TMGW 
C Buckling efficiency of the web
      REAL     EFFW 
C Buckling efficiency of the covers
      REAL     EFFC 
C Young’s Modulus for wing material          (psi)
      REAL     ESW   
C Ult. compressive strength of wing          (psi)
      REAL     FCSW    
C DSW         Density of the wing material               (lb/in^3)
      REAL     DSW
C KDEW        Knock-down factor for Young's Modulus
      REAL     KDEW 
C KDFW        Knock-down factor for Ultimate strength
      REAL     KDFW  
C Geometric Parameters
C     ISTAMA      ! 1 - Position of wing is unknown
C                 ! 2 - position is known
      INTEGER  ISTAMA  
                      
C     CS1         ! Position of structural wing box from leading edge as percent of root chord
      REAL     CS1   
C     CS2         ! Position of structural wing box from trailing edge as percent of root chord
      REAL     CS2 
C     UWWG        ! Wing Weight / Wing Area of baseline aircraft      (lb/ft^2)
      REAL     UWWG   
c     REAL     XWLOC1      ! Location of wing as a percentage of body length
C Structural Concept
C     CLAQR       ! Ratio of body lift to wing lift
      REAL     CLAQR  
C     IFUEL       ! 1 - No fuel is stored in the wing
C                 ! 2 - Fuel is stored in the wing
      INTEGER  IFUEL    
C     CWMAN       ! Design maneuver load factor
      REAL     CWMAN   
C     CF          ! Shanley's const. for frame bending
      REAL     CF  


C Tails
C     ITAIL       ! 1 - Control surfaces mounted on tail
C                 ! 2 - Control surfaces mounted on wing
      INTEGER  ITAIL 
C     UWT         ! (Htail Weight + Vtail Weight) / Htail Area of baseline aircraft      (lb/ft^2)
      REAL     UWT 
C     CLRT        ! Location of tail as a percentage of body length
      REAL     CLRT 
C     HAREA       ! Htail planform area                        (ft^2)
      REAL     HAREA 


C Fuselage
C Geometry
c     REAL     FRN         ! Fineness ratio of the nose section         (length/diameter)
c     REAL     FRAB        ! Fineness ratio of the after-body section   (length/diameter)
c     REAL     BODL        ! Length of the fuselage                     (ft)
c     REAL     BDMAX       ! Maximum diameter of fuselage               (ft)
C     VBOD        ! Fuselage total volume                      (ft^3)
      REAL     VBOD 
C     VOLNOSE     ! Nose volume                                (ft^3)
      REAL     VOLNOSE 
C     VOLTAIL     ! Tail volume                                (ft^3)
      REAL     VOLTAIL  
C Structural Concept
C     CKF         ! Frame stiffness coefficient
      REAL     CKF 
C     EC          ! Power in approximation equation for buckling stability
      REAL     EC  
C     KGC         ! Buckling coefficient for component general buckling of stiffener web panel
      REAL     KGC  
C     KGW         ! Buckling coefficient for component local buckling of web panel
      REAL     KGW  
c     INTEGER  KCONT(12)   ! Structural Geometry Concept Top/Bottom
c     INTEGER  KCONB(12)   ! 2 - Simply stiffened shell, frames, sized for minimum weight in buckling
                           ! 3 - Z-stiffened shell, frames, best buckling
                           ! 4 - Z-stiffened shell, frames, buckling-minimum gage compromise
                           ! 5 - Z-stiffened shell, frames, buckling-pressure compromise
                           ! 6 - Truss-core sandwich, frames, best buckling
                           ! 8 - Truss-core sandwich, no frames, best buckling
                           ! 9 - Truss-core sandwich, no frames, buckling-min. gage-pressure compromise
C Material Properties
c     REAL     FTST(12)    ! Tensile Strength on top/bottom             (psi)
c     REAL     FTSB(12)
c     REAL     FCST(12)    ! Compressive Strength top/bottom            (psi)
c     REAL     FCSB(12)
c     REAL     EST(12)     ! Young's Modulus for the shells top/bottom  (psi)
c     REAL     ESB(12)
c     REAL     EFT(12)     ! Young's Modulus for the frames top/bottom  (psi)
c     REAL     EFB(12)
c     REAL     DST(12)     ! Density of shell material on top/bottom    (lb/in^3)
c     REAL     DSB(12)
c     REAL     DFT(12)     ! Density of frame material on top/bottom    (lb/in^3)
c     REAL     DFB(12)
c     REAL     TMGT(12)    ! Minimum gage thickness top/bottom          (in)
c     REAL     TMGB(12)
c     REAL     KDE         ! Knock-down factor for modulus
c     REAL     KDF         ! Knock-down factor for strength
C Geometric Parameters
C     CLBR1       ! Fuselage break point as a fraction of total fuselage length
      REAL     CLBR1  
C     ICYL        ! 1 - modeled with a mid-body cylinder
C                 ! 0 - use two power-law bodies back to back
      INTEGER  ICYL 


C Engines
C     NENG        ! Total number of engines
      INTEGER  NENG  
C     NENGWING    ! Number of engines on wing
      INTEGER  NENGWING 
C     WFP         ! (Engine Weight * NENG) / WGTO
      REAL     WFP  
C     CLRW1       ! Location of first engine pair.  Input 0 for centerline engine.  For wing mounted input as a fraction of semispan,
C                 ! measured from body centerline.
      REAL     CLRW1 
C     CLRW2       ! Location of second engine pair.  For wing mounted input as a fraction of semispan, measured from body centerline.
      REAL     CLRW2 
C     CLRW3       ! Location of third engine pair.  For wing mounted input as a fraction of semispan, measured from body centerline.
      REAL     CLRW3  


C Loads
c     REAL     DESLF       ! Design load factor
c     REAL     ULTLF       ! Ultimate load factor (usually 1.5*DESLF)
C     AXAC        ! Axial acceleration                         (g's)
      REAL     AXAC 
C     CMAN        ! Weight fraction at maneuver
      REAL     CMAN  
c     INTEGER  ILOAD       ! 1 - Analyze maneuver only
                           ! 2 - Analyze maneuver and landing only
                           ! 3 - Analyze bump, landing and maneuver
c     REAL     PGT(12)     ! Fuselage gage pressure on top/bottom       (psi)
c     REAL     PGB(12)
c     REAL     WFBUMP      ! Weight fraction at bump
c     REAL     WFLAND      ! Weight fraction at landing


C Landing Gear
c     REAL     VSINK       ! Design sink velocity at landing            (ft/sec)
c     REAL     STROKE      ! Stroke of landing gear                     (ft)
C     CLRG1       ! Length fraction of nose landing gear measured as a fraction of total fuselage length
      REAL     CLRG1   
C     CLRG2       ! Length fraction of main landing gear measured as a fraction of total fuselage length
      REAL     CLRG2  
C     WFGR1       ! Weight fraction of nose landing gear
      REAL     WFGR1   
C     WFGR2       ! Weight fraction of main landing gear
      REAL     WFGR2  
C     IGEAR       ! 1 - Main landing gear located on fuselage
C                 ! 2 - Main landing gear located on wing
      INTEGER  IGEAR  
c     REAL     GFRL        ! Ratio of force taken by nose landing gear to force taken by main gear at landing
C     CLRGW1      ! Position of wing gear as a fraction of structural semispan
      REAL     CLRGW1 
C     CLRGW2      ! Position of second pair wing gear as a fraction of structural semispan
C                 ! If only 1 wing gear, set CLBR2 = 0.0
      REAL     CLRGW2 


C Weights
c     REAL     WGTO        ! Gross takeoff weight
C     WTFF        ! Weight fraction of fuel
      REAL     WTFF  
C******************   these 2 varables are duplicated, don't know why    *****************
c     REAL     CBUM        ! Weight fraction at bump
c     REAL     CLAN        ! Weight fraction at landing


C Factors
C     ISCHRENK    ! 1 - use Schrenk load distribution on wing
C                 ! Else - use trapezoidal distribution
      INTEGER  ISCHRENK  
C     ICOMND      ! 1 - print gross shell dimensions envelope
C                 ! 2 - print detailed shell geometry
      INTEGER  ICOMND  
C     WGNO        ! Nonoptimal factor for wing (including the secondary structure)
      REAL     WGNO 
c     REAL     SLFMB       ! Static load factor for bumps
C     WMIS        ! Volume component of secondary structure
      REAL     WMIS  
C     WSUR        ! Surface area component of secondary structure
      REAL     WSUR  
C     WCW         ! Factor in weight equation for nonoptimal weights
      REAL     WCW  
C     WCA         ! Factor in weight equation multiplying surface areas for nonoptimal weights
      REAL     WCA  
C     NWING       ! Number of wing segments for analysis
      INTEGER  NWING  


C###############################
C Internal Variable Declerations

      REAL     ENPD, LBOX, LNOSE, LAB
      DIMENSION XFUS(51),DFUS(51)

      INCLUDE 'STRTCM.INC'
      INCLUDE 'VEHICON.INC'
      INCLUDE 'LAND.INC'
      
      COMMON/LOAD  /CG    , CMAN  , WTO   , IFUEL , WBOD  ,
     *              CL    , WWINGT, CGW   , ABOD  , CLW1  , 
     *              CLW2  , POW   , A0M   , CLAQR ,
     *              SPLAN , ATAIL , CLT   , SLFM  , DX    , 
     *              CLP1  , CLP2  , WPROP , WTAIL , B     , 
     *              AVBMM1(60), ABODL , CPB   , RG    , RGB, 
     *              RGW   , CGUST , A0G   , V     , VGU   ,
     *              AVBMG(60) , CX1   , CX2   , AVBML(60) , A0L, 
     *              DTL   , VELL  , Y10   , Y20   , C1    , 
     *              C2    , CLAND , TFL   , ZETA  ,
     *              ITAIL , CGP   , WGEAR , WGEAR1, WGEAR2,
     *              IGEAR , CGG   , CLG1  , CLG2  , CLG   ,
     *              ICYL  , WPROPW, WPROPF, ENPD  , ENWG  ,
     *              VBOD  , VOLNOSE,VOLTAIL,CLRP1 , CLRP2

      COMMON/WWIN1/WINGL, ASPR  , TAPER , ROOTCT, TIPC  , GAML  , 
     *    GAMT  , WGNO  , SPAN  , WTFF  , NWING , TRATW ,  
     1    DSW   , ESW   , FCSW  , FACS  , CLRW1 , CLRW2 ,
     2    CLRW3 , WBOX  , XCLWNG, NBOX1 , NBOX2 , TBOX  , LBOX  , 
     3    WWBOX , EFFC  , EFFW  , TBCOV , NBOX3 , RBMAX , UWWG  , 
     4    XCLWNGR,CS1   , CS2   , EC    , KGC   , KGW   , TMGW  , 
     5    CLPE1 , CLPE2 , CLPE3 , WENG1 , WENG2 , WENG3 , CPW   , 
     6    CLINT , WWGPD , WFUEL , CWMAN , D     ,ISCHRENK,TRATWR,
     7    TRATWT, CLRGW1,CLRGW2 , KDEW  , KDFW  , WFP   , UWT,
     8    XWGLOC

      COMMON/AX1/WSAV(60),STA(60),RB(60),VB(60),AS(60),AP(60),
     *  CPBD(60),CGB

      COMMON/AX/POW1,POW2,CL1,CL2,P111,P112,P121,P122,P211,P212,
     1 P221,P222,DENB,CL1A,CL1B,CL2A,CL2B
     
      COMMON      /CONCEPT/         BRAT,TRAT,ANGLR,MARK,KX

      COMMON /PDCYLCOM/   ICOMND,  CLRG1,  CLRG2,  WFGR1,  WFGR2,
     1    ISTAMA, FR    , PS    ,CF     ,
     2    CLRT  , WFPROP,  CKF  ,  WCW  ,  WCA  ,
     5    AXAC  , ART   ,CLBR1  ,  WMIS  , WSUR , PHI

      COMMON /OUTPUTS/  WEBSB, TORK, WSHEAR, WBEND, WSHBOX, WBDBOX,
     1  WTOBOX, DELTIP, CNTRLA, CTST, WNOP, WNOPS, WSEC

C########################################
      CALL PDCYLM()
      STOP
      END
