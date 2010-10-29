C  LAND VARIABLE DECLARATIONS AND COMMON BLOCK
C
C     TYPE      VARIBLE          DESCRIPTION
C........X.........X.........X.........X.........X.........X.........X.........X
C
      INTEGER  	ILOAD
C                      	LOADCASE: 1,2,3 for Maneuver, Landing, Bump (inclusive)
      REAL     	CLAN
C                       Weight fraction of WGTO at Landing
      REAL     	CBUM
C                       Weight fraction of WGTO at Bump
      REAL     	WFLAND
C                   	Fraction of Lift/Weight at Landing
      REAL	WFBUMP    
C     			Fraction of Lift/Weight at BUMP
      REAL 	VSINK
C 			Constant Velocity of SINK during impact, ft/sec
      REAL	STROKE
C			STROKE length during impact, ft
      REAL      GFRL
C         		Fraction of Ground FoRce at Landing, Nose Gear
      REAL 	GFRB
C			Fraction of Ground FoRce at Bump, Nose Gear 
      REAL      SLFMB
C                       Design Static Load Factor at Bump
      CHARACTER*4 BMSTR(60) 
C                       Type of max. moment at station: MAN,LAN,BUM,NONE
C
C........X.........X.........X.........X.........X.........X.........X.........X
C	
C																
C  COMMON BLOCK FOR LANDING LOADS
C
      COMMON/LAND/ ILOAD , CLAN  , CBUM  , WFLAND, WFBUMP, 
     1             VSINK , STROKE, GFRL  , GFRB  , SLFMB,
     2             BMSTR
