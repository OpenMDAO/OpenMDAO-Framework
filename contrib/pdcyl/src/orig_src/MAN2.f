      SUBROUTINE MAN2

C  DATE----MODIFIED BY--------REASON
C  --------------------------------------------------------
C  8/94    M. C. CHAMBERS     MOMENT DUE TO LANDING GEAR 
C                             CORRECTION
C  9/94    M. C. CHAMBERS     ADDITION OF LANDING AND BUMP 
C                             LOADING
C
C
C  AE  WB MANEUVER LOADS  M.D.ARDEMA  11/16/66

      DIMENSION BMMAN(60),BMLAN(60),BMBUM(60),BMMAX(60)

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

      COMMON/AX/POW1,POW2,CL1,CL2,P111,P112,P121,P122,P211,P212,
     1P221,P222,DENB,CL1A,CL1B,CL2A,CL2B

      COMMON/AX1/WSAV(60),STA(60),RB(60),VB(60),AS(60),AP(60),
     *  CPBD(60),CGB

      COMMON      /CONCEPT/         BRAT,TRAT,ANGLR,MARK,KX 

      INCLUDE 'LAND.INC'

      REAL LBOX, MGEAR, KDEW, KDFW
      real kgc, kgw

      PI = 3.1415
      CLINTP = CLINT+LBOX

C     New statements for LANding loads
      GRAV = 32.2
      CGOLD = CG

C     Loop over all loading cases: MANeuver, LANding, and BUMP
      DO 141 J = 1, ILOAD
        IF (J .EQ. 1) THEN
           WM = CMAN * WTO
        ENDIF

        IF (J .EQ. 2) THEN
           WM = CLAN * WTO
           GFRN = GFRL
	   GFRM = 1.0 - GFRN
	   SLFM = 1.0 + 0.5947 * VSINK * VSINK / (GRAV * STROKE)
	   FGEAR = 0.5947 *WM*VSINK*VSINK/(GRAV*STROKE)+WM*
     &             (1.-WFLAND)
	   CMAN = CLAN
        ENDIF

        IF (J .EQ. 3) THEN 
C          REPLACE GFRB INPUT WITH GEAR WEIGHT FRACTIONS OF EACH GEAR
           GFRB = WGEAR1/WGEAR
	   WM = CBUM * WTO
           SLFM = SLFMB
	   GFRN = GFRB
           GFRM = 1.0 - GFRN
	   CMAN = CBUM
           FGEAR = WM * (SLFM - WFBUMP)
        ENDIF

C     End new LANding load statements
      IF (IFUEL .EQ. 2) GO TO 110
      WBM = WTO * (CMAN - 1.0) + WBOD
      CGM = (CG + CGB*(CMAN-1.0))/CMAN
      WWM = WWINGT
      GO TO 120
  110 WWM = WTO * (CMAN-1.0) + WWINGT
      CGM = (CG + CGW*(CMAN-1.0))/CMAN
      WBM = WBOD
  120 CONTINUE
      CG = CGM
      A0MR=PI/180.*A0M

C     New ILOAD flag
      IF (J .EQ. 1) THEN
           DELTR=-(CLAQR*ABODL*(CG-CPB)+SPLAN*(CG-CPW))/ATAIL/(CG-CLT)*
     &           A0MR-A0MR
           CLAQW=WM /(CLAQR*ABODL*A0MR+SPLAN*A0MR+ATAIL*(A0MR+DELTR))
           CLAQB=CLAQR*CLAQW
           DELT=180./PI*DELTR
           BODL=CLAQB*ABODL*A0MR
           CLAQBD=PI/180.*CLAQB
           FL=CLAQW*SPLAN*A0MR
           CLAQWD=PI/180.*CLAQW
           TAILL=CLAQW*ATAIL*(DELTR+A0MR)
           STAMA=(CPW-CG)/CL*100.
           CKBCT = CLAQW * (CLAQR * ABODL + SPLAN + ATAIL)
           AM=SLFM*WM*(CGM-CLT)/CLAQW/(CLAQR*ABODL*(CPB-CLT)+SPLAN*
     &        (CPW-CLT))
           DELTMR = (SLFM * WM - CKBCT * AM)/ CLAQW/ATAIL

C     New LANding load statements
      ELSE
           MGEAR=GFRN*FGEAR*(CLG1-CG)+GFRM*FGEAR*(CLG2-CG)
           ACONA=(CLAQR*ABODL+SPLAN+ATAIL)*A0MR
           ACONB=(CLAQR*ABODL*(CPB-CG)+SPLAN*(CPW-CG)+ATAIL*(CLT-CG))*
     &           A0MR
           AM = A0MR
           IF (J .EQ. 2) THEN
             DELTR=-(MGEAR*ACONA+WFLAND*WM*ACONB)/(MGEAR*ATAIL+
     &           WFLAND*WM*ATAIL*(CLT-CG))
             CLAQW = (WFLAND * WM) / (ACONA + ATAIL * DELTR)
             DELTMR = DELTR
           ELSE
             DELTR=-(MGEAR*ACONA+WFBUMP*WM*ACONB)/(MGEAR*ATAIL+
     &           WFBUMP*WM*ATAIL*(CLT-CG))
             CLAQW = (WFBUMP * WM) / (ACONA + ATAIL * DELTR)
             DELTMR = DELTR
           ENDIF
         CLAQB = CLAQR * CLAQW
         DELT = 180.0 / PI * DELTR
         BODL = CLAQB * ABODL * A0MR
         FL = CLAQW * SPLAN * A0MR
         TAILL = CLAQW * ATAIL * (A0MR + DELTR)
         STAMA = (CPW - CG) / CL * 100.0
      ENDIF

C     End new LANding load statements
      AMD = 180. / PI * AM
      DELTMD = 180. / PI*DELTMR
      FLB = CLAQB * ABODL * AM
      FLW = CLAQW * SPLAN * AM
      FLT = CLAQW * ATAIL * (AM + DELTMR)

      IF(IGEAR.EQ.1) GO TO 360
      XAG = (2.*LBOX**2.-3.*(CLG2-CLINT)*LBOX)/(3.*LBOX-6.
     $*(CLG2-CLINT))

C     Added new LANding load terms for GEAR2
      IF (J .EQ. 1) THEN 
           CONAG = (-SLFM*WGEAR2)*(3.*(CLG2-CLINT)-3.*XAG-2.
     $     *(LBOX-XAG))/(XAG*LBOX)
           CONBG = (-SLFM*WGEAR2)*(3.*(CLG2-CLINT)-XAG)
     $     /((LBOX-XAG)*LBOX)
      ELSE
           CONAG = (-SLFM*WGEAR2+GFRM*FGEAR)*(3.*(CLG2-CLINT)-3.*XAG-2.
     $     *(LBOX-XAG))/(XAG*LBOX)
           CONBG = (-SLFM*WGEAR2+GFRM*FGEAR)*(3.*(CLG2-CLINT)-XAG)
     $     /((LBOX-XAG)*LBOX)
      ENDIF

 360  CONTINUE
      IF(ITAIL.EQ.1) GO TO 361
      XAT = (2.*LBOX**2.-3.*(CLT-CLINT)*LBOX)/(3.*LBOX-6.
     $*(CLT-CLINT))
      CONAT = ((FLT-SLFM*WTAIL)*(3.*(CLT-CLINT)-(3.*XAT)
     $-(2.*(LBOX-XAT))))/(XAT*LBOX)
      CONBT = ((FLT-SLFM*WTAIL)*(3.*(CLT-CLINT)-XAT))
     $/((LBOX-XAT)*LBOX)

 361  CONTINUE     
      XAW = (2.*LBOX**2.-3.*(CPW-CLINT)*LBOX)/(3.*LBOX-6.
     $*(CPW-CLINT))   
      XBW = LBOX-XAW
      CONAW = ((FLW-SLFM*WWM)*(3.*(CPW-CLINT)-(3.*XAW)
     $-(2.*XBW)))/(XAW*LBOX)                
      CONBW = ((FLW-SLFM*WWM)*(3.*(CPW-CLINT)-XAW))/(XBW*LBOX)
      IF(CLRW1.LT.0.1) GO TO 366
      XAP = (2.*LBOX**2.-3.*(CGP-CLINT)*LBOX)/(3.*LBOX-6.
     $*(CGP-CLINT))
      XBP = LBOX-XAP
      CONAP = ((-SLFM*WPROP)*(3.*(CGP-CLINT)-(3.*XAP)
     $-(2.*XBP)))/(XAP*LBOX)
      CONBP = ((-SLFM*WPROP)*(3.*(CGP-CLINT)-XAP))/(XBP*LBOX)
 366  CONTINUE

      X = 0.0

C  Begin marching down craft
      DO 130 I=1, 60 
      X = X + DX
      XWING = X-CLINT
      IF(IGEAR.EQ.1) GO TO 362
      YGEAR = (CONAG+CONBG)/LBOX*XWING-CONAG
 362  CONTINUE
      IF(ITAIL.EQ.1) GO TO 363
      YTAIL = (CONAT+CONBT)/LBOX*XWING-CONAT
 363  CONTINUE
      YWING = (CONAW+CONBW)/LBOX*XWING-CONAW
      IF(CLRW1.LT.0.1) GO TO 364
      YPROP = (CONAP+CONBP)/LBOX*XWING-CONAP
 364  CONTINUE
      IF(X .GT. CL) GO TO 140

      IF (ICYL .EQ. 0) THEN
        IF (X .LE. CL1) THEN
          WBX=DENB*PI*B*B*(X**P121)/4./(CL1**(2.*POW1))/P121
          CGBX=X*P121/P122
          Y=B/2.*(X/CL1)**POW1
          FLBX=CLAQB*AM*B*(X**P111)/(CL1**POW1)/P111
          CPBX=X*P111/P112
          VXTEMP=WBX/DENB
          FLBXTEMP=FLBX
          CGBXTEMP=CGBX
          WBXTEMP=WBX
          CPBXTEMP=CPBX
        ELSE IF (X .GT. CL1) THEN
          WBX=WBM-DENB*PI*B*B*((CL-X)**P221)/4./(CL2**(2.*POW2))/P221
          CG2=CL-(CL-X)*P221/P222
          V2=PI*B*B*((CL-X)**P221)/4./(CL2**(2.*POW2))/P221
          V1=WBX/DENB
          CGBX=CGB*WBOD/WBX-CG2*V2*DENB/WBX
          Y=B/2.*((CL-X)/CL2)**POW2
          FLBX=FLB-CLAQB*AM*B*((CL-X)**P211)/(CL2**POW2)/P211
          CP2=CL-(CL-X)*P211/P212
          A2=B*((CL-X)**P211)/(CL2**POW2)/P211
          A1=FLBX/CLAQB/AM
          CPBX=(CPB*ABODL-CP2*A2)/A1
        END IF

      ELSE IF (ICYL .EQ. 1) THEN
        IF (X .LT. CL1A) THEN
          WBX=DENB*PI*B*B*(X**P121)/4./(CL1A**(2.*POW1))/P121
          CGBX=X*P121/P122
          Y=B/2.*(X/CL1A)**POW1
          FLBX=CLAQB*AM*B*(X**P111)/(CL1A**POW1)/P111
          CPBX=X*P111/P112
        ELSE IF (X .GE. CL1A .AND. X .LE. CL1B) THEN
          WBX=DENB*PI*B*B*(CL1A/P121+(X-CL1A))/4.
          CGBX=(CL1A*CL1A/P122+X*X/2.-CL1A*CL1A/2.)/
     >         (CL1A/P121+(X-CL1A))
          Y=B/2.
          FLBX=CLAQB*AM*B*(CL1A/P111+(X-CL1A))
          CPBX=(CL1A*CL1A/P112+X*X/2.-CL1A*CL1A/2.)/
     >         (CL1A/P111+X-CL1A)
        ELSE IF (X .GT. CL1B .AND. X .LE. CL) THEN
          WBX=DENB*PI*B*B*(CL1A/P121+(CL1B-CL1A)-((CL-X)/CL2B)**
     >        P221*CL2B/P221+CL2B/P221)/4.
          CGBX=(CL1A*CL1A/P122+(CL1B*CL1B-CL1A*CL1A)/2.+((CL-X)/
     >      CL2B)**P221*((CL-X)*CL2B/P222-CL*CL2B/
     >      P221)-CL2B*CL2B/P222+CL*CL2B/P221)/(CL1A/P121+
     >      (CL1B-CL1A)-((CL-X)/CL2B)**P221*CL2B/P221+CL2B/P221)
          Y=B/2.*((CL-X)/CL2B)**POW2
          FLBX=CLAQB*AM*B*((((CL-X)/CL2B)**P221)*CL2B/P221+
     >      CL1A/P111+(CL1B-CL1A))
          CPBX=(CL1A*CL1A/P112+(CL1B*CL1B-CL1A*CL1A)/2.+((CL-X)/
     >      CL2B)**P221*(CL2B*(CL-X)/P222-CL2B*CL/P221)-CL2B*CL2B/
     >      P222+CL*CL2B/P221)/(CL1A/P111+(CL1B-CL1A)-(((CL-X)/CL2B)
     >      **P221)*CL2B/P221+CL2B/P221)
        END IF
      END IF

      WTAILP = WTAIL
      WGEARP = WGEAR
      WGEAR1P = WGEAR1
      WGEAR2P = WGEAR2     
      IF(CLRW1.LT.0.1) GO TO 167
      IF(IGEAR.EQ.2) THEN
         IF(X.LT.CLINT) WSAV(I) = WBX
         GO TO 161
      ELSE 
           IF(X.LT.CLINT.AND.X.LE.CLG1) WSAV(I) = WBX
           IF(X.GT.CLG1.AND.X.LT.CLINT) WSAV(I) = WBX+WGEAR1
      ENDIF 
 161  CONTINUE

      IF(X.GT.CLINT.AND.X.LT.CLINTP) WSAV(I) = 0.
      IF(ITAIL.EQ.2) THEN 
             IF(IGEAR.EQ.2) THEN  
                   IF(X.GT.CLINTP) WSAV(I) = WBM-WBX
                   GO TO 165
             ELSE  
                   IF(X.GT.CLG2) WGEAR2 = 0.
                   IF(X.GT.CLINTP) WSAV(I) = WBM-WBX+WGEAR2
                   ENDIF
                   GO TO 165 
      ELSE
             IF(IGEAR.EQ.2) THEN
                   IF(X.GT.CLT) WTAIL = 0.
                   IF(X.GT.CLINTP) WSAV(I) = WBM-WBX+WTAIL
                   GO TO 165
             ELSE 
                  IF(X.GT.CLT) WTAIL = 0.
                  IF(X.GT.CLG2) WGEAR2 = 0.
                  IF(X.GT.CLINTP) WSAV(I) = WBM-WBX+WTAIL+WGEAR2 
                  ENDIF
      ENDIF
  165 CONTINUE
      GO TO 175

  167 IF(ITAIL.EQ.1) WTAIL = 0.
      IF(IGEAR.EQ.1) WGEAR = 0.
      TWT = WWM+WTAIL+WGEAR
      IF(X.LT.CLP1) THEN
          TWTX = 0.
          IF(X.GT.CLINT) TWTX = (TWT/LBOX)*(X-CLINT)
          IF(CLP1.LT.CLINT) TWTX = 0.
          IF(CLP1.GT.CLINTP) TWTX = TWT
          IF(IGEAR.EQ.1.AND.X.GT.CLG1) WGEAR = WGEAR1
          WSAV(I) = WBX+TWTX+WGEAR
          GO TO 175

      ELSE IF(X.GT.CLP1.AND.X.LT.CLP2) THEN
           WSAV(I) = 0.
           GO TO 175
           ELSE IF(X.GT.CLP2) THEN
                    IF(CLP2.GT.CLINTP) TWTX = TWT
                    IF(CLP2.LT.CLINT) TWTX = 0.
                    IF(X.LT.CLINTP) TWTX = (TWT/LBOX)*(X-CLINT)
                    IF(X.GT.CLINTP) TWTX = TWT

C                   ADDITION FOR TAIL ON BODY, M. ARDEMA, 7/95
                    IF (ITAIL .EQ. 1) WTAIL = WTAILP
                    IF(X.GT.CLT.OR.ITAIL.EQ.2) WTAIL = 0.
                    IF(IGEAR.EQ.1) WGEAR = WGEAR2
                    IF(X.GT.CLG2) WGEAR = 0.
                    WSAV(I) = WBM-WBX+TWT-TWTX+WTAIL+WGEAR
                    GO TO 175
                 ELSE
                    GO TO 175
      ENDIF
 175  CONTINUE
      WTAIL = WTAILP
      WGEAR = WGEARP
      WGEAR1 = WGEAR1P
      WGEAR2 = WGEAR2P 

C     Begin bending moment calculations
      BMBW = -SLFM * WBX * (X-CGBX) 
      BMG = 0.0

C     Added new LANding load terms for GEAR1
      IF (J .EQ. 1) THEN
           IF (X.GT.CLG1)BMG=(-SLFM*WGEAR1) * (X - CLG1)
      ELSE
           IF (X.GT.CLG1)BMG=(-SLFM*WGEAR1+GFRN*FGEAR) * (X - CLG1)
      ENDIF
      IF (X .GT. CLINT) GO TO 170
      BMP = 0.0 
      BMW = 0.0
      BMT = 0.0
      GO TO 190
 170  IF(X .GT. CLINTP) GO TO 180
      IF(IGEAR.EQ.1) GO TO 171
      BMG = (XWING**2.*(YGEAR)/2.-.5*(CONAG+YGEAR)
     $*XWING**2.*2./3.) + BMG
 171  CONTINUE
      IF(ITAIL.EQ.1) GO TO 172
      BMT = (XWING**2.*(YTAIL)/2.-.5*(CONAT+YTAIL)
     $*XWING**2.*2./3.)
 172  CONTINUE 
      BMW = (XWING**2.*(YWING)/2.-.5*(CONAW+YWING)
     $*XWING**2.*2./3.)
      IF(CLRW1.LT.0.1) GO TO 173
      BMP = (XWING**2.*(YPROP)/2.-.5*(CONAP+YPROP)
     $*XWING**2.*2./3.)
 173  CONTINUE
      GO TO 190
 180  CONTINUE 
      IF(IGEAR.EQ.1) GO TO 181
      IF (J .EQ. 1) THEN
           BMG = (-SLFM * WGEAR2) * (X - CLG2) + BMG
      ELSE
           BMG = (-SLFM * WGEAR2+GFRM*FGEAR) * (X - CLG2) + BMG
      ENDIF
 181  CONTINUE
      IF(ITAIL.EQ.1) GO TO 182
      BMT = (FLT-SLFM*WTAIL)*(X-CLT)      
 182  CONTINUE
      BMW = (FLW-SLFM*WWM)*(X-CPW)
      IF(CLRW1.LT.0.1) GO TO 183
      BMP = (-SLFM*WPROP)*(X-CGP)
 183  CONTINUE
 190  CONTINUE
      BMBL = FLBX * (X-CPBX)
      IF(IGEAR.EQ.2) GO TO 191

C     Added new LANding load terms to GEAR1 moment
      IF (J .EQ. 1) THEN
           IF(X.GT.CLG1) BMG = (-SLFM*WGEAR1)*(X-CLG1)
           IF(X.GT.CLG2) BMG = ((-SLFM*WGEAR1)*(X-CLG1))
     $     +((-SLFM*WGEAR2)*(X-CLG2))
      ELSE
           IF(X.GT.CLG1) BMG = (-SLFM*WGEAR1+GFRN*FGEAR)*(X-CLG1)
           IF(X.GT.CLG2) BMG = ((-SLFM*WGEAR1+GFRN*FGEAR)*(X-CLG1))
     $     +((-SLFM*WGEAR2+GFRM*FGEAR)*(X-CLG2))
      ENDIF
 191  CONTINUE
      IF(CLRW1.GT.0.1) GO TO 420
      IF(X.GT.CLP1) GO TO 410
      BMP=0.0
      GO TO 420
  410 IF(X.GT.CLP2) GO TO 430
      BMP=-SLFM*WPROP*(X-CLP1)/(CLP2-CLP1)*(X-CLP1)/2.
      GO TO 420
  430 CONTINUE
      BMP=-SLFM*WPROP*(X-(CLP2+CLP1)/2.)
  420 CONTINUE
      IF(ITAIL.EQ.2) GO TO 421
      BMT = 0.0
      IF (X .GT. CLT)BMT = (FLT-SLFM*WTAIL)*(X-CLT)
  421 CONTINUE
      BM = BMBW+BMBL+BMW+BMP+BMT+BMG

C     CREATE ARRAYS FOR Bending Moment FROM MANeuver, LANding, BUMp
C     FIND CASE OF LARGEST ABSOLUTE VALUE AT EACH SLICE DOWN CRAFT
C     OUTPUT MAX VALUE TO PDCYL, ALONG WITH CHARACTER STRING DEFINING
C     WHICH CASE CONTROLS
      IF (J .EQ. 1) BMMAN(I) = ABS(BM)
      IF (J .EQ. 2) BMLAN(I) = ABS(BM)
      IF (J .EQ. 3) BMBUM(I) = ABS(BM)
      BMMAX(I) = AMAX1(BMMAN(I), BMLAN(I), BMBUM(I))
      IF (BMMAX(I) .EQ. BMMAN(I)) BMSTR(I) = 'MAN'
      IF (BMMAX(I) .EQ. BMLAN(I)) BMSTR(I) = 'LAN'
      IF (BMMAX(I) .EQ. BMBUM(I)) BMSTR(I) = 'BUM'
      BMSTR(60) = 'NONE'
      AVBMM1(I) = BMMAX(I)

  130 CONTINUE
  140 CONTINUE
      CG=CGOLD
  141 CONTINUE
      RETURN

  310 FORMAT(1H )
  320 FORMAT(106H CRUISE          WEIGHT    ALPHA   DEFLEC     LIFTB     
     1LIFTW     LIFTT    CLAQW    CLAQB    STAMA      CGM            )
  322 FORMAT(106H LANDING         WEIGHT    ALPHA   DEFLEC     LIFTB     
     1LIFTW     LIFTT    CLAQW    CLAQB    STAMA      CGM            )
  323 FORMAT(106H BUMP            WEIGHT    ALPHA   DEFLEC     LIFTB     
     1LIFTW     LIFTT    CLAQW    CLAQB    STAMA      CGM            )
  330 FORMAT(15H PARAMETERS    ,F8.0,2(2X,F7.2),3(2X,F8.0),3(2X,F7.2),2X
     1,F7.1)
  340 FORMAT(105H MANEUVER       SLFM     ALPHA   DEFLEC    LIFTB    LIF
     1TW     LIFTT                                                 )
  342 FORMAT(105H LANDING        SLFM     ALPHA   DEFLEC    LIFTB    LIF
     1TW     LIFTT      FGEAR                                     )                                                      )
  343 FORMAT(105H BUMP           SLFM     ALPHA   DEFLEC    LIFTB   DEFLEC LIF
     1TW     LIFTT      FGEAR                                     )                                                      )
  350 FORMAT(16H PARAMETERS     ,F5.2,4X,2(F6.2,2X),3(F8.0,2X))
  351 FORMAT(16H PARAMETERS     ,F5.2,4X,2(F6.2,2X),3(F8.0,2X),F8.0)
  369 FORMAT(4X,1HX,7X,1HY,4X,11HBEND MOMENT,7X,7HWSAV(I),4X,
     14HBMBW,8X,4HBMBL,8X,3HBMW,9X,3HBMP,9X,3HBMT,
     29X,3HBMG,5X,10HMAX MOMENT)
  370 FORMAT(1X,2(F6.2,2X),E11.4,2X,F9.0,6(1X,F11.0),2X,E11.4)
      END
