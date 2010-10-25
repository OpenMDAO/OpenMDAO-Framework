      SUBROUTINE WING2(ICALC)


      REAL KGC, KGW, SELL, XELL, AELL1, AELL2, CPELL2,
     1 KDEW, KDFW, DELTIP

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

      COMMON /OUTPUTS/  WEBSB, TORK, WSHEAR, WBEND, WSHBOX, WBDBOX,
     1  WTOBOX, DELTIP, CNTRLA, CTST, WNOP, WNOPS, WSEC


      REAL LBOX,KWINGL,KTC,KASPR,KTAPER,KGAMS


      PI = 3.1415
      WBEND = 0.
C     CALCULATE THE WEIGHT OF ONE POD OF PROPULSION, M. CHAMBERS, 8/95
      WPOD = WPROP / ENPD

C     KNOCKDOWN FACTORS FOR YOUNG'S MODULUS AND COMPRESSIVE STRENGTH
C     M. CHAMBERS, 8/95
      FCSW = FCSW * KDFW
      ESW = ESW * KDEW

      IF (TAPER .LT. 0.01) TAPER = .01
      GAMLR = GAML*PI/180.
      TGAML = TAN(GAMLR)
      TGAMT = TGAML+2.*ROOTCT/SPAN*(TAPER-1.)
      GAMTR = ATAN(TGAMT)
      GAMT = 180./PI*GAMTR  
      TGAMS = 3.*TGAML/4. + TGAMT/4.
      GAMS = ATAN(TGAMS)

C     ******************** BOEING ANALYSIS *************************
      BSBW = .7072*(SPLAN**1.334)
      KWINGL = (.00286*(SLFM**.581)*WINGL)+
     *(.1624*(SLFM**.5588))
      KTC = (.0141/(TRATW**1.385))+.785
      KASPR = (.0588*(ASPR**1.148))+.28
      KTAPER = .47*TAPER+.833
      KGAMS = .9031*((COS(GAMS))**(-1.282))
      SBW = BSBW*KWINGL*KTC*KASPR*KTAPER*KGAMS

      GAMSD = GAMS*(180./PI)
      TGAMB = TGAML+2.*CS1/SPAN*(TIPC-ROOTCT)
      XCLWNG = XCLWNGR*CL
      CLBOX1 = XCLWNG+CS1*ROOTCT
      RBOX = 0.0

      XFT = 0.0
      DO 978 I=1,60
        XFT = XFT+DX
        IF (ICYL .EQ. 0) THEN
          IF(XFT.LE.CL1) RB(I) = (B/2.)*(XFT/CL1)**POW1    
          IF (XFT .GT. CL1 .AND. XFT .LE. CL) THEN 
            RB(I) = (B/2.)*((CL-XFT)/CL2)**POW2
          END IF
        ELSE IF (ICYL .EQ. 1) THEN
          IF(XFT.LT.CL1A) RB(I) = (B/2.)*(XFT/CL1A)**POW1    
          IF(XFT.GE.CL1A.AND.XFT.LE.CL1B) RB(I) = B/2.
          IF (XFT .GT. CL1B .AND. XFT .LE. CL) THEN
            RB(I)=(B/2.)*((CL-XFT)/CL2B)**POW2 
            END IF
        END IF        
  978  CONTINUE

       DO 3 I=1,60
        STA(I) = DX*I
    3  CONTINUE 
 
       DO 614 I=1,60
        IF(STA(I).LT.CLBOX1) GO TO 615
        RBOXP = RBOX
        RBOX = (STA(I)-CLBOX1)/TGAMB
        IF(RBOX.LT.RB(I)) GO TO 615
        DEN = RB(I-1)-RB(I)-RBOXP+RBOX
        CLINT = ((RB(I-1)-RBOXP)*STA(I)+(RBOX-RB(I))*STA(I-1))/DEN
        WBOX = 2.*(RB(I-1)*RBOX-RB(I)*RBOXP)/DEN
        NBOX1 = I 
        GO TO 617
 615    CONTINUE
 614  CONTINUE

 617  CONTINUE
      SPLANQ = .25*SPLAN
      ROOTC = ROOTCT-((WBOX/SPAN)*(ROOTCT-TIPC))
      AQ = ROOTC-TIPC
      BQ = ((ROOTC+ROOTCT)*(SPAN-WBOX))/2
      CQ = SPLANQ*(SPAN-WBOX)
      HP = SQRT(BQ*BQ-4*AQ*CQ)
      H = (BQ-HP)/(2*AQ)
      D = TGAML*H
      CPW = XCLWNG+(.25*ROOTCT)+D

      BP = .5*SPAN
      DPLP1 = TGAMS*BP*CLRW1
      DPLP2 = TGAMS*BP*CLRW2
      DPLP3 = TGAMS*BP*CLRW3
      CLPE1 = XCLWNG+(.25*ROOTCT)+DPLP1
      CLPE2 = XCLWNG+(.25*ROOTCT)+DPLP2
      CLPE3 = XCLWNG+(.25*ROOTCT)+DPLP3

      LBOX = (1.-CS1-CS2)*(ROOTCT+(TIPC-ROOTCT)*WBOX/SPAN)
      CLINTP = CLINT +LBOX

      DO 616 I=NBOX1,60
      IF(STA(I).GT.CLINTP) GO TO 618
 616  CONTINUE
 618  NBOX2 = I-1 
      TBOX = TRATWR*ROOTC
      DO 501 I=NBOX1,NBOX2
      RBT = RB(I)+TBOX/2.
      IF(RBT.LT.RBMAX) GO TO 502
 501  CONTINUE
      I = NBOX2+1
 502  NBOX3 = I
 505  CONTINUE

C     This BS from AC centerline
      BS = SPAN/2./COS(GAMS) 
      CLPW1 = BS*CLRW1
      CLPW2 = BS*CLRW2
      CLPW3 = BS*CLRW3

C     ADDITION FOR BENDING MOMENT DUE TO GEAR ON WING,M. CHAMBERS, 7/95
C     COMPUTE CLGW FROM AIRCRAFT CENTERLINE
      CLGW1 = CLRGW1 * BS 
      CLGW2 = CLRGW2 * BS 

C     CHANGES FOR ENGINES ON WING AND BODY (WPROPW), M. CHAMBERS, 7/95
      IF (ENWG .EQ. 6.) THEN
        WENG1 = WPROPW/ENWG
        WENG2 = WPROPW/ENWG
        WENG3 = WPROPW/ENWG
      ELSE IF (ENWG .EQ. 4.) THEN 
        WENG1 = WPROPW/ENWG
        WENG2 = WPROPW/ENWG
        WENG3 = 0.
      ELSE IF (ENWG .EQ. 2.) THEN 
        WENG1 = WPROPW/ENWG
        WENG2 = 0.
        WENG3 = 0.
      ELSE IF (ENWG .EQ. 0.) THEN 
        WENG1 = 0.
        WENG2 = 0.
        WENG3 = 0.
      ENDIF

C     SUBTRACTED PORTION OF WING NOT USED FOR STRUCTURE.  NOW USING 
C     ONLY (1-CS1-CS2) PORTION.  ALSO ACCOUNTING FOR WING SWEEP
C     BY MULTIPLYING BY COS(GAMS), M. CHAMBERS, 4/96
      VWING = 2.*BS*(1-CS1-CS2)*COS(GAMS)*(TRATWR*(ROOTC*ROOTC + 
     $ ROOTC*(TIPC-ROOTC) + (TIPC-ROOTC)
     $ *(TIPC-ROOTC)/3.)+(TRATWT-TRATWR)*(ROOTC*ROOTC/2. - 
     $ 2.*ROOTC*(ROOTC-TIPC)/3. + (ROOTC-TIPC)*(ROOTC-TIPC)/4.))

      IF (IFUEL.EQ.2) WWGPD = WTO*(CWMAN-1.)+WWGPD
      DENW = WWGPD/VWING
      WGLM = SLFM*WINGL*FACS*CWMAN
      DNWM = SLFM*DENW*FACS
      X1 = ROOTC/4.*COS(GAMLR)/COS(GAMLR-GAMS)
      X2 = ROOTC*0.75*COS(GAMTR)/COS(GAMTR-GAMS)
      X3 = X1*TAPER
      X4 = X2*TAPER
      Y1 = 3.*ROOTC/4.*SIN(GAMS)
      Y2 = Y1*TAPER

C     X5: Distance from TE-fuselage intersection to LE along X-axis
      X5 = X1+X2+Y1/BS*(X3+X4-X1-X2)
      WSHEAR = 24.*DSW*Y2*Y2*WGLM*(X3+X4)/FCSW
      XRT = X1+X2-X3-X4
      CPT = Y2/3.
      APT = Y2*(X3+X4)/2.
      BSP = BS

C     This BS only outside fuselage
      BS = BS*(SPAN-WBOX)/SPAN
      DY = BS/NWING
      ROOTCP = ROOTC
      X1 = ROOTC/4.*COS(GAMLR)/COS(GAMLR-GAMS)
      X2 = ROOTC*0.75*COS(GAMTR)/COS(GAMTR-GAMS)
      Y1 = 3.*ROOTC/4.*SIN(GAMS)
      X5 = X1+X2+Y1/BS*(X3+X4-X1-X2)
      XRT = X1+X2-X3-X4

      CLPW1 = CLPW1-BSP+BS
      CLPW2 = CLPW2-BSP+BS
      CLPW3 = CLPW3-BSP+BS

C     Initialize Y to (BS + DY), end of wing + dy
      Y = BS+DY
C     Initialize MWING
      MWING = NWING+1

      IF (ICALC .EQ. 3) THEN
        WRITE(2,1000)
C2345&*89 123456789 123456789 123456789 123456789 123456789 123456789 12
        WRITE(2,*)'   SPAN      BS      ROOTCP   TIPC    TAPER  TRATWR
     $TRATWT   GAML      GAMT    GAMSD     VWING     WFUEL      DENW'
        WRITE(2,1007)
        WRITE(2,760) SPAN,BS,ROOTCP,TIPC,TAPER,TRATWR,TRATWT,GAML,
     1             GAMT,GAMSD,VWING,WFUEL,DENW
        WRITE(2,1001)
        WRITE(2,1001)
        WRITE(2,1002)
        WRITE(2,1003)
        WRITE(2,1006)
      ENDIF

      DELTIP = 0.

C     Begin sizing wing at tip, moving toward fuselage
      DO 776 I=1,MWING
      Y = Y-DY
      if(y. gt. bs) y=bs
      R = ROOTC+Y*(TIPC-ROOTC)/BS

C     If ISCHRENK = 1 then do SCHRENK's Approximation, else use
C     trapezoidal planform.
      SELL = (SPAN-WBOX)/4. * ROOTC * (1 + TAPER) 
      IF (ISCHRENK .EQ. 1) THEN
        XELL = 4.*SELL/PI/BS*(1.-(Y/BS)**2.)**0.5
        AELL1 = (2.*SELL/PI/BS/BS*(Y*(BS*BS-Y*Y)**0.5+
     1         BS*BS*ASIN(Y/BS)))
        AELL2 = SELL - AELL1
        CPELL2 = 4./3./PI*(BS-Y)
      ENDIF

C     Use X5 for Y < where TE intersects fuselage 
      IF(Y.GT.Y1) GO TO 777
      X = X1+Y*(X5-X1)/Y1
      A1 = APT
      A2 = (X3+X4)*(BS-Y1)
      A3 = (X5-X3-X4)*(BS-Y1)/2.

C     A4: Area between LE and A5 (rectangular shape)
      A4 = X*(Y1-Y)

C     A5: Area between fuselage intersection and A4 (triangle shape)
      A5 = (X5-X)*(Y1-Y)/2.
      CP1 = BS-Y+Y2/3.
      CP2 = Y1-Y+(BS-Y1)/2.
      CP3 = Y1-Y+(BS-Y1)/3.
      CP4 = (Y1-Y)/2.
      CP5 = 2.*(Y1-Y)/3.
      A = A1+A2+A3+A4+A5
      CP = (CP1*A1+CP2*A2+CP3*A3+CP4*A4+CP5*A5)/A

C     XPRIME IS X SHORTENED BY FUSELAGE INTERSECTION, M. CHAMBERS, 8/95
      XPRIME = X

C     X IS X UNAFFECTED BY FUSELAGE INTERSECTION
      X = X1+X2-Y*XRT/BS
      GO TO 778
C  Ends fuselage-intersection-influenced analysis

C     For wing tip to intersection of TE with fuselage
 777  CONTINUE
      X = X1+X2-Y*XRT/BS
      A = (X3+X4)*(BS-Y)+0.5*(X-X3-X4)*(BS-Y)+APT
      CP = (APT*(BS-Y+Y2/3.)+(X3+X4)*(BS-Y)*(BS-Y)/2.+0.5*
     $(X-X3-X4)*(BS-Y)*(BS-Y)/3.)/A
      XPRIME = X
 778  CONTINUE

C     Following volume good for entire wing
C     SUBTRACTED PORTION OF WING NOT USED FOR STRUCTURE.  NOW USING 
C     ONLY (1-CS1-CS2) PORTION.  ALSO ACCOUNTING FOR WING SWEEP
C     BY MULTIPLYING BY COS(GAMS), M. CHAMBERS, 4/96
      V = (1-CS1-CS2)*COS(GAMS)*TRATWR*(ROOTC*ROOTC*(BS-Y)+ROOTC*
     $ (TIPC-ROOTC)*(BS*BS
     $ -Y*Y)/BS+(TIPC-ROOTC)*(TIPC-ROOTC)*(BS*BS*BS-Y*Y*Y)/3.
     $ /BS/BS) + (TRATWT-TRATWR)*(ROOTC*ROOTC*(BS*BS-Y*Y)/2./BS
     $ -2.*ROOTC*(ROOTC-TIPC)*(BS*BS*BS-Y*Y*Y)/3./BS/BS+(ROOTC-
     $ TIPC)*(ROOTC-TIPC)*(BS*BS*BS*BS-Y*Y*Y*Y)/4./BS/BS/BS)

      IF(I.GT.1) GO TO 611
      CG = 0.
      GO TO 612

C---- CG FOR VOLUME OUTBOARD OF STATION AT Y, CONSTANT T/C
C---- SUBTRACTED PORTION OF WING NOT USED FOR STRUCTURE.  NOW USING 
C---- ONLY (1-CS1-CS2) PORTION.  ALSO ACCOUNTING FOR WING SWEEP
C---- BY MULTIPLYING BY COS(GAMS), M. CHAMBERS, 4/96
 611  CG = (1-CS1-CS2)*COS(GAMS)*TRATWR/V*(ROOTC*ROOTC*
     $ (BS*BS-Y*Y)/2.+2.*ROOTC*(TIPC
     $ -ROOTC)*(BS*BS*BS-Y*Y*Y)/3./BS+(TIPC-ROOTC)*(TIPC-ROOTC)
     $ *(BS*BS*BS*BS-Y*Y*Y*Y)/4./BS/BS) + (TRATWT-TRATWR)/V*
     $ (ROOTC*ROOTC*(BS*BS*BS-Y*Y*Y)/3./BS - ROOTC*(ROOTC-TIPC)
     $ *(BS*BS*BS*BS-Y*Y*Y*Y)/2./BS/BS + (ROOTC-TIPC)*(ROOTC-
     $ TIPC)*(BS*BS*BS*BS*BS-Y*Y*Y*Y*Y)/5./BS/BS/BS) - Y

 612  CONTINUE

C     If ISCHRENK = 1 then do SCHRENK's Approximation, else use
C     trapezoidal planform.
      IF (ISCHRENK .EQ. 1) THEN
        A = (AELL2 + A)/2.
        CP = (CPELL2 + CP)/2. 
      ENDIF

      FL = WGLM*A
      FW = DNWM*V
      BM = FL*CP-FW*CG
      IF(Y.LT.CLPW1) BM = BM-WENG1*(CLPW1-Y)*SLFM*FACS
      IF(Y.LT.CLPW2) BM = BM-WENG2*(CLPW2-Y)*SLFM*FACS
      IF(Y.LT.CLPW3) BM = BM-WENG3*(CLPW3-Y)*SLFM*FACS

C---- ADDITION FOR BENDING MOMENT DUE TO GEAR ON WING,
C---- M. CHAMBERS, 7/95

C     RE-COMPUTE CLGW FROM INTERSECTION OF WING WITH BODY
      CLGW1 = CLGW1 * (SPAN - WBOX) / SPAN
      CLGW2 = CLGW2 * (SPAN - WBOX) / SPAN

C     FIND NUMBER OF MAIN GEAR, NG2
      IF (CLRGW1 .LT. .001) NG2 = 0
      IF (CLRGW1 .GT. .001 .AND. CLRGW2 .LT. .001) NG2 = 2
      IF (CLRGW1 .GT. .001 .AND. CLRGW2 .GT. .001) NG2 = 4

C     COMPUTE REDUCTION IN BENDING MOMENT FROM GEAR INERTIA LOAD
      IF (NG2 .GT. 0) THEN
        NG2 = FLOAT(NG2)
        IF(Y.LT.CLGW1) BM = BM-WGEAR2/NG2*(CLGW1-Y)*SLFM*FACS
        IF(Y.LT.CLGW2) BM = BM-WGEAR2/NG2*(CLGW2-Y)*SLFM*FACS
      END IF 
      
      IF ((BM.LT.0.0).AND.(ICALC.EQ.3)) WRITE(2,779)
 779  FORMAT(20H MOMENT IS NEGATIVE   )
      IF(BM.LT.0.0) BM = ABS(BM)

C     XS IS UNAFFECTED BY FUSELAGE INTERSECTION, XSPRIME IS 
C     SHORTENED BY FUSELAGE INTERSECTION, M. CHAMBERS, 8/95
      XS = X*(1.-CS1-CS2)
      XSPRIME = XPRIME*(1.-CS1-CS2)
      BMI = BM/XS

C     ADD ABILITY TO EVALUATE THICKNESS WITH LINEARLY VARYING
C     THICKNESS/CHORD RATIO, MDA 6/95
      T = (TRATWR + Y * (TRATWT - TRATWR) / BS) * R

C     END ADDITION FOR VARYING T/C
      BMIP = BMI/T/T/ESW/144 
      FSHEAR = FL-FW
      IF(Y.LT.CLPW1) FSHEAR = FSHEAR-WENG1*SLFM*FACS
      IF(Y.LT.CLPW2) FSHEAR = FSHEAR-WENG2*SLFM*FACS
      IF(Y.LT.CLPW3) FSHEAR = FSHEAR-WENG3*SLFM*FACS
      ASHEAR = FSHEAR/FCSW/144.
      ASHEAR = ABS(ASHEAR)
      DEL = DY
      IF(I.EQ.1) DEL = DY/2.
      IF(I.EQ.MWING) DEL = DY/2.
      WSHEAR = WSHEAR+DSW*DEL*ASHEAR*1728.
      DWT1 = (1.-2.*EC)/((1.-EC)*(SQRT(2.*EFFW)))
      DWT2 = (2.*EC-3.)/(2.*EC)
      DWT3 = 1.5/EC
      DWT = DWT1*(BMIP**DWT2)*(EFFC**DWT3)
      WEBS = T*DWT**((2.*EC)/(4.*EC-3.))
      WEBSC = WEBS*12.
      TC = WEBS*((BMIP*T)/EFFC/WEBS)**(1./EC)/2.
      TCC = TC*12.
      TGC = KGC*TC
      TGCC = TGC*12.
      NJW = 4

C     ****************** MINIMUM GAGE CHECK***************        
      IF(TGCC.GT.TMGW) GO TO 988 
       TGCC = TMGW
       TGC = TGCC/12.
       TC = TGC/KGC
       TCC = TC*12.
       NJW = 3
 988  CONTINUE

C     ****************** STRESS CHECK *******************
      TS = BM/(T*XS*FCSW*144.)
      TSC = TS*12.
      IF(TCC.GT.TSC) GO TO 999
       TCC = TSC
       TC = TCC/12.
       TGC = KGC*TC
       TGCC = TGC*12.
       NJW = 5
 999  CONTINUE

      TW1 = BMIP**(2.-(1/EC))*(EFFC*WEBS/T)**(1./EC)*(2./EFFW)
      TW = T*SQRT(TW1)
      TWC = TW*12.
      TGW = KGW*TW
      TGWC = TGW*12.

      IF (TGWC.LE.TMGW) THEN
        TGWC = TMGW
        TGW = TGWC/12.
        TW = TGW/KGW
        TWC = TW*12.
      ENDIF
 989  CONTINUE

C     USE XSPRIME FOR WEIGHTS TO ACCOUNT FOR ONLY FOR AREA UNAFFECTED
C     BY FUSELAGE INTERSECTION
      WCOVERS = 2.*TC*DEL*XSPRIME*DSW*1728.
      WWEBS = TW*T*DEL*DSW*1728.*(XSPRIME/WEBS)

      WBENDT = WCOVERS+WWEBS
      WBEND = WBEND+WBENDT

     
      PHI = (2.*TC*WEBS)/TW
      UWC = TC*DSW*1728.
      UWW = TW*DSW*1728.

      IF (ICALC .EQ. 3) THEN
        WRITE(2,761)Y,R,XS,XSPRIME,BM,WEBSC,TCC,TWC,TGCC,TGWC,
     $         UWC,UWW,NJW
      ENDIF 
 761  FORMAT(1X,F8.3,3X,F7.4,3X,F7.4,2X,F7.4,2X,F10.0,3X,F7.4,
     $3X,F6.4,4X,F7.5,6X,F6.4,5X,F7.5,3X,F8.4,1X,F9.4,4X,I1)

C     COMPUTE DEVIATION OF WINGTIP FROM TANGENT TO DEFLECTION CURVE 
C     DRAWN FROM ROOT (ASSUME ROOT IS FIXED SUPPORT).  THIS WIIL
C     BE THE SAME AS THE TIP DEFLECTION.  M. CHAMBERS, 8/95.
      DELTIP = DELTIP + BMIP/TC * (BS-Y) * DEL

  776  CONTINUE

C     WEIGHT OF THE LIFTING STRUCTURE (NO CARRY-THROUGH)
      WWGPD = WGNO*(WSHEAR+WBEND)*2.

C     BEGIN CARRY-THROUGH STRUCTURE WITH CONSTANT T/C
      BMBI = BM/LBOX*COS(GAMS)
      BMBIP = BMBI/TBOX/TBOX/ESW/144.
      DWTB = DWT1*(BMBIP**DWT2)*(EFFC**DWT3)
      WEBSB = TBOX*DWTB**((2.*EC)/(4.*EC-3))
      TCB = WEBSB*((BMBIP*TBOX)/EFFC/WEBSB)**(1./EC)/2.
      TCBC = TCB*12.
      TGCB = KGC*TCB
      TGCBC = TGCB*12.
      NJW = 4

C     ************* MINIMUM GAGE CHECK ******************
      IF(TGCBC.GT.TMGW) GO TO 1100
       TGCBC = TMGW
       TGCB = TGCBC/12.
       TCB = TGCB/KGC
       TCBC = TCB*12.
       NJW = 3
 1100 CONTINUE

C     ************* STRESS CHECK ***********************
      TSB = BM/(TBOX*LBOX*FCSW*144.)
      TSBC = TSB*12.
      IF(TCBC.GT.TSBC) GO TO 1200
       TCBC = TSBC
       TCB = TCBC/12.
       TGCB = KGC*TCB
       TGCBC = TGCB*12.
       NJW = 5
 1200 CONTINUE

      TW1B = BMBIP**(2.-(1/EC))*(EFFC*WEBSB/TBOX)**(1.
     $/EC)*(2./EFFW)
      TWB = TBOX*SQRT(TW1B)
      TWBC = TWB*12.
      TGWB = KGW*TWB
      TGWBC = TGWB*12.
      IF(TGWBC.GT.TMGW) GO TO 1300
       TGWBC = TMGW
       TGWB = TGWBC/12.
       TWB = TGWB/KGW
       TWBC = TWB*12.
 1300 CONTINUE

      WCOVERSB = 2.*TCB*WBOX*LBOX*DSW*1728.
      WWEBSB = TWB*TBOX*WBOX*DSW*1728.*(LBOX/WEBSB)
      WBDBOX = WCOVERSB+WWEBSB
      TBCOV = 12.*WEBSB*SQRT(BMBIP*TBOX/EFFC/WEBSB)/2.
      WENGT = (WENG1+WENG2+WENG3)*SLFM*FACS
      TORK = BM*SIN(GAMS)-(FL-FW-WENGT)*ROOTC/4.
      TTO = TORK/2./TBOX/LBOX/FCSW/144.
      WSHBOX = ASHEAR*WBOX*DSW*1728.
      WTOBOX = TTO*2.*(TBOX+LBOX)*WBOX*DSW*1728.
      TTO = 12.*TTO
      TBCOV = TBCOV+TTO
      WWBOX = WSHBOX+WBDBOX+WTOBOX

C      IF (ICALC .EQ. 3) THEN
C        WRITE(2,1005) CLBOX1,CLINT,CLINTP,LBOX,WBOX,TBOX,NJW,
C     $       WEBSB,TORK,TTO,TBCOV
C      ENDIF

 789  CONTINUE
      WWINGT = WWGPD+WWBOX

      IF (ICALC .EQ. 3) THEN
        WRITE (2,1005) LBOX,WBOX,TBOX,WEBSB,TTO+TBCOV,TORK,NJW,
     $      WWGPD,WSHEAR,WBEND,WWBOX,WSHBOX,WBDBOX,WTOBOX,WWINGT,
     $      DELTIP
      ENDIF

      LEB1 = ROOTCP*CS1
      LEB2 = TIPC*CS1
      TEB1 = ROOTCP*CS2
      TEB2 = TIPC*CS2
      BSA = .5*(SPAN-WBOX) 
      LEA = ((LEB1+LEB2)/2.)*BSA
      TEA = ((TEB1+TEB2)/2.)*BSA
      CNTRLA = 2.*(LEA+TEA)
      ABOX = LBOX*WBOX
      SB1 = (1.-CS1-CS2)*ROOTCP
      SB2 = (1.-CS1-CS2)*TIPC
      SA = ((SB1+SB2)/2)*BSA
      CTST = (2.*SA)+ABOX  
   
      RETURN

 760  FORMAT(1X,F8.4,2X,F8.3,2X,F7.4,2X,F7.4,2X,F5.3,2X,F5.3,2X,
     $      F5.3,2X,F8.5,2X,F8.5,2X,F6.3,2X,F9.3,2X,F9.2,2X,F8.3)   
 762  FORMAT(F7.3,2X,F7.3,2X,F7.3,2X,F7.4,2X,F7.4,2X,F6.3,
     $8X,I1,4X,F6.4,3X,F11.1,2X,F9.4,2X,F6.4)
 763  FORMAT(1X,F8.2,3X,F10.2,3X,F10.2,3X,F8.2,3X,F8.2,3X,F8.2,
     $3X,F9.2,3X,F9.2,3X,F8.2,3X,F6.3)
 1004 FORMAT('  Carrythrough Dimensions',/)
 1005 FORMAT(/,2X, 'Carrythrough Box Dimensions',/,
     $   4X,'Length', T40,F12.4,' ft',/, 
     $   4X,'Width',T40,F12.4,' ft',/,
     $   4X,'Height', T40,F12.4,' ft',/,
     $   4X,'Side Plate Thickness',T40,F12.4,' ft',/,
     $   4X,'Top Plate Thickness', T40,F12.4,' ft',//,
     $   4X,'Total Box Torque',T40,F12.2,' ft-lbs',/,
     $   4X,'Failure Criterion',T46,I1,//,
     $   2X,'Wing Weight Breakdown',/,
     $   4X,'Wing Panels Total',T40,F12.0,' lbs',/,
     $   6X,'Weight due to Shear (each)',T40,F12.4,' lbs',/,
     $   6X,'Weight due to Bending (each)',T40,F12.4,' lbs',/,
     $   4X,'Carrythrough Box Total',T40,F12.0,' lbs',/,
     $   6X,'Weight due to Shear',T40,F12.4,' lbs',/,
     $   6X,'Weight due to Bending',T40,F12.4,' lbs',/,
     $   6X,'Weight due to Torque',T40,F12.4,' lbs',/,
     $   4X,'Total Wing Structural Weight',T40,F12.0,' lbs',//,
     $   4X,'Tip Deflection',T40,F12.4,' ft',///)
 1008 FORMAT('  ')
 1009 FORMAT('  ')
 1000 FORMAT(2X,'Beam Method Structures Output',//,
     1  2X, 'Wing Spanwise Conditions',/)            
 1001 FORMAT(' ')
 1002 FORMAT('  Wing      Chord     Length     Length     Bend',
     *'      Web',
     1'    Cover        Web         Cgage       Wgage',
     2'      UnitWt    UnitWt  Cond                    ')
 1003 FORMAT('  Station                        Prime      Mom',
     *'      Space',
     1'   Thick       Thick        Thick       Thick      Covers',
     2'    Webs                      ')                          
 1006 FORMAT('     ft        ft        ft       ft       ft-lbs',
     $'       in',
     1'       in         in          in          in',
     2'       lb/ft2    lb/ft2               ')
 1007 FORMAT('    FT        FT      FT       FT',
     1'                              DEG.      DEG.    DEG.',
     2'      FT3        LBS        LB/FT3           ')
 5000 FORMAT('  CONTROL AREA        STRUCTURE AREA         SPLAN     ')
 5001 FORMAT('      FT2.                 FT2.               FT2.     ')
 5002 FORMAT(4X,F7.2,14X,F7.2,12X,F6.0)
      END


