      SUBROUTINE PDCEX(ICALC)

C    RECEIVED FROM DR. M. ARDEMA    6/02/86
C    TYPED BY STERLING SOFTWARE     6/11/86
C    Modification History
C    1. 7-31-86  H. Miura
C       Convert Call INPUT to namelist INDAT read from unit 5.
C    2. 7-31-86  H. Miura
C       All format statements put together at the end of program.
C    3. 7-31-86  H. Miura
C       All labels corrected to appear in ascending order.  Labels for
C       FORMAT statements were adjusted to start from 1000.
C    4. 7-19-94  M. Chambers
C       TPS Code, etc. commented out.
C    5. 8-94     M. CHAMBERS
C       PROBLEM WITH GEAR BENDING MOMENTS FIXED (M@CL = 0).
C    6. 8-94     M. CHAMBERS
C       DETAILED SHELL GEOMETRY PARAMETERS AND OUTPUT ADDED.
C    7. 9-94     M. CHAMBERS
C       ADDED LANDING AND BUMP LOAD INPUT SIMILAR TO HAVOC 
C       (INCLUDING NEW MANEUVER SUBROUTINE SIMILAR TO HAVOC).
C    8. 9-94     M. CHAMBERS
C       COMMENTED OUT "DIMENSION  ILOAD(40)".  ILOAD(40) IS NO LONGER USED
C       AND IS REPLACED BY "INTEGER  ILOAD".
C    9. 10-94    M. CHAMBERS
C       INSTALLED AND RAN PDCYL AND SUBROUTINES ON SGI WORKSTATIONS.
C       REQUIRED FORMING MAKEFILE AND RUN UNIX SCRIPT FILES.
C    9. 11-94    M. CHAMBERS
C       CONDENSATION OF PDCYL AND SUBROUTINES INTO SINGLE FILE TO BE
C       PLACED INTO ACSYNT TO CALCULATE WBODY AND WWING
C   10. 2-95     M. CHAMBERS
C       INTEGRATION OF POWER LAW-CYLINDRICAL-POWER LAW BODY TO MATCH
C       ACSYNT PLANFORM.  NOW CALCULATE POW1 & POW2 BY EQUATING VOLUMES
C       OF FOREBODY AND AFTERBODY WITH ACSYNT
C   11. 7-95     M. ARDEMA
C       CHANGE T/C FROM CONSTANT TO LINEAR VARYING FROM TIP TO ROOT OF 
C       WING IN SUBROUTINE WING2.  T/C = T/C @ ROOT IF T/C @ TIP = 0.
C   12. 7-95     M. CHAMBERS
C       NUMBER OF ENGINES ON WING AND FUSELAGE, AND PROPULSION WEIGHT
C       INPUT FROM ACSYNT NAMELISTS WPOD, FPOD, AND FIXW.
C   13. 7-95     M. CHAMBERS
C       ADDED LANDING GEAR INERTIA TO WING WHEN MAIN GEAR IS ATTACHED 
C       TO WING.  THIS REDUCES THE WING MOMENTS DURING MANEUVER.  ALSO 
C       HAD TO ADD POSITION OF GEAR AS FRACTION OF SPAN.
C   14. 8-95     M. CHAMBERS
C       ENGINE POSITIONS ON WING AND BODY INPUT FROM ACSYNT NAMELISTS WPOD
C       AND FPOD.  INITIALIZED WEIGHT OF THE WING BENDING MATERIAL FOR 
C       OSIRIS (IT IS NOT DONE AUTOMATICALLY).
C   15. 8-95     M. CHAMBERS
C       REMOVED INPUT VARIABLES IN COMMON WITH ACSYNT.  

C  AE0530  WB STRUCTURAL WEIGHTS  M.D.ARDEMA  11/16/66


      DIMENSION AVBMM(60)

      REAL NXB,NXA,NXP,NYP,NXSC,NXST,NXT,
     *KGW,KGC, KDEW,KDFW,LBOX

      INTEGER ISTAMA,IGEAR,ICOMND,ITAIL,NWING,
     *IFUEL,ISCHRENK,ICYL

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

      COMMON/AX/POW1,POW2,CL1,CL2,P111,P112,P121,P122,P211,
     *  P212,P221,P222,DENB,CL1A,CL1B,CL2A,CL2B

      COMMON/AX1/WSAV(60),STA(60),RB(60),VB(60),AS(60),AP(60),
     *  CPBD(60),CGB

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

      REAL KX
      COMMON      /CONCEPT/         BRAT,TRAT,ANGLR,MARK,KX 

      COMMON /PDCYLCOM/   ICOMND,  CLRG1,  CLRG2,  WFGR1,  WFGR2,
     1    ISTAMA, FR    , PS    ,CF     ,
     2    CLRT  , WFPROP,  CKF  ,  WCW  ,  WCA  ,
     5    AXAC  , ART   ,CLBR1  ,  WMIS  , WSUR , PHI
     
      COMMON /OUTPUTS/  WEBSB, TORK, WSHEAR, WBEND, WSHBOX, WBDBOX,
     1  WTOBOX, DELTIP, CNTRLA, CTST, WNOP, WNOPS, WSEC


      INCLUDE 'VEHICON.INC'
      INCLUDE 'LAND.INC'

      A0M = 7.
      DX = 60.
      PI=3.14159

      XCLWNGR = (XCLWNG/CL) - (CS1*ROOTCT/CL)
      IF (ICYL .EQ. 0) THEN
        POW1 = 0.5*((PI*CL*B*B/4./VBOD) - 1.)
        POW2 = POW1
      ELSE IF (ICYL .EQ. 1) THEN
        POW1 = PI*B*B*CL1A/8.0/VOLNOSE - 0.5
        POW2 = PI*B*B*CL2B/8.0/VOLTAIL - 0.5
      END IF

      P111=POW1+1.
      P112=POW1+2.
      P121=2.*POW1+1.
      P122=2.*POW1+2.
      P211=POW2+1.
      P212=POW2+2.
      P221=2.*POW2+1.
      P222=2.*POW2+2.

C     -----------------------
C     Basic Geometry analysis
C     -----------------------
      FR = CL / B
      CLB1=CLBR1*CL
      CLB2=CLBR2*CL
      CLB3=CLBR3*CL
      CLB4=CLBR4*CL
      CLB5=CLBR5*CL
      CLB6=CLBR6*CL
      CLB7=CLBR7*CL
      CLB8=CLBR8*CL
      CLB9=CLBR9*CL
      CLB10=CLBR10*CL
      CLB11=CLBR11*CL
      DX=CL/DX
      CL2=CL-CL1
      CLT=CLRT*CL

C     CHANGES FOR ENGINES ON WING AND BODY, M. CHAMBERS, 7/95
      CLP1=CLRP1*CL
      CLP2=CLRP2*CL
      WPROP=WFP*WTO

C     WEIGHT FRACTIONS FOR PROPULSION ON WING & FUSELAGE
      WFPW = ENWG * WFP / ENPD
      WFPF = (ENPD - ENWG) * WFP / ENPD

C     WEIGHTS OF WING AND FUSELAGE PROPULSION
      WPROPW = WFPW * WTO
      WPROPF = WFPF * WTO

      SPLAN=WTO/WINGL
      ATAIL=ART*SPLAN
      WTAIL=UWT*ATAIL            
      WGEAR1 = WFGR1*WTO
      WGEAR2 = WFGR2*WTO 
      WGEAR = WGEAR1+WGEAR2
      CLG1 = CL*CLRG1
      CLG2 = CL*CLRG2
      CGG = ((WGEAR1*CLG1)+(WGEAR2*CLG2))/(WGEAR)

      IF (ICYL .EQ. 0) THEN
        CGB = (CL1*CL1/P122-CL2*CL2/P222+CL2*(CL1+CL2)/P221)/
     >  (CL1/P121+CL2/P221)
      ELSE IF (ICYL .EQ. 1) THEN
        CGB = (CL1A*CL1A/P122+(CL1B*CL1B-CL1A*CL1A)/2.-CL2B*CL2B/
     >  P222+CL*CL2B/P221)/(CL1A/P121+(CL1B-CL1A)+CL2B/P221)
      END IF

      WFUEL = WTO*WTFF
      WWGPD = UWWG*SPLAN
      WBOD = WTO-WWGPD-WTAIL-WPROP-WGEAR
      IF (IFUEL.EQ.2) WBOD = WBOD-WFUEL
      IF (IFUEL.EQ.2) WWGPD = WWGPD+WFUEL 

C     ----------------------
C     Wing weight estimation
C     ----------------------
      CALL WING2(ICALC)


C     WBOD : Body weight             (lbs)
      WBOD=WTO-WWINGT-WTAIL-WPROP-WGEAR
      IF (IFUEL.EQ.2) WBOD = WBOD-WFUEL
      IF (IFUEL.EQ.2) WWINGT = WWINGT+WFUEL

C     CGP  : Propulsion CG position
C     CHANGES FOR ENGINES ON WING AND BODY, M. CHAMBERS, 7/95
      CGPF = (CLP2+CLP1)/2.
      IF (ENWG .GT. 0.) THEN
        CGPW = ((CLPE1*WENG1)+(CLPE2*WENG2)+(CLPE3*WENG3))
     1  /(WENG1+WENG2+WENG3)
      ELSE
        CGPW = 0.
      END IF      
      CGP = (CGPW*WPROPW + CGPF*WPROPF) / WPROP

C     STAMA: STATIC MARGIN
      IF(ISTAMA.EQ.2) GO TO 40
      CG=(WBOD*CGB + WWINGT*STAMA*CL/100. + WTAIL*CLT + WPROP*CGP
     *+WGEAR*CGG) / (WTO-WWINGT)
      CPW = CG+STAMA*CL/100.
      XCLWNG = CPW-(.25*ROOTCT)-D
      GO TO 50
 40   CG = (WBOD*CGB + CPW*WWINGT + CLT*WTAIL + WPROP*CGP
     $+ WGEAR*CGG)
     1/WTO

C2345&*89 123456789 123456789 123456789 123456789 123456789 123456789 12
      WRITE(2,4000)CGB,WBOD,CPW,WWINGT,CLT,WTAIL,CGP,WPROP,CGG,WGEAR,CG,
     & WBOD+WWINGT+WTAIL+WPROP+WGEAR

      STAMA = 100.*(CPW-CG)/CL
 50   CONTINUE
      CGW = CPW
      GAMLR=GAML*PI/180. 
      CLW1 = CPW-2.*ROOTCT/3.
      CLW2 = CLW1+ROOTCT

      IF (ICYL .EQ. 0) THEN
        ABOD=B*(CL1/P111+CL2/P211)
        VBOD=PI*B*B*(CL1/P121+CL2/P221)/4.
        VOL=VBOD 
        ASUR=PI*ABOD
        DENB=WBOD/VBOD
        DEN=WTO/VBOD
        ABODL = ABOD
        CPB = (CL1*CL1/P112-CL2*CL2/P212+CL2*(CL1+CL2)/P211)
     >  /(CL1/P111+CL2/P211)

      ELSE IF (ICYL .EQ. 1) THEN
        ABOD=B*(CL1A/P111+CL2B/P211+(CL1B-CL1A))
        VBOD=PI*B*B*(CL1A/P121+CL2B/P221+(CL1B-CL1A))/4.
        VOL=VBOD
        ASUR=PI*ABOD
        DENB=WBOD/VBOD
        DEN=WTO/VBOD
        ABODL=ABOD
        CPB=((CL1A**2./P112)+((CL1B**2.-CL1A**2.)/2)-(CL2B**2./P212)
     >  +(CL*CL2B)/P211)/((CL1A/P111)+(CL1B-CL1A)+(CL2B/P211))
      END IF

C     Print geometry parameters
      IF (ICALC .EQ. 3) THEN
c        WRITE(2,2300)
c        WRITE(2,2400)WTO,WBOD,WWINGT,WPROP,WTAIL,CG,RG
c        WRITE(2,2600)
c        WRITE(2,2700)
c        WRITE(2,2800)VBOD,DEN,CL1,FR,CL,B,ABOD,ASUR,CLP1,CLP2
c        WRITE(2,2600)
c        WRITE(2,3700)
c        WRITE(2,3800)ATAIL,CLT
c        WRITE(2,3820)
      ENDIF

C     -------------------------
C     Maneuver Load Calculation
C     -------------------------
      CALL MAN2

      X=0.0
      DO 140 I=1,60
      X=X+DX      
      IF(X.GT.CL)GO TO 230
      AVBMM(I)=AVBMM1(I)*FACS

  140 CONTINUE
      X=0.0
  230 CONTINUE

C     Print 
C      IF (ICALC .EQ. 3) THEN
C        WRITE(2,1000)
C        WRITE(2,2600)
C        WRITE(2,3400)
C        WRITE(2,3500)CKF,FSK,EFF,CK,PG,CF,CTHIC,FACS,DEFL
C      ENDIF
      NTB = 1
  650 CONTINUE

C     Print out titles     
      IF ((ICALC .EQ. 3).AND.(NTB .EQ. 2)) THEN
        IF ( ICOMND .EQ. 1 ) THEN
C          WRITE(2,1001)
          WRITE(2,1019)
          WRITE(2,1020)
          WRITE(2,1021)

        ELSE
C       Print out shell geometry titles
C          WRITE(2,1001)
          WRITE(2,1033)
          WRITE(2,*)
          WRITE(2,1034)
          WRITE(2,1035)
          WRITE(2,1036)
        END IF
      ENDIF

      XFT=0.0
      DO 419 I=1,60
        IF (I.EQ.60) GO TO 400
        XFT=XFT+DX
        IF (XFT.GT.CL) GO TO 430
        X=12.0*XFT

        IF (ICYL .EQ. 1) THEN
          IF (XFT .LT. CL1A) Y = 6.0*B*(XFT/CL1A)**POW1
          IF (XFT .GE. CL1A .AND. XFT .LE. CL1B) Y = 6.0*B
          IF (XFT.GT.CL1B .AND. XFT.LE.CL)Y=6.0*B*((CL-XFT)/CL2B)**POW2
        ELSE IF (ICYL .EQ. 0) THEN
          IF (XFT .LE. CL1) Y=6.0*B*(XFT/CL1)**POW1
          IF (XFT .GT. CL1) Y=6.0*B*((CL-XFT)/CL2)**POW2
        END IF

250     FSP=FSK*Y
        P=2.*3.14*Y
        IF (P.LT.0.01) P=0.01
        A=3.14*Y*Y
        CMIY=A*Y
        IF (CMIY.LT.01) CMIY=.01

        IF (XFT.GT.CLB1) GO TO 630
        K=1
        GO TO 603
630     IF (XFT.GT.CLB2) GO TO 631
        K=2
        GO TO 603
631     IF (XFT.GT.CLB3) GO TO 632
        K=3
        GO TO 603
632     IF (XFT.GT.CLB4) GO TO 633
        K=4
        GO TO 603
633     IF (XFT.GT.CLB5) GO TO 634  
        K=5
        GO TO 603
634     IF (XFT.GT.CLB6) GO TO 635
        K=6
        GO TO 603
635     IF (XFT.GT.CLB7) GO TO 636
        K=7
        GO TO 603
636     IF (XFT.GT.CLB8) GO TO 637
        K=8
        GO TO 603
637     IF(XFT.GT.CLB9) GO TO 638
        K=9
        GO TO 603
638     IF (XFT.GT.CLB10) GO TO 639
        K=10
        GO TO 603
639     IF (XFT.GT.CLB11) GO TO 640
        K=11
        GO TO 603
640     CONTINUE
        K=12
        GO TO 603
603     CONTINUE

C       NEW VEHICLE CONCEPT SUBROUTINE
        CALL VEHCON(NTB,K)    
        ESHL=SE
        TBM=CK*TMG     
        NXB=12.*AVBMM(I)*Y/CMIY
        NXA=AXAC*WSAV(I)/P
        NXP=PG*A/P
        NYP=PG*Y*CPR
        IF(JCON.EQ.2) NXP=2.*A/P
        IF(JCON.EQ.2) NYP=2.*Y*CPR
        NXSC=NXB
        IF(JCON.EQ.1) NXSC=NXB-NXP
        IF(XFT.LE.CLP1)NXSC=NXSC+NXA
        S=NXSC
        NXST=NXB+NXP
        IF(XFT.GT.CLP2)NXST=NXST+NXA
        NXST=AMAX1(0.0,NXST)
        NXT=AMAX1(NXST,NYP)
        TBT=NXT/FTS
        IF(S.GT.0.0)GO TO 290
        IF(TBM-TBT)271,271,272
271     TB=TBT
        TG=TB/CK
        NJ=1
        GO TO 280
272     TB=TBM
        TG=TMG
        NJ=2
280     CONTINUE
        AF=0.0
        FS=100.
        GO TO 399
290     TBC=S/FCS
        B1=2.*(CF*Y*CMIY*S/CKF/EF)**.5
        B2=(S/ES/EFF)**.5
        TBB = (3.*DF*B1*B2*B2*B2/DS)**0.25
        TBBT = 1.333*TBB
        IF(ICON.EQ.2) TBB=Y*(S/Y/ES/EFF)**(1./ESHL)
        IF(TBB-TBC)300,300,330
300     IF(TBC-TBT)310,310,354
310     IF(TBT-TBM)320,320,370
320     TB=TBM
        TG=TMG
        NJ=3
        GO TO 380
330     IF(TBB-TBT)310,310,340
340     IF(TBB-TBM)320,320,350
350     TB=TBB
        TG=TB/CK
        FS=(3.*DF*B1/DS/B2)**.5
        NJ=4
        GO TO 390
354     IF(TBC-TBM)320,320,360
360     TB=TBC
        TG=TB/CK
        NJ=5
        GO TO 380
370     TB=TBT
        TG=TB/CK
        NJ=6
380     FS=TB*TB*PS*ES*EFF/S
390     AF=2.*(CF*Y*CMIY*S/FS/EF/CKF)**.5
399     CONTINUE
        IF(ICON.EQ.2) AF=0.0
        WSU=DS*P*TB*12.0
        WFU=DF*AF*P/FS*12.0
        THIC=0.0
        IF (S.GT.0.0)THIC=CTHICK*(TB*TB*TB*ES/S)**.5
        UWS = WSU / P *12.
        UWF = WFU / P *12.
        IF (I .EQ. 60) GO TO 400
        IF (I .EQ. 60 - 1) GO TO 401
        IF (I .EQ. 1) GO TO 402
        WS = WSU * DX + WS
        WF = WFU * DX + WF
        SUR = P * DX/12. + SUR
        VDEC = P * THIC * DX / 144. + VDEC
        GO TO 400
402     WS = 9. * WSU * DX/8.
        WF = 9. * WFU * DX/8.
        SUR = 9. * P * DX/12./8.
        VDEC = 9. * P * THIC * DX / 144. / 8.
        GO TO 400
401     WS = 9. * WSU * DX/8. + WS
        WF = 9. * WFU * DX/8. + WF
        SUR = 9. * P * DX/12./8. + SUR
        VDEC = 9. * P * THIC * DX / 144. / 8. + VDEC
400     CONTINUE
        STRESS=12.0*AVBMM(I)*Y/TB/CMIY
        ASEC=A/144.0

C       Print out bending moment data
        IF ((ICOMND.EQ.1).AND.(ICALC.EQ.3).AND.(NTB.EQ.2)) THEN
          WRITE(2,1024) XFT,AVBMM(I),THIC,STRESS,TB,
     1      TG,FS,NJ,ASEC,UWS,UWF,BMSTR(I)

        ELSEIF ((ICOMND.EQ.0).AND.(ICALC.EQ.3).AND.(NTB.EQ.2)) THEN
C         Get shell structure geometry 
          CALL GEOSHELL (ICOMND,MARK,TB,ANGLR,TRAT,BRAT,KX,
     1      S,FS,ES,TS,TW,BW,BS,BF,HIGH,AF)
          WRITE(2,1032) XFT,TS,TW,BW,BS,BF,ANGLR,HIGH,
     1      AF,KCON
        END IF

419   CONTINUE

430   CONTINUE
      VFRAC = 1. - VDEC/ VOL
      W=WS+WF
      WBODY=W*WCW+SUR*WCA
      WNOP = WBODY - W
      WSEC = WMIS + WSUR*SUR
      WTOT = WBODY + WSEC
      WVOLPEN = 32052. * (1.-VFRAC)*VOL/16667.0
      WGTOT = WTOT + WVOLPEN
      PHI = WGTOT + 32052.*(1.-VFRAC)
      WSF = WS/WTO
      WFF = WF/WTO
      WNOPF = WNOP/WTO
      WSECF = WSEC/WTO
      WTOTF = WTOT/WTO
      WPENF = WVOLPEN/WTO
      WGTF = WGTOT/WTO
      IF(SUR.LT.01) SUR=.01
      WSS = WS/SUR
      WFS = WF/SUR
      WNOPS = WNOP/SUR
      WSECS = WSEC/SUR
      WTOTS = WTOT/SUR
      WPENS = WVOLPEN/SUR
      WGTS = WGTOT/SUR
      GAM=GAM*180.0/PI
      DELC=DELCC
      DELT=DELTC
      IF(ITRIM.EQ.2) DELC=60.0
      IF(IWT.EQ.2) WTO=0.0
     
C      IF (ICALC .EQ. 3) THEN
C        WRITE (2, 3401) WS     , WSF     , WSS     ,
C     1                WF     , WFF     , WFS     ,
C    4                WNOP   , WNOPF   , WNOPS   ,
C     R                WSEC   , WSECF   , WSECS   ,
C     6                WTOT   , WTOTF   , WTOTS   ,
C     7                WVOLPEN, WPENF   , WPENS   ,
C     8                WGTOT  , WGTF    , WGTS    
C        WRITE (2,3301) SUR,VFRAC,PHI
C      ENDIF

510   CONTINUE

1     CONTINUE
      IF (NTB.EQ.2) GO TO 651
      NTB=2
      WSP=WS
      WFP=WF
      WNOPP=WNOP
      WTOTP=WTOT
      WVOLPP=WVOLPEN
      WGTOTP=WGTOT
      WSFP=WSF
      WFFP=WFF
      WNOPFP=WNOPF
      WTOTFP=WTOTF
      WPENFP=WPENF
      WGTFP=WGTF
      WSSP=WSS
      WFSP=WFS
      WNOPSP=WNOPS
      WTOTSP=WTOTS
      WPENSP=WPENS
      WGTSP=WGTS
      GO TO 650

651   CONTINUE
      WS=(WS+WSP)/2.
      WF=(WF+WFP)/2.
      WNOP=(WNOP+WNOPP)/2.
      WTOT=(WTOT+WTOTP)/2.
      WVOLPEN=(WVOLPEN+WVOLPP)/2.
      WGTOT=(WGTOT+WGTOTP)/2.
      WSF=(WSF+WSFP)/2.
      WFF=(WF+WFFP)/2.
      WNOPF=(WNOPF+WNOPFP)/2.
      WTOTF=(WTOTF+WTOTFP)/2.   
      WPENF=(WPENF+WPENFP)/2.
      WGTF=(WGTF+WGTFP)/2.
      WSS=(WSS+WSSP)/2.
      WFS=(WFS+WFSP)/2.
      WNOPS=(WNOPS+WNOPSP)/2.
      WTOTS=(WTOTS+WTOTSP)/2.
      WPENS=(WPENS+WPENSP)/2.
      WGTS=(WGTS+WGTSP)/2.
      WTSYN = WTOT

C     ADDED AVERAGE WEIGHT SUMMARY (TOP + BOTTOM) 
      IF (ICALC .EQ. 3) THEN
        WRITE (2, 3401) WS     , WSF     , WSS     ,
     1                WF     , WFF     , WFS     ,
     4                WNOP   , WNOPF   , WNOPS   ,
     R                WSEC   , WSECF   , WSECS   ,
     6                WTOT   , WTOTF   , WTOTS   ,
     7                WVOLPEN, WPENF   , WPENS   ,
     8                WGTOT  , WGTF    , WGTS    
        WRITE (2,3301) SUR,VFRAC,PHI
      ENDIF

      RETURN

 3401 FORMAT(//,2X,'Fuselage Weight Breakdown',
     1       //,22X,'Weight          Weight          Unit',
     2        /,22X,'(lbs)          Fraction        Weight',
     3        /,22X,'                              (lbs/ft2)',
     4        /,5X,'-------------------------------------------------'
     5         ,   '-------',  
     6       //,5X,'Shell    ', 5X, F10.2, 5X, F10.4, 5X, F10.4,
     7        /,5X,'Frames   ', 5X, F10.2, 5X, F10.4, 5X, F10.4,
     +        /,5X,'NonOptimum', 4X, F10.2, 5X, F10.4, 5X, F10.4,
     R        /,5X,'SEC      ', 5X, F10.2, 5X, F10.4, 5X, F10.4,
     2        /,5X,'SubTotal  ', 4X, F10.2, 5X, F10.4, 5X, F10.4,
     3        /,5X,'Vol Penalty', 3X, F10.2, 5X, F10.4, 5X, F10.4,
     4        /,5X,'Total    ', 5X, F10.2, 5X, F10.4, 5X, F10.4)
 3301 format(
     * ///5x,'Surface Area, sqft               ', F14.2,
     * /  5x,'Volume Ratio                     ', F14.2,
     * /  5x,'Fuselage Total Structural Weight ', F14.2)
 1000 FORMAT(1H0)
 1001 FORMAT(1H1)
 1019 FORMAT(4X,4HFuse,6X,7HBending,6X,5HThick,6X,5HShell,6X,5HEquiv,
     1 5X,4HGage,5X,5HFrame,6X,4HCond,3X,
     2 7HSection,5X,5HShell,4X,5HFrame,4X,3HMax)
 1020 FORMAT(4X,4HStat,6X,6HMoment,17X,6HStress,6X,5HThick,4X,5HThick,
     1 5X,5HSpace,12X,4HArea,8X,6HUnitWt,
     7 3X,6HUnitWt,2X,7HBending)
 1021 FORMAT(5X,'ft',8X,'ft lbs',6X,' in',9X,'psi',8X,'in',8X,'in',8X,
     1 'in',13X,'sq ft',7X,'lb/ft2',3X,'lb/ft2',3X,'Case')
 1024 FORMAT(F9.4,1X,F13.3,1X,F8.4,1X,F13.4,2X,F7.4
     1       ,1X,F8.4,1X,F10.4,4X,I1,4X,F9.4,1X,F9.4,F9.4,4X,A4)
 1032 FORMAT(T2,F7.2,T12,F9.5,T22,F9.5,T32,F9.5,T42,F9.5,T52,F9.5,
     1       T62,F9.5,T75,F9.5,T85,F9.5,T102,I1)
 1033 FORMAT(T23,'SHELL GEOMETRIC DIMENSIONS')
 1034 FORMAT(T4,'FUSE',T14,'SKIN',T24,'STIFFENER',T34,'STIFFENER',T44,    
     1       'STIFFENER',T55,'FLANGE',T65,'ANGLE',T79,'WEB',T89,'FRAME',
     2       T101,'KCON')
 1035 FORMAT(T4,'STAT',T14,'THICKNESS',T25,'THICKNESS',T35,'HEIGHT',
     1       T45,'SPACING',T55,'LENGTH',T78,'HEIGHT',T89,'AREA',T99)
 1036 FORMAT(T4,' FT',T14,' IN',T26,' IN',T37,' IN',T46,' IN',T55,
     1       'IN',T64,'DEGREES',T78,'IN',T89,'IN*2')
 1100 FORMAT(1H1)
 1200 FORMAT(15H FRAME WEIGHT =,F7.0,24HLBS, FRAME UNIT WEIGHT =,F7.4,10
     1HLBS/SQFT   )
 1300 FORMAT(15H SHELL WEIGHT =,F7.0,24HLBS, SHELL UNIT WEIGHT =,F7.4,22
     1HLBS/SQFT              )
 1400 FORMAT(15H STRUC WEIGHT =,F7.0,24HLBS, TOTAL UNIT WEIGHT =,F7.4,24
     1HLBS/SQFT, SURFACE AREA =,F7.0,19HSQFT, BODWIWTFRAC =,F7.5)
 1500 FORMAT(1X,F6.2,2X,E11.4,2X,F6.4,2X,E11.4,2X,F6.4,2X,F6.4,2X,F6.3,2
     1X,F6.4,2X,F6.2,2X,F6.2,2X,I2,2X,F7.3,2X,F6.3,2X,F6.3,2X,I2)
 1600 FORMAT(15H TOTAL WEIGHT =,F7.0,24HLBS, TOTAL UNIT WEIGHT =,F7.4,24
     1HLBS/SQFT, SURFACE AREA =,F7.0,19HSQFT, STRUCWTFRAC =,F7.5)
 1700 FORMAT(9H   WBPT =,F7.0,12HLBS, WFBPT =,F7.5)
 1800 FORMAT(9H WBPTPW =,F7.0,12HLBS, WFBTW =,F7.5)
 2300 FORMAT(101H WEIGHTS       WTO      WBOD      WWING     WPROP     W
     1TAIL      CG       RG                                )
 2400 FORMAT(13X,5(F8.0,2X),2(F7.3,2X))
 2500 FORMAT(8H NERC1 =,I2,9H, NERC2 =,I2)
 2600 FORMAT(1H )
 2700 FORMAT(101H BODY/PROP   VOLUME   DENSITY    CL1    FIN RAT  LENGTH
     1    WIDTH     ABOD     ASUR     CLP1    CLP2          )
 2800 FORMAT(13H PARAMETERS  ,F7.0,2X,F7.4,2X,F7.3,2X,F7.4,2X,2(F7.3,2X)
     1,2(F7.1,2X),2(F6.2,2X))
 3400 FORMAT(99H STRUCTURAL       CKF      FSK      EFF      CK       PG
     &          CF        CTHIC   SAFEFAC    DEFL       )
 3500 FORMAT(11H PARAMETERS,5X,F7.4,2X,F7.5,2X,F7.5,2X,F7.4,2X,F7.3,2X,E
     &       11.4,3(2X,F7.3))
 3600 FORMAT(8(2X,E11.4))
 3604 FORMAT(2X,F8.0,4(2X,F8.3))
 3700 FORMAT(27H TAIL         ATAIL    CLT           )
 3800 FORMAT(13H PARAMETERS   ,F6.0,2X,F6.2)
 3820 FORMAT(2X,'Fuselage Station Conditions',//)
 4000 FORMAT(1X,'Center of Gravity Estimation',/,
     $       3X,'Note: Locations are referenced to the nose',/,
     $       3X,'Note: These weights are only used for Loads and CG esti
     $mations',/,
     $       3X,'Fuselage CG      ',F10.2,' ft',/,
     $       3X,'  Weight         ',F10.2,' lbs',/,
     $       3X,'Wing CG          ',F10.2,' ft',/,
     $       3X,'  Weight         ',F10.2,' lbs',/,
     $       3X,'Tail CG          ',F10.2,' ft',/,
     $       3X,'  Weight         ',F10.2,' lbs',/,
     $       3X,'Propulsion CG    ',F10.2,' ft',/,
     $       3X,'  Weight         ',F10.2,' lbs',/,
     $       3X,'Landing Gear CG  ',F10.2,' ft',/,
     $       3X,'  Weight         ',F10.2,' lbs',/,
     $       1X,'Overall CG       ',2X,F10.2,' ft',/
     $       1X,'  Weight         ',2X,F10.2,' lbs',///)

      END
