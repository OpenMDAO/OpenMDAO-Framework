CSTA2
C           SUBROUTINE STA2
C     SATISFY CONTINUITY OF FLOW AT ROTOR EXIT
      SUBROUTINE STA2
C
      REAL MFSTOP
      LOGICAL PREVER
      COMMON /SNTCP/G,AJ,PRPC,ICASE,PREVER,MFSTOP,JUMP,LOPIN,ISCASE,
     1 K,GAMF,IP,SCRIT,PTRN,ISECT,KSTG,WTOL,RHOTOL,PRTOL,TRLOOP,LSTG,
     2LBRC,IBRC,ICHOKE,ISORR,CHOKE,PT0PS1(6,8),PTRS2(6,8),TRDIAG,SC,RC,
     3DELPR,PASS,IPC,LOPC,ISS
C
      COMMON /SINIT/H1(6,8),H2(6,8),DP0(6,8),DP1(6,8),DP1A(6,8),DP2(6,8)
     1,DP2A(6,8),CSALF1(6,8),ALF1(6,8),CSBET2(6,8),BET2(6,8),RADSD(6,8),
     2RADRD(6,8),ANN1(6,8),ANN2(6,8),ANN2A(6,8),ANN1A(6,8),U1A(6,8),
     3U2(6,8),ANN0(6,8),PT0(6,8),TT0(6,8),ALPHA0(6,8),PTP(6,8)
C
      COMMON /SINPUT/
     1PTPS,PTIN,TTIN,WAIR,FAIR,DELC,DELL,DELA,AACS,VCTD,STG,SECT,EXPN,
     2EXPP,EXPRE,RG,RPM,PAF,SLI,STGCH,ENDJOB,XNAME(20),TITLE(20),
     3PCNH(6),GAM(6,8),DR(6,8),DT(6,8),RWG(6,8),ALPHAS(6,8),ALPHA1(6,8),
     4ETARS(6,8),ETAS(6,8),CFS(6,8),ANDO(6,8),BETA1(6,8),BETA2(6,8),ETAR
     5R(6,8),ETAR(6,8),CFR(6,8),TFR(6,8),ANDOR(6,8),OMEGAS(6,8),AS0(6,8)
     6,ASMP0(6,8),ACMN0(6,8),A1(6,8),A2(6,8),A3(6,8),A4(6,8),A5(6,8),A6(
     76,8),OMEGAR(6,8),BSIA(6,8),BSMPIA(6,8),BCMNIA(6,8),B1(6,8),B2(6,8)
     8,B3(6,8),B4(6,8),B5(6,8),B6(6,8),SESTHI(8),RERTHI(8)
     9,fairx(5,8),wairx(5,8),rg1(8),rg1a(8),rg2(8),rg2a(8)
      COMMON /SADDIN/ PS1P(8),PS2P(8)
C
      REAL MR1A
      COMMON /SSTA1A/VU1A(6,8),WG1A(6,8),WGT1A(8),VZ1A(6,8),  CP1A(8),
     1PS1A(6,8),RU1A(6,8),R1A(6,8),BET1A(6,8),RI(6,8),TTR1A(6,8),PTR1A(6
     2,8),MR1A(6,8)
C
      COMMON /SSTA2/V2(6,8),TTR2(6,8),PTR2(6,8),WG2(6,8),WGT2(8),TA2(8),
     1           PS2(6,8),PHI2(6,8)
C
      REAL MR2,M2     ,MF2
      COMMON /SFLOW2/TS2(6,8),CP2(8),R2(6,8),RHOS2(6,8),BET2E(6,8),RU2(6
     1,8),VU2(6,8),DPDR2(6,8),VZ2(6,8),MR2(6,8),MF2(6,8),M2(6,8)
      COMMON/RPMCOM/RPMK(8)
      COMMON /TDIL/TWG (6,8),pwg(6,8)
C     COMMON RES(6),PCNFS(6),        DTT(6),DPT(6),TR2M1(6)
      COMMON RES(8),PCNFS(6),        DTT(6),DPT(6),TR2M1(6)
      COMMON/DESOPT/RVU1(6,8),RVU2(6,8),WG,EPR
      COMMON/SLSHFT/STRF1(6,8),STRF2(6,8),UPSI1(6)
      EQUIVALENCE (STRF2     ,PCNF)
C
C
      DIMENSION WGT2C(8),PCNF(6,8),IS2(8)
C
      PCNFS(1)=WG1A(1,K)/WGT1A(K)/2.
      PTRN=0.
      J=1
      TGJCP=2.*G*AJ*CP1A(K)
      TWGF=0.
      IF(TWG(4,K).GT.0.) TWGF=1.
      SCRIT=0.0
      PTRMO=1.
      IS2(K)=0
      RUOT=0.
      EXI=GAM(3,K)/(GAM(3,K)-1.)
      WR=RWG(4,K)/RWG(3,K)
      DO 1 I=1,ISECT
      IF(PCNF(I,K).GT.0.) GO TO 1160
      TTR2(I,K)=TTR1A(I,K)+(U2(I,K)**2 - U1A(I,K)**2)/TGJCP
      PTR2(I,K)=PTR1A(I,K)*(TTR2(I,K)/TTR1A(I,K))**EXI
      if (twgf.gt.0.1) then
        twgr=twg(4,k)+u2(i,k)**2/tgjcp
        ttm=(ttr2(i,k)+(wr-1.)*twgr)/wr
        tpp=(ttm+ttr2(i,k))/2.
        tpc=(ttm+twgr)/2.
        call cpa(ptr1a(i,k),tpc,0.0,0.0,cpc)
        call gama(ptr1a(i,k),tpp,fairx(3,k),wairx(3,k),gamtpp)
        extpp=gamtpp/(gamtpp-1.)
        cpp=rg1a(k)*extpp/aj
        ttr2(i,k)=(cpp*ttr2(i,k)+(wr-1.)*cpc*twgr)/(cpp+(wr-1.)*cpc)
      end if
1160  IF(I.EQ.1) GO TO 1
      PCNFS(I)=PCNFS(I-1)+(WG1A(I-1,K)+WG1A(I,K))/2./WGT1A(K)
1     WG2(I,K)=WR*WG1A(I,K)
      KF=ISECT-1
      CALL BESFIT(ISECT,PCNFS,U1A(1,K),KF,TR2M1,RES)
      IU=KF+1
      KF=ISECT-1
      CALL BESFIT(ISECT,PCNFS,PTR1A(1,K),KF,DPT,RES)
      IPT=KF+1
      KF=ISECT-1
      CALL BESFIT(ISECT,PCNFS,TTR1A(1,K),KF,DTT,RES)
      ITT=KF+1
      WGT2(K)=WR*WGT1A(K)
      I=IP
      ID=-1
      WGT2C(K)=0.
C     IF(ICHOKE)26,26,3
C     replced by ..............
      IF(ICHOKE.LE.0) THEN 
         GO TO 26
      ELSE
         GO TO 3
      ENDIF
C26    IF(LOPIN)27,27,3
C     replced by ..............
26    IF(LOPIN.LE.0) THEN 
         GO TO 27
      ELSE
         GO TO 3
      ENDIF
C27    IF(GAMF)2,2,16
C     replced by ..............
27    IF(GAMF.LE.0.) THEN 
         GO TO 2
      ELSE
         GO TO 16
      ENDIF
2     TA2(K)=.95*TTR2(IP,K)
      CALL GAMA(PTR2(I,K),TA2(K),FAIRx(4,k),WAIRx(4,k),GAM(4,K))
16    CP2(K)=rg2(k)*EXI/AJ
      IF(RVU1(IP,K).NE.0.)
     *RUOT=(RVU2(IP,K)/DP2(IP,K)+.5*U2(IP,K))**2/CP2(K)/TTR2(IP,K)/G/AJ
     1*2.
      FFA2     =WG2(I,K)*SQRT(TTR2(I,K))/(144.*PTR2(I,K)*
     1ANN2(I,K))
      IF(RVU1(I,K).EQ.0.)FFA2=FFA2/CSBET2(I,K)
      CALL PRATIO(FFA2      ,GAM(4,K),rg2(k),PTRS2 (I ,K),PRTOL,
     1 ETAR(I ,K),RUOT,k)
      IF(PS2P(K).NE.0.) PTRS2(I,K)= PTR2(I,K) / PS2P(K)
      IF(PCNF(1,K).LE.0.) GO TO 1103
3     DO 1150 I=1,ISECT
      TTR2IK=0.
      PTR2(I,K)=0.
      UPSI1(I)=0.
      PSIRP=1.
      DO 1140 IFT=1,ISECT
      IF(IFT.GT.IU) GO TO 1130
      UPSI1(I)=UPSI1(I)+TR2M1(IFT)*PSIRP
1130  IF(IFT.GT.IPT) GO TO 1120
      PTR2(I,K)=PTR2(I,K)+DPT(IFT)*PSIRP
1120  IF(IFT.GT.ITT) GO TO 1140
      TTR2 I K =TTR2 I K +DTT(IFT)*PSIRP
1140  PSIRP=PSIRP*PCNF(I,K)
      TTR2(I,K)= TTR2IK+(U2(I,K)**2-UPSI1(I)**2)/TGJCP
      PTR2(I,K)=PTR2(I,K)*(TTR2(I,K)/TTR2IK)**EXI
      if (twgf.gt.0.1) then
        twgr=twg(4,k)+u2(i,k)**2/tgjcp
        ttm=(ttr2(i,k)+(wr-1.)*twgr)/wr
        tpp=(ttm+ttr2(i,k))/2.
        tpc=(ttm+twgr)/2.
        call cpa(ptr1a(i,k),tpc,0.0,0.0,cpc)
        call gama(ptr1a(i,k),tpp,fairx(3,k),wairx(3,k),gamtpp)
        extpp=gamtpp/(gamtpp-1.)
        cpp=rg1a(k)*extpp/aj
        ttr2(i,k)=(cpp*ttr2(i,k)+(wr-1.)*cpc*twgr)/(cpp+(wr-1.)*cpc)
      end if
1150  continue
      I=IP
1103  CALL FLOW2(I)
      IF (PREVER) GO TO 22
      WGT2C(K)=WGT2C(K)+WG2(I,K)
      L=1
      IF (PTRS2(I,K).LE.PTRS2(IP,K))   L=I
C     IF(ISECT-I)7,7,4
C     replaced by ..........
      IF((ISECT-I).LE.0) THEN 
         GO TO 7
      ELSE
         GO TO 4
      ENDIF
4     I=I+ID
C     IF(I)5,5,6
C     replaced by ..........
      IF(I.LE.0) THEN 
         GO TO 5
      ELSE
         GO TO 6
      ENDIF
5     ID=1
      I=IP+ID
6     L=I-ID
      GM2=(GAM(4,K)-1.)/2.*VU2(L,K)**2/GAM(4,K)/G/rg2(k)/TS2(L,K)
      DRSQ=(DP2(L,K)/DP2(I,K))**2
      PS2(I,K)=PS2(L,K)
     1         *(1.+GM2*(1.-DRSQ))   **(GAM(4,K)/(GAM(4,K)-1.))
      PTRS2(I,K)=PTR2(I,K)/PS2(I,K)
C     IF (PTRS2(I,K)-1.)19,19,1103
C     replaced by ..........
      IF (PTRS2(I,K)-1..LE.0) THEN 
         GO TO 19  
      ELSE
         GO TO 1103
      ENDIF
C      REVERSE FLOW INDICATION
19    PTRS2(I,K) = 1.0  + PRTOL
      PTRN=-1.
      GO TO 1103
C7     IF(IS2(K))8,8,9
C     replaced by ..........
7     IF(IS2(K).LE.0) THEN 
         GO TO 8   
      ELSE
         GO TO 9
      ENDIF
8     EXI=GAM(4,K)/(GAM(4,K)-1.)
      IF(RUOT.NE.0.)GO TO 1210
      CALL PHIM(EXI,ETAR(L,K),PHIX,PRCRIT)
      PRUP=PTR2(IP,K)*PRCRIT*PS2(L,K)/(PTR2(L,K)*PS2(IP,K))
     1*(1.+PRTOL)
      GO TO 1220
1210  PRUP=1./(1.-(GAM(4,K)-1.+2.*RUOT)/(GAM(4,K)+1.)/ETAR(IP,K))**EXI
1220  PRLOW=1.
9     IS2(K)=IS2(K)+1
      L = IBRC + 1
      IF(PS2P(K).NE.0.) GO TO 40
      IF(ICHOKE.EQ.L) PTRS2(IP,K) = PRUP
      PCNF (1,K)=WG2(1,K)/WGT2C(K)/2.
      DO 50 I=2,ISECT
50    PCNF(I,K)=(WG2(I-1,K)+WG2(I,K))/2./WGT2C(K)+PCNF (I-1,K)
C     IF(WGT2(K)-WGT2C(K))12,15,11
C     replaced by ..........
      IF(WGT2(K)-WGT2C(K).LT.0.) THEN 
         GO TO 12
      ELSEIF (WGT2(K)-WGT2C(K).EQ.0.) THEN
         GO TO 15
      ELSEIF (WGT2(K)-WGT2C(K).GT.0.) THEN
         GO TO 11
      ENDIF
C
11    PRLOW= PTRS2(IP,K)*.8+.2*PRLOW
      GO TO 13
12    PRUP= PTRS2(IP,K)*.8+.2*PRUP
      IS2(K)=1
13    WE=1.-WGT2(K)/WGT2C(K)
      J=J+1
C     IF(J-26)29,17,17
C     replaced by ..........
      IF(J-26.LT.0) THEN 
         GO TO 29  
      ELSE
         GO TO 17
      ENDIF
C29    IF(ICHOKE-L) 30,31,30
C     replaced by ..........
29    IF((ICHOKE-L).NE.0) THEN 
         GO TO 30  
      ELSE
         GO TO 31
      ENDIF
31    SCRIT= -WE
      GO TO 15
C30    IF(LOPIN)14,14,15
C     replaced by ..........
30    IF(LOPIN.LE.0) THEN 
         GO TO 14  
      ELSE
         GO TO 15
      ENDIF
14    CONTINUE
C                                              
      IF(PTRS2(IP,K).LT.1.15) GO TO 71
C     IF (ABS( WE)- WTOL)15,15,24
C     replaced by ..........
      IF ((ABS( WE)- WTOL).LE.0.)  THEN  
         GO TO 15  
      ELSE
         GO TO 24
      ENDIF
C71    IF(ABS(WE)-10.*WTOL) 15,15,24
C     replaced by ..........
71    IF((ABS(WE)-10.*WTOL).LE.0.) THEN 
         GO TO 15  
      ELSE
         GO TO 24
      ENDIF
17    CONTINUE
      IF(PTRN.LT.0.) GO TO 18
      IF(WE.GT.10.*WTOL) GO TO 18
      IF(PTRS2(IP,K).GE.1.15.AND.WE.GT.WTOL) GO TO 18
      IF(PTRS2(IP,K).LT.1.15) GO TO 73
C     IF (ABS(WE)-WTOL)15,15,23
C     replaced by ..........
      IF ((ABS(WE)- WTOL).LE.0.)  THEN  
         GO TO 15  
      ELSE
         GO TO 23
      ENDIF
C73    IF(ABS(WE)-10.*WTOL) 15,15,23
C     replaced by ..........
73    IF((ABS(WE)-10.*WTOL).LE.0.) THEN 
         GO TO 15  
      ELSE
         GO TO 23
      ENDIF
24    PTRMO=PTRS2(IP,K)
      I=IP
      ID=-1
C     IF (SCRIT)28,28,15
C     replaced by .........
      IF (SCRIT.GT.0.) GO TO 15       
28    CONTINUE
      IF (PTRS2(IP,K).LE.PRCRIT
     1.OR.RVU1(I,K).NE.0.) PRPC=0.
      WGT2C(K)=0.0
      PTRS2 (IP,K)=(PRUP+PRLOW)*0.5
      GO TO 3
40    DO 45 I= 1,ISECT
      COSB2E= CSBET2(I,K) * WGT2(K) / WGT2C(K)
      CSBET2(I,K) = COSB2E
45    BET2E(I,K)= ATAN2(SQRT(1.-COSB2E*COSB2E),COSB2E)
      IF(J.GE.6) GO TO 15
      J= J+1
      I= IP
      WGT2C(K)=0.0
      ID= -1
      GO TO 3
23    SCRIT= 1.
15    IF(TRLOOP.EQ.0.) GO TO 25
18    WRITE(16,1000)K,PRUP,PRLOW,WE,PRCRIT,J,WGT2(K),WGT2C(K),(WG2(L,K),
     1 L=1,ISECT)
      WRITE(16,1001)(PTRS2(L,K),L=1,ISECT)
1000  FORMAT(2X,2HK=,I4,    2X,6H PRUP=,
     &F8.5,2X,6HPRLOW=,F8.5,2X,6H   WE=,
     1F8.5,1X,7HPRCRIT=,F8.5,2X,2HJ=,I4/
     22X,6H WGT2=,F8.3,2X,6HWGT2C=,F8.3/
     32X,6H  WG2=,6F8.3)
1001  FORMAT(2X,6HPTPS2=,6F8.5)
25    j=2
      GO TO (20,21),J
20    CALL DIAGT(4)
      GO TO 22
21    CALL LOOP
22    RETURN
      END
