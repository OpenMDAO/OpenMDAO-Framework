CSTA1
C           SUBROUTINE STA1
C     SATISFY CONTINUITY OF FLOW AT EXIT OF ALL STATORS
C                        AFTER THE FIRST STATOR
      SUBROUTINE STA1
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
      COMMON /TDIL/TWG (6,8),pwg(6,8)
      COMMON /SADDIN/ PS1P(8),PS2P(8)
C
      REAL M0
      COMMON /SSTA01/CP0(8),w0(6),               PS0(6,8),V0(6,8),TS0(6,
     18),VU0(6,8),VZ0(6,8),RHOS0(6,8),PS1(6,8),WGT1(8),TA1(8),WG1(6,8),
     2            DPDR1(6,8),SI(6,8),  CP1(8),PHI1(6,8),TS1(6,8),V1(6,8)
     3,RHOS1(6,8),ALF1E(6,8),VU1(6,8),VZ1(6,8),M0(6,8)
C
      REAL M2A,MF2A
      COMMON /SSTA2A/WG2A(6,8),WGT2A(8),VU2A(6,8),VZ2A(6,8),PS2A(6,8),
     1ALF2A(6,8),TT2A(6,8),PT2A(6,8),TTBAR(8),PTBAR(8),STT0(8),SPT0(8),
     2M2A(6,8),MF2A(6,8),CP2A(8),V2A(6,8),TS2A(6,8),TAS(8),PAS(8),GAMS(8
     1),CPS(8),DELHVD(6,8)
      COMMON/DESOPT/RVU1(6,8),RVU2(6,8),WG,EPR
      COMMON/TPT1/PT1(6),TT1(6,8)
      COMMON/SLSHFT/STRF1(6,8),STRF2(6,8),UPSI1(6)
      DIMENSION PCNF(6,8)
      EQUIVALENCE (STRF1     ,PCNF)
      COMMON RES(8),PCNFS(6),        DTT(6),DPT(6)
C
      DIMENSION WGT1C(8),LC1(8)
C
C
      J=1
      SCRIT=0.0
      PTRMO=1.
      WR=RWG(2,K)/RWG(5,K-1)
      TWGF=0.
      IF(TWG(2,K).GT.0.) TWGF=1.
      PCNFS(1)=WG2A(1,K-1)/WGT2A(K-1)/2.
      DO 1 I=1,ISECT
      PT1(I)=PT0(I,K)
      TT1(I,K)=TT0(I,K)
      if (twgf.gt.0.0) then
        ttm=(tt0(i,k)+(wr-1.)*twg(2,k))/wr
        tpp=(ttm+tt0(i,k))/2.
        tpc=(ttm+twg(2,k))/2.
        call cpa(pt0(i,k),tpc,0.0,0.0,cpc)
        call gama(pt0(i,k),tpp,fairx(5,k-1),wairx(5,k-1),gamtpp)
        extpp=gamtpp/(gamtpp-1.)
        cpp=rg2a(k-1)*extpp/aj
        tt1(i,k)=(cpp*tt0(i,k)+(wr-1.)*cpc*twg(2,k))/(cpp+(wr-1.)*cpc)
      end if
      WG1(I,K)=WR*WG2A(I,K-1)
      ALPHA0(I,K)  =ALF2A(I,K-1)
      DP0(I,K)=DP2A(I,K-1)
      PS0(I,K) =   PS2A(I,K-1)
      V0(I,K) =    V2A(I,K-1)
      TS0(I,K) =   TS2A(I,K-1)
      VU0(I,K) =   VU2A(I,K-1)
      VZ0(I,K) =   VZ2A(I,K-1)
      M0(I,K) =    M2A(I,K-1)
      IF(I.EQ.1) GO TO 1
      PCNFS(I)=PCNFS(I-1)+(WG2A(I,K-1)+WG2A(I-1,K-1))/WGT2A(K-1)/2.
1     WGT1(K)=WR*WGT2A(K-1)
      KF=ISECT-1
      CALL BESFIT(ISECT,PCNFS,TT0(1,K),KF,DTT,RES)
      ITT=KF+1
      KF=ISECT-1
      CALL BESFIT(ISECT,PCNFS,PT0(1,K),KF,DPT,RES)
      IPT=KF+1
      I=IP
      GAM(1,K)= GAM(5,K-1)
      CP0(K)= CP2A(K-1)
      ID=-1
      WGT1C(K)=0.0
      LC1(K)=0
      IF(ICHOKE)17,17,16
17    IF(LOPIN)18,18,16
18    IF(GAMF)2,2,3
2     TA1(K)=.95*TT0(IP,K)
      CALL GAMA(PT0(IP,K),TA1(K),FAIRx(2,k),WAIRx(2,k),GAM(2,K))
3     EXI=GAM(2,K)/(GAM(2,K)-1.)
      CP1(K)=rg1(k)*EXI/AJ
      VUOT=RVU1(IP,K)**2/CP1(K)/DP1(IP,K)**2/TT0(IP,K)/G/AJ*2.
      FFA1     =WG1(I,K)*SQRT(TT0(I,K))/(144.*PT0(I,K)*ANN1(I,K))
      IF(RVU1(1,K).EQ.0.) FFA1=FFA1/CSALF1(IP,K)
      CALL PRATIO(FFA1      ,GAM(2,K),rg1(k),PT0PS1(I ,K),PRTOL,
     1 ETAS(I ,K), VUOT,k)
      IF(PS1P(K).NE.0.) PT0PS1(I,K)= PT0(I,K) / PS1P(K)
16    CALL FLOW1(I)
      IF (PREVER) GO TO 23
      WGT1C(K)=WGT1C(K)+WG1(I,K)
      L=1
      IF (PT0PS1(I,K).LE.PT0PS1(IP,K)) L=I
      IF(ISECT-I)7,7,4
4     I=I+ID
      IF(I)5,5,6
5     ID=1
      I=IP+ID
6     L=I-ID
      GM2=(GAM(2,K)-1.)/2.*VU1(L,K)**2/GAM(2,K)/G/rg1(k)/TS1(L,K)
      DRSQ=(DP1(L,K)/DP1(I,K))**2
      PS1(I,K)=PS1(L,K)
     1         *(1.+GM2*(1.-DRSQ))   **(GAM(2,K)/(GAM(2,K)-1.))
      PT0PS1(I,K)=PT0(I,K)/PS1(I,K)
      GO TO 16
7     IF(LC1(K))8,8,9
8     LC1(K)=1
      EX=GAM(2,K)/(GAM(2,K)-1.)
      IF(VUOT.NE.0.)GO TO 1210
      CALL PHIM(EX,ETAS(L,K),PHIX,PRCRIT)
      PRUP= PT0PS1(IP,K)*PRCRIT/PT0PS1(L,K)
     1*(1.+PRTOL)
      GO TO 1220
1210  PRUP=1./(1.-(GAM(2,K)-1.+2.*VUOT)/(GAM(2,K)+1.)/ETAR(IP,K))**EX
1220  PRLOW=1.
      GO TO 10
9     LC1(K)=LC1(K)+1
10    L = IBRC + 1
      IF(PS1P(K).NE.0.) GO TO 40
      IF(ICHOKE.EQ.L) PT0PS1(IP,K) = PRUP
      PCNF(1,K)=WG1(1,K)/WGT1C(K)/2.
      DO 42 I=2,ISECT
42    PCNF(I,K)=(WG1(I-1,K)+WG1(I,K))/WGT1C(K)/2.+PCNF(I-1,K)
      DO 1150 I=1,ISECT
      PSIRP=1.
      TT1(I,K)=0.
      PT1(I  )=0.
      DO 1140 IFT=1,ISECT
      IF(IFT.GT.IPT) GO TO 1120
      PT1(I)  =PT1(I)  +DPT(IFT)*PSIRP
1120  IF(IFT.GT.ITT) GO TO 1140
      TT1(I,K)=TT1(I,K) +DTT(IFT)*PSIRP
1140  PSIRP=PSIRP*PCNF(I,K)
      if (twgf.gt.0.0) then
        ttm=(tt1(i,k)+(wr-1.)*twg(2,k))/wr
        tpp=(ttm+tt1(i,k))/2.
        tpc=(ttm+twg(2,k))/2.
        call cpa(pt0(i,k),tpc,0.0,0.0,cpc)
        call gama(pt0(i,k),tpp,fairx(5,k-1),wairx(5,k-1),gamtpp)
        extpp=gamtpp/(gamtpp-1.)
        cpp=rg2a(k-1)*extpp/aj
        tt1(i,k)=(cpp*tt1(i,k)+(wr-1.)*cpc*twg(2,k))/(cpp+(wr-1.)*cpc)
      end if
1150  continue
      IF(WGT1(K)-WGT1C(K))12,15,11
11    PRLOW=PT0PS1(IP,K)*.8+.2*PRLOW
      GO TO 13
12    PRUP=PT0PS1(IP,K)*.8+.2*PRUP
13    WE=1.-WGT1(K)/WGT1C(K)
      J=J+1
      IF(J-26)29,21,21
29    IF(ICHOKE-L) 30,31,30
31    SCRIT= -WE
      GO TO 15
30    IF(LOPIN)14,14,15
14    CONTINUE
      IF(PT0PS1(IP,K).LT.1.15) GO TO 71
      IF (ABS( WE)- WTOL)15,15,27
71    IF(ABS(WE)-10.*WTOL) 15,15,27
21    CONTINUE
      IF(PT0PS1(IP,K).LT.1.15) GO TO 73
      IF (ABS(WE)-WTOL)15,15,20
73    IF(ABS(WE)-10.*WTOL) 15,15,20
27    PTRMO=PT0PS1(IP,K)
      WGT1C(K)=0.0
      I=IP
      ID=-1
      IF (SCRIT)19,19,15
19    CONTINUE
      IF (PT0PS1(IP,K).LE.PRCRIT
     1.OR.RVU1(I,K).NE.0.) PRPC=0.
      PT0PS1(IP,K)=(PRUP+PRLOW)*0.5
      GO TO 16
40    DO 45 I= 1,ISECT
      COSA1E= CSALF1(I,K) * WGT1(K) / WGT1C(K)
      CSALF1(I,K) = COSA1E
45    ALF1E(I,K)= ATAN2(SQRT(1.-COSA1E*COSA1E),COSA1E)
      IF(J.GE.6) GO TO 15
      J= J+1
      WGT1C(K)=0.0
      I= IP
      ID= -1
      GO TO 16
20    SCRIT= 1.
15    IF(TRLOOP.EQ.0.) GO TO 28
      WRITE(16,1000)K,PRUP,PRLOW,WE,PRCRIT,J,WGT1(K),WGT1C(K),(WG1(L,K),
     1 L=1,ISECT)
      WRITE(16,1001)(PT0PS1(L,K),L=1,ISECT)
1000  FORMAT(2X,2HK=,I4,    2X,6H PRUP=,
     &F8.5,2X,6HPRLOW=,F8.5,2X,6H   WE=,
     1F8.5,1X,7HPRCRIT=,F8.5,2X,2HJ=,I4/
     22X,6H WGT1=,F8.3,2X,6HWGT1C=,F8.3/
     32X,6H  WG1=,6F8.3)
1001  FORMAT(1X,7HPT0PS1=,6F8.5)
  28  j=2
      GO TO (23,24),J
23    CALL DIAGT(2)
      GO TO 25
24    CALL LOOP
25    RETURN
      END
