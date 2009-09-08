CST2A-      STA2A
C           SUBROUTINE STA2A
C     DETERMINE INLET FLOW CONDITIONS TO ALL STATORS
c                     AFTER THE FIRST STATOR
      SUBROUTINE STA2A
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
C
      REAL M0
      COMMON /TDIL/TWG (6,8),pwg(6,8)
      COMMON/T1A/TT1A(6,8)
      COMMON /SSTA01/CP0(8),w0(6),               PS0(6,8),V0(6,8),TS0(6,
     18),VU0(6,8),VZ0(6,8),RHOS0(6,8),PS1(6,8),WGT1(8),TA1(8),WG1(6,8),
     2DPDR1(6,8),SI(6,8),CP1(8),PHI1(6,8),TS1(6,8),V1(6,8),
     3RHOS1(6,8),ALF1E(6,8),VU1(6,8),VZ1(6,8),M0(6,8)
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
C
      REAL M2A,MF2A
      COMMON /SSTA2A/WG2A(6,8),WGT2A(8),VU2A(6,8),VZ2A(6,8),PS2A(6,8),
     1ALF2A(6,8),TT2A(6,8),PT2A(6,8),TTBAR(8),PTBAR(8),STT0(8),SPT0(8),
     2M2A(6,8),MF2A(6,8),CP2A(8),V2A(6,8),TS2A(6,8),TAS(8),PAS(8),GAMS(8
     3),CPS(8),DELHVD(6,8)
      COMMON/DESOPT/RVU1(6,8),RVU2(6,8),WG,EPR
      COMMON/SLSHFT/STRF1(6,8),STRF2(6,8),UPSI1(6)
      COMMON DVU(6),RES(6)
C
      DIMENSION TTTS2A(6)
C
C
      KF=ISECT-1
      CALL BESFIT(ISECT,STRF1(1,K),VU1A(1,K),KF,DVU,RES)
      IVU=KF+1
      TWGF=0.
      IF(TWG(5,K).GT.0.) TWGF=1.
      ID=-1
      I=IP
      TS2A(I,K)=TS2(I,K)
      WR=RWG(5,K)/RWG(4,K)
      WR2=RWG(4,K)/RWG(3,K)-1.
      SUMT=0.0
      SUMLP=0.0
      WGT2A(K)=WR*WGT2(K)
12    VU2A(I,K)=VU2(I,K)*DP2(I,K)/DP2A(I,K)
11    IF (I-IP)28,24,28
24    IF (GAMF)25,25,26
25    TAS(K)=.5*(TA1(K)+TA2(K))
      PAS(K)=PT0(IP,K)
      CALL GAMA(PAS(K),TAS(K),FAIRx(3,k),WAIRx(3,k),GAMS(K))
      GO TO 27
26    GAMS(K)=.5*(GAM(2,K)+GAM(4,K))
27    E4=GAMS(K)/(GAMS(K)-1.)
      CPS(K)=(rg1a(k)+rg2(k))/2.*E4/AJ
28    VUPSI=0.
      PSIRP=1.
      DO 210 IFT=1,IVU
      VUPSI=VUPSI+DVU(IFT)*PSIRP
210   PSIRP=PSIRP*STRF2(I,K)
      DELHVD(I,K)=(UPSI1(I)*VUPSI+(WR2+1.)*U2(I,K)*VU2(I,K))/AJ/G
c  ** DELHVD is per unit of primary flow entering the rotor **
      TT2=TTr2(I,K)-(r2(i,k)**2-v2(i,k)**2)/(2.*g*aj*cp2(k))
      TT2A(I,K)=TT2
      if (twgf.gt.0.0) then
        ttm=(tt2+(wr-1.)*twg(5,k))/wr
        tpp=(ttm+tt2)/2.
        tpc=(ttm+twg(5,k))/2.
        call cpa(pt0(ip,k),tpc,0.0,0.0,cpc)
        call gama(pt0(ip,k),tpp,fairx(4,k),wairx(4,k),gamtpp)
        extpp=gamtpp/(gamtpp-1.)
        cpp=rg2(k)*extpp/aj
        tt2a(i,k)=(cpp*tt2+(wr-1.)*cpc*twg(5,k))/(cpp+(wr-1.)*cpc)
      end if
      WG2A(I,K)=WR*WG2(I,K)
      RHOSTR=RHOS2(I,K)
1     VZ2A(I,K)=WR*VZ2(I,K)*ANN2(I,K)*RHOS2(I,K)/(ANN2A(I,K)*RHOSTR)
      V2A(I,K)=SQRT(VU2A(I,K)*VU2A(I,K)+VZ2A(I,K)*VZ2A(I,K))
      IF(I-IP)4,2,4
2     IF(    GAMF)3,3,4
3     TA2A=.5*(TT2A(I,K)+TS2A(I,K))
      CALL GAMA(PTR2(IP,K),TA2A   ,FAIRx(5,k),WAIRx(5,k),GAM(5,K))
4     EX=(GAM(5,K)-1.)/GAM(5,K)
      EXI=1./EX
      CP2A(K)=rg2a(k)*EXI/AJ
      DELTS=(V2(I,K)*V2(I,K)-V2A(I,K)*V2A(I,K))/(2.*G*AJ*CP2A(K))
      TS2A(I,K)=TS2(I,K)+DELTS+TT2A(I,K)-TT2
      IF(TS2A(I,K).GT.0.) GO TO 32
      PREVER = .TRUE.
      MFSTOP = 2.
      GO TO 30
32    PS2A(I,K)=PS2(I,K)*(TT2*TS2A(I,K)/TT2A(I,K)/TS2(I,K))**EXI
      RHOS2A     =144.*PS2A(I,K)/(rg2a(k)*TS2A(I,K))
      IF(ABS(RHOSTR-RHOS2A     )-1.E-07)6,6,5
5     RHOSTR=RHOS2A
      GO TO 1
6     SALF2A     =VU2A(I,K)/V2A(I,K)
      ALF2A(I,K)=ATAN2(SALF2A     ,SQRT(1.-SALF2A     *SALF2A     ))
      IF(RVU1(I,K).NE.0..AND.K.LT.KSTG)RADSD(I,K+1)=ALF2A(I,K)
      M2A(I,K)=V2A(I,K)/SQRT(GAM(5,K)*G*rg2a(k)*TS2A(I,K))
      TTTS2A(I)=  1.+(M2A(I,K)*M2A(I,K)*(GAM(5,K)-1.)/2.)
      PTPS2A     = (TTTS2A(I)  )**EXI
      PT2A(I,K)=PS2A(I,K)*PTPS2A
      MF2A(I,K)=M2A(I,K)*COS(ALF2A(I,K))
      IF (ISECT-I)13,15,13
13    I=I+ID
      IF (I)14,14,12
14    ID=1
      I=IP+ID
      GO TO 12
15    CONTINUE
      DO 16 I=1,ISECT
      RW=WG2A(I,K)/WGT2A(K)
      TR=TT2A(I,K)/TT2A(IP,K)
      PR=PT2A(I,K)/PT2A(IP,K)
      SUMT=SUMT+RW*TR
16    SUMLP=SUMLP+RW*ALOG(PR)
      E3=GAM(5,K)/(GAM(5,K)-1.)
      TTBAR(K)=TT2A(IP,K)*SUMT
      PTBAR(K)=PT2A(IP,K)*EXP(SUMLP)
      IF (K-KSTG)17,18,18
17    STT0(K+1)=TTBAR(K)
      SPT0(K+1)=PTBAR(K)
      DO 23 I=1,ISECT
29    SI(I,K+1)=ALF2A(I,K)- RADSD(I,K+1)
      IF(SI(I,K+1).GT. 1.5707) SI(I,K+1)= 1.5707
      IF(SI(I,K+1).LT.(-1.5707)) SI(I,K+1)=(-1.5707)
      IF(OMEGAS(I,K))8,8,7
7     ETARS(I,K+1)=1.0
      EXPSI=0.
      GO TO 117
8     IF(SI(I,K+1))9,9,10
9     EXPSI=EXPN
      GO TO 117
10    EXPSI=EXPP
117   IF (PAF-1.)19,20,21
C     UNIFORM PROFILES
19    PTP(I,K+1)=PTBAR(K)
      PT0(I,K+1)= PTP(I,K+1)
     1*(1.+(TTTS2A(I)  -1.)*ETARS(I,K+1)*(COS(SI(I,K+1))**EXPSI))**EXI
     2/(TTTS2A(I ) )**EXI
      TT0(I,K+1)=TTBAR(K)
      GO TO 23
C     SAVE PROFILES
20    PTP(I,K+1)=PT2A(I,K)
      PT0(I,K+1)= PTP(I,K+1)
     1*(1.+(TTTS2A(I ) -1.)*ETARS(I,K+1)*(COS(SI(I,K+1))**EXPSI))**EXI
     2/(TTTS2A(I ) )**EXI
      GO TO 22
C     SMOOTH PRESSURE PROFILES
21    PTP(I,K+1)=PTBAR(K)*(1./SUMT)**E3
      PT0(I,K+1)= PTP(I,K+1)
     1*(1.+(TTTS2A(I ) -1.)*ETARS(I,K+1)*(COS(SI(I,K+1))**EXPSI))**EXI
     2/(TTTS2A(I ) )**EXI
22    TT0(I,K+1)=TT2A(I,K)
23    CONTINUE
18    MFSTOP=MF2A(isect,K)/AACS
      j=2
      GO TO (30,31),J
30    CALL DIAGT(5)
31    RETURN
      END
