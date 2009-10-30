COVRALL
C
C     added call tempdata( )  to get value of Power (OHP)
C     10/20/2009
C
      SUBROUTINE OVRALL
C     PURPOSE IS TO CALCULATE STAGE PERFORMANCE VALUES
C     AFTER FLOW ITERATION IS COMPLETED THROUGH THE LAST STAGE
C
      REAL MFSTOP
      LOGICAL PREVER
      COMMON /SNTCP/G,AJ,PRPC,ICASE,PREVER,MFSTOP,JUMP,LOPIN,ISCASE,
     1KN,GAMF,IP,SCRIT,PTRN,ISECT,KSTG,WTOL,RHOTOL,PRTOL,TRLOOP,LSTG,
     2LBRC,IBRC,ICHOKE,ISORR,CHOKE,PT0PS1(6,8),PTRS2(6,8),TRDIAG,SC,RC,
     3DELPR,PASS,IPC,LOPC,ISS
C
      COMMON/RPMCOM/RPMK(8)
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
C
      REAL M0
      COMMON /SSTA01/CP0(8),w0(6),               PS0(6,8),V0(6,8),TS0(6,
     18),VU0(6,8),VZ0(6,8),RHOS0(6,8),PS1(6,8),WGT1(8),TA1(8),WG1(6,8),
     2            DPDR1(6,8),SI(6,8),  CP1(8),PHI1(6,8),TS1(6,8),V1(6,8)
     3,RHOS1(6,8),ALF1E(6,8),VU1(6,8),VZ1(6,8),M0(6,8)
C
      REAL MR1A
      COMMON /SSTA1A/VU1A(6,8),WG1A(6,8),WGT1A(8),VZ1A(6,8),  CP1A(8),
     1PS1A(6,8),RU1A(6,8),R1A(6,8),BET1A(6,8),RI(6,8),TTR1A(6,8),PTR1A(6
     2,8),MR1A(6,8)
      COMMON/T1A/TT1A(6,8)
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
C
      COMMON/GETPR/TRY1,TRY2,PTRY1,PTRY2,PFIND,DHFIND,PFIND1,
     1DHFND1
      COMMON /SOVRAL/DELHT(6,8),DELHTI(6,8),DELHSI(6,8),DEHATI(6,8),
     1ETATT(6,8),ETATS(6,8),ETATAT(6,8),delhtu(6,8)
      COMMON/SLSHFT/STRF1(6,8),STRF2(6,8),UPSI1(6)
      COMMON /TDIL/TWG(6,8),pwg(6,8)
C
      REAL MIS(8),MIRS(8),MR1AR(8),MR2T(8)
      DIMENSION SA0(8),SIS(8),SB1A(8),SIR(8),SA2(8),THCR(8),EPSI(8),DELT
     1(8),SETATT(8),SETATS(8),SETAAT(8),SWRTP(8),SNRT(8),SDHT(8),SETHC(8
     2),SNRTHC(8),SWRTED(8),SPTPT2(8),SPTPS2(8),ST2TT0(8),STRTT0(8),UPS(
     38),UPUPS(8),URS(8),URURS(8),VIS(8),UPVIS(8),URVIS(8),PSIPS(8),PSIR
     4S(8),RXP(8),RXR(8),DBETAR(8),DELHTS(8),DEHTIS(8),DEHSIS(8),DHATIS(
     58),PAT2A(6,8),WGT0(8),HP(8),TORQUE(8),rx2p(8),rx2r(8)
      COMMON DPT(6),DTT(6),RES(6),PCNFS(6)
      COMMON /PLOTT/ ENDPLT,IPCAS,IPSCAS(20),xnname(20),iplot(3)
      common /pltdat/ speed(20),prt(250,20),efft(250,20),flow(250,20)
C
      DATA RSL,TSL,PSL,GAMSL/53.35045,518.688,14.696,1.4/
C
C
      STT0(1)=TTIN
      SPT0(1)=PTIN
      TAO=0.0
      PAO=0.0
      GAMO=0.0
      OUP = 0.0
      OUPUP=0.0
      OURUR=0.0
      OHP=0.
      OTORQ=0.
      ODELHT=0.0
      ODEHTI = 0.
      ODEHSI = 0.
      ODHATI = 0.
5     E1=GAMSL/(GAMSL-1.)
      DO 17 K=1,KSTG
      IF (GAMF)1,1,2
1     TAO=TAO+TAS(K)
      PAO=PAO+PAS(K)
2     GAMO=GAMO+GAMS(K)
3     E2=GAM(1,K)/(GAM(1,K)-1.)
      E3=GAM(5,K)/(GAM(5,K)-1.)
      E4=GAMS(K)/(GAMS(K)-1.)
      E5=1./E4
      IF(K.GT.1)GO TO 100
      PCNFS(1)=PCNH(1)/2.
      DO 2010 I=2,ISECT
2010  PCNFS(I)=PCNFS(I-1)+(PCNH(I)+PCNH(I-1))/2.
      KF=ISECT-1
      CALL BESFIT(ISECT,PCNFS,PTP(1,1),KF,DPT,RES)
      IPT=KF+1
      KF=ISECT-1
      CALL BESFIT(ISECT,PCNFS,TT0(1,1),KF,DTT,RES)
      GO TO 110
100   CALL BESFIT(ISECT,STRF2(1,K-1 ),PTP(1,K),KF,DPT,RES)
      IPT=KF+1
      KF=ISECT-1
      CALL BESFIT(ISECT,STRF2(1,K-1 ),TT0(1,K),KF,DTT,RES)
110   ITT=KF+1
      DELHTS(K)=0.0
      DEHTIS(K)=0.0
      DEHSIS(K)=0.0
      DHATIS(K)=0.0
      DO 6 I=1,ISECT
      RW=WG2A(I,K)/WGT2A(K)
      wr2=rwg(4,k)/rwg(3,k)-1.
      DELHT(I,K)=DELHVD(I,K)*TFR(I,K)
      PSIRP=1.
      TTPSI=0.
      PTPSI=0.
      DO 120 IFT=1,ISECT
      IF(IFT.GT.IPT)GO TO 130
      PTPSI=PTPSI+DPT(IFT)*PSIRP
130   IF(IFT.GT.ITT)GO TO 120
      TTPSI=TTPSI+DTT(IFT)*PSIRP
120   PSIRP=PSIRP*STRF2(I,K)
      DELHTI(I,K)=CPS(K)*TTPSI   *(1.-(PT2A(I,K)/PTPSI   )**E5)
      ETATT(I,K)=DELHT(I,K)/DELHTI(I,K)
      DELHSI(I,K)=CPS(K)*TTPSI   *(1.-(PS2A(I,K)/PTPSI   )**E5)
      ETATS(I,K)=DELHT(I,K)/DELHSI(I,K)
      PAT2A(I,K)=PS2A(I,K)*(1.+(GAM(5,K)-1.)*MF2A(I,K)*MF2A(I,K)
     1/2.)**E3
      DEHATI(I,K)=CPS(K)*TTPSI   *(1.-(PAT2A(I,K)/PTPSI   )**E5)
      ETATAT(I,K)=DELHT(I,K)/DEHATI(I,K)
      delhtu(i,k)=delht(i,k)
      DELHT(I,K)=DELHT(I,K)-wr2*u2(i,k)**2/g/aj
      DELHTS(K)=DELHTS(K)+RW*DELHT(I,K)
      DEHTIS(K)=DEHTIS(K)+RW*DELHTI(I,K)
      DEHSIS(K)=DEHSIS(K)+RW*DELHSI(I,K)
      DHATIS(K)=DHATIS(K)+RW*DEHATI(I,K)
6     CONTINUE
13    SA0(K)=ALPHA0(IP,K)*57.2958
      SIS(K)=SI(IP,K)*57.2958
      SB1A(K)=BET1A(IP,K)*57.2958
      SIR(K)=RI(IP,K)*57.2958
      SA2(K)=ALF2A(IP,K)*57.2958
      dhsisu=dehsis(k)
      rg0=rg
      if (k.gt.1) rg0=rg2a(k-1)
      THCR(K)= GAM(1,K)*(GAMSL+1.)*RG0*STT0(K)/
     1(GAMSL*(GAM(1,K)+1.)*RSL*TSL)
      EPSI(K)=GAMSL*((GAM(1,K)+1.)/2.)**E2/(GAM(1,K)*((GAMSL
     1+1.)/2.)**E1)
      DELT(K)=SPT0(K)/PSL
      WGT0(K)= WGT1(K)/RWG(2,K)*rwg(1,k)
      do 140 ic=2,4
        wrc=rwg(ic,k)/rwg(ic-1,k)
        if (wrc.gt.1.0) then
          call cpa(ptpsi,twg(ic,k),0.0,0.0,cpc)
          call gama(ptpsi,twg(ic,k),0.0,0.0,gamc)
          cpcav=(cpc+cp2a(k))/2.
          gamav=(gamc+gam(5,k))/2.
          excav=(gamav-1.)/gamav
          fprcav=1.-(ptbar(k)/pwg(ic,k))**excav
          wcowp=(rwg(ic,k)/rwg(ic-1,k)-1.)*rwg(ic-1,k)/rwg(1,k)
          dehtis(k)=dehtis(k)+wcowp*cpcav*twg(ic,k)*fprcav
          fprsav=1.-(ps2a(ip,k)/pwg(ic,k))**excav
          dehsis(k)=dehsis(k)+wcowp*cpcav*twg(ic,k)*fprsav
        end if
  140 continue
      delhts(k)=rwg(3,k)/rwg(1,k)*delhts(k)
      SETATT(K)=DELHTS(K)/DEHTIS(K)
      SETATS(K)=DELHTS(K)/DEHSIS(K)
      SETAAT(K)=DELHTS(K)/DHATIS(K)
      SWRTP(K)=WGT0(K)*SQRT(STT0(K))/SPT0(K)
      SNRT(K)=RPMK(K)/SQRT(STT0(K))
      SDHT(K)=DELHTS(K)/STT0(K)
      SETHC(K)=DELHTS(K)/THCR(K)
      RTHCR=SQRT(THCR(K))
      SNRTHC(K)=RPMK(K)/RTHCR
      SWRTED(K)=WGT0(K)*RTHCR*EPSI(K)/DELT(K)
      SPTPT2(K)=SPT0(K)/PTBAR(K)
      SPTPS2(K)=SPT0(K)/PS2(IP,K)
      ST2TT0(K)=TTBAR(K)/STT0(K)
      STRTT0(K)=TTR1A(IP,K)/STT0(K)
      UPS(K)=(DR(3,K)+DR(4,K)+DT(3,K)+DT(4,K))*RPMK(K)*.00109083
      OUP = OUP + UPS(K)
      UPUPS(K)=UPS(K)*UPS(K)
      OUPUP=OUPUP+UPUPS(K)
      URS(K)=.5*(U1A(1,K)*DR(3,K)/DP1A(1,K)+U2(1,K)*DR(4,K)/DP2(1,K))
      URURS(K)=URS(K)*URS(K)
      OURUR=OURUR+URURS(K)
      ODELHT=ODELHT+DELHTS(K)*rwg(1,k)
      IF (DELHSI(IP,K))14,14,15
14    VIS(K)=1.
      GO TO 16
15    VIS(K)=SQRT(2.*G*AJ*DHSIsu)
16    UPVIS(K)=UPS(K)/VIS(K)
      URVIS(K)=URS(K)/VIS(K)
      PSIPS(K)=G*AJ*DELHTS(K)/UPUPS(K)
      PSIRS(K)=G*AJ*DELHTS(K)/URURS(K)
      RXP(K)=1.-(1.-(PS1(IP,K)/PTP(IP,K))**E5)/(1.-(PS2(IP,K)/
     1PTP(IP,K))**E5)
      V1Apsq=VU1A(ip,k)**2+VZ1A(ip,K)**2
      num=u1a(ip,k)**2-u2(ip,k)**2+r2(ip,k)**2-r1a(ip,k)**2
      rx2p(k)=num/(num+v1apsq-v2(ip,k)**2)
      VU1R=VU1(1,K)*DP1(1,K)/DR(2,K)
      V1R=SQRT(VU1R**2+VZ1(1,K)**2)
      PH1R=1./(1.-V1R**2/(2.*G*AJ*CP1(K)*TT0(1,K)*ETAS(1,K)))
      PTPS1R=PH1R**(GAM(2,K)/(GAM(2,K)-1.))*PTP(1,K)/PT0(1,K)
      RXR(K)=1.-(1.-(1./PTPS1R)**E5)/(1.-(PS2(1,K)/PTP(1,K))**E5)
      DBETAR(K)=(BET1A(1,K)+BET2E(1,K))*57.2958
      MIS(K)=V1(IP,K)/SQRT(GAM(2,K)*G*rg1(k)*TS1(IP,K))
      TS1R=TT0(1,K)-V1R**2/(2.*G*AJ*CP1(K))
      MIRS(K)=V1R/SQRT(GAM(2,K)*G*rg1(k)*TS1R)
      VU1AR=VU1A(1,K)*DP1A(1,K)/DR(3,K)
      V1AR=SQRT(VU1AR**2+VZ1A(1,K)**2)
      TS1AR=TT0(1,K)-V1AR**2/(2.*G*AJ*CP1A(K))
      RU1AR=VU1AR-U1A(1,K)*DR(3,K)/DP1A(1,K)
      U1AR=U1A(1,K)*DR(3,K)/DP1A(1,K)
      R1AR=SQRT(RU1AR**2+VZ1A(1,K)**2)
      MR1AR(K)=R1AR/SQRT(GAM(3,K)*G*rg1a(k)*TS1AR)
      VU2T=VU2(ISECT,K)*DP2(ISECT,K)/DT(4,K)
      V2T=SQRT(VU2T**2+VZ2(ISECT,K)**2)
      TS2T=TS2(ISECT,K)+(V2(ISECT,K)**2-V2T**2)/(2.*G*AJ*CP2(K))
      RU2T=VU2T+U2(ISECT,K)*DT(4,K)/DP2(ISECT,K)
      R2T=SQRT(RU2T**2+VZ2(ISECT,K)**2)
      MR2T(K)=R2T/SQRT(GAM(4,K)*G*rg2(k)*TS2T)
      VU2r=VU2(1,K)*DP2(1,K)/Dr(4,K)
      V2r=SQRT(VU2r**2+VZ2(1,K)**2)
      RU2r=VU2r+U2(1,K)*Dr(4,K)/DP2(1,K)
      U2r=U2(1,K)*Dr(4,K)/DP2(1,K)
      R2r=SQRT(RU2r**2+VZ2(1,K)**2)
      num=u1ar**2-u2r**2+r2r**2-r1ar**2
      rx2r(k)=num/(num+v1ar**2-v2r**2)
      HP(K)=1.4145*WGT0(K)*DELHTS(K)
      TORQUE(K)=5252.*HP(K)/RPMK(K)
      OHP=OHP+HP(K)
      OTORQ=OTORQ+TORQUE(K)
17    CONTINUE
      IF(GAMF)4,4,7
4     TAO=TAO/STG
      PAO=PAO/STG
7     GAMO=GAMO/STG
8     EO=(GAMO-1.)/GAMO
      CPO=(RG+rg2a(kstg))/2./EO/AJ
      K=KSTG
      KF=ISECT-1
      CALL BESFIT(ISECT,PCNFS,PTP(1,1),KF,DPT,RES)
      IPT=KF+1
      KF=ISECT-1
      CALL BESFIT(ISECT,PCNFS,TT0(1,1),KF,DTT,RES)
      ITT=KF+1
      DO 9 I=1,ISECT
      PSIRP=1.
      TTPSI=0.
      PTPSI=0.
      DO 3120 IFT=1,ISECT
      IF(IFT.GT.IPT)GO TO 3130
      PTPSI=PTPSI+DPT(IFT)*PSIRP
3130  IF(IFT.GT.ITT)GO TO 3120
      TTPSI=TTPSI+DTT(IFT)*PSIRP
3120  PSIRP=PSIRP*STRF2(I,K)
      RW=WG2A(I,K)/WGT2A(K)
      ODEHTI=CPO*TTPSI*(1.-(PT2A(I,K)/PTPSI)**EO)*RW+ODEHTI
      ODEHSI=CPO*TTPSI*(1.-(PS2A(I,K)/PTPSI)**EO)*RW+ODEHSI
c     ODHATI=CPO*TTPSI*(1.-(PAT2A(I,K)/PTPSI)**EO)*RW+ODHATI
      odhati=odehti
    9 continue
      odhsiu=odehsi
      do 160 kc=1,kstg
       do 150 ic=2,5
        wrc=rwg(ic,kc)/rwg(ic-1,kc)
        if (wrc.gt.1.0) then
          call cpa(ptpsi,twg(ic,kc),0.0,0.0,cpc)
          call gama(ptpsi,twg(ic,kc),0.0,0.0,gamc)
          cpcav=(cpc+cp2a(k))/2.
          gamav=(gamc+gam(5,k))/2.
          excav=(gamav-1.)/gamav
          fprcav=1.-(ptbar(k)/pwg(ic,kc))**excav
          wcowp=(rwg(ic,kc)/rwg(ic-1,kc)-1.)*rwg(ic-1,kc)
          odehti=odehti+wcowp*cpcav*twg(ic,kc)*fprcav
          fprsav=1.-(ps2a(ip,k)/pwg(ic,kc))**excav
          odehsi=odehsi+wcowp*cpcav*twg(ic,kc)*fprsav
          if(kc.eq.1.and.ic.lt.4) odhati=odehti
        end if
  150  continue
  160 continue
      OPSIP=G*AJ*ODELHT/OUPUP
      OPSIR=G*AJ*ODELHT/OURUR
      OVIS= SQRT(2.*G*AJ*ODHSIu)
      OUPVIS= OUP/STG/OVIS
      OWRTP=SWRTP(1)
c *** The following statement is for w41*sqrt(t41)/p4 ***
      owrtpg=owrtp*wgt1a(1)/wgt0(1)*sqrt(tt1a(ip,1)/stt0(1))
      OWNED=SWRTED(1)*SNRTHC(1)/60.
      ONRTHC=SNRTHC(1)
      ONRT=SNRT(1)
      ODHT=ODELHT/TTIN
      OTORP=OTORQ/PTIN
      OPT0T2=PTIN/PTBAR(KSTG)
      IF(TRY2.NE.0.0)TRY1=TRY2
      TRY2=OPT0T2
      IF(DHFIND.NE.10000.)TRY2=ODELHT
      OPT0S2=PTIN/PS2(IP,KSTG)
      OPTAT2=PTIN/PAT2A(IP,KSTG)
      OEQPTT=(1.-(GAMSL-1.)/(GAMSL+1.)*(GAMO+1.)/(GAMO-1.)*(1.-OPT0T2**
     1(-EO)))**(-E1)
      OEQPTS=(1.-(GAMSL-1.)/(GAMSL+1.)*(GAMO+1.)/(GAMO-1.)*(1.-OPT0S2**
     1(-EO)))**(-E1)
      OETATT=ODELHT/ODEHTI
      OETATS=ODELHT/ODEHSI
      OETAAT=ODELHT/ODHATI
      OETHC=ODELHT/THCR(1)
      OEQTOR=OTORQ*EPSI(K)/DELT(K)
      IF(VCTD.LT.0.0)  GO TO 27
C
C                       PRINT OUT FOR STAGE PERFORMANCE
      I=1
      WRITE(16,1000)XNAME,TITLE,ICASE,ISCASE
1000  FORMAT(1H1,6X,20A4/
     1  6X,20A4/ 30X,6HCASE  ,I3,1H.,I3/28X,17HSTAGE PERFORMANCE /16X,
     27HSTAGE 1,6X,7HSTAGE 2,6X,7HSTAGE 3,6X,7HSTAGE 4,6X,7HSTAGE 5,6X,
     37HSTAGE 6,6X,7HSTAGE 7,6X,7HSTAGE 8)
C
C
19    KS=KSTG
20    WRITE(16,1001)(STT0(K),K=I,KS)
1001  FORMAT(1X,12H        TT 0,2X,8(F10.1,3X))
      WRITE(16,1002)(SPT0(K),K=I,KS)
1002  FORMAT(1X,12H        PT 0,2X,8(F10.3,3X))
      WRITE(16,1003)(WGT0(K),K=I,KS)
1003  FORMAT(1X,12H        WG 0,2X,8(F10.3,3X))
      WRITE(16,1004)(DELHTS(K),K=I,KS)
1004  FORMAT(1X,12H       DEL H,2X,8(F10.3,3X))
      WRITE(16,1005)(SWRTP(K),K=I,KS)
1005  FORMAT(1X,12H       WRT/P,2X,8(F10.3,3X))
      WRITE(16,1006)(SDHT(K),K=I,KS)
1006  FORMAT(1X,12H        DH/T,2X,8(F10.5,3X))
      WRITE(16,1007)(SNRT(K),K=I,KS)
1007  FORMAT(1X,12H        N/RT,2X,8(F10.3,3X))
      WRITE(16,1008)(SETATT(K),K=I,KS)
1008  FORMAT(1X,12H      ETA TT,2X,8(F10.5,3X))
      WRITE(16,1009)(SETATS(K),K=I,KS)
1009  FORMAT(1X,12H      ETA TS,2X,8(F10.5,3X))
      WRITE(16,1010)(SETAAT(K),K=I,KS)
1010  FORMAT(1X,12H      ETA AT,2X,8(F10.5,3X))
      WRITE(16,1011)(PT0PS1(IP,K),K=I,KS)
1011  FORMAT(1X,12H     PT0/PS1,2X,8(F10.3,3X))
      WRITE(16,1012)(SPTPT2(K),K=I,KS)
1012  FORMAT(1X,12H     PT0/PT2,2X,8(F10.3,3X))
      WRITE(16,1013)(SPTPS2(K),K=I,KS)
1013  FORMAT(1X,12H     PT0/PS2,2X,8(F10.3,3X))
      WRITE(16,1014)(PTRS2(IP,K),K=I,KS)
1014  FORMAT(1X,12H   PTR1A/PS2,2X,8(F10.3,3X))
      WRITE(16,1015)(ST2TT0(K),K=I,KS)
1015  FORMAT(1X,12H     TT2/TT0,2X,8(F10.5,3X))
      WRITE(16,1016)(STRTT0(K),K=I,KS)
1016  FORMAT(1X,12H    TTR1/TT0,2X,8(F10.5,3X))
      WRITE(16,1017)(PS1(IP,K),K=I,KS)
1017  FORMAT(1X,12H        PS 1,2X,8(F10.3,3X))
      WRITE(16,1018)(TTR1A(IP,K),K=I,KS)
1018  FORMAT(1X,12H       TTR 1,2X,8(F10.1,3X))
      WRITE(16,1019)(PTR1A(IP,K),K=I,KS)
1019  FORMAT(1X,12H       PTR 1,2X,8(F10.3,3X))
      WRITE(16,1020)(PS2(IP,K),K=I,KS)
1020  FORMAT(1X,12H        PS 2,2X,8(F10.3,3X))
      WRITE(16,1021)(TTBAR(K),K=I,KS)
1021  FORMAT(1X,12H        TT 2,2X,8(F10.1,3X))
      WRITE(16,1022)(PTBAR(K),K=I,KS)
1022  FORMAT(1X,12H        PT 2,2X,8(F10.3,3X))
      WRITE(16,1023)(UPVIS(K),K=I,KS)
1023  FORMAT(1X,12H       UP/VI,2X,8(F10.5,3X))
      WRITE(16,1024)(URVIS(K),K=I,KS)
1024  FORMAT(1X,12H       UR/VI,2X,8(F10.5,3X))
      WRITE(16,1025)(PSIPS(K),K=I,KS)
1025  FORMAT(1X,12H      W.F. P,2X,8(F10.5,3X))
      WRITE(16,1026)(PSIRS(K),K=I,KS)
1026  FORMAT(1X,12H      W.F. R,2X,8(F10.5,3X))
      WRITE(16,1027)(RXP(K),K=I,KS)
1027  FORMAT(1X,12H        RX P,2X,8(F10.5,3X))
      WRITE(16,1028)(RXR(K),K=I,KS)
1028  FORMAT(1X,12H        RX R,2X,8(F10.5,3X))
      WRITE(16,11027)(RX2P(K),K=I,KS)
11027 FORMAT(1X,12H     REACT P,2X,8(F10.5,3X))
      WRITE(16,11028)(RX2R(K),K=I,KS)
11028 FORMAT(1X,12H     REACT R,2X,8(F10.5,3X))
      WRITE(16,1029)(SA0(K),K=I,KS)
1029  FORMAT(1X,12H     ALPHA 0,2X,8(F10.3,3X))
      WRITE(16,1030)(SIS(K),K=I,KS)
1030  FORMAT(1X,12H    I STATOR,2X,8(F10.3,3X))
      WRITE(16,1031)(SB1A(K),K=I,KS)
1031  FORMAT(1X,12H     BETA 1A,2X,8(F10.3,3X))
      WRITE(16,1032)(SIR(K),K=I,KS)
1032  FORMAT(1X,12H     I ROTOR,2X,8(F10.3,3X))
      WRITE(16,1033)(SA2(K),K=I,KS)
1033  FORMAT(1X,12H     ALPHA 2,2X,8(F10.3,3X))
      WRITE(16,1034)(DBETAR(K),K=I,KS)
1034  FORMAT(1X,12H     DBETA R,2X,8(F10.3,3X))
      WRITE(16,1035)(MIS(K),K=I,KS)
1035  FORMAT(1X,12H         M 1,2X,8(F10.5,3X))
      WRITE(16,1036)(MIRS(K),K=I,KS)
1036  FORMAT(1X,12H       M1 RT,2X,8(F10.5,3X))
      WRITE(16,1037)(MR1A(IP,K),K=I,KS)
1037  FORMAT(1X,12H       MR 1A,2X,8(F10.5,3X))
      WRITE(16,1038)(MR1AR(K), K=I,KS)
1038  FORMAT(1X,12H     MR1A RT,2X,8(F10.5,3X))
      WRITE(16,1039)(MR2(IP,K),K=I,KS)
1039  FORMAT(1X,12H        MR 2,2X,8(F10.5,3X))
      WRITE(16,1040)(MR2T(K),     K=I,KS)
1040  FORMAT(1X,12H     MR2 TIP,2X,8(F10.5,3X))
      WRITE(16,1041)(SETHC(K),K=I,KS)
1041  FORMAT(1X,12H     E/TH CR,2X,8(F10.3,3X))
      WRITE(16,1042)(SNRTHC(K),K=I,KS)
1042  FORMAT(1X,12H    N/RTH CR,2X,8(F10.1,3X))
      WRITE(16,1043)(SWRTED(K),K=I,KS)
1043  FORMAT(1X,12H   WRTHCRE/D,2X,8(F10.3,3X))
      WRITE(16,1053)(RPMK(K),K=1,KS)
1053  FORMAT(1X,12H         RPM,2X,8(F10.1,3X))
      WRITE(16,1260)(MF2A(IP,K),K=1,KS)
1260  FORMAT(1X,12H       MF 2A,2X,8(F10.5,3X))
      GO TO 22
27    XYZ=FLOAT(ISCASE-1)/7.-FLOAT((ISCASE-1)/7)
      IF(ABS(XYZ).GT..001)  GO TO 22
C
C                PRINT OUT FOR OVERALL PERFORMANCE
C
21    WRITE(16,1045)XNAME,TITLE,ICASE,ISCASE
1045  FORMAT(1H1,6X,20A4/
     1  6X,20A4/ 30X,6HCASE  ,I3,1H.,I3)
22    WRITE(16,1044) OPSIP,OPSIR,ODELHT,OWNED,OTORQ,OWRTP,ONRT,ODHT,
     1ONRTHC,OTORP,OPT0T2,OPT0S2,OPTAT2,OETHC,OEQTOR,OETATT,OETATS,
     2OETAAT,OHP,OUPVIS,OEQPTT,OEQPTS
1044  FORMAT(  47X,19HOVERALL PERFORMANCE/
     16X,7HW.F. P ,F12.5,5X,7HW.F. R ,
     &F12.5,5X,7HDEL H  ,F12.5,5X,7HWNE/60D,
     2F12.3,5X,7HTORQUE ,F12.3/6X,7HWRT/P  ,
     &F12.5,5X,7HN/RT   ,F12.3,5X,7HDH/T   ,
     3F12.5,5X,8HN/RTH CR,F11.3,5X,
     &7HTOR/P  ,F12.5/6X,7HPT0/PT2,F12.5
     4,5X,7HPT0/PS2,F12.5,5X,7HPT/PAT2,
     &F12.5,5X,7HE/TH CR,F12.5,5X,7HEQ TOR ,
     5F12.5/6X,7HETA TT ,F12.5,5X,
     &7HETA TS ,F12.5,5X,8HETA TTRP,F11.5,5X,
     67HHP     ,F12.3,5X,7HU/VIS  ,
     &F12.5/6X,7HPT/T EQ,F12.5,5X,7HPT/S EQ,F12.5)
C
C     added to store output variable values ...
C
      CALL TEMPDATA(OHP,TT2A,PT2A,WG2A,ETAS, ETAR,KS)
C
      WRITE(16,1046)WGT0(1),SWRTED(1),PT0PS1(IP,1)
1046  FORMAT(1H+,53X,7HWG 0   ,F12.4,5X,
     &7HEQ WG0 ,F12.5,5X,7HPT0/PS1,F12.5)
      IF(ENDPLT.EQ.1.0.AND.ICASE.LE.20.AND.ISCASE.LE.99) then
        if (iscase.eq.1.and.icase.eq.1) speed1=onrthc
        if (iscase.eq.1) speed(icase)=onrthc/speed1*100.
        prt(iscase,icase)=opt0t2
        if (iplot(3).eq.2) prt(iscase,icase)=opt0s2
        if (iplot(3).eq.3) prt(iscase,icase)=oupvis
        efft(iscase,icase)=oetatt
        if (iplot(2).eq.2) efft(iscase,icase)=oetats
        if (iplot(2).eq.3) efft(iscase,icase)=oetaat
        flow(iscase,icase)=swrted(1)
        if (iplot(1).eq.2) flow(iscase,icase)=swrtp(1)
        if (iplot(1).eq.3) flow(iscase,icase)=wgt0(1)
        if (iplot(1).eq.4) flow(iscase,icase)=swrted(1)*owrtpg/swrtp(1)
        if (iplot(1).eq.5) flow(iscase,icase)=owrtpg
        if (iplot(1).eq.6) flow(iscase,icase)=wgt1a(1)
      end if
      if (iplot(3).eq.3) then
        prtisc=prt(iscase,icase)
        effisc=efft(iscase,icase)
        floisc=flow(iscase,icase)
        do 999 isc=iscase,1,-1
          if (isc.gt.1) then
            prt(isc,icase)=prt(isc-1,icase)
            efft(isc,icase)=efft(isc-1,icase)
            flow(isc,icase)=flow(isc-1,icase)
          else
            prt(isc,icase)=prtisc
            efft(isc,icase)=effisc
            flow(isc,icase)=floisc
          end if
  999   continue
      end if
      RETURN
      END
