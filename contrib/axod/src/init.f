      SUBROUTINE INIT
      REAL MFSTOP
      LOGICAL PREVER
      COMMON /SNTCP/G,AJ,PRPC,ICASE,PREVER,MFSTOP,JUMP,LOPIN,ISCASE,
     1KN,GAMF,IP,SCRIT,PTRN,ISECT,KSTG,WTOL,RHOTOL,PRTOL,TRLOOP,LSTG,
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
      COMMON/SADIN2/PTINH(6),TTINH(6),ALF0H(6)
      COMMON/RPMCOM/RPMK(8)
      COMMON/DESOPT/RVU1(6,8),RVU2(6,8),WG,EPR
      dimension arh(4),arm(4),arl(4),car(4),cxs(8),cxr(8)
      dimension tangms(6,8),tangmr(6,8),tangm1(6,8),tangm2(6,8),tang0(6)
      common/slope/tangms,tangmr,tangm1,tangm2,tang0,iar,icyl
      COMMON    D(6),RES(8),                H1A(6  ),H0(6  ),H2A(6  )
      DATA KFIT/-2/
      DATA ARH/5.7162E-3,2.0062E-4,0.58705,-1.1622/
      DATA ARM/3.0740E-2,2.5228E-4,0.57930,-1.1487/
      DATA ARL/6.6559E-2,2.2132E-4,0.47043,-0.86488/
C
C     READ INPUT DATA, CHECK FOR ERRORS,
C          SKIP CHANGE CASES IF BASIC CASE HAS AN ERROR
C                                                       
3     CALL INPUT
      ICASE=ICASE+1
c     IF(STGCH)5,5,4
4     IK=1
   5  l=2
      GO TO(6,8),L
6     WRITE(6,100)ICASE
      IF(STGCH)3,3,7
7     IK=2
      GO TO 3
8     IF (IK-2)9,3,3
C        INITIALIZE INDEX REGISTERS AND FORKS
9     ISECT=SECT+.0001
      KSTG= STG+.0001
      LOPC=0
      CHOKE=0.
      ICHOKE=0
      ISORR=1
      KN=1
      LSTG=1
      IBRC=1
      LBRC=1
      DELPR=DELC
      SC=0.0
      RC=0.0
      PRPC=0.0
      IPC=0
      ISS=0
      PTRN=0.0
C        TEST STAGE LOSS INDICATOR
      IF(SLI)13,13,11
11    DO 12 I=1,ISECT
      DO 12 J=1,KSTG
      ETARS(I,J)=ETARS(I,1)
      ETAS(I,J)=ETAS(I,1)
      CFS(I,J)=CFS(I,1)
      ETARR(I,J)=ETARR(I,1)
      ETAR(I,J)=ETAR(I,1)
      CFR(I,J)=CFR(I,1)
      TFR(I,J)=TFR(I,1)
12    CONTINUE
C        TEST FOR EQUAL SECTORS
13    CONTINUE
      IF(PTINH(ISECT).NE. 0.) PTIN= 0.
      IF(TTINH(ISECT).NE. 0.) TTIN= 0.
      IF(PCNH(1).NE.1.)GO TO 43
      DO 42 I=1,ISECT
42    PCNH(I)=1./SECT
43    DO 15 I=1,ISECT
14    IF(PTINH(ISECT).NE. 0.) PTIN= PTIN + PCNH(I)*PTINH(I)
      IF(TTINH(ISECT).NE. 0.) TTIN= TTIN + PCNH(I)*TTINH(I)
      IF(PTINH(ISECT).EQ. 0.) PTINH(I)= PTIN
      PTP(I,1)= PTINH(I)
      PT0(I,1)= PTINH(I)
      IF(TTINH(ISECT).EQ. 0.) TTINH(I)= TTIN
      TT0(I,1)= TTINH(I)
      IF(ALF0H(ISECT).EQ. 0.) ALF0H(I)= 0.
      ALPHA0(I,1)= ALF0H(I)* .017453
      if (icyl.eq.1) alpha0(i,1)=atan(tan(alpha0(i,1))
     & *cos(atan(tang0(i))))
      PT0PS1(I,1)= PTPS
15    CONTINUE
      IF(ICASE.EQ.1)GO TO 16
      DO 150 K=1,KSTG
      KF=KFIT
      CALL BESFIT(ISECT,DP0 (1,K),RADSD(1,K),KF  ,D,RES)
      KFIT1=IABS(KF)+1
      DO 155 I=1,ISECT
      RADSD(I,K)=0.
      DRP=1.
      DO 155 ID=1,KFIT1
      RADSD(I,K)= RADSD(I,K)+DRP*D(ID)
155   DRP=DRP*DP0  (I,K)
      KF=KFIT
      CALL BESFIT(ISECT,DP1 (1,K),ALF1 (1,K),KF  ,D,RES)
      KFIT1=IABS(KF)+1
      DO 165 I=1,ISECT
      ALF1(I,K)=0.
      DRP=1.
      DO 165 ID=1,KFIT1
      ALF1  (I,K)= ALF1  (I,K)+DRP*D(ID)
165   DRP=DRP*DP1  (I,K)
      KF=KFIT
      CALL BESFIT(ISECT,DP1A(1,K),RADRD(1,K),KF  ,D,RES)
      KFIT1=IABS(KF)+1
      DO 175 I=1,ISECT
      RADRD(I,K)=0.
      DRP=1.
      DO 175 ID=1,KFIT1
      RADRD (I,K)= RADRD (I,K)+DRP*D(ID)
175   DRP=DRP*DP1A  (I,K)
      KF=KFIT
      CALL BESFIT(ISECT,DP2 (1,K),BET2 (1,K),KF  ,D,RES)
      KFIT1=IABS(KF)+1
      DO 185 I=1,ISECT
      BET2(I,K)=0.
      DRP=1.
      DO 185 ID=1,KFIT1
      BET2  (I,K)= BET2  (I,K)+DRP*D(ID)
185   DRP=DRP*DP2   (I,K)
150   CONTINUE
C        SET UP SECTOR HEIGHT, PITCH DIAMETER, ANNULUS AREA,
C             PITCHLINE WHEEL SPEED
16    DO 19 K=1,KSTG
      SH0=DT(1,K)-DR(1,K)
      SH1=DT(2,K)-DR(2,K)
      SH1A=DT(3,K)-DR(3,K)
      SH2=DT(4,K)-DR(4,K)
      SH2A=DT(5,K)-DR(5,K)
      DO 18 I=1,ISECT
      H0(I  )=.5*PCNH(I)*SH0
      H1(I,K)=.5*PCNH(I)*SH1
      H1A(I  )=.5*PCNH(I)*SH1A
      H2(I,K)=.5*PCNH(I)*SH2
      H2A(I  )=.5*PCNH(I)*SH2A
      IF(I-1)20,20,17
20    DP0(I,K)=DR(1,K)+   H0(I  )
      DP1(I,K)=DR(2,K)+   H1(I,K)
      DP1A(I,K)=DR(3,K)+   H1A(I  )
      DP2(I,K)=DR(4,K)+   H2(I,K)
      DP2A(I,K)=DR(5,K)+   H2A(I  )
      GO TO 21
17    DP0(I,K)=        H0(I-1  )+   H0(I  )+DP0(I-1,K)
      DP1(I,K)=        H1(I-1,K)+   H1(I,K)+DP1(I-1,K)
      DP1A(I,K)=        H1A(I-1  )+   H1A(I  )+DP1A(I-1,K)
      DP2(I,K)=        H2(I-1,K)+   H2(I,K)+DP2(I-1,K)
      DP2A(I,K)=        H2A(I-1  )+   H2A(I  ) +DP2A(I-1,K)
21    ANN0(I,K)=.02182*DP0(I,K)*H0(I  )
      ANN1(I,K)=.02182*DP1(I,K)*H1(I,K)
      ANN1A(I,K)=DP1A(I,K)*H1A(I  )*.02182
      ANN2(I,K)=.02182*DP2(I,K)*H2(I,K)
      ANN2A(I,K)=.02182*DP2A(I,K)*H2A(I  )
      U2(I,K)= 3.14159*DP2(I,K)*RPMK(K)/720.0
      U1A(I,K)= 3.14159*DP1A(I,K)*RPMK(K)/720.0
18    CONTINUE
19    CONTINUE
C        DEFINE PITCHLINE INDEX
      IT=ISECT-2*(ISECT/2)
      IF(IT)22,22,23
22    IP=ISECT/2
      GO TO 24
23    IP=(ISECT+1)/2
24    CONTINUE
c        Set Axial-Chord Correlation Coefficients
      IF (IAR.EQ.1) THEN
        DO 300 I=1,4
        CAR(I) = ARH(I)
  300   CONTINUE
      ELSE IF (IAR.EQ.2) THEN
        DO 310 I=1,4
        CAR(I) = ARM(I)
  310   CONTINUE
      ELSE if (iar.eq.3) then
        DO 320 I=1,4
        CAR(I) = ARL(I)
  320   CONTINUE
      else if (iar.ge.10.and.iar.le.20) then
        car2=(float(iar)-10.)/10.
        car1=1.0-car2
        do 321 i=1,4
        car(i)=car1*arh(i)+car2*arm(i)
  321   continue
      else if (iar.gt.20.and.iar.le.30) then
        car3=(float(iar)-20.)/10.
        car2=1.0-car3
        do 322 i=1,4
        car(i)=car2*arm(i)+car3*arl(i)
  322   continue
      END IF
c        Calculate Axial Chords and Sector Slopes
      if (iar.ne.0) then
        sumcx=0.0
        do 330 k=1,kstg
        cxs(k)=car(1)*dp1(ip,k)+car(2)*dp1(ip,k)**2+car(3)+
     &         car(4)/dp1(ip,k)
        cxr(k)=car(1)*dp2(ip,k)+car(2)*dp2(ip,k)**2+car(3)+
     &         car(4)/dp2(ip,k)
        sumcx=sumcx+cxs(k)+cxr(k)
        do 325 i=1,isect
        tangms(i,k)=(dp1(i,k)-dp0(i,k))/2./cxs(k)
        tangmr(i,k)=(dp2(i,k)-dp1a(i,k))/2./cxr(k)
  325   continue
  330   continue
        sumcx=4./3.*sumcx-cxr(kstg)/3.
      else
        do 340 k=1,kstg
        do 335 i=1,isect
        tangms(i,k)=0.0
        tangmr(i,k)=0.0
  335   continue
  340   continue
        sumcx=0.0
      end if
      do 350 i=1,isect
      tang0(i)=(tangms(i,1)+tangms(ip,1))/2.
  350 continue
      do 360 k=1,kstg
      do 355 i=1,isect
      tangm1(i,k)=(tangms(i,k)+tangmr(i,k))/2.
      if (k.ne.kstg) then
        tangm2(i,k)=(tangmr(i,k)+tangms(i,k+1))/2.
      else
        tangm2(i,k)=(tangmr(i,k)+tangmr(ip,k))/2.
      end if
  355 continue
  360 continue
      write (6,400) sumcx
  400 format (1h0,' TURBINE LENGTH = ',f6.2,' INCHES')
C        CALCULATE INLET AND EXIT ANGLES IN RADIANS
      DO 31 K=1,KSTG
      IF(ALPHA1(1,K))27,25,27
25    IF(ANDO(1,K))31,31,125
125   DO 26 I=1,ISECT
      CSALF1(I,K)=ANDO(I,K)*CFS(I,K)/(SESTHI(K)*3.14159*DP1(I,K)*
     1SQRT(ETAS(I,K)))
26    ALF1(I,K)=ATAN2(SQRT(1.-CSALF1(I,K)*CSALF1(I,K)),CSALF1(I,K))
      GO TO 31
27    DO 28 I=1,ISECT
      ALF1(I,K)=    ALPHA1(I,K)*.017453
      if (icyl.eq.1) alf1(i,k)=atan(tan(alf1(i,k))
     & *cos(atan(tangm1(i,k))))
28    CSALF1(I,K)=COS(ALF1(I,K))
31    CONTINUE
      DO 130 K=1,KSTG
      IF(BETA2(1,K))32,29,32
29    IF(ANDOR(1,K))130,130,129
129   DO 30 I=1,ISECT
      CSBET2(I,K)=ANDOR(I,K)*CFR(I,K)/(RERTHI(K)*3.14159*DP2(I,K)*
     1SQRT(ETAR(I,K)))
30    BET2(I,K)=ATAN2( SQRT(1.-CSBET2(I,K)*CSBET2(I,K)),CSBET2(I,K))
      GO TO 130
32    DO 33 I=1,ISECT
      BET2(I,K)=    BETA2(I,K)*.017453
      if (icyl.eq.1) bet2(i,k)=atan(tan(bet2(i,k))
     & *cos(atan(tangm2(i,k))))
33    CSBET2(I,K)=COS(BET2(I,K))
130   CONTINUE
34    DO 35 K=1,KSTG
      DO 35 I=1,ISECT
      IF(ALPHAS(I,K).NE.0.)
     *RADSD(I,K)=ALPHAS(I,K)*.017453
      if (icyl.eq.1.and.k.eq.1) radsd(i,k)=atan(tan(radsd(i,k))
     & *cos(atan(tang0(i))))
      if (icyl.eq.1.and.k.gt.1) radsd(i,k)=atan(tan(radsd(i,k))
     & *cos(atan(tangm2(i,k-1))))
      IF(BETA1(I,K).NE.0.)
     *RADRD(I,K)=BETA1(I,K)*.017453
      if (icyl.eq.1) radrd(i,k)=atan(tan(radrd(i,k))
     & *cos(atan(tangm1(i,k))))
35    continue
      IF(RG)36,36,37
36    CALL R(PTIN,TTIN,FAIR,WAIR,RG)
      GAMF=0.0
      GO TO 38
37    GAMF=1.0
38    continue
      do 420 k=1,kstg
      rg1(k)=rg
      rg1a(k)=rg
      rg2(k)=rg
      rg2a(k)=rg
  420 continue
      if (fair.eq.0.0.and.wair.eq.0.0) go to 40
      do 440 k=1,kstg
      do 430 i=2,5
      fairx(i,k)=fair/(1.+(rwg(i,k)-1.)*(1.+fair))
      wairx(i,k)=wair/(1.+(rwg(i,k)-1.)*(1.+wair))
      if (i.eq.2) call r(ptin,ttin,fairx(2,k),wairx(2,k),rg1(k))
      if (i.eq.3) call r(ptin,ttin,fairx(3,k),wairx(3,k),rg1a(k))
      if (i.eq.4) call r(ptin,ttin,fairx(4,k),wairx(4,k),rg2(k))
      if (i.eq.5) call r(ptin,ttin,fairx(5,k),wairx(5,k),rg1a(k))
  430 continue
  440 continue
   40 continue
      RETURN
100   FORMAT(28X,6HCASE  ,I5,13H HAS AN ERROR)
      END
