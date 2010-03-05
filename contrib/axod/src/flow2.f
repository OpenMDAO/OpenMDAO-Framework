C     CALCULATE ROTOR EXIT SECTOR FLOW
      SUBROUTINE FLOW2(I)
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
C
      COMMON /SSTA2/V2(6,8),TTR2(6,8),PTR2(6,8),WG2(6,8),WGT2(8),TA2(8),
     1           PS2(6,8),PHI2(6,8)
C
      REAL MR2,M2     ,MF2
      COMMON /SFLOW2/TS2(6,8),CP2(8),R2(6,8),RHOS2(6,8),BET2E(6,8),RU2(6
     1,8),VU2(6,8),DPDR2(6,8),VZ2(6,8),MR2(6,8),MF2(6,8),M2(6,8)
      COMMON/DESOPT/RVU1(6,8),RVU2(6,8),WG,EPR
      dimension tangms(6,8),tangmr(6,8),tangm1(6,8),tangm2(6,8),tang0(6)
      common/slope/tangms,tangmr,tangm1,tangm2,tang0,iar,icyl
C
C
C
      ETAO1=1.
      cat2=cos(atan(tangm2(i,k)))
      EX=(GAM(4,K)-1.)/GAM(4,K)
C        ISENTROPIC ROTOR RELATIVE TEMPERATURE RATIO
      PHI2(I,K)= PTRS2(I,K)**EX
      IF(OMEGAR(1,1))2,2,1
1     CALL LOSS2(I,K)
C        EXIT TEMPERATURES
2     IF(EPR.GT.0.) CALL ETAPR(PTRS2(I,K),ETAO1)
      ETA=ETAR(I,K)*ETAO1
      TS2(I,K)=TTR2(I,K)*(1.-ETA      *(1.-1./PHI2(I,K)))
      IF(I-IP)6,3,6
3     IF(    GAMF)4,4,5
4     TA2(K)=.5*(TTR2(I,K)+TS2(I,K))
      CALL GAMA(PTR2(I,K),TA2(K),FAIRx(4,k),WAIRx(4,k),GAM(4,K))
5     EXI=GAM(4,K)/(GAM(4,K)-1.)
      EX=1./EXI
C        CRITICAL PRESSURE RATIO
      CALL PHIM(EXI,ETA      ,PHI2C   ,P1AS2C   )
C        SPECIFIC HEAT AT CONSTANT PRESSURE
      CP2(K)=rg2(k)*EXI/AJ
C        RELATIVE EXIT VELOCITY
6     R2(I,K)=SQRT(2.*G*AJ*CP2(K)*(TTR2(I,K)-TS2(I,K)))
c      Pressure ratio correction to flow coefficient
      if (i.eq.ip) call etacf(ptrs2(i,k),p1as2c,xcf)
C        EXIT PRESSURE
      PS2(I,K)= PTR2(I,K)/ PTRS2(I,K)
C        EXIT DENSITY
      RHOS2(I,K)=144.*PS2(I,K)/(rg2(k)*TS2(I,K))
C        TEST CRITICAL PRESSURE RATIO
      IF(RVU1(I,K))120,130,120
120   SNBET2=(RVU2(I,K)*2./DP2(I,K)+U2(I,K))/R2(I,K)
      ASNBET=ABS(SNBET2)
      IF(ASNBET.GT.1.)SNBET2=SNBET2/(ASNBET+.01)
      CBET2E=SQRT(1.-SNBET2**2)
      BET2E(I,K)=ATAN(SNBET2/CBET2E)
130   IF( PTRS2(I,K)-P1AS2C   )140,7,7
7     IF(RVU1(I,K).NE.0.) GO TO 11
      IF (IP-I) 22,8,22
8     IF (PRPC)9,9,18
9     PRPC=1.
      GO TO 18
22    IF (PTRS2(I,K).LE.PTRS2(IP,K))   GO TO 18
      GO TO 11
18    IF ((I.EQ.1).OR.(I.EQ.ISECT))  SCRIT=1.
      GO TO 11
11    CONTINUE
      IF(EPR.GT.0.)
     *CALL ETAPR(P1AS2C,ETAO1)
      ETAC=ETAR(I,K)*ETAO1
      R2C     =SQRT(2.*G*AJ*CP2(K)*TTR2(I,K)*ETAC     *(
     1PHI2C   -1.)/PHI2C  )
      TS2C     =TTR2(I,K)*(1.-ETAC     *(1.-1./PHI2C   ))
      RHOS2C     =144.*PTR2(I,K)/(rg2(k)*P1AS2C   *TS2C     )
      IF(RVU1(I,K))15,150,15
150   continue
      cscyl=sqrt(1./(1.+(1./csbet2(i,k)**2-1.)/cat2**2))
      WG2(I,K)=RHOS2C     *R2C     *ANN2(I,K)*CScyl
     & *cat2*cfr(i,k)*xcf
      if (andor(1,k).gt.0.0) wg2(i,k)=wg2(i,k)/cfr(i,k)/xcf
C   OVEREXPANSION
      Cscyle     =WG2(I,K)/(RHOS2(I,K)*R2(I,K)*ANN2(I,K))
     & /cat2/cfr(i,k)/xcf
      if (andor(1,k).gt.0.0) cscyle=cscyle*cfr(i,k)*xcf
      cbet2e=sqrt(1./(1.+(1./cscyle**2-1.)*cat2**2))
      BET2E(I,K)=ATAN2(SQRT(1.-CBET2E     *CBET2E     ),CBET2E     )
      GO TO 16
140   IF(RVU1(I,K).NE.0.) GO TO 15
      CBET2E     =CSBET2(I,K)
      BET2E(I,K)= BET2(I,K)
15    continue
      cscyle=sqrt(1./(1.+(1./cbet2e**2-1.)/cat2**2))
      WG2(I,K)=RHOS2(I,K)*R2(I,K)*ANN2(I,K)*Cscyle
     & *cat2*cfr(i,k)*xcf
      if (andor(1,k).gt.0.0) wg2(i,k)=wg2(i,k)/cfr(i,k)/xcf
      IF(RVU1(I,K).EQ.0.) GO TO 16
      IF(PTRS2(I,K)-P1AS2C) 170,180,180
170   CSBET2(I,K)=CBET2E
      BET2(I,K)=BET2E(I,K)
      GO TO 16
180   CScyl=WG2(I,K)/RHOS2C/R2C/ANN2(I,K)
     & /cat2/cfr(i,k)/xcf
      if (andor(1,k).gt.0.0) cscyl=cscyl*cfr(i,k)*xcf
      csbet2(i,k)=sqrt(1./(1.+(1./cscyl**2-1.)*cat2**2))
      BET2(I,K)=ATAN(SQRT(1.-CSBET2(I,K)**2)/CSBET2(I,K))*SNBET2/ASNBET
16    RU2(I,K)=R2(I,K)*SIN(BET2E(I,K))
      VU2(I,K)=RU2(I,K)-U2(I,K)
      VZ2(I,K)=R2(I,K)*CBET2E
      AS2     =SQRT(GAM(4,K)*G*rg2(k)*TS2(I,K))
      V2(I,K)=SQRT(VZ2(I,K)*VZ2(I,K)+VU2(I,K)*VU2(I,K))
      M2(I,K)=V2(I,K)/AS2
      MR2(I,K)=R2(I,K)/AS2
      MF2(I,K)=MR2(I,K)*CBET2E
      IF(I.LT.ISECT) GO TO 17
      IF(PRPC.EQ.1.) PRPC=2.
  17  j=2
      GO TO (19,21),J
19    CALL DIAGT(4)
21    RETURN
      END
