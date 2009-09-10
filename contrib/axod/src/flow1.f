CCHECK
C      SUBROUTINE TO CHECK SENSE LIGHTS
c     SUBROUTINE CHECK(J)
C
c     REAL MFSTOP
c     LOGICAL PREVER
c     COMMON /SNTCP/G,AJ,PRPC,ICASE,PREVER,MFSTOP,JUMP,LOPIN,ISCASE,
c    1KN,GAMF,IP,SCRIT,PTRN,ISECT,KSTG,WTOL,RHOTOL,PRTOL,TRLOOP,LSTG,
c    2LBRC,IBRC,ICHOKE,ISORR,CHOKE,PT0PS1(6,8),PTRS2(6,8),TRDIAG,SC,RC,
c    3DELPR,PASS,IPC,LOPC,ISS
C
C     DO 1 I=1,4
C     CALL SLITET(I,J)
C     GO TO (2,1),J
C1     CONTINUE
c     J=2
c     RETURN
C2     J=1
C     PREVER=.TRUE.
C     RETURN
c     END
C     ESTABLISH VALUES FOR STATOR EXIT FLOW
      SUBROUTINE FLOW1(I)
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
      REAL M0
      COMMON /SSTA01/CP0(8),w0(6),               PS0(6,8),V0(6,8),TS0(6,
     18),VU0(6,8),VZ0(6,8),RHOS0(6,8),PS1(6,8),WGT1(8),TA1(8),WG1(6,8),
     2            DPDR1(6,8),SI(6,8),  CP1(8),PHI1(6,8),TS1(6,8),V1(6,8)
     3,RHOS1(6,8),ALF1E(6,8),VU1(6,8),VZ1(6,8),M0(6,8)
      COMMON/DESOPT/RVU1(6,8),RVU2(6,8),WG,EPR
      COMMON/TPT1/PT1(6),TT1(6,8)
      dimension tangms(6,8),tangmr(6,8),tangm1(6,8),tangm2(6,8),tang0(6)
      common/slope/tangms,tangmr,tangm1,tangm2,tang0,iar,icyl
C
C
C
      ETAO1=1.
      cat1=cos(atan(tangm1(i,k)))
      EX=(GAM(2,K)-1.)/GAM(2,K)
C        COMPUTE ISENTROPIC STATOR TEMPERATURE RATIO
7     PHI1(I,K)=PT0PS1(I,K)**EX
C        TEST FOR LOSS COEFFICIENT INPUT
      IF (OMEGAS(1,1))2,2,1
1     CALL LOSS1(I,K,EX)
2     IF(EPR.GT.0.) CALL ETAPR(PT0PS1(I,K),ETAO1)
      ETA=ETAS(I,K)*ETAO1
      TS1(I,K)=TT1(I,K)*(1.-ETA      *(1.-1./PHI1(I,K)))
      IF(I-IP)6,3,6
3     IF(GAMF)4,4,5
4     TA1(K)=.5*(TT1(I,K)+TS1(I,K))
      CALL GAMA(PT0(IP,K),TA1(K),FAIRx(2,k),WAIRx(2,k),GAM(2,K))
5     EX=(GAM(2,K)-1.0)/GAM(2,K)
      EXI=1./EX
C     CRITICAL PRESSURE RATIO
      CALL PHIM(EXI,ETA      ,PHI1C   ,PTPS1C   )
      CP1(K)=RG1(k)*EXI/AJ
C        EXIT VELOCITY
6     V1(I,K)=SQRT(2.*G*AJ*CP1(K)*(TT1(I,K)-TS1(I,K)))
c      Pressure ratio correction to flow coefficient
      if (i.eq.ip) call etacf(pt0ps1(i,k),ptps1c,xcf)
C        EXIT PRESSURE
      PS1(I,K)=PT1(I  )/PT0PS1(I,K)
C        EXIT DENSITY
      RHOS1(I,K)=144.*PS1(I,K)/(rg1(k)*TS1(I,K))
C        TEST CRITICAL PRESSURE RATIO
      IF(RVU1(I,K))120,130,120
120   SNALF1=RVU1(I,K)*2./DP1(I,K)/V1(I,K)
      ASNALF=ABS(SNALF1)
      IF(ASNALF.GT.1.)SNALF1=SNALF1/(ASNALF+.01)
      CSAL1E=SQRT (1.-SNALF1**2)
      ALF1E(I,K)=ATAN(SNALF1/CSAL1E)
130   IF(PT0PS1(I,K)-PTPS1C)140,   8,8
C        GREATER THAN CRITICAL
8     IF(RVU1(I,K).NE.0.) GOTO 11
      IF (IP-I) 21,9,21
9     IF (PRPC)10,10,22
C        PREVIOUS PITCH NONCRITICAL
10    PRPC=1.
      GO TO 22
21    IF (PT0PS1(I,K).LE.PT0PS1(IP,K))  GO TO 22
      GO TO 11
22    IF ((I.EQ.1).OR.(I.EQ.ISECT))  SCRIT=1.
      GO TO 11
C        PITCH OR OUTBOARD SECTOR
11    CONTINUE
C   PRESSURE RATIO ABOVE CRITICAL
      IF(EPR.GT.0.) CALL ETAPR(PTPS1C,ETAO1)
      ETAC=ETAS(I,K)*ETAO1
      V1C     =SQRT(2.*G*AJ*CP1(K)*TT1(I,K)*ETAC     *(PHI1C
     1-1.)/PHI1C   )
      TS1C     =TT1(I,K)*(1.-ETAC     *(1.-1./PHI1C   ))
      RHOS1C     =144.*PT1(I  )/(  PTPS1C   *TS1C     *rg1(k))
      IF(RVU1(I,K))15,150,15
150   continue
      cscyl=sqrt(1./(1.+(1./csalf1(i,k)**2-1.)/cat1**2))
      WG1(I,K)=RHOS1C     *V1C     *ANN1(I,K)*CScyl
     & *cat1*cfs(i,k)*xcf
      if (ando(1,k).gt.0.0) wg1(i,k)=wg1(i,k)/cfs(i,k)/xcf
13    CScyle     =WG1(I,K)/(RHOS1(I,K)*V1(I,K)*ANN1(I,K))
     & /cat1/cfs(i,k)/xcf
      if (ando(1,k).gt.0.0) cscyle=cscyle*cfs(i,k)*xcf
      csal1e=sqrt(1./(1.+(1./cscyle**2-1.)*cat1**2))
C     EFFECTIVE STATOR EXIT ANGLE
14    ALF1E(I,K)=ATAN2(SQRT(1.-CSAL1E     *CSAL1E     ),
     1CSAL1E)
      GO TO 16
C   PRESSURE RATIO LESS THAN CRITICAL
140   IF(RVU1(I,K).NE.0.)GO TO 15
      CSAL1E     =CSALF1(I,K)
      ALF1E(I,K)=ALF1(I,K)
15    continue
      cscyle=sqrt(1./(1.+(1./csal1e**2-1.)/cat1**2))
      WG1(I,K)=RHOS1(I,K)*V1(I,K)*ANN1(I,K)*CScyle
     & *cat1*cfs(i,k)*xcf
      if (ando(1,k).gt.0.0) wg1(i,k)=wg1(i,k)/cfs(i,k)/xcf
      IF(RVU1(I,K).EQ.0.) GO TO 16
      IF(PT0PS1(I,K)-PTPS1C)170,180,180
170   CSALF1(I,K)=CSAL1E
      ALF1(I,K)=ALF1E(I,K)
      GO TO 16
180   CScyl=WG1(I,K)/RHOS1C/V1C/ANN1(I,K)/cfs(i,k)/xcf
     & /cat1
      if (ando(1,k).gt.0.0) cscyl=cscyl*cfs(i,k)*xcf
      csalf1(i,k)=sqrt(1./(1.+(1./cscyl**2-1.)*cat1**2))
      ALF1(I,K)=ATAN(SQRT(1.-CSALF1(I,K)**2)/CSALF1(I,K))*SNALF1/ASNALF
16    VU1(I,K)=V1(I,K)*SIN(ALF1E(I,K))
      VZ1(I,K)=V1(I,K)*CSAL1E
      IF(I.LT.ISECT) GO TO 17
      IF(PRPC.EQ.1.) PRPC=2.
17    j=2
      GO TO (19,20),J
19    CALL DIAGT(2)
20    RETURN
      END
