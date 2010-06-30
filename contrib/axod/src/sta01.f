      SUBROUTINE STA01
CSTA01
C           SUBROUTINE STA01
C
C     ESTABLISH FIRST STATOR EXIT FLOW, ADJUST FLOWS FOR COOLING
C        AIR INJECTION BETWEEN STATIONS 0 AND 1, FIND INLET
C        MACH NUMBER AND INCIDENCE ANGLE LOSS AT STATION 0,
C        ADJUST PT, GET NEW FLOW AT STATION 1 FOR FINAL RESULT.
C
      REAL MFSTOP
      LOGICAL PREVER
      COMMON /SNTCP/G,AJ,PRPC,ICASE,PREVER,MFSTOP,JUMP,LOPIN,ISCASE,
     1 K,GAMF,IP,SCRIT,PTRN,ISECT,KSTG,WTOL,RHOTOL,PRTOL,TRLOOP,LSTG,
     2LBRC,IBRC,ICHOKE,ISORR,CHOKE,PT0PS1(6,8),PTRS2(6,8),TRDIAG,SC,RC,
     3DELPR,PASS,IPC,LOPC,ISS
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
      COMMON/DESOPT/RVU1(6,8),RVU2(6,8),WG,EPR
      COMMON/TPT1/PT1(6),TT1(6,8)
C     COMMON    WGT0(1),TA0(1),WG0(6,1),TT0TS0(6,1),PT0PS0(6,1),FFA0(6,1
C    1),AAS0(6,1)
C replaced by ....
      COMMON /FSTA01/ WGT0(1),TA0(1),WG0(6,1),TT0TS0(6,1),PT0PS0(6,1),
     1FFA0(6,1),AAS0(6,1)
      COMMON/SLSHFT/STRF1(6,8),STRF2(6,8),UPSI1(6)
      dimension tangms(6,8),tangmr(6,8),tangm1(6,8),tangm2(6,8),tang0(6)
      common/slope/tangms,tangmr,tangm1,tangm2,tang0,iar,icyl
      COMMON /TDIL/TWG (6,8),pwg(6,8)
C     COMMON RES(6),PCNFS(6),        DTT(6),DPT(6)
C replaced by ...............................................6/4/09
      COMMON RES(8),PCNFS(6),        DTT(6),DPT(6)
      DIMENSION PCNF(6,8)
      DIMENSION PTPPS0(6)
      EQUIVALENCE (STRF1     ,PCNF)
C
C     print *,'  Entering in sta01.f.......modified..22 continue...'
      PI=3.14159
      SCRIT=0.0
      wtol01=2.e-5
      WR=RWG(2,K)
      TWGF=0.
      IF(TWG(2,K).GT.0.) TWGF=1.
      WPREV=0.0

      IF(WG.gt.0) then
        WD1=RWG(2,K)*WG
        A1(1,1)=PI/4.*(DT(2,K)**2-DR(2,K)**2)
        FFA1=WD1/A1(1,1)*SQRT(TT1(IP,K))/PT0(IP,K)
      endif

      ID=-1
      WGT1(K)=0.0
      JW=1
      PCNFS(1)=PCNH(1)/2.
      IF(RVU1(1,K).EQ.0.) FFA1=FFA1/CSALF1(IP,K)

      DO I=1,ISECT
        PT1(I)=PT0(I,K)
C        print *,'   PT1(',I,')=',PT1(I),'    in sta01...DO loop 1.'
        TT1(I,K)=TT0(I,K)

        if (twgf.gt.0.0) then
          ttm=(tt0(i,k)+(wr-1.)*twg(2,k))/wr
          tpp=(ttm+tt0(i,k))/2.
          tpc=(ttm+twg(2,k))/2.
          call cpa(ptin,tpc,0.0,0.0,cpc)
          call gama(ptin,tpp,fair,wair,gamtpp)
          extpp=gamtpp/(gamtpp-1.)
          cpp=rg*extpp/aj
          tt1(i,k)=(cpp*tt0(i,k)+(wr-1.)*cpc*twg(2,k))/(cpp+(wr-1.)*cpc)
        end if

        IF(I.NE.1) then
          PCNFS(I)=(PCNH(I)+PCNH(I-1))/2.+PCNFS(I-1)
        endif
      enddo

      KF=ISECT-1
      CALL BESFIT(ISECT,PCNFS,TT0(1,K),KF,DTT,RES)
      ITT=KF+1

      KF=ISECT-1
      CALL BESFIT(ISECT,PCNFS,PT0(1,K),KF,DPT,RES)
      IPT=KF+1

      I=IP

      IF(GAMF.le.0) then
        TA1(K)=.95*TT0(IP,K)
        CALL GAMA(PTIN,TA1(K),FAIRx(2,k),WAIRx(2,k),GAM(2,K))
      endif

      IF(WG.gt.0) then
        EXI=GAM(2,K)/(GAM(2,K)-1.)
        CP1(K)=rg1(k)*EXI/AJ
        VUOT=RVU1(IP,K)**2/CP1(K)/DP1(IP,K)**2/TT1(IP,K)/G/AJ*2.
        PRUP=1./(1.-(GAM(2,K)-1.+2.*VUOT)/(GAM(2,K)+1.)/ETAR(IP,K))**EXI
        PRLOW=1.
        CALL PRATIO(FFA1,GAM(2,K),rg1(k),PT0PS1(I ,K),PRTOL,
     1              ETAS(I ,K),VUOT,k)
      endif
C..........................
      LOOPCOUNT = 1

3     continue
      CALL FLOW1(I)
      IF (PREVER) return

C     print *,'  WGT1(K)=',WGT1(K),'  WG1(I,K) =',WG1(I,K)
      WGT1(K)=WGT1(K)+WG1(I,K)
      LOOPCOUNT = LOOPCOUNT + 1
      IF (LOOPCOUNT.eq.20) STOP
C     print *,'  K =',K,'  WGT1(K) =',WGT1(K)
C        TEST FOR TIP SECTOR
      IF((ISECT-I).gt.0) then
        I=I+ID
C       added '22 continue ' to mask warning message...
22      CONTINUE
        IF(I.gt.0) then
C          commented statement (Label 22 removed here)
C22        L=I-ID
           L=I-ID
c         print *,'  L=',L,'  I=',I,'  ID=',ID
          GM2=(GAM(2,K)-1.)/2.*VU1(L,K)**2/GAM(2,K)/G/rg1(k)/TS1(L,K)
          DRSQ=(DP1(L,K)/DP1(I,K))**2
          PS1(I,K)=PS1(L,K)
     1             *(1.+GM2*(1.-DRSQ))   **(GAM(2,K)/(GAM(2,K)-1.))
          PT0PS1(I,K)=PT1(I  )/PS1(I,K)
          IF ((PT0PS1(I,K)-1.).lt.0) then
            PTRN=-1.
            PT0PS1(I,K)= 1.0
          endif
          GO TO 3
        endif

        ID=1
        I=IP+ID
        GO TO 22
      endif

      IF(WG.le.0) then
C        CALCULATE STA 0 FOR INCIDENCE CORRECTION
        WE1=WPREV/WGT1(K)-1.
        WPREV=WGT1(K)
C       print *,' K=',K,' WGT1(K)=',WGT1(K),'  WTO101=',WTO101
        IF((ABS(WE1)-WTOl01).le.0) then
C         print *,'  going to 18******** WE1=',WE1,'  JW=',JW
          goto 18
        else
C         print *,'  going to 16******** WE1=',WE1,' JW =',Jw
          goto 16
        endif
      endif

C     print *,'  JW =',JW
      IF((JW-20).ge.0) goto 18

      IF((JW-1).le.0) goto 16

      IF((WGT1(K)-WD1).eq.0) then
        goto 18
      else IF((WGT1(K)-WD1).gt.0) then
        PRUP=PT0PS1(IP,K)*.8+.2*PRUP
      else
        PRLOW=PT0PS1(IP,K)*.8+.2*PRLOW
      endif

      WE=WGT1(K)/WD1-1.
      IF((ABS(WE)-WTOl01).le.0) goto 18

16    continue
      IF(GAMF.le.0) then
        tA0(K)=0.99*tt0(ip,k)
        call gama(ptin,ta0(k),fair,wair,gam(1,k))
      endif

      EX=(GAM(1,K)-1.)/GAM(1,K)
      EXI=1./EX
      WGT0(K)=WGT1(K)/RWG(2,K)*RWG(1,K)
      I= IP
      WG0(I,K)=WGT0(K)*PCNH(I)
      w0(i)=wg0(i,k)
      FFA0(I,K)=WG0(I,K)*SQRT(TT0(I,K))/(144.*PTP(I,K)*ANN0(I,K))
      FFA0(I,K)=FFA0(I,K)/COS(ALPHA0(I,K))/cos(atan(tang0(i)))
      J=1

8     continue
      CALL PRATIO(FFA0(I,K),GAM(1,K),RG,PTPPS0(I),PRTOL,1.0,0.,k)
      PS0(I,K)=PTP(I,K)/PTPPS0(I)
      TT0TS0(I,K)=PTPPS0(I)**EX
      TS0(I,K)=TT0(I,K)/TT0TS0(I,K)

      IF(GAMF.le.0) then
        TA0(K)=.5*(TT0(I,K)+TS0(I,K))
        CALL GAMA(PTIN,TA0(K),FAIR,WAIR,GAM(1,K))
        EX=(GAM(1,K)-1.)/GAM(1,K)
        EXI=1./EX

        IF((J-1).le.0) then
          J=J+1
          GO TO 8
        endif
      endif

      CP0(K)=RG*EXI/AJ
      WGT1(K)=0.0
      ANNFUL=PI/144.0*(DT(1,K)*DT(1,K)-DR(1,K)*DR(1,K))/4.0
      DTFM1=DR(1,K)

      DO I=1,ISECT
        ANN0(I,K)=ANNFUL*PCNH(I)
        DTF=SQRT(ANN0(I,K)*4.0*144.0/PI+DTFM1*DTFM1)
        H0=(DTF-DTFM1)/2.0
        DP0(I,K)=DTF-H0
        DTFM1=DTF
        WG0(I,K)=WGT0(K)*PCNH(I)
        w0(i)=wg0(i,k)
        PT0MO= PT0(I,K)
        FFA0(I,K)=WG0(I,K)*SQRT(TT0(I,K))/(144.*PTP(I,K)*
     1  ANN0(I,K))
        FFA0(I,K)=FFA0(I,K)/COS(ALPHA0(I,K))/cos(atan(tang0(i)))

        IF(I.ne.IP) then
          PS0(I,K) =  PS0(IP,K)
          PTPPS0(I)=PTP(I,K)/PS0(I,K)
        endif

        TT0TS0(I,K)=PTPPS0(I)**EX
        TS0(I,K)=TT0(I,K)/TT0TS0(I,K)
        V0(I,K)=SQRT(2.*G*AJ*CP0(K)*(TT0(I,K)-TS0(I,K)))
        AAS0(I,K)=SQRT(GAM(1,K)*G*RG*TS0(I,K))
        M0(I,K)=V0(I,K)/AAS0(I,K)
        SI(I,K)=ALPHA0(I,K)- RADSD(I,K)

        IF(SI(I,K).le.0) then
          EXPS=EXPN
        else
          EXPS=EXPP
        endif

        PT0PS0(I,K)=(1.+EX*M0(I,K)*ETARS(I,K)*GAM(1,K)*M0(I,K)/2.
     1             *(COS(SI(I,K))**EXPS))**EXI
        PT0(I,K)=PS0(I,K)*PT0PS0(I,K)
        WG0(I,K)=WG0(I,K)*PT0(I,K)/PT0MO
        w0(i)=wg0(i,k)
        WG1(I,K)=WG1(I,K)*PT0(I,K)/PT0MO
        RHOS0(I,K)=144.*PS0(I,K)/(RG*TS0(I,K))
        VU0(I,K)=V0(I,K)*SIN(ALPHA0(I,K))
        VZ0(I,K)=V0(I,K)*COS(ALPHA0(I,K))
        WGT1 (K)=WGT1 (K)+WG1(I,K)
      enddo
C        END OF INCIDENCE LOSS CORRECTION LOOP

      PCNF(1,K)=WG1(1,K)/WGT1(K)/2.

      DO I=2,ISECT
        PCNF(I,K)=(WG1(I-1,K)+WG1(I,K))/WGT1(K)/2.+PCNF(I-1,K)
      enddo

      KF=ISECT-1
      CALL BESFIT(ISECT,PCNFS,PT0(1,K),KF,DPT,RES)
      IPT=KF+1

      DO I=1,ISECT
        TT1(I,K)=0.
        PT1(I  )=0.
        PSIRP=1.
        DO IFT=1,ISECT
          IF(IFT.le.IPT) then
            PT1(I)  =PT1(I)  +DPT(IFT)*PSIRP
C            print *,'   PT1(',I,')=',PT1(I),'    in sta01...DO loop 2.'
C            print *,'   DPT(',IFT,')=',DPT(IFT),'  PSIRP =',PSIRP
          endif
          IF(IFT.le.ITT) then
            TT1(I,K)=TT1(I,K)+DTT(IFT)*PSIRP
          endif
          PSIRP=PSIRP*PCNF(I,K)
        enddo

        if (twgf.gt.0.0) then
          ttm=(tt1(i,k)+(wr-1.)*twg(2,k))/wr
          tpp=(ttm+tt1(i,k))/2.
          tpc=(ttm+twg(2,k))/2.
          call cpa(ptin,tpc,0.0,0.0,cpc)
          call gama(ptin,tpp,fair,wair,gamtpp)
          extpp=gamtpp/(gamtpp-1.)
          cpp=rg*extpp/aj
          tt1(i,k)=(cpp*tt1(i,k)+(wr-1.)*cpc*twg(2,k))/(cpp+(wr-1.)*cpc)
        end if
      enddo

      WGT1(K)=0.
      I=IP
      ID=-1
      IF(WG.GT.0.)PT0PS1(IP,K)=(PRLOW+PRUP)/2.
      JW=JW+1
C     print *,'   going to 3  ID =',ID,'  I=',I
      GO TO 3

18    CONTINUE
      WGT0(K)=WGT1(K)/RWG(2,K)*RWG(1,K)
      IF(TRLOOP.ne.0.) then
        WRITE(16,1000) WGT0(K),WGT1(K),(WG0(L,K),L=1,ISECT)
        WRITE(16,1001)              (PT0PS0(L,K),L=1,ISECT)
        WRITE(16,1002)                 (WG1(L,K),L=1,ISECT)
        WRITE(16,1003)              (PT0PS1(L,K),L=1,ISECT)
1000    FORMAT(2X,6H WGT0=,F8.3,2X,6H WGT1=,F8.3/2X,6H  WG0=,6F8.3)
1001    FORMAT(1X,7HPT0PS0=,6F8.5)
1002    FORMAT(2X,6H  WG1=,6F8.3)
1003    FORMAT(1X,7HPT0PS1=,6F8.5)
      endif

      j=2
      GO TO (25,26),J
25    CALL DIAGT(1)
26    RETURN
      END
