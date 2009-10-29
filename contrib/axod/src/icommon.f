C  ICOMMON.f
      SUBROUTINE ICOMMON
c
      COMMON /SNTCP/ G,AJ,PRPC,ICASE,PREVER,MFSTOP,JUMP,LOPIN,ISCASE,
     .KN,GAMF,IP,SCRIT,PTRN,ISECT,KSTG,WTOL,RHOTOL,PRTOL,TRLOOP,LSTG,
     .LBRC,IBRC,ICHOKE,ISORR,CHOKE,PT0PS1(6,8),PTRS2(6,8),TRDIAG,SC,RC,
     .DELPR,PASS,IPC,LOPC,ISS

      COMMON /GEBE/TGEBE(10),IGEBE(9),TGEBE1(22),IGEBE1
      COMMON /SADIN2/ PTINH(6),TTINH(6),ALF0H(6)

C     COMMON /SINPUT/
C    .PTPS,PTIN,TTIN,WAIR,FAIR,DELC,DELL,DELA,AACS,VCTD,STG,SECT,EXPN,
C    .EXPP,EXPRE,RG,RPM,PAF,SLI,STGCH,ENDJOB,XNAME(20),TITLE(20),
C    .PCNH(6),GAM(6,8),DR(6,8),DT(6,8),RWG(6,8),ALPHAS(6,8),ALPHA1(6,8),
C    .ETARS(6,8),ETAS(6,8),CFS(6,8),ANDO(6,8),BETA1(6,8),BETA2(6,8),ETAR
C    .R(6,8),ETAR(6,8),CFR(6,8),TFR(6,8),ANDOR(6,8),OMEGAS(6,8),AS0(6,8)
C    .,ASMP0(6,8),ACMN0(6,8),A1(6,8),A2(6,8),A3(6,8),A4(6,8),A5(6,8),A6(
C    .6,8),OMEGAR(6,8),BSIA(6,8),BSMPIA(6,8),BCMNIA(6,8),B1(6,8),B2(6,8)
C    .,B3(6,8),B4(6,8),B5(6,8),B6(6,8),SESTHI(8),RERTHI(8)
C    .,fairx(5,8),wairx(5,8),rg1(8),rg1a(8),rg2(8),rg2a(8)

      COMMON/SINPUT/tsinput(1751)

      COMMON /SADDIN/ PS1P(8),PS2P(8)

C     COMMON /GETPR/ TRY1,TRY2,PTRY1,PTRY2,PFIND,DHFIND,PFIND1,
C    .DHFND1
      COMMON /GETPR/ TGETPR(8)                   

      COMMON /DESOPT/ RVU1(6,8),RVU2(6,8),WG,EPR

      REAL MR2,M2     ,MF2
      COMMON /SFLOW2/ TS2(6,8),CP2(8),R2(6,8),RHOS2(6,8),BET2E(6,8),
     .RU2(6,8),VU2(6,8),DPDR2(6,8),VZ2(6,8),MR2(6,8),MF2(6,8),M2(6,8)


      common/cfi/icf

      COMMON /TDIL/TWG (6,8),pwg(6,8)

      COMMON /PLOTT/ ENDPLT,IPCAS,IPSCAS(20),xnname(20),iplot(3)
      COMMON /SINIT/H1(6,8),H2(6,8),DP0(6,8),DP1(6,8),DP1A(6,8),DP2(6,8)
     1,DP2A(6,8),CSALF1(6,8),ALF1(6,8),CSBET2(6,8),BET2(6,8),RADSD(6,8),
     2RADRD(6,8),ANN1(6,8),ANN2(6,8),ANN2A(6,8),ANN1A(6,8),U1A(6,8),
     3U2(6,8),ANN0(6,8),PT0(6,8),TT0(6,8),ALPHA0(6,8),PTP(6,8)

      COMMON/RPMCOM/RPMK(8)
      REAL M0
      COMMON /SSTA01/CP0(8),w0(6),               PS0(6,8),V0(6,8),TS0(6,
     18),VU0(6,8),VZ0(6,8),RHOS0(6,8),PS1(6,8),WGT1(8),TA1(8),WG1(6,8),
     2            DPDR1(6,8),SI(6,8),  CP1(8),PHI1(6,8),TS1(6,8),V1(6,8)
     3,RHOS1(6,8),ALF1E(6,8),VU1(6,8),VZ1(6,8),M0(6,8)
      COMMON/TPT1/PT1(6),TT1(6,8)
C      COMMON /SOVRAL/DELHT(6,8),DELHTI(6,8),DELHSI(6,8),DEHATI(6,8),
C    1ETATT(6,8),ETATS(6,8),ETATAT(6,8),delhtu(6,8)

       COMMON /SOVRAL/TSOVRAL(384)        

C      COMMON /SSTA1A/VU1A(6,8),WG1A(6,8),WGT1A(8),VZ1A(6,8),  CP1A(8),
C    1PS1A(6,8),RU1A(6,8),R1A(6,8),BET1A(6,8),RI(6,8),TTR1A(6,8),PTR1A(6
C    2,8),MR1A(6,8)

       COMMON /SSTA1A/TSTA1A(544)
c
C      COMMON /SSTA2/V2(6,8),TTR2(6,8),PTR2(6,8),WG2(6,8),WGT2(8),TA2(8),
C    1           PS2(6,8),PHI2(6,8)

       COMMON /SSTA2/TSSTA2(304)
       COMMON /SSTA2A/TSTA2A(656)
       COMMON /C1FIT1/TC1FIT(427),IC1FIT(3)
       COMMON /C2FIT1/TC2FIT(42)
       COMMON /BDINST/IBDINS(244)
       COMMON RES(8),DALF(6),DSTS(6),DALFE(6),DCPS(6),DSTR(6),
     *DBT2(6),DCPR(6),DSR2(6)
C
       COMMON /SLSHFT/TSLSH(102)
C
       COMMON /DATTRF/ HP1, TT123(48), PT123(48), WG123(48), 
     *                 ETAS123(48), ETAR123(48), KS1

C      Common Block SNTCP variables:
         G = 0.
         AJ = 0.
         PRPC = 0.
         ICASE = 0 
         PREVER = 0.
         MFSTOP = 0 
         JUMP = 0 
         LOPIN = 0.
         ISCASE = 0.
         KN = 0.
         GAMF = 0.
         IP = 0.
         SCRIT = 0.
         PTRN = 0.
         ISECT = 0
         KSTG = 0
         WTOL = 0.
         PHOTOL = 0.
         PRTOL = 0.
         TRLOOP = 0.
         LSTG = 0.
         LBRC = 0
         IBRC = 0
         ICHOKE = 0
         ISORR = 0
         CHOKE = 0.
         TRDIAG = 0
         SC = 0
         RC = 0
         DELPR = 0
         PASS = 0
         IPC = 0
         LOPC = 0
         ISS = 0 
C
         WG = 0.
         EPR = 0.
         	

C     COMMON BLOCK GEBE
      IGEBE1 = 0
      DO K = 1,10
         TGEBE(K) = 0.
      ENDDO
      DO K = 1,9
         IGEBE(K) = 0.
      ENDDO
      DO K = 1,22
         TGEBE1(K) = 0.
      ENDDO

C     COMMON BLOCK  SDDIN 
      DO K = 1,8
         PS1P(K) = 0.
         PS2P(K) = 0.
         DO I = 1,6
            PT0PS1(I,K) = 0.
            PTRS2(I,K) = 0.
            TWG(I,K) = 0.
            PWG(I,K) = 0.
            RVU1(I,K) = 0.
            RVU2(I,K) = 0.
         ENDDO
      ENDDO

       DO K = 1,1751
          TSINPUT(K) = 0.
      ENDDO
C
C     COMMON BLOCK GETPR
C
      DO J = 1,8
       TGETPR(J) = 0.
      ENDDO
C
C     COMMON BLOCK Varaiables ...  SFLOW2
      DO K = 1,8
        CP2(K) = 0.
         DO I = 1,6
           TS2(I,K) = 0
           RHOS2(I,K) = 0.
           BET2E(I,K) = 0.
           R2(I,K) = 0.
           RU2(I,K) = 0.
           VU2(I,K) = 0.
           DPDR2(I,K) = 0.
           VZ2(I,K) = 0.
           MR2(I,K) = 0.
           MF2(I,K) = 0.
           M2(I,K) = 0.
        ENDDO
      ENDDO

C     COMMON BLOCK Varaiables ...  SINIT   
      DO K = 1,8
         RPMK(K) = 0.
         DO I = 1,6
            H1(I,K) = 0.
            H2(I,K) = 0.
            DP0(I,K) = 0.
            DP1(I,K) = 0.
            DP1A(I,K) = 0.
            DP2(I,K) = 0.
            DP2A(I,K) = 0.
            CSALF1(I,K) = 0.
            ALF1(I,K) = 0.
            CSBET2(I,K) = 0.
            BET2(I,K) = 0.
            RADSD(I,K) = 0.
            RADRD(I,K) = 0.
            ANN1(I,K) = 0.
            ANN2(I,K) = 0.
            ANN2A(I,K) = 0.
            ANN1A(I,K) = 0.
            U1A(I,K) = 0.
            U2(I,K) = 0.
            ANN0(I,K) = 0.
            PT0(I,K) = 0.
            TT0(I,K) = 0.
            ALPHA0(I,K) = 0.
            PTP(I,K) = 0.
c
            PTINH(I) = 0.
            TTINH(I) = 0.
            ALF0H(I) = 0.
        ENDDO
      ENDDO

C     COMMON BLOCK Varaiables ...  TPT1,SSTA01    
      DO K = 1,8
         CP0(K) = 0.
         CP1(K) = 0.
         WGT1(K) = 0.
         TA1(K) =  0.
         DO I = 1,6
            PT1(I) = 0.
            W0(I) = 0.
            TT1(I,K) = 0.
            PS0(I,K) = 0.
            V0(I,K) = 0.
            TS0(I,K) = 0.
            VU0(I,K) = 0.
            VZ0(I,K) = 0.
            RHOS0(I,K) = 0.
            PS1(I,K) = 0.
            WG1(I,K) = 0.
            DPDR1(I,K) = 0.0
            SI(I,K) = 0.
            PHI1(I,K) = 0.
            TS1(I,K) = 0.
            V1(I,K) = 0.
            RHOS1(I,K) = 0.
            ALF1E(I,K) = 0.
            VU1(I,K) = 0.
            VZ1(I,K) = 0.
            M0(I,K) = 0.
        ENDDO
      ENDDO

       DO K = 1,384   
          TSOVRAL(K) = 0.
      ENDDO
       DO K = 1,544   
         TSTA1A(K) = 0
      ENDDO

C     COMMON BLOCK Varaiables ... SSTA2     
       DO K = 1,304   
          TSSTA2(K) = 0.
      ENDDO
C     COMMON BLOCK Varaiables ...  SSTA2A    
       DO K = 1,656   
          TSTA2A(K) = 0.
      ENDDO
C     COMMON BLOCK Varaiables ... C1FIT1    
       DO K = 1,427   
          TC1FIT(K) = 0.
      ENDDO
       DO K = 1,3     
          IC1FIT(K) = 0.
      ENDDO
C     COMMON BLOCK Varaiables ... C2FIT1    
       DO K = 1,42   
          TC2FIT(K) = 0.
      ENDDO
C     COMMON BLOCK Varaiables ... BDINST    
       DO K = 1,244   
          IBDINS(K) = 0.
      ENDDO
C     COMMON BLOCK ... NO name
      DO K = 1,8      
         RES(K) = 0.
      ENDDO
      DO K = 1,6      
         DALF(K) = 0.
         DSTS(K) = 0.
         DALFE(K) = 0.
         DCPS(K) = 0.
         DSTR(K) = 0.
         DBT2(K) = 0.
         DCPR(K) = 0.
         DSR2(K) = 0.
      ENDDO

C     COMMON BLOCK ... SLSHFT
      DO K = 1,102    
         TSLSH(K) = 0.
      ENDDO
C     COMMON BLOCK ... DATTRF
      HP1 = 0
      KS1 = 0
      DO  K = 1, 48
        TT123(K) = 0.
        PT123(K) = 0.
        WG123(K) = 0.
        ETAS123(K) = 0.
        ETAR123(K) = 0.
      ENDDO
C
      RETURN
      END
