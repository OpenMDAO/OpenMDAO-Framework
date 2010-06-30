C           SUBROUTINE INST2
      SUBROUTINE INST2
      REAL MFSTOP
      LOGICAL PREVER
      COMMON /SNTCP/G,AJ,PRPC,ICASE,PREVER,MFSTOP,JUMP,LOPIN,ISCASE,
     1KN,GAMF,IP,SCRIT,PTRN,ISECT,KSTG,WTOL,RHOTOL,PRTOL,TRLOOP,LSTG,
     2LBRC,IBRC,ICHOKE,ISORR,CHOKE,PT0PS1(6,8),PTRS2(6,8),TRDIAG,SC,RC,
     3DELPR,PASS,IPC,LOPC,ISS
C
      COMMON/GEBE/DRBARR,DRBARS,E1,E2,E3,E4,E5,E6,HBRRH,HBRSH,IAFE
     1,IALF,IBT2,ICPR,ICPS,ISR2,ISTR,ISTS,L,R1,R3,R4,R5,
     2TALF,WGT0(8),WT(7),HBARS,HBARR,I
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
     3),CPO(8),DELHVD(6,8)
C
      COMMON /SOVRAL/DELHT(6,8),DELHTI(6,8),DELHSI(6,8),DEHATI(6,8),
     1ETATT(6,8),ETATS(6,8),ETATAT(6,8),delhtu(6,8)
C
      COMMON /BDINST/
     1IFMI01(4),IFMI02(4),IFMI03(4),IFMI04(4),IFMI05(4),IFMI06(4),
     2IFMI07(4),IFMI08(4),IFMI09(4),IFMI10(4),IFMI11(4),IFMI12(4),
     3IFMI13(4),IFMI14(4),IFMI15(4),IFMI16(4),IFMI17(4),IFMI18(4),
     4IFMI19(4),IFMI20(4),IFMI21(4),IFMI22(4),IFMI23(4),IFMI24(4),
     5IFMI25(4),IFMI26(4),IFMI27(4),IFMI28(4),IFMI29(4),IFMI30(4),
     6IFMI31(4),IFMI32(4),IFMI33(4),IFMI34(4),IFMI35(4),IFMI36(4),
     7IFMI37(4),IFMI38(4),IFMI39(4),IFMI40(4),IFMI41(4),IFMI42(4),
     8IFMI43(4),IFMI44(4),IFMI45(4),IFMI46(4),IFMI47(4),IFMI48(4),
     9IFMI49(4),IFMI50(4),IFMI51(4),IFMI52(4),IFMI53(4),IFMI54(4),
     1IFMI55(4),IFMI56(4),IFMI57(4),IFMI58(4),IFMI59(4),IFMI60(4),
     2IFMI61(4)
C
      COMMON /C1FIT1/ STDP0(7),STTT0(7),STPT0(7),STALF(7)
     1,STSI(7),STV0(7),STVU0(7),STVZ0(7),STTS0(7),
     2STPS0(7),STDEN0(7),STM0(7),STDP1(7),STTT1(7),
     3STALFE(7),STDELA(7),STV1(7),STVU1(7),STVZ1(7),
     4STTS1(7),STPS1(7),STDEN1(7),STM1(7),ZWIINC(7),
     5CPS(7),STDP1A(7),STTTR1(7),STPTR1(7),STBET1(7),
     6STRI(7),STR1A(7),STRU1A(7),STMR1A(7),STU1A(7),
     7STDP2(7),STBET2(7),SDBETA(7),SR2(7),SRU2(7),
     8SMR2(7),SU2(7),RX(7),STDELH(7),STPSI(7),
     9SETATT(7),SETATS(7),SETAAT(7),RZWINC(7),CPR(7),
     1STDP2A(7),STPT2(7),STTT2(7),STV2(7),STVU2(7),
     2STALF2(7),STMF2(7),STVZ2(7),STTS2(7),STPS2(7),
     3STDEN2(7),STM2(7),LJ,JJ,K
      COMMON /C2FIT1/  DBARS(7),FTANS(7),FAXS(7),DBARR(7),
     1FTANR(7),FAXR(7)
      COMMON RES(8),DALF(6),DSTS(6),DALFE(6),DCPS(6),DSTR(6),
     *DBT2(6),DCPR(6),DSR2(6)
      COMMON/TPT1/PT1(6),TT1(6,8)
      COMMON/DESOPT/RVU1(6,8),RVU2(6,8),WG,EPR
      dimension tangms(6,8),tangmr(6,8),tangm1(6,8),tangm2(6,8),tang0(6)
      common/slope/tangms,tangmr,tangm1,tangm2,tang0,iar,icyl
C
      DIMENSION IFMTA(4),IFMI1(4,61),Y(7,61)
      dimension slopd0(6),slopd1(6,8),slopd2(6,8)
      EQUIVALENCE (IFMI1(1,1),IFMI01(1)),(Y(1,1),STDP0(1))
      COMMON /DATTRF/ HP1, TT123(48), PT123(48), WG123(48), ETAS123(48),
     *                ETAR123(48), KS1

C
C
      DATA DOR/57.29578/
C                              
      LJ=1
      kj=1
      if (isect.eq.1) kj=3
      IF(ISECT.EQ.2) GO TO 20
      IF(ISECT.EQ.3) GO TO 20
c      if(isect.eq.1) go to 30
      JJ=7
      GO TO 40
20    JJ=5
c      go to 40
c30    jj=3
40    CONTINUE
      IF(ISECT.EQ.6) GO TO 50
      CALL FIT1
10    CONTINUE
      CALL FIT2
      IT=ISECT-2*(ISECT/2)
C     IF(IT)60,60,62
C     replaced by ...............
      IF(IT.LE.0) THEN
C60    GO TO(70,9,8,8,50,9,50),L
        GO TO(70,9,8,8,50,9,50),L
      ELSE
C62    GO TO(70,83,82,9,63,8,65),L
        GO TO(70,83,82,9,63,8,65),L
      ENDIF
C63    IF(ISECT-1)84,84,50
C     replaced by ........
63    IF(ISECT.LE.1)  THEN  
        GO TO 84
      ELSE
        GO TO 50
      ENDIF
C65    IF(ISECT-1)75,50,50
C     replaced by ........
65    IF(ISECT.LT.1)  THEN  
        GO TO 75
      ELSE
        GO TO 50
      ENDIF
C        CALCULATE   TIP   VALUES
70    I=ISECT
c     if(isect.eq.1) go to 43
      IF(ISECT.EQ.2) GO TO 45
      IF(ISECT.EQ.3) GO TO 45
      L=7
      GO TO 46
c3    l=3
c     go to 46
45    L=5
46    CONTINUE
      STDP0(L)=DT(1,K)
      R1=DP0(I,K)/DT(1,K)
      STDP1(L)=DT(2,K)
      R2(1,1)=DP1(I,K)/DT(2,K)
      STDP1A(L)=DT(3,K)
      R3=DP1A(I,K)/DT(3,K)
      STDP2(L)=DT(4,K)
      R4=DP2(I,K)/DT(4,K)
      TALF=SIN(ALF1(I,K))*R3/COS(ALF1(I,K))
      STDP2A(L)=DT(5,K)
      R5=DP2A(I,K)/DT(5,K)
      L1=L-3
      IF(ISECT.EQ.4)L1=3
      L2=L-2
      L3=L-1
      GO TO 10
50    IF(ISECT.EQ.6)L=7
      PIO4= 45./DOR
      FAXDHS= PIO4*(STPS0(1)+STPS1(1))*DBARS(1)*(STDP1(1)-STDP0(1))
      FAXDTS=-PIO4*(STPS0(L)+STPS1(L))*DBARS(L)*(STDP1(L)-STDP0(L))
      FAXDHR= PIO4*(STPS1(1)+STPS2(1))*DBARR(1)*(STDP2(1)-STDP1A(1))
      FAXDTR=-PIO4*(STPS1(L)+STPS2(L))*DBARR(L)*(STDP2(L)-STDP1A(L))
      IF(ISECT.EQ.3) GO TO 8
      IF(ISECT.EQ.5) GO TO 8
      IF(ISECT.EQ.1) GO TO 75
      I=ISECT/2
C     IF(ISECT-4)52,52,53
C     replaced by ........
      IF(ISECT.LE.4) THEN  
        GO TO 52
      ELSE
        GO TO 53
      ENDIF
52    L=I+2
      GO TO 54
53    L=I+1
54    CONTINUE
      STDP0(L)=(DR(1,K)+DT(1,K))/2.
      R1=DP0(I,K)/STDP0(L)
      STDP1(L)=(DR(2,K)+DT(2,K))/2.
      R2(1,1)=DP1(I,K)/STDP1(L)
      STDP1A(L)=(DR(3,K)+DT(3,K))/2.
      R3=DP1A(I,K)/STDP1A(L)
      STDP2(L)=(DR(4,K)+DT(4,K))/2.
      R4=DP2(I,K)/STDP2(L)
      STDP2A(L)=(DR(5,K)+DT(5,K))/2.
      R5=DP2A(I,K)/((DR(5,K)+DT(5,K))/2.)
      GO TO 10
75    I=1
      L=3
      PH=.3
80    STDP0(L)=PH*(DT(1,K)-DR(1,K))+DR(1,K)
      R1=DP0(I,K)/STDP0(L)
      STDP1(L)=PH*(DT(2,K)-DR(2,K))+DR(2,K)
      R2(1,1)=DP1(I,K)/STDP1(L)
      STDP1A(L)=PH*(DT(3,K)-DR(3,K))+DR(3,K)
      R3=DP1A(I,K)/STDP1A(L)
      STDP2(L)=PH*(DT(4,K)-DR(4,K))+DR(4,K)
      R4=DP2(I,K)/STDP2(L)
      TALF=SIN(ALF1(I,K))*R3/COS(ALF1(I,K))
      STDP2A(L)=PH*(DT(5,K)-DR(5,K))+DR(5,K)
      R5=DP2A(I,K)/(PH*(DT(5,K)-DR(5,K))+DR(5,K))
      GO TO 10
82    L=2
      PH=.1
      GO TO 80
83    L=5
      PH=.7
      GO TO 80
84    L=6
      PH=.9
      GO TO 80
8     CONTINUE
      FTANTS=0.
      FAXTS=0.
      FTANTR=0.
      FAXTR=0.
      DO 121 L=LJ,JJ
      IF(L.EQ.1) GO TO 121
      DER=(DBARS(L)-DBARS(L-1))/4.
      FTANTS=FTANTS+(FTANS(L)+FTANS(L-1))*DER
      FAXTS= FAXTS+(FAXS(L)+FAXS(L-1))*DER
      DER=(DBARR(L)-DBARR(L-1))/4.
      FTANTR= FTANTR+(FTANR(L)+FTANR(L-1))*DER
      FAXTR= FAXTR+(FAXR(L)+FAXR(L-1))*DER
121   CONTINUE
      IF(ISECT.NE.6) GO TO 1211
      DH= HBARS/(DBARS(7)-DBARS(1))/2.
      FTANTS= FTANTS*DH
      FAXTS= FAXTS*DH
      DH= HBARR/(DBARR(7)-DBARR(1))/2.
      FTANTR= FTANTR*DH
      FAXTR= FAXTR*DH
1211  CONTINUE
      do 210 iw=1,isect
      slopd1(iw,k)=atan(tangm1(iw,k))*dor
      slopd2(iw,k)=atan(tangm2(iw,k))*dor
      if (k.eq.1) slopd0(iw)=atan(tang0(iw))*dor
  210 continue
      WRITE(16,1000) XNAME,TITLE,ICASE,ISCASE
1000  FORMAT(1H1,20X,29H     TURBINE COMPUTER PROGRAM/
     16X,20A4/6X,20A4/30X,5HCASE ,I3,1H.,I3/24X,
     223HINTER-STAGE PERFORMANCE/ )
      WRITE(16,1001) K
1001  FORMAT(5X,5HSTA 0,2X,12HSTATOR INLET,10X,
     15HSTAGE,I3,1H. )
      WRITE(16,301) (Y(KW,1),KW=lj,jj,kj)
      if (k.eq.1) write(16,1301) (slopd0(kw),kw=1,isect)
      if (k.gt.1) write(16,1301) (slopd2(kw,k-1),kw=1,isect)
      if (k.eq.1) write(16,1302) (w0(kw),kw=1,isect)
      if (k.gt.1) write(16,1302) (wg2a(kw,k-1),kw=1,isect)
      WRITE(16,302) (Y(KW,2),KW=lj,jj,kj)
      WRITE(16,303) (Y(KW,3),KW=lj,jj,kj)
      WRITE(16,304) (Y(KW,4),KW=lj,jj,kj)
      WRITE(16,305) (Y(KW,5),KW=lj,jj,kj)
      WRITE(16,306) (Y(KW,6),KW=lj,jj,kj)
      WRITE(16,307) (Y(KW,7),KW=lj,jj,kj)
      WRITE(16,308) (Y(KW,8),KW=lj,jj,kj)
      WRITE(16,309) (Y(KW,9),KW=lj,jj,kj)
      WRITE(16,310) (Y(KW,10),KW=lj,jj,kj)
      WRITE(16,311) (Y(KW,11),KW=lj,jj,kj)
      WRITE(16,312) (Y(KW,12),KW=lj,jj,kj)
      WRITE(16,1010)
1010  FORMAT(/5X,5HSTA 1,2X,11HSTATOR EXIT )
      WRITE(16,313) (Y(KW,13),KW=lj,jj,kj)
      write(16,1313) (slopd1(kw,k),kw=1,isect)
      write(16,1314) (wg1(kw,k),kw=1,isect)
      WRITE(16,314) (Y(KW,14),KW=lj,jj,kj)
      WRITE(16,315) (Y(KW,15),KW=lj,jj,kj)
      WRITE(16,316) (Y(KW,16),KW=lj,jj,kj)
      WRITE(16,317) (Y(KW,17),KW=lj,jj,kj)
      WRITE(16,318) (Y(KW,18),KW=lj,jj,kj)
      WRITE(16,319) (Y(KW,19),KW=lj,jj,kj)
      WRITE(16,320) (Y(KW,20),KW=lj,jj,kj)
      WRITE(16,321) (Y(KW,21),KW=lj,jj,kj)
      WRITE(16,322) (Y(KW,22),KW=lj,jj,kj)
      WRITE(16,323) (Y(KW,23),KW=lj,jj,kj)
      WRITE(16,324) (Y(KW,24),KW=lj,jj,kj)
      WRITE(16,325) (Y(KW,25),KW=lj,jj,kj)
      WRITE(16,2000)
2000  FORMAT(/12X,13HSTATOR FORCES )
      WRITE(16,2005) (DBARS(I),I=lj,jj,kj)
2005  FORMAT(1X,7HAVG DIA,7F9.3)
      WRITE(16,2006) (FTANS(I),I=lj,jj,kj)
2006  FORMAT(1X,7HFTAN/IN,7F9.1)
      WRITE(16,2007) FTANTS
2007  FORMAT(28X,7H  F TAN,F9.1)
      WRITE(16,2008) (FAXS(I),I=lj,jj,kj)
2008  FORMAT(1X,7H FAX/IN,7F9.1)
      WRITE(16,2009) FAXTS,FAXDHS,FAXDTS
2009  FORMAT(31X,4HF AX,F9.1/2X,6HF DRUM,F9.1,45X,F9.1)
      WRITE(16,1000)XNAME,TITLE,ICASE,ISCASE
      WRITE(16,1015) K
1015  FORMAT(4X,6HSTA 1A,2X,11HROTOR INLET,10X,5HSTAGE,I3 )
      WRITE(16,326) (Y(KW,26),KW=lj,jj,kj)
      write(16,1326)(slopd1(kw,k),kw=1,isect)
      write(16,1327)(wg1a(kw,k),kw=1,isect)
      WRITE(16,327) (Y(KW,27),KW=lj,jj,kj)
      WRITE(16,328) (Y(KW,28),KW=lj,jj,kj)
      WRITE(16,329) (Y(KW,29),KW=lj,jj,kj)
      WRITE(16,330) (Y(KW,30),KW=lj,jj,kj)
      WRITE(16,331) (Y(KW,31),KW=lj,jj,kj)
      WRITE(16,332) (Y(KW,32),KW=lj,jj,kj)
      WRITE(16,333) (Y(KW,33),KW=lj,jj,kj)
      WRITE(16,334) (Y(KW,34),KW=lj,jj,kj)
      WRITE(16,1020)
1020  FORMAT(/5X,5HSTA 2,2X,10HROTOR EXIT )
      WRITE(16,335) (Y(KW,35),KW=lj,jj,kj)
      write(16,1335)(slopd2(kw,k),kw=1,isect)
      write(16,1336)(wg2(kw,k),kw=1,isect)
      WRITE(16,336) (Y(KW,36),KW=lj,jj,kj)
      WRITE(16,337) (Y(KW,37),KW=lj,jj,kj)
      WRITE(16,338) (Y(KW,38),KW=lj,jj,kj)
      WRITE(16,339) (Y(KW,39),KW=lj,jj,kj)
      WRITE(16,340) (Y(KW,40),KW=lj,jj,kj)
      WRITE(16,341) (Y(KW,41),KW=lj,jj,kj)
      WRITE(16,342) (Y(KW,42),KW=lj,jj,kj)
      WRITE(16,343) (Y(KW,43),KW=lj,jj,kj)
      WRITE(16,344) (Y(KW,44),KW=lj,jj,kj)
      WRITE(16,345) (Y(KW,45),KW=lj,jj,kj)
      WRITE(16,346) (Y(KW,46),KW=lj,jj,kj)
      WRITE(16,347) (Y(KW,47),KW=lj,jj,kj)
      WRITE(16,348) (Y(KW,48),KW=lj,jj,kj)
      WRITE(16,349) (Y(KW,49),KW=lj,jj,kj)
      WRITE(16,2010)
2010  FORMAT(/12X,12HROTOR FORCES )
      WRITE(16,2005) (DBARR(I),I=lj,jj,kj)
      WRITE(16,2006) (FTANR(I),I=lj,jj,kj)
      WRITE(16,2007) FTANTR
      WRITE(16,2008) (FAXR(I),I=lj,jj,kj)
      WRITE(16,2009) FAXTR,FAXDHR,FAXDTR
      WRITE(16,2020)
2020  FORMAT(/4X,6HSTA 2A,2X,10HSTAGE EXIT)
      WRITE(16,350) (Y(KW,50),KW=lj,jj,kj)
      write(16,1350)(slopd2(kw,k),kw=1,isect)
      write(16,1351)(wg2a(kw,k),kw=1,isect)
c     print *,'  wg2a...  isect =',isect,' k=',k
      DO 555 I = 1,isect
         WG123(I) = WG2A(I,k)
555   CONTINUE
      WRITE(16,351) (Y(KW,51),KW=lj,jj,kj)
      
c     print *,'   lj,jj,kj  =',lj,jj,kj
      WRITE(16,352) (Y(KW,52),KW=lj,jj,kj)
C
C     save PT2A () values
      nii = 0
      DO 556 I = lJ,JJ,KJ
         nii = nii + 1
         KW = I
         PT123(NII) = Y(KW,51)
556   CONTINUE
C
C     save TT2A () values
      nii = 0
      DO 557 I = lJ,JJ,KJ
         nii = nii + 1
         KW = I
         TT123(NII) = Y(KW,52)
557   CONTINUE
      WRITE(16,353) (Y(KW,53),KW=lj,jj,kj)
      WRITE(16,354) (Y(KW,54),KW=lj,jj,kj)
      WRITE(16,355) (Y(KW,55),KW=lj,jj,kj)
      WRITE(16,356) (Y(KW,56),KW=lj,jj,kj)
      WRITE(16,357) (Y(KW,57),KW=lj,jj,kj)
      WRITE(16,358) (Y(KW,58),KW=lj,jj,kj)
      WRITE(16,359) (Y(KW,59),KW=lj,jj,kj)
      WRITE(16,360) (Y(KW,60),KW=lj,jj,kj)
      WRITE(16,361) (Y(KW,61),KW=lj,jj,kj)
  301 FORMAT (1X,7H DIAM 0,7F9.3)
 1301 format (1x,7hSLOPE 0,9x,6f9.2)
 1302 format (1x,7h   WG 0,9x,6f9.3)
  302 FORMAT (1X,7H   TT 0,7F9.1)
  303 FORMAT (1X,7H   PT 0,7F9.3)
  304 FORMAT (1X,7HALPHA 0,7F9.3)
  305 FORMAT (1X,7HI STA 0,7F9.3)
  306 FORMAT (1X,7H    V 0,7F9.3)
  307 FORMAT (1X,7H   VU 0,7F9.3)
  308 FORMAT (1X,7H   VZ 0,7F9.3)
  309 FORMAT (1X,7H   TS 0,7F9.1)
  310 FORMAT (1X,7H   PS 0,7F9.3)
  311 FORMAT (1X,7H DENS 0,7F9.5)
  312 FORMAT (1X,7H    M 0,7F9.5)
  313 FORMAT (1X,7H DIAM 1,7F9.3)
 1313 format (1x,7hSLOPE 1,9x,6f9.2)
 1314 format (1x,7h   WG 1,9x,6f9.3)
  314 FORMAT (1X,7H   TT 1,7F9.1)
  315 FORMAT (1X,7HALPHA 1,7F9.3)
  316 FORMAT (1X,7H  DEL A,7F9.3)
  317 FORMAT (1X,7H    V 1,7F9.3)
  318 FORMAT (1X,7H   VU 1,7F9.3)
  319 FORMAT (1X,7H   VZ 1,7F9.3)
  320 FORMAT (1X,7H   TS 1,7F9.1)
  321 FORMAT (1X,7H   PS 1,7F9.3)
  322 FORMAT (1X,7H DENS 1,7F9.5)
  323 FORMAT (1X,7H    M 1,7F9.5)
  324 FORMAT (1X,7HZWI INC,7F9.4)
  325 FORMAT (1X,7H  ETA S,7F9.4)
  326 FORMAT (1X,7HDIAM 1A,7F9.3)
 1326 format (1x,7hSLOP 1A,9x,6f9.2)
 1327 format (1x,7h  WG 1A,9x,6f9.3)
  327 FORMAT (1X,7H TTR 1A,7F9.1)
  328 FORMAT (1X,7H PTR 1A,7F9.3)
  329 FORMAT (1X,7HBETA 1A,7F9.3)
  330 FORMAT (1X,7HI ROTOR,7F9.3)
  331 FORMAT (1X,7H   R 1A,7F9.3)
  332 FORMAT (1X,7H  RU 1A,7F9.3)
  333 FORMAT (1X,7H  MR 1A,7F9.5)
  334 FORMAT (1X,7H   U 1A,7F9.3)
  335 FORMAT (1X,7H DIAM 2,7F9.3)
 1335 format (1x,7hSLOPE 2,9x,6f9.2)
 1336 format (1x,7h   WG 2,9x,6f9.3)
  336 FORMAT (1X,7H BETA 2,7F9.3)
  337 FORMAT (1X,7H  DBETA,7F9.3)
  338 FORMAT (1X,7H    R 2,7F9.3)
  339 FORMAT (1X,7H   RU 2,7F9.3)
  340 FORMAT (1X,7H   MR 2,7F9.5)
  341 FORMAT (1X,7H    U 2,7F9.3)
  342 FORMAT (1X,7H     RX,7F9.5)
  343 FORMAT (1X,7H  DEL H,7F9.3)
  344 FORMAT (1X,7H    PSI,7F9.5)
  345 FORMAT (1X,7H ETA TT,7F9.5)
  346 FORMAT (1X,7H ETA TS,7F9.5)
  347 FORMAT (1X,7H ETA AT,7F9.5)
  348 FORMAT (1X,7HZWI INC,7F9.4)
  349 FORMAT (1X,7H  ETA R,7F9.4)
  350 FORMAT (1X,7HDIAM 2A,7F9.3)
 1350 format (1x,7hSLOP 2A,9x,6f9.2)
 1351 format (1x,7h  WG 2A,9x,6f9.3)
  351 FORMAT (1X,7H  PT 2A,7F9.3)
  352 FORMAT (1X,7H  TT 2A,7F9.1)
  353 FORMAT (1X,7H   V 2A,7F9.3)
  354 FORMAT (1X,7H  VU 2A,7F9.3)
  355 FORMAT (1X,7HALPH 2A,7F9.3)
  356 FORMAT (1X,7H  MF 2A,7F9.5)
  357 FORMAT (1X,7H  VZ 2A,7F9.3)
  358 FORMAT (1X,7H  TS 2A,7F9.1)
  359 FORMAT (1X,7H  PS 2A,7F9.3)
  360 FORMAT (1X,7HDENS 2A,7F9.5)
  361 FORMAT (1X,7H   M 2A,7F9.5)
9     K=K+1
      RETURN
      END
