C SUBROUTINE AXOD     
C 
c
c *** This version of AXOD reads input from 'axod.inp' and writes output
c     to 'axod.out'.
c
C     NASA TURBINE PROGRAM
C
      SUBROUTINE AXOD (HPOWER, TOTT, TOTP, TOTF, EFFS, EFFR)
      REAL HPOWER, TOTT(48), TOTP(48), TOTF(48), EFFS(48), EFFR(48)
Cf2py intent(out) HPOWER
Cf2py intent(out) TOTT
Cf2py intent(out) TOTP
Cf2py intent(out) TOTF
Cf2py intent(out) EFFS
Cf2py intent(out) EFFR

      REAL MFSTOP
      LOGICAL PREVER
      COMMON /SNTCP/ G,AJ,PRPC,ICASE,PREVER,MFSTOP,JUMP,LOPIN,ISCASE,
     .KN,GAMF,IP,SCRIT,PTRN,ISECT,KSTG,WTOL,RHOTOL,PRTOL,TRLOOP,LSTG,
     .LBRC,IBRC,ICHOKE,ISORR,CHOKE,PT0PS1(6,8),PTRS2(6,8),TRDIAG,SC,RC,
     .DELPR,PASS,IPC,LOPC,ISS

      COMMON /SADIN2/ PTINH(6),TTINH(6),ALF0H(6)

      COMMON /SINPUT/
     .PTPS,PTIN,TTIN,WAIR,FAIR,DELC,DELL,DELA,AACS,VCTD,STG,SECT,EXPN,
     .EXPP,EXPRE,RG,RPM,PAF,SLI,STGCH,ENDJOB,XNAME(20),TITLE(20),
     .PCNH(6),GAM(6,8),DR(6,8),DT(6,8),RWG(6,8),ALPHAS(6,8),ALPHA1(6,8),
     .ETARS(6,8),ETAS(6,8),CFS(6,8),ANDO(6,8),BETA1(6,8),BETA2(6,8),ETAR
     .R(6,8),ETAR(6,8),CFR(6,8),TFR(6,8),ANDOR(6,8),OMEGAS(6,8),AS0(6,8)
     .,ASMP0(6,8),ACMN0(6,8),A1(6,8),A2(6,8),A3(6,8),A4(6,8),A5(6,8),A6(
     .6,8),OMEGAR(6,8),BSIA(6,8),BSMPIA(6,8),BCMNIA(6,8),B1(6,8),B2(6,8)
     .,B3(6,8),B4(6,8),B5(6,8),B6(6,8),SESTHI(8),RERTHI(8)
     .,fairx(5,8),wairx(5,8),rg1(8),rg1a(8),rg2(8),rg2a(8)

      COMMON /SADDIN/ PS1P(8),PS2P(8)

      COMMON /GETPR/ TRY1,TRY2,PTRY1,PTRY2,PFIND,DHFIND,PFIND1,
     .DHFND1

      COMMON /DESOPT/ RVU1(6,8),RVU2(6,8),WG,EPR

      REAL MR2,M2     ,MF2
      COMMON /SFLOW2/ TS2(6,8),CP2(8),R2(6,8),RHOS2(6,8),BET2E(6,8),
     .RU2(6,8),VU2(6,8),DPDR2(6,8),VZ2(6,8),MR2(6,8),MF2(6,8),M2(6,8)

      DIMENSION CS(8),CR(8)

      common/cfi/icf

      COMMON /TDIL/TWG (6,8),pwg(6,8)

      COMMON /PLOTT/ ENDPLT,IPCAS,IPSCAS(20),xnname(20),iplot(3)

      dimension tangms(6,8),tangmr(6,8),tangm1(6,8),tangm2(6,8),tang0(6)
      common/slope/tangms,tangmr,tangm1,tangm2,tang0,iar,icyl

      COMMON /DATTRF/ HP1, TT123(48), PT123(48), WG123(48), ETAS123(48),
     *                ETAR123(48), KS1

      OPEN(15,FILE='axod.inp',STATUS='OLD')
      OPEN(16,FILE='axod.out',STATUS='UNKNOWN')
c      open(10,file='etaout',status='UNKNOWN')

      WRITE (16,'(///,A,/,A,/,A)') 
     .'     Conceptual Analysis for Axial-flow Turbines (AXOD)',
     .'               A COMPUTER PROGRAM DEVELOPED AT',
     .'         NASA GLENN RESEARCH CENTER, CLEVELAND, OHIO'
      WRITE (16,'(A,///)') '             '

      CALL ICOMMON
C     print *,'  After  ICOMMON *******************'
      ENDPLT=0.0
      endjob=0.0 
      icf=0

      DO K=1,8
        DO I=1,6
           PTINH(I)=0.0
           TTINH(I)=0.0
           ALF0H(I)=0.0

            RVU1(I,K)=0.0
            RVU2(I,K)=0.0
             cfs(i,k)=1.0
             cfr(i,k)=1.0
           etars(i,k)=1.0
           etarr(i,k)=1.0
             tfr(i,k)=1.0
             twg(i,k)=0.0
             pwg(i,k)=0.0
          ALPHAS(I,K)=0.0
           BETA1(I,K)=0.0
        enddo

        ALPHA1(1,K)=0.0
          ANDO(1,K)=0.0
         BETA2(1,K)=0.0
         ANDOR(1,K)=0.0
      enddo

      WG=0.0
      WAIR=0.0
      FAIR=0.0
      PTPS=1.10
      DELC=0.1
      DELL=0.1
      DELA=0.05
      EXPN=4.0
      EXPP=3.0
      EXPRE=0.0
      epr=1.0
      RG=0.0
      PFIND =1000.0
      DHFIND =10000.0
      VCTD1=0.0
      JL=1
      JL1=1
      PAF=0.0

      SLI=1.0
      stgch=1.0
      iplot(1)=1
      iplot(2)=1
      iplot(3)=1
      AACS=1.0
      SECT=1.0
      VCTD=0.0
      WTOL=1.E-05
      RHOTOL=1.E-04
      PRTOL=1.E-06
      PCNH(1)=1.0
      GAM(1,1)=0.0
      RWG(1,1)=1.0
      ETAS(1,1)=0.0
      ALPHA1(1,1)=0.0
      ETAR(1,1)=0.0
      BETA2(1,1)=0.0
      omegas(1,1)=0.0
      omegar(1,1)=0.0
      TRLOOP=0.
      TRDIAG=0.0
      G=32.17405
      AJ=778.161
      ICASE=0
      iar=0
      icyl=0

1     continue
      PREVER=.FALSE.
      TRY1=1000.0
      TRY2=1000.0
      PTRY2=1.0
      PTRY1=1.0
      JLIND=0
      JL=1
      JL1=1
      THSSIT=0.0
C                                                    
C     print *,'  going to  INIT ******************'
      CALL INIT
C     print *,'  BACK from  INIT ******************'
C     print *,' endjob =',endjob
c     if (endjob.eq.0.0) stop

c     rwc mod
      if (endplt.eq.1.0) OPEN(7,FILE='pltfile',STATUS='UNKNOWN')

      M=IFIX(TRDIAG+.001)
      DELPR1=DELC
      CHOKE1=CHOKE
      SCRIT1=SCRIT
      ICHOK1=ICHOKE
      LOPIN1=LOPIN
      KN1=KN
      JUMP1=JUMP
      IBRC1=IBRC
      ISCASE=1
      IF (PREVER) GO TO 1

      DO I=1,8
        PS1P(I)= 0.
        PS2P(I)= 0.
        CS(I)=0.0
        CR(I)=0.0
      enddo

      PASS=0

2     continue
      PRPC=CS(KN)
      PRPC1=PRPC
C     print *,'  going to  sta01******************'
      CALL STA01
C     print *,'  Back from sta01******************'
C     if (endjob.eq.0.0) stop
      CS(KN)= PRPC
      IF(M.EQ.6.OR.M.EQ.1) CALL DIAGT(1)
      IF(M.EQ.6.OR.M.EQ.2) CALL DIAGT(2)
      IF (PREVER) GO TO 40
      IF(ICHOKE.NE.0)  GO TO 3
      IF(SCRIT.EQ.1.) SC=SC+1.

3     continue
      CALL STA1A
      IF(M.EQ.6.OR.M.EQ.3) CALL DIAGT(3)
      IF (PREVER) GO TO 40
      LOPIN=0

4     continue
      JUMP=0
      PRPC=CR(KN)
      CALL STA2
      CR(KN)=PRPC
      IF(M.EQ.6.OR.M.EQ.4) CALL DIAGT(4)
      IF (PREVER) GO TO 40
      IF ((1.-MF2(isect,KN)).lt.0) then
        goto 24
      endif

      IF (JUMP.gt.0) then
        goto 20
      endif
CC
      CALL STA2A
      IF(M.EQ.6.OR.M.EQ.5) CALL DIAGT(5)
      IF (PREVER) GO TO 40
      IF ((KN-KSTG).ge.0) GO TO 9

      KN=KN+1
      LOPIN=0

8     continue
      JUMP=0
      PRPC=CS(KN)
      CALL STA1
      CS(KN)=PRPC
      IF(M.EQ.6.OR.M.EQ.2) CALL DIAGT(2)
      IF (PREVER) GO TO 40
C     IF (JUMP)3,3,20
C     replaced by .........
      IF (JUMP.LE.0) THEN
         GO TO 3
      ELSE
         GO TO 20
      ENDIF

9     continue
      CALL OVRALL
      jll = 0
      IF (VCTD.gt.0) then
        CALL INSTG
      endif

      PASS=1.
      IF((M-7).eq.0) then
        CALL DIAGT(0)
      endif

      IF (THSSIT.GE.1.0)    GO TO 24
      IF (WG.GT.0.)         GO TO 24
      IF ((1.-MFSTOP).le.0) GO TO 24
      IF (DELC.le.0)        GO TO 24
      IF (DELL.gt.0) then
        IF (DELPR.le.0)     GO TO 24
      else
        IF (CHOKE.ne.0)     GO TO 24
      endif

      ISCASE=ISCASE+1

      IF ((PFIND.NE.1000.0) .or. (DHFIND.ne.10000.0)) then

        PTRY1=PTRY2

c       Array bounds error RWC 12/9/03
        if(jl1.gt.8) then
          ptry2=ptrs2(ip,jl1-8)
        else
          PTRY2=PT0PS1(IP,JL1)
        endif

        IF(DHFIND.NE.10000.0)PFIND=DHFIND

        IF (PFIND.le.TRY2) then

c         Array error mod here RWC 12/9/03
          if(jl1.gt.8) then
            PTrs2(IP,JL-8)=(PFIND-TRY1)/(TRY2-TRY1)*(PTRY2-PTRY1)+PTRY1
c            PT0PS1(IP,JL-8)=(PFIND-TRY1)/(TRY2-TRY1)*(PTRY2-PTRY1)+PTRY1
          else
            PT0PS1(IP,JL)=(PFIND-TRY1)/(TRY2-TRY1)*(PTRY2-PTRY1)+PTRY1
          endif

          VCTD=1.0
          THSSIT=THSSIT+1.0

          IF (JLIND.ne.1) then
            SCRIT=SCRIT2
            PRPC=PRPC2
            ICHOKE=ICHOK2
            LSTG=LSTG2
            LOPIN=LOPIN2
            JUMP=JUMP2
            CHOKE=CHOKE2
            LBRC=LBRC2
          endif

          GO TO 41

        endif

      endif

      JL=(ISORR-1)*8+LSTG
      JLIND=0
      JL2=JL1
      JL1=JL

      IF (JL1.ne.JL2) then

c       Array error mod here RWC 12/9/03
        if(jl1.gt.8) then
          ptry2=ptrs2(ip,jl1-8)
c          ptry2=pt0ps1(ip,jl-8)
        else
          PTRY2=PT0PS1(IP,JL)
        endif

        JLIND=1
      endif

      SCRIT2=SCRIT
      PRPC2=PRPC
      ICHOK2=ICHOKE
      LSTG2=LSTG
      LOPIN2=LOPIN
      JUMP2=JUMP
      CHOKE2=CHOKE
      LBRC2=LBRC
      IF(SC.EQ.1.) DELPR=DELL

c     RWC array error mod here 12/10/03
      if(jl1.gt.8) then
        PTRS2(IP,JL-8)=PTRS2(IP,JL-8)+DELPR
c        PT0PS1(IP,JL-8)=PT0PS1(IP,JL-8)+DELPR
      else
        PT0PS1(IP,JL)=PT0PS1(IP,JL)+DELPR
      endif

20    continue
      LOPIN=1

41    continue
      KN=LSTG
      IBRC=LBRC
      IPC=0
      IF ((ISORR-1).le.0) then
        IF((KN-1).le.0) then
          goto 2
        else
          goto 8
        endif
      endif
      goto 4

40    continue
      WRITE(16,106)
106   FORMAT(//3X,'THE PREVIOUS CASE HAS BEEN TERMINATED DUE TO ERRORS',
     .'- CHECK DUMP.'//' **IF THIS IS THE FIRST POINT ON A SPEED LINE, T
     .HE INPUT VALUE OF PTPS MAY BE TOO LOW**')

24    continue
      IPSCAS(ICASE)=ISCASE
      IF(MF2(isect,KN).GT.1.) IPSCAS(ICASE)=ISCASE-1
      IF ((ENDJOB-1.).lt.0) then
        goto 1
      endif

      IPCAS=ICASE
C     print *,'  endplt =',endplt
      IF ((ENDPLT-1.).lt.0) then
c     IF ((ENDPLT-1.).le.0) then
        print *,'  closing unit 6 & unit 5 files...'
        CLOSE (UNIT=15)
        CLOSE (UNIT=16)
        stop
      endif

      DO I=1,20
        XNNAME(I)=XNAME(I)
      enddo

      
      CALL PLOTER
c     print *,'   RETURNING from axod.f ****KSTG =', KSTG
      CALL GETDATA(HPOWER, TOTT, TOTP, TOTF, EFFS, EFFR, KSTG)
C     print *,'  HPOWER =', HPOWER
C     print *,' TOTT =',(TOTT(III),III=1,KSTG)
C     print *,' TOTP =',(TOTP(III),III=1,KSTG)
C     print *,' TOTF =',(TOTF(III),III=1,KSTG)
C     print *,' EFFS =',(EFFS(III),III=1,KSTG)
C     print *,' EFFR =',(EFFR(III),III=1,KSTG)
      CLOSE (UNIT=15)
      close(16)
      close(7)

      RETURN
C     STOP
      END

