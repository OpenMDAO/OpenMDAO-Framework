c----- CONMIN double precision version
      SUBROUTINE CONMIN (X,VLB,VUB,G,SCAL,DF,A,S,G1,G2,B,C,ISC,IC,MS1,N1
     1,N2,N3,N4,N5) 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      revision history
c       double precision version for workstations
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit           double precision(a-h,o-z)
c
      COMMON /CNMN1/ DELFUN,DABFUN,FDCH,FDCHM,CT,CTMIN,CTL,CTLMIN,ALPHAX
     1,ABOBJ1,THETA,OBJ,NDV,NCON,NSIDE,IPRINT,NFDG,NSCAL,LINOBJ,ITMAX,IT
     2RM,ICNDIR,IGOTO,NAC,INFO,INFOG,ITER
C
      DIMENSION X(N1), VLB(N1), VUB(N1), G(N2), SCAL(N1), DF(N1), A(N1,N
     13), S(N1), G1(N2), G2(N2), B(N3,N3), C(N4), ISC(N2), IC(N3), MS1(N
     25)
      COMMON /CONSAV/ DM1,DM2,DM3,DM4,DM5,DM6,DM7,DM8,DM9,DM10,DM11,DM12
     1,DCT,DCTL,PHI,ABOBJ,CTA,CTAM,CTBM,OBJ1,SLOPE,DX,DX1,FI,XI,DFTDF1,A
     2LP,FFF,A1,A2,A3,A4,F1,F2,F3,F4,CV1,CV2,CV3,CV4,APP,ALPCA,ALPFES,AL
     3PLN,ALPMIN,ALPNC,ALPSAV,ALPSID,ALPTOT,RSPACE,IDM1,IDM2,IDM3,JDIR,I
     4OBJ,KOBJ,KCOUNT,NCAL(2),NFEAS,MSCAL,NCOBJ,NVC,KOUNT,ICOUNT,IGOOD1,
     5IGOOD2,IGOOD3,IGOOD4,IBEST,III,NLNC,JGOTO,ISPACE(2) 
C     ROUTINE TO SOLVE CONSTRAINED OR UNCONSTRAINED FUNCTION
C     MINIMIZATION. 
C     BY G. N. VANDERPLAATS                          APRIL, 1972. 
C     * * * * * * * * * * *   JUNE, 1979 VERSION   * * * * * * * * * * *
C     NASA-AMES RESEARCH CENTER, MOFFETT FIELD, CALIF.
C     REFERENCE;  CONMIN - A FORTRAN PROGRAM FOR CONSTRAINED FUNCTION 
C         MINIMIZATION:  USER'S MANUAL,  BY G. N. VANDERPLAATS, 
C         NASA TM X-62,282, AUGUST, 1973. 
C     STORAGE REQUIREMENTS: 
C         PROGRAM - 7000 DECIMAL WORDS (CDC COMPUTER) 
C         ARRAYS  - APPROX. 2*(NDV**2)+26*NDV+4*NCON, 
C               WHERE N3 = NDV+2. 


Cf2py intent(in,out)  X
Cf2py intent(in)  VLB
Cf2py intent(in)  VUB
Cf2py intent(in)  G
Cf2py intent(in,out)  SCAL
Cf2py intent(in)  DF
Cf2py intent(in,out)  A
Cf2py intent(in,out) S
Cf2py intent(in,out) G1
Cf2py intent(in,out) G2
Cf2py intent(in,out) B
Cf2py intent(in,out) C
Cf2py intent(in,out)  ISC
Cf2py intent(in,out)  IC
Cf2py intent(in,out) MS1

      if (iprint.gt.100) then
      write(*,*) 'CALLED CONMIN WITH THE FOLLOWING ARGUMENTS:'
      write(*,*) 'X'
      write(*,'(6f10.3)') (x(i),i=1,n1)
      write(*,*) 'VLB'
      write(*,'(6f10.3)') (vlb(i),i=1,n1)
      write(*,*) 'VUB'
      write(*,'(6f10.3)') (vub(i),i=1,n1)
      write(*,*) 'G'
      write(*,'(6f10.3)') (g(i),i=1,n2)
      write(*,*) 'SCAL'
      write(*,'(6f10.3)') (scal(i),i=1,n1)
      write(*,*) 'DF'
      write(*,'(6f10.3)') (df(i),i=1,n1)
      write(*,*) 'A'
      do i = 1,n1
        write(*,'(6f10.3)') (a(i,j),j=1,n3)
        write(*,*)
      enddo
      write(*,*) 'S'
      write(*,'(6f10.3)') (s(i),i=1,n1)
      write(*,*) 'G1'
      write(*,'(6f10.3)') (g1(i),i=1,n2)
      write(*,*) 'G2'
      write(*,'(6f10.3)') (g2(i),i=1,n2)
      write(*,*) 'B'
      do i = 1,n3
        write(*,'(6f10.3)') (b(i,j),j=1,n3)
        write(*,*)
      enddo
      write(*,*) 'C'
      write(*,'(6f10.3)') (c(i),i=1,n4)
      write(*,*) 'ISC'
      write(*,'(20i4)') (isc(i),i=1,n2)
      write(*,*) 'IC'
      write(*,'(20i4)') (ic(i),i=1,n3)
      write(*,*) 'MS1'
      write(*,'(20i4)') (MS1(i),i=1,n5)
      endif

C     RE-SCALE VARIABLES IF REQUIRED. 
      do i = 1,ndv
        write(*,'(f10.4)') x(i)
      enddo
      IF (NSCAL.EQ.0.OR.IGOTO.EQ.0) GO TO 20
      DO 10 I=1,NDV 
10    X(I)=C(I) 
20    CONTINUE
C     CONSTANTS.
      NDV1=NDV+1
      NDV2=NDV+2
      IF (IGOTO.EQ.0) GO TO 40
C     ------------------------------------------------------------------
C                     CHECK FOR UNBOUNDED SOLUTION
C     ------------------------------------------------------------------
C     STOP IF OBJ IS LESS THAN -1.0D+40 
      IF (OBJ.GT.-1.0D+40) GO TO 30 
      WRITE (6,980) 
      GO TO 810 
30    CONTINUE
      GO TO (160,390,380,670,690),IGOTO 
C     ------------------------------------------------------------------
C                      SAVE INPUT CONTROL PARAMETERS
C     ------------------------------------------------------------------
40    CONTINUE
      IF (IPRINT.GT.0) WRITE (6,1220) 
      IF (LINOBJ.EQ.0.OR.(NCON.GT.0.OR.NSIDE.GT.0)) GO TO 50
C     TOTALLY UNCONSTRAINED FUNCTION WITH LINEAR OBJECTIVE. 
C     SOLUTION IS UNBOUNDED.
      WRITE (6,970) LINOBJ,NCON,NSIDE 
      RETURN
50    CONTINUE
      IDM1=ITRM 
      IDM2=ITMAX
      IDM3=ICNDIR 
      DM1=DELFUN
      DM2=DABFUN
      DM3=CT
      DM4=CTMIN 
      DM5=CTL 
      DM6=CTLMIN
      DM7=THETA 
      DM8=PHI 
      DM9=FDCH
      DM10=FDCHM
      DM11=ABOBJ1 
      DM12=ALPHAX 
C     ------------------------------------------------------------------
C                                DEFAULTS 
C     ------------------------------------------------------------------
      IF (ITRM.LE.0) ITRM=3 
      IF (ITMAX.LE.0) ITMAX=20
      NDV1=NDV+1
      IF (ICNDIR.EQ.0) ICNDIR=NDV1
      IF (DELFUN.LE.0.) DELFUN=.0001
      CT=-ABS(CT) 
      IF (CT.GE.0.) CT=-.1
      CTMIN=ABS(CTMIN)
      IF (CTMIN.LE.0.) CTMIN=.004 
      CTL=-ABS(CTL) 
      IF (CTL.GE.0.) CTL=-0.01
      CTLMIN=ABS(CTLMIN)
      IF (CTLMIN.LE.0.) CTLMIN=.001 
      IF (THETA.LE.0.) THETA=1. 
      IF (ABOBJ1.LE.0.) ABOBJ1=.1 
      IF (ALPHAX.LE.0.) ALPHAX=.1 
      IF (FDCH.LE.0.) FDCH=.01
      IF (FDCHM.LE.0.) FDCHM=.01
C     ------------------------------------------------------------------
C                     INITIALIZE INTERNAL PARAMETERS
C     ------------------------------------------------------------------
      INFOG=0 
      ITER=0
      JDIR=0
      IOBJ=0
      KOBJ=0
      NDV2=NDV+2
      KCOUNT=0
      NCAL(1)=0 
      NCAL(2)=0 
      NAC=0 
      NFEAS=0 
      MSCAL=NSCAL 
      CT1=ITRM
      CT1=1./CT1
      DCT=(CTMIN/ABS(CT))**CT1
      DCTL=(CTLMIN/ABS(CTL))**CT1 
      PHI=5.
      ABOBJ=ABOBJ1
      NCOBJ=0 
      CTAM=ABS(CTMIN) 
      CTBM=ABS(CTLMIN)
C     CALCULATE NUMBER OF LINEAR CONSTRAINTS, NLNC. 
      NLNC=0
      IF (NCON.EQ.0) GO TO 70 
      DO 60 I=1,NCON
      IF (ISC(I).GT.0) NLNC=NLNC+1
60    CONTINUE
70    CONTINUE
C     ------------------------------------------------------------------
C          CHECK TO BE SURE THAT SIDE CONSTRAINTS ARE SATISFIED 
C     ------------------------------------------------------------------
      IF (NSIDE.EQ.0) GO TO 110 
      DO 100 I=1,NDV
      IF (VLB(I).LE.VUB(I)) GO TO 80
      XX=.5*(VLB(I)+VUB(I)) 
      X(I)=XX 
      VLB(I)=XX 
      VUB(I)=XX 
      WRITE (6,1120) I
80    CONTINUE
      XX=X(I)-VLB(I)
      IF (XX.GE.0.) GO TO 90
C     LOWER BOUND VIOLATED. 
      WRITE (6,1130) X(I),VLB(I),I
      X(I)=VLB(I) 
      GO TO 100 
90    CONTINUE
      XX=VUB(I)-X(I)
      IF (XX.GE.0.) GO TO 100 
      WRITE (6,1140) X(I),VUB(I),I
      X(I)=VUB(I) 
100   CONTINUE
110   CONTINUE
C     ------------------------------------------------------------------
C                        INITIALIZE SCALING VECTOR, SCAL
C     ------------------------------------------------------------------
      IF (NSCAL.EQ.0) GO TO 150 
      IF (NSCAL.LT.0) GO TO 130 
      DO 120 I=1,NDV
120   SCAL(I)=1.
      GO TO 150 
130   CONTINUE
      DO 140 I=1,NDV
      SI=ABS(SCAL(I)) 
      IF (SI.LT.1.0D-20) SI=1.0D-5
      SCAL(I)=SI
      SI=1./SI
      X(I)=X(I)*SI
      IF (NSIDE.EQ.0) GO TO 140 
      VLB(I)=VLB(I)*SI
      VUB(I)=VUB(I)*SI
140   CONTINUE
150   CONTINUE
C     ------------------------------------------------------------------
C     ***** CALCULATE INITIAL FUNCTION AND CONSTRAINT VALUES  ***** 
C     ------------------------------------------------------------------
      INFO=1
      NCAL(1)=1 
      IGOTO=1 
      GO TO 950 
160   CONTINUE
      OBJ1=OBJ
      IF (DABFUN.LE.0.) DABFUN=.001*ABS(OBJ)
      IF (DABFUN.LT.1.0D-10) DABFUN=1.0D-10 
      IF (IPRINT.LE.0) GO TO 270
C     ------------------------------------------------------------------
C                    PRINT INITIAL DESIGN INFORMATION 
C     ------------------------------------------------------------------
      IF (IPRINT.LE.1) GO TO 230
      IF (NSIDE.EQ.0.AND.NCON.EQ.0) WRITE (6,1290)
      IF (NSIDE.NE.0.OR.NCON.GT.0) WRITE (6,1230) 
      WRITE (6,1240) IPRINT,NDV,ITMAX,NCON,NSIDE,ICNDIR,NSCAL,NFDG,LINOB
     1J,ITRM,N1,N2,N3,N4,N5 
      WRITE (6,1260) CT,CTMIN,CTL,CTLMIN,THETA,PHI,DELFUN,DABFUN
      WRITE (6,1250) FDCH,FDCHM,ALPHAX,ABOBJ1 
      IF (NSIDE.EQ.0) GO TO 190 
      WRITE (6,1270)
      DO 170 I=1,NDV,6
      M1=MIN0(NDV,I+5)
170   WRITE (6,1010) I,(VLB(J),J=I,M1)
      WRITE (6,1280)
      DO 180 I=1,NDV,6
      M1=MIN0(NDV,I+5)
180   WRITE (6,1010) I,(VUB(J),J=I,M1)
190   CONTINUE
      IF (NSCAL.GE.0) GO TO 200 
      WRITE (6,1300)
      WRITE (6,1460) (SCAL(I),I=1,NDV)
200   CONTINUE
      IF (NCON.EQ.0) GO TO 230
      IF (NLNC.EQ.0.OR.NLNC.EQ.NCON) GO TO 220
      WRITE (6,1020)
      DO 210 I=1,NCON,15
      M1=MIN0(NCON,I+14)
210   WRITE (6,1030) I,(ISC(J),J=I,M1)
      GO TO 230 
220   IF (NLNC.EQ.NCON) WRITE (6,1040)
      IF (NLNC.EQ.0) WRITE (6,1050) 
230   CONTINUE
      WRITE (6,1440) OBJ
      WRITE (6,1450)
      DO 240 I=1,NDV
      X1=1. 
      IF (NSCAL.NE.0) X1=SCAL(I)
240   G1(I)=X(I)*X1 
      DO 250 I=1,NDV,6
      M1=MIN0(NDV,I+5)
250   WRITE (6,1010) I,(G1(J),J=I,M1) 
      IF (NCON.EQ.0) GO TO 270
      WRITE (6,1470)
      DO 260 I=1,NCON,6 
      M1=MIN0(NCON,I+5) 
260   WRITE (6,1010) I,(G(J),J=I,M1)
270   CONTINUE
      IF (IPRINT.GT.1) WRITE (6,1360) 
C     ------------------------------------------------------------------
C     ********************  BEGIN MINIMIZATION  ************************
C     ------------------------------------------------------------------
280   CONTINUE
      ITER=ITER+1 
      IF (ABOBJ1.LT..0001) ABOBJ1=.0001 
      IF (ABOBJ1.GT..2) ABOBJ1=.2 
      IF (ALPHAX.GT.1.) ALPHAX=1. 
      IF (ALPHAX.LT..001) ALPHAX=.001 
C 
C  THE FOLLOWING TWO LINES OF CODE WERE COMMENTED OUT ON 3/5/81 
C 
C     NFEAS=NFEAS+1 
C     IF (NFEAS.GT.10) GO TO 810
      IF (IPRINT.GT.2) WRITE (6,1310) ITER
      IF (IPRINT.GT.3.AND.NCON.GT.0) WRITE (6,1320) CT,CTL,PHI
      CTA=ABS(CT) 
      IF (NCOBJ.EQ.0) GO TO 340 
C     ------------------------------------------------------------------
C     NO MOVE ON LAST ITERATION.  DELETE CONSTRAINTS THAT ARE NO
C     LONGER ACTIVE.
C     ------------------------------------------------------------------
      NNAC=NAC
      DO 290 I=1,NNAC 
      IF (IC(I).GT.NCON) NAC=NAC-1
290   CONTINUE
      IF (NAC.LE.0) GO TO 420 
      NNAC=NAC
      DO 330 I=1,NNAC 
300   NIC=IC(I) 
      CT1=CT
      IF (ISC(NIC).GT.0) CT1=CTL
      IF (G(NIC).GT.CT1) GO TO 330
      NAC=NAC-1 
      IF (I.GT.NAC) GO TO 420 
      DO 320 K=I,NAC
      II=K+1
      DO 310 J=1,NDV2 
310   A(J,K)=A(J,II)
320   IC(K)=IC(II)
      GO TO 300 
330   CONTINUE
      GO TO 420 
340   CONTINUE
      IF (MSCAL.LT.NSCAL.OR.NSCAL.EQ.0) GO TO 360 
      IF (NSCAL.LT.0.AND.KCOUNT.LT.ICNDIR) GO TO 360
      MSCAL=0 
      KCOUNT=0
C     ------------------------------------------------------------------
C                          SCALE VARIABLES
C     ------------------------------------------------------------------
      DO 350 I=1,NDV
      SI=SCAL(I)
      XI=SI*X(I)
      SIB=SI
      IF (NSCAL.GT.0) SI=ABS(XI)
      IF (SI.LT.1.0D-10) GO TO 350
      SCAL(I)=SI
      SI=1./SI
      X(I)=XI*SI
      IF (NSIDE.EQ.0) GO TO 350 
      VLB(I)=SIB*SI*VLB(I)
      VUB(I)=SIB*SI*VUB(I)
350   CONTINUE
      IF (IPRINT.LT.4.OR.(NSCAL.LT.0.AND.ITER.GT.1)) GO TO 360
      WRITE (6,1330)
      WRITE (6,1460) (SCAL(I),I=1,NDV)
360   CONTINUE
      MSCAL=MSCAL+1 
      NAC=0 
C     ------------------------------------------------------------------
C          OBTAIN GRADIENTS OF OBJECTIVE AND ACTIVE CONSTRAINTS 
C     ------------------------------------------------------------------
      INFO=2
      NCAL(2)=NCAL(2)+1 
      IF (NFDG.NE.1) GO TO 370
      IGOTO=2 
      GO TO 950 
370   CONTINUE
      JGOTO=0 
380   CONTINUE
      CALL CNMN01 (JGOTO,X,DF,G,ISC,IC,A,G1,VLB,VUB,SCAL,C,NCAL,DX,DX1,F
     1I,XI,III,N1,N2,N3,N4) 
      IGOTO=3 
      IF (JGOTO.GT.0) GO TO 950 
390   CONTINUE
      INFO=1
      IF (NAC.GE.N3) GO TO 810
      IF (NSCAL.EQ.0.OR.NFDG.EQ.0) GO TO 420
C     ------------------------------------------------------------------
C                              SCALE GRADIENTS
C     ------------------------------------------------------------------
C     SCALE GRADIENT OF OBJECTIVE FUNCTION. 
      DO 400 I=1,NDV
400   DF(I)=DF(I)*SCAL(I) 
      IF (NFDG.EQ.2.OR.NAC.EQ.0) GO TO 420
C     SCALE GRADIENTS OF ACTIVE CONSTRAINTS.
      DO 410 J=1,NDV
      SCJ=SCAL(J) 
      DO 410 I=1,NAC
410   A(J,I)=A(J,I)*SCJ 
420   CONTINUE
      IF (IPRINT.LT.3.OR.NCON.EQ.0) GO TO 470 
C     ------------------------------------------------------------------
C                                   PRINT 
C     ------------------------------------------------------------------
C     PRINT ACTIVE AND VIOLATED CONSTRAINT NUMBERS. 
      M1=0
      M2=N3 
      IF (NAC.EQ.0) GO TO 450 
      DO 440 I=1,NAC
      J=IC(I) 
      IF (J.GT.NCON) GO TO 440
      GI=G(J) 
      C1=CTAM 
      IF (ISC(J).GT.0) C1=CTBM
      GI=GI-C1
      IF (GI.GT.0.) GO TO 430 
C     ACTIVE CONSTRAINT.
      M1=M1+1 
      MS1(M1)=J 
      GO TO 440 
430   M2=M2+1 
C     VIOLATED CONSTRAINT.
      MS1(M2)=J 
440   CONTINUE
450   M3=M2-N3
      WRITE (6,1060) M1 
      IF (M1.EQ.0) GO TO 460
      WRITE (6,1070)
      WRITE (6,1480) (MS1(I),I=1,M1)
460   WRITE (6,1080) M3 
      IF (M3.EQ.0) GO TO 470
      WRITE (6,1070)
      M3=N3+1 
      WRITE (6,1480) (MS1(I),I=M3,M2) 
470   CONTINUE
C     ------------------------------------------------------------------
C            CALCULATE GRADIENTS OF ACTIVE SIDE CONSTRAINTS 
C     ------------------------------------------------------------------
      IF (NSIDE.EQ.0) GO TO 530 
      MCN1=NCON 
      M1=0
      DO 510 I=1,NDV
C     LOWER BOUND.
      XI=X(I) 
      XID=VLB(I)
      X12=ABS(XID)
      IF (X12.LT.1.) X12=1. 
      GI=(XID-XI)/X12 
      IF (GI.LT.-1.0D-6) GO TO 490
      M1=M1+1 
      MS1(M1)=-I
      NAC=NAC+1 
      IF (NAC.GE.N3) GO TO 810
      MCN1=MCN1+1 
      DO 480 J=1,NDV
480   A(J,NAC)=0. 
      A(I,NAC)=-1.
      IC(NAC)=MCN1
      G(MCN1)=GI
      ISC(MCN1)=1 
C     UPPER BOUND.
490   XID=VUB(I)
      X12=ABS(XID)
      IF (X12.LT.1.) X12=1. 
      GI=(XI-XID)/X12 
      IF (GI.LT.-1.0D-6) GO TO 510
      M1=M1+1 
      MS1(M1)=I 
      NAC=NAC+1 
      IF (NAC.GE.N3) GO TO 810
      MCN1=MCN1+1 
      DO 500 J=1,NDV
500   A(J,NAC)=0. 
      A(I,NAC)=1. 
      IC(NAC)=MCN1
      G(MCN1)=GI
      ISC(MCN1)=1 
510   CONTINUE
C     ------------------------------------------------------------------
C                                  PRINT
C     ------------------------------------------------------------------
C     PRINT ACTIVE SIDE CONSTRAINT NUMBERS. 
      IF (IPRINT.LT.3) GO TO 530
      WRITE (6,1090) M1 
      IF (M1.EQ.0) GO TO 530
      WRITE (6,1100)
      WRITE(6,1480) (MS1(J),J=1,M1) 
530   CONTINUE
C     PRINT GRADIENTS OF ACTIVE AND VIOLATED CONSTRAINTS. 
      IF (IPRINT.LT.4) GO TO 570
      WRITE (6,1340)
      DO 540 I=1,NDV,6
      M1=MIN0(NDV,I+5)
540   WRITE (6,1010) I,(DF(J),J=I,M1) 
      IF (NAC.EQ.0) GO TO 570 
      WRITE (6,1350)
      DO 560 I=1,NAC
      M1=IC(I)
      M2=M1-NCON
      M3=0
      IF (M2.GT.0) M3=IABS(MS1(M2)) 
      IF (M2.LE.0) WRITE (6,990) M1 
      IF (M2.GT.0) WRITE (6,1000) M3
      DO 550 K=1,NDV,6
      M1=MIN0(NDV,K+5)
550   WRITE (6,1010) K,(A(J,I),J=K,M1)
560   WRITE (6,1360)
570   CONTINUE
C     ------------------------------------------------------------------
C     ******************  DETERMINE SEARCH DIRECTION *******************
C     ------------------------------------------------------------------
      ALP=1.0D+20 
      IF (NAC.GT.0) GO TO 580 
C     ------------------------------------------------------------------
C                        UNCONSTRAINED FUNCTION 
C     ------------------------------------------------------------------
C     FIND DIRECTION OF STEEPEST DESCENT OR CONJUGATE DIRECTION.
C 
C  S. N. 575 ADDED ON 2/25/81 
C 
 575  NVC=0 
      NFEAS=0 
      KCOUNT=KCOUNT+1 
C     IF KCOUNT.GT.ICNDIR  RESTART CONJUGATE DIRECTION ALGORITHM. 
      IF (KCOUNT.GT.ICNDIR.OR.IOBJ.EQ.2) KCOUNT=1 
      IF (KCOUNT.EQ.1) JDIR=0 
C     IF JDIR = 0 FIND DIRECTION OF STEEPEST DESCENT. 
      CALL CNMN02 (JDIR,SLOPE,DFTDF1,DF,S,N1) 
      GO TO 630 
580   CONTINUE
C     ------------------------------------------------------------------
C                          CONSTRAINED FUNCTION 
C     ------------------------------------------------------------------
C     FIND USABLE-FEASIBLE DIRECTION. 
      KCOUNT=0
      JDIR=0
      PHI=10.*PHI 
      IF (PHI.GT.1000.) PHI=1000. 
C 
C  THE FOLLOWING LINE OF CODE WAS COMMENTED OUT ON 3/5/81 
C 
C     IF (NFEAS.EQ.1) PHI=5.
C     CALCULATE DIRECTION, S. 
      CALL CNMN05 (G,DF,A,S,B,C,SLOPE,PHI,ISC,IC,MS1,NVC,N1,N2,N3,N4,N5)
C 
C  THE FOLLOWING LINE WAS ADDED ON 2/25/81
C 
      IF(NAC.EQ.0) GO TO 575
C 
C  THE FOLLOWING FIVE LINES WERE COMMENTED OUT ON 3/5/81
C  REASON : THEY WERE NOT IN G. VANDERPLAATS LISTING
C 
C     IF THIS DESIGN IS FEASIBLE AND LAST ITERATION WAS INFEASIBLE, 
C     SET ABOBJ1=.05 (5 PERCENT). 
C     IF (NVC.EQ.0.AND.NFEAS.GT.1) ABOBJ1=.05 
C     IF (NVC.EQ.0) NFEAS=0 
      IF (IPRINT.LT.3) GO TO 600
      WRITE (6,1370)
      DO 590 I=1,NAC,6
      M1=MIN0(NAC,I+5)
590   WRITE (6,1010) I,(A(NDV1,J),J=I,M1) 
      WRITE (6,1210) S(NDV1)
600   CONTINUE
C     ------------------------------------------------------------------
C     ****************** ONE-DIMENSIONAL SEARCH ************************
C     ------------------------------------------------------------------
      IF (S(NDV1).LT.1.0D-6.AND.NVC.EQ.0) GO TO 710 
C     ------------------------------------------------------------------
C                 FIND ALPHA TO OBTAIN A FEASIBLE DESIGN
C     ------------------------------------------------------------------
      IF (NVC.EQ.0) GO TO 630 
      ALP=-1. 
      DO 620 I=1,NAC
      NCI=IC(I) 
      C1=G(NCI) 
      CTC=CTAM
      IF (ISC(NCI).GT.0) CTC=CTBM 
      IF (C1.LE.CTC) GO TO 620
      ALP1=0. 
      DO 610 J=1,NDV
610   ALP1=ALP1+S(J)*A(J,I) 
      ALP1=ALP1*A(NDV2,I) 
      IF (ABS(ALP1).LT.1.0D-20) GO TO 620 
      ALP1=-C1/ALP1 
      IF (ALP1.GT.ALP) ALP=ALP1 
620   CONTINUE
630   CONTINUE
C     ------------------------------------------------------------------
C                       LIMIT CHANCE TO ABOBJ1*OBJ
C     ------------------------------------------------------------------
      ALP1=1.0D+20
      SI=ABS(OBJ) 
      IF (SI.LT..01) SI=.01 
      IF (ABS(SLOPE).GT.1.0D-20) ALP1=ABOBJ1*SI/SLOPE 
      ALP1=ABS(ALP1)
      IF (NVC.GT.0) ALP1=10.*ALP1 
      IF (ALP1.LT.ALP) ALP=ALP1 
C     ------------------------------------------------------------------
C                   LIMIT CHANGE IN VARIABLE TO ALPHAX
C     ------------------------------------------------------------------
      ALP11=1.0D+20 
      DO 640 I=1,NDV
      SI=ABS(S(I))
      XI=ABS(X(I))
      IF (SI.LT.1.0D-10.OR.XI.LT.0.1) GO TO 640 
      ALP1=ALPHAX*XI/SI 
      IF (ALP1.LT.ALP11) ALP11=ALP1 
640   CONTINUE
      IF (NVC.GT.0) ALP11=10.*ALP11 
      IF (ALP11.LT.ALP) ALP=ALP11 
      IF (ALP.GT.1.0D+20) ALP=1.0D+20 
      IF (ALP.LE.1.0D-20) ALP=1.0D-20 
      IF (IPRINT.LT.3) GO TO 660
      WRITE (6,1380)
      DO 650 I=1,NDV,6
      M1=MIN0(NDV,I+5)
650   WRITE (6,1010) I,(S(J),J=I,M1)
      WRITE (6,1110) SLOPE,ALP
660   CONTINUE
      IF (NCON.GT.0.OR.NSIDE.GT.0) GO TO 680
C     ------------------------------------------------------------------
C           DO ONE-DIMENSIONAL SEARCH FOR UNCONSTRAINED FUNCTION
C     ------------------------------------------------------------------
      JGOTO=0 
670   CONTINUE
      CALL CNMN03 (X,S,SLOPE,ALP,FFF,A1,A2,A3,A4,F1,F2,F3,F4,APP,N1,NCAL
     1,KOUNT,JGOTO) 
      IGOTO=4 
      IF (JGOTO.GT.0) GO TO 950 
      JDIR=1
C     PROCEED TO CONVERGENCE CHECK. 
      GO TO 700 
C     ------------------------------------------------------------------
C       SOLVE ONE-DIMENSIONAL SEARCH PROBLEM FOR CONSTRAINED FUNCTION 
C     ------------------------------------------------------------------
680   CONTINUE
      JGOTO=0 
690   CONTINUE
      CALL CNMN06 (X,VLB,VUB,G,SCAL,DF,S,G1,G2,CTAM,CTBM,SLOPE,ALP,A2,A3
     1,A4,F1,F2,F3,CV1,CV2,CV3,CV4,ALPCA,ALPFES,ALPLN,ALPMIN,ALPNC,ALPSA
     2V,ALPSID,ALPTOT,ISC,N1,N2,NCAL,NVC,ICOUNT,IGOOD1,IGOOD2,IGOOD3,IGO
     3OD4,IBEST,III,NLNC,JGOTO) 
      IGOTO=5 
      IF (JGOTO.GT.0) GO TO 950 
      IF (NAC.EQ.0) JDIR=1
C     ------------------------------------------------------------------
C     *******************     UPDATE ALPHAX   **************************
C     ------------------------------------------------------------------
700   CONTINUE
710   CONTINUE
      IF (ALP.GT.1.0D+19) ALP=0.
C     UPDATE ALPHAX TO BE AVERAGE OF MAXIMUM CHANGE IN X(I) 
C     AND ALHPAX. 
      ALP11=0.
      DO 720 I=1,NDV
      SI=ABS(S(I))
      XI=ABS(X(I))
      IF (XI.LT.1.0D-10) GO TO 720
      ALP1=ALP*SI/XI
      IF (ALP1.GT.ALP11) ALP11=ALP1 
720   CONTINUE
      ALP11=.5*(ALP11+ALPHAX) 
      ALP12=5.*ALPHAX 
      IF (ALP11.GT.ALP12) ALP11=ALP12 
      ALPHAX=ALP11
      NCOBJ=NCOBJ+1 
C     ABSOLUTE CHANGE IN OBJECTIVE. 
      OBJD=OBJ1-OBJ 
      OBJB=ABS(OBJD)
      IF (OBJB.LT.1.0D-10) OBJB=0.
      IF (NAC.EQ.0.OR.OBJB.GT.0.) NCOBJ=0 
      IF (NCOBJ.GT.1) NCOBJ=0 
C     ------------------------------------------------------------------
C                                  PRINT
C     ------------------------------------------------------------------
C     PRINT MOVE PARAMETER, NEW X-VECTOR AND CONSTRAINTS. 
      IF (IPRINT.LT.3) GO TO 730
      WRITE (6,1390) ALP
730   IF (IPRINT.LT.2) GO TO 800
      IF (OBJB.GT.0.) GO TO 740 
      IF (IPRINT.EQ.2) WRITE (6,1400) ITER,OBJ
      IF (IPRINT.GT.2) WRITE (6,1410) OBJ 
      GO TO 760 
740   IF (IPRINT.EQ.2) GO TO 750
      WRITE (6,1420) OBJ
      GO TO 760 
750   WRITE (6,1430) ITER,OBJ 
760   WRITE (6,1450)
      DO 770 I=1,NDV
      FF1=1.
      IF (NSCAL.NE.0) FF1=SCAL(I) 
770   G1(I)=FF1*X(I)
      DO 780 I=1,NDV,6
      M1=MIN0(NDV,I+5)
780   WRITE (6,1010) I,(G1(J),J=I,M1) 
      IF (NCON.EQ.0) GO TO 800
      WRITE (6,1470)
      DO 790 I=1,NCON,6 
      M1=MIN0(NCON,I+5) 
790   WRITE (6,1010) I,(G(J),J=I,M1)
800   CONTINUE
C 
C  THE FOLLOWING CODE WAS ADDED ON 3/5/81 
C 
C  IT HAD NOT BEEN REPORTED AS A FIX TO MAOB
C  BUT WAS SENT TO JEFF STROUD A YEAR AGO 
C  SEE OTHER COMMENTS IN CONMIN SUBROUTINE FOR DELETIONS OF CODE
C  ON 3/5/81 PERTAINING TO THIS FIX 
C 
C 
C                   CHECK FEASIBILITY 
C 
      IF(NCON.LE.0) GO TO 808 
      NFEASCT = 10
c  added by slp 11/17/94
      DO 804 I=1,NCON 
      C1=CTAM 
      IF(ISC(I).GT.0) C1=CTBM 
      IF(G(I).LE.C1) GO TO 804
      NFEAS=NFEAS+1 
      GO TO 806 
 804  CONTINUE
      IF(NFEAS.GT.0) ABOBJ1=.05 
ccc
      NFEAS=0 
      PHI=5.
 806  IF(NFEAS.GE.NFEASCT) GO TO 810 
 808  CONTINUE
C 
C  END OF INSERTED FIX
C 
C     ------------------------------------------------------------------
C                          CHECK CONVERGENCE
C     ------------------------------------------------------------------
C     STOP IF ITER EQUALS ITMAX.
      IF (ITER.GE.ITMAX) GO TO 810
C     ------------------------------------------------------------------
C                     ABSOLUTE CHANGE IN OBJECTIVE
C     ------------------------------------------------------------------
      OBJB=ABS(OBJD)
      KOBJ=KOBJ+1 
      IF (OBJB.GE.DABFUN.OR.NFEAS.GT.0) KOBJ=0
C     ------------------------------------------------------------------
C                     RELATIVE CHANGE IN OBJECTIVE
C     ------------------------------------------------------------------
      IF (ABS(OBJ1).GT.1.0D-10) OBJD=OBJD/ABS(OBJ1) 
      ABOBJ1=.5*(ABS(ABOBJ)+ABS(OBJD))
      ABOBJ=ABS(OBJD) 
      IOBJ=IOBJ+1 
      IF (NVC.GT.0.OR.OBJD.GE.DELFUN) IOBJ=0
      IF (IOBJ.GE.ITRM.OR.KOBJ.GE.ITRM) GO TO 810 
      OBJ1=OBJ
C     ------------------------------------------------------------------
C           REDUCE CT IF OBJECTIVE FUNCTION IS CHANGING SLOWLY
C     ------------------------------------------------------------------
      IF (IOBJ.LT.1.OR.NAC.EQ.0) GO TO 280
      CT=DCT*CT 
      CTL=CTL*DCTL
      IF (ABS(CT).LT.CTMIN) CT=-CTMIN 
      IF (ABS(CTL).LT.CTLMIN) CTL=-CTLMIN 
      GO TO 280 
810   CONTINUE
      IF (NAC.GE.N3) WRITE (6,1490) 
C     ------------------------------------------------------------------
C     ****************  FINAL FUNCTION INFORMATION  ********************
C     ------------------------------------------------------------------
      IF (NSCAL.EQ.0) GO TO 830 
C     UN-SCALE THE DESIGN VARIABLES.
      DO 820 I=1,NDV
      XI=SCAL(I)
      IF (NSIDE.EQ.0) GO TO 820 
      VLB(I)=XI*VLB(I)
      VUB(I)=XI*VUB(I)
820   X(I)=XI*X(I)
C     ------------------------------------------------------------------
C                           PRINT FINAL RESULTS 
C     ------------------------------------------------------------------
830   IF (IPRINT.EQ.0.OR.NAC.GE.N3) GO TO 940 
      WRITE (6,1500)
      WRITE (6,1420) OBJ
      WRITE (6,1450)
      DO 840 I=1,NDV,6
      M1=MIN0(NDV,I+5)
840   WRITE (6,1010) I,(X(J),J=I,M1)
      IF (NCON.EQ.0) GO TO 900
      WRITE (6,1470)
      DO 850 I=1,NCON,6 
      M1=MIN0(NCON,I+5) 
850   WRITE (6,1010) I,(G(J),J=I,M1)
C     DETERMINE WHICH CONSTRAINTS ARE ACTIVE AND PRINT. 
      NAC=0 
      NVC=0 
      DO 870 I=1,NCON 
      CTA=CTAM
      IF (ISC(I).GT.0) CTA=CTBM 
      GI=G(I) 
      IF (GI.GT.CTA) GO TO 860
      IF (GI.LT.CT.AND.ISC(I).EQ.0) GO TO 870 
      IF (GI.LT.CTL.AND.ISC(I).GT.0) GO TO 870
      NAC=NAC+1 
      IC(NAC)=I 
      GO TO 870 
860   NVC=NVC+1 
      MS1(NVC)=I
870   CONTINUE
      WRITE (6,1060) NAC
      IF (NAC.EQ.0) GO TO 880 
      WRITE (6,1070)
      WRITE (6,1480) (IC(J),J=1,NAC)
880   WRITE (6,1080) NVC
      IF (NVC.EQ.0) GO TO 890 
      WRITE (6,1070)
      WRITE (6,1480) (MS1(J),J=1,NVC) 
890   CONTINUE
900   CONTINUE
      IF (NSIDE.EQ.0) GO TO 930 
C     DETERMINE WHICH SIDE CONSTRAINTS ARE ACTIVE AND PRINT.
      NAC=0 
      DO 920 I=1,NDV
      XI=X(I) 
      XID=VLB(I)
      X12=ABS(XID)
      IF (X12.LT.1.) X12=1. 
      GI=(XID-XI)/X12 
      IF (GI.LT.-1.0D-6) GO TO 910
      NAC=NAC+1 
      MS1(NAC)=-I 
910   XID=VUB(I)
      X12=ABS(XID)
      IF (X12.LT.1.) X12=1. 
      GI=(XI-XID)/X12 
      IF (GI.LT.-1.0D-6) GO TO 920
      NAC=NAC+1 
      MS1(NAC)=I
920   CONTINUE
      WRITE (6,1090) NAC
      IF (NAC.EQ.0) GO TO 930 
      WRITE (6,1100)
      WRITE (6,1480) (MS1(J),J=1,NAC) 
930   CONTINUE
      WRITE (6,1150)
      IF (ITER.GE.ITMAX) WRITE (6,1160) 
      IF (NFEAS.GE.NFEASCT) WRITE (6,1170) 
      IF (IOBJ.GE.ITRM) WRITE (6,1180) ITRM 
      IF (KOBJ.GE.ITRM) WRITE (6,1190) ITRM 
      WRITE (6,1200) ITER 
      WRITE (6,1510) NCAL(1)
      IF (NCON.GT.0) WRITE (6,1520) NCAL(1) 
      IF (NFDG.NE.0) WRITE (6,1530) NCAL(2) 
      IF (NCON.GT.0.AND.NFDG.EQ.1) WRITE (6,1540) NCAL(2) 
C     ------------------------------------------------------------------
C                   RE-SET BASIC PARAMETERS TO INPUT VALUES 
C     ------------------------------------------------------------------
940   ITRM=IDM1 
      ITMAX=IDM2
      ICNDIR=IDM3 
      DELFUN=DM1
      DABFUN=DM2
      CT=DM3
      CTMIN=DM4 
      CTL=DM5 
      CTLMIN=DM6
      THETA=DM7 
      PHI=DM8 
      FDCH=DM9
      FDCHM=DM10
      ABOBJ1=DM11 
      ALPHAX=DM12 
      IGOTO=0 
950   CONTINUE
      IF (NSCAL.EQ.0.OR.IGOTO.EQ.0) RETURN
C     UN-SCALE VARIABLES. 
      DO 960 I=1,NDV
      C(I)=X(I) 
960   X(I)=X(I)*SCAL(I) 
      RETURN
C     ------------------------------------------------------------------
C                                FORMATS
C     ------------------------------------------------------------------
C 
C 
970   FORMAT (///5X,72HA COMPLETELY UNCONSTRAINED FUNCTION WITH A LINEAR
     1 OBJECTIVE IS SPECIFIED//10X,8HLINOBJ =,I5/10X,8HNCON   =,I5/10X,8
     2HNSIDE  =,I5//5X,35HCONTROL RETURNED TO CALLING PROGRAM)
980   FORMAT (///5X,56HCONMIN HAS ACHIEVED A SOLUTION OF OBJ LESS THAN -
     11.0E+40/5X,32HSOLUTION APPEARS TOABE UNBOUNDED/5X,26HOPTIMIZATION 
     2IS TERMINATED)
990   FORMAT (5X,17HCONSTRAINT NUMBER,I5) 
1000  FORMAT (5X,27HSIDE CONSTRAINT ON VARIABLE,I5) 
1010  FORMAT (3X,I5,1H),2X,6E13.5)
1020  FORMAT (/5X,35HLINEAR CONSTRAINT IDENTIFIERS (ISC)/5X,36HNON-ZERO 
     1INDICATES LINEAR CONSTRAINT)
1030  FORMAT (3X,I5,1H),2X,15I5)
1040  FORMAT (/5X,26HALL CONSTRAINTS ARE LINEAR)
1050  FORMAT (/5X,30HALL CONSTRAINTS ARE NON-LINEAR)
1060  FORMAT (/5X,9HTHERE ARE,I5,19H ACTIVE CONSTRAINTS)
1070  FORMAT (5X,22HCONSTRAINT NUMBERS ARE) 
1080  FORMAT (/5X,9HTHERE ARE,I5,21H VIOLATED CONSTRAINTS)
1090  FORMAT (/5X,9HTHERE ARE,I5,24H ACTIVE SIDE CONSTRAINTS) 
1100  FORMAT (5X,43HDECISION VARIABLES AT LOWER OR UPPER BOUNDS,30H (MIN
     1US INDICATES LOWER BOUND))
1110  FORMAT (/5X,22HONE-DIMENSIONAL SEARCH/5X,15HINITIAL SLOPE =,E12.4,
     12X,16HPROPOSED ALPHA =,E12.4) 
1120  FORMAT (///5X,35H* * CONMIN DETECTS VLB(I).GT.VUB(I)/5X,57HFIX IS 
     1SET X(I)=VLB(I)=VUB(I) = .5*(VLB(I)+VUB(I) FOR I =,I5)
1130  FORMAT (///5X,41H* * CONMIN DETECTS INITIAL X(I).LT.VLB(I)/5X,6HX(
     1I) =,E12.4,2X,8HVLB(I) =,E12.4/5X,35HX(I) IS SET EQUAL TO VLB(I) F
     2OR I =,I5)
1140  FORMAT (///5X,41H* * CONMIN DETECTS INITIAL X(I).GT.VUB(I)/5X,6HX(
     1I) =,E12.4,2X,8HVUB(I) =,E12.4/5X,35HX(I) IS SET EQUAL TO VUB(I) F
     2OR I =,I5)
1150  FORMAT (/5X,21HTERMINATION CRITERION) 
1160  FORMAT (10X,17HITER EQUALS ITMAX) 
1170  FORMAT (10X,'NFEASCT CONSECUTIVE ITERATIONS FAILED TO PRODUCE A 
     1FEASIBLE DESIGN')
1180  FORMAT (10X,43HABS(1-OBJ(I-1)/OBJ(I)) LESS THAN DELFUN FOR,I3,11H 
     1ITERATIONS) 
1190  FORMAT (10X,43HABS(OBJ(I)-OBJ(I-1))   LESS THAN DABFUN FOR,I3,11H 
     1ITERATIONS) 
1200  FORMAT (/5X,22HNUMBER OF ITERATIONS =,I5) 
1210  FORMAT (/5X,28HCONSTRAINT PARAMETER, BETA =,E14.5)
1220  FORMAT (1H1,////12X,27(2H* )/12X,1H*,51X,1H*/12X,1H*,20X,11HC O N 
     1M I N,20X,1H*/12X,1H*,51X,1H*/12X,1H*,15X,21H FORTRAN PROGRAM FOR 
     2,15X,1H*/12X,1H*,51X,1H*/12X,1H*,9X,33HCONSTRAINED FUNCTION MINIMI
     3ZATION,9X,1H*/12X,1H*,51X,1H*/12X,27(2H* )) 
1230  FORMAT (////5X,33HCONSTRAINED FUNCTION MINIMIZATION//5X,18HCONTROL
     1 PARAMETERS)
1240  FORMAT (/5X,60HIPRINT  NDV    ITMAX    NCON    NSIDE  ICNDIR   NSC
     1AL   NFDG/8I8//5X,12HLINOBJ  ITRM,5X,2HN1,6X,2HN2,6X,2HN3,6X,2HN4,
     26X,2HN5/8I8)
1250  FORMAT (/9X,4HFDCH,12X,5HFDCHM,11X,6HALPHAX,10X,6HABOBJ1/1X,4(2X,E
     114.5))
1260  FORMAT (/9X,2HCT,14X,5HCTMIN,11X,3HCTL,13X,6HCTLMIN/1X,4(2X,E14.5)
     1//9X,5HTHETA,11X,3HPHI,13X,6HDELFUN,10X,6HDABFUN/1X,4(2X,E14.5))
1270  FORMAT (/5X,40HLOWER BOUNDS ON DECISION VARIABLES (VLB))
1280  FORMAT (/5X,40HUPPER BOUNDS ON DECISION VARIABLES (VUB))
1290  FORMAT (////5X,35HUNCONSTRAINED FUNCTION MINIMIZATION//5X,18HCONTR
     1OL PARAMETERS)
1300  FORMAT (/5X,21HSCALING VECTOR (SCAL)) 
1310  FORMAT (////5X,22HBEGIN ITERATION NUMBER,I5)
1320  FORMAT (/5X,4HCT =,E14.5,5X,5HCTL =,E14.5,5X,5HPHI =,E14.5) 
1330  FORMAT (/5X,25HNEW SCALING VECTOR (SCAL)) 
1340  FORMAT (/5X,15HGRADIENT OF OBJ) 
1350  FORMAT (/5X,44HGRADIENTS OF ACTIVE AND VIOLATED CONSTRAINTS)
1360  FORMAT (1H )
1370  FORMAT (/5X,37HPUSH-OFF FACTORS, (THETA(I), I=1,NAC)) 
1380  FORMAT (/5X,27HSEARCH DIRECTION (S-VECTOR)) 
1390  FORMAT (/5X,18HCALCULATED ALPHA =,E14.5)
1400  FORMAT (////5X,6HITER =,I5,5X,5HOBJ =,E14.5,5X,16HNO CHANGE IN OBJ
     1) 
1410  FORMAT (/5X,5HOBJ =,E15.6,5X,16HNO CHANGE ON OBJ) 
1420  FORMAT (/5X,5HOBJ =,E15.6)
1430  FORMAT (////5X,6HITER =,I5,5X,5HOBJ =,E14.5)
1440  FORMAT (//5X,28HINITIAL FUNCTION INFORMATION//5X,5HOBJ =,E15.6) 
1450  FORMAT (/5X,29HDECISION VARIABLES (X-VECTOR)) 
1460  FORMAT (3X,7E13.4)
1470  FORMAT (/5X,28HCONSTRAINT VALUES (G-VECTOR))
1480  FORMAT (5X,15I5)
1490  FORMAT (/5X,59HTHE NUMBER OF ACTIVE AND VIOLATED CONSTRAINTS EXCEE
     1DS N3-1./5X,66HDIMENSIONED SIZE OF MATRICES A AND B AND VECTOR IC 
     2IS INSUFFICIENT/5X,61HOPTIMIZATION TERMINATED AND CONTROL RETURNED
     3 TO MAIN PROGRAM.)
1500  FORMAT (1H1,////4X,30HFINAL OPTIMIZATION INFORMATION) 
1510  FORMAT (/5X,32HOBJECTIVE FUNCTION WAS EVALUATED,8X,I5,2X,5HTIMES) 
1520  FORMAT (/5X,35HCONSTRAINT FUNCTIONS WERE EVALUATED,I10,2X,5HTIMES)
1530  FORMAT (/5X,36HGRADIENT OF OBJECTIVE WAS CALCULATED,I9,2X,5HTIMES)
1540  FORMAT (/5X,40HGRADIENTS OF CONSTRAINTS WERE CALCULATED,I5,2X,5HTI
     1MES)
      END 


c----- CNMN01
      SUBROUTINE CNMN01 (JGOTO,X,DF,G,ISC,IC,A,G1,VLB,VUB,SCAL,C,NCAL,DX
     1,DX1,FI,XI,III,N1,N2,N3,N4) 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      revision history
c     double precision version for workstations
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit           double precision(a-h,o-z)
      COMMON /CNMN1/ DELFUN,DABFUN,FDCH,FDCHM,CT,CTMIN,CTL,CTLMIN,ALPHAX
     1,ABOBJ1,THETA,OBJ,NDV,NCON,NSIDE,IPRINT,NFDG,NSCAL,LINOBJ,ITMAX,IT
     2RM,ICNDIR,IGOTO,NAC,INFO,INFOG,ITER 
      DIMENSION X(N1), DF(N1), G(N2), ISC(N2), IC(N3), A(N1,N3), G1(N2),
     1 VLB(N1), VUB(N1), SCAL(N1), NCAL(2), C(N4) 
C     ROUTINE TO CALCULATE GRADIENT INFORMATION BY FINITE DIFFERENCE. 
C     BY G. N. VANDERPLAATS                         JUNE, 1972. 
C     NASA-AMES RESEARCH CENTER,  MOFFETT FIELD, CALIF. 
      IF (JGOTO.EQ.1) GO TO 10
      IF (JGOTO.EQ.2) GO TO 70
      INFOG=0 
      INF=INFO
      NAC=0 
      IF (LINOBJ.NE.0.AND.ITER.GT.1) GO TO 10 
C     ------------------------------------------------------------------
C                    GRADIENT OF LINEAR OBJECTIVE 
C     ------------------------------------------------------------------
      IF (NFDG.EQ.2) JGOTO=1
      IF (NFDG.EQ.2) RETURN 
10    CONTINUE
      JGOTO=0 
      IF (NFDG.EQ.2.AND.NCON.EQ.0) RETURN 
      IF (NCON.EQ.0) GO TO 40 
C     ------------------------------------------------------------------
C       * * * DETERMINE WHICH CONSTRAINTS ARE ACTIVE OR VIOLATED * * *
C     ------------------------------------------------------------------
      DO 20 I=1,NCON
      IF (G(I).LT.CT) GO TO 20
      IF (ISC(I).GT.0.AND.G(I).LT.CTL) GO TO 20 
      NAC=NAC+1 
      IF (NAC.GE.N3) RETURN 
      IC(NAC)=I 
20    CONTINUE
      IF (NFDG.EQ.2.AND.NAC.EQ.0) RETURN
      IF ((LINOBJ.GT.0.AND.ITER.GT.1).AND.NAC.EQ.0) RETURN
C     ------------------------------------------------------------------
C                  STORE VALUES OF CONSTRAINTS IN G1
C     ------------------------------------------------------------------
      DO 30 I=1,NCON
30    G1(I)=G(I)
40    CONTINUE
      JGOTO=0 
      IF (NAC.EQ.0.AND.NFDG.EQ.2) RETURN
C     ------------------------------------------------------------------
C                            CALCULATE GRADIENTS
C     ------------------------------------------------------------------
      INFOG=1 
      INFO=1
      FI=OBJ
      III=0 
50    III=III+1 
      XI=X(III) 
      DX=FDCH*XI
      DX=ABS(DX)
      FDCH1=FDCHM 
      IF (NSCAL.NE.0) FDCH1=FDCHM/SCAL(III) 
      IF (DX.LT.FDCH1) DX=FDCH1 
      X1=XI+DX
      IF (NSIDE.EQ.0) GO TO 60
      IF (X1.GT.VUB(III)) DX=-DX
60    DX1=1./DX 
      X(III)=XI+DX
      NCAL(1)=NCAL(1)+1 
C     ------------------------------------------------------------------
C                         FUNCTION EVALUATION 
C     ------------------------------------------------------------------
      JGOTO=2 
      RETURN
70    CONTINUE
      X(III)=XI 
      IF (NFDG.EQ.0) DF(III)=DX1*(OBJ-FI) 
      IF (NAC.EQ.0) GO TO 90
C     ------------------------------------------------------------------
C             DETERMINE GRADIENT COMPONENTS OF ACTIVE CONSTRAINTS 
C     ------------------------------------------------------------------
      DO 80 J=1,NAC 
      I1=IC(J)
80    A(III,J)=DX1*(G(I1)-G1(I1)) 
90    CONTINUE
      IF (III.LT.NDV) GO TO 50
      INFOG=0 
      INFO=INF
      JGOTO=0 
      OBJ=FI
      IF (NCON.EQ.0) RETURN 
C     ------------------------------------------------------------------
C             STORE CURRENT CONSTRAINT VALUES BACK IN G-VECTOR
C     ------------------------------------------------------------------
      DO 100 I=1,NCON 
100   G(I)=G1(I)
      RETURN
      END 


c----- CNMN02 
 
      SUBROUTINE CNMN02 (NCALC,SLOPE,DFTDF1,DF,S,N1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      revision history
c     double precision version for workstations
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit           double precision(a-h,o-z)
      COMMON /CNMN1/ DELFUN,DABFUN,FDCH,FDCHM,CT,CTMIN,CTL,CTLMIN,ALPHAX
     1,ABOBJ1,THETA,OBJ,NDV,NCON,NSIDE,IPRINT,NFDG,NSCAL,LINOBJ,ITMAX,IT
     2RM,ICNDIR,IGOTO,NAC,INFO,INFOG,ITER 
      DIMENSION DF(N1), S(N1) 
C     ROUTINE TO DETERMINE CONJUGATE DIRECTION VECTOR OR DIRECTION
C     OF STEEPEST DESCENT FOR UNCONSTRAINED FUNCTION MINIMIZATION.
C     BY G. N. VANDERPLAATS                       APRIL, 1972.
C     NASA-AMES RESEARCH CENTER, MOFFETT FIELD, CALIF.
C     NCALC = CALCULATION CONTROL.
C         NCALC = 0,     S = STEEPEST DESCENT.
C         NCALC = 1,     S = CONJUGATE DIRECTION. 
C     CONJUGATE DIRECTION IS FOUND BY FLETCHER-REEVES ALGORITHM.
C     ------------------------------------------------------------------
C                   CALCULATE NORM OF GRADIENT VECTOR 
C     ------------------------------------------------------------------
      DFTDF=0.
      DO 10 I=1,NDV 
      DFI=DF(I) 
10    DFTDF=DFTDF+DFI*DFI 
C     ------------------------------------------------------------------
C     **********                FIND DIRECTION S              **********
C     ------------------------------------------------------------------
      IF (NCALC.NE.1) GO TO 30
      IF (DFTDF1.LT.1.0D-20) GO TO 30 
C     ------------------------------------------------------------------
C                 FIND FLETCHER-REEVES CONJUGATE DIRECTION
C     ------------------------------------------------------------------
      BETA=DFTDF/DFTDF1 
      SLOPE=0.
      DO 20 I=1,NDV 
      DFI=DF(I) 
      SI=BETA*S(I)-DFI
      SLOPE=SLOPE+SI*DFI
20    S(I)=SI 
      GO TO 50
30    CONTINUE
      NCALC=0 
C     ------------------------------------------------------------------
C                  CALCULATE DIRECTION OF STEEPEST DESCENT
C     ------------------------------------------------------------------
      DO 40 I=1,NDV 
40    S(I)=-DF(I) 
      SLOPE=-DFTDF
50    CONTINUE
C     ------------------------------------------------------------------
C                  NORMALIZE S TO MAX ABS VALUE OF UNITY
C     ------------------------------------------------------------------
      S1=0. 
      DO 60 I=1,NDV 
      S2=ABS(S(I))
      IF (S2.GT.S1) S1=S2 
60    CONTINUE
      IF (S1.LT.1.0D-20) S1=1.0D-20 
      S1=1./S1
      DFTDF1=DFTDF*S1 
      DO 70 I=1,NDV 
70    S(I)=S1*S(I)
      SLOPE=S1*SLOPE
      RETURN
      END 


c----- CNMN03 
      SUBROUTINE CNMN03 (X,S,SLOPE,ALP,FFF,A1,A2,A3,A4,F1,F2,F3,F4,APP,N
     11,NCAL,KOUNT,JGOTO) 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      revision history
c     double precision version for workstations
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit           double precision(a-h,o-z)
      COMMON /CNMN1/ DELFUN,DABFUN,FDCH,FDCHM,CT,CTMIN,CTL,CTLMIN,ALPHAX
     1,ABOBJ1,THETA,OBJ,NDV,NCON,NSIDE,IPRINT,NFDG,NSCAL,LINOBJ,ITMAX,IT
     2RM,ICNDIR,IGOTO,NAC,INFO,INFOG,ITER 
      DIMENSION X(N1), S(N1), NCAL(2) 
C     ROUTINE TO SOLVE ONE-DIMENSIONAL SEARCH IN UNCONSTRAINED
C     MINIMIZATION USING 2-POINT QUADRATIC INTERPOLATION, 3-POINT 
C     CUBIC INTERPOLATION AND 4-POINT CUBIC INTERPOLATION.
C     BY G. N. VANDERPLAATS                         APRIL, 1972.
C     NASA-AMES RESEARCH CENTER,  MOFFETT FIELD, CALIF. 
C     ALP = PROPOSED MOVE PARAMETER.
C     SLOPE = INITIAL FUNCTION SLOPE = S-TRANSPOSE TIMES DF.
C     SLOPE MUST BE NEGATIVE. 
C     OBJ = INITIAL FUNCTION VALUE. 
      ZRO=0.
      IF (JGOTO.EQ.0) GO TO 10
      GO TO (50,80,110,140,180,220,270),JGOTO 
C     ------------------------------------------------------------------
C                     INITIAL INFORMATION  (ALPHA=0)
C     ------------------------------------------------------------------
10    IF (SLOPE.LT.0.) GO TO 20 
      ALP=0.
      RETURN
20    CONTINUE
      IF (IPRINT.GT.4) WRITE (6,360)
      FFF=OBJ 
      AP1=0.
      A1=0. 
      F1=OBJ
      A2=ALP
      A3=0. 
      F3=0. 
      AP=A2 
      KOUNT=0 
C     ------------------------------------------------------------------
C            MOVE A DISTANCE AP*S AND UPDATE FUNCTION VALUE 
C     ------------------------------------------------------------------
30    CONTINUE
      KOUNT=KOUNT+1 
      DO 40 I=1,NDV 
40    X(I)=X(I)+AP*S(I) 
      IF (IPRINT.GT.4) WRITE (6,370) AP 
      IF (IPRINT.GT.4) WRITE (6,380) (X(I),I=1,NDV) 
      NCAL(1)=NCAL(1)+1 
      JGOTO=1 
      RETURN
50    CONTINUE
      F2=OBJ
      IF (IPRINT.GT.4) WRITE (6,390) F2 
      IF (F2.LT.F1) GO TO 120 
C     ------------------------------------------------------------------
C                     CHECK FOR ILL-CONDITIONING
C     ------------------------------------------------------------------
      IF (KOUNT.GT.5) GO TO 60
      FF=2.*ABS(F1) 
      IF (F2.LT.FF) GO TO 90
      FF=5.*ABS(F1) 
      IF (F2.LT.FF) GO TO 60
      A2=.5*A2
      AP=-A2
      ALP=A2
      GO TO 30
60    F3=F2 
      A3=A2 
      A2=.5*A2
C     ------------------------------------------------------------------
C                 UPDATE DESIGN VECTOR AND FUNCTION VALUE 
C     ------------------------------------------------------------------
      AP=A2-ALP 
      ALP=A2
      DO 70 I=1,NDV 
70    X(I)=X(I)+AP*S(I) 
      IF (IPRINT.GT.4) WRITE (6,370) A2 
      IF (IPRINT.GT.4) WRITE (6,380) (X(I),I=1,NDV) 
      NCAL(1)=NCAL(1)+1 
      JGOTO=2 
      RETURN
80    CONTINUE
      F2=OBJ
      IF (IPRINT.GT.4) WRITE (6,390) F2 
C     PROCEED TO CUBIC INTERPOLATION. 
      GO TO 160 
90    CONTINUE
C     ------------------------------------------------------------------
C     **********        2-POINT QUADRATIC INTERPOLATION       **********
C     ------------------------------------------------------------------
      JJ=1
      II=1
      CALL CNMN04 (II,APP,ZRO,A1,F1,SLOPE,A2,F2,ZRO,ZRO,ZRO,ZRO)
      IF (APP.LT.ZRO.OR.APP.GT.A2) GO TO 120
      F3=F2 
      A3=A2 
      A2=APP
      JJ=0
C     ------------------------------------------------------------------
C                  UPDATE DESIGN VECTOR AND FUNCTION VALUE
C     ------------------------------------------------------------------
      AP=A2-ALP 
      ALP=A2
      DO 100 I=1,NDV
100   X(I)=X(I)+AP*S(I) 
      IF (IPRINT.GT.4) WRITE (6,370) A2 
      IF (IPRINT.GT.4) WRITE (6,380) (X(I),I=1,NDV) 
      NCAL(1)=NCAL(1)+1 
      JGOTO=3 
      RETURN
110   CONTINUE
      F2=OBJ
      IF (IPRINT.GT.4) WRITE (6,390) F2 
      GO TO 150 
120   A3=2.*A2
C     ------------------------------------------------------------------
C                  UPDATE DESIGN VECTOR AND FUNCTION VALUE
C     ------------------------------------------------------------------
      AP=A3-ALP 
      ALP=A3
      DO 130 I=1,NDV
130   X(I)=X(I)+AP*S(I) 
      IF (IPRINT.GT.4) WRITE (6,370) A3 
      IF (IPRINT.GT.4) WRITE (6,380) (X(I),I=1,NDV) 
      NCAL(1)=NCAL(1)+1 
      JGOTO=4 
      RETURN
140   CONTINUE
      F3=OBJ
      IF (IPRINT.GT.4) WRITE (6,390) F3 
150   CONTINUE
      IF (F3.LT.F2) GO TO 190 
160   CONTINUE
C     ------------------------------------------------------------------
C     **********       3-POINT CUBIC INTERPOLATION      **********
C     ------------------------------------------------------------------
      II=3
      CALL CNMN04 (II,APP,ZRO,A1,F1,SLOPE,A2,F2,A3,F3,ZRO,ZRO)
      IF (APP.LT.ZRO.OR.APP.GT.A3) GO TO 190
C     ------------------------------------------------------------------
C     UPDATE DESIGN VECTOR AND FUNCTION VALUE.
C     ------------------------------------------------------------------
      AP1=APP 
      AP=APP-ALP
      ALP=APP 
      DO 170 I=1,NDV
170   X(I)=X(I)+AP*S(I) 
      IF (IPRINT.GT.4) WRITE (6,370) ALP
      IF (IPRINT.GT.4) WRITE (6,380) (X(I),I=1,NDV) 
      NCAL(1)=NCAL(1)+1 
      JGOTO=5 
      RETURN
180   CONTINUE
      IF (IPRINT.GT.4) WRITE (6,390) OBJ
C     ------------------------------------------------------------------
C                         CHECK CONVERGENCE 
C     ------------------------------------------------------------------
      AA=1.-APP/A2
      AB2=ABS(F2) 
      AB3=ABS(OBJ)
      AB=AB2
      IF (AB3.GT.AB) AB=AB3 
      IF (AB.LT.1.0D-15) AB=1.0D-15 
      AB=(AB2-AB3)/AB 
      IF (ABS(AB).LT.1.0D-15.AND.ABS(AA).LT..001) GO TO 330 
      A4=A3 
      F4=F3 
      A3=APP
      F3=OBJ
      IF (A3.GT.A2) GO TO 230 
      A3=A2 
      F3=F2 
      A2=APP
      F2=OBJ
      GO TO 230 
190   CONTINUE
C     ------------------------------------------------------------------
C     **********        4-POINT CUBIC INTERPOLATION       **********
C     ------------------------------------------------------------------
200   CONTINUE
      A4=2.*A3
C     UPDATE DESIGN VECTOR AND FUNCTION VALUE.
      AP=A4-ALP 
      ALP=A4
      DO 210 I=1,NDV
210   X(I)=X(I)+AP*S(I) 
      IF (IPRINT.GT.4) WRITE (6,370) ALP
      IF (IPRINT.GT.4) WRITE (6,380) (X(I),I=1,NDV) 
      NCAL(1)=NCAL(1)+1 
      JGOTO=6 
      RETURN
220   CONTINUE
      F4=OBJ
      IF (IPRINT.GT.4) WRITE (6,390) F4 
      IF (F4.GT.F3) GO TO 230 
      A1=A2 
      F1=F2 
      A2=A3 
      F2=F3 
      A3=A4 
      F3=F4 
      GO TO 200 
230   CONTINUE
      II=4
      CALL CNMN04 (II,APP,A1,A1,F1,SLOPE,A2,F2,A3,F3,A4,F4) 
      IF (APP.GT.A1) GO TO 250
      AP=A1-ALP 
      ALP=A1
      OBJ=F1
      DO 240 I=1,NDV
240   X(I)=X(I)+AP*S(I) 
      GO TO 280 
250   CONTINUE
C     ------------------------------------------------------------------
C                 UPDATE DESIGN VECTOR AND FUNCTION VALUE 
C     ------------------------------------------------------------------
      AP=APP-ALP
      ALP=APP 
      DO 260 I=1,NDV
260   X(I)=X(I)+AP*S(I) 
      IF (IPRINT.GT.4) WRITE (6,370) ALP
      IF (IPRINT.GT.4) WRITE (6,380) (X(I),I=1,NDV) 
      NCAL(1)=NCAL(1)+1 
      JGOTO=7 
      RETURN
270   CONTINUE
      IF (IPRINT.GT.4) WRITE (6,390) OBJ
280   CONTINUE
C     ------------------------------------------------------------------
C                    CHECK FOR ILL-CONDITIONING 
C     ------------------------------------------------------------------
      IF (OBJ.GT.F2.OR.OBJ.GT.F3) GO TO 290 
      IF (OBJ.LE.F1) GO TO 330
      AP=A1-ALP 
      ALP=A1
      OBJ=F1
      GO TO 310 
290   CONTINUE
      IF (F2.LT.F3) GO TO 300 
      OBJ=F3
      AP=A3-ALP 
      ALP=A3
      GO TO 310 
300   OBJ=F2
      AP=A2-ALP 
      ALP=A2
310   CONTINUE
C     ------------------------------------------------------------------
C                       UPDATE DESIGN VECTOR
C     ------------------------------------------------------------------
      DO 320 I=1,NDV
320   X(I)=X(I)+AP*S(I) 
330   CONTINUE
C     ------------------------------------------------------------------
C                     CHECK FOR MULTIPLE MINIMA 
C     ------------------------------------------------------------------
      IF (OBJ.LE.FFF) GO TO 350 
C     INITIAL FUNCTION IS MINIMUM.
      DO 340 I=1,NDV
340   X(I)=X(I)-ALP*S(I)
      ALP=0.
      OBJ=FFF 
350   CONTINUE
      JGOTO=0 
      RETURN
C     ------------------------------------------------------------------
C                                 FORMATS 
C     ------------------------------------------------------------------
C 
C 
360   FORMAT (/////5X,60H* * * UNCONSTRAINED ONE-DIMENSIONAL SEARCH INFO
     1RMATION * * *)
370   FORMAT (/5X,7HALPHA =,E14.5/5X,8HX-VECTOR)
380   FORMAT (5X,6E13.5)
390   FORMAT (/5X,5HOBJ =,E14.5)
      END 


c----- CNMN04
       SUBROUTINE CNMN04 (II,XBAR,EPS,X1,Y1,SLOPE,X2,Y2,X3,Y3,X4,Y4) 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      revision history
c     double precision version for workstations
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit           double precision(a-h,o-z)
C     ROUTINE TO FIND FIRST XBAR.GE.EPS CORRESPONDING TO A MINIMUM
C     OF A ONE-DIMENSIONAL REAL FUNCTION BY POLYNOMIEL INTERPOLATION. 
C     BY G. N. VANDERPLAATS                          APRIL, 1972. 
C     NASA-AMES RESEARCH CENTER,  MOFFETT FIELD, CALIF. 
C 
C     II = CALCULATION CONTROL. 
C          1:  2-POINT QUADRATIC INTERPOLATION, GIVEN X1, Y1, SLOPE,
C              X2 AND Y2. 
C          2:  3-POINT QUADRATIC INTERPOLATION, GIVEN X1, Y1, X2, Y2, 
C              X3 AND Y3. 
C          3:  3-POINT CUBIC INTERPOLATION, GIVEN X1, Y1, SLOPE, X2, Y2,
C              X3 AND Y3. 
C          4:  4-POINT CUBIC INTERPOLATION, GIVEN X1, Y1, X2, Y2, X3, 
C              Y3, X4 AND Y4. 
C     EPS MAY BE NEGATIVE.
C     IF REQUIRED MINIMUM ON Y DOES NOT EXITS, OR THE FUNCTION IS 
C     ILL-CONDITIONED, XBAR = EPS-1.0 WILL BE RETURNED AS AN ERROR
C     INDICATOR.
C     IF DESIRED INTERPOLATION IS ILL-CONDITIONED, A LOWER ORDER
C     INTERPOLATION, CONSISTANT WITH INPUT DATA, WILL BE ATTEMPTED, 
C     AND II WILL BE CHANGED ACCORDINGLY. 
      XBAR1=EPS-1.
      XBAR=XBAR1
      X21=X2-X1 
      IF (ABS(X21).LT.1.0D-20) RETURN 
      NSLOP=MOD(II,2) 
      GO TO (10,20,40,50),II
10    CONTINUE
C     ------------------------------------------------------------------
C                 II=1: 2-POINT QUADRATIC INTERPOLATION 
C     ------------------------------------------------------------------
      II=1
      DX=X1-X2
      IF (ABS(DX).LT.1.0D-20) RETURN
      AA=(SLOPE+(Y2-Y1)/DX)/DX
      IF (AA.LT.1.0D-20) RETURN 
      BB=SLOPE-2.*AA*X1 
      XBAR=-.5*BB/AA
      IF (XBAR.LT.EPS) XBAR=XBAR1 
      RETURN
20    CONTINUE
C     ------------------------------------------------------------------
C                 II=2: 3-POINT QUADRATIC INTERPOLATION 
C     ------------------------------------------------------------------
      II=2
      X21=X2-X1 
      X31=X3-X1 
      X32=X3-X2 
      QQ=X21*X31*X32
      IF (ABS(QQ).LT.1.0D-20) RETURN
      AA=(Y1*X32-Y2*X31+Y3*X21)/QQ
      IF (AA.LT.1.0D-20) GO TO 30 
      BB=(Y2-Y1)/X21-AA*(X1+X2) 
      XBAR=-.5*BB/AA
      IF (XBAR.LT.EPS) XBAR=XBAR1 
      RETURN
30    CONTINUE
      IF (NSLOP.EQ.0) RETURN
      GO TO 10
40    CONTINUE
C     ------------------------------------------------------------------
C                   II=3: 3-POINT CUBIC INTERPOLATION 
C     ------------------------------------------------------------------
      II=3
      X21=X2-X1 
      X31=X3-X1 
      X32=X3-X2 
      QQ=X21*X31*X32
      IF (ABS(QQ).LT.1.0D-20) RETURN
      X11=X1*X1 
      DNOM=X2*X2*X31-X11*X32-X3*X3*X21
      IF (ABS(DNOM).LT.1.0D-20) GO TO 20
      AA=((X31*X31*(Y2-Y1)-X21*X21*(Y3-Y1))/(X31*X21)-SLOPE*X32)/DNOM 
      IF (ABS(AA).LT.1.0D-20) GO TO 20
      BB=((Y2-Y1)/X21-SLOPE-AA*(X2*X2+X1*X2-2.*X11))/X21
      CC=SLOPE-3.*AA*X11-2.*BB*X1 
      BAC=BB*BB-3.*AA*CC
      IF (BAC.LT.0.) GO TO 20 
      BAC=SQRT(BAC) 
      XBAR=(BAC-BB)/(3.*AA) 
      IF (XBAR.LT.EPS) XBAR=EPS 
      RETURN
50    CONTINUE
C     ------------------------------------------------------------------
C                    II=4: 4-POINT CUBIC INTERPOLATION
C     ------------------------------------------------------------------
      X21=X2-X1 
      X31=X3-X1 
      X41=X4-X1 
      X32=X3-X2 
      X42=X4-X2 
      X11=X1*X1 
      X22=X2*X2 
      X33=X3*X3 
      X44=X4*X4 
      X111=X1*X11 
      X222=X2*X22 
      Q2=X31*X21*X32
      IF (ABS(Q2).LT.1.0D-30) RETURN
      Q1=X111*X32-X222*X31+X3*X33*X21 
      Q4=X111*X42-X222*X41+X4*X44*X21 
      Q5=X41*X21*X42
      DNOM=Q2*Q4-Q1*Q5
      IF (ABS(DNOM).LT.1.0D-30) GO TO 60
      Q3=Y3*X21-Y2*X31+Y1*X32 
      Q6=Y4*X21-Y2*X41+Y1*X42 
      AA=(Q2*Q6-Q3*Q5)/DNOM 
      BB=(Q3-Q1*AA)/Q2
      CC=(Y2-Y1-AA*(X222-X111))/X21-BB*(X1+X2)
      BAC=BB*BB-3.*AA*CC
      IF (ABS(AA).LT.1.0D-20.OR.BAC.LT.0.) GO TO 60 
      BAC=SQRT(BAC) 
      XBAR=(BAC-BB)/(3.*AA) 
      IF (XBAR.LT.EPS) XBAR=XBAR1 
      RETURN
60    CONTINUE
      IF (NSLOP.EQ.1) GO TO 40
      GO TO 20
      END 


c----- CNMN05
      SUBROUTINE CNMN05 (G,DF,A,S,B,C,SLOPE,PHI,ISC,IC,MS1,NVC,N1,N2,N3,
     1N4,N5)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      revision history
c     double precision version for workstations
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit           double precision(a-h,o-z)
      COMMON /CNMN1/ DELFUN,DABFUN,FDCH,FDCHM,CT,CTMIN,CTL,CTLMIN,ALPHAX
     1,ABOBJ1,THETA,OBJ,NDV,NCON,NSIDE,IPRINT,NFDG,NSCAL,LINOBJ,ITMAX,IT
     2RM,ICNDIR,IGOTO,NAC,INFO,INFOG,ITER 
      DIMENSION DF(N1), G(N2), ISC(N2), IC(N3), A(N1,N3), S(N1), C(N4), 
     1MS1(N5), B(N3,N3) 
C     ROUTINE TO SOLVE DIRECTION FINDING PROBLEM IN MODIFIED METHOD OF
C     FEASIBLE DIRECTIONS.
C     BY G. N. VANDERPLAATS                            MAY, 1972. 
C     NASA-AMES RESEARCH CENTER, MOFFETT FIELD, CALIF.
C     NORM OF S VECTOR USED HERE IS S-TRANSPOSE TIMES S.LE.1. 
C     IF NVC = 0 FIND DIRECTION BY ZOUTENDIJK'S METHOD.  OTHERWISE
C     FIND MODIFIED DIRECTION.
C     ------------------------------------------------------------------
C     ***  NORMALIZE GRADIENTS, CALCULATE THETA'S AND DETERMINE NVC  ***
C     ------------------------------------------------------------------
      NDV1=NDV+1
      NDV2=NDV+2
      NAC1=NAC+1
      NVC=0 
      THMAX=0.
      CTA=ABS(CT) 
      CT1=1./CTA
      CTAM=ABS(CTMIN) 
      CTB=ABS(CTL)
      CT2=1./CTB
      CTBM=ABS(CTLMIN)
      A1=1. 
      DO 40 I=1,NAC 
C     CALCULATE THETA 
      NCI=IC(I) 
      NCJ=1 
      IF (NCI.LE.NCON) NCJ=ISC(NCI) 
      C1=G(NCI) 
      CTD=CT1 
      CTC=CTAM
      IF (NCJ.LE.0) GO TO 10
      CTC=CTBM
      CTD=CT2 
10    IF (C1.GT.CTC) NVC=NVC+1
      THT=0.
      GG=1.+CTD*C1
      IF (NCJ.EQ.0.OR.C1.GT.CTC) THT=THETA*GG*GG
      IF (THT.GT.50.) THT=50. 
      IF (THT.GT.THMAX) THMAX=THT 
      A(NDV1,I)=THT 
C     ------------------------------------------------------------------
C                    NORMALIZE GRADIENTS OF CONSTRAINTS 
C     ------------------------------------------------------------------
      A(NDV2,I)=1.
      IF (NCI.GT.NCON) GO TO 40 
      A1=0. 
      DO 20 J=1,NDV 
      A1=A1+A(J,I)**2 
20    CONTINUE
      IF (A1.LT.1.0D-20) A1=1.0D-20 
      A1=SQRT(A1) 
      A(NDV2,I)=A1
      A1=1./A1
      DO 30 J=1,NDV 
30    A(J,I)=A1*A(J,I)
40    CONTINUE
C     ------------------------------------------------------------------
C     CHECK FOR ZERO GRADIENT.  PROGRAM CHANGE-FEB, 1981, GV. 
C     ------------------------------------------------------------------
      I=0 
41    I=I+1 
42    CONTINUE
      IF(A(NDV2,I).GT.1.0D-6) GO TO 45
C     ZERO GRADIENT IS FOUND.  WRITE ERROR MESSAGE. 
      IF(IPRINT.GE.2) WRITE(6,43)IC(I)
43    FORMAT(5X,13H** CONSTRAINT,I5,18H HAS ZERO GRADIENT/
     *5X,23HDELETED FROM ACTIVE SET)
C     REDUCE NAC BY ONE.
      NAC=NAC-1 
C     SHIFT COLUMNS OF A AND ROWS OF IC IF I.LE.NAC.
      IF(I.GT.NAC) GO TO 46 
C     SHIFT.
      DO 44 J=I,NAC 
      J1=J+1
      IC(J)=IC(J1)
      DO 44 K=1,NDV2
44    A(K,J)=A(K,J1)
      IF(I.LE.NAC) GO TO 42 
45    CONTINUE
      IF(I.LT.NAC) GO TO 41 
46    CONTINUE
      IF(NAC.LE.0) RETURN 
      NAC1=NAC+1
C     DETERMINE IF CONSTRAINTS ARE VIOLATED.
      NVC=0 
      DO 47 I=1,NAC 
      NCI=IC(I) 
      NCJ=1 
      IF(NCI.LE.NCON) NCJ=ISC(NCI)
      CTC=CTAM
      IF(NCJ.GT.0) CTC=CTBM 
      IF(G(NCI).GT.CTC) NVC=NVC+1 
47    CONTINUE
C     ------------------------------------------------------------------
C     NORMALIZE GRADIENT OF OBJECTIVE FUNCTION AND STORE IN NAC+1 
C     COLUMN OF A 
C     ------------------------------------------------------------------
      A1=0. 
      DO 50 I=1,NDV 
      A1=A1+DF(I)**2
50    CONTINUE
      IF (A1.LT.1.0D-20) A1=1.0D-20 
      A1=SQRT(A1) 
      A1=1./A1
      DO 60 I=1,NDV 
60    A(I,NAC1)=A1*DF(I)
C     BUILD C VECTOR. 
      IF (NVC.GT.0) GO TO 80
C     ------------------------------------------------------------------
C                 BUILD C FOR CLASSICAL METHOD
C     ------------------------------------------------------------------
      NDB=NAC1
      A(NDV1,NDB)=1.
      DO 70 I=1,NDB 
70    C(I)=-A(NDV1,I) 
      GO TO 110 
80    CONTINUE
C     ------------------------------------------------------------------
C                   BUILD C FOR MODIFIED METHOD 
C     ------------------------------------------------------------------
      NDB=NAC 
      A(NDV1,NAC1)=-PHI 
C     ------------------------------------------------------------------
C           SCALE THETA'S SO THAT MAXIMUM THETA IS UNITY
C     ------------------------------------------------------------------
      IF (THMAX.GT.0.00001) THMAX=1./THMAX
      DO 90 I=1,NDB 
      A(NDV1,I)=A(NDV1,I)*THMAX 
90    CONTINUE
      DO 100 I=1,NDB
      C(I)=0. 
      DO 100 J=1,NDV1 
100   C(I)=C(I)+A(J,I)*A(J,NAC1)
110   CONTINUE
C     ------------------------------------------------------------------
C                      BUILD B MATRIX 
C     ------------------------------------------------------------------
      DO 120 I=1,NDB
      DO 120 J=1,NDB
      B(I,J)=0. 
      DO 120 K=1,NDV1 
120   B(I,J)=B(I,J)-A(K,I)*A(K,J) 
C     ------------------------------------------------------------------
C                    SOLVE SPECIAL L. P. PROBLEM
C     ------------------------------------------------------------------
      CALL CNMN08 (NDB,NER,C,MS1,B,N3,N4,N5)
      IF (IPRINT.GT.1.AND.NER.GT.0) WRITE (6,180) 
C     CALCULATE RESULTING DIRECTION VECTOR, S.
      SLOPE=0.
C     ------------------------------------------------------------------
C                  USABLE-FEASIBLE DIRECTION
C     ------------------------------------------------------------------
      DO 140 I=1,NDV
      S1=0. 
      IF (NVC.GT.0) S1=-A(I,NAC1) 
      DO 130 J=1,NDB
130   S1=S1-A(I,J)*C(J) 
      SLOPE=SLOPE+S1*DF(I)
140   S(I)=S1 
      S(NDV1)=1.
      IF (NVC.GT.0) S(NDV1)=-A(NDV1,NAC1) 
      DO 150 J=1,NDB
150   S(NDV1)=S(NDV1)-A(NDV1,J)*C(J)
C     ------------------------------------------------------------------
C     CHECK TO INSURE THE S-VECTOR IS FEASIBLE. 
C     PROGRAM MOD-FEB, 1981, GV.
C     ------------------------------------------------------------------
      DO 174 J=1,NAC
C     S DOT DEL(G). 
      SG=0. 
      DO 172 I=1,NDV
172   SG=SG+S(I)*A(I,J) 
C     IF(SG.GT.0.) GO TO 176
C 
C  THIS CHANGE MADE ON 4/8/81 FOR G. VANDERPLAATS 
C 
      IF(SG.GT.1.0D-04) GO TO 176 
C     FEASIBLE FOR THIS CONSTRAINT.  CONTINUE.
174   CONTINUE
      GO TO 179 
176   CONTINUE
C     S-VECTOR IS NOT FEASIBLE DUE TO SOME NUMERICAL PROBLEM. 
      IF(IPRINT.GE.2) WRITE(6,178)
178   FORMAT(5X,38H** CALCULATED S-VECTOR IS NOT FEASIBLE/5X, 
     * 19HBETA IS SET TO ZERO)
      S(NDV1)=0.
      NVC=0 
      RETURN
179   CONTINUE
C     ------------------------------------------------------------------
C                  NORMALIZE S TO MAX ABS OF UNITY
C     ------------------------------------------------------------------
      S1=0. 
      DO 160 I=1,NDV
      A1=ABS(S(I))
      IF (A1.GT.S1) S1=A1 
160   CONTINUE
C     IF (S1.LT.1.0E-10) RETURN 
C 
C  E-10 CHANGED TO E-04 ON 1/12/81
C 
      IF (S1.LT.1.0D-04) RETURN 
      S1=1./S1
      DO 170 I=1,NDV
170   S(I)=S1*S(I)
      SLOPE=S1*SLOPE
      S(NDV1)=S1*S(NDV1)
      RETURN
C     ------------------------------------------------------------------
C                           FORMATS 
C     ------------------------------------------------------------------
C 
C 
180   FORMAT (//5X,46H* * DIRECTION FINDING PROCESS DID NOT CONVERGE/5X,
     129H* * S-VECTOR MAY NOT BE VALID) 
      END 


c----- CNMN06
      SUBROUTINE CNMN06 (X,VLB,VUB,G,SCAL,DF,S,G1,G2,CTAM,CTBM,SLOPE,ALP
     1,A2,A3,A4,F1,F2,F3,CV1,CV2,CV3,CV4,ALPCA,ALPFES,ALPLN,ALPMIN,ALPNC
     2,ALPSAV,ALPSID,ALPTOT,ISC,N1,N2,NCAL,NVC,ICOUNT,IGOOD1,IGOOD2,IGOO
     3D3,IGOOD4,IBEST,III,NLNC,JGOTO) 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      revision history
c     double precision version for workstations
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit           double precision(a-h,o-z)
      COMMON /CNMN1/ DELFUN,DABFUN,FDCH,FDCHM,CT,CTMIN,CTL,CTLMIN,ALPHAX
     1,ABOBJ1,THETA,OBJ,NDV,NCON,NSIDE,IPRINT,NFDG,NSCAL,LINOBJ,ITMAX,IT
     2RM,ICNDIR,IGOTO,NAC,INFO,INFOG,ITER 
      DIMENSION X(N1), VLB(N1), VUB(N1), G(N2), SCAL(N1), DF(N1), S(N1),
     1 G1(N2), G2(N2), ISC(N2), NCAL(2) 
C     ROUTINE TO SOLVE ONE-DIMENSIONAL SEARCH PROBLEM FOR CONSTRAINED 
C     FUNCTION MINIMIZATION.
C     BY G. N. VANDERPLAATS                           AUG., 1974. 
C     NASA-AMES RESEARCH CENTER, MOFFETT FIELD, CALIF.
C     OBJ = INITIAL AND FINAL FUNCTION VALUE. 
C     ALP = MOVE PARAMETER. 
C     SLOPE = INITIAL SLOPE.
C 
C     ALPSID = MOVE TO SIDE CONSTRAINT. 
C     ALPFES = MOVE TO FEASIBLE REGION. 
C     ALPNC = MOVE TO NEW NON-LINEAR CONSTRAINT.
C     ALPLN = MOVE TO LINEAR CONSTRAINT.
C     ALPCA = MOVE TO RE-ENCOUNTER CURRENTLY ACTIVE CONSTRAINT. 
C     ALPMIN = MOVE TO MINIMIZE FUNCTION. 
C     ALPTOT = TOTAL MOVE PARAMETER.
      ZRO=0.
      IF (JGOTO.EQ.0) GO TO 10
      GO TO (140,310,520),JGOTO 
10    IF (IPRINT.GE.5) WRITE (6,730)
      ALPSAV=ALP
      ICOUNT=0
      ALPTOT=0. 
C     TOLERANCES. 
      CTAM=ABS(CTMIN) 
      CTBM=ABS(CTLMIN)
C     PROPOSED MOVE.
20    CONTINUE
C     ------------------------------------------------------------------
C     *****  BEGIN SEARCH OR IMPOSE SIDE CONSTRAINT MODIFICATION  ***** 
C     ------------------------------------------------------------------
      A2=ALPSAV 
      ICOUNT=ICOUNT+1 
      ALPSID=1.0D+20
C     INITIAL ALPHA AND OBJ.
      ALP=0.
      F1=OBJ
      KSID=0
      IF (NSIDE.EQ.0) GO TO 70
C     ------------------------------------------------------------------
C     FIND MOVE TO SIDE CONSTRAINT AND INSURE AGAINST VIOLATION OF
C     SIDE CONSTRAINTS
C     ------------------------------------------------------------------
      DO 60 I=1,NDV 
      SI=S(I) 
      IF (ABS(SI).GT.1.0D-20) GO TO 30
C     ITH COMPONENT OF S IS SMALL.  SET TO ZERO.
      S(I)=0. 
      SLOPE=SLOPE-SI*DF(I)
      GO TO 60
30    CONTINUE
      XI=X(I) 
      SI=1./SI
      IF (SI.GT.0.) GO TO 40
C     LOWER BOUND.
      XI2=VLB(I)
      XI1=ABS(XI2)
      IF (XI1.LT.1.) XI1=1. 
C     CONSTRAINT VALUE. 
      GI=(XI2-XI)/XI1 
      IF (GI.GT.-1.0D-6) GO TO 50 
C     PROPOSED MOVE TO LOWER BOUND. 
      ALPA=(XI2-XI)*SI
      IF (ALPA.LT.ALPSID) ALPSID=ALPA 
      GO TO 60
40    CONTINUE
C     UPPER BOUND.
      XI2=VUB(I)
      XI1=ABS(XI2)
      IF (XI1.LT.1.) XI1=1. 
C     CONSTRAINT VALUE. 
      GI=(XI-XI2)/XI1 
      IF (GI.GT.-1.0D-6) GO TO 50 
C     PROPOSED MOVE TO UPPER BOUND. 
      ALPA=(XI2-XI)*SI
      IF (ALPA.LT.ALPSID) ALPSID=ALPA 
      GO TO 60
50    CONTINUE
C     MOVE WILL VIOLATE SIDE CONSTRAINT.  SET S(I)=0. 
      SLOPE=SLOPE-S(I)*DF(I)
      S(I)=0. 
      KSID=KSID+1 
60    CONTINUE
C     ALPSID IS UPPER BOUND ON ALPHA. 
      IF (A2.GT.ALPSID) A2=ALPSID 
70    CONTINUE
C     ------------------------------------------------------------------
C               CHECK ILL-CONDITIONING
C     ------------------------------------------------------------------
      IF (KSID.EQ.NDV.OR.ICOUNT.GT.10) GO TO 710
      IF (NVC.EQ.0.AND.SLOPE.GT.0.) GO TO 710 
      ALPFES=-1.
      ALPMIN=-1.
      ALPLN=1.1*ALPSID
      ALPNC=ALPSID
      ALPCA=ALPSID
      IF (NCON.EQ.0) GO TO 90 
C     STORE CONSTRAINT VALUES IN G1.
      DO 80 I=1,NCON
      G1(I)=G(I)
80    CONTINUE
90    CONTINUE
C     ------------------------------------------------------------------
C                  MOVE A DISTANCE A2*S 
C     ------------------------------------------------------------------
      ALPTOT=ALPTOT+A2
      DO 100 I=1,NDV
      X(I)=X(I)+A2*S(I) 
100   CONTINUE
      IF (IPRINT.LT.5) GO TO 130
      WRITE (6,740) A2
      IF (NSCAL.EQ.0) GO TO 120 
      DO 110 I=1,NDV
110   G(I)=SCAL(I)*X(I) 
      WRITE (6,750) (G(I),I=1,NDV)
      GO TO 130 
120   WRITE (6,750) (X(I),I=1,NDV)
C     ------------------------------------------------------------------
C                   UPDATE FUNCTION AND CONSTRAINT VALUES 
C     ------------------------------------------------------------------
130   NCAL(1)=NCAL(1)+1 
      JGOTO=1 
      RETURN
140   CONTINUE
      F2=OBJ
      IF (IPRINT.GE.5) WRITE (6,760) F2 
      IF (IPRINT.LT.5.OR.NCON.EQ.0) GO TO 150 
      WRITE (6,770) 
      WRITE (6,750) (G(I),I=1,NCON) 
150   CONTINUE
C     ------------------------------------------------------------------
C               IDENTIFY ACCAPTABILITY OF DESIGNS F1 AND F2 
C     ------------------------------------------------------------------
C     IGOOD = 0 IS ACCAPTABLE.
C     CV = MAXIMUM CONSTRAINT VIOLATION.
      IGOOD1=0
      IGOOD2=0
      CV1=0.
      CV2=0.
      NVC1=0
      IF (NCON.EQ.0) GO TO 170
      DO 160 I=1,NCON 
      CC=CTAM 
      IF (ISC(I).GT.0) CC=CTBM
      C1=G1(I)-CC 
      C2=G(I)-CC
      IF (C2.GT.0.) NVC1=NVC1+1 
      IF (C1.GT.CV1) CV1=C1 
      IF (C2.GT.CV2) CV2=C2 
160   CONTINUE
      IF (CV1.GT.0.) IGOOD1=1 
      IF (CV2.GT.0.) IGOOD2=1 
170   CONTINUE
      ALP=A2
      OBJ=F2
C     ------------------------------------------------------------------
C     IF F2 VIOLATES FEWER CONSTRAINTS THAN F1 BUT STILL HAS CONSTRAINT 
C     VIOLATIONS RETURN 
C     ------------------------------------------------------------------
      IF (NVC1.LT.NVC.AND.NVC1.GT.0) GO TO 710
C     ------------------------------------------------------------------
C             IDENTIFY BEST OF DESIGNS F1 ANF F2
C     ------------------------------------------------------------------
C     IBEST CORRESPONDS TO MINIMUM VALUE DESIGN.
C     IF CONSTRAINTS ARE VIOLATED, IBEST CORRESPONDS TO MINIMUM 
C     CONSTRAINT VIOLATION. 
      IF (IGOOD1.EQ.0.AND.IGOOD2.EQ.0) GO TO 180
C     VIOLATED CONSTRAINTS.  PICK MINIMUM VIOLATION.
      IBEST=1 
      IF (CV1.GE.CV2) IBEST=2 
      GO TO 190 
180   CONTINUE
C     NO CONSTRAINT VIOLATION.  PICK MINIMUM F. 
      IBEST=1 
      IF (F2.LE.F1) IBEST=2 
190   CONTINUE
      II=1
C     ------------------------------------------------------------------
C     IF CV2 IS GREATER THAN CV1, SET MOVE LIMITS TO A2.
C     PROGRAM MOD-FEB, 1981, GV.
C     ------------------------------------------------------------------
      IF(CV2.LE.CV1) GO TO 195
      ALPLN=A2
      ALPNC=A2
      ALPCA=A2
195   CONTINUE
      IF (NCON.EQ.0) GO TO 230
C     ------------------------------------------------------------------
C     *****                 2 - POINT INTERPOLATION                *****
C     ------------------------------------------------------------------
      III=0 
200   III=III+1 
      C1=G1(III)
      C2=G(III) 
      IF (ISC(III).EQ.0) GO TO 210
C     ------------------------------------------------------------------
C                        LINEAR CONSTRAINT
C     ------------------------------------------------------------------
      IF (C1.GE.1.0D-5.AND.C1.LE.CTBM) GO TO 220
      CALL CNMN07 (II,ALP,ZRO,ZRO,C1,A2,C2,ZRO,ZRO) 
      IF (ALP.LE.0.) GO TO 220
      IF (C1.GT.CTBM.AND.ALP.GT.ALPFES) ALPFES=ALP
      IF (C1.LT.CTL.AND.ALP.LT.ALPLN) ALPLN=ALP 
      GO TO 220 
210   CONTINUE
C     ------------------------------------------------------------------
C                     NON-LINEAR CONSTRAINT 
C     ------------------------------------------------------------------
      IF (C1.GE.1.0D-5.AND.C1.LE.CTAM) GO TO 220
      CALL CNMN07 (II,ALP,ZRO,ZRO,C1,A2,C2,ZRO,ZRO) 
      IF (ALP.LE.0.) GO TO 220
      IF (C1.GT.CTAM.AND.ALP.GT.ALPFES) ALPFES=ALP
      IF (C1.LT.CT.AND.ALP.LT.ALPNC) ALPNC=ALP
220   CONTINUE
      IF (III.LT.NCON) GO TO 200
230   CONTINUE
      IF (LINOBJ.GT.0.OR.SLOPE.GE.0.) GO TO 240 
C     CALCULATE ALPHA TO MINIMIZE FUNCTION. 
      CALL CNMN04 (II,ALPMIN,ZRO,ZRO,F1,SLOPE,A2,F2,ZRO,ZRO,ZRO,ZRO)
240   CONTINUE
C     ------------------------------------------------------------------
C                         PROPOSED MOVE 
C     ------------------------------------------------------------------
C     MOVE AT LEAST FAR ENOUGH TO OVERCOME CONSTRAINT VIOLATIONS. 
      A3=ALPFES 
C     MOVE TO MINIMIZE FUNCTION.
      IF (ALPMIN.GT.A3) A3=ALPMIN 
C     IF A3.LE.0, SET A3 = ALPSID.
      IF (A3.LE.0.) A3=ALPSID 
C     LIMIT MOVE TO NEW CONSTRAINT ENCOUNTER. 
      IF (A3.GT.ALPNC) A3=ALPNC 
      IF (A3.GT.ALPLN) A3=ALPLN 
C     MAKE A3 NON-ZERO. 
      IF (A3.LE.1.0D-20) A3=1.0D-20 
C     IF A3=A2=ALPSID AND F2 IS BEST, GO INVOKE SIDE CONSTRAINT 
C     MODIFICATION. 
      ALPB=1.-A2/A3 
      ALPA=1.-ALPSID/A3 
      JBEST=0 
      IF (ABS(ALPB).LT.1.0D-10.AND.ABS(ALPA).LT.1.0D-10) JBEST=1
      IF (JBEST.EQ.1.AND.IBEST.EQ.2) GO TO 20 
C     SIDE CONSTRAINT CHECK NOT SATISFIED.
      IF (NCON.EQ.0) GO TO 260
C     STORE CONSTRAINT VALUES IN G2.
      DO 250 I=1,NCON 
      G2(I)=G(I)
250   CONTINUE
260   CONTINUE
C     IF A3=A2, SET A3=.9*A2. 
      IF (ABS(ALPB).LT.1.0D-10) A3=.9*A2
C     MOVE AT LEAST .01*A2. 
      IF (A3.LT.(.01*A2)) A3=.01*A2 
C     LIMIT MOVE TO 5.*A2.
      IF (A3.GT.(5.*A2)) A3=5.*A2 
C     LIMIT MOVE TO ALPSID. 
      IF (A3.GT.ALPSID) A3=ALPSID 
C     MOVE A DISTANCE A3*S. 
      ALP=A3-A2 
      ALPTOT=ALPTOT+ALP 
      DO 270 I=1,NDV
      X(I)=X(I)+ALP*S(I)
270   CONTINUE
      IF (IPRINT.LT.5) GO TO 300
      WRITE (6,780) 
      WRITE (6,740) A3
      IF (NSCAL.EQ.0) GO TO 290 
      DO 280 I=1,NDV
280   G(I)=SCAL(I)*X(I) 
      WRITE (6,750) (G(I),I=1,NDV)
      GO TO 300 
290   WRITE (6,750) (X(I),I=1,NDV)
300   CONTINUE
C     ------------------------------------------------------------------
C              UPDATE FUNCTION AND CONSTRAINT VALUES
C     ------------------------------------------------------------------
      NCAL(1)=NCAL(1)+1 
      JGOTO=2 
      RETURN
310   CONTINUE
      F3=OBJ
      IF (IPRINT.GE.5) WRITE (6,760) F3 
      IF (IPRINT.LT.5.OR.NCON.EQ.0) GO TO 320 
      WRITE (6,770) 
      WRITE (6,750) (G(I),I=1,NCON) 
320   CONTINUE
C     ------------------------------------------------------------------
C       CALCULATE MAXIMUM CONSTRAINT VIOLATION AND PICK BEST DESIGN 
C     ------------------------------------------------------------------
      CV3=0.
      IGOOD3=0
      NVC1=0
      IF (NCON.EQ.0) GO TO 340
      DO 330 I=1,NCON 
      CC=CTAM 
      IF (ISC(I).GT.0) CC=CTBM
      C1=G(I)-CC
      IF (C1.GT.CV3) CV3=C1 
      IF (C1.GT.0.) NVC1=NVC1+1 
330   CONTINUE
      IF (CV3.GT.0.) IGOOD3=1 
340   CONTINUE
C     DETERMINE BEST DESIGN.
      IF (IBEST.EQ.2) GO TO 360 
C     CHOOSE BETWEEN F1 AND F3. 
      IF (IGOOD1.EQ.0.AND.IGOOD3.EQ.0) GO TO 350
      IF (CV1.GE.CV3) IBEST=3 
      GO TO 380 
350   IF (F3.LE.F1) IBEST=3 
      GO TO 380 
360   CONTINUE
C     CHOOSE BETWEEN F2 AND F3. 
      IF (IGOOD2.EQ.0.AND.IGOOD3.EQ.0) GO TO 370
      IF (CV2.GE.CV3) IBEST=3 
      GO TO 380 
370   IF (F3.LE.F2) IBEST=3 
380   CONTINUE
      ALP=A3
      OBJ=F3
C     IF F3 VIOLATES FEWER CONSTRAINTS THAN F1 RETURN.
      IF (NVC1.LT.NVC) GO TO 710
C     IF OBJECTIVE AND ALL CONSTRAINTS ARE LINEAR, RETURN.
      IF (LINOBJ.NE.0.AND.NLNC.EQ.NCON) GO TO 710 
C     IF A3 = ALPLN AND F3 IS BOTH GOOD AND BEST RETURN.
      ALPB=1.-ALPLN/A3
      IF ((ABS(ALPB).LT.1.0D-20.AND.IBEST.EQ.3).AND.(IGOOD3.EQ.0)) GO TO
     1 710
C     IF A3 = ALPSID AND F3 IS BEST, GO INVOKE SIDE CONSTRAINT
C     MODIFICATION. 
      ALPA=1.-ALPSID/A3 
      IF (ABS(ALPA).LT.1.0D-20.AND.IBEST.EQ.3) GO TO 20 
C     ------------------------------------------------------------------
C     **********            3 - POINT INTERPOLATION            *********
C     ------------------------------------------------------------------
      ALPNC=ALPSID
      ALPCA=ALPSID
      ALPFES=-1.
      ALPMIN=-1.
C     ------------------------------------------------------------------
C     IF A3 IS GREATER THAN A2 AND CV3 IS GREATER THAN CV2, SET 
C     MOVE LIMITS TO A3.  PROGRAM MOD-FEB, 1981, GV.
C     ------------------------------------------------------------------
      IF(A3.LE.A2.OR.CV3.LE.CV2) GO TO 285
      ALPLN=A3
      ALPNC=A3
      ALPCA=A3
285   CONTINUE
      IF (NCON.EQ.0) GO TO 440
      III=0 
390   III=III+1 
      C1=G1(III)
      C2=G2(III)
      C3=G(III) 
      IF (ISC(III).EQ.0) GO TO 400
C     ------------------------------------------------------------------
C     LINEAR CONSTRAINT.  FIND ALPFES ONLY.  ALPLN SAME AS BEFORE.
C     ------------------------------------------------------------------
      IF (C1.LE.CTBM) GO TO 430 
      II=1
      CALL CNMN07 (II,ALP,ZRO,ZRO,C1,A3,C3,ZRO,ZRO) 
      IF (ALP.GT.ALPFES) ALPFES=ALP 
      GO TO 430 
400   CONTINUE
C     ------------------------------------------------------------------
C                     NON-LINEAR CONSTRAINT 
C     ------------------------------------------------------------------
      II=2
      CALL CNMN07 (II,ALP,ZRO,ZRO,C1,A2,C2,A3,C3) 
      IF (ALP.LE.ZRO) GO TO 430 
      IF (C1.GE.CT.AND.C1.LE.0.) GO TO 410
      IF (C1.GT.CTAM.OR.C1.LT.0.) GO TO 420 
C     ALP IS MINIMUM MOVE.  UPDATE FOR NEXT CONSTRAINT ENCOUNTER. 
410   ALPA=ALP
      CALL CNMN07 (II,ALP,ALPA,ZRO,C1,A2,C2,A3,C3)
      IF (ALP.LT.ALPCA.AND.ALP.GE.ALPA) ALPCA=ALP 
      GO TO 430 
420   CONTINUE
      IF (ALP.GT.ALPFES.AND.C1.GT.CTAM) ALPFES=ALP
      IF (ALP.LT.ALPNC.AND.C1.LT.0.) ALPNC=ALP
430   CONTINUE
      IF (III.LT.NCON) GO TO 390
440   CONTINUE
      IF (LINOBJ.GT.0.OR.SLOPE.GT.0.) GO TO 450 
C     ------------------------------------------------------------------
C              CALCULATE ALPHA TO MINIMIZE FUNCTION 
C     ------------------------------------------------------------------
      II=3
      IF (A2.GT.A3.AND.(IGOOD2.EQ.0.AND.IBEST.EQ.2)) II=2 
      CALL CNMN04 (II,ALPMIN,ZRO,ZRO,F1,SLOPE,A2,F2,A3,F3,ZRO,ZRO)
450   CONTINUE
C     ------------------------------------------------------------------
C                       PROPOSED MOVE 
C     ------------------------------------------------------------------
C     MOVE AT LEAST ENOUGH TO OVERCOME CONSTRAINT VIOLATIONS. 
      A4=ALPFES 
C     MOVE TO MINIMIZE FUNCTION.
      IF (ALPMIN.GT.A4) A4=ALPMIN 
C     IF A4.LE.0, SET A4 = ALPSID.
      IF (A4.LE.0.) A4=ALPSID 
C     LIMIT MOVE TO NEW CONSTRAINT ENCOUNTER. 
      IF (A4.GT.ALPLN) A4=ALPLN 
      IF (A4.GT.ALPNC) A4=ALPNC 
C     LIMIT MOVE TO RE-ENCOUNTER CURRENTLY ACTIVE CONSTRAINT. 
      IF (A4.GT.ALPCA) A4=ALPCA 
C     LIMIT A4 TO 5.*A3.
      IF (A4.GT.(5.*A3)) A4=5.*A3 
C     UPDATE DESIGN.
      IF (IBEST.NE.3.OR.NCON.EQ.0) GO TO 470
C     STORE CONSTRAINT VALUES IN G2.  F3 IS BEST.  F2 IS NOT. 
      DO 460 I=1,NCON 
      G2(I)=G(I)
460   CONTINUE
470   CONTINUE
C     IF A4=A3 AND IGOOD1=0 AND IGOOD3=1, SET A4=.9*A3. 
      ALP=A4-A3 
      IF ((IGOOD1.EQ.0.AND.IGOOD3.EQ.1).AND.ABS(ALP).LT.1.0D-20) A4=.9*A
     13 
C     ------------------------------------------------------------------
C                   MOVE A DISTANCE A4*S
C     ------------------------------------------------------------------
      ALP=A4-A3 
      ALPTOT=ALPTOT+ALP 
      DO 480 I=1,NDV
      X(I)=X(I)+ALP*S(I)
480   CONTINUE
      IF (IPRINT.LT.5) GO TO 510
      WRITE (6,720) 
      WRITE (6,740) A4
      IF (NSCAL.EQ.0) GO TO 500 
      DO 490 I=1,NDV
490   G(I)=SCAL(I)*X(I) 
      WRITE (6,750) (G(I),I=1,NDV)
      GO TO 510 
500   WRITE (6,750) (X(I),I=1,NDV)
510   CONTINUE
C     ------------------------------------------------------------------
C              UPDATE FUNCTION AND CONSTRAINT VALUES
C     ------------------------------------------------------------------
      NCAL(1)=NCAL(1)+1 
      JGOTO=3 
      RETURN
520   CONTINUE
      F4=OBJ
      IF (IPRINT.GE.5) WRITE (6,760) F4 
      IF (IPRINT.LT.5.OR.NCON.EQ.0) GO TO 530 
      WRITE (6,770) 
      WRITE (6,750) (G(I),I=1,NCON) 
530   CONTINUE
C     DETERMINE ACCAPTABILITY OF F4.
      IGOOD4=0
      CV4=0.
      IF (NCON.EQ.0) GO TO 550
      DO 540 I=1,NCON 
      CC=CTAM 
      IF (ISC(I).GT.0) CC=CTBM
      C1=G(I)-CC
      IF (C1.GT.CV4) CV4=C1 
540   CONTINUE
      IF (CV4.GT.0.) IGOOD4=1 
550   CONTINUE
      ALP=A4
      OBJ=F4
C     ------------------------------------------------------------------
C                     DETERMINE BEST DESIGN 
C     ------------------------------------------------------------------
      GO TO (560,610,660),IBEST 
560   CONTINUE
C     CHOOSE BETWEEN F1 AND F4. 
      IF (IGOOD1.EQ.0.AND.IGOOD4.EQ.0) GO TO 570
      IF (CV1.GT.CV4) GO TO 710 
      GO TO 580 
570   CONTINUE
      IF (F4.LE.F1) GO TO 710 
580   CONTINUE
C     F1 IS BEST. 
      ALPTOT=ALPTOT-A4
      OBJ=F1
      DO 590 I=1,NDV
      X(I)=X(I)-A4*S(I) 
590   CONTINUE
      IF (NCON.EQ.0) GO TO 710
      DO 600 I=1,NCON 
      G(I)=G1(I)
600   CONTINUE
      GO TO 710 
610   CONTINUE
C     CHOOSE BETWEEN F2 AND F4. 
      IF (IGOOD2.EQ.0.AND.IGOOD4.EQ.0) GO TO 620
      IF (CV2.GT.CV4) GO TO 710 
      GO TO 630 
620   CONTINUE
      IF (F4.LE.F2) GO TO 710 
630   CONTINUE
C     F2 IS BEST. 
      OBJ=F2
      A2=A4-A2
      ALPTOT=ALPTOT-A2
      DO 640 I=1,NDV
      X(I)=X(I)-A2*S(I) 
640   CONTINUE
      IF (NCON.EQ.0) GO TO 710
      DO 650 I=1,NCON 
      G(I)=G2(I)
650   CONTINUE
      GO TO 710 
660   CONTINUE
C     CHOOSE BETWEEN F3 AND F4. 
      IF (IGOOD3.EQ.0.AND.IGOOD4.EQ.0) GO TO 670
      IF (CV3.GT.CV4) GO TO 710 
      GO TO 680 
670   CONTINUE
      IF (F4.LE.F3) GO TO 710 
680   CONTINUE
C     F3 IS BEST. 
      OBJ=F3
      A3=A4-A3
      ALPTOT=ALPTOT-A3
      DO 690 I=1,NDV
      X(I)=X(I)-A3*S(I) 
690   CONTINUE
      IF (NCON.EQ.0) GO TO 710
      DO 700 I=1,NCON 
      G(I)=G2(I)
700   CONTINUE
710   CONTINUE
      ALP=ALPTOT
      IF (IPRINT.GE.5) WRITE (6,790)
      JGOTO=0 
      RETURN
C     ------------------------------------------------------------------
C                                  FORMATS
C     ------------------------------------------------------------------
C 
C 
720   FORMAT (/5X,25HTHREE-POINT INTERPOLATION) 
730   FORMAT (/////58H* * * CONSTRAINED ONE-DIMENSIONAL SEARCH INFORMATI
     1ON * * *) 
740   FORMAT (//5X,15HPROPOSED DESIGN/5X,7HALPHA =,E12.5/5X,8HX-VECTOR) 
750   FORMAT (1X,8E12.4)
760   FORMAT (/5X,5HOBJ =,E13.5)
770   FORMAT (/5X,17HCONSTRAINT VALUES) 
780   FORMAT (/5X,23HTWO-POINT INTERPOLATION) 
790   FORMAT (/5X,35H* * * END OF ONE-DIMENSIONAL SEARCH) 
      END 


c----- CNMN07
       SUBROUTINE CNMN07 (II,XBAR,EPS,X1,Y1,X2,Y2,X3,Y3) 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      revision history
c     double precision version for workstations
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit           double precision(a-h,o-z)
C     ROUTINE TO FIND FIRST XBAR.GE.EPS CORRESPONDING TO A REAL ZERO
C     OF A ONE-DIMENSIONAL FUNCTION BY POLYNOMIEL INTERPOLATION.
C     BY G. N. VANDERPLAATS                          APRIL, 1972. 
C     NASA-AMES RESEARCH CENTER,  MOFFETT FIELD, CALIF. 
C     II = CALCULATION CONTROL. 
C          1:  2-POINT LINEAR INTERPOLATION, GIVEN X1, Y1, X2 AND Y2. 
C          2:  3-POINT QUADRATIC INTERPOLATION, GIVEN X1, Y1, X2, Y2, 
C              X3 AND Y3. 
C     EPS MAY BE NEGATIVE.
C     IF REQUIRED ZERO ON Y DOES NOT EXITS, OR THE FUNCTION IS
C     ILL-CONDITIONED, XBAR = EPS-1.0 WILL BE RETURNED AS AN ERROR
C     INDICATOR.
C     IF DESIRED INTERPOLATION IS ILL-CONDITIONED, A LOWER ORDER
C     INTERPOLATION, CONSISTANT WITH INPUT DATA, WILL BE ATTEMPTED AND
C     II WILL BE CHANGED ACCORDINGLY. 
      XBAR1=EPS-1.
      XBAR=XBAR1
      JJ=0
      X21=X2-X1 
      IF (ABS(X21).LT.1.0D-20) RETURN 
      IF (II.EQ.2) GO TO 30 
C 
10    CONTINUE
C     ------------------------------------------------------------------
C                  II=1: 2-POINT LINEAR INTERPOLATION 
C     ------------------------------------------------------------------
      II=1
      YY=Y1*Y2
      IF (JJ.EQ.0.OR.YY.LT.0.) GO TO 20 
C     INTERPOLATE BETWEEN X2 AND X3.
      DY=Y3-Y2
      IF (ABS(DY).LT.1.0D-20) GO TO 20
      XBAR=X2+Y2*(X2-X3)/DY 
      IF (XBAR.LT.EPS) XBAR=XBAR1 
      RETURN
20    DY=Y2-Y1
C     INTERPOLATE BETWEEN X1 AND X2.
      IF (ABS(DY).LT.1.0D-20) RETURN
      XBAR=X1+Y1*(X1-X2)/DY 
      IF (XBAR.LT.EPS) XBAR=XBAR1 
      RETURN
30    CONTINUE
C     ------------------------------------------------------------------
C                 II=2: 3-POINT QUADRATIC INTERPOLATION 
C     ------------------------------------------------------------------
      JJ=1
      X31=X3-X1 
      X32=X3-X2 
      QQ=X21*X31*X32
      IF (ABS(QQ).LT.1.0D-20) RETURN
      AA=(Y1*X32-Y2*X31+Y3*X21)/QQ
      IF (ABS(AA).LT.1.0D-20) GO TO 10
      BB=(Y2-Y1)/X21-AA*(X1+X2) 
      CC=Y1-X1*(AA*X1+BB) 
      BAC=BB*BB-4.*AA*CC
      IF (BAC.LT.0.) GO TO 10 
      BAC=SQRT(BAC) 
      AA=.5/AA
      XBAR=AA*(BAC-BB)
      XB2=-AA*(BAC+BB)
      IF (XBAR.LT.EPS) XBAR=XB2 
      IF (XB2.LT.XBAR.AND.XB2.GT.EPS) XBAR=XB2
      IF (XBAR.LT.EPS) XBAR=XBAR1 
      RETURN
      END 


c----- CNMN08 
       SUBROUTINE CNMN08 (NDB,NER,C,MS1,B,N3,N4,N5)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      revision history
c     double precision version for workstations
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit           double precision(a-h,o-z)
      DIMENSION C(N4), B(N3,N3), MS1(N5)
C     ROUTINE TO SOLVE SPECIAL LINEAR PROBLEM FOR IMPOSING S-TRANSPOSE
C     TIMES S.LE.1 BOUNDS IN THE MODIFIED METHOD OF FEASIBLE DIRECTIONS.
C     BY G. N. VANDERPLAATS                             APRIL, 1972.
C     NASA-AMES RESEARCH CENTER,  MOFFETT FIELD, CALIF. 
C     REF.  'STRUCTURAL OPTIMIZATION BY METHODS OF FEASIBLE DIRECTIONS',
C     G. N. VANDERPLAATS AND F. MOSES, JOURNAL OF COMPUTERS 
C     AND STRUCTURES, VOL 3, PP 739-755, 1973.
C     FORM OF L. P. IS BX=C WHERE 1ST NDB COMPONENTS OF X CONTAIN VECTOR
C     U AND LAST NDB COMPONENTS CONTAIN VECTOR V.  CONSTRAINTS ARE
C     U.GE.0, V.GE.0, AND U-TRANSPOSE TIMES V = 0.
C     NER = ERROR FLAG.  IF NER.NE.0 ON RETURN, PROCESS HAS NOT 
C     CONVERGED IN 5*NDB ITERATIONS.
C     VECTOR MS1 IDENTIFIES THE SET OF BASIC VARIABLES. 
C     ------------------------------------------------------------------
C     CHOOSE INITIAL BASIC VARIABLES AS V, AND INITIALIZE VECTOR MS1
C     ------------------------------------------------------------------
      NER=1 
      M2=2*NDB
C     CALCULATE CBMIN AND EPS AND INITIALIZE MS1. 
      EPS=-1.0D+10
      CBMIN=0.
      DO 10 I=1,NDB 
      BI=B(I,I) 
      CBMAX=0.
      IF (BI.LT.-1.0D-6) CBMAX=C(I)/BI
      IF (BI.GT.EPS) EPS=BI 
      IF (CBMAX.GT.CBMIN) CBMIN=CBMAX 
10    MS1(I)=0
      EPS=.0001*EPS 
C     IF (EPS.LT.-1.0E-10) EPS=-1.0E-10 
C 
C  E-10 CHANGED TO E-03 ON 1/12/81
C 
      IF (EPS.LT.-1.0D-03) EPS=-1.0D-03 
      IF (EPS.GT.-.0001) EPS=-.0001 
      CBMIN=CBMIN*1.0D-6
C     IF (CBMIN.LT.1.0D-10) CBMIN=1.0D-10 
C 
C  E-10 CHANGED TO E-05 ON 1/12/81
C 
      IF (CBMIN.LT.1.0D-05) CBMIN=1.0D-05 
      ITER1=0 
      NMAX=5*NDB
C     ------------------------------------------------------------------
C     **********             BEGIN NEW ITERATION              **********
C     ------------------------------------------------------------------
20    ITER1=ITER1+1 
      IF (ITER1.GT.NMAX) RETURN 
C     FIND MAX. C(I)/B(I,I) FOR I=1,NDB.
      CBMAX=.9*CBMIN
      ICHK=0
      DO 30 I=1,NDB 
      C1=C(I) 
      BI=B(I,I) 
C     IF (BI.GT.EPS.OR.C1.GT.0.) GO TO 30 
      IF (BI.GT.EPS.OR.C1.GT.-1.0D-05) GO TO 30 
C 
C  0. CHANGED TO -1.0E-05 ON 1/12/81
C 
      CB=C1/BI
      IF (CB.LE.CBMAX) GO TO 30 
      ICHK=I
      CBMAX=CB
30    CONTINUE
      IF (CBMAX.LT.CBMIN) GO TO 70
      IF (ICHK.EQ.0) GO TO 70 
C     UPDATE VECTOR MS1.
      JJ=ICHK 
      IF (MS1(JJ).EQ.0) JJ=ICHK+NDB 
      KK=JJ+NDB 
      IF (KK.GT.M2) KK=JJ-NDB 
      MS1(KK)=ICHK
      MS1(JJ)=0 
C     ------------------------------------------------------------------
C                     PIVOT OF B(ICHK,ICHK) 
C     ------------------------------------------------------------------
      BB=1./B(ICHK,ICHK)
      DO 40 J=1,NDB 
40    B(ICHK,J)=BB*B(ICHK,J)
      C(ICHK)=CBMAX 
      B(ICHK,ICHK)=BB 
C     ELIMINATE COEFICIENTS ON VARIABLE ENTERING BASIS AND STORE
C     COEFICIENTS ON VARIABLE LEAVING BASIS IN THEIR PLACE. 
      DO 60 I=1,NDB 
      IF (I.EQ.ICHK) GO TO 60 
      BB1=B(I,ICHK) 
      B(I,ICHK)=0.
      DO 50 J=1,NDB 
50    B(I,J)=B(I,J)-BB1*B(ICHK,J) 
      C(I)=C(I)-BB1*CBMAX 
60    CONTINUE
      GO TO 20
70    CONTINUE
      NER=0 
C     ------------------------------------------------------------------
C     STORE ONLY COMPONENTS OF U-VECTOR IN 'C'.  USE B(I,1) FOR 
C     TEMPORARY STORAGE 
C     ------------------------------------------------------------------
      DO 80 I=1,NDB 
      B(I,1)=C(I) 
80    CONTINUE
      DO 90 I=1,NDB 
      C(I)=0. 
      J=MS1(I)
      IF (J.GT.0) C(I)=B(J,1) 
      IF (C(I).LT.0.) C(I)=0. 
90    CONTINUE
      RETURN
      END 

