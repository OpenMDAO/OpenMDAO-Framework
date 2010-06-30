CBSFT
C           BESFIT - LEAST SQUARES CURVE-FITTING ROUTINE
C           USING ORTHOGONAL POLYNOMIALS WITH STATISTICAL
C           TERMINATION
C
C           PROGRAMMER - J.J.MILIA HMED
C
      SUBROUTINE BESFIT(N,X,Y,K,D,RES)
      DIMENSION X(1),Y(1),D( 6),A( 8),B( 8),S( 8),RES( 8),TAB( 6)
      DIMENSION P0(  6),P1(  6),R(6,6)
c     EQUIVALENCE (P0,R),(P1,R(  7))
      EQUIVALENCE (P0,R(1,1)),(P1,R(1,2))
      DATA  TAB           /12.706,4.303,3.182,2.776,2.571,2.447/
      IF(N.GT.1) GO TO 50
      D(1)=Y(1)
      K=0
      GO TO 55
50    KT=0
      IF=K
      IAF=MIN0(IABS(IF),20)
      M=N
      NDEG=M-1
      J=0
      C0=M
      A(1)=0.
      B(1)=0.
      S(1)=0.
      RES(1)=0.
      DO 1 I=1,M
      P0(I)=0.
      P1(I)=1.
      S(1)=S(1)+Y(I)
      A(1)=A(1)+X(I)
1     RES(1)=Y(I)**2+RES(1)
      A(1)=A(1)/C0
      S(1)=S(1)/C0
      RES(1)=RES(1)-S(1)**2*C0
      RMS=RES(1)/(C0-1.)
3     J=J+1
      IF(J.GT. IAF.AND.IF.NE.0 ) GO TO 27
      IF(J.LE.22) GO TO 24
27    K=IAF
      LL=K+1
C     WRITE(16,29) IAF
      GO TO 4
24    C1=0.
      XPP=0.
      YP=0.
      DO 2 I=1,M
      Q=P1(I)
      P1(I)=(X(I)-A(J))*Q-B(J)*P0(I)
      P0(I)=Q
      PP=P1(I)**2
      C1=PP+C1
      XPP=XPP+X(I)*PP
2     YP=YP+Y(I)*P1(I)
      S(J+1)=YP/C1
      SSR=S(J+1)**2*C1
      RES(J+1)=RES(J)-SSR
      IF(RES(J+1).GT.0.) GO TO 13
      K=J
      IF(RES(J).LT.ABS(RES(J+1))) K=J-1
      LL=K+1
C     WRITE(16,14) K
      CONTINUE
      GO TO 4
13    A(J+1)=XPP/C1
      B(J+1)=C1/C0
      C0=C1
      LL=J+1
C     IF(IF.GT.0) IF(LL-IF) 3,3,4
C     replaced by ..........
      IF(IF.GT.0) THEN 
         IF(( LL-IF).LE.0) THEN
             GO TO 3
         ELSE
             GO TO 4 
         ENDIF
      ENDIF
      NDEG=NDEG-1
      RMS=RES(J+1)/FLOAT(NDEG)
C     ISUB=MIN0(NDEG,20)
      ISUB=MIN0(NDEG, 6)
      T=SQRT(SSR/RMS)
C array index error here - 11/25/03 RWC
	if (ISUB.LT.1) ISUB=1

      IF(T.LE.TAB(ISUB)) GO TO 17
      KT=0
      GO TO 3
17    S(J+1)=0.
      RES(J+1)=RES(J)
      NDEG=NDEG+1
      IF(KT.EQ.1) GO TO 18
      KT=1
      GO TO 3
18    LL=J-1
      K=J-2
4     R(1,1)=1.
      R(1,2)=-A(1)
      R(2,2)=1.
      R(2,1)=0.
C     IF(K-2) 6,8,8
C     replaced by..........
      IF(K.LT.2) THEN 
        GO TO 6
      ELSE
        GO TO 8
      ENDIF
8     DO 9 I=3,LL
      R(1,I)=-A(I-1)*R(1,I-1)-B(I-1)*R(1,I-2)
      LOOP=I-2
      R(I-1,I)=R(I-2,I-1)-A(I-1)*R(I-1,I-1)
      R(I,I)=1.
      DO 9 JJ=2,LOOP
      R(JJ,I)=R(JJ-1,I-1)-A(I-1)*R(JJ,I-1)-B(I-1)*R(JJ,I-2)
9     CONTINUE
6     DO 10 JJ=1,LL
      D(JJ)=0.
      DO 10 L=JJ,LL
10    D(JJ)=D(JJ)+S(L)*R(JJ,L)
55    RETURN
14    FORMAT(28H0BESFIT RETURNED WITH ORDER ,I2//)
29    FORMAT(18H0BESFIT LIMITED TO,I3,9HTH ORDER.//)
      END
