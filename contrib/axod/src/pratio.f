CPRATIO
C     CALCULATE PRESSURE RATIO
      SUBROUTINE PRATIO(TFF,GAMX,RX,PTR ,PRTOL,ETA,C,k)
      A=GAMX/(GAMX-1.)
      D=TFF*SQRT(RX/(64.3481*A))
      XUP=(GAMX-1.+2.*C)/(GAMX+1.)/ETA
      XLOW=C/ETA
      DX=1.
      X=XLOW+D**2/ETA
      PTR=1.7
      I=0
      GO TO 17
1     X=(XUP+XLOW)/2.
      DX=XUP-XLOW
3     CALL ETAPR(PTR,ETA1)
      PTR=1./(1.-X/ETA1)**A
      I=I+1
      IF(I-15)7,9,9
7     IF(ABS(DX)-PRTOL)9,9,5
5     RTVZ=SQRT(ETA*X-C)
      F=D*PTR*(1.-ETA*X)-RTVZ
      DFDX=D*(A-ETA*(1.+X/(GAMX-1.)))*PTR/(1.-X)
     1-ETA/2./RTVZ
      DX=-F/DFDX
      IF(DX)11,11,13
11    XUP=X
      GO TO 15
13    XLOW=X
15    X=X+DX
17    IF(ABS(2.*X-XUP-XLOW)-XUP+XLOW)3,1,1
9     CONTINUE
      RETURN
      END
