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
C     IF(I-15)7,9,9
C     replaced by ...........
      IF((I-15).LT.0) THEN  
         GO TO 7
      ELSE
         GO TO 9
      ENDIF
C7     IF(ABS(DX)-PRTOL)9,9,5
C     replaced by ...........
7     IF((ABS(DX)-PRTOL).LE.0.) THEN  
         GO TO 9
      ELSE
         GO TO 5
      ENDIF
5     RTVZ=SQRT(ETA*X-C)
      F=D*PTR*(1.-ETA*X)-RTVZ
      DFDX=D*(A-ETA*(1.+X/(GAMX-1.)))*PTR/(1.-X)
     1-ETA/2./RTVZ
      DX=-F/DFDX
C     IF(DX)11,11,13
C     replaced by ...........
      IF(DX.LE.0.) THEN  
         GO TO 11
      ELSE
         GO TO 13
      ENDIF
11    XUP=X
      GO TO 15
13    XLOW=X
15    X=X+DX
C17    IF(ABS(2.*X-XUP-XLOW)-XUP+XLOW)3,1,1
C     replaced by ...........
17    IF((ABS(2.*X-XUP-XLOW)-XUP+XLOW).LT.0.) THEN  
         GO TO 3
      ELSE
         GO TO 1
      ENDIF
9     CONTINUE
      RETURN
      END
