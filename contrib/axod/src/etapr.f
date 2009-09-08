CEPR
C           CORRELATION OF BLADE ROW EFFICIENCY
C           VS TOTAL/STATIC PRESSURE RATIO
      SUBROUTINE ETAPR(PR,ETA)
      data a,b,c,d/.011852,-.115556,.355555,.64815/
      prx=pr
      pr1=2.5
      pr3=4.0
      if (pr.gt.pr3) prx=pr3
      if (prx.gt.pr1) then
c       eta3=.98
c      eta=1.+(eta3-1.)/(pr1-pr3)**2*(pr1-prx)**2
C RWC to return to orginal form comment above 2 lines
C RWC mod to revert to old formula        
	eta=a*prx**3+b*prx**2+c*prx+d
      else
        eta=1.
      end if
c output file testing the value of eta RWC 12/1/03
c	if (eta.lt.0.999999) write (10,55) eta
c 55   format(1x,f15.9)
      RETURN
      END
