      subroutine etacf(pr,prc,x)
      COMMON/cfi/icf
      pr1=1.2
      x1=1.02
      xc=0.950
      prx=pr
      prcx=prc*xc
      if (pr.lt.pr1) prx=pr1
      if (pr.gt.prcx) prx=prcx
      x=1.+(prx-prcx)/(pr1-prcx)*(x1-1.)
      if (icf.eq.1) x=1.
      return
      end
