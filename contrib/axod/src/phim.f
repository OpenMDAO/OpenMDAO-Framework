CPHIM
      SUBROUTINE PHIM(EXI,ETA,TR,PR)
      A = EXI-.5
      B = -(EXI+(1.-ETA)/2.)
      C = ETA/2.
      X = (-B -SQRT(B**2 -4.*A*C))/(2.*A)
      TR = ETA/(ETA-X)
      PR = TR**EXI
      RETURN
      END
