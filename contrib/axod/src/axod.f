C  main program AXOD
C
C  *** This version of AXOD reads input from 'axod.inp' and writes output
c     to 'axod.out'.
c
C     NASA TURBINE PROGRAM

      PROGRAM MAIN
      REAL HPOWER, TOTT(48), TOTP(48), TOTF(48), EFFS(48), EFFR(48)
      CALL AXOD(HPOWER, TOTT, TOTP, TOTF, EFFS, EFFR)
      END
