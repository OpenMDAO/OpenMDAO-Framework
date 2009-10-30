C GETDATA.f  
C
C    THE ROUTINE IS ADDED TO get VALUES OF hp, tt2a pt2a wg2a  ETAS ETAR VARIABLES
c
C     ETAS       = STATOR EFFICIENCY (input)
C     ETAR       = ROTOR EFFICIENCY  (input)
C     WG2A       = Mass Flow
C     TT2A       = Total Temperature
C     PT2A       = Total Pressure
C     HP         = Power output
c
C     CALLED from OVRALL .....
C
      SUBROUTINE GETDATA(HP, TT2A, PT2A, WG2A, ETAS, ETAR, KS)
C
C     REAL HP, TT2A(6,8), PT2A(6,8), WG2A(6,8), ETAS, ETAR
      INTEGER KS1, KS, I
      REAL HP1, TT2A(KS), PT2A(KS), WG2A(KS), ETAS(KS), ETAR(KS)
      COMMON /DATTRF/ HP1, TT123(48), PT123(48), WG123(48), ETAS123(48),
     *                ETAR123(48), KS1

      KS  = KS1
      HP  = HP1
C     print *,'  In GETDATA **********HP1 =',HP1 ,'  KS1 =',KS1   
      
      DO 100 I = 1,KS
c
         TT2A(I) = TT123(I)
         PT2A(I) = PT123(I)
         WG2A(I) = WG123(I)
         ETAS(I) = ETAS123(I)
         ETAR(I) = ETAR123(I)
       
100   CONTINUE
      RETURN
      END
