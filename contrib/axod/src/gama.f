CGAMA
C
C     CALCULATE SPECIFIC HEAT RATIO FOR MIXTURE
      SUBROUTINE GAMA(P,T,F,W,GAMX)
      CALL CPA(P,T,F,W,CPAX)
C     IF(F)2,2,1
C     replaced by ..............
      IF(F.LE.0.) THEN
         GO TO 2
      ELSE
         GO TO 1
      ENDIF
1     CALL CPF(P,T,F,W,CPFX)
C2     IF(W)4,4,3
C     replaced by ..............
2     IF(W.LE.0.) THEN
         GO TO 4
      ELSE
         GO TO 3
      ENDIF
3     CALL CPW(P,T,F,W,CPWX)
4     CPGX=(CPAX+F*CPFX+W*CPWX)/(1.+F+W)
      CALL R(P,T,F,W,RX)
      GAMX=CPGX/(CPGX-RX/778.161)
      RETURN
      END
