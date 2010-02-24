C---------------------------------------
C ANALYSIS PROGRAM FOR A THREE BAR TRUSS 
C WITH STRESS, DISPACEMENT AND FREQUENCY
C---------------------------------------
C

      SUBROUTINE runbar3truss(PX, PY, M0, A1,A2,A3,E,EL,RHO,
     *                        FORCE1,FORCE2,FORCE3,
     *                        S1, S2,S3, U,V,FF,OBJ) 
C
      IMPLICIT None

      Integer mxndv, mxcon
      PARAMETER (MXNDV = 150, MXCON = 400)

      Double Precision px, py,m0
      Double Precision a1,a2,a3
      Double Precision E, EL, RHO 

      Double Precision force1,force2,force3
      Double Precision S1, S2, S3
      Double Precision U, V, FF 
      Double Precision obj

Cf2py intent(in) px  
Cf2py intent(in) py
Cf2py intent(in) mo
Cf2py intent(in) a1 
Cf2py intent(in) a2  
Cf2py intent(in) a3   
Cf2py intent(in) e
Cf2py intent(in) el
Cf2py intent(in) rho
Cf2py intent(out) force1
Cf2py intent(out) force2
Cf2py intent(out) force3
Cf2py intent(out) s1
Cf2py intent(out) s2 
Cf2py intent(out) s3    
Cf2py intent(out) u   
Cf2py intent(out) v      
Cf2py intent(out) ff     
Cf2py intent(out) obj    

      Double Precision DOBJ(3) 
      Double Precision GSTRES(3,3), GDIS(3,2), GRADFF(3)
      Double Precision SOM, SOM2, OMEGA, CON, BTM

      Double Precision EE

      Integer I

      Double Precision Q  
      Double Precision PI, ABOT, ABOT2


      Double Precision FFI, FF2, ZZZ 


C
C   BAR FORCES ( F1, F2, F3 )
C   BAR STRESSES ( S1,S2,S3)
C   FREQUENCY FF
C   NODAL DISPLACEMENTS ( U,V)
C   LOADS PX,PY
C   LENGTH PARAMETER EL
C
C   CALCULATION OF STRESSES
C   THE PARAMETERS ARE AS SPECIFIED

      Q = DSQRT(2.0D0)
      PI = 3.1415926535D0
      ABOT = A1 * A2 +  A2 * A3  + Q * A1 * A3
      ABOT2 = ABOT * ABOT
C
      S1 =( A3 * PY -  ( A3 + Q * A2  ) * PX ) / ABOT
      S2 = - ( ( A3 - A1 )* PX -  ( A1 + A3  )* PY ) / ABOT
      S3 =  (  ( A1 + Q * A2 ) * PX + A1 * PY ) / ABOT

C calculate forces

      force1 = a1*S1
      force2 = a2*S2
      force3 = a3*S3

      V = EL*S2/E
      U = (EL/E)* ( - 2* S1 + S2 )
      FFI =   (1/ (2 * Q) ) *(  (A1 + A3 + Q * A2) -
     $     DSQRT (  (A1 - A3 )* ( A1 - A3 ) + 2 * A2*A2  ) )
      FF2= FFI* ( (1000* E ) /(EL * M0 ) )
      FF = DSQRT (FF2)/(2*PI)
C
C  GRADIENT OF WEIGHT

      OBJ =  RHO*(A1*EL*Q  + A2*EL + A3*EL*Q)
C
      DOBJ(1) = RHO*EL*Q
      DOBJ(2) = RHO*EL
      DOBJ(3) = RHO*EL*Q
C 
C STRESS GRADIENTS GSTRESS(3X3)
C THIS IS A 3X3 MATRIX ROWS ARE WITH RESPECT TO AREAS
C
C COLUMNS WITH RESPECT TO STRESS VARIABLES
C
      ZZZ = (PX*(A2 + Q*A3)*(A3+Q*A2)
     $      - PY*(A3*(A2 + Q*A3))  )

      GSTRES(1,1) = (   PX* (A2 + Q *A3)* (A3+Q*A2)
     $      - PY* ( A3 * ( A2 + Q * A3) )  ) /ABOT2

      GSTRES(2,1) = (   PX*  A3 *  (A3-A1)
     $      + PY*(- A3 * ( A1 + A3) )   )/ABOT2

      GSTRES(3,1) = ( PX*  A2 *(A1+ Q*A2)
     $      + PY*( A1*A2  )   )/ABOT2

      GSTRES(1,2) = ( PX*   Q*A3 *(A3+Q*A2)
     $      - PY*( A3 *  Q * A3)    )/ABOT2

      GSTRES(2,2) = ( PX*(A3 + A1 )*(A3-A1)
     $      - PY*( A1 + A3 )* (A1 + A3  )   )/ABOT2

      GSTRES(3,2) = ( PX*(-Q*A1)*(A1+ Q*A2)
     $      + PY*(- A1*Q* A1  )   )/ABOT2

      GSTRES(1,3) = (PX* (- A2 * ( Q*A2 +A3))
     $      + PY* A3 * A2   )/ABOT2

      GSTRES(2,3) = ( PX*(A1 )*(A3-A1)
     $      - PY*( A1  )* (A1 + A3  )   )/ABOT2

      GSTRES(3,3) = ( PX*(- A1 - Q*A2 )*(A2+ Q*A1)
     $      - PY*( A1 * ( A2 + Q*A1  ) ) )/ABOT2


C
C DISPLACEMENT GRADIENT
C
C THE TWO DISPLACEMENT GRADIENTS ARE ( GDIS (2X2) )
C
C COLUMNS FOR U AND V , ROWS FOR A1, A2, A3
C
       DO 30 I = 1,3
          GDIS(I,1)  =  (EL/E)* GSTRES (I,2)
          GDIS(I,2)  =  (EL/E)* (-2.d0* GSTRES(I,1) + GSTRES(I,2) )
30    CONTINUE

C
C FREQUENCY GRADIENTS (3X1) GRADF
C

      SOM = 2.d0*PI*FF
      EE = E*1000.d0
      SOM2 = SOM*SOM
      OMEGA = ( EL*M0)*SOM2/EE
      CON =DSQRT ( EE / ( EL* M0 ) )* ( 1 / (8.d0*Q*PI*DSQRT (OMEGA ) ))
      BTM = DSQRT ( ( A1 - A3 )* ( A1 - A3 ) + 2.d0* A2*A2 )
C
      GRADFF(1) = CON* (1 - ( A1 - A3 )/BTM )
      GRADFF(2) = CON* ( Q - 2*A2/BTM )
      GRADFF(3) = CON * ( 1 + (A1 - A3 )/ BTM )
 
      RETURN
      END
