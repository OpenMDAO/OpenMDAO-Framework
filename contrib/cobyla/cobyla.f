      SUBROUTINE COBYLA (CALCFC, N,M,X,RHOBEG,RHOEND,IPRINT,MAXFUN,
     & W,IACT,IFAIL,NFVALS,IOUT,IFILE,FF,GG)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER IFAIL,NFVALS,IOUT
      CHARACTER*(*) IFILE
      EXTERNAL CALCFC
      DIMENSION X(*),W(*),IACT(*),GG(*)
C
C     This subroutine minimizes an objective function F(X) subject to M
C     inequality constraints on X, where X is a vector of variables that has
C     N components. The algorithm employs linear approximations to the
C     objective and constraint functions, the approximations being formed by
C     linear interpolation at N+1 points in the space of the variables.
C     We regard these interpolation points as vertices of a simplex. The
C     parameter RHO controls the size of the simplex and it is reduced
C     automatically from RHOBEG to RHOEND. For each RHO the subroutine tries
C     to achieve a good vector of variables for the current size, and then
C     RHO is reduced until the value RHOEND is reached. Therefore RHOBEG and
C     RHOEND should be set to reasonable initial changes to and the required   
C     accuracy in the variables respectively, but this accuracy should be
C     viewed as a subject for experimentation because it is not guaranteed.
C     The subroutine has an advantage over many of its competitors, however,
C     which is that it treats each constraint individually when calculating
C     a change to the variables, instead of lumping the constraints together
C     into a single penalty function. The name of the subroutine is derived
C     from the phrase Constrained Optimization BY Linear Approximations.
C
C     The user must set the values of N, M, RHOBEG and RHOEND, and must
C     provide an initial vector of variables in X. Further, the value of
C     IPRINT should be set to 0, 1, 2 or 3, which controls the amount of
C     printing during the calculation. Specifically, there is no output if
C     IPRINT=0 and there is output only at the end of the calculation if
C     IPRINT=1. Otherwise each new value of RHO and SIGMA is printed.
C     Further, the vector of variables and some function information are
C     given either when RHO is reduced or when each new value of F(X) is
C     computed in the cases IPRINT=2 or IPRINT=3 respectively. Here SIGMA
C     is a penalty parameter, it being assumed that a change to X is an
C     improvement if it reduces the merit function
C                F(X)+SIGMA*MAX(0.0,-C1(X),-C2(X),...,-CM(X)),
C     where C1,C2,...,CM denote the constraint functions that should become
C     nonnegative eventually, at least to the precision of RHOEND. In the
C     printed output the displayed term that is multiplied by SIGMA is
C     called MAXCV, which stands for 'MAXimum Constraint Violation'. The
C     argument MAXFUN is an integer variable that must be set by the user to a
C     limit on the number of calls of CALCFC, the purpose of this routine being
C     given below. The value of MAXFUN will be altered to the number of calls
C     of CALCFC that are made. The arguments W and IACT provide real and
C     integer arrays that are used as working space. Their lengths must be at
C     least N*(3*N+2*M+11)+4*M+6 and M+1 respectively.
C
C     In order to define the objective and constraint functions, we require
C     a subroutine that has the name and arguments
C                SUBROUTINE CALCFC (N,M,X,F,CON)
C                DIMENSION X(*),CON(*)  .
C     The values of N and M are fixed and have been defined already, while
C     X is now the current vector of variables. The subroutine should return
C     the objective and constraint functions at X in F and CON(1),CON(2),
C     ...,CON(M). Note that we are trying to adjust X so that F(X) is as
C     small as possible subject to the constraint functions being nonnegative.
C
C     Partition the working space array W to provide the storage that is needed
C     for the main calculation.
C
      MPP=M+2
      ICON=1
      ISIM=ICON+MPP
      ISIMI=ISIM+N*N+N
      IDATM=ISIMI+N*N
      IA=IDATM+N*MPP+MPP
      IVSIG=IA+M*N+N
      IVETA=IVSIG+N
      ISIGB=IVETA+N
      IDX=ISIGB+N
      IWORK=IDX+N
      CALL COBYLB (CALCFC,N,M,MPP,X,RHOBEG,RHOEND,IPRINT,MAXFUN,W(ICON),
     1  W(ISIM),W(ISIMI),W(IDATM),W(IA),W(IVSIG),W(IVETA),W(ISIGB),
     2  W(IDX),W(IWORK),IACT,IFAIL,NFVALS,IOUT,IFILE,FF,GG)
      RETURN
      END
