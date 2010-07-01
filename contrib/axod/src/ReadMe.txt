Following modifications were made to get output data to python environment.

1.  Added a common block  DATTRF in subroutines axod, tempdata,inst2

      COMMON /DATTRF/HP1, TT123(48), PT123(48), WG123(48), ETAS(48), ETAR(48), KS

             HP1 = Turbine Power
             TT123 = Total Temperature
             PT123 = Total Pressure
             WG123 = Total Mass flow
             ETAS  = Stator Efficiency
             ETAR  = Rotor Efficiency
             KS    = Number of Stages

2.   Added  subroutines 'tempdata.f, getdata.f'

3.   Modified routines axodm.f, ovrall.f, inst2.f

4.   Module axod.so is prepared as:

     make f2py

5.   This can be tested by:

     cd ../test
     python testaxod.py

     This will display AXOD internals, then run eee_hpt and one_stage
     input files.  

6.   This version of code was done on 6/28/2010

     All "Arithmetic IF" statements are replaced by equivalent "IF THEN"
     statements.  This was done to remove all warning messages seen during
     gfortran compilation process.

