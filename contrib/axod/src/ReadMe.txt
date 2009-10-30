Following modifications were made to get output data to python environment.


1.  Added a common block  DATTRF in subroutines axod, tempdata,inst2

      COMMON /DATTRF/HP1, TT123(48), PT123(48), WG123(48), ETAS(48), ETAR(48), KS

             HP1 = Turbne Power
             TT123 = Total Temperature
             PT123 = Total Pressure
             WG123 = Total Mass flow
             ETAS  = Stator Efficiency
             ETAR  = Rotor Efficiency
             KS    = Number of Stages

2.   Added  subroutines 'tempdata.f, getdata.f'

3.   Modified routines axodm.f, ovrall.f, inst2.f

4.   Module  axodn.so is prepared as:

     f2py -c -m axodn --f77flags='-g -fno-automatic'  *.f

5.   program was executed as:

      python axod_comp2.py

      The input file used "hpt.inp" is for G90 engine.
      Output file is axod.out
