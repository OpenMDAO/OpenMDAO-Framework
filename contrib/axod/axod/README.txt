Use of f2py to run a Fortran-77 code.


F2PY is a program which provides a connection between Python and Fortran routines
by wrapping a Fortran program and creating a module which can then be executed.
To understand the use of f2py, a simple Fortran code related to turbine flows AXOD is used.


AXOD is an Axial-flow Turbine Off-design Fortran-77 code, made of 30 routines.
Followwing modifications were needed to create a python wrapped module.


1.  Main program (Program AXOD) was modified to run as a subroutine 
    axodm.f.

        SUBROUTINE AXOD ( )

2.  f2py allows only one no-name common block. To satisfy this requirement,
    2 common blocks were changed from no-name to named common blocks.

     C1FIT1, C2FIT2 in (fit.f,fit2.f, inst2.f, instg.f)

     Only one no-name common block is left in the code.  No-name common block 
     variable 'RES( )' dimension was changed. The common block does not have the same
     structute in various routines. 

3.   A  new subroutine 'icommon.f' is added which initializes the values of common block
      variables.

4.   The f2py module axodm was created as:

      f2py  -c -m axodm  --f77flags=' -g - fno-automatic'  *.f

      This will create a wrapper module "axodm.so" which can be used to run
      the AXOD  code under python environment.  The script to run the code 
      'testaxod.py' is created.

5.    Execute it as "python testaxod.py" from the commond line.

6.    The wrapper module was tested by executing the code using two different
      input files, namely, hpt.inp & lpt.inp. The script allows to run the code
      in a loop by changing input files. The output produced matches with the output 
      obtained from a run with the original code.

7.    Makefile was modified to create an f2py wrapper module 'axodm.so'.
  
      Use  "make f2py"                  for  f2py module
           "make     "                  for  axod executable (default)
