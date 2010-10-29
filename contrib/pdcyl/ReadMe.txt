
PDCYL code on torpedo:(Linux)

Following modifications were made to get output data to python environment.


1.  Converted  MAIN.f  to SUBROUTINE PDCYLM()

2.  Replaced the lines which has comments after the statements,  that is,
    have  "!   comments  "  by equivalent code.

3.  All "Arithmetic IF" statements were replaced by equivalent " IF THEN ELSE"
    statement in subroutine PDCEX()..

4.  A MAIN.f is added which calls subroutine PDCYLM().

5.  module pdcylm.so was created as:

    make f2py

6.  f2py module pdcylm.so was tested using script testpdcyl.py

        python testpdcyl.py

    Input  filename = PDCYL.in
    Output filename = PDCYL.out

7.  In test directory, file "test_pdcyl_comp.py" is used to execute and test
    component PdcylComp() in file  pdcyl_comp.py.

    Three tests were included:

    1.   when input file is not defined.

    2.   when code is executed with the given input file.

          input file_name:  PDCYL.inp
          output file name = PDCYL.out

    3.   when input file is being modified -- new input data received from
         other component, input file is being updated with new data and code PDCYL
         was executed.  The output (obtained COMMON BLOCKS) were sent to
         another component.


       10/25/2010
