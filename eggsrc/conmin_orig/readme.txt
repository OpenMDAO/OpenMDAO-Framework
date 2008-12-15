This archive contains the files necessary to use the conmin optimizer in Python.  
If you have Python 2.4 and numpy 1.0.1, then you may be able to use conmin.py and 
conmin_1.pyd as they are, without needing any of the other files in this archive.  
If this does not work or you have different versions of Python or numpy, you may 
need to rebuild conmin_1.pyd.

To rebuild, you'll need f2py and the MinGW fortran compiler.  
Run the following command:

f2py -c -m conmin_1 conmin_original.f

and if all goes well, you will have a new version of conmin_1.pyd.  
Note that numpy 1.0b5 has a bug that prevents this build from finishing, 
so you may need to upgrade your numpy if you have this version.

cmtest.py is an example testing script.  If conmin_1.pyd builds correctly, 
you should be able to run cmtest.py from the command line to see some optimization 
results.  Use cmtest.py as an example for creating your own programs that utilize 
conmin.
