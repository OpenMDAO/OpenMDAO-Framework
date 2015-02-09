To set up a development virtual environment for OpenMDAO, run:

    python go-openmdao-dev.py

where python is in the 2.7.x series.

This will create a virtualenv within the current directory
called 'devenv', with all of the openmdao namespace packages
installed in it as 'develop' eggs.

To set up using conda:

    #OS X/Linux
    . conda-openmdao-dev.sh
    
    #Windows
    conda-openmdao-dev.bat

For more information, go to http://openmdao.org/dev_docs
