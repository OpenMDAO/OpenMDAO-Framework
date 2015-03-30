To set up a development virtual environment for OpenMDAO, run:

    python install_openmdao_dev.py

where python is in the 2.7.x series.

If you have vanilla Python installed, this will create a virtualenv within the current directory
called 'devenv'. If you have Anaconda, this will create a conda environment called 'openmdao-dev' by default.
Either installs  all of the openmdao namespace packages into their respective environments as 'develop' eggs.

For additional options, run:

    python install_openmdao_dev.py --help

For more information, go to http://openmdao.org/dev_docs
