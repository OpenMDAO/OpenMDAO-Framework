
Introduction
------------

There are currently two ways you can install OpenMDAO on your system. You can 
install from *source* or install from a *bundle*. Both are based on
``zc.buildout``, so they both result in an installation of OpenMDAO and its
dependent packages that should not interfere with the system level Python
install. It is **not** recommended that you use ``easy_install`` to install
OpenMDAO into the system level Python packages directory, because OpenMDAO has
a fairly large number of dependencies, and doing so would likely cause version
conflicts with existing packages.


System Requirements
-------------------

OpenMDAO requires the following list of programs or packages to be installed
at the system level.

    - Python 2.6
    - Python header files (for compiling C/C++/Fortran extensions)
    - setuptools  (a Python package)
    - numpy (a Python package, needed for f2py)
    - C/C++/Fortran compilers
    - bazaar (needed only if access to the OpenMDAO source repository is needed)
    

Specific Platform Requirements
++++++++++++++++++++++++++++++

On **Fedora**, the names of the needed RPM packages are:

    - python-devel
    - python-setuptools
    - gcc-gfortran
    - numpy-f2py
    - bzr


On **Ubuntu**, the .deb package names are:

    - python-dev
    - python-setuptools
    - python-numpy
    - gfortran
    - bzr
    

On **Windows**, download and run the following installers if the corresponding
applications or packages are not already installed:

    - `Python 2.6`__
    - `setuptools`__
    - `numpy`__
    - `mingw32`__
    - `bazaar`__
    
.. __: http://www.python.org/ftp/python/2.6.4/python-2.6.4.msi
.. __: http://pypi.python.org/packages/2.6/s/setuptools/setuptools-0.6c11.win32-py2.6.exe
.. __: http://sourceforge.net/projects/numpy/files/NumPy/1.3.0/numpy-1.3.0-win32-superpack-python2.6.exe/download
.. __: http://sourceforge.net/projects/mingw/files/Automated%20MinGW%20Installer/MinGW%205.1.6/MinGW-5.1.6.exe/download
.. __: http://launchpad.net/bzr/2.1/2.1.0/+download/bzr-2.1.0-1.win32-py2.6.exe


On **OS X**, make sure to install *Xcode*. It's included on the OS X install
disk, and installing it will give you access to *gcc* and *gfortran*.
Next, download and run the following installers if they're not already
installed:

    - `Python 2.6`__
    - `numpy`__
    - `bazaar`__

.. __: http://www.python.org/ftp/python/2.6.4/python-2.6.4_macosx10.3.dmg
.. __: http://sourceforge.net/projects/numpy/files/NumPy/1.3.0/numpy-1.3.0-py2.6-macosx10.5.dmg/download
.. __: http://launchpad.net/bzr/2.1/2.1.0/+download/Bazaar-2.1.0-3.dmg


Installing from Source
----------------------

Official releases of OpenMDAO will have downloadable source tar files available
on Launchpad at ``<put src tar URL here>``.  Once you've downloaded the tar file,
untar it in the desired location.

If you happen to have Bazaar installed on your system or you want to obtain a
"bleeding edge" version, you can obtain the source via the following command:

::

    bzr branch lp:openmdao <your_branch_name>

.. _Building:


Building
++++++++

You should now have a copy of the OpenMDAO source, whether you obtained it via
Bazaar or by untarring a source distribution. Inside of the top level directory
of the distribution is the ``buildout`` directory.  Go to that directory and
execute the following command:

::

    python isolated_bootstrap.py

This should give you output similar to the following:

::

    Creating directory '/myopenmdao_v1.2.3/buildout/bin'.
    Creating directory '/myopenmdao_v1.2.3/buildout/parts'.
    Creating directory '/myopenmdao_v1.2.3/buildout/develop-eggs'.
    Generated script '/myopenmdao_v1.2.3/buildout/bin/buildout'.

Note that on systems with more than one install of Python, you may have
to include the Python version number to get the correct version.  OpenMDAO
requires Python 2.6, so using ``python2.6`` instead of just ``python`` in the 
command above may be necessary.

Once the bootstrapping is complete, it's time to run the buildout. This will
install all of the required Python packages into the buildout's ``eggs`` directory
or into another directory of your choosing if you've specified the buildout
*eggs-directory* in your ``default.cfg`` file.
See :ref:`Setting-Up-a-Local-Cache-of-Installed-Distributions` for details.

To run the buildout, type:

::

    bin/buildout
    
or ``bin\buildout`` on Windows.


This could take a while depending upon whether it's your first OpenMDAO buildout
and whether you have an installed egg cache.


.. note:: Currently, buildout will fail on Windows if the OpenMDAO install
      directory has a pathname that contains spaces.

At this point, your OpenMDAO install should be ready to use. You can test it by
typing:

::

    bin/test --all
    
This will run all of the OpenMDAO unit tests.


Installing a Bundle
-------------------

An OpenMDAO *bundle* is an archive file containing all of the OpenMDAO Python 
packages and any additional third-party packages that OpenMDAO depends upon. It
also contains a buildout configuration file.  To use a bundle, you must first
untar it in the desired location.  As mentioned earlier, on Windows the
destination path should not contain spaces in it or the buildout will fail.

After untarring the bundle, the remaining steps are identical to those in the 
:ref:`Building` section above.



