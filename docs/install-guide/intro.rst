
Introduction
------------

In order to avoid version conflicts with system level Python packages,
OpenMDAO is best installed using a bootstrap script that installs OpenMDAO in
a *virtual* Python environment.  This script is called ``go-openmdao.py`` and
it can be downloaded from:

::

    http://openmdao.org/downloads/<openmdao_version>/go-openmdao.py
    
where *<openmdao_version>* is the version of openmdao you want to install. To
get the latest version, download

::

    http://openmdao.org/downloads/latest/go-openmdao.py
    


System Requirements
-------------------

OpenMDAO requires the following list of programs or packages to be installed
at the system level.

    - Python 2.6
    - bazaar (needed only if access to the OpenMDAO source repository is needed)
    

Specific Platform Requirements
++++++++++++++++++++++++++++++

On **Fedora**, the names of the needed RPM packages are:

    - python-devel
    - gcc-gfortran
    - bzr


On **Ubuntu**, the .deb package names are:

    - python-dev
    - gfortran
    - bzr
    

On **Windows**, download and run the following installers if the corresponding
applications or packages are not already installed:

    - `Python 2.6`__
    - `mingw32`__
    - `bazaar`__
    
.. __: http://www.python.org/ftp/python/2.6.4/python-2.6.4.msi
.. __: http://sourceforge.net/projects/mingw/files/Automated%20MinGW%20Installer/MinGW%205.1.6/MinGW-5.1.6.exe/download
.. __: http://launchpad.net/bzr/2.1/2.1.0/+download/bzr-2.1.0-1.win32-py2.6.exe


On **OS X**, make sure to install *Xcode*. It's included on the OS X install
disk, and installing it will give you access to *gcc* and *gfortran*.
Next, download and run the following installers if they're not already
installed:

    - `Python 2.6`__
    - `bazaar`__

.. __: http://www.python.org/ftp/python/2.6.4/python-2.6.4_macosx10.3.dmg
.. __: http://launchpad.net/bzr/2.1/2.1.0/+download/Bazaar-2.1.0-3.dmg


Installing from Source
----------------------

Official releases of OpenMDAO will have downloadable source tar files available
on Launchpad at ``http://openmdao.org/downloads/<version_id>/openmdao_src-<version_id>.tar.gz``.
Once you've downloaded the tar file, untar it in the desired location.

If you happen to have Bazaar installed on your system or you want to obtain a
"bleeding edge" version, you can obtain the source via the following command:

::

    bzr branch lp:openmdao <your_branch_name>

.. _Building:


Building
++++++++

You should now have a copy of the OpenMDAO source, whether you obtained it via
Bazaar or by untarring a source distribution. 

.. note:: add explanation of bootstrapping script


At this point, your OpenMDAO install should be ready to use. You can test it by
typing:

::

    bin/nosetests openmdao
    
This will run all of the OpenMDAO unit tests.




