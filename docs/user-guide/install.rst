
Installing OpenMDAO
===================

System Requirements
-------------------

These instructions assume that you intend to install OpenMDAO for normal use.
You will not be hacking on the OpenMDAO infrastructure or standard library, or
developing any plugins that require compilation. If you do intend to do
OpenMDAO development of some kind, you should refer to the installation
instructions in the Developer Guide, because the developer environment has
some additional system requirements on certain platforms.

OpenMDAO requires the following programs or packages to be installed
at the system level on all platforms.

    - Python 2.6
    

**Platform-Specific Requirements**

**Linux:**

Our current distribution for Linux is a source distribution, so in order to 
use it, you must have compilers (C and fortran) on your system.

On **Fedora**, the names of the needed RPM packages are:

    - python-devel
    - numpy
    - gcc-gfortran

On **Ubuntu**, the .deb package names are:

    - python-dev
    - python-numpy
    - gfortran    

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


.. note:: If you wish to view the source files, please see :ref:`Installing-from-Source`. 

Installing and Running the Script
----------------------------------

To avoid version conflicts with system level Python packages, OpenMDAO is best installed using a
bootstrap script that installs OpenMDAO in a *virtual* Python environment. This script is called
``go-openmdao.py``. 

**Step 1)** Acquire and run the script.

   1. Download the installation script: `go-openmdao.py <http://openmdao.org/downloads/latest/go-openmdao.py>`_ 

   2. Run the script. If you specify a directory name, the virtual environment will be
      installed there. If you don't, it will default to a name based on the version, for example:
      openmdao-0.1. 

::

   python2.6 go-openmdao.py


**Step 2)** Activate your virtual environment.

Navigate into the folder that was just created by your install script, and type the following
command:

::

  Linux: source bin/activate       (you must be running bash to do this)

  Windows: Scripts\activate.bat
  
Activating your virtual environment adds OpenMDAO's `devenv/bin` directory to your system
path, so that when you launch Python, you have access to everything in OpenMDAO.

Now you are ready to go with OpenMDAO!


.. note:: If you need an earlier version of OpenMDAO, its bootstrap script can be downloaded from:
   ``http://openmdao.org/downloads/<openmdao_version>/go-openmdao.py`` where
   where ``<openmdao_version>`` is the version of OpenMDAO you want to install. 

