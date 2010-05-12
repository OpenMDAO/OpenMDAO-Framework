
.. _Installing-OpenMDAO:


.. _System-Requirements:

System Requirements
===================

These instructions assume that you intend to install OpenMDAO for normal use.
You will not be hacking on the OpenMDAO infrastructure or standard library, or
developing any plugins that require compilation. If you *do* intend to perform
some kind of OpenMDAO development, you should refer to the installation
instructions in the Developer Guide, because the developer environment has
some additional system requirements on certain platforms.

OpenMDAO requires **Python 2.6** to be installed at the system level on all platforms. 
It is highly recommended that **numpy** (version 1.3 or higher) be 
installed at the system level as well.

**Platform-Specific Requirements**

**Linux:**

Our current OpenMDAO distribution for Linux is a source distribution, so in order to 
use it, you must have compilers (C and fortran) on your system.

On **Fedora**, the names of the needed RPM packages are:

    - python-devel
    - numpy
    - gcc-gfortran

On **Ubuntu**, the .deb package names are:

    - python-dev
    - python-numpy
    - gfortran

On **Windows**, you'll need `Python 2.6`__, and numpy can be found
`here`__.
    
.. __: http://www.python.org/ftp/python/2.6.4/python-2.6.4.msi

.. __: http://sourceforge.net/projects/numpy/files/NumPy/1.4.1/numpy-1.4.1-win32-superpack-python2.6.exe/download


On **OS X**, make sure to install *Xcode*. It's included on the OS X install
disk, and installing it will give you access to *gcc*. You'll need `Python 2.6`__
and `numpy`__, and you can obtain gfortran binaries `here`__.

.. __: http://www.python.org/ftp/python/2.6.4/python-2.6.4_macosx10.3.dmg

.. __: http://sourceforge.net/projects/numpy/files/NumPy/1.4.1/numpy-1.4.1-py2.6-python.org.dmg/download

.. __: http://gcc.gnu.org/wiki/GFortranBinaries#MacOS


Installation
============

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

On Linux or OS X (You must be running bash. If you're not, just type ``bash`` first):

::

   source bin/activate

On Windows:

::

   Scripts\activate


Activating your virtual environment adds your virtual environment's `bin` directory to 
the front of your system path in the current shell, so that when you type ``python``, 
you'll get a Python interpreter that's customized for your virtual environment, 
giving you access to everything in OpenMDAO.

Now you are ready to start using OpenMDAO.


.. note:: If you need an earlier version of OpenMDAO, its bootstrap script can be downloaded from:
   ``http://openmdao.org/downloads/<openmdao_version>/go-openmdao.py`` 
   where ``<openmdao_version>`` is the version of OpenMDAO you want to install. 

