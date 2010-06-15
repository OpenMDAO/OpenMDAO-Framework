
.. _Installing-OpenMDAO:


.. _System-Requirements:

System Requirements
===================

These instructions assume that you intend to install OpenMDAO for normal use -- that you will not be hacking on
the OpenMDAO infrastructure or standard library, or developing any plugins that require compilation. If you *do*
intend to perform some kind of OpenMDAO development, you should refer to the installation instructions in the
*Developer Guide,* because the developer environment has some additional system requirements on certain
platforms.

OpenMDAO requires **Python 2.6** to be installed at the system level on all platforms. 
It is highly recommended that **NumPy** (version 1.3 or higher) be 
installed at the system level as well.

**Platform-Specific Requirements**

**Linux:**

Our current OpenMDAO distribution for Linux is a source distribution, so to 
use it, you must have compilers (C and Fortran) on your system.

On **Fedora**, the names of the needed RPM packages are:

    - python-devel
    - numpy
    - gcc-gfortran

On **Ubuntu**, the .deb package names are:

    - python-dev
    - python-numpy
    - gfortran

On **Windows**, you'll need `Python 2.6`__; numpy can be found
`here`__.
    
.. __: http://www.python.org/download/

.. __: http://sourceforge.net/projects/numpy/files/


On **OS X**, make sure to install *Xcode*. It's included on the OS X install
disk, and installing it will give you access to *gcc*. You can also download a newer version
from Apple, but you'll have to fill out a (free) registration to do it. 

If you're running **Snow Leopard**, you'll need to install a version of Python 2.6 from
``python.org`` because the built-in version has a distutils bug that will cause some of the OpenMDAO
tests to fail.  It's not clear exactly what release the fix first appeared in, but version `2.6.5`__
definitely has the fix.

.. __: http://python.org/ftp/python/2.6.5/python-2.6.5-macosx10.3-2010-03-24.dmg

You'll also need gfortran.  You can get a version that goes with Xcode `here`__.

.. __: http://r.research.att.com/gfortran-42-5646.pkg

If you're running **Leopard**, you'll also need `Python 2.6`__ 
and `numpy`__, and you can get gfortran `here`__.  Some other options for installing
gfortran can be found on this `page`__.

.. __: http://python.org/ftp/python/2.6.5/python-2.6.5-macosx10.3-2010-03-24.dmg

.. __: http://sourceforge.net/projects/numpy/files/NumPy/1.4.1/numpy-1.4.1-py2.6-python.org.dmg/download

.. __: http://openmdao.org/downloads/misc/gfortran-macosx-leopard-x86.dmg

.. __: http://gcc.gnu.org/wiki/GFortranBinaries#MacOS

.. _Installation:

Installation
============

To avoid version conflicts with system level Python packages, OpenMDAO is best installed using a
bootstrap script that installs OpenMDAO in a *virtual* Python environment. This script is called
``go-openmdao.py``. 

**Acquire and run the script**

1. Download the installation script: `go-openmdao.py <http://openmdao.org/downloads/latest/go-openmdao.py>`_. If you are using
   Internet Explorer (IE), right click on the link to the left, and then select "Save Target as."

   .. note:: In the case of IE, be sure to save the `go-openmdao.py 
      <http://openmdao.org/downloads/latest/go-openmdao.py>`_ script to the folder where you want to install
      OpenMDAO. Other browsers may automatically download the file to a specific folder (e.g., a Downloads
      folder), and you will have to move it to the desired location (where you want to install OpenMDAO). We
      highly recommend you pick a folder without any spaces in the path name! For instance, on a Windows machine,
      you could put all your OpenMDAO installations in ``C:\\OpenMDAO``.

2. Run the script. If you specify a directory name, the virtual environment will be installed there. If you don't supply a directory name, it
   will default to a name based on the version, for example, ``openmdao-0.1``. 

::

   python go-openmdao.py


This script will check the version of Python you are running. You must be running a version greater than or equal to 2.6 but
less than 3.0. After the script completes successfully you have installed OpenMDAO. There are just a couple more steps to follow
before you can start using it. 

**Activate your virtual environment**

Each time before you want to use openmdao you need to activate it. Activating your virtual environment adds your 
virtual environment's ``bin`` directory to the front of your system path in the current shell so that when you 
type ``python``, you'll get a Python interpreter that's customized for your virtual environment, 
giving you access to everything in OpenMDAO.

Navigate into the folder that was just created by your install script and type the following
command:

On Linux or OS X, you must be running Bash. If you are in Bash, omit this step. 

:: 

   bash

Next, type the following, making sure to include the "." in the command:

::

   . bin/activate


On Windows:

::

   Scripts\activate



**Run tests to verify valid install.**

OpenMDAO has a large test suite which allows you to check and make sure all of the functionality of OpenMDAO will work 
on your system. You can run these tests yourself to double check your installation. It takes only a couple of 
minutes to run them all. 

Type the following to run all tests.

::

   openmdao_test
   
If you don't get any errors or failed tests, everything worked. If you did have some problems, the 
`forum <http://openmdao.org/discussion/forum/3>`_ is your next stop!

Now you are ready to start using OpenMDAO.  When you are done using it, you can deactivate the environment
by typing:

::

   deactivate
   

.. note:: If you need an earlier version of OpenMDAO, its bootstrap script can be downloaded from:
   ``http://openmdao.org/downloads/<openmdao_version>/go-openmdao.py`` 
   where ``<openmdao_version>`` is the version of OpenMDAO you want to install. You can browse 
   through the older versions `here <http://openmdao.org/downloads/>`_.

