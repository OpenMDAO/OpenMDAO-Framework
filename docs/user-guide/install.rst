
.. _Installing-OpenMDAO:


.. _System-Requirements:


System Requirements
===================

These instructions assume that you intend to install OpenMDAO for normal use -- that you will not be
hacking on the OpenMDAO infrastructure or standard library, or developing any plugins that require
compilation. If you *do* intend to perform some kind of OpenMDAO development, you should refer to
the installation instructions in the *Developer Guide* because the developer environment has some
additional :ref:`system requirements <developer-requirements>` on certain platforms.

OpenMDAO requires that the following software be installed at the system level on all platforms:

- Python 2.6 

- NumPy (version 1.3 or higher) 

- SciPy  

- Matplotlib


**Platform-Specific Requirements**

We recommend that you read the entire section relating to your desired platform before installing any software.

**Linux:**

Our current OpenMDAO distribution for Linux is a source distribution, so to 
use it, you must have compilers (C and Fortran) on your system.

- On **Fedora**, the names of the needed RPM packages are:

 - python-devel
 - numpy
 - scipy
 - python-matplotlib
 - gcc-gfortran

- On **Ubuntu**, the .deb package names are:

 - python-dev
 - python-numpy
 - python-scipy
 - python-matplotlib
 - gfortran

**Windows**: 

- `Python 2.6 <http://www.python.org/download/releases//>`_

- `numpy <http://sourceforge.net/projects/numpy/files/>`_ 

- `SciPy <http://sourceforge.net/projects/scipy/files/>`_

- `Matplotlib <http://sourceforge.net/projects/matplotlib/files/matplotlib/matplotlib-1.0/>`_


**Mac OS X**:

Our current OpenMDAO distribution for OS X is a source distribution, so to 
use it, you must have compilers (C and Fortran) on your system.

- Xcode -- It's included on the OS X install disk, and installing it will give you access to gcc. You can
           also download a newer version from Apple, but you'll have to fill out a (free) registration to do it.

- gfortran -- It's sometimes hard to figure out which version of gfortran to install on your Mac. See
              this `page <http://gcc.gnu.org/wiki/GFortranBinaries#MacOS>`_ for a pretty good overview 
              of what's available.
              
- On **Snow Leopard:**

 - Python 2.6 -- Install a new version from ``python.org`` because the built-in version has a distutils bug that
   will cause some of the OpenMDAO tests to fail. It's not clear exactly what release the fix first
   appeared in, but version `Python 2.6.5 <http://python.org/ftp/python/2.6.5/python-2.6.5-macosx10.3-2010-03-24.dmg>`_ definitely has the
   fix.
 - `numpy <http://sourceforge.net/projects/numpy/files/>`_ 
 - `SciPy <http://sourceforge.net/projects/scipy/files/>`_
 - `gfortran <http://r.research.att.com/gfortran-42-5646.pkg>`_ -- This version goes with Xcode. 
 - `Matplotlib <http://sourceforge.net/projects/matplotlib/files/matplotlib/matplotlib-1.0/>`_


- On **Leopard:**

 - `Python 2.6`__ 
 - `numpy <http://sourceforge.net/projects/numpy/files/>`_                  
 - `SciPy <http://sourceforge.net/projects/scipy/files/>`_
 - `gfortran`__ 
 - `Matplotlib <http://sourceforge.net/projects/matplotlib/files/matplotlib/matplotlib-1.0/>`_

.. __: http://python.org/ftp/python/2.6.5/python-2.6.5-macosx10.3-2010-03-24.dmg

.. __: http://openmdao.org/downloads/misc/gfortran-macosx-leopard-x86.dmg


.. _Installation:

Installation
============

To avoid version conflicts with system level Python packages, OpenMDAO is best installed using a
bootstrap script that installs OpenMDAO in a *virtual* Python environment. This script is called
``go-openmdao.py``. 

**Acquire and run the script**

1. Download the installation script: `go-openmdao.py <http://openmdao.org/downloads/latest/go-openmdao.py>`_
   
   If you are using Internet Explorer, right click on the above link; then select "Save Target as." Be sure to
   save the `go-openmdao.py  <http://openmdao.org/downloads/latest/go-openmdao.py>`_ script to the folder
   where you want to install OpenMDAO. Other browsers may automatically download the file to a specific
   folder (e.g., a Downloads folder), and you will have to move it to the location where you want
   to install OpenMDAO. We highly recommend you pick a folder without any spaces in your path name! For
   instance, on a Windows machine, you could put all your OpenMDAO installations in ``C:\\OpenMDAO``.

   If you are on a Windows machine, even though you have Python installed, the Path environment
   variable will not be configured to point to the Python installation directory. You will have to put
   ``Python26`` in the path. For help doing this, please see the following `video
   <http://showmedo.com/videotutorials/video?name=960000&fromSeriesID=96>`_. Please note that this
   video is for Python 2.5, and you want to use **Python 2.6**.  

2. Run the script. 

   If you specify a directory name, the virtual environment will be installed there. If you don't supply a directory name, it
   will default to a name based on the version, for example, ``openmdao-0.1``. 

   ::

      python go-openmdao.py


   This script will check the version of Python you are running. You must be running a version
   greater than or equal to 2.6 but less than 3.0. After the script completes successfully, you
   will have installed OpenMDAO. There are just a couple more steps to follow before you can start
   using it. 

**Activate your virtual environment**

Each time before you want to use openmdao, you need to activate its virtual environment. 
Activation adds your virtual environment's ``bin`` directory to the front of your system path in the current 
shell so that when you type ``python``, you'll get a Python interpreter that's customized for your virtual 
environment, giving you access to everything in OpenMDAO.

Navigate into the folder that was just created by your install script.

If you are on **Linux or Mac OS X,** you must be running Bash. If you are in Bash, omit this step; otherwise, type: 

 :: 

    bash

 Next, type the following, making sure to include the "." in the command:

 ::

    . bin/activate


If you are on **Windows,** type:

 ::

    Scripts\activate



**Run tests to verify valid install**

OpenMDAO has a large test suite which allows you to check and make sure all of the functionality of OpenMDAO will work 
on your system. You can run these tests yourself to double check your installation. It takes only a couple of 
minutes to run them all. 

To run all tests, type the following:

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

