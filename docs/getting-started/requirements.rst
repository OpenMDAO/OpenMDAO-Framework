
.. _System-Requirements:


System Requirements
===================

These instructions assume that you intend to install OpenMDAO for normal use -- that you will not be
hacking on the OpenMDAO infrastructure or standard library, or developing any plugins that require
compilation. If you *do* intend to perform some kind of OpenMDAO development, you should refer to
the installation instructions in the *Developer Guide* because the developer environment has some
additional :ref:`system requirements <developer-requirements>` on certain platforms.

OpenMDAO requires that the following software be installed at the system level on all platforms:

- Python (2.6.x or 2.7.x)

  - In general, OpenMDAO supports the specified versions of Python found at python.org.  Alternate Python builds, such as Enthought,Active State python or python(x,y) are not officially supported at this time, though some may work with OpenMDAO.  
 
  - OpenMDAO does not support Python 3.x, nor does it support Python versions below 2.6.5.  Having the incorrect Python installation in the PATH and/or not having the OpenMDAO-approved Python version in the PATH will only lead to problems.

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

- `Python 2.6.x or 2.7.x <http://www.python.org/download/releases//>`_

- `NumPy <http://sourceforge.net/projects/numpy/files/>`_ 

- `SciPy <http://sourceforge.net/projects/scipy/files/>`_

- `Matplotlib <http://sourceforge.net/projects/matplotlib/files/matplotlib/matplotlib-1.0/>`_

Not required, but highly recommended:

- `pywin32 <http://sourceforge.net/projects/pywin32/files/>`_

This will improve distributed simulation startup time significantly by allowing
public/private key pairs to be stored securely.

**Mac OS X**:

Our current OpenMDAO distribution for OS X is a source distribution, so to 
use it, you must have compilers (C and Fortran) on your system.

- Xcode -- It's included on the OS X install disk, and installing it will give you access to gcc. You can
           also download a newer version from Apple, but you'll have to fill out a (free) registration to do it.

- gfortran -- It's sometimes hard to figure out which version of gfortran to install on your Mac. See
              this `page <http://gcc.gnu.org/wiki/GFortranBinaries#MacOS>`_ for a pretty good overview 
              of what's available.
              
- On **Snow Leopard:**

 - Python -- Install a new version (2.6.x or 2.7.x) from ``python.org`` because the built-in version has a distutils bug that
   will cause some of the OpenMDAO tests to fail. It's not clear exactly what release the fix first
   appeared in, but version `Python 2.6.5 <http://python.org/ftp/python/2.6.5/python-2.6.5-macosx10.3-2010-03-24.dmg>`_ definitely has the
   fix.
 - `NumPy <http://sourceforge.net/projects/numpy/files/>`_ 
 - `SciPy <http://sourceforge.net/projects/scipy/files/>`_
 - `gfortran <http://r.research.att.com/gfortran-42-5646.pkg>`_ -- This version goes with Xcode. 
 - `Matplotlib <http://sourceforge.net/projects/matplotlib/files/matplotlib/matplotlib-1.0/>`_


- On **Leopard:**

 - `Python (2.6.x or 2.7.x)`__ 
 - `NumPy <http://sourceforge.net/projects/numpy/files/>`_
 - `SciPy <http://sourceforge.net/projects/scipy/files/>`_
 - `gfortran`__  - Click on ``fortran-macosx-leopard-x86.dmg`` under
   **Miscellaneous Downloads.**
 - `Matplotlib <http://sourceforge.net/projects/matplotlib/files/matplotlib/matplotlib-1.0/>`_
 
 If you have g77 installed on Leopard, you may get build errors like: 
 ``ld: library not found for -lcc_dynamic``. This indicates that g77, which won't
 work, is being used instead of `gfortran`. At the moment, the recommended fix
 is to change the name of g77 to something else, for example, ``_g77`` so that
 it won't be found by ``numpy.distutils``.

.. __: http://python.org/ftp/python/2.6.5/python-2.6.5-macosx10.3-2010-03-24.dmg

.. __: http://openmdao.org/downloads

- On **Lion (OS X 10.7):**

 - Python 2.7.x (OS X Lion has 2.7.1 as its native version) - Install this `version of NumPy <http://sourceforge.net/projects/numpy/files/NumPy/1.6.1/numpy-1.6.1-py2.7-python.org-macosx10.6.dmg/download>`_
 
 - Install this `version of SciPy <http://sourceforge.net/projects/scipy/files/scipy/0.10.1/scipy-0.10.1-py2.7-python.org-macosx10.6.dmg/download>`_
 
 - Make sure that Xcode version 4.3.2 or higher is installed, via the Mac App Store. When you install Xcode, it does not by default install the compilers you
   need.  So go to Xcode's  **Preferences** menu, choose **Downloads**, and then choose **Command Line Tools.**

   .. figure:: OSX_Lion_Screenshot.png
      :align: center
      :alt: Screenshot of XCode's Downloads screen showing options
   
      XCode's *Downloads* Screen
   
 - `gfortran 4.6.2 <http://quatramaran.ens.fr/~coudert/gfortran/gfortran-4.6.2-x86_64-Lion.dmg>`_
 
 - `gcc 4.2 <http://web.mit.edu/mfloyd/www/computing/mac/gfortran/>`_





