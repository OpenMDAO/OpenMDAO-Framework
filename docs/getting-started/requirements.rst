
.. _System-Requirements:


System Requirements
===================

These instructions assume that you intend to install OpenMDAO for normal use -- that you will not be
hacking on the OpenMDAO infrastructure or standard library, or developing any plugins that require
compilation. If you *do* intend to perform some kind of OpenMDAO development, you should refer to
the installation instructions in the *Developer Guide* because the developer environment has some
additional :ref:`system requirements <developer-requirements>` on certain platforms.

OpenMDAO requires that the following software be installed at the system level on all platforms:

- Python (2.7.x)

  - In general, OpenMDAO supports the specified versions of Python found at ``python.org``.  Alternative Python
    builds, such as Enthought, ActiveState Python, or Python(x,y) are not officially supported at this time,
    though some may work with OpenMDAO.    

  - OpenMDAO does not support Python 3.x or any Python versions below 2.7. Having the incorrect Python
    installation in the PATH and/or not having the OpenMDAO-approved Python version in the PATH will only
    lead to problems.

  
- NumPy (version 1.6 or higher) 

- SciPy (version .10 or higher)

- Matplotlib

- For the GUI: Chrome Web Browser (version 20 or higher). Note: Things should work in recent versions of Firefox, but 
  we're not currently testing with that browser. Things **will not** work properly in Internet Explorer. 


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

- `Python 2.7.x <http://www.python.org/download/releases/>`_

- `NumPy <http://sourceforge.net/projects/numpy/files/NumPy/>`_ 

- `SciPy <http://sourceforge.net/projects/scipy/files/>`_

- `Matplotlib <http://sourceforge.net/projects/matplotlib/files/matplotlib/matplotlib-1.0/>`_


.. note:: 

  Numpy and SciPy have one-click installers. You should use those unless  you have a very good
  reason not to. You need to pick the proper installer for your version of Python (v2.6 or
  v2.7). For instance, if you had Python 2.6, you want a Numpy version named something like
  ``numpy-1.7.0-win32-superpack-python2.6.exe``. If you have Python 2.7, then you want  a
  SciPy version called ``scipy-0.11.0-win32-superpack-python2.7.exe``. The  version numbers
  might be different than the ones here, but make sure you get something with ``superpack`` and
  ``.exe`` in the name. 

  If you go to the Numpy link above, you might see: "Looking for the 
  latest version? Download numpy-1.7.0.zip." You **DON'T** want to get that one. 
  It's a source distribution, not the installer. The same goes for SciPy. Click through 
  to the latest version and get the ``.exe`` file. 

Not required, but highly recommended:

- `pywin32 <http://sourceforge.net/projects/pywin32/files/>`_

This will improve distributed simulation startup time significantly by allowing
public/private key pairs to be stored securely.

**Mac OS X**:

- Python -- Install a new version (2.6.x or 2.7.x) from ``python.org`` because the built-in version has a distutils bug that
  will cause some of the OpenMDAO tests to fail. It's not clear exactly what release the fix first
  appeared in, but version `Python 2.6.5 <http://python.org/ftp/python/2.6.5/python-2.6.5-macosx10.3-2010-03-24.dmg>`_ definitely has the
  fix.
- `NumPy <http://sourceforge.net/projects/numpy/files/NumPy/>`_ 
- `SciPy <http://sourceforge.net/projects/scipy/files/>`_
- `Matplotlib <http://sourceforge.net/projects/matplotlib/files/matplotlib/matplotlib-1.0/>`_

.. note:: 

   We provide pre-compiled binaries for all Intel OS X Macs, for Snow Leopard or higher.  However,
   if you you're running an older version of OS X or a PowerPC architecture, then  you'll need to
   set up some compilers on your system to get OpenMDAO to install. This should  be a rare
   situation, but if you happen to run into it, go to our developer docs for the details about
   which compilers you need. Get the compilers set up the way we suggest :ref:`here
   <developer-requirements>`, but then come back and follow the release installation
   instructions from the next section of these docs.






