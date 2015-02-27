
.. _System-Requirements:


System Requirements
===================

These instructions assume that you intend to install OpenMDAO for normal use -- that you will not be
hacking on the OpenMDAO infrastructure or standard library, or developing any plugins that require
compilation. If you *do* intend to perform some kind of OpenMDAO development, you should refer to
the installation instructions in the *Developer Guide* because the developer environment has some
additional :ref:`system requirements <developer-requirements>` on certain platforms.

OpenMDAO requires that the following software be installed at the system level on all platforms:

- Python (2.7.5 or higher)

  - In general, OpenMDAO supports the specified versions of Python found at ``python.org``.

  - Two alternative Python builds that are supported by the team are :ref:`Anaconda <Anaconda-Installation>` and Python(x,y).

  - Enthought, ActiveState Python, and other Python distributions are not supported, though some
    of these distributions may work with OpenMDAO.

  - OpenMDAO does not support Python 3.x or any Python versions below 2.7.5 Having the incorrect Python
    installation in the PATH and/or not having the OpenMDAO-approved Python version in the PATH will only
    lead to problems.

  - OpenMDAO does not yet support any 64-bit distributions of Python on Windows.  You can install a 32-bit distribution
    on Windows and have success.


- NumPy (version 1.6 or higher)

- SciPy (version .11 or higher)

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


.. note::

   For our Ubuntu Pangolin users, be aware that Numpy and SciPy versions that you
   attempt to ``sudo apt-get install`` may report back as being the latest
   available version but will still be beneath OpenMDAO's threshold. The
   system is showing you the latest version available on the servers that support
   Pangolin, but the version will not meet the minimum requirements needed
   by OpenMDAO. According to the Unbuntu Software Center: "Canonical does not
   provide updates for Scientific tools for Python. Some updates may be provided
   by the Ubuntu community."

   Fortunately, you can circumvent this problem in a few easy steps:

   1. Download the packages from SourceForge.net

      - `SciPy <http://sourceforge.net/projects/scipy/files/scipy/>`__

      - `Numpy <http://sourceforge.net/projects/numpy/files/NumPy/>`__

   2. ``sudo python setup.py install`` from each package. At this point, you're close, but Numpy
      and SciPy aren't importable by Python yet, because the Python installer restricts default
      access rights. To get the right permissions on the packages, continue to step three.

   3. Change permissions inside the distribution with these commands.

      ::

	cd /usr/local/lib/python2.7/dist-packages
	sudo find scipy -type d -exec chmod o=rx {} \;
	sudo find scipy -type f -exec chmod o=r {} \;
	sudo find numpy -type d -exec chmod o=rx {} \;
	sudo find numpy -type f -exec chmod o=r {} \;

    **Please note**: Some users have reported that this method of installation may somehow uninstall
    Matplotlib. In the event that occurs, you may have to install `Matplotlib
    <http://sourceforge.net/projects/matplotlib/files/>`__ in the same fashion as the packages above.


**Windows**:

- `Python 2.7.5+ <https://www.python.org/download/releases/>`_

- `NumPy <http://sourceforge.net/projects/numpy/files/NumPy/>`__

- `SciPy <http://sourceforge.net/projects/scipy/files/>`__

- `Matplotlib <http://matplotlib.org/downloads.html>`__

.. note::

   Numpy and SciPy have one-click installers. You should use those unless you have a very good
   reason not to. For Python 2.7.x you want a SciPy version such as
   ``scipy-0.11.0-win32-superpack-python2.7.exe``. The  version number might be different than the
   one here, but make sure you get something with ``superpack`` and ``.exe`` in the name.

   If you go to the Numpy link above, you might see: "Looking for the
   latest version? Download numpy-1.7.0.zip." You **DON'T** want to get that one.
   It's a source distribution, not the installer. The same goes for SciPy. Click through
   to the latest version and get the ``.exe`` file.

Not required, but highly recommended:

- `pywin32 <http://sourceforge.net/projects/pywin32/files/>`_

This will improve distributed simulation startup time significantly by allowing
public/private key pairs to be stored securely.

**Mac OS X**:

- Python -- Install a new version (2.7.5+) from ``python.org`` because the built-in version has a
  distutils bug that will cause some of the OpenMDAO tests to fail.
- `NumPy <http://sourceforge.net/projects/numpy/files/NumPy/>`_
- `SciPy <http://sourceforge.net/projects/scipy/files/>`_
- `Matplotlib <http://matplotlib.org/downloads.html>`_

.. note::

   We provide pre-compiled binaries for all Intel OS X Macs, for Lion or higher.  However,
   if you you're running an older version of OS X or a PowerPC architecture, then  you'll need to
   set up some compilers on your system to get OpenMDAO to install. This should  be a rare
   situation, but if you happen to run into it, go to our developer docs for the details about
   which compilers you need. Get the compilers set up the way we suggest :ref:`here
   <developer-requirements>`, but then come back and follow the release installation
   instructions from the next section of these docs.
