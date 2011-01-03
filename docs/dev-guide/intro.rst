Getting Started
---------------

This document attempts to explain the OpenMDAO development process and how a
developer should interact with the various tools the project uses for
version control, testing, deployment, etc. The source files for the
*Developer Guide* can be found in the ``docs/dev-guide`` directory in the top
level of your OpenMDAO source repository.

.. index:: Bazaar


.. _`developer-requirements`:

System Requirements
===================

Working with OpenMDAO as a developer has some system requirements in
addition to those mentioned in the :ref:`System-Requirements` section of the *User
Guide.*  These requirements are described below.


**Bazaar**
  We use Bazaar for version control.  You'll need it to access the OpenMDAO
  source repository.  Installers for various platforms can be found `here`__.
    
.. __: http://wiki.bazaar.canonical.com/Download

**C/C++ and Fortran Compilers**
  Certain packages used in OpenMDAO contain Python extensions, meaning that they
  contain non-Python source code that must be compiled. Packages currently in use require
  either C/C++ or Fortran compilers.

  - *Linux*:

    - *gcc*
    
    - *gfortran*
      
      If they are not already on your system, they should be easily installable using
      your package manager. OpenMDAO currently builds and passes all tests with
      gcc/gfortran 4.1.2. We expect that later versions of gcc/gfortran 4.X should also
      work.

      
  - *Mac OS X*:
   
    - *gcc*
      
      Is available as part of *Xcode*, which can be found on the OS X distribution disks but typically is not 
      installed by default.  You can also download gcc and install it from source, although
      this is more prone to installation problems.
        
    - *gfortran*
      
      Binaries for gfortran are available `here <http://gcc.gnu.org/wiki/GFortranBinaries#MacOS>`_.

.. _`Windows`:

  - *Windows*:
   
    - *Visual C++ 2008*
      
      We use the Express version, but others (Professional, Standard) should work too. To get this software,
      go to the `downloads page <http://www.microsoft.com/express/downloads/#2008-Visual-CPP>`_.

    - *mingw32*   (for Fortran and optionally as a Visual C++ replacement)
      
      Make sure to put the ``bin`` directory of the mingw32 install in your path.
      You can find mingw32 `here`__.
      
      If you intend to use mingw32 as a Visual C++ replacement, you must do two things when installing it:
            
      - Check the C++ compiler installation option to get g++ (required to run OpenMDAO)
      
      - Create a file in your home directory called ``pydistutils.cfg`` that contains the following lines:
      
        ::
      
          [build_ext]
          compiler=mingw32

         
         
.. __: http://sourceforge.net/projects/mingw/files


.. index:: proxy settings

System Configuration
====================

Some steps of the development process, e.g., downloading a branch of the source repository and
downloading Python distributions, require network access.  If you're using Linux or Mac OS X and
are behind an http proxy, you may have to set the ``http_proxy`` environment variable on
your system for Bazaar and :term:`virtualenv` to function properly. If you're using Windows 7,
please follow this `link <http://answers.oreilly.com/topic/675-how-to-configure-proxy-settings-in-windows-7/>`_
for information on configuring proxy settings.

*Bazaar User Setup*
+++++++++++++++++++

If you have not previously used Bazaar on a particular machine where you intend
to work with Bazaar repositories, you should run the ``whoami``
command so that Bazaar will know your email address. You need to supply your
first and last name and your email address in the following way:

::

    bzr whoami "Joe Cool <joe@example.com>"


.. index:: repository

This way, your contact information will be included whenever you :term:`commit`
to a :term:`repository` on that machine.

.. index:: pair: source code; location
.. index:: pair: branch; creating

.. _Creating-a-Branch:


Getting the Source Code
+++++++++++++++++++++++

Before you can do any development work on OpenMDAO, you'll need a copy of the source code.
The source repository for the OpenMDAO  project is available on :term:`Launchpad`. You can
create a copy of the repository by typing:

::

   bzr branch lp:openmdao <branch_name>
   
   
where ``<branch_name>`` is the name you are assigning the top level directory of your branch
repository. It's a good idea to name branches based on ticket numbers in our bug  tracker
(:term:`Trac`) using the form ``T<ticket_number>-<desc>``, where ``ticket_number`` is the
Trac ticket number and ``<desc>`` is a short description of the branch, for example,
``T0029-workflow_fix``.  Trac automatically assigns a ticket number when you submit a bug or
request an enhancement. You can visit the `OpenMDAO website
<http://openmdao.org/wiki/Home>`_  to find out more about how we use Trac and about the
OpenMDAO `development <http://openmdao.org/wiki/Development>`_ process.


In any case, the name you give your branch should reflect the purpose of the branch to avoid
confusion if you have multiple branches active at the same time. If you do not supply a
``<branch_name>``, the name by default will be the last part of the source repository URI,
which in this case is ``openmdao``.



.. _Creating-the-Virtual-Environment:


Creating the Virtual Environment
________________________________


After you've created your branch, run ``python go-openmdao-dev.py`` from the top directory of your
branch to set up your development environment. (The ``devenv`` directory that is created is not part
of the source repository.) The script will check the version of Python you are running. **You must
be running version 2.6 (or 2.6.5 in some cases).** (To find out which Python version you are
running, you can type ``python --version``.)

.. note:: On Windows, you need to run the installer from a command window that has
   the Visual Studio environment variables set.  The easiest way to do this is to
   select the *Visual Studio 2008 Command Prompt* from the *Visual Studio Tools* menu
   under *Microsoft Visual C++ 2008 Express Edition* in the Start menu. If you're
   using something other than the Express edition, then the name of the Start menu 
   option will be slightly different, i.e., replace "Express" with "Professional" or
   "Standard."

::

   python go-openmdao-dev.py
   
Running ``go-openmdao-dev.py`` populates your virtual Python environment with all of the packages that
OpenMDAO depends upon and installs the openmdao namespace packages in your virtual Python
environment as "develop" eggs so that you can make changes to the source code and immediately
see the results without having to rebuild any distributions.


.. _Activating-the-Virtual-Environment:

Activating the Virtual Environment
__________________________________

The next step is to activate your virtual Python environment. Change your directory to
``devenv``. 

On Linux or Mac OS X, you must be running the Bash shell. If you are in Bash, omit this step.

  ::

     bash
   
 
  Next, type the following, making sure to include the "." in the command:

  ::

     . bin/activate



Or, on Windows, type:

  ::

     Scripts\activate

At this point, your ``devenv`` directory should contain the following subdirectories, unless you are
on Windows. On Windows, the directory structure is slightly different, as noted below.

``bin`` 
    Contains Python and a number of other scripts that are associated with the Python
    packages that are installed in the virtual environment. On **Windows,** this
    directory is called ``Scripts``.

``etc``
    Contains miscellaneous files that don't fit in ``bin, lib,`` or ``include``.
    
``include``
    Contains Python C header files. If you are on **Windows,** you will not have this directory.


``lib``
    Contains Python standard library and installed modules.

After your virtual Python environment has been activated, you can add other 
distributions to the environment by using ``easy_install`` or :term:`pip` in
the same manner that you would add packages to the system level Python.

If you make doc changes and need to rebuild the docs, you can run ``openmdao_build_docs``.
Running ``openmdao_docs`` will display the documents in HTML in the default browser.

You can deactivate the environment by typing:


:: 

  deactivate
  
 

.. index:: source repository


*Layout of a Source Repository*
+++++++++++++++++++++++++++++++

The directory structure of your repository should look like this:

``contrib`` 
    The directory containing source to be packaged into distributions that can
    be released separately from OpenMDAO. These distributions may or may not depend upon
    OpenMDAO. Distributions that have not yet been approved to be part of
    ``openmdao.lib`` can live here -- as long as their license is compatible with NOSA. No
    proprietary code or GPL code can live in the OpenMDAO repository.

``devenv``
    The directory containing the OpenMDAO virtual environment. Note that
    this is not part of the source repository. You will build it by running
    the ``go-openmdao-dev.py`` script that sits at the top of the source
    repository.
    
``docs``  
    The directory containing all user documentation for OpenMDAO. The
    documentation is broken up into several major documents, each found in a separate 
    subdirectory, e.g., ``user-guide`` contains the *User Guide,* ``dev-guide`` contains
    the *Developer Guide,* and so on.
  
``examples``
    Python package containing examples of using OpenMDAO.
    
``misc``
    The directory containing miscellaneous scripts and configuration files used by
    OpenMDAO developers.

``openmdao.devtools``
    Python package containing scripts intended for developers and maintainers
    of openmdao to do things like build the docs or create a release.
    These scripts assume that the source repository is present, so this
    package is not distributed as part of an OpenMDAO release.

``openmdao.lib``
    Python package containing source for the OpenMDAO standard library of 
    modules.

``openmdao.main``
    Python package containing all infrastructure source for OpenMDAO.
     
``openmdao.test``
    Python package containing source for various OpenMDAO plugins used for
    testing.
    
``openmdao.units``
     Python package containing tools for doing unit conversion.   

``openmdao.util``
    Python package containing source for various Python utility routines
    used by OpenMDAO developers.
    
    
.. index:: namespace package


*Layout of a Namespace Package*
+++++++++++++++++++++++++++++++

OpenMDAO is split up into multiple Python packages, all under a top level
package called ``openmdao``. This top package, called a *namespace* package,
is a sort of fake package that allows us to maintain and release our
subpackages separately while appearing to the user to be all part of the
same top level package. The following packages under the ``openmdao``
namespace have a similar directory layout: ``openmdao.main``,
``openmdao.lib``, ``openmdao.devtools``, ``openmdao.util`` and
``openmdao.test``. The layout is shown below.

``openmdao.<package>``
    The top level directory for the package denoted by ``<package>``. This
    contains the ``setup.py`` script which is used to build and 
    create a distribution for the package.
    
``openmdao.<package>/src``
    Contains all of the package source code.
    
``openmdao.<package>/src/openmdao``
    Contains a special ``__init__.py`` file and a ``<package>``
    subdirectory.
    
``openmdao.<package>/src/openmdao/<package>``
    Contains the actual source code, usually a bunch of Python files. There could be a
    standard Python package directory structure under this directory as well.

``openmdao.<package>/src/openmdao/<package>/test``
    Contains unit tests for this package. These are executed by
    ``openmdao_test``.
