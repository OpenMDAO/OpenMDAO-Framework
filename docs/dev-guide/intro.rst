Getting Started
---------------

This document attempts to explain the OpenMDAO development process and how a
developer should interact with the various tools the project uses for
version control, testing, deployment, etc. The source files for the
*Developer Guide* can be found in the ``docs/dev-guide`` directory in the top
level of your OpenMDAO source repository.

.. index:: Git


.. _`developer-requirements`:

System Requirements
===================

Working with OpenMDAO as a developer has some system requirements in addition to those mentioned in
the :ref:`System-Requirements` section under *Getting Started.*  These requirements are described
below.


**Git**
  We use Git for version control.  You'll need it to access the OpenMDAO
  source repository.  GitHub, where our source repository is stored, has
  excellent documentation describing how to install Git and how to get
  familiar with Git and GitHub.  You can find it `here`__.
    
.. __: http://help.github.com

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

    - *mingw32*   (for Fortran and C++)
      
     
      You can find mingw32 `here`__. You must do the following things when installing it:
            
      - Check the C++ compiler installation option to get g++ (required to run OpenMDAO)
      
      - Create a file in your home directory called ``pydistutils.cfg`` that contains the following lines:
      
        ::
      
          [build_ext]
          compiler=mingw32
       
      - Make sure to put the ``bin`` directory of the mingw32 install in your path.
           

         
    - *Visual C++ 2008 (Optional)*
      
      You can optionally use Visual C++ 2008 as your C++ compiler. You don't need it, mingw32 will work fine,
      but if you prefer Visual C++ 2008, you're welcome to use it instead. The Express version will work, 
      but others (Professional, Standard) should work too. To get this software,
      go to the `downloads page <http://www.microsoft.com/visualstudio/en-us/products/2010-editions/express/#2008-Visual-CPP>`_.  
         
         
.. __: http://sourceforge.net/projects/mingw/files


.. index:: proxy settings

System Configuration
====================

Some steps of the development process, e.g., downloading a branch of the source repository and
downloading Python distributions, require network access.  If you're using Linux or Mac OS X and
are behind an http proxy, you may have to set the ``http_proxy`` environment variable on
your system for Git and :term:`virtualenv` to function properly. If you're using Windows 7,
please follow this 
`link <http://answers.oreilly.com/topic/675-how-to-configure-proxy-settings-in-windows-7/>`_
for information on configuring proxy settings.


*Using Git and GitHub*
++++++++++++++++++++++

The source repository for the OpenMDAO project is available on
:term:`GitHub`.  There is a wealth of good documentation available online 
about :term:`Git` and Github itself. The 
`GitHub help page <http://help.github.com/>`_ is a good place to start. If you're a 
Windows user, make sure to read the details about using Git Bash on the `Windows 
specific installation instructions <http://help.github.com/win-set-up-git/>`_. 
The `Pro Git book <http://progit.org/book/>`_ is also excellent.  It's very
important to take a look at these, because Git differs from other version
control systems in some significant ways. 

The first major difference is that Git has a *staging area* that files must be
placed in before they're committed.  Luckily the ``git commit`` command has 
an option, ``-a``, that will eliminate this odd behavior and commit all of the
modified files in the repository without having to stage them first. See the 
:ref:`Committing-changes` section for further explanation of ``git commit``.

The other major difference is how branches are handled.  In Git, creating a branch
does not create a separate copy of the repository, but instead is basically a pointer
to a commit history within the repository. This makes Git branches cheap to create. This
means that you should not hesitate to make a new branch when working on something. This
will be discussed a little more below in the :ref:`getting-the-source-code` section. 


The following figure gives an overall view of the version control process while 
using Git and GitHub.

.. figure:: version_control.png
   :align: center

   Version Control Process

From the OpenMDAO repository on GitHub, you create a personal "fork" of the 
project, so there is now a repository that you can write to.  In addition to
creating a fork, you must also create a clone of the OpenMDAO repository on 
your machine.  This is the repository that you will be working directly off 
of, via the creation of branches.  Once you are satisfied with a branch's 
development, you can push this branch back up to your personal fork.  From here,
if you would like to contribute this work back to the OpenMDAO repository, you 
must issue a pull request to have your modifications merged in.


*Git User Setup*
++++++++++++++++

If you have not previously used Git on a particular machine where you intend to work with
Git repositories, you should follow the instructions `here`__ to set your username, email,
and API token. This way, your contact information will be included whenever you
:term:`commit` to a :term:`repository` on that machine.

.. __: http://help.github.com/set-your-user-name-email-and-github-token


.. index:: repository

If you're using Git on Windows, to follow the above instructions you'll first need to
create a bash terminal by running ``Git -> Git Bash`` from the Windows start menu.

.. accessing GitHub::

Accessing OpenMDAO on GitHub -- SSH Keys
++++++++++++++++++++++++++++++++++++++++

The source repository for OpenMDAO is read-accessible to the public, so making
a clone of it does not require that you have a :term:`GitHub`
account. If you plan to contribute to the OpenMDAO project on GitHub, you will
need to have a GitHub account and to register your public SSH key with that
account. The following section describes how to register your SSH key with
GitHub.

These instructions assume that you already have a GitHub account. If you do
not, please go to https://github.com and register for an account. Go ahead
and log in to your GitHub account, since you will need to be logged in to
register your key.

If you're using Git on Windows, to follow the instructions below
you'll first need to create a bash terminal by running ``Git --> Git Bash`` from
the Windows start menu.


*Creating Your Key*
~~~~~~~~~~~~~~~~~~~

1. First, check to see if you already have an SSH key.  Look for a file called ``~/.ssh/id_rsa.pub``.
   If the file is there, skip to the next section and learn how to register your key with GitHub.
2. You should be in your home directory on your Linux machine. At the prompt, type: 
   ``ssh-keygen -t rsa -C "your_email@youremail.com"``. 
3. When prompted, press *Enter* to accept the default file name for your key. 
4. Press *Enter* when prompted for a password and then press it again to
   confirm that you are NOT entering a password. Your key pair is stored in ``~/.ssh/
   as id_rsa.pub`` (public key) and ``id_rsa`` (private key).

.. note::  In the unusual event that the ``ssh-keygen`` command fails, you may need to install
   OpenSSH. To do this requires that you have admin privileges. On Ubuntu, you can install
   OpenSSH by opening your terminal and typing: ``sudo apt-get install openssh-client``. 


*Registering the Key with GitHub*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You need to register and upload the *public* portion of your SSH key to GitHub. 

1. Open your public key in a text editor and copy its contents to your clipboard. The public key
   file has the extension ``.pub``; for example:  ``id_rsa.pub`` 
2. You must be logged into GitHub for this step. Go to *Account Settings --> SSH Public Keys*, 
   then click on *Add another public key*.
3. Enter a name into the *Title* text field to remind you which machine/account you're 
   entering ssh information for.
4. Paste your public key into the *Key* text box and then click the *Add Key* button (below the
   text box) to continue. 


.. index:: pair: source code; location

.. _getting-the-source-code:


Getting the Source Code
+++++++++++++++++++++++

The *official* OpenMDAO-Framework repository lives on GitHub at
https://github.com/OpenMDAO/OpenMDAO-Framework. 

To create a local
OpenMDAO-Framework repository, you need to *clone* the OpenMDAO-Framework
repository on GitHub using the following command:

::

   git clone git://github.com/OpenMDAO/OpenMDAO-Framework.git
   
   
or, if the port that git:// uses is blocked by your firewall, try this:

::

   git clone http://github.com/OpenMDAO/OpenMDAO-Framework.git


.. _Making-a-Personal-Fork-of-OpenMDAO-Framework:


Making a Personal Fork of OpenMDAO-Framework
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you intend to make contributions to the project, you'll need to make your
own personal fork of OpenMDAO-Framework on GitHub. Making your own fork is
easy; just log into GitHub, go to the OpenMDAO-Framework repository page at
https://github.com/OpenMDAO/OpenMDAO-Framework, and click the *Fork* button
near the top of the page.

Later, when you finish working on a branch in your local repository, you'll be
able to push it up to your personal fork and issue a pull request to get your
changes into the *dev* branch of the official repository.


.. index:: source repository


*Layout of a Source Repository*
+++++++++++++++++++++++++++++++

The directory structure of your repository should look like this:

``contrib`` 
    The directory containing source to be packaged into distributions that can
    be released separately from OpenMDAO. These distributions may or may not depend upon
    OpenMDAO. Distributions that have not yet been approved to be part of
    ``openmdao.lib`` can live here -- as long as their license is compatible with Apache V2.0. No
    proprietary code or GPL code should be placed in the OpenMDAO-Framework repository.

``devenv``
    The directory containing the OpenMDAO virtual environment. Note that
    this is not part of the source repository. You will build it by running
    the ``go-openmdao-dev.py`` script that sits at the top of the source
    repository.  See :ref:`Creating-the-Virtual-Environment`.
    
``docs``  
    The directory containing all user documentation for OpenMDAO. The
    documentation is broken up into several major documents, each found in a separate 
    subdirectory, e.g., ``plugin-guide`` contains the *Plugin Developer Guide,* ``dev-guide`` contains
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
