Introduction to OpenMDAO Development Process
--------------------------------------------

This documentation for OpenMDAO developers attempts to explain the OpenMDAO
development process and how to interact with the various tools the project uses for
configuration management, testing, deployment, etc.  The source files for this tutorial
can be found in the ``docs/dev-guide`` directory in the top level directory of your
OpenMDAO source :term:`branch`. 

.. index:: Bazaar


System Configuration
====================

Some steps of the development process, e.g., downloading a branch of the source
repository and downloading Python eggs, require network access.  If you're
behind an http proxy, you may have to set the *http_proxy* environment variable
on your system in order for Bazaar and ``virtualenv`` to function properly.


Getting Started
===============

Before you can start working on source code or running tests, you need to get
your own copy of the source code. We use `Bazaar, <http://bazaar-vcs.org>`_ 
for version control, so you'll need to have that installed on your system.
To install all of the packages you'll need to run OpenMDAO, just run the
``go-openmdao.py`` script found at the top of the source repository.  Under the
covers, it uses virtualenv to build the OpenMDAO virtual environment.


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
.. index:: branch


*Code Location*
+++++++++++++++
    

The Bazaar repository for the OpenMDAO source code is available on Launchpad.
You can get a copy of the repository as follows:

::

    bzr branch lp:openmdao <your_branch_name>

To submit patches you must upload your branch to the openmdao project on Launchpad
and request that it be merged. Please see :ref:`Pushing-a-Branch-Back-to-Launchpad`


.. index:: source repository


*Layout of a Source Repository*
+++++++++++++++++++++++++++++++

Within an OpenMDAO branch repository,  the directory structure will look like
this:

``devenv``
    The directory containing the the OpenMDAO virtual environment. Note that
    this is not part of the source repository. You will build it by running
    the ``go-openmdao-dev.py`` script that sits at the top of the source
    repository.
    
``docs`` All Sphinx user documentation for OpenMDAO.  The documentation is broken up into
    several major documents, each found in a separate  subdirectory, e.g., ``user-guide``
    contains the User Guide, ``dev-guide`` contains the Developer's Guide, and so on.   

``openmdao.main``
    Python package containing all infrastructure source for OpenMDAO.
    
``openmdao.lib``
    Python package containing source for the OpenMDAO standard library of 
    modules.
    
``openmdao.util``
    Python package containing source for various Python utility routines
    used by OpenMDAO developers.
    
``openmdao.test``
    Python package containing source for various OpenMDAO plugins used for
    testing.
    
``openmdao.examples``
    Python package containing examples of using OpenMDAO.
    
``misc``
    Miscellaneous scripts and configuration files used by OpenMDAO developers.
 
.. index:: egg
    
``contrib``
    Contains source to be packaged into Python :term:`eggs` that can be released
    separately from OpenMDAO. These eggs may or may not depend upon OpenMDAO. 
    Eggs that have not yet been approved to be part of ``openmdao.lib`` can live
    here, as long as their license is compatible with NOSA. No proprietary code
    or GPL code can live in the OpenMDAO repository.


.. index:: namespace package

*Layout of a Namespace Package*
+++++++++++++++++++++++++++++++

OpenMDAO is large enough that it makes sense to split it up into multiple Python
packages, but we want all of those packages to be under the umbrella of
``openmdao``. To do this in Python, we use what is called a *namespace*
package.  Namespace  packages all have a similar directory layout.  Currently in
OpenMDAO,  ``openmdao.main``, ``openmdao.lib``, ``openmdao.devtools``,
``openmdao.util`` and ``openmdao.test`` are all namespace
packages that are in the ``openmdao`` namespace.  They all  have a layout like
this:

``openmdao.<package>``
    The top level directory for the package denoted by ``<package>``. This
    contains the ``setup.py`` script which is used to build and 
    create an egg for the package.
    
``openmdao.<package>/src``
    Contains all of the package source code.
    
``openmdao.<package>/src/openmdao``
    Contains a special ``__init__.py`` file and a ``<package>``
    subdirectory.
    
``openmdao.<package>/src/openmdao/<package>``
    This is where the actual source code, usually a bunch of Python files,
    is located.  There could be a standard Python package directory structure
    under this directory as well.
    

.. index:: pair: branch; creating 


.. _Installing-from-Source:


*Installing from Source*
+++++++++++++++++++++++++

The first step in installing from source is to create a branch.

.. _Creating-a-Branch:

Creating a Branch
_________________

To create a branch, use the command:

::

   bzr branch lp:openmdao <branch_name>
   
   
where ``<branch_name>`` is the name you are giving to the top level directory
of your branch repository.  The name should reflect the purpose of the branch to
avoid confusion in the case where you have multiple branches active at the same time.
If you do not supply ``<branch_name>``, the name by default will be the last part of
the source repository URI, which in this case is ``openmdao``.

Branches are typically named based on ticket numbers in the bug  tracker, and we
use the form ``T<ticket_number>-<desc>`` where ``ticket_number`` is the bug
tracker ticket number and ``<desc>`` is a short description of the branch. For
example, ``T0029-workflow_fix``.
   

Creating and Activating the Virtual Dev Environment
___________________________________________________


After you've created your branch, you must run ``python go-openmdao-dev.py`` from the top
directory of your branch before you'll be able to execute OpenMDAO in any way. 

::

   python2.6 go-openmdao-dev.py
   
Running ``go-openmdao-dev.py`` populates your virtual Python environment with all of the packages that
OpenMDAO depends upon and installs the openmdao namespace packages in your virtual Python
environment as "develop" eggs so that you can make changes to the source code and immediately
see the results. 

The next step is to activate your virtual environment. This requires that you are running the
bash shell if you are on a Linux or OS X machine.  

Change your directory to ``devenv`` and run:

::

   source bin/activate

or, on Windows

::

   Scripts\activate.bat

At this point, your ``devenv`` directory should contain the following
subdirectories:

``bin``
    Contains python and a number of other scripts that are associated with
    the Python packages that are installed in the virtual environment. On
    Windows, this directory is called *Scripts*

``lib``
    Contains Python standard library and installed modules.
    
``include``
    Contains Python C header files.
    
``etc``
    Contains miscellaneous files that don't fit in bin, lib, or include.


You can add new packages you need to the environment by using *easy_install* or *pip* in the
same manner that you would add packages to the system level Python.  

If you make doc changes and need to rebuild the docs, you can run ``openmdao_build_docs``.
Running ``openmdao_docs`` will display the documents in HTML in the default browser.
