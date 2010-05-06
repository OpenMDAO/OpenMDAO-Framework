Introduction to OpenMDAO Development Process
--------------------------------------------

This document attempts to explain the OpenMDAO development process and how a
developer should interact with the various tools the project uses for
configuration management, testing, deployment, etc. The source files for the
Developer Guide can be found in the ``docs/dev-guide`` directory in the top
level directory of your OpenMDAO source repository.

.. index:: Bazaar


System Configuration
====================

Some steps of the development process, e.g., downloading a branch of the source
repository and downloading Python distributions, require network access.  If you're
behind an http proxy, you may have to set the *http_proxy* environment variable
on your system in order for Bazaar and ``virtualenv`` to function properly.


Getting Started
===============

Before you can start working on source code or running tests, you need to get
your own copy of the source code. We use `Bazaar, <http://bazaar-vcs.org>`_ 
for version control, so you'll need to have that installed on your system.


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

Before you can do any development work on OpenMDAO, you'll need
a copy of the source code. The source repository for the OpenMDAO 
project is available on Launchpad. You can get a copy of the repository 
as follows:

::

   bzr branch lp:openmdao <branch_name>
   
   
where ``<branch_name>`` is the name you are giving to the top level directory
of your branch repository.  The name should reflect the purpose of the branch to
avoid confusion in the case where you have multiple branches active at the same time.
If you do not supply ``<branch_name>``, the name by default will be the last part of
the source repository URI, which in this case is ``openmdao``.

It's a good idea to name branches based on ticket numbers in the bug  tracker using the 
form ``T<ticket_number>-<desc>`` where ``ticket_number`` is the bug
tracker ticket number and ``<desc>`` is a short description of the branch. For
example, ``T0029-workflow_fix``.


.. _Creating-and-Activating-the Development-Environment:


Creating and Activating the Development Environment
___________________________________________________


After you've created your branch, run ``python go-openmdao-dev.py`` from the top
directory of your branch to set up your development environment. 

::

   python2.6 go-openmdao-dev.py
   
Running ``go-openmdao-dev.py`` populates your virtual Python environment with all of the packages that
OpenMDAO depends upon and installs the openmdao namespace packages in your virtual Python
environment as "develop" eggs so that you can make changes to the source code and immediately
see the results without having to rebuild any distributions.

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


After your virtual python environment has been activated, you can add additional
distributions to the environment by using *easy_install* or *pip* in
the same manner that you would add packages to the system level Python.

If you make doc changes and need to rebuild the docs, you can run ``openmdao_build_docs``.
Running ``openmdao_docs`` will display the documents in HTML in the default browser.

.. index:: source repository


*Layout of a Source Repository*
+++++++++++++++++++++++++++++++

The directory structure of your repository should look like this:

``devenv``
    The directory containing the the OpenMDAO virtual environment. Note that
    this is not part of the source repository. You will build it by running
    the ``go-openmdao-dev.py`` script that sits at the top of the source
    repository.
    
``docs`` All Sphinx user documentation for OpenMDAO.  The documentation is broken up into
    several major documents, each found in a separate  subdirectory, e.g., ``user-guide``
    contains the User's Guide, ``dev-guide`` contains the Developer's Guide, and so on.

``openmdao.main``
    Python package containing all infrastructure source for OpenMDAO.
    
``openmdao.lib``
    Python package containing source for the OpenMDAO standard library of 
    modules.
    
``openmdao.util``
    Python package containing source for various Python utility routines
    used by OpenMDAO developers.
    
``openmdao.devtools``
    Python package containing scripts intended for developers and maintainers
    of openmdao, to do things like build the sphinx docs or create a release.
    These scripts assume that the source repository is present, so this
    package is not distributed as part of an OpenMDAO release.
    
``openmdao.test``
    Python package containing source for various OpenMDAO plugins used for
    testing.
    
``openmdao.examples``
    Python package containing examples of using OpenMDAO.
    
``misc``
    Miscellaneous scripts and configuration files used by OpenMDAO developers.
     
``contrib``
    Contains source to be packaged into distributions that can be released
    separately from OpenMDAO. These distributions may or may not depend upon
    OpenMDAO. Distributions that have not yet been approved to be part of
    ``openmdao.lib`` can live here, as long as their license is compatible
    with NOSA. No proprietary code or GPL code can live in the OpenMDAO
    repository.


.. index:: namespace package

*Layout of a Namespace Package*
+++++++++++++++++++++++++++++++

OpenMDAO is split up into multiple Python packages, all under a top level
package called ``openmdao``. This top package is what is called a *namespace*
package, is sort of a fake package that allows us to maintain and release our
subpackages separately while appearing to the user to be all part of the same
top level package. The following packages under the ``openmdao`` namespace
have a similar directory layout: ``openmdao.main``, ``openmdao.lib``,
``openmdao.devtools``, ``openmdao.util`` and ``openmdao.test``. The layout is
shown below.

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
    This is where the actual source code, usually a bunch of Python files,
    is located.  There could be a standard Python package directory structure
    under this directory as well.
    
