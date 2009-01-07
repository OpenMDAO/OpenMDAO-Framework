
--------------------------------
The OpenMDAO Development Process
--------------------------------

.. contents:: Contents

.. sectnum::



Introduction
------------

This is the beginning of the documentation for OpenMDAO developers that
attempts to explain the process of how OpenMDAO development works, and
how to interact with the various tools we use for configuration management,
testing, deployment, etc.  Right now, this is only a very basic outline
that needs to be greatly improved before it is ready for production use,
so if you're reading this now, consider yourself an alpha tester of the
process.  When you come across something that doesn't work or is confusing,
or you have thought of a way to improve some aspect of the process, please
write it down and email it to Bret.A.Naylor@nasa.gov or edit the file
yourself. The source file for this tutorial can be found in
``docs/devprocess.rst`` in the top level directory of your OpenMDAO source 
branch. If you don't know what a branch is, that will be explained below.


Getting Started
---------------

Before you can start working on source code or running tests, you need to get
your own copy of the source code and create an environment that contains all  of
the python modules you need.  To accomplish these things, we'll be using two
tools: `bazaar <http://bazaar-vcs.org>`_ to configuration manage our source
code, and  `zc.buildout <http://pypi.python.org/pypi/zc.buildout>`_ to keep track 
of all  of the python
packages that our code depends on.  A python script called ``mkbranch.py`` 
has been written to help cut down on the number of manual steps required before
you can start working on your branch.  For a gentle video introduction to
zc.buildout, check out http://rhodesmill.org/brandon/buildout. Note that on
*torpedo* the sound doesn't work, so it's better to view the buildout video
intro from another machine. 


Where's The Code?
==================

The OpenMDAO project source files are located under ``/OpenMDAO/dev`` on
*torpedo*.  This directory is what is called a shared repository, meaning that
any branches created under it share the same version tree.  Under
``/OpenMDAO/dev`` is a directory called ``trunk``.  This is the *official*
version of the OpenMDAO source. Developers cannot write directly to this
version.  Writing to the trunk can only be done by the configuration manager. 
To make changes to the code, a developer must first create a branch, then make 
and test changes, then make the branch available to the configuration manager 
who will then merge the changes back into the trunk.



Creating Your Branch
====================

A python script, called ``mkbranch.py`` located in the ``util`` directory of
the trunk will help create and configure your development branch for you.  It will
create your branch and create a buildout for you on the branch. Internally, the
script is simply talking to bazaar_ and zc.buildout_. You could perform these
tasks manually, but you should use the script in order to keep your branch
consistent with others in OpenMDAO.  This will make it easier for the
configuration manager to locate and merge your branch, and it will also make it
easier for other developers on the team to help you if you run into a problem.

The following command will create a branch as well as create and bootstrap the
buildout:

::

  <python> /OpenMDAO/dev/trunk/util/mkbranch.py -t <ticket number> [-d <description>][-s <source repository>][-b <config file>][-u <user name>]

where the following parameters are user specified:

``<python>`` 
   The specific version of python you want to use for the
   branch, for example, ``python2.6``.  Whatever version of python you use for
   this command will be *hard-wired* into all of the buildout-generated scripts.

``<ticket number>``
   The ticket number used by the bug tracking system
   
``<description>``
   *(optional)* A short description  of the purpose of the branch. The description
   should be less than 15 characters in length. 
   
``<source repository>``
   *(optional)* The top directory of the repository you want to branch from. If
   not supplied, this defaults to the top directory of the trunk.
   
``<config file>``
   *(optional)* The pathname of a buildout configuration file that will be used
   to run the buildout for the new branch.  This file will be copied into
   ``buildout/buildout.cfg`` in the top level of the new branch.  If not 
   supplied, the buildout.cfg file from the trunk will be used. If that isn't
   what you want, you can easily modify the buildout.cfg file after creating
   the branch and run the buildout again.
   
``<user name>``
   *(optional)* This should be your username on *torpedo*.  This is set 
   automatically for you based on the LOGNAME environment variable, so 
   generally you should not have to set this one.
   

As an example, if I wanted to create a branch off of the trunk to fix a bug in the
unit conversion code based on ticket 321 in the bug tracker and wanted to use
version 2.6 of python, I could issue the following command:

::

   python2.6 /OpenMDAO/dev/trunk/util/mkbranch.py -t 321 -d units_fix 


After the script runs, it places you in the 
``/OpenMDAO/dev/developers/<username>`` directory, where ``<username>`` is your
user name on *torpedo*.  For example, since my user name is *bnaylor*, my branch
from the command above would be created in 
``/OpenMDAO/dev/developers/bnaylor/T321-units_fix``. Branches are named using the
following form:

::

  T<ticket number>-<desc>


where ``<desc>`` is the short description supplied using the ``-d`` argument. 

At this point, your buildout should be configured, and your top level ``buildout``
directory should contain the following subdirectories:

``bin``
    Contains a buildout script, a buildout specific
    python interpreter, and other scripts that depend upon which parts you've
    included as part of your buildout.
    
``develop-eggs``
    Contains links to any directories that you've
    specified in the *develop* list in your ``buildout.cfg`` file.
    
``eggs``
    Contains all of the installed eggs you've listed as dependencies in your
    ``buildout.cfg`` file.
    
``parts``
    Contains any files specific to any parts you've installed as part of your
    buildout. These could be anything. They don't have to be python related.


Working on Your Branch
======================

As you make changes to the source code, you may want to modify your buildout
in some way, possibly adding new eggs, updating to new versions, etc. Whenever
this happens, you must re-run the ``buildout`` script that lives in the top
level ``bin`` directory of your buildout.


Editing/Debugging Source Code
=============================

Wing is a very nice integrated editor and debugger for python that is available to
local OpenMDAO developers.  To run it, type ``wing3.1``.

- TODO: create a buildout recipe to customize a wing project file specific to a buildout


Adding New Source Files
=======================

If you create new files or directories that you want to be part of OpenMDAO, you
must add them to your repository by running the command

::

   bzr add <filename>
        
If ``<filename>`` is a directory, all files within the directory will also be
added to the repository, unless they match any of the patterns in the
``.bzrignore``
file located in the top level directory of the branch.  To add a new pattern
for bazaar to ignore, type

::

   bzr ignore <pattern>
   
where ``<pattern>`` can be a filename or a wildcard expression, e.g., ``*.exe``.


If you add a file or directory to the repository by mistake, type

::

   bzr remove <filename> --keep
   
which will remove the file from the repository but will **not** delete it.


            
Testing
=======

By default, your top level ``buildout/bin`` directory will contain a script called
``test`` that script uses a python package called ``nose`` to run all of the unit
tests for any package that you specify. For example, to run all of the openmdao
unit tests, do the following:

::

   bin/test openmdao
   
which should generate output something like this:

::

   ..
   ----------------------------------------------------------------------
   Ran 82 tests in 0.888s

   OK

To get a list of options available with ``bin/test``, type ``bin/test --help``
from the ``buildout`` directory.
   
Test Coverage
+++++++++++++

There is a python package called ``coverage`` that is accessible through ``nose``
that makes it easy to determine if your tests cover every line of code in your
source files.

- TODO: talk about coverage


Adding New Tests
++++++++++++++++

- TODO: explain how to develop a unit test


The Distribution Cache
======================

Sometimes you will be creating new python packages or new versions of existing
ones. When this happens you need to build a python egg of your package, making
sure to update the version number.  Once you've built the egg, you can upload it
to the distribution cache by running the ???? script on it.  Also, if you
introduce a dependency on some third party python package, you will need to 
grab a distribution of it and add it to the distribution cache in the same way.

If you package does not contain any python extensions, i.e., compiled code, you
only need to create a source distribution, but if it does contain compiled code,
you will have to create a binary distribution for Windows, linux, and OSX.

For instructions on how to build eggs, see the setuptools `documentation
<http://peak.telecommunity.com/DevCenter/setuptools>`_.

    

Creating New zc.buildout Recipes
================================

    - build an egg
    - zc.buildout entry points for each recipe
    - simple API
        - __init__(self, options, name, buildout)
        - install(self)  # returns list of files/dirs for later uninstall
        - update(self)
        - uninstall(self) # usually not necessary




