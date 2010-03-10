
Notes for Developers at GRC
---------------------------

This section outlines policies that are specific to OpenMDAO development that
happens locally on computers at the NASA Glenn Research Center. If you
are not a developer at GRC, you can skip this section.



Code Location
=============


The OpenMDAO project source files are located under ``/OpenMDAO/`` on
*torpedo*.  Under ``/OpenMDAO/``
is a directory called ``trunk``.  This is the *official* version of the
OpenMDAO source. Developers cannot write directly to this version.  Writing
to the trunk can be done only by the configuration manager.  To make changes
to the code, a developer must first create a :term:`branch`, make  and test
changes, and then make the branch available to the configuration manager 
who will then merge the changes back to the trunk.

.. index:: pair: OpenMDAO; directory structure

The overall directory structure for OpenMDAO looks like this:

``/OpenMDAO``
    The top level directory
    
``/OpenMDAO/trunk``
    Release version of the code

``/OpenMDAO/dev/<username>``
    Shared repository where all active branches for a specific user (identified by ``<username>``) are
    located

``/OpenMDAO/dev/<username>/T<ticket number>-<desc>``
    Top level directory of an active branch owned by a specific user (identified by ``<username>``) for a
    specific :term:`ticket`.  The branch name typically includes the ``<ticket number>`` and a very brief description
    (``<desc>``) of the ticket.
    
``/OpenMDAO/dev/shared``
    Directory for repositories shared between developers.

``/OpenMDAO/dev/shared/working_main``
    Shared repository for all developers.

``/OpenMDAO/eggs``
    Directory containing Python distributions for all packages used in
    OpenMDAO
    
``/OpenMDAO/eggs/<package name>``
    Directory containing all distributions of a particular package


Creating a Branch
=================


A Python script, called ``mkbranch.py`` located in the ``util`` directory of the
trunk will help create and configure your development branch for you.  It will
create your branch and bootstrap a buildout for you on the branch. Internally,
the script is simply talking to Bazaar and ``zc.buildout``. You could perform these
tasks manually, but you should use the script to keep your branch
consistent with others in OpenMDAO.  This will make it easier for the
configuration manager to locate and merge your branch, and it will also make it
easier for other developers on the team to help you if you run into a problem.


.. index:: ticket

The following command will create a branch as well as create and bootstrap the
buildout:

::

  <python> /OpenMDAO/trunk/util/mkbranch.py -t <ticket number> [-d <description>][-s <source repository>][-u <user name>]

where the following parameters are user specified:

``<python>`` 
   The specific version of Python you want to use for the branch, for example,
   ``python2.6``.  Whatever version of Python you use for this command will be
   *hard-wired* into all of the buildout-generated scripts.

``<ticket number>``
   The ticket number used by the bug tracking system
   
``<description>``
   *(optional)* A short description  of the purpose of the branch. The description
   should be fewer than 15 characters in length. 
   
``<source repository>``
   *(optional)* The top directory of the repository you want to branch from. If
   not supplied, this defaults to the top directory of the trunk.
   
``<user name>``
   *(optional)* This should be your username on *torpedo*.  This is set 
   automatically for you based on the *LOGNAME* environment variable, so 
   generally you should not have to set this one.
   

As an example, if you wanted to create a branch off of the trunk to fix a bug in
the unit conversion code based on ticket #321 in the bug tracker and wanted to
use version 2.6 of Python, you could issue the following command:

::

   python2.6 /OpenMDAO/trunk/util/mkbranch.py -t 321 -d units_fix 


After the script runs, change your directory to
``/OpenMDAO/dev/<username>`` directory, where ``<username>`` is your
user name on *torpedo*.  For example, for user name *bnaylor*, the branch
from the command above would be created in 
``/OpenMDAO/dev/bnaylor/T321-units_fix``. Branches are named using the
following form:

::

  T<ticket number>-<desc>


.. index:: Wing

Editing/Debugging Source Code
=============================

Wing is a very nice integrated editor and debugger for Python that is available to
local OpenMDAO developers.  OpenMDAO comes with a buildout recipe called 
``openmdao.recipes:wingproj`` that will create a Wing project file with
Python path and executable settings that will make it work with the buildout.

To run Wing for your buildout, type:

::

    bin/wing
    
from your ``buildout`` directory. If the eggs used in your buildout change and you
re-run your buildout while Wing is still running, you will be notified by Wing
that your project settings have changed. Select *Discard Changes and Reload*
if your Wing path needs to be updated. Otherwise, select *Don't Reload* to
keep your existing project file. If your Wing project seems to not be working
properly after this happens, you can remove the Wing project file
(``<buildout_dir>/parts/wingproj/wingproj.wpr``) and re-run the buildout to
create a new one. 


.. index:: repo.py

Repository Utility
==================

The script ``repo.py`` is a utility script for manipulating and navigating in repositories.

::

    Usage: repo.py OP [options] repository, where OP may be:
       check  -- check for lock
       lock   -- lock repository
       unlock -- unlock repository
       set    -- set this as current repository
       fix    -- fix permissions

    Options:
      -h, --help     show this help message and exit
      -f, --force    forced unlock
      -v, --verbose  print info messages

*Repository* is a directory under ``/OpenMDAO/dev/<username>`` or
``/OpenMDAO/dev/shared``.

The *check, lock*, and *unlock* operations can be used to avoid
more than one developer trying to update a shared repository at the same time.
Before making changes, do a *lock*.  If that succeeds, then proceed with
your changes and when complete, do an *unlock.*  If the *lock* fails, then
you'll know who to wait for.  The *check* operation will test for a locked
repository.  Note that no enforcement is done.  Locking/unlocking merely
sets a flag.  If people ignore this convention, then they can potentially
interfere with each other's changes to the shared repository.

The *set* operation sets the given repository directory as your current
repository.  This will start a new shell process with the ``OPENMDAO_REPO``
environment variable set to the full path of the repository.  The local
system scripts will use this to update your *PATH* so the ``buildout/bin``
and ``scripts`` directories are at the beginning.  You will also get some
convenient aliases for navigating around in the repository directory
structure.  Finally, if the repository is under ``/OpenMDAO/dev/shared``,
your umask will be set to 002, allowing others in the *mdao* group to
update files you own.

The *fix* operation is used to fix file permissions in shared repositories.
It will traverse the directory tree and try to ensure all operations enabled
for owner are also enabled for group.  If you don't own the file,
the operation will fail and the owner's user id will be reported.

