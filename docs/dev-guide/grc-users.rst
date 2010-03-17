
Notes for Developers at GRC
---------------------------

This section outlines policies that are specific to OpenMDAO development that
happens locally on computers at the NASA Glenn Research Center. If you
are not a developer at GRC, you can skip this section.


Creating a Branch
=================

When you create a branch, you should name your branch using a form like this:

::

    T<ticket_number>-<description>
    
where ``<ticket number>`` is the ticket number in the bug tracker that corresponds
to your branch, and ``<description>`` is an optional short description for the
work being done on the branch. The description should be fewer than 15
characters in length.
   

As an example, if you wanted to create a branch off of the trunk to fix a bug in
the unit conversion code based on ticket #321 in the bug tracker, you could issue 
the following command:

::

    bzr branch lp:openmdao T321-units_fix



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
    
from the ``buildout`` directory in the top level of the repository. If the
eggs used in your buildout change and you re-run your buildout while Wing is
still running, you will be notified by Wing that your project settings have
changed. Select *Discard Changes and Reload* if your Wing path needs to be
updated. Otherwise, select *Don't Reload* to keep your existing project file.
If your Wing project seems to not be working properly after this happens, you
can remove the Wing project file
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

