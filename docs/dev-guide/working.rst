.. index:: branch; working on

Working on Your Branch
----------------------

You must run ``bin/buildout`` from your ``buildout`` directory before you'll be
able to execute OpenMDAO in any way. Running ``bin/buildout`` populates your
``eggs`` directory with all of the eggs that OpenMDAO depends upon and also builds
the OpenMDAO Sphinx documentation, which can be viewed in a browser by typing 
``bin/docs``.

As you make changes to the source code, you may want to modify your buildout
in some way, possibly adding new eggs, updating to new versions, etc. Whenever
this happens, you must re-run ``bin/buildout``.

When you make changes to the OpenMDAO documentation, you can either run ``bin/buildout``
to rebuild the Sphinx docs, or you can run ``bin/sphinx-build`` if you don't want to 
run a full buildout.


.. index:: pair: source code; editing and debugging
.. index:: pair: source files; adding


Adding New Source Files
=======================

If you create new files or directories that you want to be part of OpenMDAO, you
must add them to your repository by running the command:

::

   bzr add <filename>
        
If ``<filename>`` is a directory, all files within the directory will also be
added to the repository, unless they match any of the patterns in the
``.bzrignore`` file located in the top level directory of the branch.  To add a
new pattern for Bazaar to ignore, type:

::

   bzr ignore <pattern>
   
where ``<pattern>`` can be a filename or a wildcard expression, e.g., ``*.exe``.


If you add a file or directory to the repository by mistake, type:

::

   bzr remove <filename> --keep
   
This will remove the file from the repository but will **not** delete it.


