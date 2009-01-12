.. index:: pair: branch; working on

Working on Your Branch
----------------------

As you make changes to the source code, you may want to modify your buildout
in some way, possibly adding new eggs, updating to new versions, etc. Whenever
this happens, you must re-run the ``buildout`` script that lives in the top
level ``bin`` directory of your buildout.


.. index:: pair: source code; editing and debugging
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
    
from your buildout directory.  Whenever you re-run your buildout, you will be
prompted by Wing that your project settings have changed. Select ``Discard
Changes and Reload`` if your Wing path needs to be updated. Otherwise, select
``Don't Reload`` to keep your existing project file.


.. index:: pair: source files; adding

Adding New Source Files
=======================

If you create new files or directories that you want to be part of OpenMDAO, you
must add them to your repository by running the command:

::

   bzr add <filename>
        
If ``<filename>`` is a directory, all files within the directory will also be
added to the repository, unless they match any of the patterns in the
``.bzrignore``
file located in the top level directory of the branch.  To add a new pattern
for Bazaar to ignore, type:

::

   bzr ignore <pattern>
   
where ``<pattern>`` can be a filename or a wildcard expression, e.g., ``*.exe``.


If you add a file or directory to the repository by mistake, type:

::

   bzr remove <filename> --keep
   
This will remove the file from the repository but will **not** delete it.

