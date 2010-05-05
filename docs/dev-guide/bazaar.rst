.. index:: Bazaar; commands

.. _Helpful-Bazaar-Commands:


Appendix: Helpful Bazaar Commands
=================================

This section contains commands that developers would use on a regular basis when performing  everyday
tasks. Most of them are Bazaar commands. Please note that earlier sections in this guide
contain everything you need to get started working, including information on Bazaar setup, code location,
and how to create your branch. The information here is a quick reference for some common tasks you will be
doing. 

.. note::
   Text included in pointy brackets means you have to supply specific information (e.g.,
   ``<file_name>``. Remember that the examples may include a ``/`` (Linux). Windows users will
   use a ``\`` instead.
   

References:

* Bazaar User Guide: http://doc.bazaar-vcs.org/latest/en/user-guide/index.html
* Bazaar in five minutes: http://doc.bazaar-vcs.org/latest/en/mini-tutorial/index.html
* Bazaar Tutorial: http://doc.bazaar-vcs.org/latest/en/tutorials/tutorial.html

.. index Bazaar commands

Common Bazaar Commands
++++++++++++++++++++++

To use these commands, type ``bzr <command_name>``, for example ``bzr add``.

::
  
  add       (Adds files/directories to the Bazaar repository on your branch.)
  branch    (Creates a new copy of a branch.)
  commit    (Commits changes into a new revision. You must add a commit message via "-m" or another text editor.)
  conflicts (Lists files with conflicts.)
  log --forward (Displays revisions on a branch. The "--forward" option means the most recent activity will be displayed last.)    
  merge     (Pulls in committed changes from another branch.)
  revert    (Cancels all changes since the last merge, so you revert to the previous revision.)
  status    (Displays pending changes, if any; if no uncommitted changes are pending, it returns to the prompt.)
  
Note that all files on your branch are available to be changed. By running the ``bzr status``
command, you can see all of the uncommitted changes on your branch. 

  
Managing Files
+++++++++++++++

This section discusses some of the commands used to manage your files.


*Creating a Directory*
~~~~~~~~~~~~~~~~~~~~~~

If you want to create a new versioned directory, type the following:

::

  bzr mkdir <directory_name>
  
.. index:: Bazaar, adding a file
.. index:: adding a file
  
*Adding a File*
~~~~~~~~~~~~~~~

To add a file or directory to the Bazaar repository on your branch, type:

::

  bzr add
  
You can provide an argument with this command (i.e., a specific file name). If you add a
file whose parent directory is not versioned, Bazaar will automatically add the parent
directory and so on up to the root. 

If you use this command without an argument, Bazaar will add everything under the current
directory that has not yet been added to the repository.

After you type ``bzr add``, Bazaar will display a list of the files and directories that were added.

.. index:: removing a file/directory

*Removing a File or Directory*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Bazaar's ``remove`` command is similar to the UNIX command, and either can be used to remove a file.

::

  bzr remove <file_name>     (Bazaar "remove" command)
  rm <file_name>             (UNIX "remove" command)
    
However, to remove a directory, it's easier to use the UNIX remove command (``rm``), as follows:

::
  
  rm -rf <directory_name>    (Removes a directory and recursively removes the files in it.)


.. index:: moving a file/directory
.. index:: renaming a file/directory


*Moving or Renaming a File or Directory*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The move command (``bzr mv``) is used to rename or move a file, depending on the arguments you
provide. When moving a file, you must provide the path to the new location. When you
move a file, Bazaar deletes the file from its current location.

To rename or move a file, you must be in the directory containing the file you want to rename or move; then enter
the appropriate command. See the examples that follow: 

::

  bzr mv <old_file_name> <new_file_name>                  (Renames a file)
  Example:
  bzr mv test1.rst test2.rst                              (Renames "test1.rst" to "test2.rst")
  
  bzr mv <file_to_move> <path_to_new_location><new_name>  (Moves and renames a file)
  Examples: 
  bzr mv test1.rst ../user-guide/test2.rst                (Moves "test1.rst" to "user-guide" directory & renames it "test2.rst")   
  bzr mv test1.rst ../user-guide/.                        (Moves "test1.rst" to "user-guide" directory & keeps the same name)


.. note::
   If you need to move an entire directory, use the ``bzr mv`` command, NOT
   the UNIX command, to ensure that the bazaar understands that all of the versioned
   files in the directory have moved.


.. index:: diff command

*Viewing Changes in a File*
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you have edited a file and want to see what you have done, type:

::

  bzr diff <file_name>
  
Bazaar will display the name of the modified file (the name you specified) and then list the additions and deletions with a
plus (+) or minus (-) sign in front of the changed lines.


.. index:: log command

*Viewing the Revision Log*
~~~~~~~~~~~~~~~~~~~~~~~~~~

You can see the history of your branch by browsing its log. To see a list of
the last 10 revisions, type:

::

  bzr log -r-10..
  
Information will be provided about each revision, including:

  * Revision number
  * Name of the person who committed the revision
  * Name of branch where revision originated
  * Date/time the revision was committed
  * Commit message 

If you'd prefer to see the most recent revision at the bottom, add the ``--forward`` option to
the command above.

 
.. index:: merge; canceling
.. index:: reverting changes

.. _`Canceling-a-Merge-and-Reverting-Changes`:


Canceling a Merge and Reverting Changes
++++++++++++++++++++++++++++++++++++++++

If you encounter a problem when merging to your branch, and things are messed
up enough that you want to start over, you can cancel the merge by using the ``revert``
command. Type:

::

  bzr revert         (Reverts to the previous revision and removes uncommitted changes.)

You can also use this command if you do not want to commit changes you've made. In this case, it is a
good idea to see what files will be removed, so type:

::

  bzr status    (Shows which files have been modified, deleted, or added.)
  bzr revert    (Reverts to the previous revision.)
  
  
