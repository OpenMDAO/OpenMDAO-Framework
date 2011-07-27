.. index:: Git; commands

.. _Helpful-Git-Commands:


Appendix A: Helpful Git Commands
===================================

This section contains commands that developers would use on a regular basis
when performing everyday tasks. Most of them are Git commands. Please note
that earlier sections in this guide contain everything you need to get started
working, including information on Git setup, code location, and how to
create your branch. The information here is a quick reference for some common
tasks you will be doing.

.. note::
   Text included in pointy brackets means you have to supply specific information (e.g.,
   ``<file_name>``. Remember that the examples may include a ``/`` (Linux). Windows users will
   use a ``\`` instead.
   

References:

* Pro Git: http://progit.org/book/

.. index Git commands

Common Git Commands
+++++++++++++++++++

To use these commands, type ``git <command_name>``, for example ``git add``.


===================  =================================================================================
Command Name         Description
===================  =================================================================================
``add``              Adds files/directories to a Git repository.
-------------------  ---------------------------------------------------------------------------------
``branch``           Creates a new branch that points to the current HEAD when the command is run.
-------------------  ---------------------------------------------------------------------------------
``commit -a``        Commits the current snapshot to the repository database. You must add a commit 
                     message via ``-m`` (command line) or a text editor.  You should always run this
                     with a ``-a`` argument, which forces all modified files that are part of the
                     repository to be committed.  Without the ``-a``, only files that have been
                     explicitly added using ``git add`` since the last commit will be committed.
-------------------  ---------------------------------------------------------------------------------
``help <cmd>``       Prints a detailed help page for the specified command <cmd>
-------------------  ---------------------------------------------------------------------------------
``log``              Displays commit history.
-------------------  ---------------------------------------------------------------------------------
``merge``            Pulls in committed changes from another branch.
-------------------  ---------------------------------------------------------------------------------
``reset HEAD``       Cancels all changes since the last commit.
-------------------  ---------------------------------------------------------------------------------
``status``           Displays files that have been modified and/or staged for the next commit.
===================  =================================================================================
  
  
Managing Files
+++++++++++++++

This section discusses some of the commands used to manage your files.


*Adding a File or Directory*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To add a file or directory to your Git repository, type:

::

  git add
  
You can provide an argument with this command (i.e., a specific file name). If you add a
file whose parent directory is not versioned, Git will automatically add the parent
directory and so on up to the root. 

If you use this command without an argument, Git will add everything under the current
directory that has not yet been added to the repository. It is wise to use this command in the
directory where you want to add files, so you don't add temporary files you do not want.

A list of glob patterns for files or directories that should never be added to the repository
can be placed in ``.gitignore`` files.  A ``.gitignore`` file will control what is ignored in
its current directory and all subdirectories, so putting a single ``.gitignore`` at the top
of the repository will set ignore behavior for the whole repository.


.. index:: removing a file/directory

*Removing a File or Directory*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To remove a file from both the file system and the staging area, use

::

   git rm <file_name>

   
To remove it only from the staging area, use

::

   git rm --cached <file_name>
   
  

.. index:: moving a file/directory
.. index:: renaming a file/directory


.. index:: diff command

*Viewing Changes in a File*
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you have edited a file and want to see the differences in the working tree versus the staging area,
you can type:

::

  git diff -- <file_name>
  

To see the difference between a file in the staging area and a file in an old commit, use:

::

   git diff --staged [commit] -- <file_name>
   
If commit is not supplied, it is assumed to be HEAD. To target some other commit, use the hash id
of that commit.


.. index:: log command

*Viewing the Revision Log*
~~~~~~~~~~~~~~~~~~~~~~~~~~

You can see the history of your branch by browsing its log. To see a list of
the last 10 revisions, type:

::

  git log -10
  
Information will be provided about each revision, including:

  * Commit hash id
  * Name and email of the person who committed the revision
  * Date/time the revision was committed
  * Commit message 

 
.. index:: merge; canceling
.. index:: reverting changes

.. _`Canceling-a-Merge-and-Reverting-Changes`:


*Custom Configuration*
~~~~~~~~~~~~~~~~~~~~~~

The ``git config`` command can be used to customize your Git experience by 
setting a number of preferences, for example:

To set the editor used for commit messages:

::


    git config --global core.editor <editor_name>

   
To set the graphical merge tool:

::


    git config --global merge.tool <tool_name>


To create a Git command alias:

::


    git config --global alias.<name> ‘aliased_command’ 

    
For example, ``git config --global alias.unstage ‘reset HEAD --’``
If the alias runs an external command, prefix it with a **!**, 
e.g., “!gitk”


To list all of your current config settings, do:

::


    git config --list
    
    
