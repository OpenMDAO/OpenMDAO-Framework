.. index:: Bazaar; commands

.. _Bazaar-Commands:

Bazaar Commands 
===============

Please note that the OpenMDAO *Developer's Guide* contains everything you need to get started working,
including information on Bazaar setup, code location, and how to create your branch. The information here is a
quick reference for some common tasks you will be doing. 

.. note::
   - In the examples, the percent sign (%) represents the command-line prompt. 
   - Text included in pointy brackets means you have to supply a name or other
     information. For example, ``/OpenMDAO/dev/<your_working_directory>`` requires you
     to supply a name for the working directory, such as: ``/OpenMDAO/dev/pziegfel``.
   - The examples provided assume you are working on OpenMDAO's Linux server. 

References:

* Bazaar User Guide: http://doc.bazaar-vcs.org/latest/en/user-guide/index.html
* Bazaar in five minutes: http://doc.bazaar-vcs.org/latest/en/mini-tutorial/index.html
* Bazaar Tutorial: http://doc.bazaar-vcs.org/latest/en/tutorials/tutorial.html

.. index Bazaar commands

Common Bazaar Commands
----------------------

To use these commands, type ``bzr <command_name>``, for example ``bzr add``.

::
  
  add 		(Adds files/directories to the Bazaar repository on your branch.)
  branch	(Creates a new copy of a branch.)
  commit	(Commits changes into a new revision. You must add a commit message via "-m" or NEdit, a text editor.)
  conflicts	(Lists files with conflicts.)
  log --forward	(Displays revisions on a branch. The "--forward" option means the most recent activity will be displayed last.)    
  merge		(Pulls in committed changes from another branch.)
  revert	(Cancels all changes since the last merge, so you revert to the previous revision.)
  status	(Displays pending changes, if any; if no uncommitted changes are pending, it returns to the prompt.)
  
Note that all files on your branch are available to be changed. By running the ``bzr status``
command, you can see all of the uncommitted changes on your branch. 

  
Managing Files
--------------

This section discusses some of the commands used to manage your files.


*Creating a Directory*
++++++++++++++++++++++

If you want to create a new versioned directory, type the following:

::

  %bzr mkdir <directory_name>
  
  
*Adding a File*
+++++++++++++++

To add a file or directory to the Bazaar repository on your branch, type:

::

  %bzr add
  
You can provide an argument with this command (i.e., a specific file name). If you add a
file whose parent directory is not versioned, Bazaar will automatically add the parent
directory and so on up to the root. 

If you use this command without an argument, Bazaar will add everything under the current
directory that has not yet been added to the repository.

After you type ``bzr add``, Bazaar will display a list of the files and directories that were added.

.. index:: removing a file/directory



*Removing a File or Directory*
++++++++++++++++++++++++++++++

Bazaar's ``remove`` command is similar to the UNIX command, and either can be used to remove a file.

::

  %bzr remove <file_name>     (Bazaar "remove" command)
  %rm <file_name> 	      (UNIX "remove" command)
    
However, to remove a directory, it's easier to use the UNIX remove command (``rm``), as follows:


::
  
  %rm -rf <directory_name>    (Removes a directory and recursively removes the files in it.)


.. index:: moving a file/directory
.. index:: renaming a file/directory


*Moving or Renaming a File*
+++++++++++++++++++++++++++

The move command (``bzr mv``) is used to rename or move a file, depending on the arguments you
provide. When moving a file, you must provide the path to the new location. When you
move a file, Bazaar deletes the file from its current location.

To rename or move a file, you must be in the directory containing the file you want to rename or move; then enter
the appropriate command. See the examples that follow: 

::

  %bzr mv <old_file_name> <new_file_name>                  (Renames a file)
  Example:
  %bzr mv test1.rst test2.rst                              (Renames "test1.rst" to "test2.rst")
  
  %bzr mv <file_to_move> <path_to_new_location><new_name>  (Moves and renames a file)
  Examples: 
  %bzr mv test1.rst ../user-guide/test2.rst                (Moves "test1.rst" to "user-guide" dir & renames it "test2.rst")   
  %bzr mv test1.rst ../user-guide/.                        (Moves "test1.rst" to "user-guide" dir & keeps the same name)


.. note::
   If you need to move an entire directory, use the ``bzr mv`` command, NOT the UNIX command, to ensure that
   the directory and all its files get moved correctly.


.. index:: diff command

*Viewing Changes in a File*
+++++++++++++++++++++++++++

If you have edited a file and want to see what you have done, type:

::

  %bzr diff <file_name>
  
Bazaar will display the name of the modified file (the name you specified) and then list the additions and deletions with a
plus (+) or minus (-) sign in front of the changed lines.   	


.. index:: log command

*Viewing the Revision Log*
++++++++++++++++++++++++++


You can see the history of your branch by browsing its log. To see a complete list of revisions on the current branch
beginning with the first revision and displaying the most recent revision last, type: 

::

  %bzr log --forward 
  
Information will be provided about each revision, including:

  * Revision number
  * Name of the person who committed the revision
  * Name of branch where revision originated
  * Date/time the revision was committed
  * Commit message 

If you do not use the ``--forward`` option and merely type ``bzr log``, the first revision will be
displayed last, and you will have to scroll up to view the most recent revisions.

 

.. index:: branch; creating

Creating a Branch from working_main
-------------------------------------

You need to be in your OpenMDAO working directory (e.g., pziegfel, ktmoore1), so type:

::

  %cd /OpenMDAO/dev/<your_working_directory>  				   (Takes you to your working_directory.)
  %bzr branch /OpenMDAO/dev/shared/working_main/ T<ticket#>-<branch_name>  (Creates branch from "working_main.)
  Example:
  %bzr branch /OpenMDAO/dev/shared/working_main/ T30-user_guide_updates	   (Creates branch: "T30-user_guide_updates.")

Your Trac ticket number and branch number should correspond. When working on your branch, be sure
to add any new files that you create using the ``bzr add`` command. You can use the command to
add a specific file or directory (``bzr add <filename>``), but it's easier to type it by itself. In the
latter case, everything in the current directory will be added. So, if you enter the command at the top of
your branch, all unadded directories and files will be added.


.. index:: branch; building on

.. _Building-on-Your-Branch:

Building on Your Branch
-----------------------

If you are in your home directory, type:

::

  %cd /OpenMDAO/dev/<your_working_directory>/T<ticket#>-<branch_name>  (Takes you to your branch.)
  %cd /buildout			                         (Takes you to the "buildout" directory.) 
  %python2.6 isolated_bootstrap.py                       (Runs the script needed before you can build the first time.)
  %bin/buildout			                         (Builds on your branch.)		
  %bin/docs			                         (Displays the documentation.)  			
  %bin/test --all		                         (Runs the test suite.)

.. note:: You must run the ``python2.6 isolated_bootstrap.py`` script the first time you build on
   a branch. However, for subsequent builds, only ``bin/buildout`` is required. 
   
   The test suite includes tests for code snippets in the documentation.


.. index:: branch; merging to

Merging working_main to Your Branch
------------------------------------

As you work on your branch, you may want to periodically update it from ``working_main`` to avoid conflicts
when you merge back. In the example that follows, first we go to ``working_main`` and display the log to see what
was recently committed. You can then decide if you want to merge to your branch. Type:

::

  %cd /OpenMDAO/dev/shared/working_main
  %bzr log --forward 	  (Checks the log for recent activity.) 
  %bzr status		  (Checks to make sure there is no pending merge by another team member. If there is, check later.)
 		
You decide to merge out from ``working_main``, so go to your branch.

::
  
  %cd /OpenMDAO/dev/<your_working_directory>/T<ticket#>-<branch_name>
  Example:
  %cd /OpenMDAO/dev/pziegfel/T30-user_guide_updates
  
  %bzr status		  (Checks your branch for uncommitted changes; you cannot merge if you have any.)
  
If you have uncommitted changes, use the ``bzr commit`` command. If you have no uncommitted changes, go
straight to the ``buildout`` directory.

::
  
  %bzr commit -m "<commit_message>"  (Commits changes and allows you to add commit message. [Or, you can use NEdit].)
  %cd buildout 		             (Takes you to your "buildout" directory.)
  %bin/buildout		             (Makes sure branch builds before you merge to it [and after your commit, if applicable].)
  %bin/docs 		             (Checks that docs display correctly. Optional if no doc changes.)	
  %bin/test --all 	             (Runs test suite; all tests should pass before you merge.)
  %bzr merge /OpenMDAO/dev/shared/working_main  (Merges from working_main to your branch.)

You must resolve any conflicts that come up during the merge. If conflicts arise, see :ref:`if you have a
conflict <if-you-have-a-conflict>`. After you have resolved any conflicts or if you had none, type:

::

  %bin/buildout    		       (Makes sure you can build on the branch after the merge.)
  %bin/test --all		       (Makes sure the tests pass after merging and before committing the changes.)
  %bzr status			       (Lists pending changes & merges, since merged files have not yet been committed.)
  %bzr commit -m "<commit_message>"    (Commits changes from the merge and allows you to add required commit message. 
                                        You can also omit the "-m" and write the message using the NEdit text editor.)
  %bzr python2.6 isolated_bootstrap.py (Runs rerquired script before first build after committing changes.)
  %bin/buildout    		       (Builds on the branch after the merge.)
  
.. index:: branch; merging from
 

Merging Your Branch to working_main
------------------------------------

You need to commit your changes to your local repository before merging your branch to ``working_main``. When
you commit changes, you must add comments about the revision. If you forget to add "-m" and/or the commit message,
you will automatically go into a file in NEdit, a text editor. Enter your commit comments and save them when
exiting.

::

  %cd /OpenMDAO/dev/<your_working_directory>/T<ticket#>-<branch_name>        (Takes you to the branch to be merged.) 
  %bzr status			       (Checks for uncommitted changes. You cannot merge if there are any.)
  %bzr commit -m "<commit_message>"    (Needed only if you have uncommitted changes.)       
  %cd buildout			       (Takes you to the "buildout" directory.)
  %bin/buildout 		       (Builds your branch. You should be able to build without errors or warnings.)
  %bin/test --all	               (Runs the test suite. Tests should pass on your branch before you merge.)
  %cd ../../shared/working_main	       (Takes you up two levels and then to "working_main."
  %bzr status			       (Checks to make sure there are no pending merges.)
  %bzr merge /OpenMDAO/dev/<your_working_directory>/T<ticket#>-<branch_name>  (Merges your branch to "working_main.")

Bazaar will merge your branch to ``working_main`` and then list all added, removed, and modified files. It will
also list any conflicts. 

- If you have **NO** conflicts, you can build, commit, and fix permissions on ``working_main``. On
  ``working_main``, type the following:

::

  %cd /buildout		             	
  %python2.6 isolated_bootstrap.py    (Required script that must be run before building on "working_main.")
  %bin/buildout 		      (Makes sure you can build on "working_main" after the merge.)
  %bin/docs			      (Displays the documentation.)
  %bin/test --all		      (Runs the test suite. All tests must pass before you can commit your  
                                       changes to "working_main.")	
  

.. note::
   If tests passed on your branch but do not pass on ``working_main``, you must revert the changes and contact
   bret.a.naylor@nasa.gov to resolve any issues. See :ref:`Canceling a Merge and Reverting Changes
   <Canceling-a-Merge-and-Reverting-Changes>`.
 
If all tests passed, you may continue with the merge process.
   
::

  %bzr status			      (Lists pending changes & merges that have not yet been committed.)	
  %bzr commit -m "<commit_message>"   (Commits changes from the merge to "working_main.")
  %repo.py fix 	    		      (Always run this script after building on "working_main." It fixes any file 
                                       permissions that may have gotten changed during the merge and cleans up 
				       temporary files created during the buildout.) 
				     

.. _`if-you-have-a-conflict`:

- If you **HAVE** a conflict, you must resolve it:

Bazaar will display the changes in files or directories and will also indicate the number of conflicts and where they
occurred. See the following example:


.. figure:: ../images/quick-ref/merge_conflict.png
   :align: center
   
   Example of Conflicts When Merging


In the above example the "+N" indicates new files or directories. The "M" indicates modified files or
directories. If a file or directory is deleted, "-D" appears before its name.

To bring up a graphical interface for displaying the conflicts, type the following:

:: 
  			
  %conrez.py

Bazaar automatically creates three versions of the file in conflict, each with a
different suffix. The files appear in columns across the screen, left to right, in the
order listed here:


        | ``filename.BASE`` 	(original file)
	| ``filename.OTHER``	(file being merged)
	| ``filename.THIS``	(file you are merging to)

.. note::

   When you are merging to ``working_main``, your file will be ``.OTHER`` and ``working_main`` will be ``THIS``.
   However, if you are merging out from ``working_main`` to update your branch, ``.OTHER`` will be ``working_main``,
   and ``.THIS`` will be your branch.

Conflicts will be displayed in colored text across all three files. See the following example:

.. figure:: ../images/quick-ref/gui_merge_conflict.png
   :align: left
   
   GUI Showing Versions of a File in Conflict
 
|
  
In the above example, a new index entry ``CONMIN driver`` shows up in the ``.OTHER`` file (blue background
and red text). In the ``.THIS`` file on the right, the text with the green background is new. 

In some cases, the difference may just be the way the text is formatted. You must look at the files and
decide which version to send to ``filename.THIS`` or if the file is okay as it is. If the ``.THIS`` file is
okay, you can keep scrolling down. However, if you have to update it, select the appropriate change and
click on the arrow next to it. If you make a mistake, you can select *undo* from the menu bar at the top of the screen.

You may have to scroll to the right to read each of the files. After you have reviewed the conflicts and
made your selections, save your changes and click the "X" in the upper right corner to exit.

When you exit you will be asked if you want to *Save Selected*. Assuming that you do, click that option
and then click *Yes* when asked to save the file. 

Make sure there are no more conflicts. If there are, resolve them as above. If there are none, you may build and
commit your changes. Type: 

::

  %bzr conflicts    		     (Checks to see if there are still conflicts. Displays them if there are.)
  %cd /buildout			     (Takes you to the "buildout" directory.)
  %python2.6 isolated_bootstrap.py   (Required script that must be run before building on "working_main.")
  %bin/buildout 		     (Builds the branch.)						
  %bin/docs		             (Displays the documentation.)
  %bin/test --all		     (All tests should pass before you commit.)	
  %bzr status			     (Shows all the merged files from "working_main" that have not yet been  
                                      committed on your branch.)		
  %bzr commit -m "<commit_message>"  (Commits merged files on your branch and allows you to enter a commit message.)
  %repo.py fix 	  		     (Runs a script to fix file permissions that may have gotten changed when you 
                                      did a buildout. Always run this script after building on "working_main.") 


.. index:: merge; canceling
.. index:: reverting changes

.. _`Canceling-a-Merge-and-Reverting-Changes`:

Canceling a Merge and Reverting Changes
----------------------------------------

If you encounter a problem when merging and the issue cannot be resolved quickly, you can cancel the
merge by using the ``revert`` command. Type:

::

  %bzr revert		(Reverts to the previous revision and removes uncommitted changes.)

You can also use this command if you do not want to commit changes you've made. In this case, it is a
good idea to see what files will be removed, so type:

::

  %bzr diff		(Shows differences [additions, deletions] between two files.)			      
  %bzr revert		(Reverts to the previous revision.)
  
  
