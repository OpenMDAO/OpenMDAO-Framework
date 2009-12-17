.. index:: Bazaar; commands

.. _Bazaar-Commands:

Bazaar Commands 
===============

Please note that the OpenMDAO *Developer's Guide* contains everything you need to get started working,
including information on Bazaar setup, code location, and how to create your branch. The information here is a
quick reference for some common tasks you will be doing. 

.. note::
   In the examples, the percent sign (%) represents the command-line prompt. 
   Text included in pointy brackets means you have to supply a name or other
   information. For example, ``/OpenMDAO/dev/<your_working_directory>`` requires you
   to supply a name for the working directory, such as: ``/OpenMDAO/dev/pziegfel``  

References:

* Bazaar User Guide: http://doc.bazaar-vcs.org/latest/en/user-guide/index.html
* Bazaar in Five Minutes: http://doc.bazaar-vcs.org/latest/en/mini-tutorial/index.html
* Bazaar Tutorial: http://doc.bazaar-vcs.org/latest/en/tutorials/tutorial.html

.. index Bazaar commands

Common Bazaar Commands
----------------------

To use these commands, type ``bzr <command_name>``, for example ``bzr add``.

::
  
  add 		(Adds files/directories to the Bazaar repository on your branch. Added files are listed.)
  branch	(Creates a new copy of a branch.)
  commit	(Commits changes into a new revision. You must add commit comments via "-m" (for "message") or you automatically go into NEdit and must add comments there.)
  conflicts	(Lists files with conflicts.)
  log --forward	(Displays revisions on a branch [e.g., your branch, working_main]. The "--forward" option displays most recent activity last.)    
  merge		(Merges committed changes from a branch to working_main [for developers]. SCM merges to the trunk/mainline.)
  revert	(Reverts files to a previous revision.) 
  status	(Displays pending changes, if any; if no uncommitted changes are pending, it returns to the prompt.)
  uncommit	(Removes the last committed revision.)
  
Note that all files on your branch are available to be changed. By running the ``bzr status``
command, you can see all of the uncommitted revisions on your branch. 

  
.. index:: branch; creating

Creating a Branch from working_main
-------------------------------------

You need to be in your OpenMDAO working directory (e.g., pziegfel, ktmoore1), so type:

::

  %cd /OpenMDAO/dev/<your_working_directory>  
  %bzr branch /OpenMDAO/dev/shared/working_main/ T<ticket#>-<branch_name>

Your Trac ticket number and branch number should correspond. When working on your branch, be sure
to add any new files that you create using the ``bzr add`` command. You can use the command to
add a specific file or directory (``bzr add <filename>``), but it's easier to type it by itself,
in which case, all new files and directories are added.


.. index:: branch; building on

.. _Building-on-Your-Branch:

Building on Your Branch
-----------------------

If you are in your home directory, type:

::

  %cd /OpenMDAO/dev/<your_working_directory>/T<ticket#>-<branch_name>
  %cd /buildout				(Takes you to the "buildout" directory.) 
  %python2.6 isolated_bootstrap.py  	(Command that must be run the first time you build.)
  %bin/buildout				(Builds on your branch.)		
  %bin/docs				(Displays the documentation.)  			
  %bin/test --all			(Runs the tests.)

.. note:: You must run the ``python2.6 isolated_bootstrap.py`` script the first time you build on
   a branch. However, for subsequent builds, only ``bin/buildout`` is required. (The test suite tests
   the code snippets in the documentation.)

.. index:: branch; merging to


Merging working_main to Your Branch
------------------------------------

As you work on your branch, you may want to update it every few days (from ``working_main``) to avoid conflicts
when you merge back. In the example that follows, first we go to ``working_main`` and display the log to see what
was recently committed. If you want those changes now, you can then merge from ``working_main`` to your branch.
To do so, type:

::

  %cd /OpenMDAO/dev/shared/working_main
  %bzr log --forward 	 
  %bzr status		(Check to make sure there is not a pending merge by another team member. You want to return to the prompt.)
 		
  
If you decide to merge out from ``working_main``, type the following:

::
  
  %cd /OpenMDAO/dev/<your_working_directory>/T<ticket#>-<branch_name>   
  %bzr status		 			(Checks for any uncommitted changes.)
  %bzr commit -m "<commit comments>"		(Needed only if you have uncommitted changes.) 
  %cd buildout					(Takes you to the "buildout" directory.)
  %bin/buildout					(Makes sure your branch builds after committing changes.)
  %bin/docs 					(Checks that the documentation displays correctly.)	
  %bin/test --all				(Makes sure the tests pass before you merge.)
  %bzr status					(Needed if you committed and want to verify that nothing was missed. Should return to the prompt if OK to merge.) 
  %bzr merge /OpenMDAO/dev/shared/working_main  (Merges from working_main to your branch.)

You can check the Bazaar documentation for additional options to use with the *log* command to restrict
how much you see on the screen.

Resolve any conflicts that come up during the merge. See :ref:`if you have a conflict
<if-you-have-a-conflict>`. After you have resolved any conflicts, type:

::

  %bin/buildout
  

If you have any build errors or warnings, resolve them before continuing. When you can
build successfully without warnings, type the following:

::

  %bin/docs   			      (Displays all the documentation in Firefox.)
  %bin/test --all		      (Runs the test suite.)
  %bzr status			      (Makes sure there are no uncommitted changes.)
  %bzr commit -m "<commit comments>"  (Lets you enter commit comments, which are required, on the command line.)


.. index:: branch; merging from
 
Merging Your Branch to working_main
------------------------------------

You need to commit your changes before merging. When you commit changes, you
must add comments. If you forget to add "-m", you will automatically go into a
file in the NEdit text editor, where you will have to enter comments, save them,
and then exit the file.

::

  %cd /OpenMDAO/dev/<your_working_directory>/T<ticket#>-<branch_name>   
  %bzr status
  %bzr commit -m "<commit comments>"         
  %cd buildout
  %bin/buildout 
  %bin/test --all
  %cd /OpenMDAO/dev/shared/working_main
  %bzr status
  %bzr merge /OpenMDAO/dev/<your_working_directory>/T<ticket#>-<branch_name>

Bazaar will merge to your branch to ``working_main`` and then list all added, removed, and modified files. It will
also list any conflicts. 

- If you have **NO** conflicts, you can build, commit, and fix permissions on ``working_main``. On
  ``working_main``, type the following:

::

  %cd /buildout		             	
  %python2.6 isolated_bootstrap.py   (Always run this script before building on working_main.)
  %bin/buildout 				
  %bin/test --all				
  %bzr status					
  %bzr commit -m "<commit comments>"	
  %repo.py fix 			     (Always run this script after building on working_main. It fixes file permissions that may have gotten changed during the build process.) 

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


        | ``filename.BASE``   	 (original file)
	| ``filename.OTHER``  	 (the file being merged)
	| ``filename.THIS``  	 (the file you are merging to)

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

Make sure there are no more conflicts. If there are, resolve them as above. If there are none, you build and
commit your changes. Type: 

::

  %bzr conflicts    		
  %cd /buildout			
  %python2.6 isolated_bootstrap.py  
  %bin/buildout 
  %bin/docs				
  %bin/test --all				
  %bzr status			      (Shows all the merged files from working_main that have not yet been committed on your branch.)		
  %bzr commit -m "<commit comments>"  (Commits merged files on your branch.)
  %repo.py fix 	  		      (Always run this script after building on "working_main" to fix any permissions that were changed during the build process.) 

.. index:: merge; cancelling

Canceling a Merge/Removing Uncommitted Changes
----------------------------------------------

If you have an issue that cannot be resolved quickly, you can cancel the merge by typing:

::

  %bzr revert

You can also use this command if you don't want to commit changes you've made. In this case, it's a
good idea to see what files will be removed, so type:

::

  %bzr diff					      
  %bzr revert
  
  
Additionally, you may commit changes on your branch and then you decide that you don't want them. To remove 
them, you can type:

::

  %bzr uncommit


Removing a Directory and Its Files
----------------------------------

Although Bazaar does have a ``remove`` command, it is probably just as easy to use the
common UNIX command for removing a directory and its files:

::
  
  %rm -rf <directory_name>
