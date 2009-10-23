.. index:: Bazaar; commands

Bazaar Commands 
===============

Please note that the OpenMDAO *Developer's Guide* contains all the information you need
to get started working, including Bazaar setup, code location, and creating your branch. The
information here is a quick reference for
some common tasks you will be doing. 

.. note::
   In the examples, the percent sign (%) represents the command-line prompt. 
   Text included in pointy brackets means you have to supply a name or other
   information. For example, ``/OpenMDAO/dev/<your_working_directory>`` requires you
   to supply a name for the working directory, such as: ``/OpenMDAO/dev/pziegfel``  

Using Common Bazaar Commands 
----------------------------

References:

* Bazaar User Guide: http://doc.bazaar-vcs.org/latest/en/user-guide/index.html
* Bazaar in Five Minutes: http://doc.bazaar-vcs.org/latest/en/mini-tutorial/index.html
* Bazaar Tutorial: http://doc.bazaar-vcs.org/latest/en/tutorials/tutorial.html


.. index:: branch; creating

*Creating a branch from working_main*
++++++++++++++++++++++++++++++++++++++

You need to be in your OpenMDAO working directory (e.g., pziegfel, kmoore), so type:

::

%cd /OpenMDAO/dev/<your_working_directory>  
%bzr branch /OpenMDAO/dev/shared/working_main/ T<ticket#>-<branch_name>

Your Trac ticket number and branch number should correspond. When working on your branch, be
sure to add any new files that you create. This command will also list the files that were
added.

::

%bzr add 



.. index:: branch; building on

.. _Building-on-your-branch:

*Building on your branch*
+++++++++++++++++++++++++

::

%cd /OpenMDAO/dev/<your_working_directory>/T<ticket#>-<branch_name>
%cd /buildout			
%python<version#> isolatedbootstrap.py  (If you do not use a version #, it builds with the default.) 
%bin/buildout  			
%bin/test --all		

.. note:: Always run the ``python<version#> isolatedbootstrap.py`` script the first time you build on a
   branch. For subsequent builds, only ``bin/buildout`` is required. 
   
   The test suite tests the code snippets in the documentation.

.. index:: branch; merging


*Merging your branch to working_main*
+++++++++++++++++++++++++++++++++++++

- Commit changes and merge:

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

Bazaar will merge and then list all added, removed, and modified files. It will also
list any conflicts. 

- If you have **NO** conflicts, you can build, commit, and fix permissions:

::

 %cd /buildout			
 %python<version#> isolatedbootstrap.py   (Always run this script before building on working_main.)
 %bin/buildout 				
 %bin/test --all				
 %bzr status					
 %bzr commit -m "<commit comments>"	
 %repo.py fix 	             		  (Always run this script after building on work_main.)

The ``repo.py fix`` script fixes file permissions that may have gotten changed during the build process. 

- If you **HAVE** a conflict, you must resolve it:

Bazaar will display the number of conflicts and provide the path to the files in
conflict. (It also embeds markers in the file where there is a conflict.) You want to
confirm that there are conflicts and then bring up a graphical interface for
resolving them. Type the following:

:: 

%bzr conflicts        			
%conrez.py


For each file with a conflict, Bazaar creates 3 versions, each with a different
suffix:

	| ``filename.THIS``  	 (file you are merging to)
	| ``filename.OTHER``  	 (file that is being merged)
	| ``filename.BASE``   	 (original file)


After you select the file you want, save it. (Don't exit the GUI, just that file.) You should automatically
move on to the next conflict. If you don't, run ``conrez.py`` again and finish resolving the conflicts. Then
type:

::

%bzr conflicts    	
%bzr resolve		

Running ``resolve`` marks the files as resolved and cleans up BASE, THIS and OTHER from
working_main.

:: 

%cd /buildout			
%python<version#> isolatedbootstrap.py  
%bin/buildout 				
%bin/test --all				
%bzr status					
%bzr commit -m "<commit comments>"	
%repo.py fix 			   (Always run this script after building on work_main.)

This last script fixes permissions that may have gotten changed during the build process. 


*Canceling a merge/removing uncommitted changes*
++++++++++++++++++++++++++++++++++++++++++++++++

If you have an issue that cannot be resolved timely, you can cancel the merge by typing:

::

%bzr revert

You can also use this command if you don't want to commit changes you've made. In this case, it's a
good idea to see what files will be removed, so type:

::

%bzr diff					      
%bzr revert



*Removing a directory and its files*
++++++++++++++++++++++++++++++++++++

This is a UNIX command for removing directory and files:

::

% rm -rf <directory_name>
