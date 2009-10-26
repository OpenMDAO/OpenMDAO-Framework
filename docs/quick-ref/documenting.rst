.. index:: documentation; creating

Documenting Changes
===================

If you have any questions or would like help with the documentation, please contact the tech
writer (Paulette) at: 1-216-433-8056 or paulette.j.ziegfeld@nasa.gov. 


Updating an Existing Document
-----------------------------

All documentation is located in the ``docs/`` directory on your branch. There are separate
directories for each user document. The following is a list of documents and
their directory names in the Sphinx OpenMDAO Documentation project: 

* Architecture Document -- ``arch-doc``
* User's Guide -- ``user-guide``
* Guide to the OpenMDAO Standard Library -- ``stdlib-guide``
* Plugin Developer's Guide -- ``plugin-guide``
* Source Documentation -- ``srcdocs``
* Licenses -- ``licenses``
* Quick Reference Guide	for Working in OpenMDAO -- ``quick-ref``

There are several other files and subdirectories in this directory (e.g., images,
generated_images, config.py, Makefile, _build, _static) that you can ignore for now. User
document files (e.g., index.rst, glossary.rst) are in :term:`reStructuredText` (reST) markup
language and always end in ".rst". The subdirectories for the documents in many cases contain a hyphen in their
name. 

1. In the following example, we will add text to an existing file (overview.rst) in the *Guide to
   the OpenMDAO Standard Library,* so you need to go to that document's directory on your branch:

::

%cd /OpenMDAO/dev/<your_working_directory>/T<ticket#>-<branch_name>/docs
%dir      		
%cd stdlib-guide	 	
%dir						
%nedit overview.rst &		

  | NEdit is an XWindows text editor. By using the "&" you're telling it to run in the background. 

.. seealso:: :ref:`Using-NEdit`


2. After you have added the text to the file, save it. 

   Remember that you also have to commit your changes. Always check the status before you commit changes.

::

%bzr status		
%bzr commit -m "<commit comments>"   	

  | When you request a status, Bazaar will display any pending changes on your branch---additions, 			removals,
    modifications, unknown files you forgot to add. When you commit changes, you are required to enter
    comments. If you do not add the comments here, you will go directly into the Vi text editor and must add
    them.

3. If you need to create a new file, just type: 

::

%nedit &

  | This will bring up an empty file. Add the desired text to the file and save it under a new name
    (e.g., *new_file.rst*) in the ``stdlib-guide`` directory.  


.. note::
   When you are using NEdit on a file, you can launch a spell-checking program (ispell) by
   typing ``Alt+b``. When you are finished with ispell, the top of the file you are editing will
   say that the file has been modified, even if you made no spelling changes. So be sure to save your
   file.


4. Now you must add the name of the file to the index.rst in the same directory (``stdlib-guide``),
   so Sphinx knows to grab it and where to place it in the document.  

:: 

%nedit index.rst &

 | There are just a couple of placeholder files in the ``stdlib-guide`` directory (at the time of this
   writing), so add your file after the overview. The list of files would then look like:

::
      
      ======================================
      Guide to the OpenMDAO Standard Library
      ======================================
      
      .. toctree::
         :maxdepth: 4

         overview.rst
         new_file.rst
         changes      

As mentioned previously, it is not necessary to include the ".rst" suffix when adding a file to the
index, but it does no harm. In this case, the document author added ".rst" to the overview file but
not to the changes file. 


.. note::  
   When you finish your documentation (e.g., complete a ticket and are ready to merge), you
   should update the changes.rst file. If you forget for would prefer that the tech writer do
   this, please email the tech writer and include the following: a summary of your changes,the
   ticket number, and the date you merged your changes. The latest changes go at the top of
   the list, for example:
   
::

   **Updates 07/30/09**
       - Updated a number of diagrams to better reflect current source
       - Replaced Variable class diagram with TraitType class diagram
       - Removed API section and instead point to classes and interfaces
         in source code

   **Updates 05/05/09**
       - Made changes to geometry section since last updates
       - Removed release info and date
       - Added "seealso" directive; fixed bullets 

.. index:: documentation; creating
.. index:: documentation; updating
 
Creating a New Document
-----------------------

If you need to create a new document within the OpenMDAO documentation, you must:

* Create a new subdirectory in the ``/docs`` directory 
* Create your new .rst files in that subdirectory, including an index.rst 
* Add the new files to your index
* Add the document (new subdirectory) to the project index, so Sphinx knows about it

There is a main index.rst file for the entire OpenMDAO documentation project. Additionally, each
document subdirectory has its own index.rst file that lists the files in its document (in the
order they are to appear). For example, if you are at the top level on your branch and want to
create a new document called *New Guide* in our OpenMDAO documentation, you would do the
following:   

1. Create the new subdirectory:

::

%cd docs/   	
%mkdir  new-guide   	
%cd new-guide

2. Create your files:
	 
::

%nedit & 

  | This command brings up blank document that you save under the desired name, such as
    *file1.rst*.  Create as many files as you need, including an index.

3. Add the file names to your index:

::

%nedit index.rst &	

  | After adding the names of the files you created to the index.rst, your  file might look
    something like this.  

::

   =========
   New Guide
   =========

   .. toctree::
      :maxdepth: 3

      intro.rst
      file1.rst
      file2.rst 
      changes.rst


.. note:: Be sure to align the file names correctly or your file(s) will not display. You can
   always go to another index and copy its structure. (If you copy the source from this file, be
   sure to start flush left.)

In general use overline and underline only for the title of a document (e.g., in the index file 
of a document). It's easier to use just underline. If you use both, the length of the lines must
match or your build will fail. If you use just underline, and the line is shorter than the text you
are underlining, you will get a warning, but it will build. If you use just underline and it is
longer than the text, Sphinx doesn't seem to mind.


4. Now that you have a subdirectory with files and have added the file names to the
   index.rst for your document, you must add the document to the index.rst for the entire
   OpenMDAO Documentation project. 

   After saving your file, go up a level to the ``docs/`` directory. Then use NEdit to add your new
   document (*New Guide*) to the the project's index.rst: 

::

%cd ../	  	
%nedit index.rst &
	

 | You should see something similar to this:

::

     ====================================
     Welcome to OpenMDAO's Documentation. 
     ====================================

     Contents:

     .. toctree::
	:maxdepth: 2

	arch-doc/index
	user-guide/index
	stdlib-guide/index
	plugin-guide/index
	dev-guide/index
	srcdocs/index
	licenses/index

Add ``new-guide/index`` in the desired location in the list of documents and save the file. 

5. If you have not done so, add any new files to Bazaar using ``bzr add``. 



Building and Displaying Documents
----------------------------------

You must be in the ``buildout`` directory: 

::

%cd /OpenMDAO/dev/<your_working_directory>/T<ticket#>-<branch_name>   
%cd buildout/	
%bin/buildout  	
%bin/docs     	

The ``bin/buildout`` command builds the documentation just as it builds all code files, while
``bin/docs`` will display the documentation in HTML using Firefox. In the above example, it is
assumed that you have built at least once on your branch. If you haven't, you need to run the
following script ``python<version#> isolatedbootstrap.py`` before ``bin/buildout``. Refer to the
section on :ref:`Building-on-your-branch` under *Bazaar Commands.*

	






