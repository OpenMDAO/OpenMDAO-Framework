.. index:: documentation; creating

Documenting Changes
===================

You must update the OpenMDAO documentation any time you make a change that will affect the
user or developer.

All documentation is located in the ``docs/`` directory on your branch. There are separate
directories for each user document. The following is a recent list of documents and
their directory names in the Sphinx OpenMDAO Documentation project: 

* Installation Guide  -- ``install-guide``
* User's Guide -- ``user-guide``
* Plugin Developer Guide -- ``plugin-guide``
* Developer's Guide -- ``dev-guide``
* Source Documentation -- ``srcdocs``
* Quick Reference Guide	for Working in OpenMDAO -- ``quick-ref``
* Licenses -- ``licenses``
* Architecture Document -- ``arch-doc``

There are several other files and subdirectories in this directory (e.g., ``images,
generated_images, config.py, Makefile, _build, _static``) that you can ignore for now. User
document files (e.g., ``index.rst, glossary.rst``) are in :term:`reStructuredText` (reST)
markup language and always end in ``.rst``. The subdirectories for the documents in many cases
contain a hyphen in their name. 

Updating an Existing Document
-----------------------------

* If you are merely changing a file in an existing document, (e.g., ``intro.rst`` in
  the *Developer's Guide*), from top level of your branch, go to ``docs/dev-guide``. 
  
* Use the editor of your choice to open ``intro.rst`` and make your changes. 

* Save the file and then commit your changes. 

  Your doc changes will be incorporated when your
  branch is merged.

If you need to create a new file in an existing document (e.g., the *Developer's Guide*), do
the following:

* Create a file in the ``dev-guide`` directory with a text editor of your
  choice and save it with an appropriate name, in this example, ``new_file.rst``. 
  
* You must add the name of the new file to the ``index.rst`` in the same directory so that Sphinx
  knows to grab it and where to place it in the document. Open ``index.rst`` and add your
  file (in the example below, after ``intro.rst``). 
  
  The index should look similar to this:

::
      
      =================
      Developer's Guide
      =================
      
      .. toctree::
         :maxdepth: 4

         intro.rst
	 new_file.rst
	 accessing
   	 working
   	 testing
   	 guidelines
   	 distributions
  
        
It is not necessary to include the ``.rst`` suffix when adding a file to the index, but it does
no harm. Save the file.

* Be sure to use the ``bzr add`` command to add the file to the repository. Also, remember to
  commit your changes.


.. index:: documentation; creating
.. index:: documentation; updating
 
Creating a New Document
-----------------------

There is a main ``index.rst`` file for the entire OpenMDAO documentation project. Additionally,
each document subdirectory has its own ``index.rst`` that lists the files in its document (in
the order they should appear). For example, if you are at the top level on your branch and want
to create a new document called *New Guide* in our OpenMDAO documentation, you would do the
following:   

* Create a new subdirectory in the ``docs`` directory 
* Create your new files in that subdirectory, including an ``index.rst`` 
* Add the new file(s) to your index
* Add the document (new subdirectory) to the project index, so Sphinx knows about it
* Add the new directory and files to the bzr repository

See the example that follows.

* From the top level branch directory, create the new subdirectory and then go there:

  ::

    cd docs/   	
    mkdir  new-guide   	
    cd new-guide

* Create your files using an editor of your choice, e.g., ``file1.rst`` and ``file2.rst``.
	 

* Add the file names to your index. 
  After adding the names of the files you created to the ``index.rst``, your  file might look
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


.. note:: Be sure to align the file names correctly or your file(s) will not display. 


* Now that you have a subdirectory with files and have added the file names to the
  ``index.rst`` for your document, you must add the document to the ``index.rst`` for the
  entire OpenMDAO documentation project. 

  Remember, you are in ``new-guide`` directory. After saving your file, go up one level to the
  ``docs`` directory. You should see something similar to this:

::

     ====================== 
     OpenMDAO Documentation 
     ====================== 

     Contents:

     .. toctree::
	:maxdepth: 2

	install-guide/index
	user-guide/index
	plugin-guide/index
	dev-guide/index
	srcdocs/index
	quick-ref/index
	licenses/index
	arch-doc/index 


Use your text editor to add ``new-guide/index`` to the desired location in the project's
``index.rst``. Then save the file. 

*  Use ``bzr add`` to add the new directory and files to the repository. Remember to commit your
   changes when ready.



Building and Displaying Documents
----------------------------------

Your openmdao virtual development environment has scripts for building the sphinx documentation as
well as viewing it. The following example assumes that you have already created your virtual
environment on your branch. If you haven't, you must run ``python2.6 go-openmdao-dev.py`` from
the top directory in your branch repository. 

::

  cd devenv			 (Takes you to your dev environment)
  bin/openmdao_build_docs        (Builds the Sphinx documentation)
  bin/openmdao_docs              (Displays the documentation in HTML using the default browser) 

For more information, refer to the section on :ref:`Building-OpenMDAO-(openmdao)` under *Bazaar
Commands*. 







