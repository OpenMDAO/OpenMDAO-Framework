.. index:: documentation; creating

Updating OpenMDAO Documents
===========================

You must update the OpenMDAO documentation any time you make a change that will affect users or
developers. All documentation is located in the ``docs/`` directory on your branch. The
directories (e.g., ``dev-guide, mdao, tutorials``) are easily distinguished from files because they
do not  have a file extension (e.g., ``.rst, .png, .py``). User document files are created in 
:term:`reStructuredText` (reST) markup language and always end in ``.rst``. 

Updating an Existing Document
-----------------------------

If you are merely changing a file in an existing document, (in this example, ``overview.rst`` in the
`Plugin Developer Guide`), do the following:

* From the top level of your branch, change directory to ``docs/plugin-guide``. 

* Use the editor of your choice to open ``overview.rst`` and make your changes. 

* Save your changes, use the ``git add`` command to stage the file for committing, and then commit
  the changes. Refer to the :ref:`Code Contribution Tutorial & Developer Checklist <dev-checklist>`
  for these and other steps in the process of changing the docs and/or code. 

  Your doc changes will be incorporated when your branch is merged.

If you need to create a new file in an existing document (e.g., the *Plugin Developer Guide*), do
the following:

* Create a file in the ``plugin-guide`` directory with a text editor of your choice and save it with
  an appropriate name, in this example, ``new_file.rst``. 

* You must add the name of the new file to the ``index.rst`` in the same directory so that Sphinx
  knows to grab it and where to place it in the document. So open ``index.rst`` and add your
  file (below, it is the last file). 
  
  The index should then look similar to this:

  ::
      
       .. _Plugin-Developer-Guide:

       ======================== 
       Plugin Developer Guide 
       ======================== 

       .. toctree::
       :maxdepth: 2
   
       overview
       plugin_creation
       module_plugin
       variable_plugin
       extension_plugin
       filewrapper_plugin
       new_file
         
        
  Note that it is not necessary to include the ``.rst`` suffix when adding a file to the index, but
  it does no harm. 

* Save the file.

* Be sure to add the file and commit your changes. (Again, please refer to the Developer Checklist.)


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
* Add the new files to your index
* Add the document (new subdirectory) to the project index, so Sphinx knows about it
* Add the new directory and files to the Git repository

See the example that follows.

* From the top level branch directory, create the new subdirectory and then go there:

  ::

    cd docs/   	
    mkdir new-guide   	
    cd new-guide

* Create your files using an editor of your choice, e.g., ``index.rst, file1.rst`` and ``file2.rst``.
	 

* Add the file names to your index. Your ``index.rst`` might then look something like this.  

  ::

     =========
     New Guide
     =========

     .. toctree::
	:maxdepth: 2

	intro.rst
	file1.rst
	file2.rst 


  .. note:: Be sure to align the file names correctly so the files display. 


* Now that you have a subdirectory with files and have added the file names to the
  ``index.rst`` for your document, you must add the document to the ``index.rst`` for the
  entire OpenMDAO project. 

  Remember, you are in the ``new-guide`` directory. After saving your file, go up one level to the
  ``docs`` directory. 

* Use your text editor to add ``new-guide/index`` to the desired location in the project's
  ``index.rst`` and then save the file. 

* Use ``git add`` to add the new directory and files to the repository. Remember to commit
  your changes when ready. Follow the :ref:`Code Contribution Tutorial & Developer Checklist
  <dev-checklist>` for testing, merging, etc.


Building and Displaying Documents
----------------------------------

Your openmdao virtual development environment has scripts for building and displaying Sphinx
documentation. Once you have activated the virtual environment on your branch, you can run the
scripts from anywhere on the branch. 

::

  openmdao build_docs        (Builds the Sphinx documentation)
  openmdao docs              (Displays the documentation in HTML using the default browser) 












