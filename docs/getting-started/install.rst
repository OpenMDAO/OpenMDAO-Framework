
.. _Installing-OpenMDAO:

.. _Installation:

Installation
============

.. note:: 

  On some Windows machines, during the installation process, you might get the error: "The requested
  operation requires elevation." This means that you need to run as an administrator during
  installation. Please follow this link for detailed instructions on solving this error: 
  http://techrena.net/fix-requested-operation-requires-elevation-error/.  

OpenMDAO is installed using a bootstrap script that installs OpenMDAO in a *virtual* Python environment. This script is called
``go-openmdao.py``. 

**Acquire and run the script**

1. Download the installation script.

   Go to the OpenMDAO `downloads <http://openmdao.org/downloads-2/recent/>`_ page and then click on the latest 
   ``go-openmdao.py`` script (listed first). You will see that earlier versions may also
   be downloaded.

   If you are using Internet Explorer, right-click on ``go-openmdao.py`` script; then select **Save
   Target as.** Be sure to save the ``go-openmdao.py`` script to the folder where you want to install
   OpenMDAO. Other browsers may automatically download the file to a specific folder (e.g., a
   Downloads folder), and you will have to move it to the location where you want to install
   OpenMDAO. We highly recommend you pick a folder without any spaces in your path name! For
   instance, on a Windows machine, you could put all your OpenMDAO installations in ``C:\\OpenMDAO``.

   If you are on a Windows machine, even though you have Python installed, the Path environment
   variable will not be configured to point to the Python installation directory. You will have to
   put the correct Python version in the path. For help doing this, please see the following `video
   <http://showmedo.com/videotutorials/video?name=960000&fromSeriesID=96>`_. Please note that this
   video is for Python 2.5, and you want to use **Python 2.7.x**.  

2. Run the script. 

   If you specify a directory name, the virtual environment will be installed there. If you don't
   supply a directory name, it will default to a name based on the version, for example,
   ``openmdao-0.1``. 

   ::

      python go-openmdao.py


   This script will check the version of Python you are running. Currently you
   must be running at least version 2.7. After the script completes successfully, you
   will have installed OpenMDAO. There are just a couple more steps to follow
   before you can start using it.


.. note:: 

  Some steps of the installation process require Internet access. These steps might fail if you're behind 
  a proxy server. If that's the case, you may have to set the ``http_proxy`` environment variable on
  your system for the installation to work. You'll need to find out what your proxy
  address and port number are from your IT department, and then you can set the variable appropriately. 

   

.. _`activate_env`:

**Activate your virtual environment**

Each time before you want to use openmdao, you need to activate its virtual
environment. Activation adds your virtual environment's ``bin`` directory to
the front of your system path in the current shell so that when you type
``python``, you'll get a Python interpreter that's customized for your virtual
environment, giving you access to everything in OpenMDAO.

Navigate into the folder that was just created by your install script. Change
directories so you are in the ``devenv`` directory.

If you are on **Linux,** you must be running bash to activate the virtual environment. If you are
not running bash, start it up by typing:

 :: 

    bash

 Next, if you're on Linux or OS X, type the following, making sure to include the "." in the command:

 ::

    . bin/activate


If you are on **Windows,** type:

 ::

    Scripts\activate



**Run tests to verify valid install**

OpenMDAO has a large test suite which allows you to check and make sure all of the functionality of OpenMDAO will work 
on your system. You can run these tests yourself to double check your installation. 

To run all tests, type the following:

::

   openmdao test
   
If you don't get any errors or failed tests, everything worked. If you did have some problems, the 
`Community Q&A forum <http://openmdao.org/forum/questions>`_ is your next stop!

Now you are ready to start using OpenMDAO.  When you are done using it, you can deactivate the environment
by typing:

::

   deactivate
   

