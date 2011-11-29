
.. _Installing-OpenMDAO:

.. _Installation:

Installation
============

To avoid version conflicts with system level Python packages, OpenMDAO is best installed using a
bootstrap script that installs OpenMDAO in a *virtual* Python environment. This script is called
``go-openmdao.py``. 

**Acquire and run the script**

1. Download the installation script.

   Go to the OpenMDAO `downloads <http://openmdao.org/downloads>`_ page and then click on the latest 
   ``go-openmdao.py`` script (listed first). You will see that earlier versions may also
   be downloaded.

   If you are using Internet Explorer, right-click on ``go-openmdao.py`` script; then select "Save
   Target as." Be sure to save the ``go-openmdao.py`` script to the folder where you want to install
   OpenMDAO. Other browsers may automatically download the file to a specific folder (e.g., a
   Downloads folder), and you will have to move it to the location where you want to install
   OpenMDAO. We highly recommend you pick a folder without any spaces in your path name! For
   instance, on a Windows machine, you could put all your OpenMDAO installations in ``C:\\OpenMDAO``.

   If you are on a Windows machine, even though you have Python installed, the Path environment
   variable will not be configured to point to the Python installation directory. You will have to
   put the correct Python version in the path. For help doing this, please see the following `video
   <http://showmedo.com/videotutorials/video?name=960000&fromSeriesID=96>`_. Please note that this
   video is for Python 2.5, and you want to use **Python 2.6.x or 2.7.x**.  

2. Run the script. 

   If you specify a directory name, the virtual environment will be installed there. If you don't
   supply a directory name, it will default to a name based on the version, for example,
   ``openmdao-0.1``. 

   ::

      python go-openmdao.py


   This script will check the version of Python you are running. Currently you
   must be running version 2.6. After the script completes successfully, you
   will have installed OpenMDAO. There are just a couple more steps to follow
   before you can start using it.
   
   You can also specify additional required packages by passing *-r* arguments 
   to go-openmdao.py, e.g.,
   
   ::
   
      python go-openmdao.py -r myplugin -r myotherplugin==1.3
      

**Activate your virtual environment**

Each time before you want to use openmdao, you need to activate its virtual
environment. Activation adds your virtual environment's ``bin`` directory to
the front of your system path in the current shell so that when you type
``python``, you'll get a Python interpreter that's customized for your virtual
environment, giving you access to everything in OpenMDAO.

Navigate into the folder that was just created by your install script.

If you are on **Linux or Mac OS X,** you must be running bash to
activate the virtual environment. If you are not running bash, start it up
by typing:

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
on your system. You can run these tests yourself to double check your installation. It takes only a few 
minutes to run them all. 

To run all tests, type the following:

::

   openmdao_test
   
If you don't get any errors or failed tests, everything worked. If you did have some problems, the 
`Community Q&A forum <http://openmdao.org/forum/questions>`_ is your next stop!

Now you are ready to start using OpenMDAO.  When you are done using it, you can deactivate the environment
by typing:

::

   deactivate
   

