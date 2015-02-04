
.. _Installing-OpenMDAO:

.. _VirtualEnv Installation:

VirtualEnv Installation
=======================

.. note::

  On some Windows machines, during the installation process, you might get the error: "The requested
  operation requires elevation." This means that you need to run as an administrator during
  installation. Please follow this link for detailed instructions on solving this error:
  http://techrena.net/fix-requested-operation-requires-elevation-error/.

OpenMDAO is installed using a bootstrap script that installs OpenMDAO in a *virtual* Python environment. This script is called
``go-openmdao.py``.

**Acquire and run the script**

1. Download the installation script.

   Go to the OpenMDAO `downloads <http://openmdao.org/downloads/recent/>`_ page and then click on the latest
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

Navigate into the folder that was just created by your install script. It will have a name
of the form ``openmdao-X.X.X``.

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

.. _Anaconda Installation:
Anaconda Installation
=====================

To perform an Anaconda Python installation, see the `Anaconda Installation
Instructions <http://docs.continuum.io/anaconda/install.html>`_ Once you have anaconda
installed, you need to get your Anaconda configuration ready for OpenMDAO.  A user must
be connected to the Internet for the following installation to work.

**Conda configuration**

In your home directory, there will be a .condarc file that stores your preferences and settings
for your Anaconda installation.  If you're comfortable with doing so, you can edit the .condarc
file directly.  If not, the following conda config commands will make the changes for you.

Add our channel to your config.  This makes it so that while installing, conda will search
for required packages using OpenMDAO's packages on the binstar website.
::

   conda config --add channels https://conda.binstar.org/OpenMDAO

In order to see what channel conda expects to download from, during an installation
(i.e. to make sure packages are expected to come from OpenMDAO's channel.)

::

   conda config --set show_channel_urls TRUE

This is the equivalent of manually editing ~/.condarc to have the line:

show_channel_urls: True

**Development Version Installation**

To get a dev build going, you'll need to make sure you have git installed, so
that you can grab the code.  Once you have git, these commands should get you the
latest dev branch:

Mac/Linux:

::

  git clone https://github.com/OpenMDAO/OpenMDAO-Framework.git
  cd OpenMDAO-Framework
  bash conda-openmdao-dev.sh
  source activate openmdao
  openmdao test

Windows:

::

  git clone https://github.com/OpenMDAO/OpenMDAO-Framework.git
  cd OpenMDAO-Framework
  conda-openmdao-dev.bat
  activate openmdao
  openmdao test

**Release Version Installation**

As a user, with nothing openmdao, follow the configuration steps above.  To install
OpenMDAO's latest release into your root Anaconda environment, one should need to type:

::

  conda install openmdao

OpenMDAO has a lot of dependencies, and so perhaps you want to put OpenMDAO into its own conda environment,
and not in your root env.  Create a new conda env to hold the install, (let's say, for
example, for 0.12.0 release)

::

  conda create --name openmdao-0.12.0 python

Then, to install version 0.12.0 into that newly-created env:

::

  conda install --name openmdao-0.12.0 openmdao   #gets latest release

  conda install --name openmdao-0.12.0 openmdao==0.12.0  #gets specific release


If you choose not to add us to your channels, you have to install with the -c channel option

::

  conda install -c https://conda.binstar.org/openmdao --name openmdao-0.12.0 openmdao=0.12.0




.. _Site-Wide VirtualEnv Installation:

Site-Wide VirtualEnv Installation
=================================

At some sites it can be convenient to have a standard OpenMDAO configuration
available to all users.  This would include the base OpenMDAO installation,
possibly a collection of pre-installed plugins, site-specific configuration
files, etc.  Using the ``go-openmdao.py`` script of an activated environment,
you can create a zip file containing everything needed to install an OpenMDAO
configuration on a user's system.

To create the zip file, type this from within an activated environment
(in this case version 0.12.0 on a Linux machine):

::

    python go-openmdao.py --relocatable

This will create ``openmdao-0.12.0-linux-x86_64.zip``.
Now, to install at some other location, you need to extract all the files from
the created zip file and execute ``script-fixup.py``, which is part of the
generated zip file:

::

    unzip openmdao-0.12.0-linux-x86_64.zip
    cd openmdao-0.12.0
    python script-fixup.py
