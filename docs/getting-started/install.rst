.. _Installing-OpenMDAO:

.. _Installation:

.. note::

   Please read the preceding section on :ref:`System Requirements <System-Requirements>`  before attempting any
   of the below installation methods. If you don't have the proper prerequisites, or are attempting an installation on
   an unsupported system, (e.g. 64-bit Python on Windows) you will encounter difficulties that could be avoided
   with a little preparation.


.. _VirtualEnv Installation:

VirtualEnv Installation
=======================

.. note::

  On some Windows machines, during the installation process, you might get the error: "The requested
  operation requires elevation." This means that you need to run as an administrator during
  installation. Please follow this link for detailed instructions on solving this error:
  http://techrena.net/fix-requested-operation-requires-elevation-error/.

The most common way that OpenMDAO is installed is by using a bootstrap script that installs OpenMDAO in a *virtual*
Python environment. This script is called

``go-openmdao-X.Y.Z.py``.

(where X.Y.Z represents a version number, e.g. ``go-openmdao-0.12.0.py``)

**Acquire and run the script**

1. Download the installation script.

   Go to the OpenMDAO `downloads <http://openmdao.org/downloads/recent/>`_ page and then click on the latest
   ``go-openmdao-X.Y.Z.py`` script (listed first). You will see that earlier versions may also
   be downloaded.

   If you are using Internet Explorer, right-click on ``go-openmdao-X.Y.Z.py`` script; then select **Save
   Target As.** Be sure to save the ``go-openmdao-X.Y.Z.py`` script to the folder where you want to install
   OpenMDAO. Other browsers may automatically download the file to a specific folder (e.g., a
   Downloads folder), and you will have to move it to the location where you want to install
   OpenMDAO. We highly recommend you pick a folder without any spaces in your path name! For
   instance, on a Windows machine, you could put all your OpenMDAO installations in ``C:\\OpenMDAO``.

   If you are on a Windows machine, even though you have Python installed, the Path environment
   variable will not be configured to point to the Python installation directory. You will have to
   put the correct Python version in the path. For help doing this, please see the following `video
   <http://showmedo.com/videotutorials/video?name=960000&fromSeriesID=96>`_. Please note that this
   video is for Python 2.5, and you want to use **Python 2.7.5+**.

2. Run the script.

   If you specify a directory name, the virtual environment will be installed there. If you don't
   supply a directory name, the install directory will default to a name based on the X.Y.Z version, for example,
   ``openmdao-0.12.0``.

   ::

      python go-openmdao-0.12.0.py


   This script will check the version of Python you are running. Currently you
   must be running at least version 2.7.5. After the script completes successfully, you
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
of the form ``openmdao-X.Y.Z``.

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

OpenMDAO has a large test suite which allows you to check and make sure all of the functionality of OpenMDAO will work on your system. You can run these tests yourself to double check your installation.

To run all tests, type the following:

::

   openmdao test

If you don't get any errors or failed tests, everything worked. If you did have some problems, the
`Community Q&A forum <http://openmdao.org/forum/questions>`_ is your next stop!

Now you are ready to start using OpenMDAO.  When you are done using it, you can deactivate the environment
by typing:

::

   deactivate


.. _Site-Wide VirtualEnv Installation:

Site-Wide VirtualEnv Installation
=================================

At some sites it can be convenient to have a standard OpenMDAO configuration
available to all users.  This would include the base OpenMDAO installation,
possibly a collection of pre-installed plugins, site-specific configuration
files, etc.  Using the ``go-openmdao-X.Y.Z.py`` script of an activated environment,
you can create a zip file containing everything needed to install an OpenMDAO
configuration on a user's system.

To create the zip file, type the following from within an activated environment
(in this case, version 0.12.0):

::

    python go-openmdao-0.12.0.py --relocatable

This will create ``openmdao-0.12.0-linux-x86_64.zip`` on linux machines and
``openmdao-0.12.0-win32.zip`` on Windows machines.

Move the file to the desired machine.  To install at some other location, you need to
simply extract all the files from the created zip file and execute ``script-fixup.py``,
which is a file that is inserted into of the generated zip file at the time of its creation.

::

    unzip openmdao-0.12.0-linux-x86_64.zip
    cd openmdao-0.12.0
    python script-fixup.py


.. _`Anaconda-Installation`:

Anaconda Installation
=====================

Many users in the scientific community use Anaconda, a free enterprise-ready Python distribution for large-scale
data processing, predictive analytics, and scientific computing. If you're one of our many OpenMDAO users who also
use Anaconda, we have a way to install it for you.

Please note that you must be connected to the Internet for the following installation to work.

To perform an Anaconda Python installation of OpenMDAO, you'll need Anaconda or Miniconda. To get either
of them, see the `Anaconda Installation Instructions <http://docs.continuum.io/anaconda/install.html>`_.
Once you have Anaconda installed, you'll need to get your Anaconda configuration ready for OpenMDAO.

**Conda Configuration**

Before we install OpenMDAO, we need to make a couple of minor configuration changes to Anaconda. In your home
directory, there is a ``.condarc`` file that stores your preferences and the settings for your Anaconda
installation. The following conda config commands will make the changes needed, but the net result is to add
settings to your ``.condarc`` file. If you're comfortable doing so, you can edit the ``.condarc`` file
directly to change these settings.

1. Add OpenMDAO's channel to your config. Adding our channel ensures that during installation, conda will search for
   required packages using OpenMDAO packages that are hosted on the `Binstar website <https://binstar.org/openmdao>`_.

   ::

     conda config --add channels https://conda.binstar.org/OpenMDAO


2. This command is optional but helps provide peace of mind. To be certain from which channel conda expects to
   download during an installation, you can show channel URLs (to make sure a particular package is expected to
   download from OpenMDAO's channel).

   ::

     conda config --set show_channel_urls TRUE


Alternatively, to make both these changes, you can edit your ``~/.condarc`` file to include these lines:

::

  show_channel_urls: True

  channels:
    - https://conda.binstar.org/OpenMDAO
    - defaults

**Conda Environments**

Anaconda environments are just like directories that contain particular versions of packages. These can be located anywhere, but if they are within the Anaconda installation directory, conda will know about them.  To list the conda
environments that you will have after a new install, type the following:

::

  $ conda info -e
  # conda environments:
  #
  root                  *  /Users/<username>/anaconda

By default, you're in your root environment.  To create another environment, we would use the conda `create` command. Later on, we'll explore
how to create a new conda environment to hold your openmdao installation. More information about conda environments is
available at `Continuum Analytics' website <http://www.continuum.io/blog/conda>`_.

**Installation**

You'll still need to follow the configuration step above to add the OpenMDAO channel. To install OpenMDAO's latest release into your root Anaconda environment, only one command is needed:

::

  conda install openmdao

However, OpenMDAO has a lot of dependencies, so you may wish to put OpenMDAO into its own secluded conda environment. Let's say, for example, that for the 0.12.0 release we call the environment ``openmdao-0.12.0`` (but keep in mind that we could call it anything--the name is not magical). To create the conda environment and install openmdao into the created environment type:

::

  conda create --name openmdao-0.12.0 openmdao

To activate the environment:

OS X and Linux ::

  source activate openmdao-0.12.0

Windows ::

  activate openmdao-0.12.0

Once the environment is activated, you can run the OpenMDAO test suite to confirm successful installation. To do so, type:

::

  openmdao test

Once you have completed installation and testing, you're ready to use OpenMDAO.  When you're finished using the environment that you've created, you can leave it by activating another environment or simply typing:

OS X and Linux ::

  source deactivate

Windows ::

  deactivate
