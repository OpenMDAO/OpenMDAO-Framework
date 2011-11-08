.. index:: OpenMDAO-Plugins

.. _installing-plugins-from-the-official-openmdao-plugin-repository:

Installing Plugins from the Official OpenMDAO Plugin Repository
===============================================================

The OpenMDAO project also hosts a set of official plugins in a GitHub top-level
organization called **OpenMDAO-Plugins**. You can visit this site at https://github.com/OpenMDAO-Plugins/

These plugins are all open source, although some of them are component wraps of proprietary or
otherwise-restricted codes (e.g., Nastran) that you will have to obtain separately. In most cases,
the plugin only contains the OpenMDAO wrapper code.

The ``plugin_install`` script takes some additional arguments that allow you to interact with
the plugin repositories in OpenMDAO-Plugins so that you can list and install easilly. To
list the components you currently have installed, type the following at the OS prompt (always
in an activated OpenMDAO environment):

::

    plugin_install --list
    
You should get a response that looks something like this.

::
    
    Installed plugins
    -------------------
    (Note: surrogate generators currently don't show up in this list.)

    flops_wrapper 0.6
    ipoptdriver 0.9
    nastranwrapper 0.10
    overflow_wrapper 0.4
    pyopt_driver 0.3

This shows both the plugin name and the versions in a simple list. If you have never installed
a plugin, this list will be empty.
    
A shortcut for the ``--list`` argument is ``-l``:
::

    plugin_install -l
    
You can also grab a list of the plugins that are available on the official site:

::

    plugin_install --github --list

This returns a response that looks something like this:
    
::

    Available plugins
    ===================

       adpac_wrapper -- Component wrapper for ADPAC (Advanced Ducted Propfan Analysis Code)
       flops_wrapper -- Component wrapper for FLOPS
         ipoptdriver -- Driver wrapper for the IPOPT optimization code
      nastranwrapper -- Component wrapper for MSC Nastran
          neural_net -- A neural net surrogate model generator based on the FFnet library
    overflow_wrapper -- Component wrapper for OVERFLOW (OVERset grid FLOW solver)
          pdcyl_comp -- Component wrapper for PDCYL
        pyopt_driver -- Driver wrapper for the open-source optimization package pyOpt
         vsp_wrapper -- Component wrapper for VSP (Vehicle Sketch Pad)

Note that https must be available in order to communicate with the GitHub repository. If you
don't have a network connection, then these features will be unavailable. There is also a ``-g``
shortcut for the argument ``--github``:

::

    plugin_install -g -l

To install one of the plugins listed above, simply type:

::

    plugin_install -g pyopt_driver
    
You will see some text that looks something like this.

::

    https://nodeload.github.com/OpenMDAO-Plugins/pyopt_driver/tarball/0.4
    Downloading https://nodeload.github.com/OpenMDAO-Plugins/pyopt_driver/tarball/0.4
    Processing 0.4
    Running OpenMDAO-Plugins-pyopt_driver-c18e0c5/setup.py -q bdist_egg --dist-dir /tmp/easy_install-_OgLCm/OpenMDAO-Plugins-pyopt_driver-c18e0c5/egg-dist-tmp-P0HnUe
    warning: no directories found matching 'src/pyopt_driver/sphinx_build/html'
    Removing pyopt-driver 0.3 from easy-install.pth file
    Adding pyopt-driver 0.4 to easy-install.pth file

    Installed /OpenMDAO/dev/ktmoore1/OpenMDAO-Framework/devenv/lib/python2.6/site-packages/pyopt_driver-0.4-py2.6.egg
    Processing dependencies for pyopt-driver==0.4
    Finished processing dependencies for pyopt-driver==0.4

    The 'activate' file has been updated with new values added to LD_LIBRARY_PATH
    You must deactivate and reactivate your virtual environment for the
    changes to take effect
    
The message at the end indicates that you need to deactivate and reactivate for some
symbols to be added to your environment. Notice that in this case, we've actually
used ``plugin_install`` to upgrade the version of ``pyopt_driver`` from 0.3 to 0.4.

If you want a specific version of a plugin, you can use the ``easy_install`` syntax to specify
one:

::

  plugin_install -g pyopt_driver==0.3
    
This will give you ``pyopt_driver`` 0.3, not 0.4.

Here is a quick reference for these commands:

.. index:: plugin_install quick command reference
    
**Quick Command Reference for ``plugin_install``**


==================================   =================================
Action                                Command
==================================   =================================
List all installed plugins           ``plugin_install -l``
----------------------------------   ---------------------------------
List all available plugins           ``plugin_install -g -l``
----------------------------------   ---------------------------------
Install plugin foo                   ``plugin_install -g foo``
----------------------------------   ---------------------------------
Install version 0.3 of plugin foo    ``plugin_install -g foo==0.3``
==================================   =================================


