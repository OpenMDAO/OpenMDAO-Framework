
.. index:: plugin install
.. _plugin-install:

Installing an OpenMDAO Plugin
==============================

If you run ``plugin install`` from the top directory of your plugin
distribution, it will install your plugin as a *develop* egg, meaning that it
places a link to your distribution on the Python path so that you can make
changes to your plugin and test it in the environment without having to keep
reinstalling it.

If you have a distrbution tar or zip file, created either by using ``plugin makedist``
or by running ``setup.py`` directly, you can install your plugin into an OpenMDAO virtual
environment by running ``plugin install`` and passing it the name of the file, for
example:

::

    plugin install myplugin-0.5.tar.gz


Using this command will install the distribution into the ``site-packages`` directory
of your OpenMDAO virtual environment.

If you want to install a plugin distribution from a remote server, you would do it like
this:

::

    plugin install [-f <find_links_url>] <distrib_requirement>


where ``find_links_url`` is the url for a ``find_links`` server and ``distrib_reqirement`` is
a requirement string in the same form as you would pass to ``easy_install`` or ``pip``.
For example, ``myplugin``, ``myplugin==0.5``, and ``myplugin>=0.3`` are all valid requirement
strings.  If there is no version specified in the ``distrib_requirement``, then the latest
version compatible with the current platform will be installed.


If you are trying to install a plugin that exists in a public repository on ``github.com``,
there's a way to do that as well, using the ``--github [github owner]`` command line option.
The GitHub owner "OpenMDAO-Plugins" is our own special account that contains the official
OpenMDAO plugins.  If you don't specify an owner, we default that argument to
OpenMDAO-Plugins. When used in conjunction with the ``--all`` command, plugin install will try
to install all plugins the owner has available.  Here are some examples.

  Install ``pyopt_driver`` from GitHub, by default OpenMDAO-Plugins:

    ::

       plugin install pyopt_driver --github

  Install every OpenMDAO plugin listed under OpenMDAO-Plugins owner:

    ::

       plugin install --github --all


  Install plugin named *generic-plugin* from owner JohnDoe:

    ::

       plugin install generic-plugin --github JohnDoe

  Install all plugins from owner JohnDoe. **Warning:** The system will attempt to get every public repository this person owns that has the proper setup mentioned above.

    ::

       plugin install -all --github JohnDoe


  Install a plugin of a certain version number (in this example, v 0.8 of the CADRE plugin) from GitHub:

    ::

       plugin install CADRE==0.8  --github

  If you want the latest version of a plugin, simply type the plugin name:

    ::

       plugin install CADRE --github

The last command will get you the latest tagged release (in this case, from owner OpenMDAO-Plugins,
which is the default). If a repository has never been tagged, however, ``plugin install`` will
simply get the latest version of the default branch of that repository, which may not be stable.


Sharing Plugins
=================
   
You can make your plugin available to others in a number of ways -- you can simply email your
distribution to others or give it to them on a thumb drive, CD, etc.; you can put your plugin in a public
repository on GitHub; or you can place your distribution on a file server that users can
access. 

The ``plugin install`` command allows you to download and install Python distributions
from remote web servers using the ``-f`` or ``--findlinks`` option. For example, if there were a distribution called
*MyDist* on the ``openmdao.org`` server and you wanted to grab the newest version
of it, you could ``plugin install`` it into your activated OpenMDAO virtual
environment as follows:

::

    plugin install MyDist -f http://openmdao.org/dists 

If you want to distribute your plugin to the whole world but don't happen to
have your own public server, you can put your plugin up on the 
`Python Package Index`__ (PyPI), which is also known as the *Cheeseshop*. 
PyPI is the default package index for ``plugin install``, so the command

.. __: https://pypi.python.org/pypi


::

    plugin install MyDist
    
will attempt to download the MyDist distribution from PyPI. See this `link`__
for more information about how to register your plugin with PyPI.

.. __: https://docs.python.org/2/distutils/packageindex.html
