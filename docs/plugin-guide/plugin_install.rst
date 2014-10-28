
.. index:: plugin install


*Installing an OpenMDAO Plugin*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
    

which will install the distribution into the ``site-packages`` directory
of your OpenMDAO virtual environment.

Finally, if you want to install a plugin distribution from a remote server, it
would look like:

::

    plugin install [-f <find_links_url>] <distrib_requirement>
    

where ``find_links_url`` is the url for a ``find_links`` server and ``distrib_reqirement`` is
a requirement string in the same form as you would pass to ``easy_install`` or ``pip``.
For example, ``myplugin``, ``myplugin==0.5``, and ``myplugin>=0.3`` are all valid requirement
strings.  If there is no version specifier in the ``distrib_requirement``, then the latest
version compatible with the current platform will be installed.


If you are trying to install a plugin that exists in a public repository on github.com, 
there's a way to do that as well, using the --github [github owner] command line option.
The github owner "OpenMDAO-Plugins" is our own special account that contains the official
OpenMDAO plugins.  If you don't specify an owner, we default to OpenMDAO-Plugins.  When used
in conjunction with the --all command, plugin install will try to install all plugins the
owner has available.  Here are some examples.

Install pyopt_driver from github, by default OpenMDAO-Plugins

::

    plugin install pyopt_driver --github      
    
Install every OpenMDAO plugin listed under OpenMDAO-Plugins owner

::

    plugin install --github --all

    
Install plugin named generic-plugin from owner JohnDoe

::

    plugin install generic-plugin --github JohnDoe
    
Install all plugins from owner JohnDoe.  Warning: This will attempt to get every public 
repository this owner owns that has the proper setup mentioned above.
    
::

    plugin install -all --github JohnDoe


Install a plugin of a certain version number from github.

::

    plugin install CADRE==0.8  --github

If one wants the latest version of a plugin, trying simply:

::

    plugin install CADRE --github

would get the user the latest tagged release.  If a repository has never been tagged, however,
'plugin install' will simply go get the latest version of the default branch of that repository,
which may not be guaranteed to be stable.



*Making Your Plugin Available to Others*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
You can make your plugin available to others in a number of ways, from simply emailing your distribution
to others or giving it to them on a thumb drive, CD, etc., or placing your
distribution on a file server that they can access. As mentioned above,
``plugin install`` allows you to download and install Python distributions
from remote web servers. For example, if there were a distribution called
*MyDist* on the ``openmdao.org`` server and you wanted to grab the newest version
of it, you could ``plugin install`` it into your activated OpenMDAO virtual
environment as follows:

::

    plugin install -f http://openmdao.org/dists MyDist

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
