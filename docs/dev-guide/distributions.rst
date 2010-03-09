
.. index:: egg
.. index:: pair: package distributions; creating and updating

Creating and Updating Distributions
-----------------------------------

Sometimes the changes you make on your branch will result in the 
modification of an existing package or the creation of a new one. In
either case, you must create a distributable version for each new or
modified package.

For a Python package to be distributable, you have to provide
a ``setup.py`` file that knows how to build, package, and install it. The
``setup.py`` file should be located in the top level directory of the
package. For instructions on how to create distributions, see the setuptools
`documentation <http://peak.telecommunity.com/DevCenter/setuptools>`_.

If a package contains code that must be compiled, you should create
a binary egg distribution for it for Windows.  To create a binary egg 
in the current directory for the current platform, type the following:

::

   python setup.py bdist_egg -d .
   
This will create in the current directory an egg file with a name that contains
information about version of the package, platform, and the Python version,
e.g.,  ``conmin-1.0-py2.5-win32.egg``. 

Regardless of the contents of the package, you should also produce a source 
distribution of it. If your package has compiled code as mentioned above, you
will have to use the ``sdist`` command to generate a source tarball. Assuming
your setup.py file is written correctly, you can generate a source distribution
in the current directory by typing:

::

   python setup.py sdist -d .

A gzipped tar file will be generated with the version number of the package
embedded in the filename, e.g., ``openmdao.recipes-0.1dev.tar.gz``

However, if your package does **not** contain any compiled code, you can 
simply use the ``python setup.py bdist_egg -d .`` command mentioned earlier 
to generate a source egg, which will have a name containing the package version 
information and the Python version, e.g., ``openmdao.recipes-0.1-py2.5.egg``.


- TODO: describe needed metadata in setup.py file
- TODO: describe entry points used by the framework    


.. index: pair: package directory structure; creating

  
