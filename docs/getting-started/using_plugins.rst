.. index:: OpenMDAO-Plugins

.. _using-plugins-from-the-official-openmdao-plugin-repository:

Using Plugins from the Official OpenMDAO Plugin Repository
===============================================================

What Official OpenMDAO Plugins Are Available?
---------------------------------------------

The OpenMDAO project also hosts a set of official plugins in a GitHub
top-level organization called **OpenMDAO-Plugins**. You can visit this site at
https://github.com/OpenMDAO-Plugins/ .

These plugins are all open source, although some of them are component wraps
of proprietary or otherwise-restricted codes (e.g., Nastran) that you will
have to obtain separately. 

From an activated OpenMDAO environment, you can also display a list of 
the plugin distributions that are available from the OpenMDAO-Plugins
organization on GitHub as follows:

::

    plugin list --github

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


What Plugins Are Installed?
---------------------------

To list all OpenMDAO plugins you currently have installed, type the
following at the operating system prompt (always in an activated OpenMDAO environment):

::

    plugin list
    
You should get a response that looks something like this:

::
    
    Installed plugins
    -------------------

    flops_wrapper.flops_wrapper.FlopsWrapper 0.6
    openmdao.lib.components.broadcaster.Broadcaster 0.2.1
    openmdao.lib.components.expected_improvement.ExpectedImprovement 0.2.1
    openmdao.lib.components.expected_improvement_multiobj.MultiObjExpectedImprovement 0.2.1
    openmdao.lib.components.external_code.ExternalCode 0.2.1
    openmdao.lib.components.metamodel.MetaModel 0.2.1
    openmdao.lib.components.mux.DeMux 0.2.1
    openmdao.lib.components.mux.Mux 0.2.1
    openmdao.lib.components.pareto_filter.ParetoFilter 0.2.1
    openmdao.lib.differentiators.finite_difference.FiniteDifference 0.2.1
    openmdao.lib.drivers.broydensolver.BroydenSolver 0.2.1
    openmdao.lib.drivers.caseiterdriver.CaseIteratorDriver 0.2.1
    openmdao.lib.drivers.conmindriver.CONMINdriver 0.2.1
    openmdao.lib.drivers.doedriver.DOEdriver 0.2.1
    openmdao.lib.drivers.genetic.Genetic 0.2.1
    openmdao.lib.drivers.iterate.FixedPointIterator 0.2.1
    openmdao.lib.drivers.iterate.IterateUntil 0.2.1
    openmdao.lib.drivers.newsumtdriver.NEWSUMTdriver 0.2.1
    openmdao.lib.drivers.sensitivity.SensitivityDriver 0.2.1
    openmdao.lib.drivers.simplecid.SimpleCaseIterDriver 0.2.1
    openmdao.lib.surrogatemodels.kriging_surrogate 0.2.1
    openmdao.lib.surrogatemodels.logistic_regression 0.2.1
    openmdao.lib.surrogatemodels.nn_surrogate 0.2.1
    openmdao.main.assembly.Assembly 0.2.1
    openmdao.main.component_with_derivatives.ComponentWithDerivatives 0.2.1
    openmdao.main.driver_uses_derivatives.DriverUsesDerivatives 0.2.1
    openmdao.main.problem_formulation.ArchitectureAssembly 0.2.1
    openmdao.test.execcomp.ExecComp 0.2.1
    overflow_wrapper.overflow_wrapper.OverFlowWrapper 0.4


This shows both the plugin name and the versions.

You'll notice that the output includes a lot of things that are installed as part 
of the standard library. The default behavior of ``plugin list`` is to show every 
installed plugin whether it is built into the OpenMDAO distribution or has been 
added to the OpenMDAO environment from some other source.  To show only plugins 
that are *not* built into the OpenMDAO distribution, use the ``--external`` 
option. A shortcut for ``--external`` is ``-e``.  For example:

::

    plugin list -e

To show only the builtin plugins that come with OpenMDAO, use the ``--builtin`` option or its
shortcut, ``-b``.


If you only want to list plugins belonging to a specific group, 
use the ``-g`` option.  For example, to see only plugins
that are drivers, do the following:

::

    plugin list -g driver

The possible group names are : 'caserecorder', 'caseiterator', 
'optproblem', 'driver', 'surrogatemodel', 'doegenerator', 
'component', 'architecture', 'container', 'differentiator', 'variable'.


Installing Plugins
------------------

To install one of the plugins listed above, simply type, from any
directory, but within an activated OpenMDAO environment :

::

    plugin install --github pyopt_driver
    
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
used ``plugin install`` to upgrade the version of ``pyopt_driver`` from 0.3 to 0.4.

If you want a specific version of a plugin, you can use ``easy_install`` style syntax to specify
one:

::

  plugin install --github pyopt_driver==0.3
    
This will give you ``pyopt_driver`` version 0.3, not 0.4.


Viewing Plugin Documentation
----------------------------

Viewing the documentation for a plugin is easy. For example, 
if you installed the pyopt_driver using

::

    plugin install --github pyopt_driver

you can then view the documentation for it using

::

    plugin docs pyopt_driver


The last part of that, after the final dot, is the class name and
should be used in the ``plugin docs`` command. 

For an internal plugin like py_opt, it is listed as 

::

    pyopt_driver.pyopt_driver.pyOptDriver

The package name is the next to last part of that: ``pyopt_driver``.
That should be used in the ``plugin docs`` command.

Importing Plugin Modules
------------------------

Again using the example of pyopt_driver, you 
can import the module using:

::

    import pyopt_driver
    

Quick Command Reference for "plugin" Command
--------------------------------------------

.. index:: plugin quick command reference


===================================   ====================================
Action                                Command
===================================   ====================================
List all installed plugins            ``plugin list``
-----------------------------------   ------------------------------------
List builtin installed plugins        ``plugin list -b``
-----------------------------------   ------------------------------------
List external installed plugins       ``plugin list -e``
-----------------------------------   ------------------------------------
List installed plugins from a group   ``plugin list -g groupname``
-----------------------------------   ------------------------------------
List plugins available on GitHub      ``plugin list --github``
-----------------------------------   ------------------------------------
Install plugin foo from GitHub        ``plugin install --github foo``
-----------------------------------   ------------------------------------
Install version 0.3 of plugin foo     ``plugin install --github foo==0.3``
-----------------------------------   ------------------------------------
Display docs for a plugin             ``plugin docs foo``
===================================   ====================================







