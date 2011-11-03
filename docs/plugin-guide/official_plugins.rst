.. index:: OpenMDAO-Plugins

.. _installing-plugins-from-the-official-openmdao-plugin-repository:

Installing Plugins from the Official OpenMDAO Plugin Repository
===============================================================

The OpenMDAO project also hosts a set of official plugins in a GitHub
top-level organization called **OpenMDAO-Plugins**. You can visit this site at
https://github.com/OpenMDAO-Plugins/

These plugins are all open source, although some of them are component wraps
of proprietary or otherwise-restricted codes (e.g., Nastran) that you will
have to obtain separately. In most cases, the plugin only contains the
OpenMDAO wrapper code.

The ``plugin list`` command allows you to list all plugins that are already
installed, and with the addition of a ``-g`` option it will show you a list of
all plugins available for installation from the OpenMDAO-Plugins organization
on GitHub. 

To list all plugins you currently have installed, type the
following at the OS prompt (always in an activated OpenMDAO environment):

::

    plugin list
    
You should get a response that looks something like this.

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
    openmdao.lib.drivers.gradient.SensitivityDriver 0.2.1
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


This shows both the plugin name and the versions in a simple list. 

The default behavior of ``plugin list`` is to show every installed plugin whether it is
built into the OpenMDAO distribution or has been added to the OpenMDAO environment from
some other source.  To show only plugins that are *not* built into the OpenMDAO distribution,
use the ``--external`` option. A shortcut for ``--external`` is ``-e``.  For example:

::

    plugin list -e


To show only the builtin plugins that come with OpenMDAO, use the ``--builtin`` option or its
shortcut, ``-b``.


You can also grab a list of the plugin distributions that are available from the OpenMDAO-Plugins
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

Note that https must be available in order to communicate with the GitHub repository. If you
don't have a network connection, then these features will be unavailable. There is also a ``-g``
shortcut for the argument ``--github``:

::

    plugin list -g

To install one of the plugins listed above, simply type:

::

    plugin install -g pyopt_driver
    
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

  plugin install -g pyopt_driver==0.3
    
This will give you ``pyopt_driver`` 0.3, not 0.4.

Here is a quick reference for these commands:

.. index:: plugin_install quick command reference
    
**Quick Command Reference for ``plugin list and plugin install``**


==================================   =================================
Action                                Command
==================================   =================================
List all installed plugins           ``plugin list``
----------------------------------   ---------------------------------
List plugins available on GitHub     ``plugin list -g``
----------------------------------   ---------------------------------
Install plugin foo from GitHub       ``plugin install -g foo``
----------------------------------   ---------------------------------
Install version 0.3 of plugin foo    ``plugin install -g foo==0.3``
==================================   =================================


