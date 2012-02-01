
Running OpenMDAO
==================

.. _Setting-the-Top-Level-Assembly:

Setting the Top Level Assembly
------------------------------

When a Component or Assembly is instantiated as a standalone object, it is not
aware of the directory where it resides. Any component added to such an assembly
also does not know its path. The function ``set_as_top`` is available to denote an
assembly as the top level assembly in the framework. Once an assembly is set
as the top level assembly, it gains an absolute path which can be accessed
through the function ``get_abs_directory``.

The path that is set by ``set_as_top`` is always the current working directory 
in the Python environment.

    >>> from openmdao.main.api import Assembly, set_as_top   
    >>> z1 = Assembly()
    >>> z1.get_abs_directory()
    Traceback (most recent call last):
    ...
    RuntimeError: can't call get_abs_directory before hierarchy is defined
    >>>
    >>> set_as_top(z1)
    <openmdao.main.assembly.Assembly object at ...>
    >>> z1.get_abs_directory()
    '...'

The output in this example depends on your local directory structure.
All components added into this assembly will have this same absolute path. If a 
component or assembly does not have a valid absolute directory, then File 
variables will not be able to read, write, or even open their target files.

Executing Models
------------------

.. todo::

    Show how to run a model.

.. todo::

    Discuss Reset to Defaults.

Error Logging & Debugging
---------------------------

.. todo::

    Explain the error logging capability.

Saving & Loading
-----------------

.. todo::

    Show how to save and load.

Sharing Models
----------------

.. todo::

    Discuss sharing models.

.. _Specifying-Computational-Resources:

Specifying Computational Resources
------------------------------------
OpenMDAO uses the concept of *resource allocators* to decouple the specification
of a simulation from the computational resources it requires.  This makes
simulations more portable and also allows for the currently "best" resource
to be used.  The :ref:`ResourceAllocationManager <resource.py>` (RAM) manages
the selection of a server from one or more registered allocators.
:ref:`ExternalCode <external_code.py>` and
:ref:`CaseIteratorDriver <caseiterdriver.py>` are able to use this facility.

During ExternalCode execution, if the instance has an empty ``resources``
dictionary, then the external code is run locally and started directly by the
ExternalCode instance.  If, however, the ``resources`` dictionary is not empty,
then it is used to allocate a server process which can support the resource
request.  This allocation process is performed by the RAM.  Once a suitable
server is found, the ExternalCode instance will send any input files to the
server, invoke ``execute_command()`` on the server, and then retrieve any output
files.

During CaseIteratorDriver execution a resource allocation is performed for
each case to be evaluated (unless sequential execution is specified).  Once the
server is allocated, the sub-model egg is loaded into the server, input
variables are set, the model is run, and outputs are retrieved.
The resource allocator normally just looks for a compatible server based on
the sub-model's Python requirements, but you can add additional resource
information via the ``extra_resources`` attribute.
In some circumstances, particularly when submitting from a Windows client to a
Linux server (or vice-versa), there will be spurious Python incompatibilities.
You can try forcing a submission by setting the ``ignore_egg_requirements``
attribute to True.

There are several OpenMDAO resource allocators available:

:ref:`LocalAllocator <resource.py>`
    This is the default.  It returns server processes on the local host.
    The RAM is initialized with one of these, named ``LocalHost``.

:ref:`RemoteAllocator <resource.py>`
    This is a proxy for an allocator on a remote host.  It is typically
    created by ``RAM.add_remotes(server)``, providing the local RAM access to
    all allocators defined in the remote server's RAM.  Note that OpenMDAO
    servers can be accessed through an SSH tunnel.  So if a system is behind
    a firewall that allows SSH tunneling, its allocators may be added to the
    local RAM.

:ref:`ClusterAllocator <resource.py>`
    This allocator selects from a collection of dynamically started host
    servers via their respective ``LocalHost`` allocators.

:ref:`GridEngine <grid_engine.py>`
    This allocator returns servers which use the GridEngine ``qsub`` command
    when ``execute_command()`` is invoked.

:ref:`PBS <pbs.py>`
    This allocator returns servers which use the PBS ``qsub`` command
    when ``execute_command()`` is invoked.

Since some types of allocated servers are capable of submitting jobs to queuing
systems, a resource description is a dictionary that can include both
allocation and queuing information.  Allocation keys are used to find suitable
servers while queuing keys are used to describe the job to be submitted.

========================== ======  ===========================================
Allocation Key             Value   Description
========================== ======  ===========================================
``allocator``              string  Name of allocator to use
-------------------------- ------  -------------------------------------------
``localhost``              bool    Must be/must not be on the local host
-------------------------- ------  -------------------------------------------
``exclude``                list    Hostnames to exclude
-------------------------- ------  -------------------------------------------
``required_distributions`` list    List of :class:`pkg_resources.Distribution`
                                   or package requirement strings
-------------------------- ------  -------------------------------------------
``orphan_modules``         list    List of "orphan" module names
-------------------------- ------  -------------------------------------------
``python_version``         string  Python version required (e.g., "2.7")
-------------------------- ------  -------------------------------------------
``n_cpus``                 int     Number of CPUs/cores required
========================== ======  ===========================================

Values for ``required_distributions`` and ``orphan_modules`` are typically taken
from the return value of :meth:`component.save_to_egg`. The ``n_cpus`` key is
also used as a queuing key for parallel applications.

Most of the queuing keys are derived from the Distributed Resource Management
Application API (DRMAA) standard:

=============================  ======  ===============================================
Queuing Key                    Value   Description
=============================  ======  ===============================================
``job_name``                   string  Name for the submitted job
-----------------------------  ------  -----------------------------------------------
``remote_command``             string  Command to execute
                                       (just the command, no arguments)
-----------------------------  ------  -----------------------------------------------
``args``                       list    Arguments for the command
-----------------------------  ------  -----------------------------------------------
``job_environment``            dict    Any additional environment variables needed
-----------------------------  ------  -----------------------------------------------
``working_directory``          string  Directory to execute in (use with care)
-----------------------------  ------  -----------------------------------------------
``parallel_environment``       string  Used by some systems for parallel applications
-----------------------------  ------  -----------------------------------------------
``input_path``                 string  Path for stdin
-----------------------------  ------  -----------------------------------------------
``output_path``                string  Path for stdout
-----------------------------  ------  -----------------------------------------------
``error_path``                 string  Path for stderr
-----------------------------  ------  -----------------------------------------------
``join_files``                 bool    If True, stderr is joined with stdout
-----------------------------  ------  -----------------------------------------------
``email``                      list    List of email addresses to notify
-----------------------------  ------  -----------------------------------------------
``block_email``                bool    If True, do not send notifications.
-----------------------------  ------  -----------------------------------------------
``email_events``               string  When to send notifications. \
                                       ("b"=>beginning, "e"=>end, "a"=>abort, \
                                       "s"=>suspension)
-----------------------------  ------  -----------------------------------------------
``start_time``                 string  Timestamp for when to start the job
-----------------------------  ------  -----------------------------------------------
``deadline_time``              string  Timestamp for when the job must be complete
-----------------------------  ------  -----------------------------------------------
``hard_wallclock_time_limit``  int     Time limit while running or suspended (sec)
-----------------------------  ------  -----------------------------------------------
``soft_wallclock_time_limit``  int     Estimated time running or suspended (sec)
-----------------------------  ------  -----------------------------------------------
``hard_run_duration_limit``    int     Time limit while running (sec)
-----------------------------  ------  -----------------------------------------------
``soft_run_duration_limit``    int     Estimated time while running (sec)
-----------------------------  ------  -----------------------------------------------
``native_specification``       string  Queuing system specific options
=============================  ======  ===============================================

Use of ``native_specification`` is discouraged since that makes the submitting application
less portable.

The ``HOME_DIRECTORY`` and ``WORKING_DIRECTORY`` constants in
:mod:`openmdao.main.resource` may be used as placeholders in path
specifications. They are translated at the server.

