Working with Parallel Computational Resources
======================================================


Warning: This is a very advanced topic for dealing with parallel compute resources 
in OpenMDAO. If you have not gone through at least our :ref:`basic tutorial <basic-tutorial>`
and our :ref:`optimization tutorial <optimization_tutorial>`, then you should become more 
familiar with the foundations of OpenMDAO before delving into this section. 

.. _Specifying-Computational-Resources:

Specifying Computational Resources
------------------------------------
OpenMDAO uses the concept of *resource allocators* to decouple the specification
of a simulation from the computational resources it requires.  This makes
simulations more portable and also allows for the currently "best" resource
to be used.  The :ref:`ResourceAllocationManager <resource.py>` (RAM) manages
the selection of a server from one or more registered allocators.
:ref:`ExternalCode <external_code.py>`,
:ref:`CaseIteratorDriver <caseiterdriver.py>`, and
:ref:`DOEdriver <doedriver.py>` are able to use this facility.

During ExternalCode execution, if the instance has an empty ``resources``
dictionary, then the external code is run locally and started directly by the
ExternalCode instance.  If, however, the ``resources`` dictionary is not empty,
then it is used to allocate a server process which can support the resource
request.  This allocation process is performed by the RAM.  Once a suitable
server is found, the ExternalCode instance will send any input files to the
server, invoke ``execute_command()`` on the server, and then retrieve any output
files.

During CaseIteratorDriver or DOEdriver execution, a resource allocation is
performed for each case to be evaluated (unless sequential execution is
specified).  Once the server is allocated, the sub-model egg is loaded into the
server, input variables are set, the model is run, and outputs are retrieved.
The resource allocator normally just looks for a compatible server based on
the sub-model's Python requirements, but you can add additional resource
information via the ``extra_resources`` attribute. The RAM method
:ref:`max_request() <resource.py>` can be useful for generating the
``extra_resources`` attribute value when the sub-model contains resource
descriptions (for example, when the sub-model contains a wrapper for a parallel
CFD code).
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

========================== ======  ===============================================
Allocation Key             Value   Description
========================== ======  ===============================================
``allocator``              string  Name of allocator to use
-------------------------- ------  -----------------------------------------------
``localhost``              bool    Must be/must not be on the local host
-------------------------- ------  -----------------------------------------------
``exclude``                list    Hostnames to exclude
-------------------------- ------  -----------------------------------------------
``required_distributions`` list    List of :class:`pkg_resources.Distribution`
                                   or package requirement strings
-------------------------- ------  -----------------------------------------------
``orphan_modules``         list    List of "orphan" module names
-------------------------- ------  -----------------------------------------------
``python_version``         string  Python version required (e.g., "2.7")
-------------------------- ------  -----------------------------------------------
``python_platform``        string  Python platform required (e.g., "linux-x86_64")
-------------------------- ------  -----------------------------------------------
``min_cpus``               int     Minimum number of CPUs/cores required
-------------------------- ------  -----------------------------------------------
``max_cpus``               int     Maximum number of CPUs/cores that can be used
-------------------------- ------  -----------------------------------------------
``min_phys_memory``        int     Minimum amount of memory required (KB)
========================== ======  ===============================================

Values for ``required_distributions`` and ``orphan_modules`` are typically taken
from the return value of :meth:`component.save_to_egg`.
The value for ``python_platform`` is typically taken from the return value of
:meth:`distutils.util.get_platform`.
The ``min_phys_memory`` key is also used as a queuing key.
The ``min_cpus`` and ``max_cpus`` keys are also used as queuing keys for parallel
applications. They are analogous to the DRMAA (Distributed Resource Management
Application API) ``minSlots`` and ``maxSlots`` attributes, with the intent that a "cpu" can execute an MPI process
(A DRMAA "slot" is opaque and can have different interpretations).

Most of the queuing keys are derived from the DRMAA standard ``JobTemplate``:

=============================  ========  ==============================================
Queuing Key                    Value     Description
=============================  ========  ==============================================
``remote_command``             string    Command to execute
                                         (just the command, no arguments)
-----------------------------  --------  ----------------------------------------------
``args``                       list      Arguments for the command
-----------------------------  --------  ----------------------------------------------
``submit_as_hold``             bool      Submit job to start in ``HOLD`` state
-----------------------------  --------  ----------------------------------------------
``rerunnable``                 bool      Job is rerunnable (default False)
-----------------------------  --------  ----------------------------------------------
``job_environment``            dict      Any additional environment variables needed
-----------------------------  --------  ----------------------------------------------
``working_directory``          string    Directory to execute in (use with care)
-----------------------------  --------  ----------------------------------------------
``job_category``               string    Type of job, useful for parallel codes
-----------------------------  --------  ----------------------------------------------
``email``                      list      List of email addresses to notify
-----------------------------  --------  ----------------------------------------------
``email_on_started``           bool      Notify when jobs starts
-----------------------------  --------  ----------------------------------------------
``email_on_terminated``        bool      Notify when job terminates
-----------------------------  --------  ----------------------------------------------
``job_name``                   string    Name for the submitted job
-----------------------------  --------  ----------------------------------------------
``input_path``                 string    Path for stdin
-----------------------------  --------  ----------------------------------------------
``output_path``                string    Path for stdout
-----------------------------  --------  ----------------------------------------------
``error_path``                 string    Path for stderr
-----------------------------  --------  ----------------------------------------------
``join_files``                 bool      If True, stderr is joined with stdout
-----------------------------  --------  ----------------------------------------------
``reservation_id``             string    ID of reservation (obtained externally)
-----------------------------  --------  ----------------------------------------------
``queue_name``                 string    Name of queue to use
-----------------------------  --------  ----------------------------------------------
``priority``                   int       Queuing priority
-----------------------------  --------  ----------------------------------------------
``start_time``                 datetime  Timestamp for when to start the job
-----------------------------  --------  ----------------------------------------------
``deadline_time``              datetime  Timestamp for when the job must be complete
-----------------------------  --------  ----------------------------------------------
``resource_limits``            dict      Job resource limits (see below)
-----------------------------  --------  ----------------------------------------------
``accounting_id``              string    ID used for job accounting
-----------------------------  --------  ----------------------------------------------
``native_specification``       list      Queuing system specific options
=============================  ========  ==============================================

Using ``native_specification`` is discouraged since that makes the submitting
application less portable. However, at times its use is necessary in order to access specific
features of a queuing system.

DRMAA derived job categories:

============  =============================
Category      Environment
============  =============================
``MPI``       Any MPI environment
------------  -----------------------------
``GridMPI``   A GridMPI environment
------------  -----------------------------
``LAM-MPI``   A LAM/MPI environment
------------  -----------------------------
``MPICH1``    A MPICH version 1 environment
------------  -----------------------------
``MPICH2``    A MPICH version 2 environment
------------  -----------------------------
``OpenMPI``   A OpenMPI environment
------------  -----------------------------
``PVM``       A PVM environment
------------  -----------------------------
``OpenMP``    A OpenMP environment
------------  -----------------------------
``OpenCL``    A OpenCL environment
------------  -----------------------------
``Java``      A Java environment
============  =============================

DRMAA derived resource limits:

==================  =====
Name                Type
==================  =====
``core_file_size``  Soft
------------------  -----
``data_seg_size``   Soft
------------------  -----
``file_size``       Soft
------------------  -----
``open_files``      Soft
------------------  -----
``stack_size``      Soft
------------------  -----
``virtual_memory``  Soft
------------------  -----
``cpu_time``        Hard
------------------  -----
``wallclock_time``  Hard
==================  =====

Soft limits do not affect scheduling decisions.
Hard limits may be used for scheduling.

Times are in seconds.

The ``HOME_DIRECTORY`` and ``WORKING_DIRECTORY`` constants in
:mod:`openmdao.main.resource` may be used as placeholders in path
specifications. They are translated at the server.

Not all resource allocators support all the features listed above. Consult the allocator
documentation to see what is supported and to find out how the 
features are translated to the system the allocator interfaces with.
