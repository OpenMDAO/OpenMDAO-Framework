

===========
Usage Guide
===========

This package provides a mechanism to remotely submit jobs to the NASA High
End Computing (HEC) resources located at the Ames Research Center. Due to
security concerns, direct access to these systems requires either a manual
login process, or obtaining a time-limited 'token'. The workaround implemented
here is to use the 'DMZ' fileservers as an intermediary communication
channel between a local client and a special remote job entry (RJE) server.

.. note::

    Due to the indirect nature of the communications between the local client
    and remote server, transactions are very slow (many seconds). Be patient
    when using this allocator, and be advised that you probably don't want
    this allocator enabled unless you expect to require it.

To be able to remotely submit jobs you need to perform four configuration
operations:

1. 'ssh' access from one or more HEC front-end machines (i.e. ``pfe1``) to one
or both of the DMZ servers (``dmzfs1.nas.nasa.gov`` and ``dmzfs2.nas.nasa.gov``)
must be set up so that no user interaction is required (see 'ssh' documentation
for details).

2. 'ssh' (Linux/Mac) or 'plink'/'pscp' (Windows) access from your local machine
to one or both of the DMZ servers must be set up so that no user interaction is
required.

3. On the HEC front-end machine(s) your OpenMDAO environment must be configured
to enable the :mod:`nas_access` and :mod:`pbs` packages, contained in the
``contrib`` area. This can be done by installing them or setting ``PYTHONPATH``
appropriately. The ``PBS_Allocator`` should also be made available in your
``~/.openmdao/resources.cfg`` file::

    [PBS]
    classname: pbs.PBS_Allocator
    account_id: no-default-set

where ``no-default-set`` is replaced by your group account ID.

4. On your local machine your OpenMDAO environment must be configured to enable
the :mod:`nas_access` package.

Usage of the remote submission capability requires that the RJE server be
running on an HEC front-end machine and the simulation on your local machine
be set up to use the ``NAS_Allocator``.

First start the RJE server on a front-end machine (i.e. ``pfe1``)::

    python -m nas_access.rje

note that the directory in which this command is run will be used for the
server log file as well as the parent directory for object server directories.

Next, set up your environment on the local machine to use the ``NAS_Allocator``.
This can be done by updating your ``~/.openmdao/resources.cfg`` file::

    [Pleiades]
    classname: nas_access.NAS_Allocator
    dmz_host: dmzfs1.nas.nasa.gov
    server_host: pfe1

note that the ``dmz_host`` and ``server_host`` entries may be different
depending on which remote hosts you intend to use.

Alternatively, you can programmatically add the allocator to the resource
manager in your simulation code::

    from openmdao.main.resource import ResourceAllocationManager as RAM
    from nas_access import NAS_Allocator

    allocator = NAS_Allocator(name='Pleiades',
                              dmz_host='dmzfs1.nas.nasa.gov',
                              server_host='pfe1')
    RAM.add_allocator(allocator)

Now when you run your local simulation and it queries the resource allocation
manager for where to run an :class:`ExternalCode`, the ``Pleiades`` allocator
will participate in the process. Depending upon the resource description,
this allocator may be selected to be used for the run. This can easily be
forced by setting ``allocator='Pleiades'`` (or whatever you've named the
allocator).

Note that the default execution directory and file access directory is the
directory in which the OpenMDAO object server is running (a dynamically
generated subdirectory of the RJE server). You may set the execution directory
in the resource description, but at this time this has no effect on file
transfers. Consider having your job submission being a Python script which
can copy or link to other files on the remote host rather than setting the
execution directory to where those files reside.

Consult the :ref:`nas_access_src_label` section for more detail.

