

===========
Usage Guide
===========

As a client, an instance of :class:`factory.ASFactory` must be created,
referring to the host and port of the remote AnalysisServer to connect to.
This factory can then be registered with OpenMDAO via
:meth:`openmdao.main.factorymanager.regster_class_factory`. Subsequent
:meth:`openmdao.main.factorymanager.create` calls referring to component types
supported by the AnalysisServer will return a proxy component populated with
proxy variables corresponding to the exposed interface of the remote component.

To allow ModelCenter access to OpenMDAO components, ``server.py`` is started
in a directory containing component configuration files and corresponding
component Python or egg files. The configuration files specify what part of the
component interface is to be exposed. The server supports remote publishing of
components, accessible via the ``publish.py`` tool.

Consult the :ref:`analysis_server_src_label` section for more detail.

