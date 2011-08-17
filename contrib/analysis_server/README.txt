OpenMDAO AnalysisServer interface.

This code provides both client-side and server-side OpenMDAO interfaces to
Phoenix Integration's ModelCenter AnalysisServer protocol.

The client side consists of a factory (factory.ASFactory) which will create
instances on the remote AnalysisServer, create a local proxy for the remote
component, and populate that proxy component with proxy variables
corresponding to the exposed variables on the remote component. This factory
can be registered with the OpenMDAO factory manager to support a general
'create' capability.

The server side (server.main) reads configuration files describing the
components to make available and what interface they provide. Multiple
versions of a component can be supported. Each component instance is run in a
separate process to support concurrent execution.

To view the Sphinx documentation for this distribution, type:

plugin_docs analysis_server

