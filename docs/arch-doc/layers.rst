The framework's capabilities can be viewed as a number of layers supporting
component interactions between other components and the user. The diagram below,
`Framework Layers`_, shows a simulation consisting of three components, one of
which runs in a separate process from the main simulation. Also shown are two
user interfaces accessing the simulation: a wxPython GUI which runs in a process
separate from the main simulation server and a web browser which connects to a
web service embedded within the main simulation server.

.. _`Framework Layers`:

.. figure:: ../generated_images/Layers.png
   :align: center

   Framework Layers


.. index:: API
.. index:: simulation GUI
.. index:: wxPython
.. index:: AJAX Javascript 
.. index:: pair: distributed object; protocol 
.. index:: pair: Component; local
.. index:: pair: Component; remote

Component interactions occur via the framework's component :term:`API`. A
distributed object protocol and network layer provide communication between
components in separate processes.

For components whose calculations are performed by an external code, the
framework provides facilities for generating input files, finding a suitable
host, running the external code, parsing output files, etc.

The simulation :term:`GUI` is based on a library of user interface widgets
which are written in terms of a user interface API which the framework
translates to either wxPython calls or equivalent interactions with an AJAX
Javascript library. The simulation GUI accesses component data via the component
API, with the same code supporting local and remote components as well as
wxPython and AJAX based displays.

To provide web access, the main simulation server is configured to support
the HTTP and/or HTTPS protocols.

