Overview
--------

.. index:: pair: diagram; top-level context
.. index:: plugins

This overview discusses the basic structure of OpenMDAO, the implementation
language, and how the framework will be distributed.

Top-level Context Diagram
=========================


The following figure shows which pieces of the overall MDAO effort are
considered to be part of the framework infrastructure and which are considered
to be :term:`plugins` to the framework.


.. figure:: ../generated_images/TopContext.png
   :align: center
   :alt: Top level context diagram is a large circle containing plugins that surround the framework box; inside the framework are tools and interfaces.
   
   Top Level Context Diagram


.. index::  layers; framework

Framework Layers
================

.. note:: The following section describes the possibility of having a wxPython GUI
   and a web-based GUI.  Due to limited programming resources, it is more likely
   that there will be only one type of GUI, at least in the next few years. The
   GUI will probably be web based, although a final decision has not been made.

        
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
   :alt: Refer to adjacent text

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

Care will be taken to design the framework to allow easy extension through the
addition of new component types and new data types to be passed between
components. This includes support for custom editors for each new type.


.. index:: Python; programming language
.. index:: pair: Python; module


Implementation Language
=======================

OpenMDAO will be implemented using the Python_ programming language. Plugins to
the framework will be Python modules. A Python module can be either a text file
written in pure Python code or an extension module, which is a shared library
that conforms to the Python C-API. Because Python is an interpreted language and
is not as fast as compiled languages, numerically intensive components will
typically be implemented using a compiled language, such as C, C++, or Fortran,
and will be wrapped as a Python extension module or wrapped using file I/O.

.. _python: http://www.python.org


.. index:: distribution; of framework
.. index:: framework; distributing
.. index:: zope


Distribution
============

The framework will be distributed as a namespace package like other large Python
projects, e.g., zope_. Parts of the framework that are decoupled and deemed
useful on their own will be installable as individual :term:`eggs` like, for
example, *zope.interface*. The framework namespace will also contain a standard
library of open source plugin components. A number of other components that are
not open source, primarily wrappers for NASA analysis codes that cannot be
released open source for various reasons, will be available as individual eggs.

The current package layout of the project is as follows:

``openmdao.main``
    the OpenMDAO framework infrastructure
    
``openmdao.lib``
    standard library of OpenMDAO plugins
    
``openmdao.util``
    OpenMDAO utility routines for file handling, subprocess execution, etc. These
    can be used independently of the rest of OpenMDAO.

``openmdao.test``
    Classes and utilities that are specific to testing various aspects
    of OpenMDAO

``openmdao.examples``
    OpenMDAO tutorial problems and other examples

.. _zope: http://wiki.zope.org/zope3/Zope3Wiki


Deployment
==========

.. note:: Our packaging and deployment methods are likely to change in the
    future due to changes that are happening to Python's overall packaging
    strategy. When the Python community decides on a packaging *standard*, we
    will adopt it. We are currently using a strategy based on *setuptools* and
    :term:`virtualenv`, but it appears that a new package, *distutils2*, along
    with *pip*, may  become the standard.


Each OpenMDAO package will be distributable as a Python source distribution,
and each will share the same version number. Each OpenMDAO package will also
depend on a number of specific versions of third-party packages, and a user
will have to obtain all of these to assemble a complete working application. A
user accomplishes this by running the ``go-openmdao.py`` script to construct a
Python environment containing all of the necessary packages. 

When the ``go-openmdao.py`` script runs, any needed distributions will be downloaded
from one of the following: the Python Package Index, OpenMDAO's own package
directory (http://openmdao.org/dists), some other package index, or from a
local directory. Once this process is completed, a user will have a complete
version of the OpenMDAO framework.




