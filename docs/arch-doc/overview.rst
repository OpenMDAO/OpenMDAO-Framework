Overview
--------

.. index:: pair: diagram; top-level context
.. index:: plug-ins



Top-level Context Diagram
=========================


The following figure shows which pieces of the overall MDAO effort are
considered to be part of the framework infrastructure and which are considered
to be :term:`plug-ins` to the framework.


.. figure:: ../generated_images/TopContext.png
   :align: center

   Top Level Context Diagram


.. index::  layers; framework

Framework Layers
================


.. include:: layers.rst


Care will be taken to design the framework to allow easy extension through the
addition of new component types and new data types to be passed between
components. This includes support for custom editors for each new type.


.. index:: Python; programming language
.. index:: pair: Python; module


Implementation Language
=======================

OpenMDAO will be implemented using the Python_ programming language. Plug-ins to
the framework will be Python modules. A Python module can be either a text file
written in pure Python code or an extension module, which is a shared library that
conforms to the Python C-API. Because Python is an interpreted language,
numerically intensive components will typically be implemented using a compiled
language, such as C, C++, or FORTRAN, and will be wrapped as a Python extension
module or wrapped using file I/O.

.. _python: http://www.python.org


.. index:: distribution; of framework
.. index:: framework; distributing
.. index:: zope

Distribution
============

The framework will be distributed as a namespace package like other large Python
projects, e.g., zope_. Parts of the framework that are decoupled and deemed useful
on their own will be installable as individual :term:`eggs` like, for example,
*zope.interface*. The framework namespace will also contain a standard library of
open source plug-in components. A number of other components that are not open
source, primarily wrappers for NASA analysis codes that cannot be released open
source for various reasons, will be available as individual eggs.


.. _zope: http://wiki.zope.org/zope3/Zope3Wiki


