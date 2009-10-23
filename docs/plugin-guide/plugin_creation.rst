
.. index:: plugin creation

Plugin Creation Methods
=======================

There are three primary ways to create a plugin for OpenMDAO. The simplest and
most common way, for tasks that are not computationally intensive, is to
create the plugin by writing it in pure Python. For tasks that are
computationally intensive, often the best choice is to create a Python
extension using a shared library that was compiled from FORTRAN, C, or C++
code. Finally, when a legacy code must be wrapped as a Component or Driver
plugin and creating a Python extension for it is not feasible, an ExternalCode
component can be configured to communicate with the legacy code via file I/O.

   
