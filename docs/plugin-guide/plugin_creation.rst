
.. index:: plugin creation

Plugin Creation Methods
=======================

You can create a plugin for OpenMDAO in three primary ways. 

* For tasks that are not computationally intensive, the simplest and most
  common way is to write it in pure Python. 

* For tasks that are computationally intensive, often the best choice is to
  create a Python extension using a shared library that was compiled from
  Fortran, C, or C++ code. Note that this is typically done in two steps and
  the end result is two separate distrubution packages. The first step is to
  wrap the compiled code in a generic python extension that has no dependency
  on OpenMDAO. The second step is to create a python class having the
  necessary OpenMDAO plugin interface and to have that class use the extension
  internally to do the heavy lifting.  This will allow the python extension to
  be used from any python script and not just from OpenMDAO.
 
* When you must wrap a legacy code as a Component or Driver plugin and it's not
  feasible to create a Python extension for it, you can configure an
  ExternalCode component to communicate with the legacy code via file I/O.

   
