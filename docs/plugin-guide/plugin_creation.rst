
.. index:: plugin creation

Plugin Creation Methods
=======================

You can create a plugin for OpenMDAO in three primary ways. 

* The simplest and most common way, for tasks that are not computationally
  intensive, is to write it in pure Python. 

* For tasks that are computationally intensive, often the best choice is to
  create a Python extension using a shared library that was compiled from
  Fortran, C, or C++ code. 
 
* When you must wrap a legacy code as a Component or Driver plugin and it's not
  feasible to create a Python extension for it, you can configure an
  ExternalCode component to communicate with the legacy code via file I/O.

   
