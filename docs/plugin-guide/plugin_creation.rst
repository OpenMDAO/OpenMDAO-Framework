
.. index:: plugin creation

Types of Plugin Distributions
=============================

There are three main types of plugin distributions:

**Pure Python**

   In this case your plugin class is written entirely in Python.
   This is by far the easiest to develop, the most portable, and the easiest to
   distribute to others.  The only real problem is that it may not
   run fast enough if your plugin is very computationally intensive. It's often
   a good idea to write your plugin first in Python and then evaluate its
   performance, only rewriting part or all of it as a Python extension if
   its performance is unacceptable.


**Python Extension**

    For tasks that are computationally intensive, often the best choice is to
    create a Python extension using a shared library that was compiled from
    Fortran, C, or C++ code. Note that this is typically done in two steps and
    the end result is two separate distribution packages. The first step is to
    wrap the compiled code in a generic python extension that has no dependency
    on OpenMDAO. The second step is to create a python class having the
    necessary OpenMDAO plugin interface and to have that class use the extension
    internally to do the heavy lifting.  This will allow the python extension to
    be used from any python script and not just from OpenMDAO.
    
    This type of distribution should only be used if a pure python distribution
    has unacceptable performance, because distributions containing python extensions 
    are more difficult to develop, build and distribute than pure python distributions.


**External Code Wrapper**

    When you must wrap a legacy code as a Component or Driver plugin and it's not
    feasible to create a Python extension for it, you can configure an
    ExternalCode component to communicate with the legacy code via file I/O.  Using
    this option, the wrapper is pure python which is easy to distribute, but
    distributing the legacy code can often be problematic.

