
Running OpenMDAO
==================

.. _Setting-the-Top-Level-Assembly:

Setting the Top Level Assembly
------------------------------

When a Component or Assembly is instantiated as a standalone object, it is not
aware of the directory where it resides. Any component added to such an assembly
also does not know its path. The function ``set_as_top`` is available to denote an
assembly as the top level assembly in the framework. Once an assembly is set
as the top level assembly, it gains an absolute path which can be accessed
through the function ``get_abs_directory``.

The path that is set by ``set_as_top`` is always the current working directory 
in the Python environment.

    >>> from openmdao.main.api import Assembly, set_as_top   
    >>> z1 = Assembly()
    >>> z1.get_abs_directory()
    Traceback (most recent call last):
    ...
    RuntimeError: can't call get_abs_directory before hierarchy is defined
    >>>
    >>> set_as_top(z1)
    <openmdao.main.assembly.Assembly object at ...>
    >>> z1.get_abs_directory()
    '...'

The output in this example depends on your local directory structure.
All components added into this assembly will have this same absolute path. If a 
component or assembly does not have a valid absolute directory, then File 
variables will not be able to read, write, or even open their target files.

Executing Models
------------------

.. todo::

    Show how to run a model.

.. todo::

    Discuss Reset to Defaults.

Error Logging & Debugging
---------------------------

.. todo::

    Explain the error logging capability.

Saving & Loading
-----------------

.. todo::

    Show how to save and load.

Sharing Models
----------------

.. todo::

    Discuss sharing models.
