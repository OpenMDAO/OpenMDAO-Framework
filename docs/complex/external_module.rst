.. index:: F2PY

.. _Wrapping-an-External-Module-Using-F2PY:

Wrapping an External Module Using F2PY
=======================================

As the most computationally intensive component, the engine model in ``engine.py`` is the main performance
bottleneck during repeated execution. As an interpreted language, Python is not the ideal choice for
implementing a numerical algorithm, particularly where performance is important. The performance of
the engine model can be improved by implementing it in a compiled language like C or Fortran.

Python was designed to be smoothly integrated with other languages, in
particular, C and related languages Fortran and C++. This is important for a
scripting language, where code execution is generally slower and it is often
necessary to use a compiled language like C to implement computationally
intensive functions. To complement Python's native integration ability, the
Python community has developed some excellent tools, such as `F2PY
<http://cens.ioc.ee/projects/f2py2e/>`_ (Fortran to Python) and :term:`SWIG`
(Simplified Wrapper and Interface Generator), that simplify the process of
building the wrapper for a code. As the name implies, F2PY is a Python utility
that takes a Fortran source code file and compiles and generates a wrapped
object callable from Python. More information on getting your codes to run in
Python can be found in the :ref:`Plugin-Developer-Guide`.

The main algorithm in ``engine.py`` was rewritten in C as ``engine.C``. A
wrapped shared object of ``engine.C`` was created using F2Py; this tool can
also be used to generate wrappers for C code provided that the signature file
``engine.pyf`` is manually created. The file ``engine.pyf`` defines the
interface for the functions found in ``engine.C``. The C code has been placed
in a function called ``RunEngineCycle`` which can be imported and takes the design and simulation
variables as inputs.

The intent of this exercise is not to teach you how to write a signature file. More
information on this topic can be found in :ref:`Creating-an-Extension-with-F2PY`, while more
extensive details can be found in the `F2PY Users Guide <http://cens.ioc.ee/projects/f2py2e/usersguide/index.html>`_.

The shared object can be generated using the existing signature file and C code by issuing the
following command:

::

    f2py engineC.pyf engineC.c -c

A new Python component named ``engine_wrap_c.py`` was created to replace
``engine.py``. This component contains the same inputs and outputs as
``engine.py`` but replaces the engine internal calculations with a call to
the C function ``RunEngineCycle``. We can import and use this function just like
any Python function:

.. _Code8: 

.. literalinclude:: ../../examples/openmdao.examples.enginedesign/openmdao/examples/enginedesign/engine_wrap_c.py
   :start-after: engine_weight = 0.0
   :end-before: # end engine.py
   :language: python


Notice that the return values are stored in lists, so a scalar value is accessed by grabbing the first
element (element zero). This is not needed for return values from Fortran codes compiled with
F2PY (arguments in Fortran functions are passed by reference), but it is needed for C codes.

You can compare the execution time to see how much faster the C code runs on your hardware. To
run the original Python implementation of Engine, type:

::

    python engine.py
    
Note the elapsed time, which is printed after the engine completes its run. Now type:
    
::

    python engine_wrap_c.py
    
How much faster is the C implementation? We found it to be almost 10 times as fast as the Python
implementation. Both of these scripts run the engine 50 times before termination.
