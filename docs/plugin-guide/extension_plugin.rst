
Building a Plugin Using a Python Extension
==========================================

It will more often be the case that there is some existing application, written in some language 
other than Python, that needs to be turned into an openMDAO component. This section and the one 
that follows present some examples illustrating this process. The process of building a python
extension to some external code is called wrapping. There are generally two ways to wrap an
application: direct wrapping and file wrapping. In a direct wrap, data is passed directly into
and out of the wrapped function through memory, while in a file wrap an intermediate data file
is used.

When to direct wrap:

- Source (or linkable library) is available
- Source is a function that passes data as arguments (or through COMMON blocks)
- Source can be easily modified to pass data as arguments (or through COMMON blocks)
- Large amounts of data need to be passed (performance)
- OpenMDAO component needs to be executed repeatedly (performance)

When to file wrap:

- Source (or linkable library) is unavailable
- Source passes data via file I/O
- There are no resources to modify Legacy code to pass data as arguments

In general, a direct wrap should have better performance and should be less problematic, but
it is not always a viable option. To learn how to file-wrap in OpenMDAO, see 
:ref:`Building-a-Plugin-Using-a-File-Wrapper`.

For the examples presented here, it is assumed that the reader is already familiar with the
fundamentals of the OpenMDAO component API, and is familiar with creating Python components
as outlined in the :ref:`The-OpenMDAO-tutorial-problem`.

Creating an Extension with F2PY
--------------------------------

To support the integration of FORTRAN codes into Python, F2PY --Fortran to Python Interface Generator--
was developed. It is currently distributed as part of the NumPy package for Python, and is not part
of the OpenMDAO bundle at this time. In order to use F2PY, NumPy needs to be installed into the default
Python environment on your system.

::

    Wrapper Utility: F2PY
    Source Languages: C, FORTRAN77/90/95 (newer FORTRAN can also be wrapped, but not all features are supported)
    Documentation: http://cens.ioc.ee/projects/f2py2e/usersguide/index.html
    
F2PY automates the process of generating a library that contains FORTRAN functions callable from Python
as well as an interface into exposed common blocks and FORTRAN 90/95 module data. Additionally, F2PY
handles the conversion of inputs and output between their representative Python and FORTRAN types, 
particularly with arrays, which are converted into NumPy's numerical arrays on the Python side. The
most attractive feature of F2PY is its simplicity, and it can be quickly learned and used without
understanding the details under the hood.

Note that while F2PY was developed for use with FORTRAN, it can also wrap C functions with almost as 
much ease. An example of wrapping a C function with F2PY can be found in :ref:`Wrapping-an-External-Module-Using-F2PY`
in the OpenMDAO Tutorial.

To illustrate the creation of an OpenMDAO component from a FORTRAN function, a brief tutorial will now
be presented. The following instructions will help the user locate the directory containing the pieces
needed for the model relative to the install directory:

If you have a branch from the source repository:

	``examples/openmdao.examples.bar3simulation/openmdao/examples/bar3simulation``
	
If you have a distribution bundle:

	``buildout/eggs/openmdao.examples.bar3simulation-x.x.x-xxxxxx.egg/openmdao/examples/bar3simulation``
	
where the x's denote the OpenMDAO version number, the Python version, and the Operating System
description string. This will vary depending on your system and version, but there will only be
one bar3simulation egg in your bundle.

It should also be noted that a FORTRAN compiler is required. The instructions presented here are
applicable to the Unix and Mac OSX environments. There may be some differences on the Windows
platform.

The FORTRAN code bar3.f contains the subroutine *runbar3truss*, which contains an analytical solution
for a three bar truss with the following specific geometry:


.. figure:: ../images/plugin-guide/ThreeBar.jpg
   :align: center

   The 3-Bar Truss Geometry
   
The inputs to the problem are the components of the body force acting on node 1 (2d array pvec),
the cross-sectional areas of all three structural elements (a1, a2, a3), the lumped mass at node 1 (mo),
the length of bar 2 (el: this essentially scales the problem), and some material properties for the 
bars (e -- Young's Modulus, and rho - material density). The outputs of interest are the stresses 
in each bar (s1, s2, s3), the displacement at node 1 (u, v), the frequency of the first mode of
vibration (ff), and the total weight of the structure (obj). The objective of this example is
to be able to use this Fortran subroutine to locate the optimum dimensions of the three bars that
minimizes the total weight of the structure while satisfying constraints on the bar stresses, the
displacement of node 1, and the frequency of the first mode.
   
The `F2PY Users Guide <http://cens.ioc.ee/projects/f2py2e/usersguide/index.html>`_ describes three
ways to use F2PY to generate the Python-callable object. "The Quick Way" is to just run f2py on the
FORTRAN file, which produces a shared object that containing a function (or functions) that can be
called from Python. This works for the simplest case, but breaks down when F2PY doesn't know which
function argumements are inputs and which are outputs. In "The Smart Way", the user specifies the
input/output intent of each function in the signature file (extension .pyf). Finally, in the
"Quick and Smart Way", the input/output intents are specified directly in the FORTRAN code as 
comments.

This example showcases the "Quick and Smart Way." An example of "The Smart Way" can be found in 
:ref:`Wrapping-an-External-Module-Using-F2PY`, where an example of a signature file is included
as part of the engine design tutorial. The "Quick and Smart Way" should be fine for most cases,
provided there are no objections to inserting new comments into the existing source code. For
some cases, the extra flexibility of the signature file may be needed; one specific example
would be a case where you only want to expose one function from a FORTRAN file that contains
several. What can be done in this case is to instruct F2PY to generate this signature file,
after which it can be edited to the user's satisfaction.

Subroutine *runbar3truss* has the following interface:

::

      SUBROUTINE runbar3truss(PX,PY,M0,A1,A2,A3,E,EL,RHO,
     *                        S1,S2,S3,U,V,FF,OBJ) 
     
The inputs and outpus are as described above. In order to tell F2PY which of these variables are
inputs and which are outputs, a series of comments is inserted after the function header. These
comments are prefaced with Cf2py:
     
::

          Double Precision S1, S2, S3
          Double Precision U, V, FF 
          Double Precision obj

    Cf2py intent(in) pvec
    Cf2py intent(in) mo
    Cf2py intent(in) a1 
    Cf2py intent(in) a2  
    Cf2py intent(in) a3   
    Cf2py intent(in) e
    Cf2py intent(in) el
    Cf2py intent(in) rho
    Cf2py intent(out) s1
    Cf2py intent(out) s2 
    Cf2py intent(out) s3    
    Cf2py intent(out) u   
    Cf2py intent(out) v      
    Cf2py intent(out) ff     
    Cf2py intent(out) obj
    
The intent(in) marks an input, and intent(out) denotes an output. If an argument serves as
both an input and output (i.e., it passes a value to the argument and expects a change
upon completion), then intent(inout) can be used. There are several other intents that are
useful for other less common cases. One that may be of interest is intent(callback), which
can be used to pass a Python (or other) function into a FORTRAN subroutine.

Once the intents have all been declared, F2PY can be executed to produce the module by
executing the following at the command prompt:

::

    [unix_prompt]$ f2py -c -m bar3 bar3.f
    
The result is the shared object bar3.so. The next step is to build a Python component that
can run *runbar3truss*, supplying its inputs and gathering its output. An OpenMDAO wrapper
for bar3.so is available as part of this example, and can be found in bar3_wrap_f.py. The
functions that were compiled through F2PY are contained in the bar3 library, and this can
be imported into Python just like any Python file:

.. testsetup:: bar3_wrap

    from openmdao.examples.bar3simulation.bar3_wrap_f import Bar3Truss
    import numpy.numarray as numarray
    
    self = Bar3Truss()
    
    load = numarray.zeros(2,'d')
    load[0] = 50.0
    load[1] = 100.0
    lumped_mass = 0.68005
    bar1_area = 1.0
    bar2_area = 1.0
    bar3_area = 1.0
    Youngs_Modulus = 30000.0
    bar2_length = 100.0
    weight_density = 0.1

.. testcode:: bar3_wrap

    from openmdao.examples.bar3simulation.bar3 import runbar3truss, forces

Note that the namespace comes from OpenMDAO's structure. Here, we import both the function
*runbar3truss* and the common block *forces*. Calling into this function is similar to
calling a Python function; inputs are passed in as arguments, and outputs are returned
on the right hand side.

.. testcode:: bar3_wrap

        # Call the Fortran model and pass it what it needs.

        (self.bar1_stress, self.bar2_stress, self.bar3_stress, 
         self.displacement_x_dir, self.displacement_y_dir, 
         self.frequency, self.weight) \
         = runbar3truss(
                    load, lumped_mass, 
                    bar1_area,bar2_area,bar3_area,
                    Youngs_Modulus, bar2_length, weight_density)

F2PY automatically also generates a docstring for this function. This can be examined by
opening OpenMDAO's local python environment:

    >>> from openmdao.examples.bar3simulation.bar3 import runbar3truss, forces
    >>> print runbar3truss.__doc__
    runbar3truss - Function signature:
      s1,s2,s3,u,v,ff,obj = runbar3truss(pvec,m0,a1,a2,a3,e,el,rho)
    Required arguments:
      pvec : input rank-1 array('d') with bounds (2)
      m0 : input float
      a1 : input float
      a2 : input float
      a3 : input float
      e : input float
      el : input float
      rho : input float
    Return objects:
      s1 : float
      s2 : float
      s3 : float
      u : float
      v : float
      ff : float
      obj : float		    
    <BLANKLINE>    

The docstring can be useful for figuring out the arguments and returns for the
generated function. Note that most of the values passed here are floats, which
are analogous to Double Precision variables in FORTRAN. The load is stored in
pvec, which is an array that contains the x and y components of the force. To
pass this into the FORTRAN subroutine, it needs to be in the form of a NumPy
array (in this case, an array of floating point numbers):

.. testcode:: bar3_wrap_array

    import numpy.numarray as numarray
    
    load = numarray.zeros(2,'d')
    load[0] = 50.0
    load[1] = 100.0

By the same token, NumPy arrays should be used to receive arrays that are returned to
Python by the FORTRAN function.

Data in the common blocks is also accessible. In this case, the FORTRAN code stores
the values of the bar forces in a common block as force1, force2, and force3.

.. testcode:: bar3_wrap

    bar1_force = float(forces.force1)
    bar2_force = float(forces.force2)
    bar3_force = float(forces.force3)
    
There is one oddity here. Scalar variables in the common block get returned to Python
as a zero-dimensional NumPy array. It is not entirely clear why they chose to do this,
but their values can be accessed by casting them as floats or int. Note also that values
can also be input into the common block. In practice, the common block will probably be
frequently used for passing variables as opposed to cluttering the function interface.

Further examples of a more complicated wrap can be seen in the source for the OpenMDAO 
wrapper of the CONMIN optimizer (conmindriver.py).

Finally, the OpenMDAO top level assembly for this problem is given in bar3_optimization.py.
This model integrates the 3-bar truss wrapper and the CONMIN optimizer to solve the full
problem.
    
**F2PY Quick Command Reference**

============================ =============================
Ordinary Build                f2py -c -m foo foo.f
---------------------------- -----------------------------
Only Make Signature File      f2py -m foo -h foo.pyf foo.f
---------------------------- -----------------------------
Build with Signature foo.pyf  f2py foo.pyf foo.f -c
============================ =============================


Creating an Extension with SWIG
--------------------------------

::

    Wrapper Utility: SWIG
    Source Languages: C, C++
    Documentation: http://www.swig.org/doc.html
    

TODO - C Example

TODO - C++ Example


Creating an Extension with JCC
------------------------------

::

    Wrapper Utility: JCC
    Source Languages: Java
    Documentation: http://pypi.python.org/pypi/JCC/1.5

    
TODO - Java Example


Creating an Extension using Python's ctypes
-------------------------------------------
