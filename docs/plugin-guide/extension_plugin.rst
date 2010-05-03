.. index:: plugin; building using a Python Extension

.. index:: wrapping
.. index:: wrapping; file
.. index:: wrapping; code

Building a Plugin that Contains a Python Extension
==================================================

It will often be the case that there is some existing application, written in some language 
other than Python, that needs to be turned into an openMDAO component. This section and the one 
that follows present some examples illustrating this process. The process of building a Python
extension to some external code is called *wrapping.* There are generally two ways to wrap an
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
- There are no resources to modify legacy code to pass data as arguments

In general, a direct wrap should have better performance and should be less problematic, but
it is not always a viable option. To learn how to file-wrap in OpenMDAO, see 
:ref:`Building-a-Plugin-Using-a-File-Wrapper`.

For the examples presented here, we assume that you are already familiar with the
fundamentals of the OpenMDAO component API and also with creating Python components
as outlined in the :ref:`A-More-Complex-Tutorial-Problem`.

.. index:: extension; creating with F2PY
.. index:: F2PY

Creating an Extension with F2PY
--------------------------------

To support the integration of Fortran codes into Python, F2PY (Fortran to Python Interface
Generator) was developed. It is currently distributed as part of the NumPy package for Python but
is not part of the OpenMDAO bundle at this time. To use F2PY, NumPy needs to be installed
into the default Python environment on your system.

::

    Wrapper Utility: F2PY
    Source Languages: C, FORTRAN 77, Fortran 90/95 (newer Fortran can also be wrapped, but not all
    features are supported) Documentation: http://cens.ioc.ee/projects/f2py2e/usersguide/index.html
    
F2PY automates the process of generating a library that contains Fortran functions callable from Python
as well as an interface into exposed common blocks and Fortran 90/95 module data. Additionally, F2PY
handles the conversion of inputs and output between their representative Python and Fortran types, 
particularly with arrays, which are converted into NumPy's numerical arrays on the Python side. The
most attractive feature of F2PY is its simplicity--it can be quickly learned and used without
understanding the details under the hood.

Note that while F2PY was developed for use with Fortran, it can also wrap C functions with almost as 
much ease. An example of wrapping a C function with F2PY can be found in :ref:`Wrapping-an-External-Module-Using-F2PY`
in the OpenMDAO Tutorial.

To illustrate the creation of an OpenMDAO component from a Fortran function, we'll present a brief
tutorial. The following instructions will help you locate the directory containing the pieces
needed for the model relative to the install directory:

If you have a branch from the source repository:

	``examples/openmdao.examples.bar3simulation/openmdao/examples/bar3simulation``
	
If you have downloaded the latest release version from the website:

	``openmdao-X.X.X/lib/python2.6/site-packages/openmdao.examples.bar3simulation-X.X.X-######.egg/openmdao/examples/bar3simulation``
	
where X.X.X is the current OpenMDAO version, and ###### is a string that
contains the Python version, and the Operating System description. This will
vary depending on your system and version, but there will only be one
*bar3simulation* egg.

It should also be noted that a Fortran compiler is required. The instructions presented here are
applicable to the UNIX and Mac OSX environments. There may be some differences on the Windows
platform.

.. index:: three-bar truss

The Fortran code *bar3.f* contains the subroutine *runbar3truss*, which contains an analytical solution
for a three-bar truss with the following specific geometry:


.. figure:: ../images/plugin-guide/ThreeBar.jpg
   :align: center

   The 3-Bar Truss Geometry
   
The inputs to the problem are the components of the body force acting on node 1 (2d array pvec); the
initial cross-sectional areas of all three structural elements (a1, a2, a3); the lumped mass at node 1 (mo);
the length of bar 2 (el: this essentially scales the problem); and some material properties for the 
bars (e; Young's Modulus, and rho: material density). The outputs of interest are the stresses 
in each bar (s1, s2, s3); the displacement at node 1 (u, v); the frequency of the first mode of
vibration (ff); and the total weight of the structure (obj). The objective of this example is to use
this Fortran subroutine to calculate the optimal cross-sectional areas of the three bars that minimize the total
weight of the structure while satisfying constraints on the bar stresses, the displacement of node
1, and the frequency of the first mode.
   
The `F2PY Users Guide <http://cens.ioc.ee/projects/f2py2e/usersguide/index.html>`_ describes three
ways to use F2PY to generate the Python-callable object. The "quick way" is to just run F2PY on the
Fortran file, which produces a shared object containing a function (or functions) that can be
called from Python. This works for the simplest case but breaks down when F2PY doesn't know which
function arguments are inputs and which are outputs. In the "smart way," the user specifies the
input/output intent of each function in the signature file (extension .pyf). Finally, in the
"quick and smart way," the input/output intents are specified directly in the Fortran code as 
comments.

This example showcases the "quick and smart way." An example of the "smart way" can be found in 
:ref:`Wrapping-an-External-Module-Using-F2PY`, where a signature file is included
as part of the engine design tutorial. The "quick and smart way" should be fine for most cases,
provided there are no objections to inserting new comments into the existing source code. For
some cases, the extra flexibility of the signature file may be needed; one specific example
would be a case where you only want to expose one function from a Fortran file that contains
several. What you can do in this case is instruct F2PY to generate this signature file,
after which you can edit it to your satisfaction.

Subroutine *runbar3truss* has the following interface:

::

      SUBROUTINE runbar3truss(PVEC,M0,A1,A2,A3,E,EL,RHO,
     *                        S1,S2,S3,U,V,FF,OBJ) 
     
The inputs and outputs are described above. To tell F2PY which of these variables are
inputs and which are outputs, a series of comments is inserted after the function header. These
comments are prefaced with *Cf2py*:
     
::

          ...
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
    
The *intent(in)* marks an input, and *intent(out)* denotes an output. If an argument serves as
both an input and output (i.e., it passes a value to the argument and expects a change
upon completion), then *intent(inout)* can be used. There are several other intents that are
useful for other less common cases. One that may be of interest is *intent(callback),* which
can be used to pass a Python (or other) function into a Fortran subroutine.

Once the intents have all been declared, F2PY can be executed to produce the module by
executing the following at the command prompt:

::

    [unix_prompt]$ f2py -c -m bar3 bar3.f
    
The result is the shared object *bar3.so.* The next step is to build a Python component that
can run *runbar3truss*, supplying its inputs and gathering its output. An OpenMDAO wrapper
for *bar3.so* is available as part of this example and can be found in ``bar3_wrap_f.py``. The
functions that were compiled through F2PY are contained in the bar3 library, and this can
be imported into Python just like any Python file:

.. testsetup:: bar3_wrap

    from openmdao.examples.bar3simulation.bar3_wrap_f import Bar3Truss
    from numpy import zeros
    
    self = Bar3Truss()
    
    load = zeros(2,'d')
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
on the right-hand side.

.. testcode:: bar3_wrap

        # Call the Fortran model and pass it what it needs.

        (self.bar1_stress, self.bar2_stress, self.bar3_stress, 
         self.displacement_x_dir, self.displacement_y_dir, 
         self.frequency, self.weight) \
         = runbar3truss(
                    load, lumped_mass, 
                    bar1_area,bar2_area,bar3_area,
                    Youngs_Modulus, bar2_length, weight_density)

F2PY automatically generates a docstring for this function. This can be examined by
opening OpenMDAO's local Python environment:

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
are analogous to Double Precision variables in Fortran. The load is stored in
*pvec,* which is an array that contains the x and y components of the force. To
pass this into the Fortran subroutine, it needs to be in the form of a NumPy
array (in this case, an array of floating point numbers):

.. testcode:: bar3_wrap_array

    from numpy import zeros
    
    load = zeros(2,'d')
    load[0] = 50.0
    load[1] = 100.0

By the same token, NumPy arrays should be used to receive arrays that are returned to
Python by the Fortran function.

Data in the common blocks is also accessible. In this case, the Fortran code stores
the values of the bar forces in a common block as *force1, force2,* and *force3.*

.. testcode:: bar3_wrap

    bar1_force = float(forces.force1)
    bar2_force = float(forces.force2)
    bar3_force = float(forces.force3)
    
There is one oddity here. Scalar variables in the common block get returned to Python as a
zero-dimensional NumPy array. It is not entirely clear why this was done, but their values can be
accessed by casting them as floats or int. Note also that values can also be input into the common
block. In practice, the common block will probably be frequently used for passing variables as
opposed to cluttering the function interface.

Further examples of a more complicated wrap can be seen in the source for the OpenMDAO 
wrapper of the CONMIN optimizer (``conmindriver.py``).

Finally, the OpenMDAO top-level assembly for this problem is given in ``bar3_optimization.py``.
This model integrates the 3-bar truss wrapper and the CONMIN optimizer to solve the full
problem.
 
.. index:: F2PY; Quick Reference
    
**F2PY Quick Command Reference**

============================ =============================
Ordinary Build                f2py -c -m foo foo.f
---------------------------- -----------------------------
Only Make Signature File      f2py -m foo -h foo.pyf foo.f
---------------------------- -----------------------------
Build with Signature foo.pyf  f2py foo.pyf foo.f -c
============================ =============================

.. index:: SWIG

Creating an Extension with SWIG
--------------------------------

The Simplified Wrapper and Interface Generator (SWIG) is a tool that simplifies
the creation of extensions from C and C++ functions for use in a variety of
target languages, including Python. To use SWIG, you must download and install the most recent
version at the system level.

::

    Wrapper Utility: SWIG
    Source Languages: C, C++
    Documentation: http://www.swig.org/doc.html

SWIG is a bit more complicated than F2PY, so you are strongly encouraged to read
the documentation and experiment with their `example problem`__ before
attempting to wrap your own C or C++ codes.

The first step in creating a Python extension is to create the interface file for
the C functions that are to be wrapped. The interface file is analogous to the
signature file that F2PY uses, though its format is more like C. For example,
consider the engine simulation as described in the :ref:`A-More-Complex-Tutorial-Problem`.
There is one function with inputs and outputs effectively passed as arguments. The
corresponding interface file would look like this:

.. __: http://www.swig.org/tutorial.html

::

    /* engineC_SWIG.i */
 
    %module engineC_SWIG_wrap
 
    %{
        /* Put header files here or function declarations like below */
         void RunEngineCycle(double stroke, double bore, double conrod, double compRatio, double sparkAngle,
                          int nCyl, double IVO, double IVC, double Liv, double Div, double k,
                          double R, double Ru, double Hu, double Tw, double AFR, double Pexth,
                          double Tamb, double Pamb, double Air_Density, double MwAir, double MwFuel,
                          double RPM, double Throttle, double thetastep, double Fuel_Density,
                          double *Power, double *Torque, double *FuelBurn, double *EngineWeight);
    %}

    void RunEngineCycle(double stroke, double bore, double conrod, double compRatio, double sparkAngle,
                        int nCyl, double IVO, double IVC, double Liv, double Div, double k,
                        double R, double Ru, double Hu, double Tw, double AFR, double Pexth,
                        double Tamb, double Pamb, double Air_Density, double MwAir, double MwFuel,
                        double RPM, double Throttle, double thetastep, double Fuel_Density,
                        double *OUTPUT, double *OUTPUT, double *OUTPUT, double *OUTPUT);

Notice that the variables *Power, Torque, FuelBurn,* and *EngineWeight* are 
declared as outputs. Inputs don't have to be explicitly declared, although the keyword *INPUT*
should be used whenever a pointer is actually a single input value. If a variable
functions as both an input and an output, use the keyword *BOTH* in the interface file.

Generating the importable shared object from this interface is a 4-step process.

1. Run SWIG on the interface file, using Python as the target.
2. Compile the original C function on your system. (Not needed if you already have a library that contains this function.)
3. Compile the code generated in step 1.
4. Link the libraries from steps 2 and 3 (along with any other required externals) to create the shared object.

For the engine example, on a UNIX environment with GCC as the compiler, these 
steps look like this:
    
::

    swig -python engineC_SWIG.i

    gcc -fPIC -c engineC.c -I/usr/local/include/python2.6
    gcc -fPIC -c engineC_SWIG_wrap.c -I/usr/local/include/python2.6

    gcc -shared engineC.o engineC_SWIG_wrap.o -lGLU -lGL -lX11 -lXext -lpthread /usr/lib64/libstdc++.so.6 -lm -o _engineC_SWIG_wrap.so

One common mistake is to give the interface file and the shared object the same name. 
Python must be able to import them independently, so name collision should
be avoided (i.e., ``z.py`` and ``z.so`` in the same namespace). In this case, an 
underscore was prepended to the name of the shared object in the link command
to avoid this problem.
    
On the Python side, interaction with this object differs little from the one we
created with F2PY:

::

    from engineC_SWIG_wrap import RunEngineCycle
    ...
    # Call the C model and pass it what it needs.
        
    (power, torque, fuel_burn, engine_weight) = RunEngineCycle(
                stroke, bore, conrod, comp_ratio, spark_angle,
                n_cyl, IVO, IVC, L_v, D_v, k,
                R, Ru, Hu, Tw, AFR, P_exth,
                T_amb, P_amb, air_density, mw_air, mw_fuel,
                RPM, throttle, thetastep, fuel_density)
        
    # Interrogate results of engine simulation and store.
        
    self.power = power
    self.torque = torque
    self.fuel_burn = fuel_burn
    self.engine_weight = engine_weight

The only difference here is that the outputs are returned as single value
variables instead of the zero-dimensional lists that F2PY returns whenever
it generates the interface for a C function.    
    
TODO - C++ Example

TODO - SWIG helpful hints


Creating an Extension with JCC
------------------------------

::

    Wrapper Utility: JCC
    Source Languages: Java
    Documentation: http://pypi.python.org/pypi/JCC/1.5

    
TODO - Java Example


Creating an Extension using Python's ctypes
-------------------------------------------

TODO - Example wrap for an existing C dynamic link library
