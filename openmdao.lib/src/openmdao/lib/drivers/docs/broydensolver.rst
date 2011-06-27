.. index:: BroydenSolver

.. _BroydenSolver:

*BroydenSolver*
~~~~~~~~~~~~~~~~

The BroydenSolver can be used to solve for the set of inputs
(independents) that are needed to to make a model satisfy an equation (the
dependent equation) that is a function of model outputs. BroydenSolver is based
on the quasi-Newton-Raphson algorithms found in SciPy's nonlinear solver library.
As the name implies, a Broyden update is used to approximate the Jacobian matrix.
In fact, no Jacobian is evaluated with these algorithms, so they are quicker than
a full Newton solver, but they may not be suitable for all problems.

To see how to use the BroydenSolver, consider a problem where we'd like to solve
for the intersection of a line and a parabola. We can implement this as a single
component. 

.. testcode:: Broyden

    from openmdao.lib.datatypes.api import Float
    from openmdao.main.api import Component
    
    
    class MIMOSystem(Component):
        """ Two equations, two unknowns """
        
        x = Float(10.0, iotype="in", doc="Input 1")
        y = Float(10.0, iotype="in", doc="Input 2")
        
        f_xy = Float(0.0, iotype="out", doc="Output 1")
        g_xy = Float(0.0, iotype="out", doc="Output 2")
        
        def execute(self):
            """ Evaluate:
            f_xy = 2.0*x**2 - y + 2.0 
            g_xy = 2.0*x - y - 4.0 
            """
          
            x = self.x
            y = self.y
            
            self.f_xy = 2.0*x**2 - y + 2.0 
            self.g_xy = 2.0*x - y + 4.0 

Notice that this is a two-input problem -- the variables are *x* and *y*. There are
also two equations that need to be satisfied: the equation for the line and
the equation for the parabola. There are actually two solutions to this set
of equations. The solver will return the first one that it finds. You can
usually find other solutions by starting the solution from different initial
points. We start at ``(10, 10)``, as designated by the default values for the variables
*x* and *y*.

Next, we build a model that uses the BroydenSolver to find a root for the 
equations defined in MIMOSystem.

.. testcode:: Broyden

    from openmdao.lib.drivers.api import BroydenSolver
    from openmdao.main.api import Assembly
    
    class SolutionAssembly(Assembly):
        """ Solves for the root of MIMOSystem. """
    
        def __init__(self):
            """ Creates a new Assembly with this problem
            root at (0,1)
            """
            
            super(SolutionAssembly, self).__init__()    
            
            self.add('driver', BroydenSolver())
            self.add('problem', MIMOSystem())
        
            self.driver.workflow.add('problem')
        
            self.driver.add_parameter('problem.x', low=-1.0e99, high=1.0e99)
            self.driver.add_parameter('problem.y', low=-1.0e99, high=1.0e99)
        
            self.driver.add_constraint('problem.f_xy = 0.0')
            self.driver.add_constraint('problem.g_xy = 0.0')
            self.driver.itmax = 20
            self.driver.alpha = .4
            self.driver.tol = .000000001
            
The parameters are the independent variables that the solver is allowed to vary. The
method ``add_parameter`` is used to define these. Broyden does not utilize
the low and high arguments, so they are set to some large arbitrary negative and positive values.

The equations that we want to satisfy are added as equality constraints using the
``add_constraint`` method. We want to find *x* and *y* that satisfy ``f_xy=0`` and ``g_xy=0``,
so these two equations are added to the solver.

Both the ``add_parameter`` and ``add_constraint`` methods are presented in more detail in
:ref:`Tutorial:-MDAO-Architectures`.

The resulting solution should yield:

.. doctest:: Broyden

    >>> top = SolutionAssembly()
    >>> top.run()
    >>> print top.problem.x, top.problem.y
    1.61... 7.23...

.. index:: algorithm, Enum, SciPy

Five parameters control the solution process in the BroydenSolver.

**algorithm** 
  SciPy's nonlinear package contained several algorithms for solving
  a set of nonlinear equations. Three of these methods were considered by their
  developers to be of good quality, so those three were implemented as part of 
  the BroydenSolver. The variable ``algorithm`` is an Enum where the following values
  represent the algorithms that follow.

- ``broyden2``: Broyden's second method -- the same as ``broyden1`` but
  updates the inverse Jacobian directly
- ``broyden3``: Broyden's third method -- the same as ``broyden2``, but instead of
  directly computing the inverse Jacobian, it remembers how to construct it using
  vectors. When computing ``inv(J)*F``, it uses those vectors to compute this
  product, thus avoiding the expensive NxN matrix multiplication. 
- ``excitingmixing``: The excitingmixing algorithm. ``J=-1/alpha``

  The default value for ``algorithm`` is ``"broyden2"``.

  .. testsetup:: Broyden3

    from openmdao.lib.drivers.api import BroydenSolver
    from openmdao.main.api import Assembly
    
    self = Assembly()
    self.add('driver', BroydenSolver())

  .. testcode:: Broyden3

    self.driver.algorithm = "broyden2"
    
**itmax** 
  This parameter specifies the maximum number of iterations before
  BroydenSolver terminates. The default value is 10.
    
  .. testcode:: Broyden3

    self.driver.itmax = 10
    
**alpha** 
  This parameter specifies the mixing coefficient for the algorithm. The
  mixing coefficient is a linear scale factor applied to the update of the parameters, so
  increasing it can lead to quicker convergence but can also lead to instability. The 
  default value is 0.4. If you use the ``excitingmixing`` algorithm, you should try a lower
  value, such as 0.1.
    
  .. testcode:: Broyden3

    self.driver.alpha = 0.1
    
**tol** 
  Convergence tolerance for the solution. Iteration ends when the constraint
  equation is satisfied within this tolerance. The default value is 0.00001.
    
  .. testcode:: Broyden3

    self.driver.tol = 0.00001
    
**alphamax** 
  This parameter is only used for the ``excitingmixing`` algorithm
  where the mixing coefficient is adaptively adjusted. It specifies the maximum
  allowable mixing coefficient for adaptation. The default value is 1.0.

  .. testcode:: Broyden3

    self.driver.alphamax = 1.0
    
*Source Documentation for broyensolver.py*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
