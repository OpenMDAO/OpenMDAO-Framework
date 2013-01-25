.. index:: Fixed Point Iterator

.. _FixedPointIterator:

*FixedPointIterator*
~~~~~~~~~~~~~~~~~~~~

The FixedPointIterator is a simple solver that can solve a single-input
single-output problem using fixed point iteration. It provides a way
to iterate on a single input to match an output. In other words, fixed
point iteration can be used to solve the equation ``x = f(x)``. By extension,
FixedPointIterator can be used to close a loop in the data flow. The
algorithm is useful for some problems, so it is included here.
However, it may require more functional evaluations than the BroydenSolver.

As an example, let's implement a component that can be run iteratively to
produce the square root of a number.

.. testcode:: FPI

    from openmdao.lib.datatypes.api import Float
    from openmdao.main.api import Component
    
    
    class Babylonian(Component):
        """ The Babylonians had a neat way of calculating square
        roots using Fixed Point Iteration"""
        
        x = Float(1.0, iotype="in", desc="Input is x")
        y = Float(iotype="out", desc="Output is y")
        
        def execute(self):
            """ Iterate to find the square root of 2, the Babylonian way:
            """
          
            x = self.x
            self.y = 0.5*(2.0/x + x)
            
An assembly with this component and the FixedPointIterator would look
like this.

.. testcode:: FPI

    from openmdao.lib.drivers.api import FixedPointIterator
    from openmdao.main.api import Assembly
    
    class SolutionAssembly(Assembly):
        """ Solves for the root of MIMOSystem. """
    
        def configure(self):
            """ Creates a new Assembly with this problem
            the answer should be 1.4142.....
            """
            
            self.add('driver', FixedPointIterator())
            self.add('problem', Babylonian())
        
            self.driver.workflow.add('problem')
            
            # Set our independent and dependent
            self.driver.add_parameter('problem.x', low=-9.e99, high=9.e99)
            self.driver.add_constraint('problem.y = problem.x')

The *x* input and the *F(x)* output equation are specified as string expressions using the
``add_parameter`` and ``add_constraint`` methods. The constraint contains the
equation ``x = f(x)``, which we are trying to solve. Note that this is a single-input
single-output method, so it is only valid to specify one constraint and one parameter.
            
.. doctest:: FPI

    >>> top = SolutionAssembly()
    >>> top.run()
    >>> print top.problem.x
    1.4142...

Two additional parameters control the FixedPointIterator. The
parameter `tolerance` sets the convergence tolerance for the comparison
between value of ``x_out`` at the current iteration and the previous iteration.
The default value for `tolerance` is 0.001. The parameter ``max_iteration``
specifies the number of iterations to run. The default value for
``max_iterations`` is 25.

A more useful example in which the FixedPointIterator is used to converge two
coupled components is shown in :ref:`Tutorial:-MDAO-Architectures`.


*Source Documentation for iterate.py*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
