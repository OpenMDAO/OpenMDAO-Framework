Using scalers and adders
========================================

An example problem requiring scaling
-------------------------------------

In this example, the purpose of scalers and adders is demonstrated. To this end, an elliptic paraboloid function `f` is minimized with respect to two input variables, `x` and `y`. 
This function is given by

f(x,y) = (1000*x-3)^2 + (1000*x)*(0.01*y) + (0.01*y+4)^2 - 3,

and is implemented as the following openMDAO component:

.. testcode:: simple_component_Paraboloid_scale
	from openmdao.main.api import Assembly,Component
	from openmdao.lib.drivers.api import SLSQPdriver
    from openmdao.lib.datatypes.api import Float
    
    
	class Paraboloid_scale(Component):
		""" Evaluates the equation f(x,y) = (1000*x-3)^2 + (1000*x)*(0.01*y) + (0.01*y+4)^2 - 3 """
		
		# set up interface to the framework  
		# pylint: disable-msg=E1101
		x = Float(0.0, iotype='in', desc='The variable x')
		y = Float(0.0, iotype='in', desc='The variable y')

		f_xy = Float(iotype='out', desc='F(x,y)')        

			
		def execute(self):
			"""f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
			Optimal solution (minimum): x = 0.0066666666666666671; y = -733.33333333333337
			"""
			
			x = self.x
			y = self.y
			
			self.f_xy = (1000.*x-3.)**2 + (1000.*x)*(0.01*y) + (0.01*y+4.)**2 - 3.

			
It can be shown that `f` has a global minimum at `x` = 1/150 = 0.00666667 and `y` = -2200/3 = -733.333333. The gradient of this function is computed to be {20^6 x+10y - 6000, 0.08+10x+0.0002y}, showing that `f` is far more sensitive to variation in `x` than variation in `y` . Numerical optimization of `f` may not converge to the true minimum due to this difference in sensitivity, or may require a large number of iterations to reach the true minimum compared to a well-scaled problem. This is seen by performing a minimization (with SLSQP) using the following assembly:


.. testcode:: simple_component_Paraboloid_scale


	class OptimizationUnconstrainedScale(Assembly):
		"""Unconstrained optimization of the unscaled Paraboloid Component."""
		
		def configure(self):
			""" Creates a new Assembly containing an unscaled Paraboloid and an optimizer"""
			
			# Create Optimizer instance
			self.add('driver', SLSQPdriver())
			
			# Create Paraboloid component instances
			self.add('paraboloid', Paraboloid_scale())

			# Driver process definition
			self.driver.workflow.add('paraboloid')
			
			# SQLSQP Flags
			self.driver.iprint = 0
			
			# Objective 
			self.driver.add_objective('paraboloid.f_xy')
			
			# Design Variables 
			self.driver.add_parameter('paraboloid.x', low=-1000., high=1000.)
			self.driver.add_parameter('paraboloid.y', low=-1000., high=1000.)
			
Running this assembly gives the estimated minimizing `x` and `y` :

::


	>>> import time 
	>>> opt_problem = OptimizationUnconstrainedScale()
	>>> tt = time.time()
	>>> opt_problem.run()
	>>> print opt_problem.paraboloid.x,opt_problem.paraboloid.y
	(0.006667, -733.334333)
	>>> print "Elapsed time: ", time.time()-tt, "seconds"
	Elapsed time:  0.0160000324249 seconds
	
Now, consider modifying the assembly so that `x` and `y` have scalers specified when added to the assembly's driver:

.. testcode:: simple_component_Paraboloid_scale

	self.driver.add_parameter('paraboloid.x', low=-1000., high=1000.,scaler=0.001)
	self.driver.add_parameter('paraboloid.y', low=-1000., high=1000.,scaler=1000)     
	
Running the assembly with these specifications for `x` and `y` gives:

::

     
	>>> opt_problem = OptimizationUnconstrainedScale()
	>>> tt = time.time()
	>>> opt_problem.run()
	>>> print opt_problem.paraboloid.x,opt_problem.paraboloid.y
	(0.006667, -733.333313)
	>>> print "Elapsed time: ", time.time()-tt, "seconds"
	Elapsed time:  0.0 seconds

So, the computed minimizers of `f` are closer to the true minimizers of `f` when scaling is used. Furthermore, this optimization was computed more quickly with scaling than without scaling.