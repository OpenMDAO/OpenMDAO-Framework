"""
    engine_optimization.py - Top level assembly for the example problem.
"""

# Optimize an engine disign using the driving_sim component.

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Assembly, set_as_top
from openmdao.lib.drivers.api import CONMINdriver

from openmdao.examples.enginedesign.driving_sim import DrivingSim
from openmdao.examples.enginedesign.vehicle import Vehicle

class EngineOptimization(Assembly):
    """Optimization of a Vehicle."""
    
    def __init__(self):
        """ Creates a new Assembly containing a DrivingSim and an optimizer"""
        
        super(EngineOptimization, self).__init__()

        # pylint: disable-msg=E1101
        
        # Create CONMIN Optimizer instance
        self.add('driver', CONMINdriver())
        
        # Create DrivingSim instance
        self.add('driving_sim', DrivingSim())
        
        # Add Vehicle instance to vehicle socket
        self.driving_sim.add('vehicle', Vehicle())
        
        # add DrivingSim to workflow
        self.driver.workflow.add('driving_sim')

        # CONMIN Flags
        self.driver.iprint = 0
        self.driver.itmax = 30
        
        # CONMIN Objective 
        #self.driver.add_objective('driving_sim.accel_time')
        self.driver.add_objective('driving_sim.accel_time')
        
        # CONMIN Design Variables 
        self.driver.add_parameters([('driving_sim.spark_angle', -50., 10.),
                                    ('driving_sim.bore', 65., 100.)])

if __name__ == "__main__": # pragma: no cover         

    # pylint: disable-msg=E1101

    def prz(title):
        """ Print before and after"""
        
        print '---------------------------------'
        print title
        print '---------------------------------'
        print 'Engine: Bore = ', opt_problem.driving_sim.bore
        print 'Engine: Spark Angle = ', opt_problem.driving_sim.spark_angle
        print '---------------------------------'
        print '0-60 Accel Time = ', opt_problem.driving_sim.accel_time
        print 'EPA City MPG = ', opt_problem.driving_sim.EPA_city
        print 'EPA Highway MPG = ', opt_problem.driving_sim.EPA_highway
        print '\n'
    

    import time
    #import profile
    
    opt_problem = EngineOptimization()
    set_as_top(opt_problem)
    
    opt_problem.driving_sim.run()
    prz('Old Design')

    tt = time.time()
    opt_problem.run()
    prz('New Design')
    print "CONMIN Iterations: ", opt_problem.driver.iter_count
    print ""
    print "Elapsed time: ", time.time()-tt
    
# end engine_optimization.py
