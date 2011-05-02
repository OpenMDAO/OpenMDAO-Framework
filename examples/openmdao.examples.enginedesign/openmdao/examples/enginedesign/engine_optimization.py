"""
    engine_optimization.py - Top level assembly for the example problem.
"""

# Optimize a Vehicle design with CONMIN

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Assembly
from openmdao.lib.drivers.api import CONMINdriver

from openmdao.examples.enginedesign.driving_sim import SimAcceleration, \
                                                       SimEconomy
from openmdao.examples.enginedesign.vehicle import Vehicle

class EngineOptimization(Assembly):
    """Optimization of a Vehicle."""
    
    def __init__(self):
        """ Creates a new Assembly for vehicle performance optimization."""
        
        super(EngineOptimization, self).__init__()

        # pylint: disable-msg=E1101
        
        # Create CONMIN Optimizer instance
        self.add('driver', CONMINdriver())
        
        # Create Vehicle instance
        self.add('vehicle', Vehicle())
        
        # Create Driving Simulation instances
        self.add('sim_acc', SimAcceleration())
        self.add('sim_EPA_city', SimEconomy())
        self.add('sim_EPA_highway', SimEconomy())
        
        # add Sims to optimizer workflow
        self.driver.workflow.add(['sim_acc', 'sim_EPA_city', 'sim_EPA_highway'])
        
        # Add vehicle to sim workflows.
        self.sim_acc.workflow.add('vehicle')
        self.sim_EPA_city.workflow.add('vehicle')
        self.sim_EPA_highway.workflow.add('vehicle')
    
        # CONMIN Flags
        self.driver.iprint = 0
        self.driver.itmax = 30
        
        # CONMIN Objective 
        self.driver.add_objective('sim_acc.accel_time')
        
        # CONMIN Design Variables 
        self.driver.add_parameters([('vehicle.spark_angle', -50., 10.),
                                    ('vehicle.bore', 65., 100.)])
        
        # Acceleration Sim setup
        self.sim_acc.add_parameters([('vehicle.velocity', 0, 99999),
                                   ('vehicle.throttle', 0.01, 1.0),
                                   ('vehicle.current_gear', 0, 5)])
        self.sim_acc.add_objective('vehicle.acceleration')
        self.sim_acc.add_objective('vehicle.overspeed')
        
        # EPA City MPG Sim Setup
        self.sim_EPA_city.add_parameters([('vehicle.velocity', 0, 99999),
                                         ('vehicle.throttle', 0.01, 1.0),
                                         ('vehicle.current_gear', 0, 5)])
        self.sim_EPA_city.add_objective('vehicle.acceleration')
        self.sim_EPA_city.add_objective('vehicle.fuel_burn')
        self.sim_EPA_city.add_objective('vehicle.overspeed')
        self.sim_EPA_city.add_objective('vehicle.underspeed')
        self.sim_EPA_city.profilename = 'EPA-city.csv'
        self.sim_EPA_city.force_execute = True
        
        # EPA Highway MPG Sim Setup
        self.sim_EPA_highway.add_parameters([('vehicle.velocity', 0, 99999),
                                            ('vehicle.throttle', 0.01, 1.0),
                                            ('vehicle.current_gear', 0, 5)])
        self.sim_EPA_highway.add_objective('vehicle.acceleration')
        self.sim_EPA_highway.add_objective('vehicle.fuel_burn')
        self.sim_EPA_highway.add_objective('vehicle.overspeed')
        self.sim_EPA_highway.add_objective('vehicle.underspeed')
        self.sim_EPA_highway.profilename = 'EPA-highway.csv'        
        self.sim_EPA_highway.force_execute = True

if __name__ == "__main__": # pragma: no cover         

    # pylint: disable-msg=E1101

    def prz(title):
        """ Print before and after"""
        
        print '---------------------------------'
        print title
        print '---------------------------------'
        print 'Engine: Bore = ', opt_problem.vehicle.bore
        print 'Engine: Spark Angle = ', opt_problem.vehicle.spark_angle
        print '---------------------------------'
        print '0-60 Accel Time = ', opt_problem.sim_acc.accel_time
        print 'EPA City MPG = ', opt_problem.sim_EPA_city.fuel_economy
        print 'EPA Highway MPG = ', opt_problem.sim_EPA_highway.fuel_economy
        print '\n'
    

    import time
    from openmdao.main.api import set_as_top
    
    opt_problem = EngineOptimization()
    set_as_top(opt_problem)
    
    opt_problem.sim_acc.run()
    opt_problem.sim_EPA_city.run()
    opt_problem.sim_EPA_highway.run()
    prz('Old Design')

    tt = time.time()
    opt_problem.run()
    prz('New Design')
    print "CONMIN Iterations: ", opt_problem.driver.iter_count
    print ""
    print "Elapsed time: ", time.time()-tt
    
# end engine_optimization.py
