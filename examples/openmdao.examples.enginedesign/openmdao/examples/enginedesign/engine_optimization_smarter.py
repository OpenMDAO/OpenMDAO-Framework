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
    
    def configure(self):

        # pylint: disable-msg=E1101
        
        # Create CONMIN Optimizer instance
        self.add('optimizer', CONMINdriver())
        
        # Create Vehicle instance
        self.add('vehicle', Vehicle())
        
        # Create Driving Simulation instances
        self.add('sim_acc', SimAcceleration())
        self.add('sim_EPA_city', SimEconomy())
        self.add('sim_EPA_highway', SimEconomy())
        
        # add the optimizer and economy sims to driver workflow
        self.driver.workflow.add(['optimizer', 'sim_EPA_city', 'sim_EPA_highway'])
        
        # add the acceleration sim to the optimizer workflow
        self.optimizer.workflow.add('sim_acc')
        
        # Add vehicle to sim workflows.
        self.sim_acc.workflow.add('vehicle')
        self.sim_EPA_city.workflow.add('vehicle')
        self.sim_EPA_highway.workflow.add('vehicle')
    
        # CONMIN Flags
        self.optimizer.iprint = 0
        self.optimizer.itmax = 30
        
        # CONMIN Objective 
        self.optimizer.add_objective('sim_acc.accel_time')
        
        # CONMIN Design Variables 
        self.optimizer.add_parameter('vehicle.spark_angle', -50., 10.)
        self.optimizer.add_parameter('vehicle.bore', 65., 100.)
        
        # Acceleration Sim setup
        self.sim_acc.velocity_str = 'vehicle.velocity'
        self.sim_acc.throttle_str = 'vehicle.throttle'
        self.sim_acc.gear_str = 'vehicle.current_gear'
        self.sim_acc.acceleration_str = 'vehicle.acceleration'
        self.sim_acc.overspeed_str = 'vehicle.overspeed'
        
        # EPA City MPG Sim Setup
        self.sim_EPA_city.velocity_str = 'vehicle.velocity'
        self.sim_EPA_city.throttle_str = 'vehicle.throttle'
        self.sim_EPA_city.gear_str = 'vehicle.current_gear'
        self.sim_EPA_city.acceleration_str = 'vehicle.acceleration'
        self.sim_EPA_city.fuel_burn_str = 'vehicle.fuel_burn'
        self.sim_EPA_city.overspeed_str = 'vehicle.overspeed'
        self.sim_EPA_city.underspeed_str = 'vehicle.underspeed'
        self.sim_EPA_city.profilename = 'EPA-city.csv'
        
        # EPA Highway MPG Sim Setup
        self.sim_EPA_highway.velocity_str = 'vehicle.velocity'
        self.sim_EPA_highway.throttle_str = 'vehicle.throttle'
        self.sim_EPA_highway.gear_str = 'vehicle.current_gear'
        self.sim_EPA_highway.acceleration_str = 'vehicle.acceleration'
        self.sim_EPA_highway.fuel_burn_str = 'vehicle.fuel_burn'
        self.sim_EPA_highway.overspeed_str = 'vehicle.overspeed'
        self.sim_EPA_highway.underspeed_str = 'vehicle.underspeed'
        self.sim_EPA_highway.profilename = 'EPA-highway.csv'        

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
    
    opt_problem = EngineOptimization()
    
    opt_problem.sim_acc.run()
    opt_problem.sim_EPA_city.run()
    opt_problem.sim_EPA_highway.run()
    prz('Old Design')

    tt = time.time()
    opt_problem.run()
    prz('New Design')
    print "CONMIN Iterations: ", opt_problem.optimizer.iter_count
    print ""
    print "Elapsed time: ", time.time()-tt
    
# end engine_optimization.py
