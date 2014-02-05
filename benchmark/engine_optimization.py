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
        self.driver.conmin_diff = True

        # CONMIN Objective
        self.driver.add_objective('sim_acc.accel_time')

        # CONMIN Design Variables
        self.driver.add_parameter('vehicle.spark_angle', -50., 10.)
        self.driver.add_parameter('vehicle.bore', 65., 100.)

        # Acceleration Sim setup
        self.sim_acc.add_parameter('vehicle.velocity', name='velocity',
                                  low=0.0, high=150.0)
        self.sim_acc.add_parameter('vehicle.throttle', name='throttle',
                                  low=0.01, high=1.0)
        self.sim_acc.add_parameter('vehicle.current_gear', name='gear',
                                  low=0, high=5)
        self.sim_acc.add_objective('vehicle.acceleration', name='acceleration')
        self.sim_acc.add_objective('vehicle.overspeed', name='overspeed')

        # EPA City MPG Sim Setup
        self.sim_EPA_city.add_parameter('vehicle.velocity', name='velocity',
                                  low=0.0, high=150.0)
        self.sim_EPA_city.add_parameter('vehicle.throttle', name='throttle',
                                  low=0.01, high=1.0)
        self.sim_EPA_city.add_parameter('vehicle.current_gear', name='gear',
                                  low=0, high=5)
        self.sim_EPA_city.add_objective('vehicle.acceleration', name='acceleration')
        self.sim_EPA_city.add_objective('vehicle.fuel_burn', name='fuel_burn')
        self.sim_EPA_city.add_objective('vehicle.overspeed', name='overspeed')
        self.sim_EPA_city.add_objective('vehicle.underspeed', name='underspeed')
        self.sim_EPA_city.profilename = 'EPA-city.csv'

        # EPA Highway MPG Sim Setup
        self.sim_EPA_highway.add_parameter('vehicle.velocity', name='velocity',
                                  low=0.0, high=150)
        self.sim_EPA_highway.add_parameter('vehicle.throttle', name='throttle',
                                  low=0.01, high=1.0)
        self.sim_EPA_highway.add_parameter('vehicle.current_gear', name='gear',
                                  low=0, high=5)
        self.sim_EPA_highway.add_objective('vehicle.acceleration', name='acceleration')
        self.sim_EPA_highway.add_objective('vehicle.fuel_burn', name='fuel_burn')
        self.sim_EPA_highway.add_objective('vehicle.overspeed', name='overspeed')
        self.sim_EPA_highway.add_objective('vehicle.underspeed', name='underspeed')
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

    import sys
    if '-prof' in sys.argv:
        import cProfile
        import pstats
        sys.argv.remove('-prof') #unittest doesn't like -prof
        cProfile.run('opt_problem.run()', 'profout')
        p = pstats.Stats('profout')
        p.strip_dirs()
        p.sort_stats('cumulative', 'time')
        p.print_stats()
        print '\n\n---------------------\n\n'
        p.print_callers()
        print '\n\n---------------------\n\n'
        p.print_callees()
    else:
        tt = time.time()
        opt_problem.run()

        prz('New Design')
        print "CONMIN Iterations: ", opt_problem.driver.iter_count
        print ""
        print "Elapsed time: ", time.time()-tt

# end engine_optimization.py
