# engine_optimization.py
#
# Optimize an engine disign using the driving_sim component.

from openmdao.main import Assembly

from openmdao.lib.drivers.conmindriver import CONMINdriver

from openmdao.examples.engine_design.driving_sim import DrivingSim

class EngineOptimization(Assembly):
    """ Top level assembly for optimizing a vehicle. """
    
    def __init__(self, name, parent=None, directory=''):
        ''' Creates a new Assembly containing a DrivingSim and an optimizer'''
        
        super(EngineOptimization, self).__init__(name, parent, directory)

        # Create DrivingSim component instances
        DrivingSim('driving_sim', parent=self)

        # Create CONMIN Optimizer instance
        CONMINdriver('driver', self)
        
        # CONMIN Flags
        self.driver.iprint = 0
        self.driver.maxiters = 30
        
        # CONMIN Objective 
        self.driver.objective.value = 'driving_sim.accel_time'
        
        # CONMIN Design Variables 
        self.driver.design_vars.value = ['driving_sim.spark_angle', 
                                         'driving_sim.bore' ]
        
        self.driver.lower_bounds = [-50, 65]
        self.driver.upper_bounds = [10, 100]
        
    
if __name__ == "__main__":

    def prz(title):
        ''' Print before and after'''
        
        print '---------------------------------'
        print title
        print '---------------------------------'
        print 'Engine: Bore = ', z.driving_sim.get('bore')
        print 'Engine: Spark Angle = ', z.driving_sim.get('spark_angle')
        print '---------------------------------'
        print '0-60 Accel Time = ', z.driving_sim.accel_time
        print 'EPA City MPG = ', z.driving_sim.EPA_city
        print 'EPA Highway MPG = ', z.driving_sim.EPA_highway
        print '\n'
    

    import time
    #import profile
    
    z = EngineOptimization("Top")
    
    z.driving_sim.run()
    prz('Old Design')

    tt = time.time()
    z.run()
    #profile.run('z.run()')
    prz('New Design')
    print "Elapsed time: ", time.time()-tt
    
# end engine_optimization.py
