# engine_optimization.py
#
# Optimize an engine disign using the sim_vehicle component.

from openmdao.main import Assembly

from openmdao.lib.drivers.conmindriver import CONMINdriver

from openmdao.examples.engine_design.sim_vehicle import SimVehicle

class EngineOptimization(Assembly):
    """ Top level assembly for optimizing a vehicle. """
    
    def __init__(self, name, parent=None, directory=''):
        ''' Creates a new Assembly containing a SimVehicle and an optimizer'''
        
        super(EngineOptimization, self).__init__(name, parent, directory)

        # Create SimVehicle component instances
        SimVehicle('vehicle_sim', parent=self)

        # Create CONMIN Optimizer instance
        CONMINdriver('driver', self)
        
        # CONMIN Flags
        self.driver.iprint = 0
        self.driver.maxiters = 30
        
        # CONMIN Objective 
        self.driver.objective.value = 'vehicle_sim.accel_time'
        
        # CONMIN Design Variables 
        self.driver.design_vars.value = ['vehicle_sim.spark_angle', 
                                         'vehicle_sim.bore' ]
        
        self.driver.lower_bounds = [-50, 65]
        self.driver.upper_bounds = [10, 100]
        
    
if __name__ == "__main__":

    def prz(title):
        ''' Print before and after'''
        
        print '---------------------------------'
        print title
        print '---------------------------------'
        print 'Engine: Bore = ', z.vehicle_sim.get('bore')
        print 'Engine: Spark Angle = ', z.vehicle_sim.get('spark_angle')
        print '---------------------------------'
        print '0-60 Accel Time = ', z.vehicle_sim.accel_time
        print 'EPA City MPG = ', z.vehicle_sim.EPA_city
        print 'EPA Highway MPG = ', z.vehicle_sim.EPA_highway
        print '\n'
    

    import time
    #import profile
    
    z = EngineOptimization("Top")
    
    z.vehicle_sim.run()
    prz('Old Design')

    tt = time.time()
    z.run()
    #profile.run('z.run()')
    prz('New Design')
    print "Elapsed time: ", time.time()-tt
    
# end engine_optimization.py
