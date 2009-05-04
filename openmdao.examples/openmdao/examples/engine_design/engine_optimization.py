# engine_optimization.py
#
# Optimize an engine disign using the sim_vehicle component.

from openmdao.main import Model, Float, Int
from openmdao.main.variable import INPUT, OUTPUT

from openmdao.examples.engine_design.sim_vehicle import Sim_Vehicle
from openmdao.lib.drivers.conmindriver import CONMINdriver

class Engine_Optimization(Model):
    """ Engine_Optimization model. """
    
    def __init__(self, name, parent=None, directory=''):
        ''' Creates a new Model containing a Sim_Vehicle and an optimizer'''
        
        super(Engine_Optimization, self).__init__(name, parent, directory)

        # Create Sim_Vehicle component instances
        vehicle_sim = Sim_Vehicle('vehicle_sim', parent=self)
        self.add_child(vehicle_sim)
        #self.workflow.add_node(vehicle_sim)

        # Create CONMIN Optimizer instance
        CONMINdriver('driver', self)
        
        # CONMIN Flags
        self.driver.iprint = 0
        self.driver.maxiters = 30
        
        # CONMIN Objective 
        self.driver.objective = 'vehicle_sim.AccelTime'
        
        # CONMIN Design Variables 
        self.driver.design_vars = ['vehicle_sim.sparkAngle', 
                                       'vehicle_sim.bore' ]
        
        # CONMIN Constraint
        # TODO: Conmin driver currently doesn't work if you don't have a
        # constraint, so we add 1>0, which is never violated
        self.driver.constraints = ['1']

        self.driver.lower_bounds = [-50, 65]
        self.driver.upper_bounds = [10, 100]
        
    
if __name__ == "__main__":

    def prz(Title):
        print '---------------------------------'
        print Title
        print '---------------------------------'
        print 'Engine: Bore = ', z.vehicle_sim.bore
        print 'Engine: Spark Angle = ', z.vehicle_sim.sparkAngle
        print '---------------------------------'
        print '0-60 Accel Time = ', z.vehicle_sim.AccelTime
        print 'EPA City MPG = ', z.vehicle_sim.EPACity
        print 'EPA Highway MPG = ', z.vehicle_sim.EPAHighway
        print '\n'
    

    import time
    import profile
    
    z = Engine_Optimization("Top")
    
    z.vehicle_sim.run()
    prz('Old Design')

    tt = time.time()
    z.run()
    #profile.run('z.run()')
    prz('New Design')
    print "Elapsed time: ", time.time()-tt
    
# end engine_optimization.py
