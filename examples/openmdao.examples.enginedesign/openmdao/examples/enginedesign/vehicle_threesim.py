"""
    Model that simulates the 0-60 acceleration time, the EPA city fuel economy,
    and the EPA highway fuel economy of a Vehicle.
"""

from openmdao.main.api import Assembly
from openmdao.examples.enginedesign.driving_sim import SimAcceleration, \
                                                       SimEconomy
from openmdao.examples.enginedesign.vehicle import Vehicle

class VehicleSim2(Assembly):
    """Optimization of a Vehicle."""
    
    def __init__(self):
        """ Creates a new Assembly for vehicle performance optimization."""
        
        super(VehicleSim2, self).__init__()

        # Create Vehicle instance
        self.add('vehicle', Vehicle())
        
        # Create Driving Simulation instances
        self.add('sim_acc', SimAcceleration())
        self.add('sim_EPA_city', SimEconomy())
        self.add('sim_EPA_highway', SimEconomy())
        
        # add Sims to default workflow
        self.driver.workflow.add(['sim_acc', 'sim_EPA_city', 'sim_EPA_highway'])
        
        # Add vehicle to sim workflows.
        self.sim_acc.workflow.add('vehicle')
        self.sim_EPA_city.workflow.add('vehicle')
        self.sim_EPA_highway.workflow.add('vehicle')
    
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
        
if __name__ == "__main__": 

    from openmdao.main.api import set_as_top
    my_sim = VehicleSim2()
    set_as_top(my_sim)

    my_sim.run()
    
    print "Time (0-60): ", my_sim.sim_acc.accel_time
    print "City MPG: ", my_sim.sim_EPA_city.fuel_economy
    print "Highway MPG: ", my_sim.sim_EPA_highway.fuel_economy
