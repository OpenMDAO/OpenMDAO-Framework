"""
    Model that simulates the 0-60 acceleration time of a Vehicle.
"""

from openmdao.main.api import Assembly
from openmdao.examples.enginedesign.driving_sim import SimAcceleration
from openmdao.examples.enginedesign.vehicle import Vehicle

class VehicleSim(Assembly):
    """Optimization of a Vehicle."""
    
    def __init__(self):
        """ Creates a new Assembly for vehicle performance optimization."""
        
        super(VehicleSim, self).__init__()

        # Create Vehicle instance
        self.add('vehicle', Vehicle())
        
        # Create 0-60 Acceleration Simulation instance
        self.add('driver', SimAcceleration())
        
        # Add vehicle to sim workflows.
        self.driver.workflow.add('vehicle')
    
        # Acceleration Sim setup
        self.driver.velocity_str = 'vehicle.velocity'
        self.driver.throttle_str = 'vehicle.throttle'
        self.driver.gear_str = 'vehicle.current_gear'
        self.driver.acceleration_str = 'vehicle.acceleration'
        self.driver.overspeed_str = 'vehicle.overspeed'
        
if __name__ == "__main__": 

    from openmdao.main.api import set_as_top
    my_sim = VehicleSim()
    set_as_top(my_sim)

    my_sim.run()
    
    print "Time (0-60): ", my_sim.driver.accel_time