"""
    Model that simulates the 0-60 acceleration time of a Vehicle.
"""

from openmdao.main.api import Assembly
from openmdao.examples.enginedesign.driving_sim import SimAcceleration
from openmdao.examples.enginedesign.vehicle import Vehicle

class VehicleSim(Assembly):
    """Optimization of a Vehicle."""
    
    def configure(self):
        """ Configures a new Assembly for vehicle performance optimization."""
        
        # Create Vehicle instance
        self.add('vehicle', Vehicle())
        
        # Create 0-60 Acceleration Simulation instance
        self.add('driver', SimAcceleration())
        
        # Add vehicle to sim workflows.
        self.driver.workflow.add('vehicle')
    
        # Acceleration Sim setup
        self.driver.add_parameter('vehicle.velocity', name='velocity',
                                  low=0.0, high=150.0)
        self.driver.add_parameter('vehicle.throttle', name='throttle',
                                  low=0.01, high=1.0)
        self.driver.add_parameter('vehicle.current_gear', name='gear',
                                  low=0, high=5)
        self.driver.add_objective('vehicle.acceleration', name='acceleration')
        self.driver.add_objective('vehicle.overspeed', name='overspeed')
        
if __name__ == "__main__": 

    from openmdao.main.api import set_as_top
    my_sim = VehicleSim()
    set_as_top(my_sim)

    my_sim.run()
    
    print "Time (0-60): ", my_sim.driver.accel_time