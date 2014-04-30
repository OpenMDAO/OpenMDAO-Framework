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
    
    def configure(self):
        """ Configures a new Assembly for vehicle performance optimization."""
        
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
        
if __name__ == "__main__": 

    from openmdao.main.api import set_as_top
    my_sim = VehicleSim2()
    set_as_top(my_sim)

    my_sim.run()
    
    print "Time (0-60): ", my_sim.sim_acc.accel_time
    print "City MPG: ", my_sim.sim_EPA_city.fuel_economy
    print "Highway MPG: ", my_sim.sim_EPA_highway.fuel_economy
