"""
  driving_sim.py - Driving Simulation for the vehicle example problem.

  Contains OpenMDAO drivers that can simulate different driving regimes
  for a Vehicle assembly.
  
  SimAcceleration: Determines 0-60 acceleration time
  SimEconomy: Simulates fuel economy over a velocity profile
              as used to estimate EPA city and highway mpg
"""

from csv import reader
from pkg_resources import resource_stream

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Driver, convert_units
from openmdao.main.datatypes.api import Float, Str
from openmdao.main.hasobjective import HasObjectives
from openmdao.main.hasparameters import HasParameters
from openmdao.main.interfaces import IHasParameters, IHasObjectives, \
                                     implements
from openmdao.util.decorators import add_delegate


@add_delegate(HasParameters, HasObjectives)
class SimAcceleration(Driver):
    """ Simulation of vehicle acceleration performance. This is a specialized
    simulation driver whose workflow should consist of a Vehicle assembly, and
    whose connections are as follows:
    
    
    Named Parameters
    velocity:
        Vehicle velocity.
    
    throttle:
        Vehicle throttle position.
    
    gear:
        Vehicle gear position.
    
    
    Named Objectives
    acceleration:
        Vehicle acceleration.
    
    overspeed:
        Vehicle overspeed.
    
    
    Simulation Inputs
    end_speed: float
        Ending speed for the simulation (default 60 mph)
    
    timestep: float
        Simulation time step (default .01)
        
        
    Outputs
    accel_time: float
        Time to perform the acceleration test.
    """
    
    implements(IHasParameters, IHasObjectives)
    
    end_speed = Float(60.0, iotype='in', units='mi/h',
                      desc='Simulation final speed')
    timestep = Float(0.1, iotype='in', units='s', 
                     desc='Simulation time step size')
    
    accel_time = Float(0.0, iotype='out', units='s',
                       desc = 'Acceleration time')
    
    def execute(self):
        """ Simulate the vehicle model at full throttle."""
        
        # Set initial throttle, gear, and velocity
        time = 0.0
        velocity = 0.0
        throttle = 1.0
        gear = 1
        
        while velocity < self.end_speed:

            self.set_parameter_by_name('velocity', velocity)
            self.set_parameter_by_name('throttle', throttle)
            self.set_parameter_by_name('gear', gear)
            self.run_iteration()
            
            objectives = self.get_objectives()
            acceleration = objectives['acceleration'].evaluate(self.parent)
            overspeed = objectives['overspeed'].evaluate(self.parent)
            
            # If the next gear can produce more torque, let's shift.
            if gear < 5:
                self.set_parameter_by_name('gear', gear+1)
                self.run_iteration()
            
                acceleration2 = objectives['acceleration'].evaluate(self.parent)
                if acceleration2 > acceleration:
                    gear += 1
                    acceleration = acceleration2
                    overspeed = objectives['overspeed'].evaluate(self.parent)
                
            
            # If RPM goes over MAX RPM, shift gears
            # (i.e.: shift at redline)
            if overspeed:
                gear += 1
                self.set_parameter_by_name('gear', gear)
                self.run_iteration()
                acceleration = objectives['acceleration'].evaluate(self.parent)
                overspeed = objectives['overspeed'].evaluate(self.parent)
                
                if overspeed:
                    self.raise_exception("Gearing problem in Accel test.", 
                                             RuntimeError)

            acceleration = convert_units(acceleration, 'm/(s*s)', 'mi/(h*s)')
            
            if acceleration <= 0.0:
                self.raise_exception("Vehicle could not reach maximum speed "+\
                                     "in Acceleration test.", RuntimeError)
                
            velocity += (acceleration*self.timestep)
        
            time += self.timestep
                   
        self.accel_time = time

        
@add_delegate(HasParameters, HasObjectives)
class SimEconomy(Driver):
    """ Simulation of vehicle performance over a given velocity profile. Such
    a simulation can be used to mimic the EPA city and highway driving tests.
    This is a specialized simulation driver whose workflow should consist of a
    Vehicle assembly, and whose connections are as follows:
    
    
    Named Parameters
    velocity:
        Vehicle velocity.
    
    throttle:
        Vehicle throttle position.
    
    gear:
        Vehicle gear position.
    
    
    Named Objectives
    acceleration:
        Vehicle acceleration.
    
    fuel_burn:
        Vehicle fuel burn.
        
    overspeed:
        Vehicle overspeed.
    
    underspeed: 
        Vehicle underspeed.
    
    
    Simulation Inputs
    profilename: str
        Name of the file that contains profile (csv format)
        
    end_speed: float
        Ending speed for the simulation (default 60 mph)
    
    timestep: float
        Simulation time step (default .01)
        
        
    Outputs
    fuel_economy: float
        Fuel economy over the simulated profile.
    """

    implements(IHasParameters, IHasObjectives)
    
    profilename = Str('', iotype='in', \
                        desc='Name of the file that contains profile (csv)')
    
    # These can be used to adjust driving style.
    throttle_min = Float(.07, iotype='in', desc='Minimum throttle position')
    throttle_max = Float(1.0, iotype='in', desc='Maximum throttle position')
    shiftpoint1 = Float(10.0, iotype='in', \
                        desc='Always in first gear below this speed')
    
    tolerance = Float(0.01, iotype='in', 
                      desc='Convergence tolerance for Bisection solution')
    
    fuel_economy = Float(0.0, iotype='out', units='s',
                       desc = 'Simulated fuel economy over profile')
    

    def execute(self):
        """ Simulate the vehicle over a velocity profile."""
        
        # Set initial throttle, gear, and velocity
        throttle = 1.0
        gear = 1
        time1 = 0.0
        velocity1 = 0.0
        
        profile_stream = resource_stream('openmdao.examples.enginedesign',
                                         self.profilename)
        profile_reader = reader(profile_stream, delimiter=',')
        
        distance = 0.0
        fuelburn = 0.0
        
        self.set_parameter_by_name('gear', gear)
        
        for row in profile_reader:
            
            time2 = float(row[0])
            velocity2 = float(row[1])
            converged = 0
            
            command_accel = (velocity2-velocity1)/(time2-time1)
            
            #------------------------------------------------------------
            # Choose the correct Gear
            #------------------------------------------------------------

            # First, if speed is less than 10 mph, put it in first gear.
            # Note: some funky gear ratios might not like this.
            # So, it's a hack for now.
            
            if velocity1 < self.shiftpoint1:
                gear = 1
                self.set_parameter_by_name('gear', gear)
                
            # Find out min and max accel in current gear.
            
            throttle = self.throttle_min
            self.set_parameter_by_name('velocity', velocity1)
            self.set_parameter_by_name('throttle', throttle)
            gear = self._findgear(velocity1, throttle, gear)                    
            objectives = self.get_objectives()
            acceleration = objectives['acceleration'].evaluate(self.parent)
            accel_min = convert_units(acceleration, 'm/(s*s)', 'mi/(h*s)')
            
            # Upshift if commanded accel is less than closed-throttle accel
            # The net effect of this will often be a shift to a higher gear
            # when the vehicle stops accelerating, which is reasonable.
            # Note, this isn't a While loop, because we don't want to shift
            # to 5th every time we slow down.
            if command_accel < accel_min and gear < 5 and \
               velocity1 > self.shiftpoint1:
                
                gear += 1
                self.set_parameter_by_name('gear', gear)
                gear = self._findgear(velocity1, throttle, gear)                    
                objectives = self.get_objectives()
                acceleration = objectives['acceleration'].evaluate(self.parent)
                accel_min = convert_units(acceleration, 'm/(s*s)', 'mi/(h*s)')
            
            throttle = self.throttle_max
            self.set_parameter_by_name('throttle', throttle)
            self.run_iteration()
            objectives = self.get_objectives()
            acceleration = objectives['acceleration'].evaluate(self.parent)
            accel_max = convert_units(acceleration, 'm/(s*s)', 'mi/(h*s)')
            
            # Downshift if commanded accel > wide-open-throttle accel
            while command_accel > accel_max and gear > 1:
                
                gear -= 1
                self.set_parameter_by_name('gear', gear)
                gear = self._findgear(velocity1, throttle, gear)                    
                objectives = self.get_objectives()
                acceleration = objectives['acceleration'].evaluate(self.parent)
                accel_max = convert_units(acceleration, 'm/(s*s)', 'mi/(h*s)')
            
            # If engine cannot accelerate quickly enough to match profile, 
            # then raise exception    
            if command_accel > accel_max:
                self.raise_exception("Vehicle is unable to achieve " \
                "acceleration required to match EPA driving profile.", 
                                                RuntimeError)
                    
            #------------------------------------------------------------
            # Bisection solution to find correct Throttle position
            #------------------------------------------------------------

            # Deceleration at closed throttle
            throttle = self.throttle_min
            self.set_parameter_by_name('throttle', throttle)
            self.run_iteration()
            objectives = self.get_objectives()
            acceleration = objectives['acceleration'].evaluate(self.parent)
            
            if command_accel >= accel_min:
                
                min_acc = convert_units(acceleration, 'm/(s*s)', 'mi/(h*s)')
                max_acc = accel_max
                min_throttle = self.throttle_min
                max_throttle = self.throttle_max
                new_throttle = .5*(min_throttle + max_throttle)
                
                # Numerical solution to find throttle that matches accel
                while not converged:
                
                    throttle = new_throttle
                    self.set_parameter_by_name('throttle', throttle)
                    self.run_iteration()
                    objectives = self.get_objectives()
                    acceleration = objectives['acceleration'].evaluate(self.parent)
                    new_acc = convert_units(acceleration, 'm/(s*s)', 'mi/(h*s)')
                    
                    if abs(command_accel-new_acc) < self.tolerance:
                        converged = 1
                    else:
                        if new_acc < command_accel:
                            min_throttle = new_throttle
                            min_acc = new_acc
                            step = (command_accel-min_acc)/(max_acc-new_acc)
                            new_throttle = min_throttle + \
                                        step*(max_throttle-min_throttle)
                        else:
                            max_throttle = new_throttle
                            step = (command_accel-min_acc)/(new_acc-min_acc)
                            new_throttle = min_throttle + \
                                        step*(max_throttle-min_throttle)
                            max_acc = new_acc
                      
            distance += .5*(velocity2+velocity1)*(time2-time1)
            burn_rate = objectives['fuel_burn'].evaluate(self.parent)
            fuelburn += burn_rate*(time2-time1)
            
            velocity1 = velocity2
            time1 = time2
            
            #print "T = %f, V = %f, Acc = %f" % (time1, velocity1, 
            #command_accel)
            #print gear, accel_min, accel_max
            
        # Convert liter to gallon and sec/hr to hr/hr
        distance = convert_units(distance, 'mi*s/h', 'mi')
        fuelburn = convert_units(fuelburn, 'L', 'galUS')
        self.fuel_economy = distance/fuelburn
        
       
    def _findgear(self, velocity, throttle, gear):
        """ Finds the nearest gear in the appropriate range for the
        currently commanded vehicle state (throttle, velocity).
        
        This is intended to be called recursively.
        """

        self.run_iteration()
        
        objectives = self.get_objectives()
        overspeed = objectives['overspeed'].evaluate(self.parent)
        underspeed = objectives['underspeed'].evaluate(self.parent)
        
        if overspeed:
            gear += 1
            
            if gear > 4:
                self.raise_exception("Transmission gearing cannot " \
                "achieve acceleration and speed required by EPA " \
                "test.", RuntimeError)
            
        elif underspeed:
            gear -= 1
            
            # Note, no check needed for low gearing -- we allow underspeed 
            # while in first gear.
                
        else:
            return gear
            
        self.set_parameter_by_name('gear', gear)
        gear = self._findgear(velocity, throttle, gear)        

        return gear

if __name__ == "__main__": # pragma: no cover
    
    import time
    ttime = time.time()
    
    from openmdao.main.api import Assembly
    from openmdao.examples.enginedesign.vehicle import Vehicle
    
    top = Assembly()
    top.add('sim_acc', SimAcceleration())
    top.add('sim_EPA_city', SimEconomy())
    top.add('sim_EPA_highway', SimEconomy())
    top.add('vehicle', Vehicle())
    
    top.driver.workflow.add('sim_acc')
    top.driver.workflow.add('sim_EPA_city')
    top.driver.workflow.add('sim_EPA_highway')
    
    # Add vehicle to sim workflows.
    top.sim_acc.workflow.add('vehicle')
    top.sim_EPA_city.workflow.add('vehicle')
    top.sim_EPA_highway.workflow.add('vehicle')    
    
    # Acceleration Sim setup
    top.sim_acc.add_parameter('vehicle.velocity', name='velocity',
                              low=0.0, high=150.0)
    top.sim_acc.add_parameter('vehicle.throttle', name='throttle',
                              low=0.01, high=1.0)
    top.sim_acc.add_parameter('vehicle.current_gear', name='gear',
                              low=0, high=5)
    top.sim_acc.add_objective('vehicle.acceleration', name='acceleration')
    top.sim_acc.add_objective('vehicle.overspeed', name='overspeed')
    
    # EPA City MPG Sim Setup
    top.sim_EPA_city.add_parameter('vehicle.velocity', name='velocity',
                              low=0.0, high=150.0)
    top.sim_EPA_city.add_parameter('vehicle.throttle', name='throttle',
                              low=0.01, high=1.0)
    top.sim_EPA_city.add_parameter('vehicle.current_gear', name='gear',
                              low=0, high=5)
    top.sim_EPA_city.add_objective('vehicle.acceleration', name='acceleration')
    top.sim_EPA_city.add_objective('vehicle.fuel_burn', name='fuel_burn')
    top.sim_EPA_city.add_objective('vehicle.overspeed', name='overspeed')
    top.sim_EPA_city.add_objective('vehicle.underspeed', name='underspeed')
    top.sim_EPA_city.profilename = 'EPA-city.csv'
    
    # EPA Highway MPG Sim Setup
    top.sim_EPA_highway.add_parameter('vehicle.velocity', name='velocity',
                              low=0.0, high=150)
    top.sim_EPA_highway.add_parameter('vehicle.throttle', name='throttle',
                              low=0.01, high=1.0)
    top.sim_EPA_highway.add_parameter('vehicle.current_gear', name='gear',
                              low=0, high=5)
    top.sim_EPA_highway.add_objective('vehicle.acceleration', name='acceleration')
    top.sim_EPA_highway.add_objective('vehicle.fuel_burn', name='fuel_burn')
    top.sim_EPA_highway.add_objective('vehicle.overspeed', name='overspeed')
    top.sim_EPA_highway.add_objective('vehicle.underspeed', name='underspeed')
    top.sim_EPA_highway.profilename = 'EPA-highway.csv'        
    
    top.run()
    
    print "Time (0-60): ", top.sim_acc.accel_time
    print "City MPG: ", top.sim_EPA_city.fuel_economy
    print "Highway MPG: ", top.sim_EPA_highway.fuel_economy
    
    print "\nElapsed time: ", time.time()-ttime
    

# End driving_sim.py        
