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
from openmdao.lib.datatypes.api import Float, Str
from openmdao.main.api import Driver, convert_units
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.hasobjective import HasObjectives
from openmdao.main.hasparameters import HasParameters
from openmdao.util.decorators import add_delegate


class SimAcceleration(Driver):
    """ Simulation of vehicle acceleration performance. This is a specialized
    simulation driver whose workflow should consist of a Vehicle assembly, and
    whose connections are as follows:
    
    Connection Inputs
    velocity_str: str
        Variable location for vehicle velocity.
    
    throttle_str: str
        Variable location for vehicle throttle position.
    
    gear_str: str
        Variable location for vehicle gear position.
    
    acceleration_str: str
        Variable location for vehicle acceleration.
    
    overspeed_str: str
        Variable location for vehicle overspeed.
    
    Simulation Inputs
    end_speed: float
        Ending speed for the simulation (default 60 mph)
    
    timestep: float
        Simulation time step (default .01)
        
    Outputs
    accel_time: float
        Time to perform the acceleration test.
    """
    
    velocity_str = Str(iotype='in',
                       desc='Location of vehicle input: velocity.')
    throttle_str = Str(iotype='in',
                       desc='Location of vehicle input: throttle.')
    gear_str = Str(iotype='in',
                       desc='Location of vehicle input: current_gear.')
    acceleration_str = Str(iotype='in',
                       desc='Location of vehicle output: acceleration.')
    overspeed_str = Str(iotype='in',
                       desc='Location of vehicle output: overspeed.')

    end_speed = Float(60.0, iotype='in', units='mi/h',
                      desc='Simulation final speed')
    timestep = Float(0.1, iotype='in', units='s', 
                     desc='Simulation time step size')
    
    accel_time = Float(0.0, iotype='out', units='s',
                       desc = 'Acceleration time')
    
    def __init__(self):
        super(SimAcceleration, self).__init__()
        
        self._velocity_str_expr = None
        self._throttle_str_expr = None
        self._gear_str_expr = None
        self._acceleration_str_expr = None
        self._overspeed_str_expr = None
    
    def _velocity_str_changed(self, oldval, newval):
        self._velocity_str_expr = ExprEvaluator(newval, scope=self.parent)
    
    def _throttle_str_changed(self, oldval, newval):
        self._throttle_str_expr = ExprEvaluator(newval, scope=self.parent)
    
    def _gear_str_changed(self, oldval, newval):
        self._gear_str_expr = ExprEvaluator(newval, scope=self.parent)
    
    def _acceleration_str_changed(self, oldval, newval):
        self._acceleration_str_expr = ExprEvaluator(newval, scope=self.parent)
    
    def _overspeed_str_changed(self, oldval, newval):
        self._overspeed_str_expr = ExprEvaluator(newval, scope=self.parent)
    
    def execute(self):
        """ Simulate the vehicle model at full throttle."""
        
        # Set initial throttle, gear, and velocity
        time = 0.0
        velocity = 0.0
        throttle = 1.0
        gear = 1
        
        while velocity < self.end_speed:
            
            self._velocity_str_expr.set(velocity)
            self._throttle_str_expr.set(throttle)
            self._gear_str_expr.set(gear)
            self.run_iteration()
            
            acceleration = self._acceleration_str_expr.evaluate(self.parent)
            overspeed = self._overspeed_str_expr.evaluate(self.parent)
            
            # If the next gear can produce more torque, let's shift.
            if gear < 5:
                self._gear_str_expr.set(gear+1)
                self.run_iteration()
            
                acceleration2 = self._acceleration_str_expr.evaluate(self.parent)
                if acceleration2 > acceleration:
                    gear += 1
                    acceleration = acceleration2
                    overspeed = self._overspeed_str_expr.evaluate(self.parent)
                
            
            # If RPM goes over MAX RPM, shift gears
            # (i.e.: shift at redline)
            if overspeed:
                gear += 1
                self._gear_str_expr.set(gear)
                self.run_iteration()
                acceleration = self._acceleration_str_expr.evaluate(self.parent)
                overspeed = self._overspeed_str_expr.evaluate(self.parent)
                
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

        
class SimEconomy(Driver):
    """ Simulation of vehicle performance over a given velocity profile. Such
    a simulation can be used to mimic the EPA city and highway driving tests.
    This is a specialized simulation driver whose workflow should consist of a
    Vehicle assembly, and whose connections are as follows:
    
    Connections
    Parameters: [ velocity (Float),
                  throttle (Float),
                  current_gear (Enum) ]
    Objectives: [ acceleration (Float), 
                  fuel burn (Float),
                  overspeed (Bool),
                  underspeed (Bool) ]
                  
    Connection Inputs
    velocity_str: str
        Variable location for vehicle velocity.
    
    throttle_str: str
        Variable location for vehicle throttle position.
    
    gear_str: str
        Variable location for vehicle gear position.
    
    acceleration_str: str
        Variable location for vehicle acceleration.
    
    fuel_burn_str: str
        Variable location for vehicle fuel burn.
    
    overspeed_str: str
        Variable location for vehicle overspeed.
    
    underspeed_str: str
        Variable location for vehicle underspeed.
    
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

    velocity_str = Str(iotype='in',
                       desc='Location of vehicle input: velocity.')
    throttle_str = Str(iotype='in',
                       desc='Location of vehicle input: throttle.')
    gear_str = Str(iotype='in',
                       desc='Location of vehicle input: current_gear.')
    acceleration_str = Str(iotype='in',
                       desc='Location of vehicle output: acceleration.')
    fuel_burn_str = Str(iotype='in',
                       desc='Location of vehicle output: fuel_burn.')
    overspeed_str = Str(iotype='in',
                       desc='Location of vehicle output: overspeed.')
    underspeed_str = Str(iotype='in',
                       desc='Location of vehicle output: underspeed.')

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
    

    def __init__(self, *args, **kwargs):
        super(SimEconomy, self).__init__(*args, **kwargs)
        
        self._velocity_str_expr = None
        self._throttle_str_expr = None
        self._gear_str_expr = None
        self._acceleration_str_expr = None
        self._fuel_burn_str_expr = None
        self._overspeed_str_expr = None
        self._underspeed_str_expr = None
    
    def _velocity_str_changed(self, oldval, newval):
        self._velocity_str_expr = ExprEvaluator(newval, scope=self.parent)
    
    def _throttle_str_changed(self, oldval, newval):
        self._throttle_str_expr = ExprEvaluator(newval, scope=self.parent)
    
    def _gear_str_changed(self, oldval, newval):
        self._gear_str_expr = ExprEvaluator(newval, scope=self.parent)
    
    def _acceleration_str_changed(self, oldval, newval):
        self._acceleration_str_expr = ExprEvaluator(newval, scope=self.parent)
    
    def _fuel_burn_str_changed(self, oldval, newval):
        self._fuel_burn_str_expr = ExprEvaluator(newval, scope=self.parent)
    
    def _overspeed_str_changed(self, oldval, newval):
        self._overspeed_str_expr = ExprEvaluator(newval, scope=self.parent)
    
    def _underspeed_str_changed(self, oldval, newval):
        self._underspeed_str_expr = ExprEvaluator(newval, scope=self.parent)
    
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
        
        self._gear_str_expr.set(gear)
        
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
                self._gear_str_expr.set(gear)
                
            # Find out min and max accel in current gear.
            
            throttle = self.throttle_min
            self._velocity_str_expr.set(velocity1)
            self._throttle_str_expr.set(throttle)
            gear = self._findgear(velocity1, throttle, gear)                    
            acceleration = self._acceleration_str_expr.evaluate(self.parent)
            accel_min = convert_units(acceleration, 'm/(s*s)', 'mi/(h*s)')
            
            # Upshift if commanded accel is less than closed-throttle accel
            # The net effect of this will often be a shift to a higher gear
            # when the vehicle stops accelerating, which is reasonable.
            # Note, this isn't a While loop, because we don't want to shift
            # to 5th every time we slow down.
            if command_accel < accel_min and gear < 5 and \
               velocity1 > self.shiftpoint1:
                
                gear += 1
                self._gear_str_expr.set(gear)
                gear = self._findgear(velocity1, throttle, gear)                    
                acceleration = self._acceleration_str_expr.evaluate(self.parent)
                accel_min = convert_units(acceleration, 'm/(s*s)', 'mi/(h*s)')
            
            throttle = self.throttle_max
            self._throttle_str_expr.set(throttle)
            self.run_iteration()
            acceleration = self._acceleration_str_expr.evaluate(self.parent)
            accel_max = convert_units(acceleration, 'm/(s*s)', 'mi/(h*s)')
            
            # Downshift if commanded accel > wide-open-throttle accel
            while command_accel > accel_max and gear > 1:
                
                gear -= 1
                self._gear_str_expr.set(gear)
                gear = self._findgear(velocity1, throttle, gear)                    
                acceleration = self._acceleration_str_expr.evaluate(self.parent)
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
            self._throttle_str_expr.set(throttle)
            self.run_iteration()
            acceleration = self._acceleration_str_expr.evaluate(self.parent)
            
            if command_accel >= accel_min:
                
                min_acc = convert_units(acceleration, 'm/(s*s)', 'mi/(h*s)')
                max_acc = accel_max
                min_throttle = self.throttle_min
                max_throttle = self.throttle_max
                new_throttle = .5*(min_throttle + max_throttle)
                
                # Numerical solution to find throttle that matches accel
                while not converged:
                
                    throttle = new_throttle
                    self._throttle_str_expr.set(throttle)
                    self.run_iteration()
                    acceleration = self._acceleration_str_expr.evaluate(self.parent)
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
            burn_rate = self._fuel_burn_str_expr.evaluate(self.parent)
            fuelburn += burn_rate*(time2-time1)
            
            velocity1 = velocity2
            time1 = time2
            
            #print "T = %f, V = %f, Acc = %f" % (time1, velocity1, 
            #command_accel)
            #print gear, accel_min, accel_max
            
        # Convert liter to gallon and sec/hr to hr/hr
        distance = convert_units(distance, 'mi*s/h', 'mi')
        fuelburn = convert_units(fuelburn, 'l', 'galUS')
        self.fuel_economy = distance/fuelburn
        
       
    def _findgear(self, velocity, throttle, gear):
        """ Finds the nearest gear in the appropriate range for the
        currently commanded vehicle state (throttle, velocity).
        
        This is intended to be called recursively.
        """

        self.run_iteration()
        
        overspeed = self._overspeed_str_expr.evaluate(self.parent)
        underspeed = self._underspeed_str_expr.evaluate(self.parent)
        
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
            
        self._gear_str_expr.set(gear)
        gear = self._findgear(velocity, throttle, gear)        

        return gear

if __name__ == "__main__": # pragma: no cover
    
    import time
    ttime = time.time()
    
    from openmdao.main.api import set_as_top, Assembly
    from openmdao.examples.enginedesign.vehicle import Vehicle
    
    top = set_as_top(Assembly())
    top.add('sim_acc', SimAcceleration())
    top.add('sim_EPA_city', SimEconomy())
    top.add('sim_EPA_highway', SimEconomy())
    top.add('vehicle', Vehicle())
    
    top.driver.workflow.add('sim_acc')
    top.driver.workflow.add('sim_EPA_city')
    top.driver.workflow.add('sim_EPA_highway')
    
    top.sim_acc.workflow.add('vehicle')
    top.sim_acc.velocity_str = 'vehicle.velocity'
    top.sim_acc.throttle_str = 'vehicle.throttle'
    top.sim_acc.gear_str = 'vehicle.current_gear'
    top.sim_acc.acceleration_str = 'vehicle.acceleration'
    top.sim_acc.overspeed_str = 'vehicle.overspeed'
    
    top.sim_EPA_city.workflow.add('vehicle')
    top.sim_EPA_city.velocity_str = 'vehicle.velocity'
    top.sim_EPA_city.throttle_str = 'vehicle.throttle'
    top.sim_EPA_city.gear_str = 'vehicle.current_gear'
    top.sim_EPA_city.acceleration_str = 'vehicle.acceleration'
    top.sim_EPA_city.fuel_burn_str = 'vehicle.fuel_burn'
    top.sim_EPA_city.overspeed_str = 'vehicle.overspeed'
    top.sim_EPA_city.underspeed_str = 'vehicle.underspeed'
    top.sim_EPA_city.profilename = 'EPA-city.csv'
    
    top.sim_EPA_highway.workflow.add('vehicle')
    top.sim_EPA_highway.velocity_str = 'vehicle.velocity'
    top.sim_EPA_highway.throttle_str = 'vehicle.throttle'
    top.sim_EPA_highway.gear_str = 'vehicle.current_gear'
    top.sim_EPA_highway.acceleration_str = 'vehicle.acceleration'
    top.sim_EPA_highway.fuel_burn_str = 'vehicle.fuel_burn'
    top.sim_EPA_highway.overspeed_str = 'vehicle.overspeed'
    top.sim_EPA_highway.underspeed_str = 'vehicle.underspeed'
    top.sim_EPA_highway.profilename = 'EPA-highway.csv'
    
    top.run()
    
    print "Time (0-60): ", top.sim_acc.accel_time
    print "City MPG: ", top.sim_EPA_city.fuel_economy
    print "Highway MPG: ", top.sim_EPA_highway.fuel_economy
    
    print "\nElapsed time: ", time.time()-ttime
    

# End driving_sim.py        
