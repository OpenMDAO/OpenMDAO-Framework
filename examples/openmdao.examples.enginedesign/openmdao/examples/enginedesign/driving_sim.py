"""
    driving_sim.py - Driving Simulation for the vehicle example problem.
"""

# Simulates a vehicle to obatain the following:
# - 0-60mph acceleration time
# - EPA fuel economy estimate for city driving
# - EPA fuel economy estimate for highway driving
#
# Includes a socket for a Vehicle assembly.

from csv import reader

from pkg_resources import resource_stream
from enthought.traits.api import TraitError

from openmdao.main.api import Assembly
from openmdao.lib.api import Float

from openmdao.examples.enginedesign.vehicle import Vehicle


# Settings for the EPA profile simulation
THROTTLE_MIN = .07
THROTTLE_MAX = 1.0
SHIFTPOINT1 = 10.0
MAX_ERROR = .01
CONVERT_TO_MPHPS = 2.23693629


class DrivingSim(Assembly):
    """ Simulation of vehicle performance."""
    
    # Simulation Parameters
    end_speed = Float(60.0, iotype='in', units='m/h',
                           desc='Simulation final speed')
    timestep = Float(0.1, iotype='in', units='s', 
                          desc='Simulation final speed')
    
    # Outputs
    accel_time = Float(0., iotype='out', units='s', 
                            desc='Time to reach Endspeed starting from rest')
    EPA_city = Float(0., iotype='out', units='mi/galUS', 
                          desc='EPA Fuel economy - City')
    EPA_highway = Float(0., iotype='out', units='mi/galUS', 
                             desc='EPA Fuel economy - Highway')
        
    def __init__(self, doc=None, directory=''):
        """ Creates a new DrivingSim object
        
        # Simulation inputs
        end_speed          # Simulation ending speed in mph.
        timestep           # Simulation time step in sec.
            
        # Outputs
        accel_time        # Time to reach 60 mph from start
        EPA_city          # Fuel economy for city driving
        EPA_highway       # Fuel economy for highway driving
        """
        
        super(DrivingSim, self).__init__(doc, directory)    

        # set up interface to the framework  
        # Pylint: disable-msg=E1101

        self.add_container('vehicle', Vehicle())
        
        # Promoted From Vehicle -> Engine
        self.create_passthrough('vehicle.stroke')
        self.create_passthrough('vehicle.bore')
        self.create_passthrough('vehicle.conrod')
        self.create_passthrough('vehicle.comp_ratio')
        self.create_passthrough('vehicle.spark_angle')
        self.create_passthrough('vehicle.n_cyl')
        self.create_passthrough('vehicle.IVO')
        self.create_passthrough('vehicle.IVC')
        self.create_passthrough('vehicle.L_v')
        self.create_passthrough('vehicle.D_v')

        # Promoted From Vehicle -> Transmission
        self.create_passthrough('vehicle.ratio1')
        self.create_passthrough('vehicle.ratio2')
        self.create_passthrough('vehicle.ratio3')
        self.create_passthrough('vehicle.ratio4')
        self.create_passthrough('vehicle.ratio5')
        self.create_passthrough('vehicle.final_drive_ratio')
        self.create_passthrough('vehicle.tire_circumference')

        # Promoted From Vehicle -> Chassis
        self.create_passthrough('vehicle.mass_vehicle')
        self.create_passthrough('vehicle.Cf')
        self.create_passthrough('vehicle.Cd')
        self.create_passthrough('vehicle.area')

        
    def execute(self):
        """ Simulate the vehicle model at full throttle."""
        
        #--------------------------------------------------------------------
        # Simulate acceleration time from 0 to end_speed
        #--------------------------------------------------------------------
        
        velocity = 0.0
        time = 0.0
        
        end_speed = self.end_speed
        
        # Set throttle and gear
        self.vehicle.current_gear = 1
        self.vehicle.throttle = 1.0
        self.vehicle.velocity = 0.0
                   
        while velocity < end_speed:
            
            # Find acceleration.
            # If RPM goes over MAX RPM, shift gears
            # (i.e.: shift at redline)
            try:
                self.vehicle.run()
            except TraitError:
                if self.vehicle.engine.RPM != self.vehicle.transmission.RPM:
                    self.vehicle.current_gear += 1
                else:
                    raise
                try:
                    self.vehicle.run()
                except TraitError:
                    if self.vehicle.engine.RPM != self.vehicle.transmission.RPM:
                        self.raise_exception("Gearing problem in Accel test.", 
                                             RuntimeError)
                    else:
                        raise

            # Accleration converted to mph/s
            acceleration = self.vehicle.acceleration*CONVERT_TO_MPHPS
            
            if acceleration <= 0.0:
                self.raise_exception("Vehicle could not reach maximum speed "+\
                                     "in Acceleration test.", RuntimeError)
                
            velocity += (acceleration*self.timestep)
            self.vehicle.velocity = velocity
        
            time += self.timestep
            #print time, self.vehicle.current_gear, velocity, 
            #self.vehicle.transmission.RPM, self.vehicle.engine.RPM
                   
        self.accel_time = time
        
        #--------------------------------------------------------------------
        # Simulate EPA driving profiles
        #--------------------------------------------------------------------
        
        profilenames = [ "EPA-city.csv", "EPA-highway.csv" ]
        
        self.vehicle.current_gear = 1
        self.vehicle.velocity = 0.0
        
        fuel_economy = []
        
        def findgear():
            """ Finds the nearest gear in the appropriate range for the
            currently commanded velocity. 
            This is intended to be called recursively.
            """
            # Note, shifts gear if RPM is too low or too high
            try:
                self.vehicle.run()
            except TraitError, err:
                if self.vehicle.engine.RPM < self.vehicle.transmission.RPM:
                    
                    if self.vehicle.current_gear > 4:
                        self.raise_exception("Transmission gearing cannot \
                        achieve acceleration and speed required by EPA \
                        test.", RuntimeError)
                    
                    self.vehicle.current_gear += 1
                    
                elif self.vehicle.engine.RPM > self.vehicle.transmission.RPM:

                    # Note, no check needed for low gearing -- you cannot go \
                    # too slow for first gear.
                    
                    self.vehicle.current_gear -= 1
                    
                else:
                    raise
                
                findgear()
                
        
        for profilename in profilenames:
            
            profile_stream = resource_stream('openmdao.examples.enginedesign',
                                             profilename)
            profile_reader = reader(profile_stream, delimiter=',')
            
            time1 = 0.0
            velocity1 = 0.0
            distance = 0.0
            fuelburn = 0.0
            
            for row in profile_reader:
                
                time2 = float(row[0])
                velocity2 = float(row[1])
                CONVERGED = 0
                
                self.vehicle.velocity = velocity1
                command_accel = (velocity2-velocity1)/(time2-time1)
                
                #------------------------------------------------------------
                # Choose the correct Gear
                #------------------------------------------------------------

                # First, if speed is less than 10 mph, put it in first gear.
                # Note: some funky gear ratios might not like this.
                # So, it's a hack for now.
                
                if velocity1 < SHIFTPOINT1:
                    self.vehicle.current_gear = 1
                    
                # Find out min and max accel in current gear.
                
                self.vehicle.throttle = THROTTLE_MIN
                findgear()                    
                accel_min = self.vehicle.acceleration*CONVERT_TO_MPHPS
                
                # Upshift if commanded accel is less than closed-throttle accel
                # The net effect of this will often be a shift to a higher gear
                # when the vehicle stops accelerating, which is reasonable.
                # Note, this isn't a While loop, because we don't want to shift
                # to 5th every time we slow down.
                if command_accel < accel_min and \
                   self.vehicle.current_gear < 5 and \
                   velocity1 > SHIFTPOINT1:
                    
                    self.vehicle.current_gear += 1
                    findgear()
                    accel_min = self.vehicle.acceleration*CONVERT_TO_MPHPS
                
                self.vehicle.throttle = THROTTLE_MAX
                self.vehicle.run()
                accel_max = self.vehicle.acceleration*CONVERT_TO_MPHPS
                
                # Downshift if commanded accel > wide-open-throttle accel
                while command_accel > accel_max and \
                      self.vehicle.current_gear > 1:
                    
                    self.vehicle.current_gear -= 1
                    findgear()
                    accel_max = self.vehicle.acceleration*CONVERT_TO_MPHPS
                
                # If engine cannot accelerate quickly enough to match profile, 
                # then raise exception    
                if command_accel > accel_max:
                    self.raise_exception("Vehicle is unable to achieve \
                    acceleration required to match EPA driving profile.", \
                                                    RuntimeError)
                        
                #------------------------------------------------------------
                # Bisection solution to find correct Throttle position
                #------------------------------------------------------------

                # Deceleration at closed throttle
                if command_accel < accel_min:
                    self.vehicle.throttle = THROTTLE_MIN
                    self.vehicle.run()                   
                else:
                    self.vehicle.throttle = THROTTLE_MIN
                    self.vehicle.run()
                    
                    min_acc = self.vehicle.acceleration*CONVERT_TO_MPHPS
                    max_acc = accel_max
                    min_throttle = THROTTLE_MIN
                    max_throttle = THROTTLE_MAX
                    new_throttle = .5*(min_throttle + max_throttle)
                    
                    # Numerical solution to find throttle that matches accel
                    while not CONVERGED:
                    
                        self.vehicle.throttle = new_throttle
                        self.vehicle.run()
                        new_acc = self.vehicle.acceleration*CONVERT_TO_MPHPS
                        
                        if abs(command_accel-new_acc) < MAX_ERROR:
                            CONVERGED = 1
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
                fuelburn += self.vehicle.fuel_burn*(time2-time1)
                
                velocity1 = velocity2
                time1 = time2
                
                #print "T = %f, V = %f, Acc = %f" % (time1, velocity1, 
                #command_accel)
                #print self.vehicle.current_gear, accel_min, accel_max
                
            # Convert liter to gallon and sec/hr to hr/hr
            distance = distance/3600.0
            fuelburn = fuelburn*(0.264172052)
            fuel_economy.append(distance/fuelburn)
            
        self.EPA_city = fuel_economy[0]
        self.EPA_highway = fuel_economy[1]
    
def test_it(): # pragma: no cover    
    """simple testing"""
    
    import time
    ttime = time.time()
    
    toplevel = DrivingSim("new")  
    toplevel.vehicle = Vehicle("test_vehicle")
    toplevel.run()
    
    print "Time (0-60): ", toplevel.accel_time
    print "City MPG: ", toplevel.EPA_city
    print "Highway MPG: ", toplevel.EPA_highway
    
    print "\nElapsed time: ", time.time()-ttime
    
if __name__ == "__main__": # pragma: no cover    
    test_it()

# End driving_sim.py        
