# sim_vehicle.py
#
# Simulates a vehicle to obatain the following.
# Vehicle assembly fits in the socket.

from openmdao.main import Component, Float, Int
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.main.exceptions import ConstraintError

from openmdao.examples.engine_design.vehicle import Vehicle

from pkg_resources import resource_stream
from csv import reader

class Sim_Vehicle(Component):
    def __init__(self, name, parent=None, doc=None, directory=''):
        ''' Creates a new Sim_Accel object
        
            # Simulation inputs
            EndSpeed           # Simulation ending speed in mph.
            TimeStep           # Simulation time step in sec.
            
            # Outputs
            AccelTime         # Time to reach 60 mph from start
            EPACity           # Fuel economy for city driving
            EPAHighway        # Fuel economy for highway driving
            '''
        
        super(Sim_Vehicle, self).__init__(name, parent, doc, directory)    

        # set up interface to the framework  
        # Pylint: disable-msg=E1101

        # Promoted From Vehicle -> Engine
        Float('stroke', self, INPUT, units='mm', default=78.8,
              doc='Cylinder Stroke')
        Float('bore', self, INPUT, units='mm', default=82.0, 
              doc='Cylinder Bore')
        Float('conrod', self, INPUT, units='mm', default=115.0, 
              doc='Connecting Rod Length')
        Float('compRatio', self, INPUT, units=None, default=9.3, 
              doc='Compression Ratio')
        Float('sparkAngle', self, INPUT, units='deg', default=-37.0,
              doc = 'Spark Angle with respect to TDC (Top Dead Center)')
        Int('nCyl', self, INPUT, default=6,
            doc = 'Number of Cylinders')
        Float('IVO', self, INPUT, units='deg', default=11.0,
              doc = 'Intake Valve Open before TDC (Top Dead Center)')
        Float('IVC', self, INPUT, units='deg', default=53.0,
              doc = 'Intake Valve Open after BDC (Bottom Dead Center)')
        Float('Liv', self, INPUT, units='mm', default=8.0, 
              doc='Maximum Valve Lift')
        Float('Div', self, INPUT, units='mm', default=41.2, 
              doc='Inlet Valve Diameter')

        # Promoted From Vehicle -> Transmission
        Float('Ratio1', self, INPUT, units=None, default=3.54,
              doc='Gear Ratio in First Gear')
        Float('Ratio2', self, INPUT, units=None, default=2.13,
              doc='Gear Ratio in Second Gear')
        Float('Ratio3', self, INPUT, units=None, default=1.36,
              doc='Gear Ratio in Third Gear')
        Float('Ratio4', self, INPUT, units=None, default=1.03,
              doc='Gear Ratio in Fourth Gear')
        Float('Ratio5', self, INPUT, units=None, default=0.72,
              doc='Gear Ratio in Fifth Gear')
        Float('FinalDriveRatio', self, INPUT, units=None, default=2.8,
              doc='Final Drive Ratio')
        Float('TireCirc', self, INPUT, units='inch', default=75.0,
              doc='Circumference of tire (inches)')

        # Promoted From Vehicle -> Vehicle_Dynamics
        Float('Mass_Vehicle', self, INPUT, units='kg', default=1200.0,
              doc='Vehicle Mass')
        Float('Cf', self, INPUT, units=None, default=0.035,
              doc='Friction Coefficient (proportional to W)')
        Float('Cd', self, INPUT, units=None, default=0.3,
              doc='Drag Coefficient (proportional to V**2)')
        Float('Area', self, INPUT, units='m**2', default=2.164,
              doc='Frontal area')

        # Simulation Parameters
        Float('EndSpeed', self, INPUT, units='m/h', default=60.0,
              doc='Simulation final speed')
        Float('TimeStep', self, INPUT, units='s', default=0.1,
              doc='Simulation final speed')
        
        # Outputs
        Float('AccelTime', self, OUTPUT, units='s', default=0.0, 
              doc='Time to reach Endspeed starting from rest')
        Float('EPACity', self, OUTPUT, units='mi/galUS', default=0.0, 
              doc='EPA Fuel economy - City')
        Float('EPAHighway', self, OUTPUT, units='mi/galUS', default=0.0, 
              doc='EPA Fuel economy - Highway')
        
        # NOTE: This stuff will be replaced with Sockets when implemented
        self.vehicle = Vehicle("Test_Vehicle")
        #self.vehicle = None
        
    def execute(self):
        ''' Simulate the vehicle model at full throttle.
            '''
        
        # Promote all design variables from the Vehicle assembly to Sim_Vehicle
        # syntax here is temporary until socket interfaces are implemented.
        
        self.vehicle.stroke = self.stroke        
        self.vehicle.bore = self.bore        
        self.vehicle.conrod = self.conrod        
        self.vehicle.compRatio = self.compRatio        
        self.vehicle.sparkAngle = self.sparkAngle        
        self.vehicle.nCyl = self.nCyl        
        self.vehicle.IVO = self.IVO        
        self.vehicle.IVC = self.IVC        
        self.vehicle.Liv = self.Liv        
        self.vehicle.Div = self.Div        

        self.vehicle.Ratio1 = self.Ratio1        
        self.vehicle.Ratio2 = self.Ratio2        
        self.vehicle.Ratio3 = self.Ratio3        
        self.vehicle.Ratio4 = self.Ratio4        
        self.vehicle.Ratio5 = self.Ratio5        
        self.vehicle.FinalDriveRatio = self.FinalDriveRatio        
        self.vehicle.TireCirc = self.TireCirc        

        self.vehicle.Mass_Vehicle = self.Mass_Vehicle        
        self.vehicle.Cf = self.Cf        
        self.vehicle.Cd = self.Cd        
        self.vehicle.Area = self.Area        

        #--------------------------------------------------------------------
        # Simulate acceleration time from 0 to EndSpeed
        #--------------------------------------------------------------------
        
        Velocity = 0.0
        Time = 0.0
        
        # Set throttle and gear
        self.vehicle.CurrentGear = 1
        self.vehicle.Throttle = 1.0
        self.vehicle.Velocity = 0.0
                   
        while Velocity < self.EndSpeed:
            
            # Find acceleration.
            # If RPM goes over MAX RPM, shift gears
            # (i.e.: shift at redline)
            try:
                self.vehicle.execute()
            except ConstraintError:
                
                self.vehicle.CurrentGear += 1
                
                if self.vehicle.CurrentGear > 5:
                    self.raise_exception("Transmission gearing cannot achieve maximum speed in Acceleration test.", RunFailed)
                
                try:
                    self.vehicle.execute()
                except ConstraintError:
                    self.raise_exception("Gearing problem in Acceleration test.", RuntimeError)

            # Accleration converted to mph/s
            Acceleration = self.vehicle.Acceleration*2.23693629
            
            if Acceleration <= 0.0:
                self.raise_exception("Vehicle could not reach maximum speed in Acceleration test.", RuntimeError)
                
            Velocity += Acceleration*self.TimeStep
            self.vehicle.Velocity = Velocity/2.23693629
        
            Time += self.TimeStep
            #print Time, self.vehicle.CurrentGear, Velocity, self.vehicle.Transmission.RPM, self.vehicle.Engine.RPM
                   
        self.AccelTime = Time
        
        #--------------------------------------------------------------------
        # Simulate EPA driving profiles
        #--------------------------------------------------------------------
        
        profilenames = [ "EPA-city.csv", "EPA-highway.csv" ]
        
        ThrottleMin = .07
        ThrottleMax = 1.0
        ShiftPoint1 = 10.0
        MaxError = .01

        self.vehicle.CurrentGear = 1
        self.vehicle.Velocity = 0.0
        
        FuelEconomy = []
        
        def findgear():
            '''
               Finds the nearest gear in the appropriate range for the currently
               commanded velocity. 
               This is intended to be called recursively.
               '''
            # Note, shifts gear if RPM is too low or too high
            try:
                self.vehicle.execute()
            except ConstraintError:
                if self.vehicle.Transmission.RPM > self.vehicle.Engine.RPM:
                    self.vehicle.CurrentGear += 1
                    
                    if self.vehicle.CurrentGear > 5:
                        self.raise_exception("Transmission gearing cannot achieve maximum speed in EPA test.", RuntimeError)
                    
                else:
                    self.vehicle.CurrentGear -= 1
                    
                findgear()
                
        
        for profilename in profilenames:
            
            profile_stream = resource_stream('openmdao.examples.engine_design',profilename)
            profileReader = reader(profile_stream, delimiter=',')
            
            Time1 = 0.0
            Velocity1 = 0.0
            Distance = 0.0
            Fuelburn = 0.0
            
            for row in profileReader:
                
                Time2 = float(row[0])
                Velocity2 = float(row[1])
                CONVERGED = 0
                
                self.vehicle.Velocity = Velocity1/2.23693629
                CommandAccel = (Velocity2-Velocity1)/(Time2-Time1)
                
                #------------------------------------------------------------
                # Choose the correct Gear
                #------------------------------------------------------------

                # First, if speed is less than 10 mph, put it in first gear.
                # Note: some funky gear ratios might not like this.
                # So, it's a hack for now.
                
                if Velocity1 < ShiftPoint1:
                    self.vehicle.CurrentGear = 1
                    
                # Find out min and max accel in current gear.
                
                self.vehicle.Throttle = ThrottleMin
                findgear()                    
                AccelMin = self.vehicle.Acceleration*2.23693629
                
                # Upshift if commanded accel is less than closed-throttle accel
                # The net effect of this will often be a shift to a higher gear when
                # the vehicle stops accelerating, which is reasonable.
                # Note, this isn't a While loop, because we don't want to shift to 5th every
                # time we slow down.
                if CommandAccel < AccelMin and self.vehicle.CurrentGear < 5 and Velocity1 > ShiftPoint1:
                    self.vehicle.CurrentGear += 1
                    findgear()
                    AccelMin = self.vehicle.Acceleration*2.23693629
                
                self.vehicle.Throttle = ThrottleMax
                self.vehicle.execute()
                AccelMax = self.vehicle.Acceleration*2.23693629
                
                # Downshift if commanded accel is more than wide-open-throttle accel
                while CommandAccel > AccelMax and self.vehicle.CurrentGear > 1:
                    self.vehicle.CurrentGear -= 1
                    findgear()
                    AccelMax = self.vehicle.Acceleration*2.23693629
                
                # If engine cannot accelerate quickly enough to match profile, then raise exception    
                if CommandAccel > AccelMax:
                    self.raise_exception("Vehicle is unable to achieve acceleration required to match EPA driving profile.", RuntimeError)
                        
                #------------------------------------------------------------
                # Bisection solution to find correct Throttle position
                #------------------------------------------------------------

                # Deceleration at closed throttle
                if CommandAccel < AccelMin:
                    self.vehicle.Throttle = ThrottleMin
                    self.vehicle.execute()
                    
                else:
                    self.vehicle.Throttle = ThrottleMin
                    self.vehicle.execute()
                    
                    minAcc = self.vehicle.Acceleration*2.23693629
                    maxAcc = AccelMax
                    minThrottle = ThrottleMin
                    maxThrottle = ThrottleMax
                    newThrottle = .5*(minThrottle + maxThrottle)
                    
                    # Numerical solution to find throttle that matches accel
                    while not CONVERGED:
                    
                        self.vehicle.Throttle = newThrottle
                        self.vehicle.execute()
                        newAcc = self.vehicle.Acceleration*2.23693629
                        
                        if abs(CommandAccel-newAcc) < MaxError:
                            CONVERGED = 1
                        else:
                            if newAcc < CommandAccel:
                                minThrottle = newThrottle
                                minAcc = newAcc
                                Step = (CommandAccel-minAcc)/(maxAcc-newAcc)
                                newThrottle = minThrottle + Step*(maxThrottle-minThrottle)
                            else:
                                maxThrottle = newThrottle
                                Step = (CommandAccel-minAcc)/(newAcc-minAcc)
                                newThrottle = minThrottle + Step*(maxThrottle-minThrottle)
                                maxAcc = newAcc
                          
                        #print CommandAccel, newAcc, minThrottle, newThrottle
                Distance += .5*(Velocity2+Velocity1)*(Time2-Time1)
                Fuelburn += self.vehicle.FuelBurn*(Time2-Time1)
                
                Velocity1 = Velocity2
                Time1 = Time2
                
                #print "T = %f, V = %f, Acc = %f" % (Time1, Velocity1, CommandAccel)
                #print self.vehicle.CurrentGear, AccelMin, AccelMax
                
            # Convert liter to gallon and sec/hr to hr/hr
            Distance = Distance/3600.0
            Fuelburn = Fuelburn*(0.264172052)
            FuelEconomy.append(Distance/Fuelburn)
            
        self.EPACity = FuelEconomy[0]
        self.EPAHighway = FuelEconomy[1]
    
def test_it():    
    import time
    tt = time.time()
    
    z = Sim_Vehicle("New")  
    z.vehicle = Vehicle("Test_Vehicle")
    z.execute()
    print "Time (0-60): ", z.AccelTime
    print "City MPG: ", z.EPACity
    print "Highway MPG: ", z.EPAHighway
    
    print "\nElapsed time: ", time.time()-tt
    
if __name__ == "__main__": 
    test_it()

# End sim_vehicle.py        
