# sim_vehicle.py
#
# Simulates a vehicle to obatain the following.
# Vehicle assembly fits in the socket.

from openmdao.main import Component, Assembly, Float, Int
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.main.exceptions import ConstraintError

from openmdao.examples.engine_design.vehicle import Vehicle, IVehicle

from pkg_resources import resource_stream
from csv import reader

class Sim_Vehicle(Assembly):
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

        Vehicle("vehicle", self)
        
        # Promoted From Vehicle -> Engine
        self.create_passthru('vehicle.stroke')
        self.create_passthru('vehicle.bore')
        self.create_passthru('vehicle.conrod')
        self.create_passthru('vehicle.compRatio')
        self.create_passthru('vehicle.sparkAngle')
        self.create_passthru('vehicle.nCyl')
        self.create_passthru('vehicle.IVO')
        self.create_passthru('vehicle.IVC')
        self.create_passthru('vehicle.Liv')
        self.create_passthru('vehicle.Div')

        # Promoted From Vehicle -> Transmission
        self.create_passthru('vehicle.Ratio1')
        self.create_passthru('vehicle.Ratio2')
        self.create_passthru('vehicle.Ratio3')
        self.create_passthru('vehicle.Ratio4')
        self.create_passthru('vehicle.Ratio5')
        self.create_passthru('vehicle.FinalDriveRatio')
        self.create_passthru('vehicle.TireCirc')

        # Promoted From Vehicle -> Vehicle_Dynamics
        self.create_passthru('vehicle.Mass_Vehicle')
        self.create_passthru('vehicle.Cf')
        self.create_passthru('vehicle.Cd')
        self.create_passthru('vehicle.Area')

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
        
        
    def execute(self):
        ''' Simulate the vehicle model at full throttle.'''
        #--------------------------------------------------------------------
        # Simulate acceleration time from 0 to EndSpeed
        #--------------------------------------------------------------------
        
        Velocity = 0.0
        Time = 0.0
        
        # Set throttle and gear
        self.vehicle.set('CurrentGear', 1)
        self.vehicle.set('Throttle', 1.0)
        self.vehicle.set('Velocity', 0.0)
                   
        while Velocity < self.EndSpeed:
            
            # Find acceleration.
            # If RPM goes over MAX RPM, shift gears
            # (i.e.: shift at redline)
            try:
                self.vehicle.run()
            except ConstraintError:
                self.vehicle.set('CurrentGear', self.vehicle.get('CurrentGear') + 1)
                
                try:
                    self.vehicle.run()
                except ConstraintError:
                    self.raise_exception("Gearing problem in Acceleration test.", RuntimeError)

            # Accleration converted to mph/s
            Acceleration = self.vehicle.get('Acceleration')*2.23693629
            
            if Acceleration <= 0.0:
                self.raise_exception("Vehicle could not reach maximum speed in Acceleration test.", RuntimeError)
                
            Velocity += Acceleration*self.TimeStep
            self.vehicle.set('Velocity', Velocity)
        
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

        self.vehicle.set('CurrentGear', 1)
        self.vehicle.set('Velocity', 0.0)
        
        FuelEconomy = []
        
        def findgear():
            '''
               Finds the nearest gear in the appropriate range for the currently
               commanded velocity. 
               This is intended to be called recursively.
               '''
            # Note, shifts gear if RPM is too low or too high
            try:
                self.vehicle.run()
            except ConstraintError:
                if self.vehicle.get('Transmission.RPM') > self.vehicle.get('Engine.RPM'):
                    self.vehicle.set('CurrentGear', self.vehicle.get('CurrentGear') + 1)
                    
                    if self.vehicle.get('CurrentGear') > 5:
                        self.raise_exception("Transmission gearing cannot achieve maximum speed in EPA test.", RuntimeError)
                    
                else:
                    self.vehicle.set('CurrentGear', self.vehicle.get('CurrentGear') - 1)
                    
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
                
                self.vehicle.set('Velocity', Velocity1)
                CommandAccel = (Velocity2-Velocity1)/(Time2-Time1)
                
                #------------------------------------------------------------
                # Choose the correct Gear
                #------------------------------------------------------------

                # First, if speed is less than 10 mph, put it in first gear.
                # Note: some funky gear ratios might not like this.
                # So, it's a hack for now.
                
                if Velocity1 < ShiftPoint1:
                    self.vehicle.set('CurrentGear', 1)
                    
                # Find out min and max accel in current gear.
                
                self.vehicle.set('Throttle', ThrottleMin)
                findgear()                    
                AccelMin = self.vehicle.get('Acceleration')*2.23693629
                
                # Upshift if commanded accel is less than closed-throttle accel
                # The net effect of this will often be a shift to a higher gear when
                # the vehicle stops accelerating, which is reasonable.
                # Note, this isn't a While loop, because we don't want to shift to 5th every
                # time we slow down.
                if CommandAccel < AccelMin and self.vehicle.get('CurrentGear') < 5 and Velocity1 > ShiftPoint1:
                    self.vehicle.set('CurrentGear', self.vehicle.get('CurrentGear') + 1)
                    findgear()
                    AccelMin = self.vehicle.get('Acceleration')*2.23693629
                
                self.vehicle.set('Throttle', ThrottleMax)
                self.vehicle.run()
                AccelMax = self.vehicle.get('Acceleration')*2.23693629
                
                # Downshift if commanded accel is more than wide-open-throttle accel
                while CommandAccel > AccelMax and self.vehicle.get('CurrentGear')> 1:
                    self.vehicle.set('CurrentGear', self.vehicle.get('CurrentGear') - 1)
                    findgear()
                    AccelMax = self.vehicle.get('Acceleration')*2.23693629
                
                # If engine cannot accelerate quickly enough to match profile, then raise exception    
                if CommandAccel > AccelMax:
                    self.raise_exception("Vehicle is unable to achieve acceleration required to match EPA driving profile.", RuntimeError)
                        
                #------------------------------------------------------------
                # Bisection solution to find correct Throttle position
                #------------------------------------------------------------

                # Deceleration at closed throttle
                if CommandAccel < AccelMin:
                    self.vehicle.set('Throttle', ThrottleMin)
                    self.vehicle.run()                   
                else:
                    self.vehicle.set('Throttle', ThrottleMin)
                    self.vehicle.run()
                    
                    minAcc = self.vehicle.get('Acceleration')*2.23693629
                    maxAcc = AccelMax
                    minThrottle = ThrottleMin
                    maxThrottle = ThrottleMax
                    newThrottle = .5*(minThrottle + maxThrottle)
                    
                    # Numerical solution to find throttle that matches accel
                    while not CONVERGED:
                    
                        self.vehicle.set('Throttle', newThrottle)
                        self.vehicle.run()
                        newAcc = self.vehicle.get('Acceleration')*2.23693629
                        
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
                Fuelburn += self.vehicle.get('FuelBurn')*(Time2-Time1)
                
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
    z.run()
    print "Time (0-60): ", z.AccelTime
    print "City MPG: ", z.EPACity
    print "Highway MPG: ", z.EPAHighway
    
    print "\nElapsed time: ", time.time()-tt
    
if __name__ == "__main__": 
    test_it()

# End sim_vehicle.py        
