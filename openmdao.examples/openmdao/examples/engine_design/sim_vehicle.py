# sim_vehicle.py
#
# Simulates a vehicle to obatain the following.
# Vehicle model fits in the socket.

from openmdao.main import Assembly, Float, Int
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.main.exceptions import ConstraintError

from openmdao.examples.engine_design.vehicle import Vehicle, IVehicle

class Sim_Vehicle(Assembly):
    def __init__(self, name, parent=None, doc=None, directory=''):
        ''' Creates a new Sim_Vehicle object
        
            # Simulation inputs
            EndSpeed           # Simulation ending speed in mph.
            TimeStep           # Simulation time step in sec.
            
            # Outputs
            AccelTime         # Time to reach 60 mph from start
            '''
        
        super(Sim_Vehicle, self).__init__(name, parent, doc, directory)    

        # set up interface to the framework  
        # Pylint: disable-msg=E1101

        self.add_socket('vehicle', IVehicle, required=True)
        # NOTE: This stuff will be replaced with Sockets when implemented
        self.vehicle = Vehicle("Test_Vehicle")
        #self.vehicle = None

        # Promoted from Vehicle -> Engine
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
        
        
    def execute(self):
        ''' Simulate the vehicle model at full throttle.'''       
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
                self.vehicle.run()
            except ConstraintError:
                self.vehicle.CurrentGear += 1
                self.vehicle.run()

            # Accleration converted to mph/s
            Acceleration = self.vehicle.Acceleration*2.23693629
                
            Velocity += Acceleration*self.TimeStep
            self.vehicle.Velocity = Velocity/2.23693629
        
            Time += self.TimeStep
            #print Time, self.vehicle.CurrentGear, Velocity, self.vehicle.Transmission.RPM, self.vehicle.Engine.RPM
        
        self.AccelTime = Time
        
    
def test_it():    
    z = Sim_Vehicle("New")  
    z.add_child(Vehicle("vehicle"))
    z.run()
    print "Gear:", z.vehicle.CurrentGear
    print "Time:", z.AccelTime
    
if __name__ == "__main__": 
    test_it()

# End sim_vehicle.py        
